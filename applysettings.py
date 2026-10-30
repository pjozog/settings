#!/usr/bin/env python3

"""Apply settings for the current user.

Enable the configuration files in this repository by placing symbolic links
in the correction places.
"""

import os
import sys
import errno
import shlex
import shutil
import datetime
import subprocess

SCRIPT_DIR = sys.path[0]
BIN_DIR = os.path.join(SCRIPT_DIR, 'linux-stuff', 'bin')
HOME_DIR = os.getenv('HOME', "home_dir")
OWNCLOUD_DIR = os.path.join(HOME_DIR, 'ownCloud')

# Timestamped directory holding a trace of everything this run overwrote:
#   files/                backup copies of anything replaced by a symlink
#   gsettings.before      the old gsettings values, before we changed them
#   restore-gsettings.sh  a script that puts those gsettings values back
#   manifest.txt          a human-readable log of what this run did
_TIMESTAMP = datetime.datetime.now().strftime('%Y-%m-%dT%H-%M-%S')
BACKUP_ROOT = os.path.join(HOME_DIR, '.applysettings-backups')
BACKUP_DIR = os.path.join(BACKUP_ROOT, _TIMESTAMP)

# Desktop settings for GNOME/GTK-4 apps (e.g. Nautilus), which read these from
# gsettings/dconf rather than from gtk-3.0/gtk-4.0 settings.ini. All values are
# strings; _apply_gsettings assumes that when comparing current values.
GSETTINGS = [
    ('org.gnome.desktop.interface', 'icon-theme', 'Yaru'),
    ('org.gnome.desktop.interface', 'gtk-theme', 'Yaru'),
    ('org.gnome.desktop.interface', 'cursor-theme', 'Yaru'),
    ('org.gnome.desktop.interface', 'font-name', 'Ubuntu 8'),
]

# meld is launched with XDG_CONFIG_HOME pointed at MELD_SANDBOX_DIR so it reads
# none of the real ~/.config (in particular the global Emacs gtk-key-theme).
# The one thing that sandbox needs restored is a complete icon theme, which
# lives in the compiled dconf db MELD_SANDBOX_DB, built from MELD_SANDBOX_SRC.
MELD_SANDBOX_DIR = os.path.join(HOME_DIR, '.config', 'meld-sandbox')
MELD_SANDBOX_DB = os.path.join(MELD_SANDBOX_DIR, 'dconf', 'user')
MELD_SANDBOX_SRC = os.path.join(SCRIPT_DIR, 'linux-stuff', 'meld-sandbox', 'db.d')

_MANIFEST = []
_BACKUP_CREATED = False
REQUIRED_DIRS = [os.path.join(HOME_DIR, '.config'),
                 os.path.join(HOME_DIR, '.config', 'gtk-3.0'),
                 os.path.join(HOME_DIR, '.config', 'gtk-4.0'),
                 os.path.join(HOME_DIR, '.config', 'dconf'),
                 os.path.join(HOME_DIR, '.config', 'dconf', 'profile'),
                 MELD_SANDBOX_DIR,
                 os.path.join(MELD_SANDBOX_DIR, 'dconf'),
                 os.path.join(HOME_DIR, '.config', 'fontconfig'),
                 os.path.join(HOME_DIR, '.config', 'terminator'),
                 os.path.join(HOME_DIR, '.kde'),
                 os.path.join(HOME_DIR, '.kde', 'share'),
                 os.path.join(HOME_DIR, '.kde', 'share', 'apps'),
                 os.path.join(HOME_DIR, '.kde', 'share', 'config'),
                 os.path.join(HOME_DIR, '.local'),
                 os.path.join(HOME_DIR, '.local', 'share'),
                 os.path.join(HOME_DIR, 'bin'),
                 os.path.join(HOME_DIR, 'Documents'),
                 os.path.join(HOME_DIR, 'Documents', 'MATLAB'),
                 os.path.join(HOME_DIR, '.ssh')]


def _create_link(src: str, dest: str) -> None:
    os.symlink(src, dest)


def _remove_file(path: str) -> None:
    try:
        os.remove(path)
    except OSError as err:
        if (err.errno == errno.EISDIR or err.errno == errno.ENOTEMPTY):
            _print_info('\tRemoving dir ' + path)
            shutil.rmtree(path)
        else:
            pass


def _make_dir(directory: str) -> None:
    # Does nothing if directory exists.
    try:
        os.mkdir(directory)
    except OSError as err:
        if err.errno == errno.EEXIST:
            pass


def _print_info(string: str) -> None:
    print('INFO: ' + string)


def _log(msg: str) -> None:
    # Record in the manifest and echo to the console.
    _MANIFEST.append(msg)
    _print_info(msg)


def _write_text(path: str, content: str) -> None:
    with open(path, 'w') as handle:
        handle.write(content)


def _ensure_backup_dir() -> None:
    # Created lazily, so a run that changes nothing leaves no empty dir behind.
    global _BACKUP_CREATED
    if not _BACKUP_CREATED:
        os.makedirs(BACKUP_DIR, exist_ok=True)
        _BACKUP_CREATED = True


def _backup_existing(path: str) -> None:
    """Copy whatever currently lives at `path` into the backup dir.

    The original location is mirrored under <BACKUP_DIR>/files/ so nothing
    collides and it is obvious where each item came from. Symlinks are
    recreated (not followed) so the backup faithfully records the old target.
    """
    if not os.path.lexists(path):
        return
    _ensure_backup_dir()
    dest = os.path.join(BACKUP_DIR, 'files', os.path.abspath(path).lstrip(os.sep))
    os.makedirs(os.path.dirname(dest), exist_ok=True)
    if os.path.islink(path):
        target = os.readlink(path)
        os.symlink(target, dest)
        _log('backed up symlink: %s -> %s' % (path, target))
    elif os.path.isdir(path):
        shutil.copytree(path, dest, symlinks=True)
        _log('backed up directory: ' + path)
    else:
        shutil.copy2(path, dest)
        _log('backed up file: ' + path)


def _handle_link_pair(source_name: str, link_name: str) -> None:
    if os.path.islink(link_name) and os.readlink(link_name) == source_name:
        _log('ok (already linked): ' + link_name)
        return
    _log('linking: %s -> %s' % (link_name, source_name))
    _backup_existing(link_name)
    _remove_file(link_name)
    _create_link(source_name, link_name)


def _dconf_path(schema: str, key: str) -> str:
    return '/' + schema.replace('.', '/') + '/' + key


def _apply_gsettings() -> None:
    """Apply GSETTINGS, saving the prior values and a restore script first."""
    gsettings = shutil.which('gsettings')
    if not gsettings:
        _log('gsettings binary not found; skipping desktop interface settings')
        return
    dconf = shutil.which('dconf')

    before_lines = []
    restore_lines = ['#!/bin/sh',
                     '# gsettings values captured before applysettings.py ran.',
                     '# Run this script to restore them.',
                     'set -e',
                     '']
    changed = False

    for schema, key, value in GSETTINGS:
        desired = "'%s'" % value  # GVariant form for a string
        try:
            current = subprocess.run(
                [gsettings, 'get', schema, key],
                capture_output=True, text=True, check=True).stdout.strip()
        except subprocess.CalledProcessError:
            _log('gsettings: schema/key not found, skipping %s %s' % (schema, key))
            continue

        if current == desired:
            _log('gsettings ok (already %s): %s %s' % (value, schema, key))
            continue

        changed = True
        path = _dconf_path(schema, key)
        user_val = ''
        if dconf:
            user_val = subprocess.run(
                [dconf, 'read', path],
                capture_output=True, text=True).stdout.strip()

        # Record how to put it back exactly as it was.
        if user_val:
            before_lines.append('%s = %s' % (path, user_val))
            restore_lines.append('dconf write %s %s' % (path, shlex.quote(user_val)))
        elif dconf:
            # No explicit value => it was using the schema default; reset to it.
            before_lines.append('%s = (unset; default was %s)' % (path, current))
            restore_lines.append('gsettings reset %s %s' % (schema, key))
        else:
            # No dconf: best effort, restore the effective value we observed.
            before_lines.append('%s %s = %s' % (schema, key, current))
            restore_lines.append('gsettings set %s %s %s'
                                 % (schema, key, shlex.quote(current)))

        try:
            subprocess.run([gsettings, 'set', schema, key, value], check=True)
            _log('gsettings set %s %s -> %s (was %s)'
                 % (schema, key, value, current))
        except subprocess.CalledProcessError:
            _log('gsettings: FAILED to set %s %s' % (schema, key))

    if changed:
        _ensure_backup_dir()
        _write_text(os.path.join(BACKUP_DIR, 'gsettings.before'),
                    '\n'.join(before_lines) + '\n')
        restore_path = os.path.join(BACKUP_DIR, 'restore-gsettings.sh')
        _write_text(restore_path, '\n'.join(restore_lines) + '\n')
        os.chmod(restore_path, 0o755)
        _log('wrote gsettings backup and restore script to ' + BACKUP_DIR)
    else:
        _log('gsettings already up to date; nothing changed')


def _compile_meld_sandbox() -> None:
    """Build the meld sandbox icon-theme db from MELD_SANDBOX_SRC.

    `dconf compile` is idempotent: it rewrites MELD_SANDBOX_DB from scratch
    every run, so there is nothing to back up or diff.
    """
    dconf = shutil.which('dconf')
    if not dconf:
        _log('dconf binary not found; skipping meld-sandbox db')
        return
    if not os.path.isdir(MELD_SANDBOX_SRC):
        _log('meld-sandbox source missing, skipping: ' + MELD_SANDBOX_SRC)
        return
    try:
        subprocess.run([dconf, 'compile', MELD_SANDBOX_DB, MELD_SANDBOX_SRC],
                       check=True)
        _log('compiled meld-sandbox db: %s <- %s'
             % (MELD_SANDBOX_DB, MELD_SANDBOX_SRC))
    except subprocess.CalledProcessError:
        _log('FAILED to compile meld-sandbox db')


def _write_manifest() -> None:
    if not _BACKUP_CREATED:
        _print_info('Nothing was overwritten; no backup directory created.')
        return
    _write_text(os.path.join(BACKUP_DIR, 'manifest.txt'),
                '\n'.join(_MANIFEST) + '\n')
    _print_info('Trace of this run written to ' + BACKUP_DIR)


def _main() -> int:
    """Run main function."""
    link_pairs = [
        # Format:
        # (path to version-controlled file,
        #  name of symbolic link)
        #
        # Think of it as running 'ln -sf <first_thing> <second_thing>'.

        (os.path.join(SCRIPT_DIR, 'linux-stuff', '.bashrc'),
         os.path.join(HOME_DIR, '.bashrc')),

        (os.path.join(SCRIPT_DIR, 'linux-stuff', '.bash_profile'),
         os.path.join(HOME_DIR, '.bash_profile')),

        (os.path.join(SCRIPT_DIR, 'linux-stuff', '.zshrc'),
         os.path.join(HOME_DIR, '.zshrc')),

        (os.path.join(SCRIPT_DIR, 'linux-stuff', '.zshrc.local'),
         os.path.join(HOME_DIR, '.zshrc.local')),

        (os.path.join(SCRIPT_DIR, 'linux-stuff', '.zshrc.pre'),
         os.path.join(HOME_DIR, '.zshrc.pre')),

        (os.path.join(SCRIPT_DIR, 'linux-stuff', '.profile'),
         os.path.join(HOME_DIR, '.profile')),

        (os.path.join(SCRIPT_DIR, 'linux-stuff', '.shell_aliases'),
         os.path.join(HOME_DIR, '.shell_aliases')),

        (os.path.join(SCRIPT_DIR, 'emacs-stuff', '.emacs.d'),
         os.path.join(HOME_DIR, '.emacs.d')),

        (os.path.join(SCRIPT_DIR, 'openbox-stuff', 'openbox'),
         os.path.join(HOME_DIR, '.config', 'openbox')),

        (os.path.join(SCRIPT_DIR, 'awesome-stuff', 'awesome'),
         os.path.join(HOME_DIR, '.config', 'awesome')),

        (os.path.join(SCRIPT_DIR, 'ipython-stuff', 'ipython'),
         os.path.join(HOME_DIR, '.ipython')),

        (os.path.join(SCRIPT_DIR, 'ipython-stuff', 'matplotlib'),
         os.path.join(HOME_DIR, '.config', 'matplotlib')),

        (os.path.join(SCRIPT_DIR, 'ipython-stuff', 'jupyter'),
         os.path.join(HOME_DIR, '.jupyter')),

        (os.path.join(SCRIPT_DIR, 'linux-stuff', 'settings.ini'),
         os.path.join(HOME_DIR, '.config', 'gtk-3.0', 'settings.ini')),

        (os.path.join(SCRIPT_DIR, 'linux-stuff', 'settings.ini'),
         os.path.join(HOME_DIR, '.config', 'gtk-4.0', 'settings.ini')),

        (os.path.join(SCRIPT_DIR, 'linux-stuff', '.dircolors'),
         os.path.join(HOME_DIR, '.dircolors')),

        (os.path.join(SCRIPT_DIR, 'texpath'),
         os.path.join(HOME_DIR, 'texpath')),

        (os.path.join(SCRIPT_DIR, 'mercurial-stuff', '.hgrc'),
         os.path.join(HOME_DIR, '.hgrc')),

        (os.path.join(SCRIPT_DIR, 'git-stuff', '.gitconfig'),
         os.path.join(HOME_DIR, '.gitconfig')),

        (os.path.join(SCRIPT_DIR, 'git-stuff', '.gitignore'),
         os.path.join(HOME_DIR, '.gitignore')),

        (os.path.join(SCRIPT_DIR, 'git-stuff', 'git-template'),
         os.path.join(HOME_DIR, '.git-template')),

        (os.path.join(SCRIPT_DIR, 'linux-stuff', '.gtkrc-mine'),
         os.path.join(HOME_DIR, '.gtkrc-mine')),

        (os.path.join(SCRIPT_DIR, 'themes'),
         os.path.join(HOME_DIR, '.themes')),

        (os.path.join(SCRIPT_DIR, 'icons'),
         os.path.join(HOME_DIR, '.icons')),

        # This *has* to be called ~/.xmodmap, otherwise xterm won't correctly
        # handle Alt keys (e.g., Alt-b).
        (os.path.join(SCRIPT_DIR, 'linux-stuff', 'keyboard-key-layout'),
         os.path.join(HOME_DIR, '.xmodmap')),

        (os.path.join(SCRIPT_DIR, 'linux-stuff', 'keyboard-modifier-layout'),
         os.path.join(HOME_DIR, '.keyboard-modifier-layout')),

        (os.path.join(SCRIPT_DIR, 'linux-stuff', '.Xresources'),
         os.path.join(HOME_DIR, '.Xresources')),

        (os.path.join(SCRIPT_DIR, 'linux-stuff', '.aspell.en.pws'),
         os.path.join(HOME_DIR, '.aspell.en.pws')),

        (os.path.join(SCRIPT_DIR, 'linux-stuff', 'fontconfig', 'fonts.conf'),
         os.path.join(HOME_DIR, '.config', 'fontconfig', 'fonts.conf')),

        (os.path.join(SCRIPT_DIR, 'linux-stuff', 'fontconfig', 'fonts'),
         os.path.join(HOME_DIR, '.fonts')),

        (os.path.join(SCRIPT_DIR, 'gdb-stuff', '.gdbinit'),
         os.path.join(HOME_DIR, '.gdbinit')),

        (os.path.join(SCRIPT_DIR, 'matlab-stuff', 'startup.m'),
         os.path.join(HOME_DIR, 'Documents', 'MATLAB', 'startup.m')),

        (os.path.join(OWNCLOUD_DIR, 'ssh', 'config-home'),
         os.path.join(HOME_DIR, '.ssh', 'config-home')),

        (os.path.join(OWNCLOUD_DIR, 'zsh', 'prompt'),
         os.path.join(HOME_DIR, '.zshrc.prompt')),

        (os.path.join(OWNCLOUD_DIR, 'code', 'private', 'gcalendar'),
         os.path.join(HOME_DIR, 'bin', 'gcalendar')),

        (os.path.join(SCRIPT_DIR, 'terminator-stuff', 'config'),
         os.path.join(HOME_DIR, '.config', 'terminator', 'config')),

        (os.path.join(SCRIPT_DIR, 'konsole-stuff', 'konsolerc'),
         os.path.join(HOME_DIR, '.config', 'konsolerc')),

        (os.path.join(SCRIPT_DIR, 'konsole-stuff', 'konsole'),
         os.path.join(HOME_DIR, '.local', 'share', 'konsole')),

        (os.path.join(SCRIPT_DIR, 'linux-stuff', '.xsession'),
         os.path.join(HOME_DIR, '.xsession')),

        (os.path.join(SCRIPT_DIR, 'i3-stuff', 'i3'),
         os.path.join(HOME_DIR, '.i3')),

        (os.path.join(SCRIPT_DIR, 'sway'),
         os.path.join(HOME_DIR, '.sway')),

        (os.path.join(SCRIPT_DIR, 'waybar'),
         os.path.join(HOME_DIR, '.config', 'waybar')),

        (os.path.join(SCRIPT_DIR, 'xkb'),
         os.path.join(HOME_DIR, '.config', 'xkb')),

        (os.path.join(SCRIPT_DIR, 'linux-stuff', 'environment.sh'),
         os.path.join(HOME_DIR, '.config', 'environment.sh')),

        (os.path.join(SCRIPT_DIR, 'xdg-desktop-portal'),
         os.path.join(HOME_DIR, '.config', 'xdg-desktop-portal')),

        (os.path.join(SCRIPT_DIR, 'i3-stuff', 'i3blocks', 'i3blocks.conf'),
         os.path.join(HOME_DIR, '.i3blocks.conf')),

        (os.path.join(SCRIPT_DIR, 'i3-stuff', 'dunst'),
         os.path.join(HOME_DIR, '.config', 'dunst')),

        (os.path.join(SCRIPT_DIR, 'oh-my-zsh'),
         os.path.join(HOME_DIR, '.oh-my-zsh')),

        (os.path.join(SCRIPT_DIR, 'ipython-stuff', 'mypy.ini'),
         os.path.join(HOME_DIR, '.mypy.ini')),

        (os.path.join(SCRIPT_DIR, 'tmux-stuff', 'tmux.conf'),
         os.path.join(HOME_DIR, '.tmux.conf')),
    ]

    # Add all the stuff in the 'bin' directory to link_pairs.
    for basename in os.listdir(BIN_DIR):
        source_name = os.path.abspath(os.path.join(BIN_DIR, basename))
        link_name = os.path.join(HOME_DIR, 'bin', basename)
        link_pairs.append((source_name, link_name))

    for required_dir in REQUIRED_DIRS:
        _make_dir(required_dir)

    for pair in link_pairs:
        _handle_link_pair(pair[0], pair[1])

    _apply_gsettings()
    _compile_meld_sandbox()
    _write_manifest()

    return 0


if __name__ == '__main__':
    sys.exit(_main())
