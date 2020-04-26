#!/usr/bin/env python3

"""Enable the configuration files in this repository by placing symbolic links
in the correction places.

"""

import os
import sys
import errno
import shutil

SCRIPT_DIR = sys.path[0]
BIN_DIR = os.path.join(SCRIPT_DIR, 'linux-stuff', 'bin')
HOME_DIR = os.getenv('HOME')
OWNCLOUD_DIR = os.path.join(os.getenv('HOME'), 'ownCloud')
REQUIRED_DIRS = [os.path.join(HOME_DIR, '.config'),
                 os.path.join(HOME_DIR, '.config', 'gtk-3.0'),
                 os.path.join(HOME_DIR, '.config', 'fontconfig'),
                 os.path.join(HOME_DIR, '.config', 'terminator'),
                 os.path.join(HOME_DIR, '.kde'),
                 os.path.join(HOME_DIR, '.kde', 'share'),
                 os.path.join(HOME_DIR, '.kde', 'share', 'apps'),
                 os.path.join(HOME_DIR, '.kde', 'share', 'config'),
                 os.path.join(HOME_DIR, 'bin'),
                 os.path.join(HOME_DIR, 'Documents'),
                 os.path.join(HOME_DIR, 'Documents', 'MATLAB'),
                 os.path.join(HOME_DIR, '.ssh')]


def _main():
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

        (os.path.join(SCRIPT_DIR, 'linux-stuff', '.Xmodmap'),
         os.path.join(HOME_DIR, '.Xmodmap')),

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

        (os.path.join(SCRIPT_DIR, 'linux-stuff', '.xbindkeysrc.scm'),
         os.path.join(HOME_DIR, '.xbindkeysrc.scm')),

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

        (os.path.join(SCRIPT_DIR, 'i3-stuff', 'i3blocks', 'i3blocks.conf'),
         os.path.join(HOME_DIR, '.i3blocks.conf')),

        (os.path.join(SCRIPT_DIR, 'i3-stuff', 'dunst'),
         os.path.join(HOME_DIR, '.config', 'dunst')),

        (os.path.join(SCRIPT_DIR, 'oh-my-zsh'),
         os.path.join(HOME_DIR, '.oh-my-zsh')),

        (os.path.join(SCRIPT_DIR, 'tmux-stuff', 'tmux.conf'),
         os.path.join(HOME_DIR, '.tmux.conf')),
    ]

    # Add all the stuff in the 'bin' directory to link_pairs.
    for basename in os.listdir(BIN_DIR):
        source_name = os.path.abspath(os.path.join(BIN_DIR, basename))
        link_name = os.path.join(HOME_DIR, 'bin', basename)
        link_pairs.append((source_name, link_name))

    def _create_link(src, dest):
        os.symlink(src, dest)

    def _remove_file(path):
        try:
            os.remove(path)
        except OSError as err:
            if (err.errno == errno.EISDIR or err.errno == errno.ENOTEMPTY):
                _print_info('\tRemoving dir ' + path)
                shutil.rmtree(path)
            else:
                pass

    def _make_dir(directory):
        # Does nothing if directory exists.
        try:
            os.mkdir(directory)
        except OSError as err:
            if err.errno == errno.EEXIST:
                pass

    def _print_info(string):
        print('INFO: ' + string)

    for required_dir in REQUIRED_DIRS:
        _make_dir(required_dir)

    def _handle_link_pair(source_name, link_name):
        _print_info("Creating link: " + link_name)
        _remove_file(link_name)
        _create_link(source_name, link_name)

    for pair in link_pairs:
        _handle_link_pair(pair[0], pair[1])

    return 0


if __name__ == '__main__':
    sys.exit(_main())
