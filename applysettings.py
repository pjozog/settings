#!/usr/bin/env python2

import os
import sys
import errno
import shutil

scriptDir           = sys.path[0]
binDir              = os.path.join(scriptDir, 'linux-stuff', 'bin')
homeDir             = os.getenv('HOME')
dropboxDir          = os.path.join(os.getenv('HOME'), 'Dropbox')
requiredDirs        = [os.path.join(homeDir, '.config'),
                       os.path.join(homeDir, '.config', 'gtk-3.0'),
                       os.path.join(homeDir, '.config', 'terminator'),
                       os.path.join(homeDir, '.kde'),
                       os.path.join(homeDir, '.kde', 'share'),
                       os.path.join(homeDir, '.kde', 'share', 'apps'),
                       os.path.join(homeDir, '.kde', 'share', 'config'),
                       os.path.join(homeDir, 'bin'),
                       os.path.join(homeDir, '.themes'),
                       os.path.join(homeDir, 'Documents'),
                       os.path.join(homeDir, 'Documents', 'MATLAB'),
                       os.path.join(homeDir, '.ssh')]

linkPairs = [

    # format:
    # (path to version-controlled file,
    #  name of symbolic link)
    #
    # think of it as running 'ln -sf <first_thing> <second_thing>'

    (os.path.join(scriptDir, 'linux-stuff', '.bashrc'),
     os.path.join(homeDir, '.bashrc')),

    (os.path.join(scriptDir,  'linux-stuff', '.bash_profile'),
     os.path.join(homeDir, '.bash_profile')),

    (os.path.join(scriptDir,  'linux-stuff', '.zshrc'),
     os.path.join(homeDir, '.zshrc')),

    (os.path.join(scriptDir,  'linux-stuff', '.zshrc.local'),
     os.path.join(homeDir, '.zshrc.local')),

    (os.path.join(scriptDir,  'linux-stuff', '.zshrc.pre'),
     os.path.join(homeDir, '.zshrc.pre')),

    (os.path.join(scriptDir,  'linux-stuff', '.profile'),
     os.path.join(homeDir, '.profile')),

    (os.path.join(scriptDir,  'linux-stuff', '.shell_aliases'),
     os.path.join(homeDir, '.shell_aliases')),

    (os.path.join(scriptDir,  'emacs-stuff', '.emacs.d'),
     os.path.join(homeDir, '.emacs.d')),

    (os.path.join(scriptDir, 'openbox-stuff', 'openbox'),
     os.path.join(homeDir, '.config', 'openbox')),

    (os.path.join(scriptDir, 'awesome-stuff', 'awesome'),
     os.path.join(homeDir, '.config', 'awesome')),

    (os.path.join(scriptDir, 'ipython-stuff', 'ipython'),
     os.path.join(homeDir, '.ipython')),

    (os.path.join(scriptDir, 'ipython-stuff', 'matplotlib'),
     os.path.join(homeDir, '.config', 'matplotlib')),

    (os.path.join(scriptDir, 'linux-stuff', 'autostart.sh'),
     os.path.join(homeDir, '.config', 'autostart.sh')),

    (os.path.join(scriptDir, 'linux-stuff', 'settings.ini'),
     os.path.join(homeDir, '.config', 'gtk-3.0', 'settings.ini')),

    (os.path.join(scriptDir, 'linux-stuff', '.fonts.conf'),
     os.path.join(homeDir, '.fonts.conf')),

    (os.path.join(scriptDir, 'linux-stuff', '.pythonrc'),
     os.path.join(homeDir, '.pythonrc')),

    (os.path.join(scriptDir, 'texpath'),
     os.path.join(homeDir, 'texpath')),

    (os.path.join(scriptDir, 'mercurial-stuff', '.hgrc'),
     os.path.join(homeDir, '.hgrc')),

    (os.path.join(scriptDir, 'git-stuff', '.gitconfig'),
     os.path.join(homeDir, '.gitconfig')),

    (os.path.join(scriptDir, 'git-stuff', '.gitignore'),
     os.path.join(homeDir, '.gitignore')),

    (os.path.join(scriptDir, 'git-stuff', 'git-template'),
     os.path.join(homeDir, '.git-template')),

    (os.path.join(scriptDir, 'linux-stuff', '.gtkrc-mine'),
     os.path.join(homeDir, '.gtkrc-mine')),

    (os.path.join(scriptDir, 'linux-stuff', '.gtkrc-no-emacs'),
     os.path.join(homeDir, '.gtkrc-no-emacs')),

    (os.path.join(scriptDir, 'openbox-stuff', 'Radiance_ob_test'),
     os.path.join(homeDir, '.themes', 'Radiance_ob_test')),

    (os.path.join(scriptDir, 'linux-stuff', '.Xmodmap'),
     os.path.join(homeDir, '.Xmodmap')),

    (os.path.join(scriptDir, 'linux-stuff', '.aspell.en.pws'),
     os.path.join(homeDir, '.aspell.en.pws')),

    (os.path.join(scriptDir, 'linux-stuff', '.fonts'),
     os.path.join(homeDir, '.fonts')),

    (os.path.join(scriptDir, 'linux-stuff', '.xinitrc'),
     os.path.join(homeDir, '.xinitrc')),

    (os.path.join(scriptDir, 'gdb-stuff', '.gdbinit'),
     os.path.join(homeDir, '.gdbinit')),

    (os.path.join(scriptDir, 'matlab-stuff', 'startup.m'),
     os.path.join(homeDir, 'Documents', 'MATLAB', 'startup.m')),

    (os.path.join(dropboxDir, 'ssh', 'config-work'),
     os.path.join(homeDir, '.ssh', 'config-work')),

    (os.path.join(dropboxDir, 'ssh', 'config-home'),
     os.path.join(homeDir, '.ssh', 'config-home')),

    (os.path.join(dropboxDir, 'zsh', 'prompt'),
     os.path.join(homeDir, '.zshrc.prompt')),

    (os.path.join(scriptDir, 'linux-stuff', '.xbindkeysrc.scm'),
     os.path.join(homeDir, '.xbindkeysrc.scm')),

    (os.path.join(scriptDir, 'terminator-stuff', 'config'),
     os.path.join(homeDir, '.config', 'terminator', 'config')),

    (os.path.join(scriptDir, 'konsole-stuff', 'config', 'konsolerc'),
     os.path.join(homeDir, '.kde', 'share', 'config', 'konsolerc')),

    (os.path.join(scriptDir, 'konsole-stuff', 'apps', 'konsole'),
     os.path.join(homeDir, '.kde', 'share', 'apps', 'konsole')),

    (os.path.join(scriptDir, 'i3-stuff', 'i3'),
     os.path.join(homeDir, '.i3')),

    (os.path.join(scriptDir, 'i3-stuff', 'i3blocks', 'i3blocks.conf'),
     os.path.join(homeDir, '.i3blocks.conf')),

    (os.path.join(scriptDir, 'i3-stuff', 'dunst'),
     os.path.join(homeDir, '.config', 'dunst'))

]

#add all the stuff in the 'bin' directory to linkPairs
for basename in os.listdir(binDir):
    sourceName = os.path.abspath(os.path.join(binDir, basename))
    linkName = os.path.join(homeDir, 'bin', basename)
    linkPairs.append ((sourceName, linkName))

def createLink(src, dest):
    os.symlink(src, dest)

def removeFile(path):
    try:
        os.remove(path)
    except OSError, err:
        if (err.errno == errno.EISDIR or
            err.errno == errno.ENOTEMPTY):
            printInfo('\tRemoving dir ' + path)
            shutil.rmtree(path)
        else:
            pass

def makeDir(directory):
    try:
        os.mkdir(directory)
    except OSError, err: #do nothing if directory exists
        if err.errno == errno.EEXIST:
            pass

def printInfo(string):
    print 'INFO: ' + string

def main():

    for d in requiredDirs:
        makeDir(d)

    for tuple in linkPairs:
        sourceName = tuple[0]
        linkName = tuple[1]
        printInfo("Creating link: " + linkName)
        removeFile(linkName)
        createLink(sourceName, linkName)

    return 0

if __name__ == '__main__':

    sys.exit(main())
