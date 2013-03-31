#!/usr/bin/env python2

import os
import sys
import errno
import shutil

scriptDir           = sys.path[0]
binDir              = os.path.join(scriptDir, 'linux-stuff', 'bin')
homeDir             = os.getenv('HOME')
requiredDirs        = [os.path.join(homeDir, '.config'),
                       os.path.join(homeDir, '.config', 'gtk-3.0'),
                       os.path.join(homeDir, '.matplotlib'),
                       os.path.join(homeDir, 'bin'),
                       os.path.join(homeDir, '.themes'),
                       os.path.join(homeDir, 'Documents'),
                       os.path.join(homeDir, 'Documents', 'MATLAB')]

#These home dir files will be links to version controlled files
linkList = [os.path.join(homeDir, '.bashrc'),
            os.path.join(homeDir, '.zshrc'),
            os.path.join(homeDir, '.profile'),
            os.path.join(homeDir, '.shell_aliases'),
            os.path.join(homeDir, '.emacs.d'),
            os.path.join(homeDir, '.config', 'openbox'),
            os.path.join(homeDir, '.config', 'awesome'),
            os.path.join(homeDir, '.config', 'ipython'),
            os.path.join(homeDir, '.matplotlib', 'matplotlibrc'),
            os.path.join(homeDir, '.config', 'autostart.sh'),
            os.path.join(homeDir, '.config', 'gtk-3.0', 'settings.ini'),
            os.path.join(homeDir, '.fonts.conf'),
            os.path.join(homeDir, '.dircolors'),
            os.path.join(homeDir, '.pythonrc'),
            os.path.join(homeDir, 'texpath'),
            os.path.join(homeDir, '.hgrc'),
            os.path.join(homeDir, '.gitconfig'),
            os.path.join(homeDir, '.gtkrc-mine'),
            os.path.join(homeDir, '.themes', 'Radiance_ob_test'),
            os.path.join(homeDir, '.xmodmap'),
            os.path.join(homeDir, '.swapcaps'),
            os.path.join(homeDir, '.svn_project'),
            os.path.join(homeDir, '.aspell.en.pws'),
            os.path.join(homeDir, '.fonts'),
            os.path.join(homeDir, '.xinitrc'),
            os.path.join(homeDir, 'Documents', 'MATLAB', 'startup.m')]

#These will be the actual version controlled files (the sources)
sourceList = [os.path.join(scriptDir, 'linux-stuff', '.bashrc'),
              os.path.join(scriptDir,  'linux-stuff', '.zshrc'),
              os.path.join(scriptDir,  'linux-stuff', '.profile'),
              os.path.join(scriptDir,  'linux-stuff', '.shell_aliases'),
              os.path.join(scriptDir,  'emacs-stuff', '.emacs.d'),
              os.path.join(scriptDir, 'openbox-stuff', 'openbox'),
              os.path.join(scriptDir, 'awesome-stuff', 'awesome'),
              os.path.join(scriptDir, 'ipython-stuff', 'ipython'),
              os.path.join(scriptDir, 'ipython-stuff', 'matplotlibrc'),
              os.path.join(scriptDir, 'linux-stuff', 'autostart.sh'),
              os.path.join(scriptDir, 'linux-stuff', 'settings.ini'),
              os.path.join(scriptDir, 'linux-stuff', '.fonts.conf'),
              os.path.join(scriptDir, 'linux-stuff', '.dircolors'),
              os.path.join(scriptDir, 'linux-stuff', '.pythonrc'),
              os.path.join(scriptDir, 'texpath'),
              os.path.join(scriptDir, 'mercurial-stuff', '.hgrc'),
              os.path.join(scriptDir, 'git-stuff', '.gitconfig'),
              os.path.join(scriptDir, 'linux-stuff', '.gtkrc-mine'),
              os.path.join(scriptDir, 'openbox-stuff', 'Radiance_ob_test'),
              os.path.join(scriptDir, 'linux-stuff', '.xmodmap'),
              os.path.join(scriptDir, 'linux-stuff', '.swapcaps'),
              os.path.join(scriptDir, 'linux-stuff', '.svn_project'),
              os.path.join(scriptDir, 'linux-stuff', '.aspell.en.pws'),
              os.path.join(scriptDir, 'linux-stuff', '.fonts'),
              os.path.join(scriptDir, 'linux-stuff', '.xinitrc'),
              os.path.join(scriptDir, 'matlab-stuff', 'startup.m')]

sourceToDestination = {}

#make a list of tupple pairs
for i in range(0, len(linkList)):
    sourceToDestination[sourceList[i]] = linkList[i]

#add all the stuff in the 'bin' directory to sourceToDestination
for basename in os.listdir(binDir):
    source = os.path.abspath(os.path.join(binDir, basename))
    dest = os.path.join(homeDir, 'bin', basename)
    sourceToDestination[source] = dest;

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

    for src, dest in sourceToDestination.items():

        printInfo("Creating link: " + dest)
        removeFile(dest)
        createLink(src, dest)
        
    return 0
    
if __name__ == '__main__':

    sys.exit(main())

