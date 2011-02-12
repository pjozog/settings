#!/usr/bin/env python

import os
import sys
import errno
import shutil

scriptDir           = sys.path[0]
binDir              = os.path.join(scriptDir, 'linux-stuff', 'bin')
homeDir             = os.getenv('HOME')
requiredDirs        = [os.path.join(homeDir, '.config'),
                       os.path.join(homeDir, 'bin'),
                       os.path.join(homeDir, '.themes')]

#These home dir files will be links to version controlled files
bashrc              = os.path.join(homeDir, '.bashrc')
emacs               = os.path.join(homeDir, '.emacs')
emacsd              = os.path.join(homeDir, '.emacs.d')
openboxd            = os.path.join(homeDir, '.config', 'openbox')
awesomed            = os.path.join(homeDir, '.config', 'awesome')
fontsconf           = os.path.join(homeDir, '.fonts.conf')
dircolors           = os.path.join(homeDir, '.dircolors')
pythonrc            = os.path.join(homeDir, '.pythonrc')
texpathd            = os.path.join(homeDir, 'texpath')
hgrc                = os.path.join(homeDir, '.hgrc')
gtkrc               = os.path.join(homeDir, '.gtkrc-mine')
radianceTheme       = os.path.join(homeDir, '.themes', 'Radiance_ob_test')

#These will be the actual version controlled files (the sources)
bashrcSource        = os.path.join(scriptDir, 'linux-stuff', '.bashrc')
emacsSource         = os.path.join(scriptDir,  'emacs-stuff', '.emacs')
emacsdSource        = os.path.join(scriptDir,  'emacs-stuff', '.emacs.d')
openboxdSource      = os.path.join(scriptDir, 'openbox-stuff', 'openbox')
awesomedSource      = os.path.join(scriptDir, 'awesome-stuff', 'awesome')
fontsconfSource     = os.path.join(scriptDir, 'linux-stuff', '.fonts.conf')
dircolorsSource     = os.path.join(scriptDir, 'linux-stuff', '.dircolors')
pythonrcSource      = os.path.join(scriptDir, 'linux-stuff', '.pythonrc')
texpathdSource      = os.path.join(scriptDir, 'texpath')
hgrcSource          = os.path.join(scriptDir, 'mercurial-stuff', '.hgrc')
gtkrcSource         = os.path.join(scriptDir, 'linux-stuff', '.gtkrc-mine')
radianceThemeSource = os.path.join(scriptDir, 'openbox-stuff', 'Radiance_ob_test')

#make a list of tupple pairs
sourceToDestination = { bashrcSource : bashrc,
                        emacsSource : emacs,
                        emacsdSource : emacsd,
                        openboxdSource : openboxd,
                        awesomedSource : awesomed,
                        fontsconfSource : fontsconf,
                        dircolorsSource : dircolors,
                        pythonrcSource : pythonrc,
                        texpathdSource : texpathd,
                        hgrcSource : hgrc,
                        gtkrcSource : gtkrc,
                        radianceThemeSource : radianceTheme}

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

