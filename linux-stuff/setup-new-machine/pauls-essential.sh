#!/usr/bin/env bash
PACKAGES=""
function addpkg {
    PACKAGES="$PACKAGES $@"
}

function ask {
    while true; do
        if [ "${2:-}" = "Y" ]; then
            prompt="Y/n"
            default=Y
        elif [ "${2:-}" = "N" ]; then
            prompt="y/N"
            default=N
        else
            prompt="y/n"
            default=
        fi
        read -p "$1 [$prompt] " REPLY
        if [ -z "$REPLY" ]; then
            REPLY=$default
        fi
        case "$REPLY" in
            Y*|y*) return 0 ;;
            N*|n*) return 1 ;;
        esac
    done
}

# setup dotfiles and such
../../applysettings.py

# needed in order to check distribution using 'bc' provided by by pkg and 
# 'add-apt-repository' provided by python-software-properties pkg (available for Ubuntu 9.10 and above)
sudo apt-get install bc python-software-properties

DIST=$(lsb_release -rs)
NAME=$(lsb_release -cs)
echo $DIST $NAME
DIST=$(echo "scale=0; $DIST * 100" | bc)
DIST=$(printf "%.0f" $DIST)

#=========================================================================
# 1) list common packages here in alphabetical order
#=========================================================================
addpkg \
    autoconf \
    automake \
    awesome \
    build-essential \
    ccache \
    cowsay \
    dbus \
    dbus-x11 \
    doxygen \
    dvipng \
    dzen2 \
    emacs24 \
    exuberant-ctags \
    fortune-mod \
    fortunes \
    freeglut3-dev \
    gettext \
    gfortran \
    gimp \
    git \
    gnuplot \
    graphviz \
    gtk-doc-tools \
    htop \
    i3lock \
    inkscape \
    libaa1-dev \
    libatlas-base-dev \
    libavcodec-dev \
    libavformat-dev \
    libblas-dev \
    libboost-all-dev \
    libboost-program-options-dev \
    libboost-system-dev \
    libdbus-1-dev \
    libdc1394-22-dev \
    libdevil-dev \
    libeigen3-dev \
    libf2c2-dev \
    libgl1-mesa-dev \
    libglew-dev \
    libglib2.0-dev \
    libglu1-mesa-dev \
    libgoogle-glog-dev \
    libgps-dev \
    libgsl0-dev \
    libgtk-3-dev \
    libgtk2.0-dev \
    liblapack-dev \
    libncurses-dev \
    libqhull-dev \
    libraw1394-dev \
    libsdl1.2-dev \
    libspnav-dev \
    libsuitesparse-dev \
    libtiff4-dev \
    libwxgtk2.8-dev \
    libxi-dev \
    libxml2-dev \
    libxmu-dev \
    libzmq-dev \
    meld \
    mercurial \
    mesa-common-dev \
    nautilus-dropbox \
    openbox \
    openssh-server \
    pandoc \
    pavucontrol \
    python-dev \
    python-gtk2-dev \
    python-pip \
    python-zmq \
    qt4-qtconfig \
    sshfs \
    subversion \
    suckless-tools \
    terminator \
    texlive \
    texlive-generic-extra \
    texlive-latex-extra \
    texlive-science \
    vinagre \
    vlc \
    vnc4server \
    w3m \
    xautomation \
    xbindkeys \
    xfce4-power-manager \
    xfonts-100dpi \
    xfonts-75dpi \
    zsh

#=========================================================================
# 2) list distribution-specific packages here in alphabetical order
#=========================================================================
# cmake
if [ $DIST -le 804 ]; then
    addpkg cmake-gui
else
    addpkg cmake-curses-gui
fi

# opencv
if [ $DIST -ge 1004 -a $DIST -lt 1204 ]; then
    addpkg libcv-dev libcvaux-dev libhighgui-dev
elif [ $DIST -ge 1204 ]; then
    if ask "Do you want to install OpenCV from Ubuntu repositories (not recommended if using Nvidia GPU)?" Y; then
        addpkg libopencv-dev libcv-dev libhighgui-dev libopencv-contrib-dev libopencv-gpu-dev
    fi
fi

# changed package name in 12.04
if [ $DIST -ge 1204 ]; then
    addpkg libcurl4-gnutls-dev
else
    addpkg libcurl4-dev
fi

# sun
if [ $DIST -ge 910 -a $DIST -lt 1204 ]; then
    sudo add-apt-repository "deb http://archive.canonical.com/ $NAME partner"
    addpkg sun-java6-jdk
elif [ $DIST -ge 1204 ]; then
    addpkg openjdk-6-jdk
fi

# go forth!
echo "apt-get install $PACKAGES"
sudo apt-get update
sudo apt-get install $PACKAGES

# configure java
sudo update-alternatives --config java
sudo update-alternatives --config jar
sudo update-alternatives --config javac

# install awesome-session
sudo cp awesome-session /usr/bin
sudo cp awesome.desktop /usr/share/xsessions

# change to zsh
echo 'Changing default shell to zsh, please enter your password'
chsh -s /usr/bin/zsh

# run xbindkeys
echo 'Running xbindkeys to enable horizontal scrolling in firefox'
xbindkeys

# install python packages, some of which are broken on Ubuntu 14.04
sudo pip install -U cython
sudo pip install -U numpy
sudo pip install -U scipy
sudo pip install -U matplotlib
sudo pip install -U pandas
sudo pip install -U scikit-learn
sudo pip install -U scikits.sparse
sudo pip install -U scikit-image
sudo pip install -U jinja2
sudo pip install -U tornado
sudo pip install -U ipython[all]
