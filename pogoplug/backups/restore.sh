#!/bin/bash

# After formatting the destination drive as ext3, run this script AS ROOT!  NOT "sudo".
# Use "sudo su" if you have to.

BACKUP_TGZ=$1
DEST_DRIVE=$2

if [ $# -ne 2 ]; then
    echo "Usage: " $(basename $0) " <backup-tgz> <destination-drive>"
    exit
fi

cd "${BACKUP_TGZ}"
tar -xvzpf "$BACKUP_TGZ" -C "${DEST_DRIVE}"
cd "${DEST_DRIVE}"
mkdir media
mkdir mnt
mkdir proc
mkdir dev
mkdir sys
