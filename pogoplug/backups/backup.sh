#!/bin/bash
if [ $# -eq 0 ]; then
    echo "Usage: " $(basename $0) " backup-directory"
    exit
fi

OUTDIR=$1
tar -cvpzf "${OUTDIR}"/POGO`date +%m%d%y`.tgz --exclude=/proc --exclude=/mnt --exclude=/sys --exclude=/dev --exclude=/lost+found  --exclude=/media /
