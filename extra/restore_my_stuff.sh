#!/bin/bash

set -euo pipefail

home=$1
etc=$home/archive/backup/etc

mv -v /mnt/etc /mnt/etc2
rsync --archive --delete --info=flist2,progress2 $etc/ /mnt/etc
cp -v /mnt/etc2/fstab /mnt/etc

rsync --archive --delete --info=flist2,progress2 $home/ /mnt/home
