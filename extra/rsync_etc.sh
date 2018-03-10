#!/bin/bash

set -euo pipefail

etc=$1

mv -v /mnt/etc /mnt/etc2
rsync --archive --delete --info=flist2,progress2 $etc/ /mnt/etc
cp -v /mnt/etc2/fstab /mnt/etc
