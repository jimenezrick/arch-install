#!/bin/bash

set -euo pipefail

etc=$1

rsync --archive --delete --info=flist2,progress2 $etc/ /mnt/etc
