#!/bin/bash

set -euo pipefail

home=$1

rsync --archive --delete --info=flist2,progress2 $home/ /mnt/home
