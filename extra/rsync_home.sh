#!/bin/bash

set -euo pipefail

local home=$1

rsync --archive --delete --info=flist2,progress2 $home /mnt
