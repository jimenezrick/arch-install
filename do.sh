#!/bin/bash

set -euo pipefail

CWD=$(cd $(dirname $0); pwd)
source $CWD/functions.sh

DISK_MODEL='Samsung SSD 850'
DISK_DEV=/dev/$(find_disk_dev "$DISK_MODEL")

prepare_disk $DISK_DEV
#install_bootloader $DISK_DEV
