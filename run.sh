#!/bin/bash

set -euo pipefail

die() {
	echo "Error: $*" >&2
	exit 1
}

announce() {
	echo "==> $*"
}

CWD=$(cd $(dirname $0); pwd)
source $CWD/disk.sh
source $CWD/install.sh
source $CWD/config.sh

verify_uefi_boot
verify_network_connectivity
sync_clock

DISK_DEV=/dev/$(find_disk_dev "$DISK_MODEL")

#prepare_disk $DISK_DEV
#install_arch
#install_bootloader $DISK_DEV
