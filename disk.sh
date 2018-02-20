#!/bin/bash

set -euo pipefail

die() {
	echo "Error: $*" >&2
	exit 1
}

announce() {
	echo "==> $*"
}

find_disk_dev() {
	local -r model=$1
	local dev=$(lsblk -S -o kname,model | awk "/$model/"'{print $1}')

	if mount | grep /dev/$dev >/dev/null
	then
		die "disk is mounted"
	fi
	echo $dev
}

prepare_disk() {
	local -r dev=$1

	parted -s $dev -a optimal \
		mklabel gpt \
		mkpart primary 0% 513MiB \
		mkpart primary 513MiB 100% \
		set 1 boot on
}

DISK_MODEL='Samsung SSD 850'
DISK_DEV=/dev/$(find_disk_dev "$DISK_MODEL")

announce Preparing $DISK_DEV
prepare_disk $DISK_DEV
