#!/bin/bash

set -euo pipefail

die() {
	echo "Error: $*" >&2
	exit 1
}

find_disk_dev() {
	local -r model=$1
	local dev=$(lsblk -S -o kname,model | awk "/$model/"'{print $1}')
	mount | grep /dev/$dev >/dev/null
	if [[ $? == 0 ]]
	then
		die "disk is mounted"
	fi
	echo $dev
}

prepare_disk() {
	local -r dev=$1

	parted -s
}

DISK_MODEL='Samsung SSD 850'
DISK_DEV=/dev/$(find_disk_dev "$DISK_MODEL")

echo DISK_DEV=$DISK_DEV
