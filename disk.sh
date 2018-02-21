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
	local model=$1
	local dev=$(lsblk -S -o kname,model | awk "/$model/"'{print $1}')

	if mount | grep /dev/$dev >/dev/null
	then
		die "disk is mounted"
	fi
	echo $dev
}

prepare_disk() {
	local dev=$1

	announce Preparing $dev
	parted -s $dev -a optimal \
		mklabel gpt \
		mkpart primary 0% 513MiB \
		mkpart primary 513MiB 100% \
		set 1 boot on

	local dev_esp=${dev}1
	local dev_rootfs=${dev}2

	announce Formatting ESP $dev_esp
	mkfs.fat -F32 $dev_esp

	announce Formatting encrypted rootfs $dev_rootfs
	cryptsetup -y -v luksFormat --type luks2 $dev_rootfs
	cryptsetup open $dev_rootfs cryptroot
	mkfs.btrfs -f /dev/mapper/cryptroot
}

DISK_MODEL='Samsung SSD 850'
DISK_DEV=/dev/$(find_disk_dev "$DISK_MODEL")

prepare_disk $DISK_DEV
