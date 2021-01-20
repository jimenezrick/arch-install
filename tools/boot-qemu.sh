#!/bin/bash

set -e

DIR=$(cd $(dirname $0); pwd)

source $DIR/qemu.conf

# https://wiki.gentoo.org/wiki/QEMU/Options#Hard_drive
run_qemu() {
	qemu-system-x86_64 \
		-machine q35,accel=kvm -cpu host \
		-smp 2 -m 2G \
		-drive if=pflash,format=raw,readonly,file=/usr/share/edk2-ovmf/x64/OVMF_CODE.fd \
		-virtfs local,path=$DIR/..,security_model=passthrough,mount_tag=arch-rebuild \
		"$@"
		# -nographic (boot kernel with `console=ttyS0', edit boot entry with `e')
}

case $1 in
	iso)
		shift
		run_qemu \
			-device virtio-rng-pci \
			-device virtio-scsi-pci,id=scsi0 \
			-drive file=$DISK,if=none,format=raw,discard=unmap,aio=native,cache=none,id=hd0 \
			-device scsi-hd,drive=hd0,bus=scsi0.0 \
			-cdrom $DIR/$ISO \
			-boot order=d \
			"$@"
		;;
	disk)
		shift
		run_qemu \
			-device virtio-rng-pci \
			-device virtio-scsi-pci,id=scsi0 \
			-drive file=$DISK,if=none,format=raw,discard=unmap,aio=native,cache=none,id=hd0 \
			-device scsi-hd,drive=hd0,bus=scsi0.0 \
			-boot order=c \
			"$@"
		;;
	*)
		echo "Usage: $0 iso|disk [<extra_args>]"
		;;
esac
