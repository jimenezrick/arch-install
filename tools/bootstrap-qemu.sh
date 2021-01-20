#!/bin/bash

set -e

pacman --noconfirm -Sy git

echo Bootstrapping arch-rebuild from local repo...
mkdir -p /mnt/arch-rebuild
mount -t 9p -o trans=virtio,version=9p2000.L,msize=$((128 * 1024 * 1024)),rw arch-rebuild /mnt/arch-rebuild
/mnt/arch-rebuild/arch-rebuild build-arch               \
	-c /mnt/arch-rebuild/config/system.dhall        \
	-e /mnt/arch-rebuild/restore/etc.bundle         \
	-a /mnt/arch-rebuild/restore/aur
