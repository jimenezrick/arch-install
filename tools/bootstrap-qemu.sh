#!/bin/bash

set -e

echo Bootstrapping arch-rebuild from local repo...
mkdir -p /mnt/arch-rebuild
mount -t 9p -o trans=virtio,version=9p2000.L,rw arch-rebuild /mnt/arch-rebuild
/mnt/arch-rebuild/arch-rebuild build-arch               \
	-c /mnt/arch-rebuild/config/system.dhall        \
	-e /mnt/arch-rebuild/etc.bundle                 \
	-a /mnt/arch-rebuild/config/pacman/aur-packages
