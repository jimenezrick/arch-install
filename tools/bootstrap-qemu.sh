#!/bin/bash

set -e

mkdir /mnt/arch-rebuild
mount -t 9p -o trans=virtio,version=9p2000.L,rw arch-rebuild /mnt/arch-rebuild
cd /mnt/arch-rebuild
./arch-rebuild build-arch -c config/system.dhall
