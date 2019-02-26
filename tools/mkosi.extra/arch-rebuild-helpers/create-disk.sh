#!/bin/sh

set -euo pipefail

# TODO: Rewrite in Haskell

dd if=/dev/zero of=/mnt/rootfs.img bs=1M count=256
mkfs -t btrfs /mnt/rootfs.img
mkdir -p /mnt/rootfs
mount -o loop /mnt/rootfs.img /mnt/rootfs
