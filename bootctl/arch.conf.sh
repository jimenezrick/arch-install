#!/bin/bash

set -euo pipefail

cat <<EOF
title   Arch Linux
linux   /vmlinuz-linux
initrd  /amd-ucode.img
initrd  /initramfs-linux.img
options root=/dev/mapper/cryptroot rootflags=subvol=@ luks.uuid=${1} luks.name=${1}=cryptroot rw iommu=soft
EOF
