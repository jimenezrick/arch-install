#!/bin/bash

set -euo pipefail

cat <<EOF
title   Arch Linux LTS
linux   /vmlinuz-linux-lts
initrd  /amd-ucode.img
initrd  /initramfs-linux-lts.img
options root=/dev/mapper/cryptroot rootflags=subvol=@ luks.uuid=${1} luks.name=${1}=cryptroot rw iommu=soft
EOF
