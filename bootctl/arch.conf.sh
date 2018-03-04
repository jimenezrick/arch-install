#!/bin/bash

set -euo pipefail

cat <<EOF
title   Arch Linux
linux   /vmlinuz-linux
initrd  /initramfs-linux.img
options root=/dev/mapper/cryptroot rootflags=subvol=@ luks.uuid=${1} luks.name=${1}=cryptroot rw iommu=soft systemd.legacy_systemd_cgroup_controller=true
EOF
