#!/bin/bash

set -euo pipefail

(cd /mnt/arch-rebuild/tools && ./arch-rebuild build-rootfs -c ../config)
