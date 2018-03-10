#!/bin/bash

set -euo pipefail

umount -R /mnt
cryptsetup close cryptroot
