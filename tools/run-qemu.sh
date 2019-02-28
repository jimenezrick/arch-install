#!/bin/sh

set -euo pipefail

DIR=$(cd $(dirname $0); pwd)

mkosi qemu -virtfs local,path=$DIR/..,security_model=passthrough,mount_tag=arch-rebuild -nographic "$@"
