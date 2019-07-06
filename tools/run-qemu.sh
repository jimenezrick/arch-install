#!/bin/bash

set -euo pipefail

DIR=$(cd $(dirname $0); pwd)

PATH=$DIR/mkosi:$PATH
mkosi qemu -virtfs local,path=$DIR/..,security_model=passthrough,mount_tag=arch-rebuild -nographic "$@"
