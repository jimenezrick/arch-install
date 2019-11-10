#!/bin/bash

set -euo pipefail

DIR=$(cd $(dirname $0); pwd)

to_dhall() {
	jq -R | jq -s | dhall-format
}

pacman -Qqen | to_dhall >$DIR/packages.dhall
pacman -Qqem | to_dhall >$DIR/aur.dhall
pacman -Qg | awk '{print $1}' | sort -u | to_dhall >$DIR/groups.dhall
