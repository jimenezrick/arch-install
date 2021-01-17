#!/bin/bash

set -eu

DIR=$(cd $(dirname $0); pwd)

mkdir -p $DIR/../restore/aur

sudo git -C /etc bundle create $DIR/../restore/etc.bundle --all
$DIR/../arch-rebuild build-aur-packages -c $DIR/../config/system.dhall -d $DIR/../restore/aur
