#!/bin/bash

set -eu

DIR=$(cd $(dirname $0); pwd)

mkdir -p $DIR/../restore
sudo git -C /etc bundle create $DIR/../restore/etc.bundle --all
