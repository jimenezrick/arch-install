#!/bin/bash

set -e

DIR=$(cd $(dirname $0); pwd)

source $DIR/qemu.conf

torrent "https://mirror.rackspace.com/archlinux/iso/latest/${ISO}.torrent"
