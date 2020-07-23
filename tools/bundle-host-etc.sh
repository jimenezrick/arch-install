#!/bin/bash

set -eu

DIR=$(cd $(dirname $0); pwd)

sudo git -C /etc bundle create $DIR/../etc.bundle --all
