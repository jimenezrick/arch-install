#!/bin/bash

set -eu

DIR=$(cd $(dirname $0); pwd)

rsync --archive --delete --info=flist2,progress2 $(realpath $HOME) $1
