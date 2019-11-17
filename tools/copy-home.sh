#!/bin/bash

set -euo pipefail

DIR=$(cd $(dirname $0); pwd)

rsync --archive --delete --info=flist2,progress2 $(realpath $1) $2
