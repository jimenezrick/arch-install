#!/bin/bash

set -euo pipefail

DIR=$(cd $(dirname $0); pwd)

PATH=$DIR/mkosi:$PATH
mkosi --default mkosi.arch build |& tee mkosi.log
