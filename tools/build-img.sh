#!/bin/sh

set -euo pipefail

mkosi --default mkosi.arch build |& tee mkosi.log
