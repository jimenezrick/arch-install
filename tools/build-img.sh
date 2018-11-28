#!/bin/bash

set -euo pipefail

mkosi --default mkosi.arch build |& tee mkosi.log
