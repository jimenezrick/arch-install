#!/bin/bash

set -euo pipefail

sudo mkosi --default mkosi.arch build |& tee mkosi.log
