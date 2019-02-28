#!/bin/bash

set -euo pipefail

DIR=$(cd $(dirname $0); pwd)

steps=(
	configure-net.sh
	mount-virtfs.sh
)

for s in ${steps[@]}
do
	echo "> Running $s:"
	source $DIR/$s
done
