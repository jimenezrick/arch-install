#!/bin/sh

set -euo pipefail

dhcpcd $(ip link show | awk '/^2:/ { print gensub(/:/, "", "g", $2) }')
(curl -s www.google.com >/dev/null && echo Network up) || echo Network not ready
