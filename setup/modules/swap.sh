#!/usr/bin/env bash
# A swap file at /swapfile (Ubuntu only). Size: SWAP_SIZE=16G ./install.sh --with swap
set -euo pipefail
# shellcheck source=../lib.sh
. "$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)/lib.sh"

is_ubuntu || die "the swap module is for Ubuntu"
if [ -e /swapfile ]; then
  note "/swapfile already exists; leaving it alone"
  exit 0
fi

$SUDO fallocate -l "${SWAP_SIZE:-32G}" /swapfile
$SUDO chmod 600 /swapfile
$SUDO mkswap /swapfile >/dev/null
$SUDO swapon /swapfile
grep -q '^/swapfile ' /etc/fstab ||
  echo '/swapfile none swap sw 0 0' | $SUDO tee -a /etc/fstab >/dev/null
note "swap on: $(swapon --show=NAME,SIZE --noheadings | tr -s ' ')"
