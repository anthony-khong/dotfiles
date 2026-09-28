#!/usr/bin/env bash
# For a Mac used headless over SSH and mosh (the Mac minis): checks Remote Login, keeps the
# Mac awake and restarting after power cuts, and lets mosh-server through the firewall.
set -euo pipefail
# shellcheck source=../lib.sh
. "$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)/lib.sh"

is_mac || die "the mac-server module is for macOS"

if nc -z 127.0.0.1 22 2>/dev/null; then
  note "Remote Login: on"
else
  note "Remote Login is off: turn it on in System Settings > General > Sharing, then re-run"
fi

# Never sleep, restart after a power cut, wake when something on the network asks
sudo pmset -a sleep 0 disksleep 0 autorestart 1 womp 1
note "power: $(pmset -g | awk '$1 ~ /^(sleep|autorestart|womp)$/ {printf "%s %s  ", $1, $2}')"

# With the firewall on, macOS asks before mosh-server may take connections, and nobody is
# there to click Allow. Approve it up front; re-run after `brew upgrade mosh` (new path).
firewall=/usr/libexec/ApplicationFirewall/socketfilterfw
if "$firewall" --getglobalstate | grep -q enabled; then
  have mosh-server || die "mosh-server not found: run the packages step first"
  mosh_server="$(realpath "$(command -v mosh-server)")"
  sudo "$firewall" --add "$mosh_server" >/dev/null
  sudo "$firewall" --unblockapp "$mosh_server" >/dev/null
  note "firewall: allowed $mosh_server"
else
  note "firewall: off, nothing to allow"
fi

# macOS 26+ on Apple silicon takes the FileVault password over SSH at the unlock screen.
# Only from the local network: Tailscale lives on the locked disk, so it starts afterwards.
if fdesetup status | grep -q "is On"; then
  note "FileVault is on: after a restart or power cut the Mac waits at its unlock screen, off"
  note "Tailscale. From its own network (Ethernet is the most reliable): ssh <user>@<name>.local,"
  note "give the password, and reconnect once it disconnects. Before macOS 26 it needs a keyboard."
fi
