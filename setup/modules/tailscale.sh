#!/usr/bin/env bash
# Tailscale, so the laptop, the Linux boxes and the Mac minis reach each other by name from
# anywhere, with no port forwarding. Plain sshd and mosh run over it.
set -euo pipefail
# shellcheck source=../lib.sh
. "$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)/lib.sh"

if is_mac; then
  if [ -d /Applications/Tailscale.app ]; then
    note "The Tailscale app is installed, so leaving it in charge. On a headless Mac mini,"
    note "uninstall the app and re-run: the app only starts once someone logs in."
    exit 0
  fi
  # Homebrew's open-source daemon starts at boot, before anyone logs in
  have tailscale || brew install tailscale
  if ! sudo brew services list | grep -Eq '^tailscale +started'; then
    sudo brew services start tailscale
  fi
else
  have tailscale || curl -fsSL https://tailscale.com/install.sh | sh
fi

if tailscale status >/dev/null 2>&1; then
  note "on your tailnet as $(tailscale ip -4 | head -1)"
else
  note "Log in by opening the URL below, e.g. on your laptop"
  sudo tailscale up
fi
note "For an always-on box, turn off its key expiry in the admin console"
note "(Machines > … next to it > Disable key expiry), or it drops off after 180 days."
