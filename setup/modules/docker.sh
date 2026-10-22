#!/usr/bin/env bash
# Docker Engine from Docker's apt repository (Debian and Ubuntu)
set -euo pipefail
# shellcheck source=../lib.sh
. "$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)/lib.sh"

if ! is_debian; then
  note "On macOS, install Docker Desktop or OrbStack yourself."
  exit 0
fi

if ! have docker; then
  $SUDO apt-get install -y -qq ca-certificates curl gnupg
  $SUDO install -m 0755 -d /etc/apt/keyrings
  distro="$(. /etc/os-release && echo "$ID")" # debian or ubuntu, as in Docker's URLs
  curl -fsSL "https://download.docker.com/linux/$distro/gpg" |
    $SUDO gpg --dearmor --yes -o /etc/apt/keyrings/docker.gpg
  $SUDO chmod a+r /etc/apt/keyrings/docker.gpg
  codename="$(. /etc/os-release && echo "$VERSION_CODENAME")"
  echo "deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker.gpg] https://download.docker.com/linux/$distro $codename stable" |
    $SUDO tee /etc/apt/sources.list.d/docker.list >/dev/null
  $SUDO apt-get update -qq
  $SUDO env DEBIAN_FRONTEND=noninteractive apt-get install -y -qq \
    docker-ce docker-ce-cli containerd.io docker-buildx-plugin docker-compose-plugin
fi

user="$(id -un)"
if [ "$user" != "root" ] && ! id -nG "$user" | grep -qw docker; then
  $SUDO usermod -aG docker "$user"
  note "Added $user to the docker group: log out and back in to use docker without sudo."
fi
