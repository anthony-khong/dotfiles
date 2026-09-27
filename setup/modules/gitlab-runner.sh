#!/usr/bin/env bash
# A GitLab runner in Docker (Ubuntu only; needs the docker module)
set -euo pipefail
# shellcheck source=../lib.sh
. "$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)/lib.sh"

is_ubuntu || die "the gitlab-runner module is for Ubuntu"
have docker || die "needs Docker: ./install.sh --with docker,gitlab-runner"

# The docker group only applies after logging in again, so fall back to sudo
docker_cmd="docker"
docker info >/dev/null 2>&1 || docker_cmd="$SUDO docker"

if $docker_cmd ps -a --format '{{.Names}}' | grep -qx gitlab-runner; then
  note "gitlab-runner container already exists"
else
  $docker_cmd volume create gitlab-runner-config >/dev/null
  $docker_cmd run -d --name gitlab-runner --restart always \
    -v /var/run/docker.sock:/var/run/docker.sock \
    -v gitlab-runner-config:/etc/gitlab-runner \
    gitlab/gitlab-runner:latest
fi
note "To register it (once), with the URL and token from GitLab:"
note "  docker run --rm -it -v gitlab-runner-config:/etc/gitlab-runner gitlab/gitlab-runner:latest register"
