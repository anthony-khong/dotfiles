#!/usr/bin/env bash
# JVM tools for Spark work (mise/jvm.toml): Temurin 21, Maven, sbt, Clojure CLI, Babashka
set -euo pipefail
# shellcheck source=../lib.sh
. "$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)/lib.sh"

link mise/jvm.toml "$CONFIG/mise/conf.d/jvm.toml"
cd "$HOME" && mise install --yes
