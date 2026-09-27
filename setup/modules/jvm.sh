#!/usr/bin/env bash
# JVM tools for Spark work (mise/jvm.toml): Temurin 21, Maven, sbt, Clojure CLI, Babashka,
# coursier and clojure-lsp; plus Metals and jdtls, which mise doesn't carry.
# To upgrade Metals or jdtls, change its version below and re-run.
set -euo pipefail
# shellcheck source=../lib.sh
. "$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)/lib.sh"

METALS_VERSION=1.6.9
# jdtls downloads carry a build timestamp: https://download.eclipse.org/jdtls/snapshots/
JDTLS_VERSION=1.61.0
JDTLS_BUILD=202609031315

link mise/jvm.toml "$CONFIG/mise/conf.d/jvm.toml"
cd "$HOME" && mise install --yes

bin="$HOME/.local/bin"
share="${XDG_DATA_HOME:-$HOME/.local/share}"
mkdir -p "$bin"

# keep_only <dir> <version>: removes the other versions under <dir>
keep_only() { find "$1" -mindepth 1 -maxdepth 1 ! -name "$2" -exec rm -rf {} +; }

# Metals: a coursier launcher; it fetches its jars into coursier's cache on first start
metals_dir="$share/metals/$METALS_VERSION"
if [ ! -x "$metals_dir/metals" ]; then
  note "Metals $METALS_VERSION"
  mkdir -p "$metals_dir"
  cs bootstrap --java-opt -Xss4m --java-opt -Xms100m \
    "org.scalameta:metals_2.13:$METALS_VERSION" -o "$metals_dir/metals" -f
fi
ln -sfn "$metals_dir/metals" "$bin/metals"
keep_only "$share/metals" "$METALS_VERSION"

# jdtls: Eclipse's tarball. A wrapper rather than a link, so its bin/jdtls finds its own files
jdtls_dir="$share/jdtls/$JDTLS_VERSION"
if [ ! -x "$jdtls_dir/bin/jdtls" ]; then
  note "jdtls $JDTLS_VERSION"
  rm -rf "$jdtls_dir.part" && mkdir -p "$jdtls_dir.part"
  curl -fsSL "https://download.eclipse.org/jdtls/snapshots/jdt-language-server-$JDTLS_VERSION-$JDTLS_BUILD.tar.gz" |
    tar -xz -C "$jdtls_dir.part"
  rm -rf "$jdtls_dir" && mv "$jdtls_dir.part" "$jdtls_dir"
fi
rm -f "$bin/jdtls"
printf '#!/bin/sh\nexec "%s/bin/jdtls" "$@"\n' "$jdtls_dir" >"$bin/jdtls"
chmod +x "$bin/jdtls"
keep_only "$share/jdtls" "$JDTLS_VERSION"
