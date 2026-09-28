# Ubuntu packages, locale and firewall, sourced by install.sh

# mosh 1.4+ carries OSC 52 copies and true colour. Where apt's mosh is older (1.3 on 22.04),
# this release is built into /usr/local instead. To upgrade: bump both lines, re-run install.sh.
MOSH_VERSION=1.4.0
MOSH_SHA256=872e4b134e5df29c8933dff12350785054d2fd2839b5ae6b5587b14db1465ddd

step "apt packages (setup/apt.txt)"
$SUDO apt-get update -qq
packages="$(grep -Ev '^[[:space:]]*(#|$)' "$DOTFILES/setup/apt.txt")"
apt_mosh="$(apt-cache policy mosh | awk '/Candidate:/ {print $2}')"
build_mosh=0
if ! at_least "$apt_mosh" 1.4; then
  build_mosh=1
  packages="$(echo "$packages" | grep -vx mosh)"
fi
# shellcheck disable=SC2086 # one package per word
$SUDO env DEBIAN_FRONTEND=noninteractive apt-get install -y -qq $packages

if [ "$build_mosh" -eq 1 ]; then
  step "mosh $MOSH_VERSION from source (apt has $apt_mosh)"
  if /usr/local/bin/mosh-server --version 2>/dev/null | grep -q "(mosh $MOSH_VERSION)"; then
    note "already in /usr/local/bin"
  else
    $SUDO env DEBIAN_FRONTEND=noninteractive apt-get install -y -qq \
      pkg-config protobuf-compiler libprotobuf-dev zlib1g-dev libutempter-dev
    mosh_src="$(mktemp -d)"
    curl -fsSL -o "$mosh_src/mosh.tar.gz" \
      "https://github.com/mobile-shell/mosh/releases/download/mosh-$MOSH_VERSION/mosh-$MOSH_VERSION.tar.gz"
    echo "$MOSH_SHA256  $mosh_src/mosh.tar.gz" | sha256sum -c --quiet - ||
      die "mosh-$MOSH_VERSION.tar.gz doesn't match its checksum"
    tar -xzf "$mosh_src/mosh.tar.gz" -C "$mosh_src"
    note "building, about a minute"
    (cd "$mosh_src/mosh-$MOSH_VERSION" && ./configure && make -j"$(nproc)") >"$mosh_src/build.log" 2>&1 ||
      die "mosh didn't build: see $mosh_src/build.log"
    $SUDO make -C "$mosh_src/mosh-$MOSH_VERSION" install >/dev/null
    rm -rf "$mosh_src"
    note "installed $(/usr/local/bin/mosh-server --version 2>/dev/null | head -1)"
  fi
  # One mosh on the box: /usr/local/bin comes first on PATH, but apt's would still confuse
  if dpkg-query -W -f='${Status}' mosh 2>/dev/null | grep -q 'ok installed'; then
    note "removing apt's mosh $apt_mosh"
    $SUDO apt-get remove -y -qq mosh >/dev/null
  fi
fi

step "en_US.UTF-8 locale (mosh needs a UTF-8 locale on both ends)"
if locale -a 2>/dev/null | grep -qiE '^en_US\.utf-?8$'; then
  note "already there"
else
  $SUDO locale-gen en_US.UTF-8
fi

if have ufw && $SUDO ufw status 2>/dev/null | grep -q '^Status: active'; then
  step "Opening mosh's UDP ports in ufw"
  $SUDO ufw allow 60000:61000/udp
fi
