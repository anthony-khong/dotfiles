# Ubuntu packages, locale and firewall, sourced by install.sh

step "apt packages (setup/apt.txt)"
$SUDO apt-get update -qq
# shellcheck disable=SC2046 # one package per word
$SUDO env DEBIAN_FRONTEND=noninteractive apt-get install -y -qq \
  $(grep -Ev '^[[:space:]]*(#|$)' "$DOTFILES/setup/apt.txt")

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
