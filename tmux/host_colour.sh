#!/bin/sh
# Prints the tmux colour for this machine's status pills. The laptop keeps colour18; a box
# reached over SSH or mosh gets a colour picked from its hostname, so it's the same every
# time and differs between boxes. Override per machine in ~/.config/local/tmux.conf:
#   set -g @host_colour colour94
if [ -z "$SSH_CONNECTION" ]; then
  echo colour18
  exit
fi
# Dark enough for white text, and none of them red (the prefix) or blue (the current window)
set -- colour22 colour23 colour24 colour53 colour58 colour60 colour94 colour130
n=$(($(hostname -s | cksum | cut -d' ' -f1) % $# + 1))
eval "echo \${$n}"
