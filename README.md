# Anthony Khong's Dotfiles

Neovim, tmux, zsh and friends, kept the same across macOS and Ubuntu machines.

## New machine

```sh
git clone https://github.com/anthony-khong/dotfiles.git ~/dotfiles
~/dotfiles/install.sh
```

Then open a new terminal. On macOS, run `xcode-select --install` first; on Ubuntu you need sudo. The first run asks for the machine's name.

Supported: macOS, and Ubuntu 22.04 or newer (24.04 for new boxes). On 22.04, whose mosh is too old for OSC 52 copy and true colour, the installer builds mosh 1.4 from source.

- `./install.sh --with jvm,docker`: optional modules in `setup/modules/` (data, docker, gitlab-runner, jvm, mac-server, swap, tailscale)
- `./install.sh --skip packages`: skip steps, e.g. on a box without sudo
- `./install.sh --hostname NAME`: rename the machine
- `./install.sh --doctor`: what's installed and what needs fixing
- `./update.sh` (or `dotfiles-update`): updates everything without sudo; add `--plugins` to also bump Neovim plugins and commit the new `vim/lazy-lock.json`. Shells remind you after 14 days.

It's safe to re-run. When a step fails, the installer names it and exits non-zero: fix the problem and re-run, with `--skip` for the steps that are done.

## Layout

- `install.sh`, `setup/`: installer, Brewfile, apt list, modules
- `mise/`: languages and CLI tools, linked into `~/.config/mise/conf.d/`
- `vim/` (Neovim), `tmux/`, `ghostty/`, `elixir/` (IEx)
- `bash/`: `zshrc` and `bashrc` both load `bash_shortcuts.sh` (aliases and functions) and `bash_preferences.sh` (environment)

## Per-machine settings

These stay out of the repo and are loaded if present:

- `~/.config/local/shell.sh`
- `~/.config/local/tp-dirs`: more directories for `tp`, the fzf session picker (prefix f in tmux), one per line
- `~/.config/local/tmux.conf`
- `~/.config/local/nvim.lua`
- `~/.config/local/ghostty`, e.g. `font-size = 15`
- `~/.config/mise/config.toml`, which is what `mise use -g` edits
- `~/.ssh/config`: host aliases go above the include of `ssh/config`, which the link script adds at the end
