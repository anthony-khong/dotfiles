# Anthony Khong's Dotfiles

Neovim, tmux, zsh and friends, kept the same across macOS and Ubuntu machines.

## New machine

```sh
git clone https://github.com/anthony-khong/dotfiles.git ~/dotfiles
~/dotfiles/install.sh
```

Then open a new terminal. On macOS, run `xcode-select --install` first; on Ubuntu you need sudo.

- `./install.sh --with jvm,docker`: optional modules in `setup/modules/` (data, docker, gitlab-runner, jvm, swap)
- `./install.sh --skip packages`: skip steps, e.g. on a box without sudo
- `./install.sh --doctor`: what's installed and what needs fixing

It's safe to re-run.

## Layout

- `install.sh`, `setup/`: installer, Brewfile, apt list, modules
- `mise/`: languages and CLI tools, linked into `~/.config/mise/conf.d/`
- `vim/` (Neovim), `tmux/`, `bash/` (bash and zsh), `elixir/` (IEx)

## Per-machine settings

These stay out of the repo and are loaded if present:

- `~/.config/local/shell.sh`
- `~/.config/local/tmux.conf`
- `~/.config/local/nvim.lua`
- `~/.config/mise/config.toml`, which is what `mise use -g` edits
