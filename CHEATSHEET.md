# Cheatsheet

What these dotfiles set up, plus the built-in keys worth remembering. Cmd-F for the thing you half remember.

Notation: `C-x` is Ctrl-x, `M-x` is Alt-x (left Option), `S-` is Shift. **prefix** is tmux's `C-a`. **leader** in Neovim is `Space` (so is localleader). Keys are the characters you type; Ghostty's Cmd shortcuts sit where their letter is on QWERTY.

**Contents:** [Dotfiles](#dotfiles) · [Ghostty](#ghostty) · [tmux](#tmux) · [Shell](#shell) · [SSH, mosh and Tailscale](#ssh-mosh-and-tailscale) · [Neovim](#neovim) · [Snippets](#snippets) · [Elixir and IEx](#elixir-and-iex) · [mise](#mise)

## Dotfiles

| Command | What it does |
|---|---|
| `~/dotfiles/install.sh` | Set up this machine; safe to re-run. The first run asks for the machine's name |
| `./install.sh --with jvm,docker` | Plus modules: `data` `docker` `gitlab-runner` `jvm` `mac-server` `swap` `tailscale` |
| `./install.sh --skip packages,rust` | Skip steps: `packages` `link` `mise` `rust` `zsh` `nvim` |
| `./install.sh --hostname NAME` | Rename this machine |
| `./install.sh --doctor` | What's installed and what needs fixing |
| `dotfiles-update` | `update.sh`: pull, relink, update mise tools, Rust, oh-my-zsh, tmux plugins, Homebrew; Neovim plugins at `vim/lazy-lock.json` |
| `dotfiles-update --plugins` | Also move Neovim plugins to their newest versions; then commit `vim/lazy-lock.json` |
| `recreate_symbolic_links` | Relink the configs into `$HOME` (also compiles Ghostty's terminfo) |
| `sbash` | Reload the shell config |
| `ghostty_terminfo_to HOST...` | Copy Ghostty's terminfo to a box without these dotfiles |
| `infocmp -x xterm-ghostty > ghostty/xterm-ghostty.terminfo` | After a Ghostty upgrade, on the laptop |

Per-machine settings, kept out of the repo:

| File | For |
|---|---|
| `~/.config/local/shell.sh` | Shell settings (Spark's variables, `DOTFILES_UPDATE_DAYS` for the update reminder) |
| `~/.config/local/tp-dirs` | More directories for `tp`, one per line |
| `~/.config/local/tmux.conf` | e.g. `set -g @host_colour colour94` |
| `~/.config/local/nvim.lua` | Neovim, loaded last |
| `~/.config/local/ghostty` | e.g. `font-size = 15` |
| `~/.config/mise/config.toml` | Tools for this machine only (`mise use -g`) |
| `~/.ssh/config` | Host aliases, above the include of the shared `ssh/config` |

## Ghostty

macOS. On Linux Ghostty keeps its defaults (`C-S-t` new tab, `C-S-w` close, `C-S-c` / `C-S-v` copy and paste).

| Keys | What it does |
|---|---|
| `C-Space` | Show or hide Ghostty, from any app |
| `Cmd-Enter` | Full screen, in its own Space |
| `Cmd-T` / `Cmd-W` / `Cmd-N` | New tab / close / new window |
| `Cmd-Shift-[` / `Cmd-Shift-]`, `Cmd-1`…`Cmd-9` | Previous / next tab, tab N |
| `Cmd-D` / `Cmd-Shift-D` | Split right / down; `Cmd-[` / `Cmd-]` move between splits |
| `Cmd-K` | Clear the screen |
| `Cmd-F`, `Cmd-G` / `Cmd-Shift-G` | Search; next / previous match |
| `Cmd-=` / `Cmd--` / `Cmd-0` | Font bigger / smaller / reset |
| `Cmd-,` / `Cmd-Shift-,` | Open / reload the config |
| `Cmd-Shift-P` | Command palette |
| `Shift`-drag, in tmux | Ghostty's own selection, for `Cmd-C` (a plain drag is tmux's) |
| Left `Option` | Alt, for `M-` keys; right Option still types é, ñ, ¿ |

## tmux

prefix is `C-a` (`prefix C-a` sends a real `C-a`).

| Keys | What it does |
|---|---|
| `C-b` / `C-f` | Split right / below, no prefix |
| `prefix v` / `prefix h` | Split right at 40% / below at 25% |
| `C-h` `C-j` `C-k` `C-l` | Move between panes and Neovim splits, no prefix; `C-\` previous pane |
| `prefix C-l` | Clear the screen (plain `C-l` moves right) |
| `prefix f` | `tp` in a popup: pick a session or a directory in `~/repos` |
| `prefix a` | Last window |
| `prefix C-p` / `prefix C-n` | Previous / next window |
| `prefix G` | New session |
| `prefix s` | Type into every pane at once (red **S** in the status line); again to stop |
| `prefix P` | Popup shell in the current directory |
| `prefix r` | Reload `~/.tmux.conf` |

tmux defaults still in use:

| Keys | What it does |
|---|---|
| `prefix c` / `prefix ,` / `prefix &` | New window / rename it / kill it |
| `prefix $` | Rename the session |
| `prefix d` | Detach |
| `prefix z` | Zoom the pane (**[Z]** in the status line) |
| `prefix x` | Kill the pane |
| `prefix w` | Pick a window or session from a tree |
| `prefix 0`…`prefix 9` | Window N |
| `prefix !` | Pane into its own window |
| `prefix q` | Show pane numbers |
| `prefix {` / `prefix }` | Swap the pane with the previous / next |
| `prefix Space` | Cycle layouts |
| `prefix :` | Command prompt |
| `prefix ?` | List every key |

Copy mode (`prefix [`, or scroll up with the mouse):

| Keys | What it does |
|---|---|
| `v` / `C-v` / `y` | Start a selection / rectangle / copy it (reaches the laptop's clipboard, also over SSH and mosh) |
| mouse drag | Select; copies when you let go |
| `/` / `?`, `n` / `N` | Search down / up, next / previous |
| `q` | Leave copy mode |
| `o` / `C-o` / `S` | With a selection: open it / open it in `$EDITOR` / search the web for it |
| `prefix ]` | Paste tmux's last copy |

Sessions and plugins:

| Keys | What it does |
|---|---|
| `prefix g` | Go to a session by name |
| `prefix C` | New session, asks for a name |
| `prefix X` | Kill this session, without leaving tmux |
| `prefix S` | Last session |
| `prefix @` | Move this pane into a new session |
| `prefix m`, then `prefix t` + `h` / `v` / `f` | Mark a pane, then join it here beside / below / full |
| `prefix C-s` / `prefix C-r` | Save every session / restore them (tmux-resurrect) |
| `prefix I` / `prefix U` / `prefix M-u` | Install / update / remove plugins (tpm) |
| `tmux kill-server` | Stop tmux, e.g. after an upgrade: save with `prefix C-s` first, restore with `prefix C-r` |

The status line: the session pill turns red while prefix is pressed; pills take this machine's colour.

## Shell

zsh with oh-my-zsh everywhere; the aliases and functions work in bash too.

| Keys or command | What it does |
|---|---|
| `tp` | Pick a tmux session or a directory in `~/repos` (fzf); attaches outside tmux, switches inside |
| `tp DIR` / `tnew` | The session for DIR / for the current directory, created if needed |
| `mnd` / `pln` | Sessions for `~/Dropbox/mind_diary` / its `Planner` |
| `C-r` | Search history (fzf) |
| `C-t` | Insert file paths (fzf) |
| `M-c` | cd into a subdirectory (fzf) |
| `**` then `Tab` | fzf completion, e.g. `vim **`, `cd **`, `kill -9 **` |
| `→` or `End` / `M-f` | Accept the grey suggestion / one word of it |
| `take DIR` | mkdir and cd |
| `-` / `...` / `....` | Back to the previous directory / up two / up three |
| `d`, then `1`…`9` | List recent directories, jump to one |
| `md DIR` | `mkdir -p` |
| `l` / `ll` / `la` | `ls -lah` / `ls -lh` / `ls -lAh` |

In fzf: `C-n` / `C-p` (or `C-j` / `C-k`) move, `Tab` marks several, `Enter` picks, `Esc` leaves.

Git (oh-my-zsh aliases; `alias | grep git` for the rest):

| Alias | Git |
|---|---|
| `gst` / `gss` / `gsb` | `status` / `status --short` / `status --short --branch` |
| `ga` / `gaa` / `gapa` | `add` / `add --all` / `add --patch` |
| `gc` / `gcmsg "msg"` / `gc!` / `gcan!` | `commit --verbose` / `commit -m` / amend / amend all, same message |
| `gco` / `gcb` / `gsw` / `gswc` / `gswm` | `checkout` / `checkout -b` / `switch` / `switch --create` / switch to main |
| `gd` / `gds` | `diff` / `diff --staged` |
| `gl` / `gp` / `gpf` / `gpsup` | `pull` / `push` / push `--force-with-lease` / push and set upstream |
| `glog` / `glol` / `glola` | Graph logs: one line / with authors and dates / all branches |
| `grb` / `grbi` / `grbc` / `grba` / `grbm` | `rebase` / interactive / continue / abort / onto main |
| `gsta` / `gstp` / `gstl` | `stash push` / `stash pop` / `stash list` |
| `gcp` / `gm` / `gf` / `gfa` | `cherry-pick` / `merge` / `fetch` / fetch all and prune |
| `gb` / `gba` / `gbd` | `branch` / all / delete |
| `grs` / `grst` / `grh` / `grhh` | `restore` / `restore --staged` / `reset` / `reset --hard` |
| `gwip` / `gunwip` | Commit everything as WIP / undo that commit |

Aliases and functions (`bash/bash_shortcuts.sh`):

| Command | What it does |
|---|---|
| `vim FILES` | Neovim, one tab per file (`vi` is plain Vim) |
| `ppj` | Pretty-print JSON from stdin |
| `uuid` | Print a random UUID |
| `hh` | `history` |
| `purge_py` (or `remove_pyc`) | Delete `__pycache__` directories and `.pyc` / `.pyo` files below here |
| `iexmem` | `iex -S mix`, with mimalloc giving memory back sooner |
| `bb_nrepl` | Babashka nREPL on port 4444 (`<Space>cb` in Clojure connects) |
| `md_to_pdf CM IN.md OUT.pdf` | pandoc to PDF with CM cm margins |
| `auto_md_to_pdf FILE.md` | Rebuild FILE.pdf on every save (entr) |
| `tar_enc DIR` / `dec_tar DIR.secured` | tar and AES-256 encrypt / decrypt and untar here |
| `trim_image FILE` | Trim an image's borders (ImageMagick) |
| `rctags` | ctags into `.git/tags` |
| `check_my_ip` | Public IP address |
| `disable_git_status` / `enable_git_status` | Hide / show git status in the prompt, for a slow repo |
| `gitlab-runner ARGS` | gitlab-runner in Docker (the gitlab-runner module), e.g. `gitlab-runner register` |
| `flush_dns_cache`, `renew` | macOS: flush DNS / new DHCP lease on en0 |
| `x86` / `arm`, `rosetta-brew` | macOS: a login shell under Rosetta / native; Intel Homebrew |

In bash (rarely needed): `Tab` cycles through completions, `C-f` lists them.

## SSH, mosh and Tailscale

| Command | What it does |
|---|---|
| `ssh HOST` | One connection per host, reused for 10 minutes by ssh, scp, git and mosh |
| `ssh -O exit HOST` | Drop a stale shared connection, e.g. after the laptop slept |
| `mosh HOST` | Roaming shell; needs mosh 1.4 on both ends for copying and true colour |
| `mosh HOST -- tmux new -As main` | Straight into a tmux session |
| `C-^ .` | Quit a frozen mosh session |
| `tailscale status` / `tailscale ip -4` | Machines on the tailnet / this machine's address |
| `sudo tailscale up` | Log this machine in |
| `ssh USER@NAME.local` | Mac mini after a power cut, from its own network: unlocks FileVault, then reconnect |
| `~/dotfiles/scripts/auto-sync --source DIR --remote USER@HOST` | Copy DIR to the remote, then again on every change (`--port`, `--destination "~/dir"`) |

Copying on a remote box: a yank in Neovim or a copy in tmux lands on the laptop's clipboard. `p` pastes Neovim's own last yank; `Cmd-V` pastes the laptop's clipboard.

## Neovim

Not here? `:Telescope keymaps` searches every mapping.

| Keys | What it does |
|---|---|
| `C-s` / `C-x` | Save / close the window |
| `(` / `)` | Previous / next tab |
| `<Space>sv` / `<Space>sh` | Split right / below |
| `C-h` `C-j` `C-k` `C-l` | Move between splits and tmux panes |
| `<Space>nh` | Clear the search highlight (it also clears itself on insert or when idle) |
| `<Space>rw` | Remove trailing whitespace |
| `<Space>x` | Mark a list item done: its bullet becomes `* [X]` |
| `<Space>ss` | Spell check on / off |
| `<` / `>` in visual | Indent, staying in visual mode |
| `gx` | Open the URL or path under the cursor |
| `[<Space>` / `]<Space>` | Empty line above / below |

### Files and search

| Keys | What it does |
|---|---|
| `<Space>ff` / `<Space>fg` / `<Space>fb` / `<Space>fh` | Find files / grep / buffers / help (Telescope) |
| `<Space>fs` | Symbols in the workspace (LSP) |
| In Telescope: `C-v` / `C-x` / `C-t` | Open in a vertical split / split / tab |
| In Telescope: `C-q` / `Tab`, `M-q` | All results to the quickfix list / mark some, send those |
| In Telescope: `C-u` / `C-d`, `C-/` | Scroll the preview; list Telescope's keys |
| `<Space>tt` / `<Space>tf` | File tree on / off; show the current file in it |
| `<Space>tr` / `<Space>tc` / `<Space>tT` | Tree: refresh / collapse / new tab with a tree |
| In the tree: `a` / `r` / `d` | Create (end with `/` for a directory) / rename / delete |
| In the tree: `x` / `c` / `p` | Cut / copy / paste |
| In the tree: `y` / `Y` / `gy` | Copy the name / relative path / absolute path |
| In the tree: `H` / `I` / `f` | Show dotfiles / git-ignored files; filter |
| In the tree: `-` / `C-]` / `E` / `W` | Up a directory / into one / expand all / collapse all |
| In the tree: `C-v` / `C-x` / `C-t`, `g?` | Open in a split or tab; every key |
| `]q` / `[q` | Next / previous quickfix entry (`]l` / `[l` for the location list) |

### Editing

| Keys | What it does |
|---|---|
| `ysiw)` / `yss"` / `cs"'` / `ds(` | Surround a word with `()` / the line with `""` / change `"` to `'` / delete `()` |
| `S"` in visual | Surround the selection |
| `gaip=` / `ga=` in visual / `ga*,` | Align a paragraph on `=` / the selection / every `,` (easy-align) |
| `gS` / `gJ` | Split a one-liner over lines / join it back (splitjoin) |
| `gcc` / `gc{motion}` / `gc` in visual | Toggle comments |
| `af` / `if` | A function / its body, e.g. `daf`, `vif` |
| `ac` / `ic` | A class or module / its body |
| `aa` / `ia` | A parameter with / without its comma, e.g. `cia` |
| `an` / `in` in visual | Grow / shrink the selection by syntax node; `]n` / `[n` next / previous node |
| `u` / `C-r`, `:Undotree` | Undo / redo, kept across restarts; the undo tree |
| `.` | Repeats surround and friends too |
| `Enter` after `do` or `fn` | Adds the matching `end` in Elixir (likewise in Lua and shell scripts) |

### LSP and diagnostics

| Keys | What it does |
|---|---|
| `gd` / `gD` / `gi` / `<Space>D` | Definition / declaration / implementation / type definition |
| `gr` | References |
| `K` | Docs for what's under the cursor |
| `<Space>rn` / `<Space>ca` | Rename / code action |
| `<Space>fm` | Format the buffer |
| `C-s` in insert | Signature help |
| `gO` | Symbols in this file |
| `]d` / `[d`, `]D` / `[D` | Next / previous diagnostic, last / first |
| `<Space>e` (or `C-w d`) | The diagnostics under the cursor |
| `<Space>q` | All diagnostics into the location list |
| `<Space>wa` / `<Space>wr` / `<Space>wl` | Add / remove / list workspace folders |
| `:checkhealth vim.lsp` | Which servers are attached |

Formatting on save: Elixir (Expert), Python (ruff), Rust (rust-analyzer, which also runs clippy), Bash (shfmt). Everything else: `<Space>fm`.

Servers: Elixir Expert + Credo · Python pyrefly + ruff · Rust rust-analyzer · Bash bash-language-server · YAML yaml-language-server · TOML tombi · TypeScript ts_ls · SQL sql-language-server · Clojure clojure-lsp, Scala Metals, Java jdtls (with `--with jvm`).

### Completion and snippets

| Keys | What it does |
|---|---|
| `C-n` / `C-p` | Next / previous suggestion |
| `C-y` / `C-e` | Accept / close the menu |
| `Tab` / `S-Tab` | Scroll the docs beside the menu |
| `Tab` after a snippet prefix | Expand the snippet ([list below](#snippets)); then `C-n` / `C-p` jump between its fields |

Menu marks: `λ` language server, `⋗` snippet, `b` buffer, `p` path.

### Git

| Keys or command | What it does |
|---|---|
| `:G` | Status. In it: `-` stage / unstage, `=` inline diff, `cc` commit, `ca` amend, `dv` diff split, `X` discard, `g?` every key |
| `<Space>gb` / `<Space>gd` | Blame / diff the file against the index |
| `:Gread` / `:Gwrite` | Reset the file to the index / stage it |
| `:Gvdiffsplit BRANCH` | Diff against another branch |
| Sign column | `+` added, `~` changed, `_` deleted (`:Gitsigns blame_line` for the line's commit) |

### Spell check

On in markdown and git commits; `<Space>ss` elsewhere. A word passes in English, Indonesian or Spanish. With treesitter, only prose, comments and strings get checked.

| Keys | What it does |
|---|---|
| `]s` / `[s` | Next / previous misspelling |
| `z=` | Suggestions |
| `zg` / `zug` | Add the word / take it back (this machine only: `~/.local/share/nvim/site/spell/en.utf-8.add`) |
| `zw` | Mark a word as wrong |

### Crashes and swap files

| You see | Do |
|---|---|
| `E325: ATTENTION` after a crash, power cut or `tmux kill-server` | `r` recovers the unsaved edits; check, then `:w`. If it asks again next time, `d` deletes the old swap file |
| `W325: Ignoring swapfile from Nvim process` | The file is open in another Neovim too |
| `nvim -r` | List swap files |

### REPLs

| Keys | What it does |
|---|---|
| `C-c C-c` | Send the paragraph, or the selection, to tmux pane 1 of window 0 (vim-slime) |
| `C-c v` | Choose another target pane |
| `<Space>cc` in Python | Send the `# %%` cell under the cursor (IPython) |

In Elixir, a multi-line pipe goes over wrapped in `( )`, so iex takes it whole.

Clojure (Conjure). In Clojure buffers these win over the global `<Space>` keys:

| Keys | What it does |
|---|---|
| `<Space>cb` / `<Space>cf` / `<Space>cd` | Connect to port 4444 (`bb_nrepl`) / to `.nrepl-port` / disconnect |
| `<Space>ee` / `<Space>er` / `<Space>ew` | Evaluate the form / the top-level form / the word |
| `<Space>eb` / `<Space>ef` / `<Space>cc` | Evaluate the buffer / the file / the paragraph |
| `<Space>E{motion}`, `<Space>E` in visual | Evaluate a motion / the selection |
| `<Space>e!` / `<Space>ece` / `<Space>ecr` | Replace the form with its result / add the result as a comment / same, top-level form |
| `<Space>ei` | Interrupt the evaluation |
| `<Space>lv` / `<Space>cl` / `<Space>lg` / `<Space>lq` | Log in a vertical split / same, wider / toggle / close logs |
| `<Space>ta` / `<Space>tn` / `<Space>tc` | Run all loaded tests / this namespace's / the one under the cursor |
| `<Space>rr` / `<Space>ra` | Refresh changed / all namespaces |
| `<Space>v1` / `<Space>ve` / `<Space>vs` | Last result / last exception / source of the function |
| `<Space>x1` / `<Space>xa` | Macroexpand-1 / macroexpand-all the form |
| `K` / `<Space>gd` / `gd` | Docs from the REPL / Conjure's definition / clojure-lsp's |
| `[[` / `]]`, `{{` / `}}` | Previous / next top-level form, element |
| `{}` / `}{` | Swap the element forward / backward |
| `<>` / `><` | Slurp the next element / barf the last one |
| `<Space>w(` / `<Space>w)` (also `[`, `{`) | Wrap the element in `( )`, cursor at the head / tail |
| `<Space>ih` / `<Space>it` | Insert at the head / tail of the list |
| `<Space>rl` / `<Space>re` | Raise the list / the element |

Parinfer (smart mode) keeps the parentheses balanced as you indent.

### Per language

| Where | Keys or command | What it does |
|---|---|---|
| Elixir | `:Mix TASK` (or `:M`) | Run a mix task |
| Elixir | `:A`, `:Esource NAME`, `:Etest NAME` | Alternate between code and test; open by name |
| Python | `<Space>m` / `<Space>3` | `import matplotlib.pyplot as plt` above / a `####` separator line |
| Python | `[[` / `]]`, `[m` / `]m` | Previous / next class or def, method |
| Rust | `:RustLsp runnables` / `testables` | Pick something to run / test |
| Rust | `:RustLsp expandMacro` / `explainError` / `openCargo` | Expand the macro / explain the error / open Cargo.toml |
| Markdown | `j` / `k` | Move by screen line in wrapped text |
| Markdown | `[[` / `]]`, `gO` | Previous / next heading, outline |
| HTML, HEEx, CSS, JSX | `C-e ,` in insert | Expand an Emmet abbreviation, e.g. `ul>li*3` |
| Scala | | Anything past column 100 turns red |

### Commands

| Command | What it does |
|---|---|
| `:Lazy` | Plugins |
| `:checkhealth` | Everything's health |
| `:TSUpdate` | Update treesitter parsers |
| `:Inspect` / `:InspectTree` | Highlight groups under the cursor / the syntax tree |

## Snippets

Type the prefix, then `Tab`. Only the ones in `vim/vsnip/`; friendly-snippets adds many more.

Elixir, general:

| Prefix | Expands to | Prefix | Expands to |
|---|---|---|---|
| `>` | `\|> ` | `>m` | `\|> Enum.map(fn ...)` |
| `>e` | `\|> Enum.each(fn ...)` | `>f` | `\|> Enum.filter(fn ...)` |
| `>fm` | `\|> Enum.flat_map(fn ...)` | `>mpf` / `>ma` | `\|> Enum.map(&...)` / `\|> Enum.map(&(...))` |
| `>i` / `>il` | `\|> IO.inspect()` / with a label | `>d` / `dbg` | `\|> dbg()` / `dbg(...)` |
| `>1` / `>-1` / `>nth` | `List.first` / `List.last` / `Enum.at` | `>r` / `>l` | `Enum.random` / `Enum.to_list` |
| `>a` | `\|> Task.async_stream(fn ...)` | `ins` | `IO.inspect(...)` |
| `wl` / `al` | `~w(...)` / `~w(...)a` | `m` | `%{"key" => value}` |
| `defmo` | `defmodule` named after the file | `genserver` | `use GenServer` skeleton |
| `impl` | `@impl true` + `def` | `info` | `handle_info` |
| `rec` | `recompile()` | `sb` | A section-break comment |
| `RAS` | Rustler target `aarch64-apple-darwin` | | |

Elixir, Latu:

| Prefix | Expands to | Prefix | Expands to |
|---|---|---|---|
| `LT` | Latu imports and aliases | `Lconn` | `Latu.connect("sc://localhost:15002")` |
| `Lread` / `Ltable` / `Lsql` | `Latu.read` / `Latu.table` / `Latu.sql` | `Lrange` / `Lcdf` | `Latu.range` / `Latu.create_dataframe!` |
| `>Ls` / `>Lf` / `>Lwc` | `select` / `filter` / `with_columns` | `>Lg` / `>La` | `group_by` / `agg` |
| `>Lj` / `>Lo` / `>Ll` | `join` / `sort` / `limit` | `>Lr` / `>Ldr` / `>Ldist` | `rename` / `drop` / `distinct` |
| `>Lsh` / `>Lh` / `>Lc` / `>Ln` | `show!` / `head!` / `collect!` / `count!` | `>Lps` / `>Le` | `print_schema!` / `explain!` |
| `>Lw` / `>Lx` | `write!` / `to_explorer!` | `>Lcache` / `>Lpersist` / `>Lcheck` | `cache!` / `persist!` / `checkpoint!` |
| `>Lob` | `observe` | `Lcheck` | `Latu.with_checkpoint!` block |
| `Lexpr` / `Lfun` | `expr("...")` / `fun("name", args)` | `Lwhen` / `Lover` | `when_ ... otherwise` / a window function |
| `Lprog` | A progress handler | | |

Elixir, Explorer, Floki, Flow, VegaLite:

| Prefix | Expands to | Prefix | Expands to |
|---|---|---|---|
| `DF` / `S` | `require Explorer.DataFrame, as: DF` / `alias Explorer.Series` | `>Dp` | `\|> DF.print(limit: :infinity)` |
| `>Dsel` / `>Df` / `>Dm` | `DF.select` / `DF.filter` / `DF.mutate` | `>Dg` / `>Dsum` / `>Da` / `>Dj` | `group_by` / `summarise` / `arrange` / `join` |
| `>Flp` / `>Flf` / `>Flt` / `>Fla` | `Floki.parse_document!` / `find` / `text` / `attribute` | `>F` / `>Fm` / `>Ff` | `Flow.from_enumerable` + `partition` / `Flow.map` / `Flow.filter` |
| `>v` / `>vs` | `\|> Vl.` / `\|> VegaLite.Viewer.show()` | | |

Elixir, Phoenix, LiveView, Ash:

| Prefix | Expands to | Prefix | Expands to |
|---|---|---|---|
| `mount` / `params` / `event` | `mount/3` / `handle_params` / `handle_event` | `render` / `heex` | `render/1` with `~H` / a `~H` block |
| `fcomp` / `fc` (in HEEx) | A function component / `<.component>` | `lv` | `<.live_component ...>` |
| `cmount` / `cupdate` | Live component `mount/1` / `update/2` | `ashr` | `use Ash.Resource` with Postgres |
| `attrs` / `attr` | `attributes do` / `attribute` | `act` / `rel` | `actions do` / `relationships do` |

Python and Rust:

| Prefix | Expands to | Prefix | Expands to |
|---|---|---|---|
| `np` / `pd` / `pl` | numpy / pandas / polars imports | `plt` / `db` / `gl` | matplotlib / dask.bag / glob imports |
| `ifm` | `if __name__ == "__main__":` | `bp` | `breakpoint()` |
| `fn` / `pfn` / `afn` / `pafn` | Rust `fn` / `pub fn` / `async fn` / `pub async fn` | `st` / `pst` / `impl` | `struct` / `pub struct` / `impl` |
| `prn` / `fmt` | `println!("{:#?}", ...)` / `format!` | `resd` / `uerr` | `Result<_, Box<dyn Error>>` / `use std::error::Error` |
| `usd` | `use serde::{Deserialize, Serialize}` | `sb` | A section-break comment (Python and Rust) |

## Elixir and IEx

- The IEx prompt shows the time and the expression counter; `Ecto.Query` and `Ecto.Changeset` are imported when the project has them.
- Up-arrow history survives restarts, in both IEx and `erl`.
- `iexmem`: `iex -S mix` that hands memory back to the OS sooner.

## mise

| Command | What it does |
|---|---|
| `mise ls` | Tools and versions in use here |
| `mise use -g TOOL@VERSION` | For this machine (`~/.config/mise/config.toml`) |
| `mise use TOOL@VERSION` | For this project (`mise.toml`) |
| `mise outdated` / `mise upgrade` | What's behind / upgrade it |
| `mise install` | Install whatever is missing |
| `mise which TOOL` | Where a tool's binary is |
| `mise x TOOL@VERSION -- CMD` | Run CMD with that version, once |
| `mise doctor` | When something looks off |

Shared tools are in `mise/dotfiles.toml`. Rust comes from `rustup` (`rustup update`).
