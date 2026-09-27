# Joining the family

A family Mac is a full AC workstation: its own checkout, the shared Fish
commands, and tracked editor settings. Neo and Blueberry are the reference;
Frisbee exposed the missing step between a Slab-only machine and a workstation.
A running menu bar does not mean the development environment is installed.

## Connect and check out AC

Give the Mac its own hostname, join Tailscale, enable Remote Login, and authorize
an existing operator's **public** SSH key. Keep each machine's private key local.
The examples assume the personal `jas` account; use the actual login for another
host. Verify SSH before provisioning:

```sh
ssh jas@frisbee 'hostname; whoami; uname -m'
```

On the new Mac, clone AC if absent:

```sh
git clone https://knot.aesthetic.computer/aesthetic.computer/core ~/aesthetic-computer
```

For an existing checkout, inspect `git status` first. A Slab-only sparse checkout
needs expansion; this downloads the remaining tracked files without changing
branches or discarding edits:

```sh
cd ~/aesthetic-computer
git sparse-checkout disable
```

Keep repositories out of Desktop. Desktop's `Shelf` alias points to
`~/Documents/Shelf`; see [the Mac conventions](../SCORE.md).

## Install the shared environment

Run in Terminal on the new Mac, as the login user:

```sh
bash ~/aesthetic-computer/scripts/mac-family-setup.sh
```

Homebrew and the default-shell change request the administrator password locally.
The installer adds Fish, fnm, the repo's Node version, jq, ripgrep, Starship,
Neovim, and coreutils. Fish loads the AC commands and `c`/`co` helpers from the
checkout. Neovim, Starship, and Emacs configuration link to tracked dotfiles.
Starship configuration is available; the shared AC Fish prompt remains active.
Emacs itself is optional and is not installed by this baseline.

Replaced settings move to `~/.local/state/ac-dotfiles/backup.*`. Fish history,
universal variables, and other local files stay on the machine. Put intentional
machine-specific shell additions in `~/.config/fish/config.local.fish`, loaded
after the shared configuration. Re-running setup preserves matching links.

`--dotfiles-only` installs configuration without packages or administrator access.
The old `dotfiles/install.sh` and `dotfiles/symlink.sh` route Macs here too.
The broader `mac-native-bootstrap.sh` is a separate development-server recipe;
it also changes certificates, services, sudoers, and `/workspaces`, so it is not
the family baseline.

## Verify before calling it ready

```sh
bash ~/aesthetic-computer/scripts/mac-family-setup.sh --check
fish -ic 'ac-help'
```

The check fails on missing baseline tools, dotfile links, AC shell commands, a
non-Fish login shell, or missing sleep-control permission when Slab is installed.
Setup installs that permission for an existing Slab installation. Open a fresh Terminal tab and confirm it starts in Fish.
Repo updates carry shared shell changes; local overrides remain separate.
Use `git pull --ff-only` only after reviewing local work, and rerun setup when
`.node-version` or package requirements change.

Development-server dependencies and private configuration are a second step:
install the relevant project's dependencies, provision its own vault access,
and configure local certificates if it needs HTTPS. Do not copy another Mac's
whole home, browser profile, agent credentials, or Fish universal variables.
A shell check does not prove the site or authenticated services are working.

## Add the machine's role

Slab, Deskflow, agent CLIs, media tools, and compute workers have their own
installers. Preserve those already present; install the ones the new machine
will use. Test each role separately, including required macOS permissions.
For Slab's **Stay awake (lid closed)** control, install its sleep permission
rule from Terminal, then verify the actual setting:

```sh
bash ~/aesthetic-computer/slab/install.sh --sleep-control-only
~/.local/bin/claude-sleep awake
~/.local/bin/claude-sleep status
```

The last command must report `SleepDisabled=1`. Without this administrator-installed
rule, a prompt-host-only Slab installation cannot apply the menu toggle. Use
`claude-sleep auto` to restore normal sleep. Verify SSH remains reachable after
closing the lid before relying on unattended work.

Choose its cursor/accent identity through the [fleet tools](README.md).

For the fourth Mac, repeat these steps with its own name. No new Fish fork or
copied Neo home directory is needed. Record exceptions in the machine's local
configuration, and improve this shared path when another missing step appears.
