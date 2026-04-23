# Fresh Mac setup for aesthetic-computer

Bootstrap a new Apple Silicon Mac to the point where you can clone the
repo, run the dev server, and have the slab daemon + menubar running.

## Context
Fresh arm64 Mac, zsh default. Neither `brew` nor `gh` is on PATH, and
`/opt/homebrew` doesn't exist yet. `~/.zprofile` and `~/.zshrc` also
don't exist. Goal: get the prerequisites in, clone the repo, and hand
off to `slab/install.sh` for the AC-specific bits.

## Steps

### 1. Install Homebrew
Run the official installer:
```sh
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
```
- The installer is interactive: it prints what it will do and waits for RETURN to proceed.
- It will prompt for the macOS login password (sudo) to create `/opt/homebrew` and chown it.
- On arm64 Macs, Homebrew installs to `/opt/homebrew` (not `/usr/local`).

### 2. Wire `brew` onto PATH for future zsh sessions
Create `~/.zprofile` with the Homebrew shellenv line (the installer prints this exact instruction at the end):
```sh
echo >> ~/.zprofile
echo 'eval "$(/opt/homebrew/bin/brew shellenv)"' >> ~/.zprofile
eval "$(/opt/homebrew/bin/brew shellenv)"
```
The final `eval` loads `brew` into the current shell so step 3 works without opening a new terminal.

### 3. Install the GitHub CLI
```sh
brew install gh
```

### 4. Authenticate with GitHub
```sh
gh auth login
```
This is interactive. Typical answer path for a personal Mac:
- **Account:** GitHub.com
- **Protocol:** HTTPS (simplest; use SSH if you plan to push via SSH keys)
- **Authenticate Git with your GitHub credentials?** Yes
- **How would you like to authenticate?** Login with a web browser → opens github.com/login/device, paste the one-time code.

After login, `gh auth status` should show the authenticated account.

### 5. Clone the repo (+ nested vault, if you have access)
```sh
gh repo clone whistlegraph/aesthetic-computer
cd aesthetic-computer
# the vault is a separately-cloned private repo nested inside:
# git clone git@github.com:…/aesthetic-computer-vault aesthetic-computer-vault
```

### 6. Run the slab installer
Sets up the lid-ambient daemon, menubar app, launchd plists, Python
venv, and Claude Code hooks. Idempotent — re-runnable.
```sh
./slab/install.sh              # full install
./slab/install.sh --no-hooks   # skip the Claude Code settings merge
```
See [slab/README.md](../slab/README.md) for details.

## Verification
- `brew --version` prints a version.
- `brew doctor` reports "Your system is ready to brew" (warnings about Xcode CLT are fine if they appear; the installer handles CLT automatically).
- `gh --version` prints a version.
- `gh auth status` shows logged in as `<your-username>` on github.com.
- `launchctl list | grep slab` shows `computer.slab.daemon` and `computer.slab.menubar` running with exit code 0.

## Notes / caveats
- **Xcode Command Line Tools**: the Homebrew installer will trigger the CLT install if missing. That opens a GUI dialog and downloads ~1–2 GB — expect a wait on first run.
- **Sudo password**: required once during the Homebrew install, and again by `slab/install.sh` for the passwordless-pmset sudoers rule (skippable with `--no-sudoers`).
- **Interactive prompts**: steps 1 and 4 need the user at the keyboard; they can't be fully automated from here.
- **Git**: macOS ships a `git` shim that triggers CLT. After `brew install gh`, `gh` depends on git, so brew will pull in a fresh `git` as a dependency — no separate install needed.
- **Not covered here**: GPG keys (restore `~/.gnupg/` from your own backup before decrypting vault files), SSH keys (symlinked from the vault once cloned), Claude Code memory (`~/.claude/`, `~/.ac-agent-memory/` — private backup).
