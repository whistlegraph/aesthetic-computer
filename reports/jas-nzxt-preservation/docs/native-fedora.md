# Native Fedora Setup (No Devcontainer)

This guide is for running Aesthetic Computer directly on Fedora without the VS Code devcontainer. It mirrors the minimum pieces needed for `npm run aesthetic` on a workstation.

## 1) System bootstrap

- Run the helper script (installs dnf deps, fnm + Node 22, global npm CLIs, Redis, hosts entries):
  ```bash
  bash scripts/fedora-native-setup.sh                # base stack
  bash scripts/fedora-native-setup.sh --extras       # adds mkcert, stripe-cli, ngrok, doctl
  bash scripts/fedora-native-setup.sh --clone-vault  # clones aesthetic-computer-vault alongside this repo and copies .devcontainer/envs/devcontainer.env if present
  ```
- The script enables RPM Fusion for ffmpeg and installs Chromium + X11/GTK bits for headless Chrome use.
- Global npm tools install to `~/.local/bin`; add it to PATH if your shell doesn’t already.
- If Redis wasn’t enabled by the script, start it manually: `redis-server --daemonize yes` (or `valkey-server --daemonize yes`).

## 2) Secrets and environment

- Clone the secrets repo (private) alongside this repo:
  ```bash
  git clone git@github.com:whistlegraph/aesthetic-computer-vault.git aesthetic-computer-vault
  ```
  - If you used `--clone-vault`, this is done for you (using `VAULT_URL` if set).
- Copy the shared env file from the vault to this repo:
  ```bash
  cp aesthetic-computer-vault/.devcontainer/envs/devcontainer.env .devcontainer/envs/devcontainer.env
  ```
- When using `fish`, source the devcontainer helpers to pick up the same functions/paths:
  ```bash
  fish -c 'source .devcontainer/config.fish'
  ```
- Netlify/Stripe keys are expected to come from that env file; you’ll still need to `netlify login` once locally.

## 3) Repo installs

- Install dependencies (first run only):
  ```bash
  npm install
  cd system && npm install
  cd ../session-server && npm install
  ```
- Optional: if you plan to touch Tezos or other subprojects, install there too (not needed for the main loop).

## 4) Run the stack natively

- From the repo root:
  ```bash
  npm run aesthetic
  ```
  This starts:
  - Netlify dev for the site (ports 8888/8889)
  - Session server (8889)
  - Stripe mocks and URL helper
- Visit `http://localhost:8888` (or `https://localhost:8888` if you used `--extras` to install mkcert and trust the cert).

## 5) Parity notes vs. devcontainer

- Included in the script: build toolchain, ffmpeg, Redis, Chromium, caddy, fnm/Node 22, netlify-cli, kill-port, npm-check-updates, devcontainer CLI, TypeScript LSP.
- Optional extras: mkcert, stripe-cli, ngrok, doctl.
- Not included: SmartPy/Octez, gcloud SDK, PowerShell, Docker-in-Docker tricks, VS Code CDP tunnel, Emacs daemon setup, or the wider MCP tooling in `.devcontainer/entry.fish`.
- If you rely on those heavier tools, install them separately or keep using the devcontainer for that work.

## 6) Quick verification

- Check toolchain: `node -v` (>=22), `redis-cli ping`, `ffmpeg -version`, `chromium --version`.
- Run `npm run aesthetic` and load the site; confirm log output shows Netlify linked to the expected `NETLIFY_SITE_ID`.
- If you hit missing libs during npm install, rerun `bash scripts/fedora-native-setup.sh` to make sure the Fedora build deps are present.
