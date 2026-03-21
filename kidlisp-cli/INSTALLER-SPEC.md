# `kidlisp` Installer Spec

## Goal

Install `kidlisp` with no Bun/Node/Deno requirement, using one command from the website.

## Entry Points

### Unix-like installer

```bash
curl -fsSL https://kidlisp.com/install | sh
```

### Windows PowerShell installer

```powershell
irm https://kidlisp.com/install.ps1 | iex
```

## Inputs

### Environment variables

- `KIDLISP_CHANNEL` (`stable` default, `beta` optional)
- `KIDLISP_VERSION` (optional pinned version; default latest in channel)
- `KIDLISP_INSTALL_DIR` (optional override)
- `KIDLISP_BIN_NAME` (optional override, default `kidlisp`)

### CLI flags (if script is downloaded and run directly)

- `--channel <stable|beta>`
- `--version <vX.Y.Z>`
- `--install-dir <path>`
- `--yes` (non-interactive)

## Platform Detection

Map OS/arch to asset target:

- `darwin` + `arm64` -> `darwin-arm64`
- `darwin` + `x64` -> `darwin-x64`
- `linux` + `x64` -> `linux-x64`
- `linux` + `arm64` -> `linux-arm64`
- `windows` + `x64` -> `windows-x64.exe`

If unsupported:
- Exit non-zero with clear message.
- Print release page URL for manual install.

## Install Locations

Default install dir precedence:

- Unix:
  - `$HOME/.local/bin` if available
  - else `$HOME/bin`
- Windows:
  - `%USERPROFILE%\\AppData\\Local\\kidlisp\\bin`

Installer must:
- Create install directory if missing.
- Place binary as `kidlisp` (`kidlisp.exe` on Windows).
- Set executable bit on Unix.

## PATH Handling

If binary directory is not on PATH:
- Print exact command(s) to add it for the detected shell.
- Do not silently mutate shell rc files unless explicit `--yes --add-path` is set (future option).

## Integrity and Security

Installer must verify:
- SHA-256 checksum for downloaded asset.
- Checksum file authenticity from release source.

Release artifacts required:
- `kidlisp_<version>_<target>.(tar.gz|zip)`
- `kidlisp_<version>_checksums.txt`
- Optional: detached signature file.

If checksum mismatch:
- Abort install.
- Keep no partially installed binary.

## UX Contract

### Success output

- Installed version
- Install path
- Suggested next command (`kidlisp --help`)

### Failure output

- One-line reason
- Troubleshooting next command(s)
- URL to manual installation docs

## Idempotency

Running installer multiple times should:
- Replace existing binary atomically.
- Preserve previous binary permissions.
- Print old -> new version when upgraded.

## Self-Update Interop

Installer and CLI should share the same release metadata contract so:

- `kidlisp self-update`
- website installers

use identical channel/version resolution and checksum verification logic.
