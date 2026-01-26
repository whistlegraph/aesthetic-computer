# FFOS Local Build (containerized)

This folder provides a **local FFOS ISO build pipeline** using Docker (Arch Linux + archiso). It avoids Git submodules to prevent Netlify build interference.

## Goals
- Build FFOS ISO locally from a fork of `feral-file/ffos` + `feral-file/ffos-user`.
- Keep **all external repos out of the monorepo** (no submodules).
- Keep Netlify builds safe (no extra top‑level build steps).

## What it does
- Clones FFOS + FFOS-USER into a local cache directory (`.ffos-cache/`).
- Applies **optional overlay patches** from `overlays/` (example: AC kiosk service).
- Runs `mkarchiso` inside an Arch Linux container to produce an ISO.

## Quick Start
```bash
# From repo root
bash utilities/ffos-build/build.sh
```

## GitHub Actions
A workflow is provided to build the ISO on GitHub runners and upload the ISO as an artifact:
- [ .github/workflows/ffos-local-iso.yml ](.github/workflows/ffos-local-iso.yml)

## Outputs
- ISO artifacts will be written to:
  - `utilities/ffos-build/.ffos-cache/out/`

## Configure
Set env vars as needed:
- `FFOS_REPO` (default: https://github.com/feral-file/ffos.git)
- `FFOS_USER_REPO` (default: https://github.com/feral-file/ffos-user.git)
- `FFOS_BRANCH` (default: develop)
- `FFOS_USER_BRANCH` (default: develop)
- `FFOS_VERSION` (optional)

Example:
```bash
FFOS_BRANCH=develop FFOS_USER_BRANCH=develop bash utilities/ffos-build/build.sh
```

## Netlify Safety
This pipeline **does not add submodules** and **does not modify Netlify config**. The cache directory is ignored in git.

## Notes
- `mkarchiso` requires root; we run it inside Docker.
- This is **prototype tooling**. Expect iteration.

## Included overlay: Aesthetic Computer kiosk

This repo ships an overlay that adds:
- `start-aesthetic-kiosk.sh`
- `aesthetic-kiosk.service`

Paths:
- `overlays/ffos-user/users/feralfile/scripts/start-aesthetic-kiosk.sh`
- `overlays/ffos-user/users/feralfile/.config/systemd/user/aesthetic-kiosk.service`

Enablement depends on the FFOS image; you may still need to enable the user service:
```bash
systemctl --user enable --now aesthetic-kiosk.service
```
