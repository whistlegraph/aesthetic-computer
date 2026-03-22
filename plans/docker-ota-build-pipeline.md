# AC Native OS — Docker-Based OTA Build Pipeline

## Goal
One build process everywhere: local devcontainer, oven server, any CI/CD.
Produces identical, reproducible vmlinuz kernels with no user secrets.

## Architecture

```
Source (git) → Docker Build → vmlinuz (generic, no creds)
                                ↓
                    ┌───────────┴───────────┐
                    ↓                       ↓
              ac-os upload            ac-os flash
              (OTA to CDN)         (USB + user creds)
```

### Build (same everywhere)
```bash
docker build -t ac-os-builder -f fedac/native/Dockerfile.builder .
docker create -e AC_BUILD_NAME=<name> ac-os-builder
docker start -a <CID>
docker cp <CID>:/tmp/ac-build/vmlinuz ./vmlinuz
```

### Upload (OTA — no creds in kernel)
```bash
ac-os upload vmlinuz   # Signs + pushes to DO Spaces CDN
```

### Flash (local — adds user creds)
```bash
ac-os pull             # Downloads OTA vmlinuz from CDN
ac-os flash            # Writes vmlinuz + config.json (creds) to USB
```

## Transition Plan

### Phase 1: Unify `ac-os build` to use Docker
- [ ] `ac-os build` detects Docker, runs `docker build + docker run`
- [ ] Falls back to native build if no Docker (e.g. direct on host)
- [ ] `ac-os build --lisp` passes `AC_BUILD_LISP=1` to Docker
- [ ] Remove `build-and-flash.sh` dependency (keep as legacy fallback)
- [ ] `ac-os flash` only flashes — never builds (downloads OTA or uses local vmlinuz)

### Phase 2: Oven adopts Docker builds
- [ ] Update `native-builder.mjs` to use Docker instead of `build-and-flash.sh`
- [ ] Oven poller: `git pull` → `docker build` → `docker run` → `upload-release.sh`
- [ ] Remove native-cache symlinks, musl-gcc, Ubuntu package installs
- [ ] Oven only needs: Docker, git, curl (for upload)
- [ ] Deploy updated `native-builder.mjs` to oven

### Phase 3: API + UX endpoints
- [ ] `GET /native-build` — add `dockerImage`, `dockerCached` fields to status
- [ ] `GET /native-build/:id/logs` — stream Docker build logs
- [ ] VSCode extension status bar — show build stage from Docker output
- [ ] `ac-os build --upload` — local build + automatic OTA upload
- [ ] `ac-os status` — check oven build status from CLI

### Phase 4: Signing + verification
- [ ] `ac-os upload` signs vmlinuz with ed25519 key
- [ ] `ac-os pull` verifies signature before flashing
- [ ] OTA releases include `.sig` file on CDN
- [ ] Device verifies signature before applying OTA update

### Phase 5: CL variant
- [ ] `ac-os build --lisp` produces CL-based vmlinuz
- [ ] Same Docker image, different entrypoint flag
- [ ] CL kernel uploaded to separate CDN path (`os/native-cl-latest.vmlinuz`)
- [ ] `ac-os pull --lisp` downloads CL variant

## Key Principles
1. **No secrets in builds** — OTA kernels are generic, creds injected at flash
2. **Reproducible** — same Docker image = same vmlinuz, any machine
3. **One process** — local and oven use identical Docker commands
4. **Fast rebuilds** — Docker layer cache: only recompile what changed
5. **Wide compat** — Fedora 43 base, GCC 15, Linux 6.19.9, Intel i915 + simpledrm

## Files to modify
- `fedac/native/ac-os` — unify build/flash/upload around Docker
- `oven/native-builder.mjs` — switch to Docker builds
- `oven/native-git-poller.mjs` — simplify (just detect new commits)
- `fedac/native/Dockerfile.builder` — already working
- `fedac/native/docker-build.sh` — already working
- `vscode-extension/extension.ts` — OTA status bar updates
