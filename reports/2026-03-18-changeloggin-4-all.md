# Changeloggin' 4 All — March 2026 So Far

**Author:** @jeffrey
**Date:** 2026-03-18
**Purpose:** Source material for news.aesthetic.computer voiced changelog post

---

## Overview

884 commits in March so far. The work breaks down into a few major threads: new creative pieces, AC Native OS maturing into something real, KidLisp Keeps going live on mainnet, a stack of research papers, and platform-wide GPU hardening.

---

## New Pieces

### Generative / Visual
- **splat.mjs** — CPU-rendered 3D Gaussian splatting. No GPU required, runs anywhere.
- **halley.mjs** — Halley's method fractal visualization (z³ + 7). Math as landscape.
- **morpho.mjs** — Pixel sorting reframed as morphogenesis. Treating images like organisms.

### Multiplayer
- **squash.mjs** — 2D platformer with dual-channel networking (UDP for position, WebSocket for game events). First piece using the new dual-channel pattern documented in CLAUDE.md.

### Performance / Music
- **demoplay.mjs** — Conductor piece for Ars Electronica 2026 performance.
- **ars-2026.mjs** — Score file for the Ars performance.
- **score.mjs** — QWERTY bytecode instrument with pixel-audio feedback loop.

### System / Utility
- **machines.mjs** — System monitoring dashboard for remote AC machines.
- **terminal.mjs** — PTY terminal emulator (VT100) running inside AC.
- **split.mjs** — Side-by-side PTY split view (Ctrl+N).
- **power.mjs** — USB-C power role control for AC Native devices.
- **voice.mjs** — TTS control panel.
- **wifi.mjs** — WiFi management piece.
- **os.mjs** — Personalized OS image downloader.
- **claude.mjs** — Native Claude Code integration with device auth.

---

## Major Piece Updates

### notepat.mjs (the big one)
- NuPhy Air60 HE analog keyboard support (pressure-sensitive keys)
- Echo slider controlling room reverb wet mix
- Wave type selection (sine, triangle, sawtooth, square, noise)
- Tab completion in prompt
- Key labels using 6x10 BDF font
- Tablet mode detection
- Waveform visualization above pads
- WiFi + chat integration
- Mic sampling + pitch-shifted playback
- FX mix slider

### chat.mjs
- Inertial bouncy scrolling
- Handle shadows + multi-line message shadows
- Presence display improvements

### keep.mjs
- Job-based mint pipeline (prepare → status → poll)
- Live oven preview during thumbnail bake
- Auto-rebake for kept pieces
- Beacon SDK wallet integration
- Progress indicators

### profile.mjs
- Responsive layout updates

---

## AC Native OS

The bare-metal creative OS hit several milestones:

### Hardware
- USB-C power role swapping via `system.typec` API
- Intel SOF audio firmware bundled
- Meteor Lake GPU drivers (i915 DMC, GuC, HuC, GSC)
- Realtek + MediaTek WiFi drivers
- Intel Xe GPU + touchscreen support

### Software
- **PTY terminal emulator** — VT100 with tab completion, running in-piece
- **Device token login** — no browser needed, device-key auth flow
- **Wayland/cage compositor** — Firefox browser support with DRM handoff
- **WiFi auto-connect** — persistent credentials, instant TTS feedback
- **Audio diagnostics** — full boot-to-shutdown logging to MongoDB
- **Machine monitoring** — C-based system daemon with remote telemetry
- **OTA updates** — Cloudflare edge CDN, named build URLs, version mirrors
- Boot splash animations (circles, rainbow stripes, doo-dah)
- Theme system (auto dark/light switching)
- Claude Code running natively on AC devices

---

## KidLisp Keeps (NFT Marketplace)

### Contract
- v11 live on mainnet: `KT1Q1irsjSZ7EfUN4qHzAB2t7xLBPsAWYwBB`
- User-only minting (no admin path), permit-signed
- Fee: 2.5 XTZ, royalties: 9% artist + 1% platform
- v9 deprecated, all 5 original tokens migrated to v11
- v10 deprecated (bad deploy)

### buy.kidlisp.com
- Fresh/Sold tabs with auto-cycling
- Vegas theme with glow effects
- IPFS artifacts iframe preview
- Light/dark theme toggle

### keep.kidlisp.com
- Broadside-style preamble
- Unkept/Kept tabs (color-coded red/green)
- Market tab with objkt.com listings
- Wallet leaderboard (Keepers vs Buyers)
- Technology page with contract/wallet details

### Mint Flow
- Live oven baking telemetry in modal
- iframe preview during mint
- Job-based pipeline with timeouts (120s-180s)
- Retries + error handling
- Bundle regeneration with fresh URIs

---

## GPU Rendering & Graphics

### Mali/Adreno Fixes (Android)
- `mobileSafeMode` CPU fallback for blur/sharpen/contrast
- Mali-safe shaders with precomputed weights
- `texelFetch` instead of texture sampling
- Pipeline sync with `gl.finish()` before GPU effects
- Dynamic fragment shader precision (try-catch fallback)
- Blank-output sanity checks
- Fixed UNPACK_FLIP_Y_WEBGL (buggy on Android)
- readPixels into Uint8ClampedArray for Android

### Performance
- Layer0 + post-composite debug logging
- Embedded layer wipe cache
- GPU telemetry system (`/api/kidlisp-log` + dashboard)
- Auto-reporting failures

---

## Papers & Research

18 papers in the system, 4 new/updated this month:

1. **"Reading the Score"** — score analysis paper (new)
2. **"KidLisp Cards"** — index card format paper (new)
3. **JOSS KidLisp paper** — restructured with new figures and references
4. **All 15 papers** translated to Danish, Spanish, and Chinese

### Upcoming Deadlines
- ACM C&C 2026 (Apr 16) — Demos
- ICCC 2026 (Apr 24) — Short Papers
- IEEE ICIR 2026 (TBD) — Late-breaking

---

## Multiplayer Networking

New dual-channel pattern documented and implemented:
- **UDP** for high-frequency position sync (low latency, may drop packets)
- **WebSocket** for reliable game events (join/leave, scoring, round control)
- Reference implementations: squash.mjs (2D), 1v1.mjs (3D FPS), udp.mjs (minimal)

---

## Infrastructure & Tools

### Agent Memory System
- Local-first encrypted memory store (`~/.ac-agent-memory`)
- AES-256-GCM encryption
- CLI: list, remember, checkpoint, doctor, profile, flush-remote
- Codex import for continuity
- Lineage tracking (`remembered_from`)

### OTA Build Pipeline
- `native-builder.mjs` on oven
- Auto-populate kernel firmware blobs
- Git poller for automatic builds
- Named build URLs on Cloudflare edge

### Papers Pipeline
- `papermill` on oven for PDF builds
- Pre-commit hook integration
- Papers CLI for managing builds
- Multilingual compilation (xelatex + bibtex, 3-pass)

---

## User-Facing Highlights (for news post)

What users would actually notice or care about:

1. **New pieces to play with** — splat, halley, morpho are all visually striking generative works you can run right now
2. **squash.mjs** — first real multiplayer game piece, play it with friends
3. **KidLisp Keeps are live** — mint and collect KidLisp pieces as NFTs on Tezos
4. **buy.kidlisp.com** — browse and buy KidLisp keeps
5. **Android rendering fixed** — Mali/Adreno GPU issues resolved, effects work on mobile now
6. **Chat got smoother** — bouncy inertial scrolling, better shadows
7. **notepat grew up** — analog keyboard support, reverb, waveform viz, mic sampling
8. **AC Native booting on real hardware** — with WiFi, terminal, Claude Code, OTA updates
9. **Research papers** — 18 papers documenting the system, all multilingual
10. **Multiplayer infrastructure** — dual-channel UDP+WS pattern ready for new networked pieces

---

## What Users Are Up To

- KidLisp pieces being minted and traded on Tezos mainnet
- Community using chat with improved scrolling/presence
- Multiplayer pieces being tested (squash sessions)
- Ars Electronica 2026 performance preparation underway
- Research papers being submitted to ACM C&C, ICCC, IEEE ICIR
