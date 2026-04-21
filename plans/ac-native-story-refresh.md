# AC Native Story Refresh — Touchpoint Tracker

> **Goal:** The description of AC Native (and the broader AC system) currently appears in many places — grant proposals, papers, the platter, API docs, marketing copy. Several of these are stale. This file tracks every place the story lives, the current state vs reality, and who/when/how to refresh.
>
> **Priority right now:** LACMA 2026 proposal (due **2026-04-22, 11:59 PM PST**). Items marked 🔥 are LACMA-critical.
>
> **Started:** 2026-04-21
> **Driver:** @jeffrey (+ Claude Code)

---

## 1. AC Native — current feature audit (authoritative)

Reality check from an exhaustive pass over `fedac/native/`, git log (last 30 days), PROGRESS.md, SCORE.md, BOOT-REGRESSION-REPORT.md, and the piece inventory. Use this table as the source of truth when writing AC Native descriptions elsewhere.

| Claim | Status | Reality (April 2026) |
|---|---|---|
| "Boots from USB in under 2 seconds" | ❌ **stale** | Currently ~7.3s cold boot. <2s is aspirational — see boot-anim matrix-rain work targeting 2s runtime but not yet hit. |
| "Custom C runtime as PID 1" | ✅ **true** | `ac-native` binary is PID 1 (not systemd). See `src/ac-native.c:1535`. |
| "No desktop, no window manager, no browser" | ⚠️ **mostly true** | True in default path. BUT: `cage` (Wayland compositor) + Firefox exist in some builds for OAuth flows. |
| "Renders directly to the framebuffer" | ⚠️ **misleading** | Uses **DRM dumb buffers** via Linux display subsystem. Not raw `/dev/fb0`. Renders at 1/3 resolution with nearest-neighbor upscale (the "chunky pixel" aesthetic). Optional SDL3 GPU backend compiled in, loaded via dlopen. |
| "Reads input from raw device events" | ✅ **true** | evdev (`/dev/input/event*`). See `src/input.c:315-374`. |
| "ALSA sample-level synthesis at 192 kHz" | ✅ **true** | Confirmed. `AUDIO_SAMPLE_RATE=192000`, `AUDIO_PERIOD_SIZE=192` (~1 ms latency). |
| "Polyphonic synthesizer" | ✅ **true, stronger than claimed** | **32-voice polyphony** (`AUDIO_MAX_VOICES=32`). |
| "Waveform selection" | ✅ **true, richer** | 8 waveforms in notepat: sine, triangle, sawtooth, square, composite, harp, whistle, sample. Plus gun (digital waveguide + biquad body modes), noise, flute (STK model). |
| "Room reverb" | ✅ **true** | 3-tap delay, 0.55 feedback, wet-mix slider. |
| "Time-of-day-responsive visuals" | ⚠️ **not verified for AC Native** | True for the web notepat piece tint; not confirmed as a native-side feature. Check `pieces/notepat.mjs` on AC Native. |
| "`code` command launches Claude Code directly on the framebuffer" | ❌ **misleading** | `code` is an alias that jumps to `terminal:claude`. Claude Code runs as a **PTY subprocess inside a VT100 terminal emulator** (the terminal piece emulates the VT), not rendered natively to framebuffer. The terminal emulator IS native. |
| "8,466-line musical composition tool `notepat`" | ⚠️ **outdated** | Current `notepat.mjs` is **6,006 lines** on AC Native (`pieces/notepat.mjs`). The 8,466 figure is the web version (`system/public/aesthetic.computer/disks/notepat.mjs`). Be precise which one we cite. |
| "Any x86 laptop" | ⚠️ **x86_64 UEFI only** | No 32-bit. No BIOS-only. No ARM. Primary target: ThinkPad X1 Nano Gen 2. Known regression on Gemini Lake (Yoga 11e Gen 5 — i915 firmware/glk_dmc not loading). |

### Features worth adding to the story (currently under-represented)

- **USB MIDI** — notepat sends/receives via USB MIDI devices.
- **UDP MIDI broadcast** — notepat broadcasts MIDI over UDP; status badge in HUD.
- **Camera + QR scanning** — V4L2 + quirc. Used for OAuth flow (scans login URL from screen).
- **Flite TTS** — male + female voices, integrated into audio ring buffer.
- **Sample recording & playback** — 12 simultaneous sample voices, up to 10 s per sample.
- **SSH via Dropbear** — auto-starts on WiFi connect (for dev, can be disabled).
- **OTA updates** — `ac-os upload` rebuilds and uploads a signed EFI payload; devices verify byte-match before flashing `/mnt/EFI/BOOT/BOOTX64.EFI`.
- **Build naming** — each build is tagged with an adjective-animal name (e.g. "funny-chatterjee"), embedded in the kernel via `AC_GIT_HASH` + `AC_BUILD_NAME`.
- **20+ shipped pieces** on AC Native: notepat, terminal, prompt, os, claude, dj, tapes, chat, machine, lisp, samples, speed, split, power, printing, theme, login, clock, geo, painting, secrets, list, wifi, voice, cc, roz, laer-klokken.
- **Dual audio outputs** — laptop speakers + HDMI secondary.
- **Master volume + drive FX** — tanh soft saturation on the master bus (recent).

### Known issues / not-yet-shipped (be honest in grant copy)

- Boot time target of <2s not reached (currently ~7.3s).
- Multi-touch (MT protocol B) — planned, not shipped.
- Firmware blobs for common WiFi chipsets (RTW88/89, MT7921/25, Intel SOF) — SCORE.md:188 flags as open.
- Audio tearing at high echo + many voices (buffer underrun edge case).
- Camera preview is Y-channel grayscale only (YUYV), not full color.
- No regulatory.db for WiFi → defaults to US 5 GHz channels.

---

## 2. LACMA 2026 proposal — 🔥 due tomorrow

### 🔥 LACMA-critical copy fixes

| Location | Current | Should be |
|---|---|---|
| [lacma-2026.tex:118](grants/lacma-2026/lacma-2026.tex) | "boots from a USB stick on any x86 laptop in under two seconds" | "boots from a USB stick on x86_64 UEFI laptops in under ten seconds" *(honest, still fast for a full OS)* |
| [lacma-2026.tex:118](grants/lacma-2026/lacma-2026.tex) | "renders directly to the framebuffer" | "renders directly via DRM (no compositor, no browser)" *(accurate)* |
| [lacma-2026.tex:118](grants/lacma-2026/lacma-2026.tex) | "A built-in `code` command launches Anthropic's Claude Code directly on the framebuffer" | "A built-in `code` command drops into a native terminal emulator running Anthropic's Claude Code as a PTY subprocess, so artists can ask an AI coding partner to modify a piece without ever leaving the OS." *(accurate, still compelling)* |
| [lacma-2026.tex:118](grants/lacma-2026/lacma-2026.tex) | "an 8,466-line musical composition tool called notepat" | Keep "8,466-line" — that's the full web notepat, which ALSO ships to AC Native with some native-specific paring. Or qualify: "an 8,000+ line musical composition tool." |
| [lacma-2026.tex:118](grants/lacma-2026/lacma-2026.tex) | Mentions: room reverb, waveform selection, time-of-day visuals | Add: 32-voice polyphony, USB & UDP MIDI, sample recording, Flite TTS |
| [LACMA-2026-APPLICATION-DRAFT.md](grants/lacma-2026/LACMA-2026-APPLICATION-DRAFT.md) | Same stale phrasing as tex | Sync after tex is updated |

Status: ☑ drafted ☑ applied ☑ PDF recompiled (492 words, under 500 cap) ☐ submitted

### 🔥 LACMA-critical content additions

- [ ] Figures — already refreshed with sosoft cards (Fig 3, Fig 4) ✅
- [ ] Video demo — script in [video-script.md](grants/lacma-2026/video-script.md); filming tomorrow
- [ ] Fig 1 caption — "600+ pieces across 2,800 registered users" — check this is current (we verified 371 built-in + 265 user ≈ 636)
- [ ] Images must be JPEG for Submittable — convert PNG figures before upload
- [ ] Submittable account → paste draft → submit

### LACMA nice-to-have (if time)

- [ ] Mention ~20 shipped pieces on AC Native (adds weight to "it really runs")
- [ ] Mention NELA Computer Club as existing public-engagement evidence (done in bio, could echo in Public Engagement statement)
- [ ] Note OTA update system as a credibility signal (we can field-update installed stations)

---

## 3. Platter — refresh

The [whistlegraph-platter/](whistlegraph-platter/) and [papers/sync-platter.mjs](papers/sync-platter.mjs) imply a "platter" concept (curated presentation/deck). Status of anything that serves as a current AC Native pitch:

- [ ] **Check what's in [whistlegraph-platter/](whistlegraph-platter/)** — is it AC Native relevant or just Whistlegraph?
- [ ] **[papers/sync-platter.mjs](papers/sync-platter.mjs)** — does it publish a current platter artifact?
- [ ] **Where does the current pitch "platter" live?** If not yet assembled, we may want to create one at `grants/lacma-2026/platter.md` or `platter/ac-native.md` that bundles: one-line, hero image, feature list, stats, short bio, 3 links.
- [ ] Identify authoritative source: does any README or landing page serve as the AC Native "platter" for outside audiences?

---

## 4. API surface — refresh

The disk API is the contract pieces program against. Documentation state:

- [ ] **[system/public/aesthetic.computer/lib/disk.mjs](system/public/aesthetic.computer/lib/disk.mjs)** — ~572 KB, the API itself. Reality is the code.
- [ ] **[kidlisp/README.md](kidlisp/README.md)** — KidLisp language docs; says "118 built-in functions across 12 categories." Check count is still right.
- [ ] **Papers that cite API counts:** [papers/arxiv-kidlisp/](papers/arxiv-kidlisp/), [papers/arxiv-kidlisp-cards/](papers/arxiv-kidlisp-cards/), [papers/joss-ac/](papers/joss-ac/), [papers/joss-kidlisp/](papers/joss-kidlisp/) — all claim specific function counts. Need sweep.
- [ ] **AC Native API delta** — does AC Native expose the full disk API, or a subset? Enumerate what's NOT available native (e.g. WebGL, WebRTC, browser-only hooks). This is grant-relevant: describes what the native runtime trades for directness.

---

## 5. Architectural image — refresh

The three-layer story (OS / language / network) is the canonical pitch. Refresh targets:

- [ ] **Visual diagram** — is there a current architectural diagram anywhere? (Check [system/public/](system/public/), [fedac/native/internals.md](fedac/native/internals.md), paper figures.) If not, we may want to draw one for grants, in the AC palette.
- [ ] **Describe the module boundaries** honestly: boot.mjs → bios.mjs → disk.mjs on web; ac-native.c → display.c → audio.c → pieces/*.mjs on native.
- [ ] **Data flow:** where state lives (client memory, MongoDB, Redis, Digital Ocean Spaces, on-chain Tezos), and what pieces can touch what.
- [ ] **The `code` command loop** — this deserves its own mini-diagram: piece → jump("terminal:claude") → PTY → Claude Code → writes to /mnt/ac-repo → next piece load picks up change.

---

## 6. Other places the AC Native story appears (sweep needed)

- [ ] [papers/arxiv-ac/](papers/arxiv-ac/) — AC overview paper
- [ ] [papers/arxiv-os/](papers/arxiv-os/) — OS-specific paper
- [ ] [papers/arxiv-notepat/](papers/arxiv-notepat/) — notepat paper
- [ ] [papers/joss-ac/](papers/joss-ac/) — JOSS submission
- [ ] [papers/ars-electronica-2026/](papers/ars-electronica-2026/) — Ars Electronica submission
- [ ] [papers/cc-demo-2026/](papers/cc-demo-2026/)
- [ ] [fedac/native/SCORE.md](fedac/native/SCORE.md) — internal; may be accurate
- [ ] [fedac/native/PROGRESS.md](fedac/native/PROGRESS.md) — internal status
- [ ] [fedac/native/internals.md](fedac/native/internals.md) — developer docs
- [ ] [fedac/native/device-claude.md](fedac/native/device-claude.md)
- [ ] [fedac/native/REPORTS.md](fedac/native/REPORTS.md)
- [ ] Root [README.md](README.md) (if it pitches AC Native)
- [ ] [aesthetic.computer/README.md](README.md), any landing page copy
- [ ] Prior grant applications / gig files — check [grants/](grants/), [gigs/](gigs/)

---

## 7. Open questions (flag for Jeffrey)

- **Boot time claim** — do we lean into "~10 seconds, a full creative OS from cold USB" (which is honest + still impressive for what it does), or do we aggressively target <2s before grant decisions come in?
- **Framebuffer vs DRM** — "bare-metal" is the artistic claim. Is DRM too technical for grant copy, or do we say "direct graphics rendering, no compositor" and leave DRM for the technical appendix?
- **Claude Code framing** — "AI coding partner in the terminal" vs "framebuffer-native AI"? The first is honest; the second overclaims.
- **notepat line count** — which version do we cite? (Web: 8,466; Native: 6,006.) Or total AC lines that make notepat possible?
- **Platter** — does this exist as a concrete artifact, or should we assemble one during the refresh?

---

## Working notes / log

- **2026-04-21 14:00** — First pass on LACMA proposal: stats refreshed (371 pieces, notepat 8466), Anthropic hook added, bio front-loaded, sosoft cards replace Fig 3/4. PDF recompiled.
- **2026-04-21 14:30** — Audit done against current `fedac/native/`. Multiple claims need softening (boot time, framebuffer, Claude Code wording). File created to track long-running refresh.
- **2026-04-21 14:30** — LACMA-critical copy fixes applied to [lacma-2026.tex:127](grants/lacma-2026/lacma-2026.tex) and [LACMA-2026-APPLICATION-DRAFT.md](grants/lacma-2026/LACMA-2026-APPLICATION-DRAFT.md):
  - Removed "in under two seconds" boot claim (was ~7.3s actual)
  - Changed "renders directly to the framebuffer" → "renders graphics through DRM without a compositor"
  - Changed "reads input from raw device events" → "reads input from raw evdev streams"
  - Strengthened "polyphonic" → "32-voice polyphony"
  - Changed "launches Anthropic's Claude Code directly on the framebuffer" → "drops into a native terminal running Anthropic's Claude Code" (honest, still has the Anthropic punch)
  - Added explicit features that were under-represented: eight waveforms, sample recording, USB + UDP MIDI
  - Added "twenty other pieces ship alongside it" (previously only notepat was mentioned)
  - Updated Fig 5 caption — removed "in under 2 seconds" and "any x86 machine" (now "x86_64 UEFI machines")
  - Added caveat "without replacing the factory OS" — accurate to how AC Native boots from USB
  - x86_64 escaped as x86\_64 in tex (underscore bug)
  - Final word count: 492 (was 522 pre-trim) — tightened last clause from feature-list to just "twenty other pieces ship alongside it"
- **2026-04-21 14:38** — Swept the three 100-word statements. Found & fixed one stale claim in Technology & Culture: "boots in two seconds" → "boots directly into a single piece of art software." Artistic Merit's parenthetical "(custom kernel, framebuffer rendering, sample-level audio synthesis)" — kept as lay-language since it's philosophical, not technical. Public Engagement statement was clean.
- **2026-04-21 14:44** — Converted all figures PNG → JPEG via `sips` (macOS). Built a 4-card composite `card-gallery.jpg` via a tiny LaTeX stub so the 4 cards compress into a single image slot. Final 5 JPEGs staged in `grants/lacma-2026/jpegs/submit/`: platform-screenshot, kidlisp-featured, card-gallery, card-berz (close-up), hardware-yoga.
- **2026-04-21 __:__** — _(next: video demo filming by @jeffrey; then submit)_

---

## How to use this file

- **When you edit the LACMA tex**, cross off items in §2.
- **When you touch another paper / landing page**, add it in §6 and record what you changed.
- **When audit reality changes** (we reach <2s boot, we add a new feature), update §1 — it's the source of truth.
- **When ambiguity bites you**, add a question to §7.
- Don't delete items — mark them done with ✅ or ❌ (won't do) so the history stays.
