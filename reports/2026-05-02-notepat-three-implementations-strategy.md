# Notepat — Three Implementations, One Keymap

**Date:** 2026-05-02
**Author:** Claude (Opus 4.7)
**For:** @jeffrey
**Question:** notepat.com (web) is falling behind notepat-native and menuband in feature set — how should we proceed strategically?
**Sources:** `system/public/aesthetic.computer/disks/notepat.mjs`, `fedac/native/pieces/notepat.mjs`, `slab/menuband/Sources/MenuBand/`, `papers/arxiv-{notepat,keymaps,plork}/*.tex`, ~50 prior commits, prior reports (`notepat-audio-latency-report.md`, `notepat-stability.md`, `2026-04-10-notepat-percussion-and-dj-hotplug.md`, `2026-04-11-notepat-ota-usb-flash-report.md`).

---

## TL;DR

Notepat is now a **five-implementation family** (web, native, menuband, M4L, remote phone) sharing one social-software contract: the QWERTY-as-chromatic keymap. The web piece — historically the canonical implementation — has become the *laggard*: it has the worst audio latency on the platform (a measured **417 ms vs. <30 ms target**, 14× over budget), it lacks the sample-record / reverse-tape / FX-row work that landed in native over the past month, and the menuband app has overtaken it on UX polish (waveform strip, voice palette LCD, chord finder, localization).

The three papers (`notepat`, `keymaps`, `plork`) all hinge on a single invariant — **the keymap** — and treat everything else (synth, recording, network, hardware) as substrate-specific. That gives us permission to stop chasing literal feature parity and instead **specify the invariant explicitly, then position each implementation to its substrate's strengths**: web = gateway / share / pedagogy; native = performance instrument; menuband = always-on desktop companion; M4L = DAW citizen; remote = controller.

The critical near-term work is **(1)** finishing the AudioWorklet+WASM browser prototype to close the latency gap (commit `23c848771` started this — it's the only path back to a playable web), and **(2)** writing a `notepat-spec.md` so capabilities propagate by spec rather than by copy/paste.

---

## 1. The five implementations

| # | Implementation | Path | Substrate | LOC | Posture |
|---|---|---|---|---|---|
| 1 | **Web** (notepat.com) | `system/public/aesthetic.computer/disks/notepat.mjs` | Browser, Web Audio | ~8,737 | Reference / gateway |
| 2 | **Native** (AC Native OS) | `fedac/native/pieces/notepat.mjs` | QuickJS on bare metal, ALSA | ~7,259 | Performance instrument, default boot piece |
| 3 | **Menuband** | `slab/menuband/Sources/MenuBand/` (Swift) | macOS menubar, AVFoundation | — | Always-on desktop companion |
| 4 | **M4L** | `system/public/m4l/notepat.com.amxd` + recent worklet build | Ableton Live | — | DAW citizen |
| 5 | **Remote** | `system/public/aesthetic.computer/disks/notepat-remote.mjs` | Phone web piece, UDP relay → Max bridge | — | Controller / second screen |

Plus: `notepat-tv.mjs` (HDMI visualizer), `ac-electron` overlay mode (transparent floating widget — `402d953d9`), and offline WASM shells under `system/public/ac-native-wasm/notepat.html`.

The slide at `slides/notepat-keymap/template.html` already enumerates this registry. It is the closest thing we have to a published spec.

---

## 2. Capability matrix

Rows = capabilities. Cells = ✅ shipped, 🟡 partial, ❌ absent, — N/A for substrate.

| Capability | Web | Native | Menuband | M4L | Remote |
|---|:-:|:-:|:-:|:-:|:-:|
| **The keymap** (QWERTY → chromatic, two-octave hand layout) | ✅ | ✅ | ✅ | ✅ | ✅ |
| Self-documenting (letters name notes) | ✅ | ✅ | ✅ | ✅ | ✅ |
| Multi-touch / pointer pads | ✅ | ✅ | 🟡 (mouse only) | — | ✅ |
| Octave shift (1–9 + . , /) | ✅ | ✅ | ✅ (shift/capslock linger) | ✅ | ✅ (1-9 hot-switch) |
| **Synth voices** | 8 (sine/tri/saw/sq/harp/whistle/composite/stample) | 8+ Salamander Grand Piano sample bank | GM 128+ via MIDISynth | host-defined | — (MIDI only) |
| Drum kit / percussion mode | ✅ (9th mode) | ✅ (drum mode swaps upper octave) | 🟡 (loaded, not exposed) | — | ❌ |
| **Sample recording into per-key banks** | ❌ | ✅ (End-key arms) | ❌ | ❌ | ❌ |
| **Reverse tape / spacebar scrollback** | ❌ | ✅ (frozen ring between gestures) | ❌ | ❌ | ❌ |
| Mic hot-input | ❌ | ✅ (device autodetect) | ❌ | host-managed | ❌ |
| FX rows (room/glitch/drive/wobble/flange) | 🟡 (room/glitch only) | ✅ (full row + per-FX `0` reset) | ❌ | host-managed | ❌ |
| Sustain (Shift-decay 6 s, Enter-latch) | ✅ | ✅ | 🟡 | ✅ | 🟡 |
| Metronome (UTC-synced) | ✅ | ✅ (no spacebar gap) | ❌ | host-driven | ❌ |
| Autopat / song mode | ✅ | ✅ | ❌ | ❌ | ❌ |
| Hover overlay | ✅ | ✅ | ✅ (HoverLink chips, voice pill) | — | ❌ |
| MIDI in | ✅ (relay socket) | ✅ (USB) | ✅ (Virtual "Menu Band" source) | ✅ | ✅ (M4L bridge) |
| MIDI out | ❌ | ✅ (UDP heartbeat → relay) | ✅ (publishes virtual source) | ✅ | ✅ |
| **Audio latency** | **~417 ms** ⚠️ | **<10 ms** (ALSA mmap + CPU pin) | **near-zero** (AVAudioEngine) | host-bounded | network-bounded |
| AudioWorklet/WASM path | 🟡 (prototype `23c848771`) | — | — | started | — |
| Theming | octave colors | octave + handle colors | light/dark + Terminal | — | track-color flavored board |
| Bandmate sprites (Piano Man / Sample Sally / harpist) | ✅ (eye-tracking) | ✅ (per-wave) | ❌ | ❌ | ❌ |
| Localization | ❌ | ❌ | ✅ scaffold | ❌ | ❌ |
| Chord finder | ❌ (lib imported, unused) | ❌ | ✅ integrated | ❌ | ❌ |
| Waveform/LED visualizer | ✅ (oscope) | ✅ | ✅ (Metal shader strip + popover) | host-side | ✅ |
| NuPhy Air60 HE pressure → velocity | ✅ (WebHID) | ✅ (analog detect) | ❌ | — | — |
| Network multiplayer / ensemble | 🟡 (UDP to notepat-tv) | 🟡 (UDP MIDI broadcast) | ❌ | session-server | ✅ (M4L bridge) |
| Recall / undo | ❌ | ❌ | ❌ | host | ❌ |

The matrix surfaces the shape of the problem clearly: **web has the broadest visual feature set but the worst latency and no recording**; native has caught up on visuals and overtaken on audio engine + recording; menuband has eclipsed both on UX polish and is the only one with localization or a chord finder.

---

## 3. The papers' rubric

The three papers commit to a single thesis: notepat works because it externalises one social-software object (the keymap) onto every available substrate, and lets the substrate do what it's good at. From them we can extract a non-negotiable invariant set:

1. **Keymap is identical across implementations.** QWERTY letters name notes; left hand = lower octave, right hand = upper; dedicated rows for sharps. (`keymaps.tex`)
2. **Self-documenting.** No manual required. Anyone who knows C-D-E-F-G-A-B knows the layout. (`notepat.tex` + `keymaps.tex`)
3. **Playable on first power-on.** No configuration; no install (or, for native, *boot is the install*). (`plork.tex`)
4. **Built-in synthesis at the substrate's native floor.** Web Audio for browser, ALSA mmap for bare metal, AVFoundation for macOS. The instrument *uses the substrate as instrument*. (`notepat.tex`)
5. **Diatonic letter inheritance.** Western pedagogy notation is preserved; notation cost = 0. (`keymaps.tex`)
6. **Forkable.** A keymap is a small declarative table. The platform must let users author alternatives (this is the Lialina "Turing Complete User" claim and is currently *unmet*). (`keymaps.tex`)
7. **Convivial.** Used by anyone, without specialised training, to accomplish purposes they determine. (`plork.tex` quoting Illich.)

Things the papers explicitly leave to the implementation:

- Synthesis voices (the menu can vary per substrate)
- Recording / sequencing affordances
- Network / multiplayer model
- Visual identity / theme
- Hardware probes (NuPhy, USB MIDI, mic, deck)
- Pedagogy surface (song mode, autopat)

In other words: **the papers permit divergence on every axis except the keymap**. We are allowed to let web, native, and menuband each be best at different things.

---

## 4. Diagnosis: why web is behind, and what kind of "behind" it is

There are three different gaps stacked on top of each other and they need different treatments:

**Gap A — Audio floor (existential).** Web latency is 417 ms ([`notepat-audio-latency-report.md`](notepat-audio-latency-report.md)); the paper's pedagogy claim and the `plork` ensemble claim both require <30 ms. Native solved this with ALSA mmap + audio CPU pin (`9cf58384a`). Menuband never had the problem (AVAudioEngine). Web has a half-finished AudioWorklet+WASM prototype (`23c848771`). **Until this gap closes, every other feature added to web is being added to an instrument that fails the papers' minimum playability test.**

**Gap B — Audio engine richness (real but bounded).** Native has shipped the Salamander Grand Piano sample bank (`e4895fc02`), a proper Karplus-Strong banded waveguide piano (`b0815cc06`), wobble/flange (`66452b605`), drive with tanh soft-sat (`ea57b7b77`), zoo + laser kits with researched DSP (`fa1a60f65`), per-FX `0` reset (`7c67fc6c7`), reverse-only spacebar loop with frozen-history-between-gestures (`b407b7a2b`, `510ae5a1a`). Web has gotten harp + whistle ported back (`596dfbc1b`) and shares percussion via `lib/percussion.mjs` (`1828b31db`) — that's the right precedent — but the rest is one-way. **The fix is ongoing back-porting via shared libs.**

**Gap C — UX vocabulary (small but visible).** Menuband has chord finder, voice palette LCD, waveform strip, hover-link chips, localization scaffold. Web has none of these and the menuband team is moving fast. **This isn't urgent — menuband is allowed to lead on desktop UX — but a few of these (chord finder, waveform strip in the spirit of menuband's) make sense to mirror in the web piece since the lib is already imported and unused.**

There is also a **structural gap**: there is no shared spec. Capabilities have propagated by copy-paste between five implementations. The slide at `slides/notepat-keymap/template.html` is the closest we have to a published contract, and it's a slide. This is fine for three implementations; with five (and a sixth coming if VST3 happens), **it stops scaling**.

---

## 5. Strategic options

**Option A — Backport-first.** Treat web as an implementation that needs to catch up. Port sample recording, reverse tape, FX rows, Salamander, chord finder. Pro: keeps the "five implementations of one piece" story tidy. Con: web's latency makes most of these features feel worse than on native; you'd be polishing a slow instrument.

**Option B — Audio engine consolidation via shared lib.** Extend the `lib/percussion.mjs` precedent: factor each voice (harp, whistle, piano, drums, FX chain) into a substrate-agnostic spec under `shared/notepat/voices/`. Each substrate adapts: web → AudioWorklet, native → ALSA, menuband → AVFoundation. Pro: structurally sound; addresses Gap B properly. Con: large refactor; doesn't fix Gap A.

**Option C — Latency floor first.** Finish the AudioWorklet+WASM prototype before any new features land in web. Pro: required by the papers; unblocks everything else. Con: hard, deeply technical work in browser audio.

**Option D — Repositioning.** Stop trying to bring web to feature parity. Curate it to its strengths (URL-shareable, no-install, instant-share, song mode for kids, gateway-into-the-platform) and let native + menuband own the performance-instrument and desktop-companion roles. Publish this as a *design decision*, not a regression. Pro: honest about what each substrate is for; aligns with the papers' permission to diverge. Con: requires a public framing change for notepat.com.

**Option E — Spec-first.** Write `notepat-spec.md` (under `shared/` or `papers/`) and treat each implementation as a *conformance target*. Mark each capability as REQUIRED (the keymap, self-documenting, synthesis at substrate floor) vs. RECOMMENDED (FX rows, hover) vs. OPTIONAL (mic, deck, kiosk). Pro: turns ad-hoc feature drift into intentional divergence; catches up the keymaps.tex "forkable" claim. Con: bureaucracy if over-applied.

---

## 6. Recommendation

**Do C + E + B in that order, plus D as a public framing.** Skip A.

1. **Finish AudioWorklet+WASM web build (Gap A).** This is the existential blocker. Until web hits <30 ms, nothing else matters. The prototype exists (`23c848771`); the path is known.
2. **Write `papers/notepat-spec.md` (or `shared/notepat/spec.md`)** that names the keymap as the only true REQUIRED, lists RECOMMENDED capabilities (drum mode, sustain semantics, octave-shift keys), and OPTIONAL ones (sample recording, mic, deck, NuPhy pressure, kiosk). Make it short — one page. Put the slide diagram in it.
3. **Land Option D as a paragraph on notepat.com** — "web notepat is the gateway; the desktop companion is menuband; the studio instrument is native. They share a keymap." Stop framing native and menuband as "ahead"; frame them as *different organs of the same instrument*.
4. **Then begin Option B (shared voices lib).** Salamander Grand Piano, harp, whistle, drum kit, FX chain — substrate-agnostic spec, three adapters. Use `lib/percussion.mjs` as the template.
5. **Defer Option A** (broad backport into web) indefinitely. Backport selectively only when the shared-voices refactor naturally lifts a feature into web for free.

Two specific things to ship cheaply along the way because the cost is near-zero:
- Wire up the `lib/chord-detection.mjs` import that's already sitting in `notepat.mjs` (currently imported and unused). Menuband already has chord finder — getting web to display chord names while you're playing is a 50-line diff.
- Mirror menuband's waveform strip on web. The oscilloscope (scope=16) already exists; the menuband design just makes it readable.

---

## 7. Concrete next-step queue (ranked)

1. **AudioWorklet+WASM web build to playable state** (closes Gap A). Owner: web. Blocker on everything else.
2. **`notepat-spec.md`** (one page, REQUIRED/RECOMMENDED/OPTIONAL). Owner: jeffrey. Two hours of writing.
3. **notepat.com landing copy update** explicitly positioning web vs. native vs. menuband vs. M4L vs. remote. Half a day.
4. **Shared voices lib scaffold** — `shared/notepat/voices/{harp,whistle,piano,drum,fx}.mjs` with adapters. Move existing `lib/percussion.mjs` into it.
5. **Wire `chord-detection.mjs` into web** (already imported, never called).
6. **Menuband-style waveform strip in web** (visual parity, cheap).
7. **`notepat-keymap` slide → `notepat-spec.md` figure**, so the registry is part of the spec, not just a slide.
8. **Forkable keymaps as a real feature.** keymaps.tex commits to this; no implementation supports it yet. Could be as simple as a URL param `?keymap=dvorak` that loads a JSON table from `shared/notepat/keymaps/`.
9. **Recording-as-feature decision.** Sample recording exists only in native; the papers don't require it. Either backport (via shared lib) or formalise that the studio instrument records and the web instrument shares URLs. Pick one.
10. **`notepat-tv` is a separate piece — keep it that way**, but document it in the spec as "OPTIONAL secondary display target."

---

## 8. Things to flag

- **The latency report is from a single measurement run.** Re-measure on the AudioWorklet prototype before deciding the fix worked. The stability report explicitly notes that latency telemetry isn't currently collected after code reloads.
- **The slide has five implementations; the papers describe three substrate classes (browser, bare metal, host).** M4L and remote are sub-cases. Worth deciding in the spec whether they're "implementations" or "channels."
- **`notepat-remote.mjs` has had ~30 commits in the last few months** — it's the most actively iterated implementation after menuband. Worth a separate mini-report on what it's converging toward.
- **`drum mode swaps only upper octave` (`8f629669b`) is a native-only behaviour.** Web's drum mode (the 9th wave) replaces the entire keyboard. Pick one and put it in the spec.
- **The spec doc doesn't exist yet.** Until it does, every conversation about "is X a notepat feature" is going to be re-litigated per implementation.

---

## 9. References

- `system/public/aesthetic.computer/disks/notepat.mjs` (web, ~8,737 LOC)
- `fedac/native/pieces/notepat.mjs` (native, ~7,259 LOC)
- `slab/menuband/Sources/MenuBand/` (Swift macOS app, v0.9)
- `system/public/menuband/index.html` (landing only)
- `slides/notepat-keymap/template.html` (registry slide)
- `papers/arxiv-notepat/notepat.tex` (origin / instrument-platform co-evolution)
- `papers/arxiv-keymaps/keymaps.tex` (keymaps as social software)
- `papers/arxiv-plork/plork.tex` (planetary laptop orchestra; notepat as default boot piece)
- `reports/notepat-audio-latency-report.md` (417 ms measurement)
- `reports/notepat-stability.md` (3,229-note stress test passed)
- `reports/2026-04-10-notepat-percussion-and-dj-hotplug.md` (drum kit + USB hotplug)
- `reports/2026-04-11-notepat-ota-usb-flash-report.md` (OTA flash regression)
- Key commits: `23c848771` (AudioWorklet WASM prototype), `1828b31db` (shared percussion lib precedent), `596dfbc1b` (harp + whistle backport), `e4895fc02` (Salamander), `9cf58384a` (ALSA mmap + CPU pin), `b407b7a2b` (frozen reverse-replay history), `82440e7c8` (menuband i18n + hover chips), `13c6bdfb2` (menuband chord finder).
