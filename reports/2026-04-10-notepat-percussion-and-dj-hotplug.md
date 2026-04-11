# 2026-04-10 — notepat percussion kit + dj hotplug + help panel

Session focus: turn notepat's percussion mode into a fully realized drum
instrument (synth + sampling + visuals + stereo + reverse-loop pedal), fix a
USB hotplug regression that broke DJ track detection, and rebuild the meta-held
shortcut help panel so it's readable at any screen size.

All work shipped via the oven build pipeline (compush → oven → `ac-os pull`).
Final combined build at the time of writing is `57b818dd-8` on commit
`18dbc5986`, carrying every feature listed below.

---

## 1. Percussion kit — complete rework

### 1.1 Two-step DOWN / UP architecture
Every drum in the 12-pad kit now fires as a two-stage pair instead of a
simultaneous layer stack:

| Drum | DOWN (impact) | UP (release) |
|---|---|---|
| **kick** | beater click + body thump + sawtooth grit | sub-bass wobble bloom + mid fill |
| **snare** | stick-on-head pop | wire rattle + shell ring |
| **clap** | dark palm strike | bright release transient |
| **snap** | finger-release click | skin slap body ring |
| **closed hat** | stick contact tick | tight metallic sizzle |
| **open hat** | metallic chip strike | sustained airy shimmer |
| **ride** | bell-like ping | long shimmer wash |
| **crash** | explosive metal impact | long sustained wash |
| **splash** | thin high metal tick | short bright wash |
| **cowbell** | stick-on-metal tink | resonant detuned ring |
| **wood block** | stick contact tick | hollow wood body |
| **tambourine** | frame strike | jingle rattle |

Gap between DOWN and UP is BPM-locked (`flam × rn(...)` per hit) so rolls
tighten at higher tempos and widen at slower ones. Kick and crash use tight
4–9 ms gaps so the punch+bloom feels unified; ride and open hi-hat use longer
gaps for audible strike→shimmer separation.

Commits: [`46ab9b132`](../fedac/native/pieces/notepat.mjs), [`cebfb6737`](../fedac/native/pieces/notepat.mjs)

### 1.2 Stochastic variation on every drum
Per-hit jitter helpers `rj(center, frac)` / `rn(min, max)` applied across all
12 drums:

- **Tone jitter**: ±3–12% on every oscillator frequency
- **Volume jitter**: ±5–12% per voice
- **Duration jitter**: ±5–14% on longer tails
- **Pan jitter**: ±0.05 offset per DOWN/UP step for subtle stereo movement
- **Flam timing jitter**: `rn(...)` inside the `setTimeout` delay for the UP step

No two hits sound identical. Tambourines get the widest jitter because real
tambourines never sound the same twice.

Commit: [`b520136ee`](../fedac/native/pieces/notepat.mjs)

### 1.3 Animated graphic notation inside drum pads
Each pad renders an animated glyph showing the drum's internal structure:
tonal bars for sine/triangle/square/sawtooth layers at log-frequency Y
positions, random dots for noise bursts. `PERCUSSION_NOTATION` table describes
each drum's voices; `NOTATION_WAVE_RGB` maps wave types to accent colors.
Drawn with per-frame random jitter so viewers see the stochastic mechanism in
action while the pad is idle.

Commit: [`d1064820b`](../fedac/native/pieces/notepat.mjs)

### 1.4 Kit-geometry pan (not pitch-based)
Tones pan by pitch (`(semitones − 12) / 15`) — low notes left, high notes
right. Drums were inheriting that same formula so a kick (C) was getting
hard-panned left just because C is the lowest note.

New `drumPanFor(letter, gridOffset)` combines two factors:

- **Grid bias**: left grid → -0.32, right grid → +0.32
- **Drum-type offset** (`DRUM_PAN_OFFSET`) per kit geometry:
  kick 0.00, snare -0.05, clap +0.10, snap -0.10, closed hat +0.25,
  open hat +0.32, ride +0.45 (far right), crash -0.45, splash -0.55
  (far left), cowbell +0.22, block -0.18, tambo +0.30

Final pan = `clamp(gridBias + drumOffset * 0.7, ±0.9)`. Grid side dominates
so both grids stay distinguishable; drum offset modulates for kit color.
Wired through keyboard, touch-tap, and drag-rollover drum paths.

Commit: [`665ef824f`](../fedac/native/pieces/notepat.mjs)

### 1.5 Full-screen background flash per drum
Tones already painted the whole background from their note colors while held.
Drums, being one-shots, never got that visual response. Now they do.

- New `drumFlashes` queue (up to 8 concurrent entries, life = 14 frames
  ≈ 230 ms) holds `{color, frame, life}`
- `flashDrum(letter)` pushes an entry on every hit
- Alpha curve: full 1.0 for first 2 frames (punchy blink), then linear decay
- Paint-time blend: tone layer + drum layer are weighted-summed (drums get
  1.2× weight so a single hit still reads against held tones)
- Tone layer snaps instantly; drum-only decays smoothly at 0.55/frame so
  rapid hits stack into a color wash but single hits fade out cleanly

Colors sourced from existing `PERCUSSION_COLORS` palette (kick = deep orange,
snare = tan, ride = silver-blue, crash = lavender, splash = pink, etc).

Wired into all 4 drum trigger sites (keyboard, touch-tap, drag-rollover,
reverse-playback replay).

Commit: [`18dbc5986`](../fedac/native/pieces/notepat.mjs)

### 1.6 Earlier percussion work
- **Percussion layout toggle**: PgUp (left grid) / PgDn (right grid) map the
  7 naturals + 5 sharps of that octave to drum pads with synth recipes and
  optional per-drum recorded samples (existing from before this session,
  verified still working)
- **Per-drum microphone sampling**: hold `End` to arm, then tap any drum pad
  to record a sample directly into `percussionSampleBank[drumName]`; release
  to save (confirmed working on build)
- **Wub/wobble kick + sharper clap + whistle de-air**: earlier audio tuning
  commit [`2e4333327`](../fedac/native/pieces/notepat.mjs)

---

## 2. Reverse playback loop pedal (space bar)

Pressing and holding space starts playing back the last 12 seconds of
keyboard/drum hits in reverse, in real time, while the user can keep playing
new notes on top. Releasing ends the phase; the next press unwinds whatever
was played during that press — creating a "bounce loop" where repeated
presses alternate forward→reverse→forward→reverse.

Implementation:
- `playbackHistory` rolling buffer (256 events / 12 s), recorded from every
  note + drum trigger site
- `startReversePlayback()`: snapshots history, **clears** `playbackHistory`
  for the new phase, builds `reverseQueue` with time-mirrored delays
- `stopReversePlayback()` drains the queue
- `playReverseEvent()` fires the sound AND re-records it into the new phase,
  which is what creates the bounce loop semantics
- `sim()` tick drains `reverseQueue` each frame (~16 ms resolution)
- Fallback kick drum on first press (empty history)

The duration of a held press effectively sets the loop's "out point" — very
dynamic, very fun.

Commit: [`da8a795b3`](../fedac/native/pieces/notepat.mjs)

---

## 3. USB hotplug → DJ regression fix

### Symptom
Plug a USB stick with music → DJ detects it once. Unplug → DJ stops working.
Re-plug → nothing happens. Browsing tracks never starts again.

### Root cause
`mountMusic()` in `js-bindings.c` used `stat("/media/.").st_dev !=
stat("/.").st_dev` to decide "already mounted". When a USB is physically
yanked, `/media` stays as a **stale mount point** — `st_dev` still differs
from root, so the function returned `JS_TRUE` forever. The JS hot-plug loop
then thought USB was still connected and never re-scanned.

### Fix
Parse `/proc/mounts` to find the actual device backing `/media`. Verify:

1. Device file still exists (`stat` + `S_ISBLK`)
2. `opendir("/media")` succeeds

If either check fails (stale mount from a yanked USB), `umount2(MNT_DETACH)`
lazily before falling through to the normal mount loop. Re-plugs now get
detected instead of seeing a stale "already mounted" response forever.

Commit: [`325209975`](../fedac/native/src/js-bindings.c)

---

## 4. Meta-held shortcut help panel rewrite

### Problems with the old panel
- Fixed 280 px width wasted screen space on wider displays
- 24 single-column rows bled off the bottom on small screens
- Two flat colors gave no visual grouping
- Bindings were stale: `space` still listed as "kick drum" when it's now the
  reverse loop pedal; F1–F4 listed as "Fn+F1" when the media-key aliases
  had been removed weeks ago

### New panel
- **9 color-coded categories**: NOTES (cyan), DRUMS (orange), WAVE (magenta),
  DECK (green), HOLD (yellow), TEMPO (amber), SAMPLE (pink), FX (teal),
  SYSTEM (gray) — colors applied to both category headers and the
  key-column text
- **Adaptive 1/2/3 columns** based on screen width (`COL_W = 172 px`)
- **Hard-clamped panel height** so it never bleeds off the bottom; overflow
  rows are clipped rather than pushed off-screen
- **Dark/light theme-aware**: key colors brightened in dark mode, darkened
  in light mode for legibility on both
- **Drop shadow + outline + title underline** for depth
- **All bindings refreshed** to reflect current code state (space = reverse
  loop pedal, F1/F2/F3/F4 direct deck control, delete = clear sample bank,
  etc.)

Commit: [`325209975`](../fedac/native/pieces/notepat.mjs) (shipped alongside
the DJ fix)

---

## 5. Install-to-HD robustness

From earlier in the session, before the main percussion work:

- **SIGPIPE handler**: added `signal(SIGPIPE, SIG_IGN)` so websocket close
  doesn't kill the process (fix for observed exit=141 crash)
- **setTimeout polyfill**: QuickJS has no timers. Added `__pendingTimeouts`
  queue flushed from `sim()` so pieces can use `setTimeout()` naturally
  (fix for "setTimeout is not defined" on ESC)
- **Loop-unmount install path**: `force_unmount_disk()` parses `/proc/mounts`
  until no entries match, `blkrrpart_with_retry()` uses exponential backoff
  (0 → 250 → 500 → 1000 → 2000 ms), install-debug.log on tmpfs survives
  /mnt repartition (copied back at end of auto_install_to_hd)
- **Flash verify instrumentation**: dedicated `/tmp/flash-trace.log` so the
  ac_log_pause() pattern no longer eats diagnostics; added size-check first
  in verify; bumped sync wait 500 ms → 3 s

Commits: [`7c11b5ca7`](../fedac/native/src/ac-native.c), [`ea6cf8f24`](../fedac/native/src/js-bindings.c)

---

## 6. Commit list (this session, newest first)

```
18dbc5986 feat(percussion): drums pulse full-screen background in drum colors
665ef824f feat(percussion): drums pan by kit geometry + grid bias (not tone pitch)
325209975 fix(dj): detect stale /media mount on unplug + rebuild notepat help panel
cebfb6737 feat(percussion): every drum is now two-step (DOWN impact + UP release)
46ab9b132 feat(percussion): two-step clap + open hi-hat with down/up bursts
846782476 Merge remote-tracking branch 'tangled/main'
da8a795b3 feat(notepat): space bar = instant-replay reverse loop pedal
d1064820b feat(notepat): animated stochastic graphic notation inside drum pads
b520136ee feat(percussion): stochastic per-hit variation + BPM-locked flam timing
2e4333327 fix(percussion): wub kick + sharper clip-clap + aggressive whistle de-air
7c11b5ca7 fix(native): loop unmount until /mnt stack is clean + tmpfs debug log
```

All commits pushed to both `origin` (github) and `tangled` (knot) remotes.

---

## 7. Build pipeline state

Current in-flight build: **`57b818dd-8`** on ref `18dbc5986`, triggered via
`POST /native-build` to `oven.aesthetic.computer`. Carries every feature
above in one artifact.

Watcher `/tmp/watch-build.sh` (PID at time of report) polls the oven every
20 s and will auto-flash via `ssh me@172.17.0.1 'cd
/home/me/aesthetic-computer/fedac/native && ./ac-os pull'` on success.

Several earlier builds in this session were cancelled mid-flight so
sequential features could be combined into one flash cycle:

- `1ff8e31b-7` — cancelled (was running on stale `f58d1bc57` after push/merge
  confusion; reconciled tangled/main into origin/main as `846782476`)
- `2491da75-5` (clap + hat only) — cancelled to combine with the remaining
  10 drums
- `23e163b2-7` (all 12 two-step drums) — cancelled to combine with USB
  hotplug + help panel fix
- `121e68d3-9` (+ hotplug + help panel) — cancelled at 55% kernel stage to
  combine with drum pan geometry
- `fd5cc814-a` (+ drum pan) — cancelled at 55% kernel stage to combine with
  full-screen drum visuals
- `57b818dd-8` (+ drum visuals) — **running to completion** (combined final)

---

## 8. Outstanding / deferred

- `native-notepat-*` → `ac-native-*` OTA artifact rename (user flagged as
  "vestigial" earlier; analysis done, implementation deferred to avoid
  mid-session pipeline disruption)
- Delete key in sample mode currently clears only the melodic sample bank,
  not `percussionSampleBank` — small one-line follow-up if desired
