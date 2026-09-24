# Femrag++ in the round

A playable **146.67-second sample study** for the Notepat spatial sequence:
144 BPM, 88 bars, six laptop positions and a dedicated sub lane. The local
stereo audition is ready; fleet sample playback and live DMX are **not connected**.

```sh
python3 -m http.server 8793 --bind 127.0.0.1 --directory grants/culturehub-la-2026/femrag-spatial
```

Open `http://127.0.0.1:8793/` and press Play. Master starts at 25%; choose a
section or solo a seat/sub. Playback does not start automatically. The diagram
shows each struck note in large type, with small warm intensity changes.
Headphones collapse the room into stereo; this does not audition six physical
speakers or measure sub timing. Stop cancels queued and active audio.

## Source and sound

The source is the studio's released Femrag++ in `pop/maytrax`, documented in
[`pop/RELEASES.md`](../../../pop/RELEASES.md). The event map and original
instrument implementation already exist locally. Credits: FEM bell modes from
`pop/bell`; label stamp spoken by Prutti. The source provides provenance, not a
new license or independent clearance for the voice. This internal study retains
those credits and adds no third-party sample packs.

`build.mjs` extracts only the original instrument functions and renders an
11-sample mono PCM bank (about 1 MB), then maps the original musical event times
to `score.json`. It runs in about one second here; it never renders the full
master or invokes a video encoder. Rebuild with:

```sh
node grants/culturehub-la-2026/femrag-spatial/build.mjs
```

The optional `render-stems.mjs` exports seven mono 48 kHz PCM16 WAVs to
`.tmp/femrag-spatial-stems/`, with per-stem hashes and peaks in `manifest.json`.
A validation render completed in under a second: 98,560,322 bytes across seven
stems, maximum absolute PCM peak 0.06442. Generated stem WAVs were removed
after verification to keep the workspace light; the exporter remains.
**25% master is baked into these stems**: do not apply another 25% master
without compensating, or change the exporter to emit unity-master stems. The
standalone browser instead applies 25% live. Native ten-second limits still
prevent direct full-stem loading.

The map has **2,947 events**. Negative-time events from the removed intro are
excluded. The renderer logs each donk twice (donk plus its nested sub); the
export keeps one donk. Score metadata includes the source event SHA-256.

This is a sample rearrangement, not bit-identical stem separation. Bell pitches
transpose a C4 strike, changing its overtone decay with pitch. Reverse bells,
risers and sustained bass use bounded prototype samples, so their envelopes
and wobble differ from the source. The source event map does not encode every
render parameter. The seven outputs therefore require an ear-led mix pass
before replacing the released arrangement. The browser has a shared limiter;
25% means software master, not acoustic level.

## Sequence and space

Insert this after the orchestral movement, with a two-bar shared-clock handoff:
let the orchestra release, announce “Femrag plus plus, in the round,” then
place the first drop on the next agreed downbeat. Do not overlap conductors.
The two-bar transition is a proposed sequence placement, not inserted into the
existing orchestral score.

| Time | Section | Motion |
| --- | --- | --- |
| 0–26.67 | First drop | Bell strikes pass clockwise around five perimeter laptops |
| 26.67–40 | Breakdown | Center throat answers the ring; sub stays spatially fixed |
| 40–46.67 | Buildup | Center riser gathers the ring into the next drop |
| 46.67–66.67 | Second drop | Clockwise bell relay; drum location changes each bar |
| 66.67–133.33 | Ragga sequence | Bell direction reverses; center donks anchor the swung rhythm |
| 133.33–146.67 | Outro | Ring resolves; sub and candlelight release together |

The seat order exactly follows the
[connection contract](../macneopolitan/fleet/CONNECTIONS.md): left front,
right front, right rear, left rear, center rear, held center. Bell/reverse-bell
notes take successive ring seats. Snare changes seat once per bar, with hats
two seats further around. Voice, throat, riser and donk belong to held center.
Kick and pitched sub belong only to the dedicated sub lane, filtered 25–80 Hz
in the preview. Sample bass is already at its intended pitch: **do not apply
the existing synth receiver's extra octave-down rule**.

Keep SUB on the same score clock and schedule buffered starts ahead, rather
than using animation frame callbacks as musical triggers. Use the measured
clock calibration from the connection contract; any negative timing offset
remains an estimate until a microphone or loopback verifies physical onset.

## Light and screen score

The manifest carries the existing fixture map: d001/d011/d031/d021 for seats
0–3, none on seat 4, and independent USB d041 on held center. The proposed warm
profile has room ceiling 96 and center ceiling 192, with 120 ms entrances and
450 ms releases. Layer small seeded candle drift above a continuous warm floor;
close hits raise the envelope instead of restarting from black. No strobing.
The web preview illustrates this treatment; it sends no DMX. Integrate the
existing candlelight function and bounded queue adapter before running it live.

For note typography, the preview reuses one canvas at 30 FPS; there are no
movies, image decoding, shaders or per-note DOM nodes. Hardware backlight can
follow a smoothed per-seat amplitude envelope through the shared brightness
controller, returning to the current 0% idle baseline; the preview only changes its drawn circles and does not assert
hardware support.

## Native transport work still required

Current native `sound.sample.loadData` swaps one shared sample buffer
(`fedac/native/src/audio.c`), limited to ten seconds
(`fedac/native/src/audio.h`). Loading a new instrument while old sample voices
play cannot serve as an independent immutable sample bank. The existing
`.nsscore` synth scheduler and SUB bass/kick filter also do not consume this
sample-event manifest.

The useful next implementation is immutable sample handles with per-voice
buffer ownership, bounded ahead-of-time sample scheduling, and cached preload
receipts per device. Alternatively render one continuous mono stem per seat
and add shared-clock streaming/seek support; the current ten-second buffer
cannot hold this movement. In either case all seven receivers must acknowledge
the same score hash and scheduled origin before any audio or DMX starts.
No fleet upload, live cue, USB change or reboot is part of this study.

Validation: event/sample checks passed; headless Chrome loaded all 12 sections
and eight output choices with no page errors. The idle screenshot was inspected.
No live or local audible playback was started; listening acceptance is pending.
