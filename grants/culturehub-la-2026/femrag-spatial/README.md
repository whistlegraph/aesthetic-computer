# Femrag++ in the round

A playable **146.67-second sample study** for the Notepat spatial sequence:
144 BPM, 88 bars, six laptop positions and a dedicated sub lane. The local
stereo audition and full-fleet deck playback are prepared. All six native
receivers and the Windows sample SUB passed readiness at 25% on 2026-09-24.
Frisbee owns the announced cue; completion requires a run receipt.

Xbox/Oskiewar **visual dances are connected** through Neo's existing stage
service. Use the connected audition on Blueberry:

```sh
node grants/culturehub-la-2026/femrag-spatial/serve.mjs
```

Open `http://192.168.1.234:8796/`. Play publishes the browser audio clock;
Stop releases the display. Six colored figures dance to routed notes, gather
during buildup, reverse their orbit in ragga sections, and soften in the outro.
Sub hits swell a floor line. This visual connection adds no Xbox audio or DMX.
The separate port 8793 static preview below remains local-only.

The bridge publishes `GET /api/performance`; its transport heartbeat expires
after 1.25 seconds. Neo polls it through `config.femrag`, then uses the existing
AC→Xbox stage packets. Display mode is `auto`, curtain remains raised. A
coalescing browser sender bounds pending requests; compact note tuples avoid
the oversized packet failure observed with full event objects. Future note
cues cover up to one second; network/display latency is not acoustically
calibrated. The current paired LAN session expires at midnight and must be
re-established for another day.

Verified 2026-09-24: native AC and Xbox both acknowledged `femrag-round-v1`;
Xbox screenshot inspected with six dancing figures and section/time footer.
Both acknowledged the return after Stop; the feed returned `null`.
Verification was a silent visual-only probe, not a full-fleet audio audition.

`install-oskiewar.mjs` applies the small source/config integration on Neo and
keeps `.pre-femrag` backups. It preserves other local Oskiewar work. Run it on
Neo before its existing paired hot-deploy workflow; restart the stage service
when changing its source/config. `oskiewar-dance.js` is the renderer source.
`run-fleet.py` publishes its actual downbeat clock to this feed. Do not play
the standalone browser audition while the fleet conductor owns the display.

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

`render-stems.mjs` exports seven mono 48 kHz PCM16 WAVs to
`.tmp/femrag-spatial-stems/`, with hashes and peaks in `manifest.json`.
The stems use unity master (maximum PCM peak 0.257672); the shared live
25% control is applied once at playback. Each stem is 146.666688 seconds.
Native `sound.deck` loads these long files independently of the ten-second
`sound.sample` buffer. Windows decodes the dedicated SUB stem before arming;
its existing both-channel output, filter, limiter and master remain in use.

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
The web preview sends no DMX. The fleet plan contains room envelopes; the
conductor drops cues more than 200 ms late or when its queue estimate reaches
eight. Held-center light envelopes run locally with a continuous warm floor.

For note typography, the preview reuses one canvas at 30 FPS; there are no
movies, image decoding, shaders or per-note DOM nodes. Hardware backlight can
follow a smoothed per-seat amplitude envelope through the shared brightness
controller, returning to the current 0% idle baseline; the preview only changes its drawn circles and does not assert
hardware support.

## Fleet operation

`prepare-native.mjs` builds the hashed plan and six configs from the unity
stems. `deploy-native.py` performs local validation by default; `--deploy`
requires all six receivers to be idle, uploads 4 MB chunks with readback hash
checks, and loads each deck without playing. Battery/charging indicators,
large note labels and the existing composition-volume polling are retained.

`install-sub.py <existing-sub-runtime-folder>` adds the fixed stem route,
server-side checksum validation, browser predecode and stem-ready heartbeat.
Restart that server and reload/re-arm the browser after applying it, while
idle. The runtime expects `.tmp/femrag-spatial-stems/sub.wav`. Never substitute
the dummy zero-gain synth event for the actual stem: it is compatibility
metadata for the older score validator, not the bass part.

Frisbee's `~/Shelf/femrag-spatial/` holds `plan.json`, `manifest.json`,
`native-loaded.json`, `sub-score.json` and `run-fleet.py`. Its existing SSH
forwards reach Blueberry SUB8791 and Neo DMX8790. The runner defaults to a
silent check. Once the rig owner is ready:

```sh
FEMRAG_OUT=/Users/jas/Shelf/femrag-spatial \
TRIO_SUB=http://127.0.0.1:8791 TRIO_DMX=http://127.0.0.1:8790 \
python3 /Users/jas/Shelf/femrag-spatial/run-fleet.py --run
```

The runner checks hashes, decoder duration, volume, fullscreen SUB, live DMX
and a clock round trip below 40 ms for every seat. It announces on the
conducting Mac, schedules a 15-second lead, keeps receivers alive and records
status every two seconds. Any native error stops the run. Cleanup stops six
decks and SUB, releases the visual feed and requests acknowledged DMX blackout.
Deck starts are dispatched on simulation frames; acoustic onset is not calibrated.
No Mac singers, USB image changes or reboots are required for this arrangement.

Validation: 2,947-event sample checks and three visual-feed tests pass; all
six decks verified ready with matching hash, duration and volume; Windows
reported matching decoded stem, armed, fullscreen and both channels at25%.
The silent visual probe was acknowledged on AC/Xbox and visually inspected.
Listening acceptance and the full-run receipt are still pending.
