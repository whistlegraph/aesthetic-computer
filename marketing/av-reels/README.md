# av-reels/ — audiovisual synthesis reels

Turn a live aesthetic.computer instrument (or a `merry` tour) into a 9:16
Instagram Reel that carries AC's **real synthesized audio** — not a silent
capture with music laid on at post time (that's the sibling
[`kidlisp-reels/`](../kidlisp-reels/)). These reels *sound like the piece.*

```
av-reels/bin/
  capture-av.mjs     # drive a live piece + perform it → base-<slug>.mp4 (video + AC audio)
  performances.mjs   # named input "scores" (notepat-melody, bubble-taps, keys-generic)
  stamp-reel.mjs     # wrap a base mp4 in pals side-stamp chrome → <slug>-reel.mp4 (keeps audio)
```

## Pipeline

```bash
# 1. capture (headless, from production) — inject input so the instrument sounds
node marketing/av-reels/bin/capture-av.mjs notepat --perform notepat-melody --duration 10
# 2. stamp — pals side-stamps + climbing title + audio-driven LED pulse
node marketing/av-reels/bin/stamp-reel.mjs marketing/av-reels/out/notepat/base-notepat.mp4 \
  --title notepat --out ~/Desktop/av-notepat-reel.mp4
```

## How it captures audio headlessly (the trick)

Headless Chrome can't record a real speaker, but it CAN record audio that never
leaves the page. `capture-av.mjs`:

- **audio** — `evaluateOnNewDocument` patches `AudioNode.prototype.connect`
  *before AC boots* so every node routed to `ctx.destination` also tees into a
  per-context `MediaStreamDestination`. An in-page **audio-only** `MediaRecorder`
  records that (survives the headless rAF throttle, which runs on the audio
  thread). It tees **every** AudioContext — AC makes two; the 48 kHz one is the
  synth bus.
- **video** — CDP `Page.startScreencast` → timestamped PNG frames. (`canvas
  .captureStream()` emits nothing headless; MediaRecorder's mp4 muxer emits empty
  blobs — so: webm audio + screencast video, muxed by ffmpeg concat at true speed.)
- **perform** — AC instruments are **silent without input**. A "performance" is a
  timed list of key/mouse actions that plays the instrument.

## Performances

Pass a score three ways: a built-in `--perform NAME`, inline `--perform-json '[...]'`,
or `--perform-file path.json`. Action shapes (`t` = ms from capture start):

| action | plays |
|---|---|
| `{t,type:"down",key}` / `{t,type:"up",key}` | hold/release a keyboard note |
| `{t,type:"press",key}` | quick note |
| `{t,type:"tap",x,y}` | click at normalized (0..1) canvas coords |
| `{t,type:"mdown",x,y}` / `{t,type:"mup"}` | press-and-hold a pointer (hold-to-trigger pieces, e.g. fartflower) |
| `{t,type:"type",text}` | type a string |

Some pieces autoplay from URL params and need no performance, e.g.
`clock:0.5~{square}cegcdfdefgec` plays a square-wave arpeggio on boot.

## `merry` tours

`capture-av.mjs 'merry 6-notepat 6-bubble' --slug tour --perform-file score.json`
types the merry command at the prompt (auto-uses a 5 s settle so AC boots first),
then your performance must land actions *inside each piece's time window*
(notes 0–6 s, taps 6–12 s). `merry` is AC's built-in tour sequencer.

## Proven reels (all verified with real audio)

| reel | trigger | mean vol |
|---|---|---|
| notepat | `--perform notepat-melody` | −6 dB |
| bubble | tap sweep (pan/pitch climb) | −18 dB |
| fartflower | `mdown`/`mup` hold flurry (blooms need a held press) | −18 dB |
| clock | `clock:0.5~{square}cegcdfdefgec` (autoplay) | −5 dB |
| tour | `merry 6-notepat 6-bubble` + windowed score | −10 dB |

Outputs (`out/`, Desktop mp4s) are regen-able, so `out/` is gitignored per the
marketing README convention. Reels are silent-furniture-free (no progress bar /
timecode); IG brings its own.
