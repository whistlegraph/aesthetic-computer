# Talking-Head Sampler Feasibility Report

**Date:** July 12, 2026
**Subject:** Chopping a YouTube lecturer's speech, live, beat-locked to whatever EDM is playing — with an LLM as the crate-digger

---

## Executive Summary

**Verdict: ✅ FEASIBLE — and most of it is already written.**

The feature @jeffrey described ("interpolate a youtube talking head video on an audio ring buffer using whisper to relisten over an active beat… for live rapping of edutainment content over whatever EDM I'm DJing") decomposes into four parts. Three of them already exist inside **Menu Band**, in files that have never been introduced to each other:

| Part needed | Already exists as | File |
|---|---|---|
| A beat clock derived from music playing in the room | onset detection + autocorrelation BPM (70–160) + **phase-locked beat clock**, fed by a live mic tap | `slab/menuband/Sources/MenuBand/MenuBandMicTempo.swift:58-99` |
| A streaming audio source that behaves like a playable, pitch-shiftable instrument voice | KPBJ radio: long-lived HTTP stream → hand-decoded → **ring buffer** → playable as "voice −1", pitch-shifted per keypress via `AVAudioUnitTimePitch` | `slab/menuband/Sources/MenuBand/KPBJRadioStream.swift:72-115, 351-358` |
| Capturing a buffer of audio and immediately making it a playable voice | mic → ≤10 s buffer → playable sample voice | `slab/menuband/Sources/MenuBand/MenuBandSampleVoice.swift:280-404` |
| **Word-level index of the speech** | ❌ nothing in the live runtime — but the offline pipeline is mature (see below) | — |

Menu Band is *already* a streaming-audio-as-a-pitch-shiftable-instrument app with a mic-driven beat clock. The talking-head sampler is those three files wired together, with a word index replacing "notes."

**The single biggest finding: `KPBJRadioStream` is the YouTube mode, already built.** It streams a remote audio source into a ring buffer and lets you play it from the QWERTY keyboard, pitch-shifted. Swap the Icecast URL for a `yt-dlp`'d lecture and swap "pitch by key" for "trigger word slice by key" and you have the instrument.

---

## The architectural decision that matters: two clocks

Everything else follows from this.

There are **two clocks in this system and they must never touch:**

- **The audio clock** — sample-accurate, sub-millisecond, lives in `AVAudioEngine` (Swift) or the AC speaker worklet. This is what fires a word slice on the downbeat.
- **The LLM clock** — one to three *seconds*, conversational, lives in an MCP tool call.

**The LLM must never be in the audio path.** A model cannot decide, in the 6ms before a downbeat, which syllable to fire. What it *can* do is be the **crate-digger**: search a word index, assemble a phrase, and *queue* it to fire on the next bar. The audio engine performs; the LLM writes the score a few bars ahead.

This is the same split that already works in `bin/conduct.mjs` — it posts a `.play` DistributedNotification carrying a `startEpoch`, and every machine performs it locked to that timestamp. The conductor is slow; the performance is tight.

Get this wrong and the feature feels like a laggy chatbot. Get it right and the LLM feels like a hype-man who knows the whole lecture by heart.

---

## The whisper question: you probably don't want realtime

Instinct says "live audio → ring buffer → streaming whisper." That's the expensive path, and for a **YouTube URL it is unnecessary**, because a YouTube URL is not a live source. You can pre-fetch it.

The honest decomposition:

1. **Offline, once, cached:** `yt-dlp` → mono WAV → `whisper-cli` with word timestamps → `words.json`. Takes seconds. The repo already content-hash-caches exactly this (`recap/bin/transcribe.mjs`).
2. **Live, per-beat:** trigger pre-indexed word slices against the beat clock. This is **sample playback, not speech recognition** — trivially realtime.

**The hard part isn't realtime. So realtime is feasible.**

Realtime whisper (`whisper-stream`, ~300–600 ms latency with `base.en` on an M-series chip) only becomes necessary for the **ring mode** — reaching back into a *live* source you couldn't pre-fetch: a livestream, the DJ mix itself, or @jeffrey's own mic. That's a real and interesting second mode, but it's mode two, not mode one.

### Free lunch worth knowing about
`yt-dlp --write-auto-subs --sub-langs 'en.*' --skip-download` gives you YouTube's own auto-captions with timings for **zero compute**. Phrase-level, not word-level — but instant. Good for a fast preview/scrub while whisper grinds in the background.

### The gotcha the repo already learned the hard way
`recap/bin/phonetics.mjs:2-4` says it plainly: **whisper writes 0ms gaps between every token — its word boundaries are not real onsets.** They're butt-joined. Chop on raw whisper `fromMs`/`toMs` and you will clip word attacks and glue tails, and it will sound smeared instead of landing on the beat.

The repo's answer is to cross-check against audio-side onset analysis (RMS + spectral flux). Budget for this — it is the difference between a chop that *lands* and one that doesn't. The existing canonical whisper invocation (`recap/bin/transcribe.mjs:61-64`) already uses the sharpest available config:

```
whisper-cli -m ggml-base.en.bin -f in.mp3 -ojf -of out \
  --max-len 1 -ml 1   # one segment per word
  -sow                # split on word
  -nfa -dtw base.en   # token-level DTW timestamps (needs flash-attn OFF)
```

Interchange shape is `[{text, fromMs, toMs}]` — the de-facto format across `recap/`, `pop/`, and `toolchain/whistlegraph/`. Reuse it.

---

## Where does the beat come from?

"Over an active beat" is the load-bearing phrase. Three sources, ranked. **All three have both halves already built.**

### 1. Ableton (best — free, sample-accurate, has *phase*)
`ac-m4l/` already pushes `daw:tempo`, `daw:transport`, `daw:phase`, `daw:samplerate` over WebSocket into AC (`lib/disk.mjs:10494-10517`, surfaced as `sound.daw`). If @jeffrey DJs out of Ableton, the beat clock is **already arriving, with phase**, and nothing needs to be written. Today no DJ code reads it — it only feeds notepat-style pieces.

### 2. BlackHole digital tap + the MicTempo algorithm (universal, clean)
`slab/bin/slab-audio-setup.swift` already builds a CoreAudio **aggregate device** around BlackHole 2ch (mic + system loopback in, speakers + BlackHole out, so you still hear it). It was written for meeting recording. Point `MenuBandMicTempo`'s input at the BlackHole side instead of the mic and you get a **clean digital feed of whatever is playing — CDJ, Traktor, Ableton, a browser tab, anything** — into the existing BPM/phase-lock code. This is the universal answer and it is two existing files shaking hands.

### 3. Raw mic (works anywhere, dirtiest)
`MenuBandMicTempo` as-is. Menu Band hears the room and locks. Works at a gig with no cabling at all. Degrades with crowd noise.

### What is *not* an option today
The AC `dj` piece — both the native one (`fedac/native/pieces/dj.mjs`) and the web one (`disks/dj.mjs`) — has **zero BPM detection, no beatgrid, no beat clock from audio.** The native C engine has a `bpm` field, but it's a metronome you *set*, not a tempo it *detects*, and it is completely disconnected from the decks. If the plan is to DJ inside AC, that gap has to be closed first. If the plan is to DJ on real gear, **routes 1–3 sidestep it entirely** — which is the strong argument for doing this in Menu Band rather than in the `dj` piece.

*(Aside, unrelated but too cheap to leave unsaid: the native C audio engine already has two decks and a working crossfader (`fedac/native/src/audio.c:1931`, exposed as `sound.deck.setCrossfader`) that the `dj` piece never calls. It's a one-deck UI on a two-deck engine.)*

---

## Time-stretching a spoken phrase onto a beat

To make "the jiggling of atoms" land on a bar you have to stretch it. The good news:

**Menu Band already has the right unit.** `AVAudioUnitTimePitch` is in the graph today — `KPBJRadioStream` uses it to pitch the live radio per keypress. It decouples rate and pitch, in realtime, inside the engine. **No shell-out, no external binary, zero new dependencies.**

The offline/CLI story is worse and worth avoiding: this machine's ffmpeg (v8.1.2, homebrew) has **no `rubberband` filter and no `libass`**, and the `rubberband` CLI isn't installed. Seven files in `recap/` hardcode `/opt/homebrew/opt/ffmpeg-full/bin/ffmpeg`, which **does not exist here** — `recap/bin/phonetics.mjs:52` will hard-crash as written. There is a pure-native-filter escape hatch (`recap/bin/trance.mjs:4355-4399`: `asetrate` + `aresample` + an `atempo` chain), but it doesn't preserve formants.

**Conclusion: do the stretching live, in Swift, in the AVAudioEngine graph. Don't build an ffmpeg pipeline for it.**

---

## Implementation paths

### Path A — Menu Band native (recommended)

Menu Band *is* an audio app, and it already owns every hard piece.

**What to build:**
1. A new `InstrumentBackend` case. The enum is at `MenuBandController.swift:1153` — today `gm | gb | kpbj | sample`. Add `head` (or `talkinghead`). Each case is a `setXBackend()` sibling of `setRadioBackend()` (`:1178`). **This is the "YouTube mode" @jeffrey asked for, and it's the same shape as the radio mode that already ships.**
2. A `TalkingHeadVoice.swift` — clone `KPBJRadioStream.swift` (ring buffer + `AVAudioUnitTimePitch`) but feed it a local file that `yt-dlp` fetched, and give it a `words.json` so a trigger addresses a *slice* rather than a *pitch*.
3. Bind the beat: `MenuBandMicTempo`'s existing phase-locked clock quantizes every trigger to the next 1/4, 1/8, or bar.
4. A `TalkingHeadWindow.swift` panel — clone `SquawkWindow.swift` (the newest, most self-contained panel). Register a button in `ExpandedPianoWaveformView.swift:321-372` next to Gamepad/Squawk/LLMs.
5. **A DistributedNotification hook** — observer near `AppDelegate.swift:892`, handler near `:3303`. This is Menu Band's public API and *already* its documented LLM surface (`LLMGuideWindow.swift` literally hands the user a copy-paste protocol block for Claude).

**Why here:** lowest latency, the beat clock and the streaming-ring-buffer-as-instrument already exist, and the DNC control surface means the LLM seam is free.

**Costs:** Swift, no hot reload. And every new panel must pick a side of the `#if !MAC_APP_STORE` fence — anything touching global keystrokes, arbitrary filesystem, or shelling out to `yt-dlp` is **non-MAS** and lives in the direct-download build only. That's fine for "our own seat on neo and blueberry," but it means this feature likely never ships to the App Store build. Worth deciding early.

### Path B — an AC piece (best for iteration and for *sharing*)

The Disk API has more of this than expected:
- `sound.registerSample(id, Float32Array, rate)` / `updateSample()` (`disk.mjs:12730-12736`) — **you can hand raw Float32 buffers to the speaker and live-swap the buffer of a playing loop.** That is a ring buffer.
- `sound.play(id, {from, to, speed, pitch, loop})` — slice playback with the exact parameters a word-chopper needs.
- `sound.speaker.poll()` → `amplitudes`, `waveforms`, 8 frequency bands, and `beat.detected` (`lib/speaker.mjs:1083-1180`).
- `disks/stample.mjs` is already a complete record → sample → loop sampler. **Read it first.**
- `clock.time()` gives UTC-synced beat math (`marketing/av-reels/button-sync-allegory.md:75-131`).

**Why here:** hot reload, fast iteration, and the result is a *piece* — publishable, shareable, KidLisp-adjacent. This is the version other people eventually get.

**The blocker:** a browser cannot hear the DJ mix. `sound.speaker` only analyzes **AC's own output**, and mic is the only input. So the beat has to be injected from outside — via the M4L WebSocket feed, or by having Menu Band post the tempo/phase in. Path B is a great *surface* but it can't own the *clock*.

### Path C — Hybrid (where this probably lands)

Menu Band owns the audio engine, the ring buffer, and the beat clock. An AC piece is the **visual/notation surface** — the transcript scrolling, the word grid, the graphic notation of what just fired (exactly the av-reels "every audible onset spawns a synced visible mark" rule). MCP is the LLM seam between them.

Notably: **an LLM can already execute JS inside a live AC piece today, with zero new code.** `puppet_eval` → CDP `Runtime.evaluate` against the `:9222`-attached `ac-electron` window, and `bios.mjs:700-704` exposes `window.acSEND(msg)` to post straight into the running piece's worker, plus `window.acGetState()` and `window.AC.setMasterVolume()`. That channel is live right now.

---

## UX paths

### 1. YouTube mode = the radio dial, with a transcript
The KPBJ radio is already "a stream you can play like an instrument." Paste a YouTube URL; it becomes a **station**. It indexes in the background. The QWERTY keyboard, instead of playing pitches, plays *words*. This is the least new UX in the whole design — @jeffrey already knows how to play the radio voice.

### 2. Word keyboard (the notepat move)
The transcript is the keymap. A row of keys = a phrase; a modifier shifts you to the next phrase. This is Menu Band's native idiom — memorizable paths through a corpus, exactly like AC's whole thesis about memorizable command paths. **The lecture becomes an instrument you can learn.**

### 3. Ring / "relisten" mode
The last 30–60 s of *any* source — the video, the DJ mix, your own mic — is always in a ring buffer. A key grabs the last bar. Whisper transcribes the ring on demand so you can *see what just went by* and re-fire a word out of it. This is the mode that needs `whisper-stream`, and it's the one that works on a livestream. **It's also the mode where "relisten" literally means what it says.**

### 4. LLM as co-MC / crate-digger (the edutainment payoff)
You say — out loud, or typed — *"go at entropy."* The LLM searches the word index across **a corpus of indexed lectures**, assembles a phrase out of the lecturer's actual words, and queues it to fire on the next downbeat. You are not playing one video; you are playing a **library**, and the model is digging through it at the speed of conversation while the beat never stops.

This is the thing that doesn't exist anywhere else, and it's the reason to build it.

### 5. Prompt-to-cue over MCP
From Claude Code or any chat: *"drop the Feynman bit about jiggling atoms on the next 8."* → MCP tool → DistributedNotification → Menu Band fires it, beat-locked. **This is the "live loop for us first" @jeffrey described**, and it needs no UI at all — which is exactly why it should be built first.

---

## The MCP / live-loop seam

House pattern for MCP servers here is **hand-rolled JSON-RPC, no SDK** — every server says so in its header. The minimal complete example is `slab/bin/frame-mcp.mjs` (213 lines): import `httpPort/serveHttp/serveStdio` from `toolchain/mcp/http-front.mjs`, declare a `TOOLS` array, a `callTool` switch, a five-case `handleMessage` switch, register in `.mcp.json`. That's the whole thing.

So: **`slab/bin/head-mcp.mjs`, ~150 lines**, exposing something like

- `head_load(url)` — yt-dlp + whisper index a video, return its stats
- `head_search(query)` — full-text over the word index across all loaded videos → candidate phrases with timings
- `head_queue(phraseId, quantize)` — post a DistributedNotification; Menu Band fires it on the next bar
- `head_beat()` — read the current BPM/phase off MicTempo
- `head_ring()` — transcribe and return the last N seconds of the ring buffer

Every one of those either shells out to a sibling CLI (the `frame-mcp` pattern) or posts a DNC notification (the `conduct.mjs` pattern). **No new architecture.**

And it answers the "for us first" instinct correctly: the MCP tools *are* the product for now. No general-user UX has to exist until the instrument is fun.

---

## What's actually missing on this box

Nothing in the ingest/speech chain is installed. The code is mature; the machine is bare.

```bash
brew install yt-dlp        # NOT INSTALLED — nothing ingests without this
brew install whisper-cpp   # NOT INSTALLED — gives whisper-cli AND whisper-stream
curl -L -o ~/.whisper-models/ggml-base.en.bin \
  https://huggingface.co/ggerganov/whisper.cpp/resolve/main/ggml-base.en.bin
# BlackHole 2ch — needed for the clean digital beat tap (slab-audio-setup.swift bails without it)
```

Also worth fixing while in there: the repo is **split-brained about where whisper models live** — `recap/models/` (used by `recap/bin/transcribe.mjs`, `pop/bin/align.mjs`) vs `~/.whisper-models/` (used by the `pop/hellsine/` scripts). Neither directory exists. Pick one, symlink the other, or this gets debugged twice.

`ffmpeg` is present (v8.1.2) but has **no libass and no rubberband filter** — fine, because the recommendation is to never put ffmpeg in the stretching path.

---

## Recommended first move

**A headless spike, no UI, one afternoon:**

1. `brew install yt-dlp whisper-cpp` + pull `ggml-base.en`.
2. Copy `pop/bin/sample-from-youtube.mjs` — it already does *download → mono WAV → onset-detect → slice → index*. It is 80% of the ingest side and the single best template in the repo.
3. Run `recap/bin/transcribe.mjs`'s whisper invocation over the result → `words.json`.
4. Cross-check whisper's fake boundaries against audio-side onsets so the chops actually land.
5. Fire the slices from a throwaway script against a fixed BPM. **Listen to it.**

If a lecturer's voice chopped to a grid sounds good, everything above is worth building. If it sounds like a smeared mess, the fix is in step 4 and nowhere else — and better to learn that before writing a line of Swift.

The second move is `head-mcp.mjs` + a DNC hook in Menu Band, so it can be driven from a chat window while a beat runs. Panels and general-user UX come last, if ever.
