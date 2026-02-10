# Notepat KidLisp Background Visuals — Implementation Plan

## Goal

Two capabilities, layered:

1. **Local background:** Run a KidLisp visual piece **behind** notepat's UI, activated via `notepat $roz` (or any `$code`), where the KidLisp piece gets notepat's live audio amplitude piped into its `amp` global variable.

2. **Remote visualizer:** Any notepat instance becomes an **amplitude broadcaster** — it gets its own pj-style code channel, and a remote machine (projector, FF1, second screen) can connect to that channel via `pj.kidlisp.com/{channel}`, receive the live `amp` signal, and run the KidLisp visualization independently.

The effect: notepat's blue/black background is replaced by a live-rendered KidLisp generative visual, and the normal notepat UI (keys, waveform bars, top bar, toggles) floats on top. On remote screens, the same visual runs full-screen with no notepat UI, driven by the live amplitude stream.

---

## Architecture Overview

```
┌─────────────────────────────┐     WebSocket (session server)     ┌─────────────────────────┐
│  notepat instance           │ ──────────────────────────────────▶ │  pj.kidlisp.com/{ch}    │
│  (player's device)          │     { type: "audio", amp, ... }    │  (remote visualizer)    │
│                             │     { type: "code", $roz source }  │                         │
│  ┌───────────────────────┐  │                                    │  ┌───────────────────┐  │
│  │ KidLisp bg (local)    │  │     BroadcastChannel (same-origin) │  │ KidLisp piece     │  │
│  │ via api.kidlisp()     │  │ ──────────────────────────────────▶ │  │ (full AC runtime) │  │
│  │ amp from speaker      │  │     kidlisp-channel-{ch}           │  │ amp from stream   │  │
│  └───────────────────────┘  │                                    │  └───────────────────┘  │
│  ┌───────────────────────┐  │                                    └─────────────────────────┘
│  │ notepat UI on top     │  │
│  │ keys, bars, toggles   │  │
│  └───────────────────────┘  │
└─────────────────────────────┘
```

---

## Phase 1 — Local Background (Embedded `kidlisp()` Call)

### Why Embedded, Not iframe

The existing `kidlisp()` helper in disk.mjs (line ~6135) already handles `$code` resolution, caching, persistent paintings, timing expressions, and accumulation. Using it is ~30 lines vs. the iframe underlay approach which would:
- Touch ~15 places in bios.mjs's render loop (transparency, z-index, compositor hiding)
- Require `postMessage` amplitude relay every frame
- Add iframe loading latency
- Duplicate the tape-playback underlay setup/teardown logic

The local background is paint-only (no `sim()` loop), which is fine for amplitude-reactive visuals.

### Why Also Support Remote

The embedded approach only works on the same device. But a performer might want:
- A projector behind them showing the visualization full-screen
- An FF1 art computer on the wall reacting to their playing
- A second browser tab/window showing just the visuals (for streaming OBS capture)

This requires networking — notepat broadcasts its amplitude, remote viewers consume it. The pj.html + session-server infrastructure already exists for code relay; we just need to **add audio relay**.

---

## Phase 2 — Remote Visualizer (pj-style Amplitude Broadcasting)

### Current State of the Infrastructure

kidlisp.com already has:
- **index.htm (editor)** broadcasts audio to PJ viewers via 3 transports:
  - `postMessage` to same-origin popout windows
  - `BroadcastChannel("kidlisp-channel-{id}")` for same-origin tabs  
  - `WebSocket` to session server for cross-origin/remote
- **pj.html (viewer)** receives audio on all 3 transports and forwards to its KidLisp iframe via `postMessage({ type: 'kidlisp-audio', data: audioData })`
- **session-server** relays `code` and `slide` messages to channel subscribers

### Critical Gap: Audio Relay is Broken for Remote

The session server (`session.mjs`) has **no handler for `type: "audio"` messages**. The editor sends them over WebSocket, but the server silently drops them. Audio relay only works via BroadcastChannel (same-origin tabs on the same machine).

**This means remote PJ viewers on different machines currently never receive audio data.**

### Fix Required in session-server

Add an `audio` message handler alongside the existing `code` and `slide` handlers:

```js
// session.mjs — alongside the existing code/slide handlers (~line 1935):
} else if (msg.type === "audio" && msg.content?.codeChannel) {
  const targetChannel = msg.content.codeChannel;
  // Transient — don't store, just broadcast immediately (like slide)
  // Audio is high-frequency (~60Hz), so no state persistence for late joiners
  if (codeChannels[targetChannel]) {
    const packed = pack("audio", msg.content, id);
    for (const subId of codeChannels[targetChannel]) {
      if (subId !== id) connections[subId]?.send(packed);  // Don't echo back to sender
    }
  }
}
```

### How Notepat Becomes a Broadcaster

In `boot()`, notepat connects to the session server WebSocket and subscribes to a channel:

```js
// Generate or derive a channel ID
const channelId = `notepat-${Math.random().toString(36).slice(2, 8)}`;
// Or use a deterministic ID from params: notepat $roz:myshow → channel "myshow"

// Connect to session server (reuse existing net.session WebSocket if available)
const sessionWs = new WebSocket("wss://session-server.aesthetic.computer");
sessionWs.onopen = () => {
  sessionWs.send(JSON.stringify({ type: "code-channel:sub", content: channelId }));
  // Send the KidLisp $code so late-joining viewers get it
  sessionWs.send(JSON.stringify({
    type: "code",
    content: { codeChannel: channelId, piece: kidlispSource }
  }));
};
```

In `paint()`, broadcast amplitude every frame (or throttled):

```js
if (sessionWs?.readyState === WebSocket.OPEN) {
  sessionWs.send(JSON.stringify({
    type: "audio",
    content: { codeChannel: channelId, amp: amplitude * 10, timestamp: Date.now() }
  }));
}
```

### Remote Viewer Connects

A remote machine opens `pj.kidlisp.com/notepat-abc123` (or any URL that resolves the channel). The existing pj.html code already:
1. Connects to session server WebSocket
2. Subscribes to the channel
3. Receives `code` → loads the KidLisp piece in its iframe
4. Receives `audio` → forwards to iframe as `kidlisp-audio` postMessage (once the server relay is fixed)

**No changes to pj.html are needed** — it already has the `audio` receive path wired up, it just wasn't receiving anything because the server dropped the messages.

---

## Bandwidth Considerations for Audio Relay

Audio messages at 60Hz with `{ amp, leftAmp, rightAmp, beat, kick, bass, mid, treble, highMid, presence }` ≈ ~200 bytes/message × 60/sec ≈ **12 KB/sec per subscriber**. This is negligible compared to video streaming.

For extra efficiency:
- Throttle to 30Hz (every other paint frame) — still smooth for visuals
- Only send when amplitude changes by > threshold (skip silent frames)
- Batch multiple fields into a compact array format instead of named object

---

## Implementation Steps

### Phase 1: Local Background (~30 lines in notepat.mjs)

#### Step 1 — Parse `$` Param in notepat's `boot()`

**File:** `system/public/aesthetic.computer/pieces/notepat.mjs` — boot() at line ~1115

Add a new state variable and parse `$code` from `params[0]`:

```js
// New state variables (near line 910 with other state)
let kidlispBackground = null;  // e.g. "$roz" → will resolve to KidLisp source
let kidlispBgEnabled = false;

// In boot(), after existing params handling (after line ~1276):
const dollarParam = params.find(p => p.startsWith("$"));
if (dollarParam) {
  kidlispBackground = dollarParam;  // e.g. "$roz"
  kidlispBgEnabled = true;
}
```

#### Step 2 — Forward Amplitude to KidLisp Each Frame

**File:** `system/public/aesthetic.computer/pieces/notepat.mjs` — paint() at line ~2509

After the existing amplitude extraction (`const amplitudeRaw = sound.speaker?.amplitudes?.left`), forward it to the global KidLisp instance:

```js
// After line ~2514 (amplitude extraction):
if (kidlispBgEnabled) {
  // Pipe notepat's amplitude into KidLisp's global `amp`
  // Scale: sound.speaker.amplitudes.left is 0..~1, KidLisp expects 0..10
  const scaledAmp = amplitude * 10;
  api.updateKidLispAudio?.({ amp: scaledAmp });
}
```

**Note:** `updateKidLispAudio` is exposed at `disk.mjs` line ~5084 and calls `globalKidLispInstance.updateAudioGlobals()` which sets `this.globalDef.amp`. This is the same path kidlisp.com's music player uses. 

The `api` object passed to `paint()` should expose this. If not, we can call it via the disk module's exported `updateKidLispAudio` directly, or add it to the paint API.

#### Step 3 — Render KidLisp Background in paint()

**File:** `system/public/aesthetic.computer/pieces/notepat.mjs` — paint() at line ~2641

Insert the KidLisp background rendering **before** the wipe/mode cascade, so it replaces the background:

```js
// Before the existing mode cascade (line ~2641):
if (kidlispBgEnabled && kidlispBackground) {
  // Render KidLisp piece as full-screen background
  api.kidlisp(0, 0, screen.width, screen.height, kidlispBackground);
  // Skip normal wipe — KidLisp provides the background
} else if (visualizerFullscreen && !recitalMode) {
  wipe(0);
  // ... existing visualizer code
} else if (recitalMode) {
  wipe(0);
  // ... existing recital code
} else {
  wipe(bg);
}
```

The `api.kidlisp()` call handles:
- `$code` → fetch and cache the code via `getCachedCodeMultiLevel()`
- First-frame loading (returns `null` while async fetch is in flight — paint normal bg as fallback)
- Subsequent frames: executes KidLisp and returns a `painting` object
- Persistent caching via `globalKidLispInstance.persistentPaintings`

We just `paste()` the painting to fill the screen, then paint notepat's UI on top.

#### Step 4 — Ensure `api.kidlisp` is Available

**File:** `system/public/aesthetic.computer/lib/disk.mjs` — `$paintApi` object (line ~5092)

Verify that `kidlisp` is already part of the paint API passed to pieces. It's defined as a method in `$paintApi` at line ~6135. If notepat's `paint()` receives the standard `api` object, it should already have `api.kidlisp()`.

**File:** `system/public/aesthetic.computer/lib/disk.mjs` — paint API

Also verify `api.updateKidLispAudio` is exposed. If not, add it:

```js
// In $paintApi or the api object passed to piece paint():
updateKidLispAudio: updateKidLispAudio,
```

#### Step 5 — Handle Loading State

While `$code` is being fetched (first frame), `api.kidlisp()` returns `null`. During this frame, fall through to the normal wipe:

```js
if (kidlispBgEnabled && kidlispBackground) {
  const bgPainting = api.kidlisp(0, 0, screen.width, screen.height, kidlispBackground);
  if (!bgPainting) {
    wipe(bg); // Fallback while loading
  }
  // If bgPainting exists, it was already pasted to the screen by kidlisp()
} else if (visualizerFullscreen && !recitalMode) {
  // ...existing
```

#### Step 6 — HUD Label Update

When a `$code` background is active, update the HUD label to reflect it:

```js
// In boot(), after setting kidlispBgEnabled:
if (kidlispBgEnabled) {
  hud.label(`notepat ${kidlispBackground}`);
}
```

This would show `notepat $roz` in the HUD corner.

#### Step 7 — Toggle KidLisp Background On/Off

Optionally allow toggling the KidLisp background with a keyboard shortcut (e.g., pressing `V` for "visual") or a tap zone, so the user can switch between the KidLisp visual and the normal blue/reactive background during performance.

### Phase 2: Remote Visualizer (~50 lines across 3 files)

#### Step 8 — Fix Session Server Audio Relay

**File:** `session-server/session.mjs` — near line ~1935 (alongside `code` and `slide` handlers)

Add the missing `audio` message handler:

```js
} else if (msg.type === "audio" && msg.content?.codeChannel) {
  const targetChannel = msg.content.codeChannel;
  if (codeChannels[targetChannel]) {
    const packed = pack("audio", msg.content, id);
    for (const subId of codeChannels[targetChannel]) {
      if (subId !== id) connections[subId]?.send(packed);
    }
  }
}
```

This is transient (no state storage, like `slide`) — audio is high-frequency and late joiners just start receiving from the current moment.

#### Step 9 — Notepat Connects as Amplitude Broadcaster

**File:** `system/public/aesthetic.computer/pieces/notepat.mjs` — boot()

When `$code` is active, notepat opens a session-server WebSocket and creates a named channel:

```js
// In boot(), after kidlispBgEnabled is set:
let ampChannel = null;
let ampWs = null;

if (kidlispBgEnabled) {
  // Channel ID: notepat instance identifier
  // Could be random, or derived from a user-chosen name via colon param
  // e.g. notepat $roz:myshow → channel "myshow"
  const colonChannel = colon?.find(c => !wavetypes.includes(c) && !/^\d+$/.test(c));
  ampChannel = colonChannel || `np-${Math.random().toString(36).slice(2, 8)}`;
  
  const wsUrl = net.sessionServerUrl || "wss://session-server.aesthetic.computer";
  ampWs = new WebSocket(wsUrl);
  ampWs.onopen = () => {
    ampWs.send(JSON.stringify({ type: "code-channel:sub", content: ampChannel }));
    // Send the $code so remote viewers get the KidLisp piece
    ampWs.send(JSON.stringify({
      type: "code",
      content: { codeChannel: ampChannel, piece: kidlispBackground }
    }));
  };
}
```

Display the channel ID in the HUD so the user can share it:
```js
hud.label(`notepat ${kidlispBackground} → ${ampChannel}`);
```

#### Step 10 — Broadcast Amplitude in paint()

**File:** `system/public/aesthetic.computer/pieces/notepat.mjs` — paint()

Throttle to ~30Hz (every other frame) to keep bandwidth light:

```js
let ampBroadcastTick = 0;

// In paint(), after amplitude extraction:
if (ampWs?.readyState === WebSocket.OPEN && ++ampBroadcastTick % 2 === 0) {
  ampWs.send(JSON.stringify({
    type: "audio",
    content: {
      codeChannel: ampChannel,
      amp: amplitude * 10,
      timestamp: Date.now()
    }
  }));
}
```

#### Step 11 — Remote Viewer Connects

A remote machine opens `pj.kidlisp.com/{ampChannel}` — **no changes to pj.html needed**.

pj.html already:
1. Parses channel ID from URL path
2. Connects to session server and subscribes to the channel
3. Receives `code` message → loads KidLisp piece in iframe
4. Receives `audio` message → calls `sendAudioToIframe()` which posts `kidlisp-audio` to the iframe

The only thing that was broken was the session server not relaying audio (Step 8 fixes that).

---

## Amplitude Data Pipelines

### Local (Phase 1)
```
notepat AudioWorklet
  → sound.speaker.poll() (in sim)
  → sound.speaker.amplitudes.left (in paint) [0..~1 range]
  → updateKidLispAudio({ amp: amplitude * 10 }) 
  → globalKidLispInstance.globalDef.amp [0..10 range]
  → KidLisp piece reads `amp` variable during execution
```

### Remote (Phase 2)
```
notepat AudioWorklet
  → sound.speaker.amplitudes.left (in paint)
  → ampWs.send({ type: "audio", content: { amp: amplitude * 10 } })
  → session-server relays to channel subscribers
  → pj.html receives { type: "audio", content: { amp } }
  → pj.html posts { type: "kidlisp-audio", data: { amp } } to iframe
  → iframe's boot.mjs receives postMessage
  → disk.mjs updateKidLispAudio({ amp })
  → globalKidLispInstance.globalDef.amp
  → KidLisp piece reads `amp`
```

Both paths end at the same `globalDef.amp` — the KidLisp piece code is identical whether it runs locally or remotely.

---

## Visual Compositing Order (in paint)

```
1. KidLisp background renders to full screen (replaces wipe)
2. Waveform bars overlay (if visualizerFullscreen/recitalMode)  
3. Top bar piano illustration
4. Note names / chord display
5. Toggle buttons  
6. Active key highlights
7. Touch interaction zones
8. .com superscript (if notepat.com)
```

The KidLisp painting writes to the pixel buffer first. All subsequent notepat `ink()`/`line()`/`box()`/`write()` calls paint **on top**, producing the layered effect naturally — no transparency/z-index DOM tricks needed.

---

## Files Changed

| File | Phase | Change |
|------|-------|--------|
| `notepat.mjs` | 1 | `kidlispBackground`/`kidlispBgEnabled` state, `$` param parsing, `api.kidlisp()` in `paint()`, amplitude forwarding |
| `disk.mjs` | 1 | Possibly expose `updateKidLispAudio` on paint API if not already there |
| `session-server/session.mjs` | 2 | Add `audio` message relay handler (~10 lines) |
| `notepat.mjs` | 2 | WebSocket connection, channel creation, amplitude broadcast in `paint()` |

**Estimated scope:** Phase 1 ~30 lines, Phase 2 ~50 lines across 2 files.

---

## Edge Cases & Considerations

1. **Performance:** Heavy KidLisp programs (many shapes, recursion) running at notepat's framerate could cause jank. Mitigation: the `kidlisp()` helper already has persistent painting caching, so static parts don't re-render. Timing expressions (`0.15s`) self-throttle.

2. **Visual modes:** When `paintPictureOverlay` (turtle drawing mode) or `recitalMode` is active, the KidLisp background should probably be disabled/hidden since those modes have their own full-screen visuals. The cascade handles this naturally if we gate the KidLisp rendering with `!paintPictureOverlay && !recitalMode && !visualizerFullscreen`.

3. **Projector mode:** In projector mode, notepat already wipes black and draws minimal UI. KidLisp background could optionally show in projector mode too (for live performance aesthetics). This is a design choice.

4. **Multiple `$` codes:** Could support `notepat $roz $wave` for layered KidLisp backgrounds. The `kidlisp()` helper already supports multiple regions — just call it twice with different source codes and positions.

5. **KidLisp code that uses `wipe`:** If the `$code` itself contains `wipe` commands, the kidlisp() helper handles this via its `shouldReset` logic. This is fine — the KidLisp piece controls its own background clearing within its painting region.

6. **Cleanup:** When notepat piece changes, the global KidLisp instance's `persistentPaintings` are managed by disk.mjs lifecycle. No special cleanup needed in notepat.

---

## Future Extensions

- **Bottom bar on notepat.com:** "Available as Ableton Extension" marquee (already planned separately).
- **KidLisp code editor inside notepat:** Long-press the `$roz` label to open an inline editor for the background code.
- **Beat-reactive globals:** Forward `beat`, `kick`, `bass`, `mid`, `treble` from notepat's audio analysis to KidLisp — same `updateKidLispAudio` call, just add more fields. (Note: notepat currently runs in `performanceMode: "disabled"` which skips heavy analysis in the worklet. Enabling frequency band analysis would require changing this to `"enabled"` or `"bands-only"`.)
- **Shared tempo:** Forward notepat's BPM to KidLisp's `bpm` global for beat-synced visuals.
- **Named channels:** `notepat $roz:liveset` creates channel `liveset` instead of random — shareable, memorable for performances.
- **QR code display:** Show a QR code on notepat's screen (tap a button) linking to `pj.kidlisp.com/{channel}` so audience members can see the visuals on their phones.
- **FF1 cast integration:** `ac-ff1 cast pj.kidlisp.com/{channel}` sends the remote visualizer directly to the FF1 art computer.
- **Multi-notepat jam:** Multiple notepat instances on different devices broadcast to the same channel — the remote visualizer merges their amplitude signals (max, average, or per-player).
- **Audience participation:** Remote viewers on `pj.kidlisp.com/{channel}` could send interaction events back (taps → visual effects), making the visualizer bidirectional.
