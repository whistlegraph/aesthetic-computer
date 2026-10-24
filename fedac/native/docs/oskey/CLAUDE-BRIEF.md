# oskey — the brief for Oskie's Claude

Paste this URL into Claude Code on the MacBook and say "read this, then help me with oskey":

    https://raw.githubusercontent.com/whistlegraph/aesthetic-computer/main/fedac/native/docs/oskey/CLAUDE-BRIEF.md

Everything below is for Claude. It explains the ThinkPad, the instrument running on it, how to change it over wifi, and where the operating system's source is when something lower down needs fixing.

## The device

- A Lenovo ThinkPad 11e booting **AC OS** from a USB stick. AC OS is a purpose-built Linux (kernel 6.14, embedded initramfs, no package manager, no desktop) whose PID 1 is `ac-native`, a C program that owns the display (DRM), audio (ALSA), wifi, keyboard and trackpad, and runs one JavaScript "piece" at a time in QuickJS.
- The running piece is **oskey**: `/pieces/oskey.mjs` on the device, `fedac/native/pieces/oskey.mjs` in the repo. It opens first at boot (config.json `piece`).
- Screen is 1366×768 at pixel scale 2, so the piece draws into **683×384** logical pixels. 60 fps.
- It joins the house wifi by itself and announces an mDNS name. This one is **ac13.local** (also `ac-device.local`). `dns-sd -B _http._tcp` lists it; `dns-sd -G v4 ac13.local` gives the IP.
- Device identity: AC handle @oskie. There is no Claude session on the device itself; you work from the MacBook.

## The loop (no git, no build, no auth)

The device runs a small HTTP server on port 80 (`fedac/native/src/lanserv.c`):

```
curl http://ac13.local/                       # {"name","build","piece","ip","uptime"}
curl http://ac13.local/pieces/oskey.mjs -o oskey.mjs   # the live piece — fetch before editing
curl -X PUT --data-binary @oskey.mjs "http://ac13.local/pieces/oskey.mjs?jump=1"   # push + hot-reload
curl http://ac13.local/logs | tail -80        # runtime log: console.log from the piece, JS exceptions, audio/wifi/input lines
curl http://ac13.local/lib/percussion.mjs     # any /lib module the piece imports
```

- Piece changes reload instantly (each load is a fresh module). `/lib` changes only take effect after a reboot (QuickJS caches lib imports).
- If a push makes the piece throw, the exception is in `/logs` with a line number; fix and push again. If the screen is black after a push, the module failed to load; the log says why.
- `fedac/native/scripts/dev-push.sh ac13.local oskey --watch` does the fetch/push loop on save if the repo is checked out.
- ssh (dropbear, key-only) exists but only @jeffrey's key is baked; ask him if you need a shell.

## The piece shape

`export { boot, paint, act, sim, leave }` — the same five hooks every AC piece has.

- `boot({ sound, system })` once. `paint({ wipe, ink, box, line, write, comicWrite, screen })` every frame. `sim({ sound })` every frame (good for timing). `act({ event: e, sound, system, screen })` per input event. `leave({ sound })` when jumping away.
- Events: `e.is("keyboard:down")` / `e.is("keyboard:up")` with `e.key` lowercase (`"a"`, `"1"`, `"f1"`…`"f12"`, `" "` for space, `"escape"`, `"tab"`, `"capslock"`, `"shift"`, `"enter"`, `"backspace"`, `"delete"`, `"insert"`, `"home"`, `"end"`, `"arrowleft"`…), `e.repeat` for held keys. Trackpad as a pointer: `e.is("touch")` / `e.is("draw")` / `e.is("lift")` with `e.x`, `e.y` in logical pixels. Whole hand: `trackpad.contacts` (array of `{x, y, pressure}` in 0..1, plus `aspect`, `generation`) is passed in the api object.
- Drawing: `wipe(r,g,b)`, `ink(r,g,b[,a])`, `box(x,y,w,h[,"outline"])`, `line(x1,y1,x2,y2)`, `write(text, {x, y, size, font})` with fonts `"font_1"` (6×10, default), `"matrix"` (tiny), `"unifont"` (16 px, has every glyph); `comicWrite(text, x, y, heightPx, r, g, b)` is the hand-drawn comic face used for the wordmark. `screen.width`/`screen.height`. Named colors are not supported; use RGB.
- Sound: `sound.synth({ type, tone, duration, volume, attack, decay, pan, gmProgram })` → a voice `{ id, update({tone, volume, pan}), kill }`; `sound.kill(voice, fadeSeconds)`. `type` is `sine | triangle | sawtooth | square | noise | piano`; with `type:"piano"` and `gmProgram: 0-127` you get the General MIDI synth (acoustic pianos, e-pianos, organs, basses, strings, leads, pads, kalimba, steel drums…). `duration: Infinity` holds until killed. `sound.bpm(n)`.
- Drums: `import { playPercussion } from "/lib/percussion.mjs"`; `playPercussion(sound, letter, { volume, pan, pitchFactor })`, letters c d e f g a b = kick snare clap snap closed-hat open-hat ride, c# d# f# g# a# = crash splash cowbell block tambourine. It is pure synthesis, so it can be retuned by editing that lib (reboot to pick up).
- Samples: `sound.zoo.play(name, { volume, pan, pitch })` with bird cat cow dog frog horse lion owl pig sheep snake whale (48 kHz float32 mono files baked at `/samples/zoo/`). Mic: `sound.microphone.rec()` starts, `sound.microphone.cut()` stops and returns sample count, `sound.microphone.sampleLength/sampleRate`; then `sound.sample.play({ tone, base, volume, pan, loop })` plays the recording resampled so `tone/base` is the pitch ratio. `sound.sample.getData()` / `loadData(float32array, rate)` / `saveTo(path)` / `loadFrom(path)`; `/mnt/samples/` is persistent storage on the stick.
- Sends and effects: `sound.room.setMix(0..1)` + `sound.room.toggle()`; `sound.echo.setMix(0..1)`; `sound.glitch.setMix(0..1)` + `toggle()`; `sound.volume.setMix(0..1)` is master. There is no filter in the engine yet; a cutoff/resonance would be new C in `audio.c`.
- MIDI (USB class-compliant controllers, since build 529d487f29): `e.is("midi")`, `midi:noteon` / `midi:noteoff` (`e.note`, `e.velocity`, `e.channel` 1-16), `midi:cc` (`e.control`, `e.value`), `midi:bend` (`e.bend` -1..1, `e.value` 0..16383), `midi:pressure` (`e.value` channel aftertouch). Program change and poly aftertouch are not delivered. The port shows up in the log as `[midi-in] opened /dev/snd/midiC1D0`.
- System: `system.jump("prompt")` leaves to the command line (`oskey` there comes back; `piano`, `notepat`, `dj`, `samples` are other instruments to study). `system.readFile/writeFile(path)` for `/mnt` persistence. `console.log` goes to `/logs`.

## What oskey does today (so you don't rebuild it)

F1 instrument / F2 drums / F3 sampler / F4 sequencer view. Number row = nine tracks. Letter block = scale degrees: +1 per key right, +2 per row up, so a diagonal of three keys is a triad. Drums layer: 12 kit pads under the left hand, 12 zoo samples under the right. F5 scale, F6 chord, F7 tap tempo, F8 root, F9 mic record/cut, F10 loop 1↔2 bars, F11 click, F12 record-arm, Space play/pause, Tab patch (14 patches), -/= octave, Shift accent, Caps + 1/2/3 room/echo/glitch, Insert undo, Delete clear track, Esc stop (three times → prompt). Trackpad XY = echo/room sends. Light paper UI, "oskey" wordmark in comic pink, a pink pal who bops on the beat. State is a handful of module-level lets at the top of the file; `hitFor()` turns a key into a replayable hit, `fire()` plays one, `record()` quantises it onto the running 16/32-step loop, `tick()` replays the loop from `sim`.

## The operating system's source (for lower-level problems)

Public mirror: https://github.com/whistlegraph/aesthetic-computer — everything for the device is under `fedac/native/`. A cheap checkout:

```
git clone --filter=blob:none --sparse https://github.com/whistlegraph/aesthetic-computer.git
cd aesthetic-computer && git sparse-checkout set fedac/native system/public/aesthetic.computer/lib
```

Where things live (line counts are approximate):

| file | what |
|---|---|
| `fedac/native/src/js-bindings.c` (9k lines) | every `sound.*`, `system.*`, `write`, `box`… the piece can call. Search `JS_SetPropertyStr(ctx, sound, "` to list the sound API, `js_synth(` for synth options, `parse_wave_type` for voice names. |
| `fedac/native/src/audio.c` (5k) | the engine: oscillators, envelopes, mixer buses, room/echo/glitch, mic capture, sample playback, GM synth hooks. A new effect or filter goes here plus a binding in js-bindings.c. |
| `fedac/native/src/gm_synth.c`, `gm_sf2.c`, `docs/gm-synthesis/` | the General MIDI voices and how they are synthesised. |
| `fedac/native/src/input.c` | evdev keyboard + multitouch trackpad; `input_key_name()` is the key-name table. |
| `fedac/native/src/ac-native.c` (5.5k) | PID 1: boot, DRM main loop at 60 fps, piece loading (`/pieces/<name>.mjs`), jump handling, config.json. |
| `fedac/native/src/lanserv.c` | the port-80 server + mDNS responder you push through. |
| `fedac/native/src/wifi.c` | wpa_supplicant driver; preset networks live here and in `scripts/flash-mac.sh`. |
| `fedac/native/internals.md` | the boot-to-main-loop narrative. Read first for anything below JS. |
| `fedac/native/device-claude.md` | what a Claude *on* the device sees (BusyBox ash, /tmp is tmpfs, /mnt is the stick, perf traces in /mnt/perf). |
| `fedac/native/pieces/*.mjs` | every bundled piece. `piano.mjs` is the smallest instrument, `notepat.mjs` the biggest (sample bank, per-key recording, trackpad drum head), `dj.mjs` decks and scratch. |
| `system/public/aesthetic.computer/lib/percussion.mjs` | the drum kit the device bakes to `/lib/percussion.mjs`. |
| `fedac/native/SCORE.md` | how the OS itself is released: C changes need a commit on the main repo and a remote "oven" build (`ac-os oven`), which only @jeffrey can trigger. |

Debugging on the fly:

- **Something silent?** `/logs` first. Audio lines are tagged `[audio]`, `[sample]`, `[mic]`, `[js]`; a failed `sound.sample.play` logs `play FAILED (sample_len=0)` which means nothing was recorded.
- **Lag or dropped frames?** The runtime writes per-frame timing to `/mnt/perf/`; a frame over 16667 µs is a dropped frame. Heavy `paint` work (hundreds of `write` calls, big loops) is the usual cause; draw less or cache.
- **A key does nothing?** Log `e.key` in `act` and push; the name table is `input_key_name()` in `input.c`.
- **Wifi gone?** The device reconnects on its own; its preset list is in `wifi.c` (`ATT2AWTpcr` is in it). `/logs` shows `[wifi]` lines.
- **Changes you cannot make from here:** anything in `src/*.c` or `/lib` needs a rebuild and a reflash or OTA. Write the C change, test the reasoning against the source, and hand it to @jeffrey with the file and line; he runs the oven and the device picks the update up over wifi.

Rules: keep oskey one readable file, push and play after each change, read the log when anything is quiet, describe what you want musically and map it onto these calls.
