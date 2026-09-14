# Notepat for Roblox

A local, native Luau prototype of the Aesthetic Computer instrument and prompt.
It starts on twelve colored pads. Click **ac> notepat** (or press backtick) to
open the prompt, then type `notepat` and press Enter to return.

The pitch mapping, sharp-key shortcuts, and RGB palette come from
[`notepat.mjs`](../../system/public/aesthetic.computer/disks/notepat.mjs).
This is a small playable adaptation, not the browser runtime embedded in Roblox.

| Notes | C | C♯ | D | D♯ | E | F | F♯ | G | G♯ | A | A♯ | B |
|---|---|---|---|---|---|---|---|---|---|---|---|---|
| Keys | c | v | d | s | e | f | w | g | r | a | q | b |

Tap/click pads or use these keyboard keys. `[` / `]` and the onscreen − / +
buttons select octaves 2–6. Multiple notes can play together. Releasing a key
does not cut off a finger holding the same pad. Leaving the window, changing
octave, or opening the prompt clears held voices. Avatar controls are disabled
while this standalone instrument owns the screen.

## Sound

The prototype plays the client's bundled `rbxasset://sounds/volume_slider.ogg`
through native `Sound` objects, changing `PlaybackSpeed` for each note. The
installed Mac client contains this file; its dominant pitch measured about
392 Hz (G4). A4 is computed as 440 Hz using equal temperament. No audio upload,
external media request, or paid asset is involved.

This is a short sample with its own timbre and decay. Holding a pad **does not
sustain an oscillator**. It is not yet AC's sine/composite/triangle synthesis,
and the bundled sample is not a guaranteed cross-version musical asset API.
The UI reports a preload failure. Before shipping, replace it with an approved,
owned, accurately tuned sample and verify audio permissions on the target
experience. Native Roblox playback changes pitch and duration together:
[Sound.PlaybackSpeed](https://create.roblox.com/docs/reference/engine/classes/Sound).
Roblox's [audio overview](https://create.roblox.com/docs/audio) explains its
asset-based audio pipeline.

## Build and verify

From the monorepo root, with Rojo and Luau installed:

```sh
mkdir -p roblox/notepat/build
rojo build roblox/notepat/default.project.json -o roblox/notepat/build/notepat.rbxlx
luau roblox/notepat/tests/logic.luau
```

On this session's host the downloaded tools are `/tmp/ac-rojo/rojo` (7.7.0) and
`/tmp/ac-luau/luau` (0.738). The build and 19 logic checks passed. All scripts
also passed `luau-compile`; the generated XML contains the expected server,
client, and two shared modules. See [evidence.json](evidence.json).

Open `build/notepat.rbxlx` in Studio and Play to inspect the actual UI and sound.
After assigning a **separate Notepat development target**, the existing
Robloxplorer place-publishing command can upload the artifact. Do not upload
this project to the arena target: a place upload replaces that place's contents.
[`tests/headless.luau`](tests/headless.luau) contains ten server-side assertions
for Open Cloud execution after an upload; it has been compiled but **not run
on Roblox**. No target was created and no version was published in this trial.

Client playtest still required: keyboard chords; two fingers on separate pads;
keyboard + touch on one pad; release outside a pad; focus loss; octaves; portrait
layout; prompt navigation; preload failures; audible pitch and latency. A
headless server cannot verify these perceptual/input behaviors.

## Toward an AC OS game

The prompt currently recognizes only `notepat` and `prompt`. A next step is to
extract a native piece interface (`mount`, `leave`) and command registry, with
one shell owning keyboard focus, navigation, and lifecycle cleanup. Notepat's
`Notes` and `Holds` modules can remain independent of that shell. An arena
destination can later teleport to its own place rather than mixing avatar
movement and instrument keybindings.

The visual prototype uses Roblox's built-in `Enum.Font.Code`. **YWFT Processing
and KidLisp lettering are not implemented here.** Integrate the shared AC UI
and glyph renderer once the parent project establishes them. This isolated
trial deliberately does not modify `roblox/shared` or arena files.

Not included: multiplayer note relays, recording/playback, MIDI, waveform
selection, AC accounts, network data collection, or custom asset uploads.
