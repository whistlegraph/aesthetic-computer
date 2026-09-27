# Oskiewar on AC OS

Boot to the prompt, join Wi-Fi, then upload from the monorepo:

```sh
node fedac/native/tools/oskiewar-live.mjs ac7.local:8080 --watch
```

The tool bundles the shared Xbox game and native adapter into one piece,
verifies the uploaded bytes, and reloads it. Imported modules otherwise stay
cached across native reloads. The LAN port is reported by `/status`; another
process using port 80 can make the native server choose 8080.

The M30 bridge in `tools/oskiewar-usb-pad.c` supports the 8BitDo M30 in Xbox
360 USB mode on kernels without xpad. It claims only that device and writes
`/tmp/oskiewar-gamepads.json`. The native client rejects stale input after two
seconds. Keyboard controls remain available.

Write exactly `1` to `/tmp/oskiewar-gpu` to enable hardware triangles, or to
`/tmp/oskiewar-debug` to enable hitboxes at the next piece reload. Debug is off
by default; development USB init must not write `1` to that marker. The View
controller button can toggle diagnostics during play. Mesa needs
its EGL vendor manifest; `scripts/bundle-oskiewar.sh` includes that file, the
Comic Relief font, and the generated piece in OS builds.

To capture the actual game framebuffer, write a unique request to
`/tmp/oskiewar-capture`. Read `/tmp/oskiewar-frame.json` after the next capture
poll. Its `runs` are alternating ARGB32 pixel values and counts. Device state,
input, render timing, and netplay counters are in `/tmp/oskiewar-status.json`.

Today's Xbox-versus-AC test:

```sh
node xbox/tools/oskiewar-lan-test.mjs ac7.local:8080 --hot
```

The seats are named `xbox` and `ac`; the override expires at local midnight.
It uses the existing Internet relay, so being on the same LAN does not imply
LAN latency. A state mismatch records both hashes and state evidence, pauses for three
seconds, then automatically deals a fresh match on both devices. Repeated
failures back off to 30 seconds; 30 seconds of stable simulation clears the
backoff. AC queues reports for the existing `/api/piece-log` endpoint and
retries failed uploads; Xbox uses its existing native error reporter.
Remove `/tmp/oskiewar-lan-test.json` and reload AC to return to local play;
`node xbox/tools/oskiewar-release.mjs deploy-xbox-dev --hot` restores ordinary
Xbox play.

HDMI mirroring is handled by AC OS for every piece. Nearest-neighbor scaling
fills the display while preserving aspect ratio; borders remain only when
the source and display have different proportions. The mode picker prefers
modes within its pixel budget.
The TV's refresh rate is independent of the game's measured frame rate.
To select an ALSA output on engine startup, save its device name in
`/mnt/audio-device`; `AC_AUDIO_DEVICE` takes precedence. AC7's connected TV
uses `hw:0,3` at 48 kHz stereo. Remove the selection to restore automatic
device choice. A USB with two boot partitions needs the setting on both.

Live uploads change RAM. The September 24 development USB also contains the
native binary, generated piece, font, EGL manifest, and M30 helper in both
initramfs copies. It still boots to the prompt. The larger ACBOOT partition
retains `initramfs.before-oskiewar.cpio.gz` for rollback of either copy;
the smaller EFI partition cannot hold three complete images during updates.
These are development images,
not an uploaded public OS release.

## Concert display and curtain

The local `oskiewar-stage` MCP serves on `127.0.0.1:7792/mcp`. Its configuration
is `~/.ac-os/oskiewar-stage.json`; launchd runs
`computer.aesthetic.oskiewar-stage`. Tools:

- `oskiewar_curtain {down:true}`: darken and silence both displays immediately;
  the paired simulation settles to the same frame, then pauses.
- `oskiewar_curtain {down:false}`: raise the curtain and resume.
- `oskiewar_display {mode:"performance"}`: composition title and progress with
  ambient heads. `game` returns to the fight, keeping the current composition
  at bottom center; `auto` follows active music.
- `oskiewar_performance_status {}`: read source state and device acknowledgments.

Xbox receives this control through today's paired test. A command only reports
both displays acknowledged after AC and Xbox have returned its command ID.
The bridge observes Menu Band cue notifications and the native trio/spatial
status files. It does not start audio, change surround routing, or cue lights.
Stale transports cannot mask an advancing source; score notes are used only
when their hash matches the receiver. If all feeds stall, the footer retains
the last composition and confirmed progress with “signal lost.” Timing follows transport metadata, not
microphone analysis or an acoustic calibration.

AC displays battery percentage and estimated remaining time at the top right.
The six round maps rotate deterministically in the paired match. Three
skateboards retain independent riders and throw state.

The paired simulation uses deterministic math on both hosts. AC OS can run
that same operation order in C (`src/oskiewar-math.h`) through the versioned
`oskiewarMath` API; older runtimes fall back to JavaScript. Run
`node --test fedac/native/tests/oskiewar-math.test.mjs` on both compiler targets
when changing either implementation. Do not enable floating-point contraction
or substitute platform libm in simulation: single-bit differences caused the
captured projectile mismatch. Rendering continues to use fast platform math.

Periodic performance CSV writes and log fsync run in bounded workers; shutdown
still drains pending CSV records. Debug frame-meter runs coalesce on AC's
smaller framebuffer. Neither change disables diagnostics.
