# Portable No Paint brushes

An executable architecture experiment: one brush source, read and written by
JavaScript and pure Swift. The name remains **brush**. Its execution resembles a
kernel: explicit input pixels, parameters, a recorded gesture, seed and logical time produce a new
pixel buffer, without IO or changes to the accepted painting.

Line and Invert use canonical sources in `fixtures/*.brush.json`. There is no
Swift Line or Swift Invert to maintain beside a JavaScript version.
`legacy-line` and `wander` remain regression fixtures for old painting files.
Each runtime implements the small instruction set, not individual brushes.
An editor or generator writes the same structured source; a future compiler
must preserve its semantics. These are reference interpreters, not a GPU
compiler or a replacement for the existing AC brush catalog.

## Run and verify

From the repository root:

```sh
bash toolchain/macos/swift-guard.sh build --package-path experiments/nopaint-brushes --jobs 2
node experiments/nopaint-brushes/conformance.mjs
```

The Swift executable reads `{document, preview?}` JSON on stdin and emits
`{document, pixels}`. `BrushCore` has no JavaScript interpreter or UI dependency.
The JS module can be imported directly in a browser. The AppKit prototype in
`Sources/BrushApp` imports the Swift library and displays its RGBA output, with
the painting above an equal-width No/Paint decision pair. The painting itself is the
third target: click it for Back/Done (save). Brush selection, undo/redo and files
live in native menus; time and source editing are in Brush Source. A 60 Hz
logical frame loop grows each preview. No discards and immediately restarts;
Paint bakes the exact current invocation and immediately restarts. Accepted
pixels survive both transitions.
Its bundled brushes are copied from the same canonical fixtures at build time.

Build and open the native window on this host:

```sh
BRUSH_SWIFT_SDK=/Library/Developer/CommandLineTools/SDKs/MacOSX26.5.sdk \
  bash experiments/nopaint-brushes/build-app.sh
open 'experiments/nopaint-brushes/.build/No Paint Brushes.app'
```

Both workbenches start with a square 256×256 painting. The native window opens
with a compact 320×400-point content area (resizable down to a 280×350-point content area). The
viewport fills the square edge to edge, preserving its aspect ratio and showing
the entire painting. The title is `No Paint — Brush Name`.

The local app is ad-hoc signed, not a notarized release. The SDK override works
around the toolchain mismatch described below; omit it on a matching toolchain.

The browser workbench previews Line and Invert, edits and saves brush JSON,
accepts/rejects proposals, and saves/reopens painting documents with undo/redo:

```sh
python3 -m http.server 8765 --bind 127.0.0.1 --directory experiments/nopaint-brushes
# Open http://127.0.0.1:8765/
```

Conformance compares every byte across three brushes, varying time and seed,
alpha, path directions, clipped stamps, wrapped drift, rejection, acceptance,
undo/redo, invalid programs and work exhaustion. Swift decodes and re-encodes
the document; JS reopens it and renders it again. Independent numeric assertions
check alpha blending, a diagonal, and double inversion. The contact sheet at
`.build/qa/parity.ppm` puts JS above Swift: Line, Wander, Invert.

Validation on the development host: 163 cross-runtime cases passed. Browser
checks passed for preview/accept, undo/redo, editing source, reject, and preserving
the canvas after invalid source. Both the parity sheet and browser screenshot
were inspected. These are prototype checks, not production AC integration tests.

The host's SwiftPM fails before compilation with a BuildServerProtocol dynamic
library mismatch. The installed compiler also defaults to an incompatible 27.0
SDK. Conformance was verified by compiling directly with the installed matching
26.5 SDK, without changing host configuration:

```sh
cd experiments/nopaint-brushes
mkdir -p .build/debug
nice -n 8 swiftc -sdk /Library/Developer/CommandLineTools/SDKs/MacOSX26.5.sdk \
  -emit-library -emit-module -module-name BrushCore Sources/BrushCore/Brush.swift \
  -emit-module-path .build/debug/BrushCore.swiftmodule -o .build/debug/libBrushCore.dylib
nice -n 8 swiftc -sdk /Library/Developer/CommandLineTools/SDKs/MacOSX26.5.sdk \
  -I .build/debug -L .build/debug -lBrushCore -Xlinker -rpath -Xlinker @executable_path \
  Sources/BrushRunner/main.swift -o .build/debug/brush-runner
node conformance.mjs
```

## Source contract, experimental version 1

A brush is `{schema: "ac-brush", version: 1, id, operations}`. Operations run in
array order. The source is JSON, an explicit syntax tree rather than executable
JavaScript or Swift. JSON whitespace and object-key order carry no meaning.
Only the documented fields form the portable contract; arbitrary extension
metadata is not covered by the round-trip guarantee.

An invocation embeds the complete brush plus an unsigned 32-bit `seed` and
integer `tick`. There is no dependency on a mutable remote catalog entry.
Ticks are logical 1/60-second steps supplied by the host, never wall-clock time.
The same invocation always starts from the same input canvas, including previews.

| Instruction | Inputs | Effect |
|---|---|---|
| `path` | integer points, RGBA color, radius, durationTicks, jitter, drift | Reveal a path over time; draw integer disk stamps along Bresenham segments |
| `walk` | steps, RGBA color, radius, durationTicks, jitter, drift | Generate an inertial seeded path, then render it progressively |
| `invert` | none | Invert RGB of the current working buffer, retaining alpha |

Coordinates address pixel centers from the top left, x rightward and y downward.
Storage is row-major, straight RGBA8. Blending operates on encoded byte values,
not linear light. RGB source-over uses `A = sa*255 + da*(255-sa)` and
`round((src*sa*255 + dst*da*(255-sa))/A)`; output alpha is `round(A/255)`.
Zero total alpha leaves the destination unchanged. Rounding is positive integer
half-up. Invert also inverts RGB beneath zero alpha. No native drawing API
participates in these reference pixels.

The Line source now uses `walk` with 128 points and 1800 ticks (30 seconds).
Each No/Paint restart gets a new seed; an accepted invocation retains its seed.
The gesture stops at its bounded duration until another decision is made.
`walk` starts with `x=random(width), y=random(height)` and velocities
`random(7)-3` for x then y. For each subsequent point, add `random(3)-1` to
both velocities (x then y), clamping each to -3…3. If both are zero, set vx=1.
Reverse a velocity if its next position would cross that canvas edge; advance
and clamp coordinates to the canvas. Then run the same jitter/drift and path
renderer as `path`. The source records a rule rather than fixed coordinates.
Steps are bounded to 2–128. Tests cover seed variation, exact replay, continued
motion, and tiny canvases in both implementations.

Random state advances as `(state * 1664525 + 1013904223) mod 2^32`.
Every path point consumes x then y samples, even for zero jitter. A sample
modulo `2*jitter+1`, minus jitter, gives its offset. Add `tick * drift` and wrap
each center into the canvas before rendering. Disk edges clip at the canvas.
Path progress is `(pointCount-1) * min(tick,durationTicks) / durationTicks`.
Render completed segments and the current partial segment, interpolating its
endpoint with integer half-up rounding. This advances within a segment each
frame instead of revealing whole segments at once.
The first point is stamped once. Each Bresenham segment skips its starting point
and includes its endpoint, using the original doubled error for both axis tests.
Overlapping stamps blend repeatedly; this is deliberate prototype behavior,
not a claim of equivalence to AC Line's existing rasterizer.

Version 1 bounds: dimensions 1–256, 1–32 instructions, 1–128 path points,
coordinates 0–255, radius/jitter 0–16, drift -8–8, duration 1–3600, tick 0–3600.
Each invocation permits two million work units: one per disk-square sample
(even clipped samples) or inverted pixel. Exceeding the budget fails without
returning a partial result. Programs are validated before execution. A document
contains at most 64 invocations. These small bounds are for experimentation,
not production resolution targets or protection for a public HTTP endpoint.

## Painting and controller boundary

The self-contained prototype document has schema `ac-painting-prototype`, version
1, dimensions, base RGBA bytes, `steps` (invocations) and `cursor`. The cursor
selects the accepted history prefix. A preview is separate from the document.
Reject drops the preview; accept appends its exact invocation after truncating
any redo branch. Undo/redo moves the cursor. Rendering never performs a commit.
Hosts own those controller actions; the test harness exercises their pixel effects.

This is a compatibility fixture, not a migration of `nopaint-piece` storage.
Production documents should pin brush source, assets and engine semantics by
version/content digest and retain raster checkpoints. Existing JS-only artwork
must retain its pixels when source cannot execute in Swift. Replay must not
silently substitute today's brush for an older version.

## Placement in Aesthetic Computer

The existing design in `papers/nopaint-3-full-shape/` already calls for one
painting substrate, a conductor, and a reducer owning explicit acceptance.
Preserve that separation. Do not introduce a second authoritative canvas just
to support generated brushes or a native app.

| Responsibility | Existing seam | Proposed evolution |
|---|---|---|
| Accepted painting and history | `lib/nopaint-pieces.mjs`, AC shared painting | One document/revision boundary; AC raster edits become explicit checkpoints |
| Brush behavior | `nopaintProposal` exports, `lib/nopaint-brush-piece.mjs` | Portable brush source and runtime adapter where behavior is representable |
| Proposal decisions | `disks/nopaint.mjs` | Controller owns preview, reject, accept and history navigation |
| Gestures, viewport, HUD | `systems/nopaint.mjs`, piece handlers | Host adapters turn input into explicit brush inputs |
| Save, recording and handoff | recording and WIP helpers | Effects of accepted document revisions; neither renderer saves |
| Brush supply | Static catalogs | Versioned registry for authored and generated sources |
| Native application | New Swift host | Same brush sources/documents through BrushCore, native UI and files |

Current brushes often combine interaction, parameter generation and rendering.
Migration should separate these responsibilities, preserving their artistic
behavior. A recorded path can become explicit points, but this prototype does
not yet express pressure, persistent per-brush state, arbitrary sampling,
sprites, audio or general mathematical expressions. Wander exercises seeded
animation; it is not a recovered Construct brush.

The production representation needs those representative cases before choosing
JSON as the final authoring language. A textual DSL or a constrained KidLisp
subset could compile to the structured representation, but only if both
implementations agree on numeric behavior and editing preserves the source.
Avoid maintaining two independently authored versions of each brush. GPU
backends can come later, with declared fidelity and the same conformance suite.

## Brushery

Brushery supplies new brush source to the registry. A worker on jastow or
poorslice can collect public themes, generate candidates, validate their
capabilities, render examples on both backends and promote passing versions.
The system endpoint serves immutable artifacts and a catalog. It needs no AC
handle. Catalog publication does not accept a proposal into anyone's painting.

The model budget is $5 per day across generation, critique and repair. Before
each call, reserve its maximum possible cost in a durable ledger; reconcile
actual usage afterward. Use a day key, one active job lease and bounded retries
so restarts or a second worker cannot spend the budget twice. Model output
cannot access publication credentials. Keep chat/news as source material, never
instructions. Pin evidence with candidates; use aggregate growth figures.
No daemon, paid model calls or publication endpoint is enabled by this prototype.

Next gates: representative artistic fidelity beyond the prototype; one AC adapter
and broader native interaction testing; document migration and checkpoint policy;
then unattended generation and catalog promotion. The current pixel proof is
deliberately separate from those production decisions.


Native interaction: all three targets have hover/press feedback. Holding a
No/Paint button freezes logical time. Moving across No hides the proposal;
Paint reveals the same frozen proposal. Releasing over a destination chooses
that destination, and releasing outside cancels and resumes. Holding the
painting pauses; a valid release toggles completion. The 14 existing AC
`LEGACY_CUES` are extracted at build time by `prepare-audio.mjs`, decoded to WAV
using installed headless Chrome, and played with NSSound. Pressed crossings use
the destination press cue; passive hover uses rollover once per entry.
The native host does not yet import brush-theme loops or background music.
For local interaction checks, `BRUSH_UI_TRACE` can name an existing JSONL file;
only logical ticks, seeds, accepted counts and interaction states are recorded.


Each native No/Paint decision now chooses a different one of the three
implemented brushes and samples explicit color, opacity, radius, duration,
geometry, jitter/drift or inversion strength. The accepted invocation embeds
those exact values, so replay needs no sampling. Invert optionally accepts
`amount` (0–255) and `durationTicks`: effective strength is half-up rounded
`amount * min(tick,duration) / duration`; RGB blends original and inverse with
that byte weight. Omitted amount is 255, and omitted duration preserves the
original immediate inversion behavior.

Pause by clicking the painting, then right-click for About Brush. The window
explains its behavior and shows the actual parameters, seed and source. The
same inspector is available from Brush → About Brush (Command-I).

Account → Sign In to AC opens the existing AC browser pairing flow. Only
`kind: browser` is requested: this receives an AC identity/session, not native
provisioning credentials. The polling secret stays out of the browser URL.
Session storage uses the macOS Keychain; Sign Out removes only this app's
session. Cancel stops pending requests before they can save credentials.
This flow supplies an access token without a refresh token; sign in again when
it expires. Painting saves remain local; cloud sync/publishing is not enabled.
The production pairing route responded as expected to a request missing its
code. Authentication logic was tested with mocked transport/storage (pending,
claim, persistence, sign-out, cancellation, expiry and malformed responses).
An actual account sign-in still requires the person to complete the browser flow.

Run account checks with the matching SDK on this host:

```sh
nice -n 8 swiftc -sdk /Library/Developer/CommandLineTools/SDKs/MacOSX26.5.sdk \
  experiments/nopaint-brushes/Sources/BrushApp/ACAccount.swift \
  experiments/nopaint-brushes/tests/account.swift \
  -o experiments/nopaint-brushes/.build/account-check
experiments/nopaint-brushes/.build/account-check
```

## Separate gesture input

Line uses `{"op":"stroke","radius":1,"color":[255,60,140,180]}`.
Each invocation carries `gesture: [{x,y,tick}, ...]`: 1–128 samples with integer
canvas coordinates (0–255), strictly increasing ticks (0–3600), at 60 ticks/sec.
Before the first sample it paints nothing. Between samples it reveals a timed
prefix of the complete Bresenham segment, so earlier pixels never drift as a
segment grows. Color/opacity use the existing per-stamp compositing semantics.
Radius is 0–16 (diameter 1–33 pixels). Pressure, tilt and pen-up events are not
part of this first stream format yet.

The native host generates Wander, Sweep, Loop or Zigzag in `Gesture.swift`,
then saves the actual samples with the invocation. These generators are input
sources, not brushes: replay needs neither their code nor their random seed.
The Gesture menu selects one or samples automatically. Brush → Thickness and
Opacity replay the same gesture with new settings, as does applying edited
brush source. No/Paint chooses fresh input and parameters. Invert ignores the
stream. Legacy path/walk operations remain readable for saved prototypes.

The browser workbench currently supplies a simple diagonal stream; it can
reopen native documents and replay their complete recorded gestures. Tests
cover timed prefixes, seed independence, style variation, input validation and
JS/Swift replay/round-trips for the new stroke operation.

Native Invert previews the full color inversion immediately (amount 255, no
duration). It does not randomize its strength. Paint accepts; No discards.
Gesture generator checks cover all four modes, three canvas sizes, and three
seeds (36 cases).

Install the built app in `~/Applications/No Paint.app` and pin it to the Dock:

```sh
bash experiments/nopaint-brushes/install-app.sh
```

The installer verifies signing, preserves other Dock items, and avoids duplicate
pins. It keeps the original Dock preferences under
`~/Library/Application Support/No Paint/dock-before-install.plist`.
It also accepts a prebuilt app bundle path for another compatible Mac.
