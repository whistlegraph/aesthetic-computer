# PixelGroove v1

PixelGroove is Piecefarm's fixed native computer. A resident owns one
`160×160×RGB24` visible substrate. The central field is mutable sensed media;
the surrounding 16 concentric pixel rings are the record read by the VM.
The field executes in one of four protected hardware profiles: Q `32²`, H
`64²`, 1X `128²`, or 2X `256²`. The dashboard resamples only for presentation;
verification and memory accounting happen at the recorded native resolution.

## Machine constants

- Record margin: 9,216 RGB pixels / 27,648 bytes.
- Sequence: eight fixed 24-byte instruction slots.
- Functions: 19 capability entries and 18 bounded 16-word micro-bodies. The
  nineteenth entry is the verifier-owned `cellular` field rule and needs no
  mutable micro-body.
- Lifecycle: eight 16-word vectors (`boot`, `tick`, `reprobe`, `organized`,
  `graft`, `terminal`, `zero`, `start`).
- Sprites: four fixed 32×32 RGB banks.
- Reader clock: eight logical needle slots per 30 Hz sequence cycle, or 240
  VM ticks per second per resident.
- No resident rest/hold state. Every occupied resident receives every cycle.
- Native execution is bounded; there is no arbitrary machine code, allocation,
  network instruction, or unbounded branch.
- Hardware class and exact field byte extent are protected header values.
- Every dashboard is a perfect 4×3 grid. Tile geometry is identical while the
  actual pixel density exposes the unequal machines.

The 240 Hz clock is virtual machine time, not a claim about host CPU MHz. The
native absolute-deadline loop never grants extra instructions after a late host
frame. Deadline misses belong in live telemetry; the program's computational
budget does not expand or contract with browser scheduling or display refresh.

## Linear record layout

The byte stream walks each concentric ring clockwise, outermost to innermost.
Each address below is an RGB-pixel address; multiply by three for a byte offset.

| Track | Pixel range | Bytes | Authority |
|---|---:|---:|---|
| Header/superblock | 0–63 | 192 | protected |
| Main sequence | 64–127 | 192 | protected |
| Function directory | 128–191 | 192 | protected |
| Function bodies | 192–479 | 864 | protected |
| Projection bodies | 480–527 | 144 | protected |
| Lifecycle vectors | 528–655 | 384 | protected |
| Live reader state | 656–719 | 192 | mutable |
| Four sprite banks | 720–4819 | 12,300 | mutable |
| Candidate bodies | 4820–5107 | 864 | quarantined |
| Canonical Lisp source | 5108–5299 | 576 | protected |
| Sensory fringe | 5300–9215 | 11,748 | mutable/non-executable |

The protected hash covers the header (with its checksum word zeroed), sequence, function directory and bodies,
projection bodies, lifecycle vectors, and source track. Reader state, sprites,
candidate bodies, and sensory fringe may evolve without invalidating the
verified program.

## Reader semantics

The state track stores the program counter, active lifecycle vector, probe
epoch, failed reprobes, completed sequence passes, projection generation, and
physical needle pixel. The reader fetches instructions only from the sequence
track. Function bodies and lifecycle handlers are resolved only through their
fixed directories. Invalid magic, extent, version, checksum, opcode, body, or
address quarantines the record.

Health does not pause execution. A terminal observation vectors the reader to
the record's terminal handler, which increments its failed-reprobe counter and
calls its stored reprobe handler. Outside interventions likewise invoke the
record's organized or graft vectors.

## Pressing records

Commands write new files and never target the running farm:

```sh
npm run groove -- start '(raster (shift 1 0) (edges))' --output specimen
npm run groove -- start '(raster (line 0 0 127 127 255 0 90))' --profile double --output specimen-2x
npm run groove -- zero --id blank-01 --output blank-01
npm run groove -- inspect specimen.pgr
npm run groove -- print specimen.pgr --output specimen.ppm
npm run groove -- upconvert archive.json --output archive-pixel-groove-v1.json
```

`start` presses verified Lisp into a record. `zero` creates a valid blank record
without erasing another record in place. `print` emits a portable 160×160 PPM
whose margin pixels are the actual record bytes. Legacy archive restoration
deterministically presses missing v1 records while preserving resident identity,
parentage, generation, niche, iteration, and archive chronology.

New stochastic proposals combine the archive's persisted PRNG word with the
current synchronized Aesthetic Computer UTC millisecond when that authority is
available. Both inputs and their provenance are pressed into the protected
header. Offline creation uses persisted organism memory alone. Runtime fringe
evolution additionally folds in resident identity, lifetime, prior fringe
bytes, and the current authority tick, so repeated stochastic calls do not
collapse into a globally shared sequence.

The SDL reader posts one mutable groove back to the loopback server each
second. Twelve residents therefore complete a persistence rotation every
twelve seconds without blocking their 240 Hz readers. The dashboard reads the
same per-record endpoint and renders the actual groove bytes around the real
profile-sized center; it does not synthesize a decorative margin.

## HP v2 rubric

HP is a viability signal, not a beauty or quality score. An alive program gets
a lifecycle state factor of `1.00` (`.42` dormant, `.08` collapsed, `.24`
short-cycle flicker). Within that factor, 65% is a viability baseline, 20% is
bounded response to the previous field, and 15% is spatial/variance
differentiation. A `.75..1.00` continuity multiplier ramps over 90 healthy
steps. Noise, temporal coherence, muddiness, colorfulness, novelty, and the
verifier's aesthetic quality remain visible as independent traits and never
subtract from HP.

Intervention is not triggered by one HP sample. The oracle compares each
resident with the current population median and median absolute deviation over
sustained windows. A terminal cull additionally requires repeated failed
reprobes. The dashboard draws HP as a black horizontal capacity bar filled
red→orange→yellow→green, with a white population-cutoff tick. Exact values,
traits, and the rubric remain available through the live runtime snapshot.

## Materialization and density

A blank record contains its fixed machine skeleton. `start` fills the protected
sequence and source tracks. Execution then changes reader state, sprite banks,
candidate bodies, and the sensory fringe; verified promotions can replace a
bounded protected body and renew its checksum. Thus a groove can become more
occupied and chromatically dense over time, but it cannot become larger.
`inspect` reports occupied-pixel and occupied-byte density for the whole groove
and every individual track so this growth is measurable rather than inferred
from appearance.

Every dashboard tile overlays a blinking white/pink reader pixel at the live
`needlePixel` coordinate. Its position comes from native telemetry; the blink
is only a legibility pulse and does not invent reader movement.

The record itself remains byte-accurate RGB. A separate spectral key makes its
tracks screen-legible without mutating those bytes: header red, sequence
orange, functions yellow, bodies green, projection teal, lifecycle cyan, state
blue, sprites violet, proposals purple, source pink, and fringe ivory. Browser
tiles label every band and fill its rail by live occupied-pixel density; the
compact SDL tiles use the same order and hues as an eleven-segment edge rail.
At the tile's bottom edge, the only type is the last executed opcode; a
three-pixel health bar sits immediately below it. Historical console lines and
address/profile badges remain available as machine state but are not painted.
