# PieceVM v4: from stage soup to small computers

Status: executable JavaScript reference VM plus strict-C11 native interpreter.
One admitted genome is presented simultaneously at stable addresses `A1`,
`B1`, and `C1` as 64², 128², and 256² native canaries; nine PixelGroove v1
residents remain as the control population. PieceVM descendants reach those
addresses only after exact JavaScript/native replay at all three resolutions
and a Git-backed admission.

## Why v1 cannot grow these capabilities

PixelGroove v1 is a verified pipeline of at most eight 24-byte stage records.
Its mutation grammar chooses among named host functions. A review can bias
which functions appear, but no program can name a data address, define a
function, branch, call, hold two render targets, or maintain a transform stack.
Consequently, adding `cellular` makes one more real behavior available but does
not change the machine's expressive class.

PieceVM changes the compiler target. A piece becomes bounded code operating on
typed regions of its own memory. Host services are a small kernel membrane;
program structure lives in the piece.

## Machine contract

One instruction is eight bytes:

```text
opcode:u8  a:u8  b:u8  c:u8  immediate:i32-le
```

The initial canary permits 512 instructions (4 KiB), 32 signed Q16.16 scalar
registers, a 256-entry operand stack, a 32-frame call stack, and 256 scene
records. Every tick starts with a fixed fuel allowance. Fetch, memory access,
branch, call, drawing, and matrix work all consume fuel. Exhaustion yields a
measured frame fault and preserves the last complete front buffer.

Program-visible memory is split by capability, not by convention:

| Region | Authority | Initial extent |
|---|---|---:|
| code | read/execute | 4 KiB |
| constants | read | 4 KiB |
| data | named, bounds-checked byte regions | 16 KiB |
| framebuffer A | RGB read/write | profile² × 3 |
| framebuffer B | RGB read/write | profile² × 3 |
| depth | u16 read/write | profile² × 2 |
| scene records | typed read/write | 8 KiB |
| glyph ROM | shared read-only | 2 KiB |
| sensing membrane | normalized read-only | 8 bytes |
| stack/call frames | VM-only | 4 KiB |
| PixelGroove record | mediated | 27,648 bytes |

Even a 256² piece stays below roughly 600 KiB including two RGB buffers,
depth, VM state, and its Groove. Twelve worst-case visible pieces therefore use
single-digit MiB for the PieceVM layer. The 2 GiB service limit is governed by
the parallel verifier population, not these live buffers.

## Minimal instruction families

The first season should have general operations, not dozens of visual effects:

- Control: `halt`, `jump`, `jump-if`, `call`, `return` and nested scalar
  function signatures with one-to-four parameters.
- Data: `constant`, `move`, legacy raw `load8`/`store8`, and v4 named
  `(data name bytes)` regions accessed through bounds-checked `read8`/`write8`.
- Math: `add`, `sub`, `mul`, `div`, `mod`, `min`, `max`, `sin`, `cos`, `sqrt`,
  deterministic `random`.
- Buffer: `front`, `back`, `clear`, `sample`, `pixel`, `line`, `triangle`,
  `blit`, `glyph`, `swap`.
- Transform: `identity`, `push-transform`, `pop-transform`, `translate`,
  `scale`, `rotate-x/y/z`, `perspective`, `project`.
- Scene: `begin-node`, `end-node`, `child`, `draw-node`.
- Sensing: bounded UTC beat phase, health vector, field energy, and selected
  Groove cells. No direct network or filesystem operation.

`swap` is the only operation that publishes a completed back buffer. The
display never reads a buffer while the piece writes it. Native SDL's existing
presentation buffers remain a separate host-level safety boundary.

Text is deliberately bootstrapped, not claimed as an accidental discovery.
The read-only glyph ROM supplies a small pixel alphabet; pieces must discover
layout, repetition, spacing, motion, hierarchy, and semantic use by composing
`glyph`, memory, branches, and time. Waiting for random RGB arithmetic to
invent a legible alphabet is not a serious search strategy.

Likewise, the transform kernel supplies arithmetic and projection, not a fixed
cube effect. A hierarchy emerges when Lisp functions call one another while
push/pop transform frames delimit local coordinate systems. Branching over
memory and sensed state makes the scene temporal and conditional.

## Lisp surface

The intended surface is a small expression language with explicit resource
types:

```lisp
(piece
  (buffer front (rgb 128 128))
  (buffer back  (rgb 128 128))
  (fn branch ((depth int) (phase fixed))
    (when (> depth 0)
      (push-transform)
      (rotate-y phase)
      (glyph back 65 0 0 ink)
      (branch (- depth 1) (+ phase 0.125))
      (pop-transform)))
  (frame
    (clear back night)
    (branch 6 beat-phase)
    (swap front back)))
```

This is illustrative syntax, not permission for unbounded recursion. The
compiler emits a control-flow graph and a resource proof. Calls are accepted
only when their maximum stack/fuel behavior is bounded, or they execute under
the hard dynamic fuel and call-depth limits with the frame remaining atomic.

The current executable subset includes scalar Q16.16 arithmetic and branches,
bounded calls plus a 256-value operand stack, `load8`/`store8` persistent data,
two physical RGB buffers, atomic `swap`, glyph ROM, contiguous typed `vec3`
resources, transform-stack projection, and generic point/line/wire-triangle
rasterization. `sense8` reads one of eight normalized, host-owned channels:
UTC musical beat phase, four-beat bar phase, HP, actual and potential energy,
spatial energy, temporal coherence, or the protected margin fringe. Programs
cannot address outside that bank or write back into it. The canary uses those operations to recursively construct two
children per node while preserving each caller's depth on the operand stack;
an eight-bit data cell advances its phase from one published buffer to the next.
Sampling, filled triangles, blitting, scene records, and the deterministic
native math kernel remain subsequent VM work, not capabilities already claimed.

## Verification and tiering

The authority checks, in order:

1. Parse and type-check every form and function signature.
2. Resolve every branch/call target to an instruction boundary.
3. Prove each memory operation stays inside a region granted to that opcode.
4. Bound code, constants, registers, call depth, scene nodes, and per-frame fuel.
5. Replay multiple initial memories and beat phases in the JS reference VM.
6. Reject collapse, out-of-bounds access, non-finite math, or incomplete swaps.
7. Differentially replay accepted bytecode in native PieceVM and compare exact
   buffer hashes plus fault/fuel traces.

Only then can hot blocks tier upward: reference interpreter → native threaded
interpreter → optional compiled block. A compiled block must replay identically
against its interpreter trace before promotion. Code pages remain W^X; a piece
never writes executable host memory directly.

## Evolution and review

Mutation moves from stage replacement to typed program edits:

- replace an expression while preserving its type;
- splice a verified basic block;
- add or remove a bounded branch;
- clone a function and alter constants;
- graft a reviewed capability recipe at a matching signature;
- share a verified function across a lineage by content hash.

OpenAI remains advisory. It may identify a capability gap, criticize an output,
or propose a typed source edit. It cannot set HP, mark a proof valid, mutate a
running front buffer, or bypass admission. Criticism, source, bytecode hash,
proof summary, parents, differential trace hash, and outcome all enter the Git
edition so the farm's archive remains part of the work rather than an external
chat memory.

## Rollout

1. Build the JS VM and compiler with exact tests for branch, call, swap, glyph,
   and transform behavior.
2. Add a native interpreter behind a new Groove version/engine tag.
3. Run one fixed-address canary tile while eleven v1 residents continue.
4. Differentially compare every canary frame and measure frame faults, fuel,
   HP, visual novelty, and cost.
5. Widen only after the canary survives one Git edition boundary without a
   proof mismatch or display-rate regression.

Steps 1–3 are active as of 2026-07-24. The 77-instruction canary uses 27 Q16.16
registers and 6,680 fuel on its first 64² frame. JavaScript on macOS, JavaScript
on Fedora, native C on macOS, and native C on Fedora all produce raw RGB SHA-256
`e9d4277832eb2c307c957d68e578c9e7178765b36debfd458d656f7236871359` for
frame one and `eb6acb9e6819c0249998604fd24ca6204ed675a5c03b18dbac54a94de48e2d7c`
for frame two. The differing frame hashes come from a program-owned data byte
that advances its transform phase. The live SDL membrane sends PieceVM code to
`A1` only and labels that tile `PIECEVM`.

The first admission nursery is now executable. Every 30 seconds it proposes a
bounded batch of typed source edits: balanced child-block clone/prune, draw
insert/delete, rotation insertion/axis replacement, vector-component change,
branch-depth change, color change, reusable function graft, a second
independent persistent-memory oscillator, a sensing graft, an argument-bearing
function graft, a named data-layout graft, a lineage crossover, or a bounded
environment graft. A function graft factors a spatial
draw into a new labeled call body with its own protected transform scope; an
oscillator allocates three registers and a distinct data address, then lets
that evolving cell rotate an existing hierarchy. A sensing graft allocates a
register, reads only beat, bar, or fringe during initial evolution, and routes
that signal through an existing transform. A lineage crossover imports a
balanced transform-and-draw leaf from a different verified ancestor recovered
from the bounded Git lineage, gives the graft one typed color perturbation,
and records that ancestor as a second parent. Imported control flow and calls
remain forbidden, so a donor cannot smuggle an unresolved dependency into its
descendant. When no crossover currently occupies the nursery, its first valid
candidate receives verification priority; this does not bypass the incumbent
score threshold or any replay proof. Each proposal compiles and
executes for eight JavaScript frames. The nursery measures temporal difference, spatial
structure, coverage, palette, structural complexity, and fuel, then asks the
strict-C11 interpreter to reproduce the leading candidates' first two raw RGB
hashes. A native mismatch cannot enter the nursery or reach the display.

An admitted descendant is now replayed independently at 64², 128², and 256².
All three JavaScript/native framebuffer pairs must match before admission. The
native membrane then presents a profile ladder at stable addresses `A1`, `B1`,
and `C1`: half, standard, and double resolution. SDL keys a live VM by code hash
and resolution, so a profile or genome change rebuilds only the affected
runtime. The remaining nine tiles stay v1 controls. The fixed dashboard is an
overview boundary; double-resolution memory remains 256² internally. Tile
previews preserve square pixels and whole-frame extent at explicit integer
ratios: 64² at 1:1, 128² at 1:2, and 256² at 1:4. Actual protected-core and
mutable-fringe bytes occupy a WYSIWYG perimeter. Every displayed perimeter
pixel has one stable memory address: the outer two logical pixels read mutable
fringe RGB bytes and the inner two read protected/core bytes, with no grouping,
tint, cursor blink, or decorative spectral rail. A separate 1:1 inspection
surface remains subsequent display work.
The nursery is bounded to 32 structural niches.
Source, bytecode, proof, traits, mutation, parent, generation, native evidence,
and PRNG state live in `archive.json`. A bounded 128-record lineage ledger is
also reconstructed from earlier `archive.json` blobs in Git, so same-niche
replacement does not erase ancestry from later editions. Exact parent/child
frame equality is now a live-admission rejection: a cheaper no-op may be useful
as an optimization experiment, but it cannot displace the visible champion.
The native board reports a persistent `X` crossover count beside generation
and lineage depth, rather than losing the event when a later child becomes
champion. Its compact machine row also reports function (`F`), argument (`A`),
and layout (`L`) counts; an environment-derived champion shows
`ECO capability<donor` from the persisted admission provenance.
Each admission makes a
`piecevm-admission` commit in the farm's own Git history. This is the initial
code-growth loop, not the final language: it still needs vector/record
arguments, layout resize/split/merge mutations, richer sensing composition, a
1:1 inspection surface, and longer canary evidence before the canary can widen
beyond three addresses.

The v4 ecology membrane observes only an allowlisted capability vocabulary in
the twelve currently visible outer residents (`line`, `triangle`, `rotate`,
`shift`, `copy`, `paste`, `cellular`, `flood`, `box`, `edges`). It never executes
outer source. A visible neighbor may bias a typed function or data-layout graft;
the candidate records neighbor id and capability and still passes the ordinary
score threshold plus exact native replay. Three proposals in each batch descend
from the champion while one rotates across other nursery residents, keeping
older functional branches reproductively available.

This is the boundary between a soup of effects and a farm of programs.
