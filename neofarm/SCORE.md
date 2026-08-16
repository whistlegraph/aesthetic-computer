# Score for Neofarm

A piece-farming daemon in the bitcoind mold: always up on every Mac we have
(neo, blueberry, later the minis), quietly breeding small audiovisual programs
and queueing the certified ones for a human cancel/ok. Mechanical evolution in
the inner loop, LLM grafts on a slow clock, and a regularized agentic poker
that reports, analyzes, and leaves digests.

Neofarm is the third farm and it inherits deliberately:

- from **cancelok/**: the taste loop (generate → gate → enqueue → human
  verdict feeds the next generation), pheromone memory, ship-to-bags.
- from **experiments/intelligent-terrarium/** (Piecefarm): executable
  contracts, budget verification, lineage as Git sediment, the proposal
  membrane for outside models — and its unfinished frontier: *proper bytecode,
  a proof-oracle interpreter, a readable Lisp form for microcode*.
- from **kidlisp-wasm/**: source-to-WASM compilation of real pieces — and its
  gap: no intermediate instruction set, host `Math.random` breaking replay.

## The machine comes first

The organism is not source text. The organism is a **bytecode program** for a
small, versioned, fully-specified virtual machine (`isa.mjs` is normative).
Everything else — evolvability, verification, portability, honesty — falls out
of getting this abstraction right:

- **Fixed-width instructions** (8 bytes: op, dst, a, b, f32 immediate) over a
  16-register f32 file. Any byte string decodes to *something* runnable:
  unknown opcodes decode as NOP, register fields wrap. Mutation and crossover
  are therefore **total** — no syntax errors, ever. This is the property the
  mechanical inner loop lives on.
- **Branchless and loop-free by construction.** No jumps in v1; conditionals
  are `SEL` (branchless select). Termination is structural, not proven —
  a program's cost is exactly its instruction count times its section rate.
- **Three sections, three rates.** `SETUP` runs once per organism life.
  `PIXEL` runs per pixel over the 128×128 field (a shader, effectively).
  `BEAT` runs per musical eighth at AC UTC 60 BPM and emits bounded audio
  events. Visual substrate and sonic output are outputs of *one* program —
  no post-hoc listener deciding what the image sounds like.
- **Deterministic replay.** The only randomness is the `RND` opcode over a
  seeded PRNG; the seed lives in the genome header. Same genome → same field,
  same events, same hash, on any machine.
- **Budgets in the header, checked by the reference interpreter.** Max
  instructions per section, register file only (no heap), fixed field, fixed
  event slots. The reference interpreter in `isa.mjs` is slow, obvious, and
  canonical — the proof oracle. Fast backends (WASM via the kidlisp-wasm
  emitters, native later) must bit-match it in differential tests before they
  are trusted; they are accelerators, never authorities.
- **Round-trip to a readable Lisp.** `disassemble()` renders any genome as a
  canonical s-expression form; the assembler accepts it back. Grafts propose
  in Lisp, mechanics mutate bytecode, one lineage records both.

## The daemon

`neofarm/daemon.mjs`, run by a LaunchAgent (`KeepAlive`, niced, jittered
ticks). One tick:

1. pick parents by pheromone (verdict-weighted, novelty-biased),
2. mutate/crossover at the bytecode level,
3. gate on the reference interpreter: budgets, non-collapse (field variance,
   temporal change), sonic non-silence, determinism hash,
4. score behavior descriptors (field stats + event-stream stats) against the
   novelty archive,
5. admit to the population shard or dissolve; append to `lineage/<host>.json`.

Ticks are cheap (no Chromium, no LLM, no network). Certification — a real
audiovisual render for the human queue — happens rarely, one at a time,
niced, deferring to the 8 GB rule on neo.

## Grafts

A few times a day, an LLM (claude CLI, or local Gemma via MLX when offline)
receives the disassembled Lisp of the current elite plus the pheromone taste
block, and proposes whole organisms or subsequence grafts — through the same
gate as every mechanical candidate. Proposals are provenance-tagged in
lineage. No graft bypasses the reference interpreter.

## The poker and digests

On a regular clock (default: twice daily), an agentic session reads the
shards, lineage, and gate statistics across all farm hosts and writes
`digests/<date>-<host>.md`: population health, novelty coverage, notable
births and dissolutions, knob recommendations (mutation rate, graft cadence)
with reasons. The poker may adjust knobs **within declared bounds** in
`knobs.json` and must record every adjustment in the digest. It never edits
code, never ships, never touches the repo outside `neofarm/` state paths.
Digests are the farm's voice; the human reads them like mining-pool stats.

## Shipping and surfacing

A certified organism ships as its Lisp form via the store-kidlisp API —
a `$code`, instantly URL-addressable, no commit, no deploy. Shipping is
**off by default** (`NEOFARM_SHIP=1` per host); until then certified
candidates wait in the queue for cancel/ok. The observatory is an AC piece
reading lineage — public by construction, no SDL windows.

## Fleet roles

- **neo** — light node: mechanical ticks, gating, occasional certification.
- **blueberry** — heavy node: grafts, certification, the shipper.
- **minis** — season two (no git; would spool through blueberry via rsync).
- Lineage shards are per-host; git reconciles them; artifacts never need git.

## Season one cut-lines

In: ISA v1 + reference interpreter + assembler/disassembler + fuzz proof of
mutation-totality; daemon tick loop; gate; lineage shards; LaunchAgent on neo;
digest skeleton. Out (deliberately): WASM/native backends, minis, autoship,
the observatory piece, Tezos anything.
