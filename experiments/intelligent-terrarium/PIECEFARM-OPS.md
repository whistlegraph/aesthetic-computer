# Piecefarm live operations

Last verified: 2026-07-24 17:46 PDT

## Current season

- Host: `jas-nzxt` (`jastow`)
- User service: `piecefarm.service`
- State root: `/home/me/.local/share/piecefarm/state`
- Experiment: `/home/me/piecefarm/experiments/intelligent-terrarium`
- API: loopback-only at `127.0.0.1:8788`
- Displays: two native SDL3/OpenGL panels at 2560×1440, approximately 142–143 FPS
- Visible embodiment: twelve fixed-address RGB residents, sampled pixel-perfectly
- Search population: heterogeneous 32×32, 64×64, 128×128, and 256×256 fields
- Audio: active stereo Piecefarm stream on the tower's ALC1220 analog Line Out
- Search cadence: 100 ms launch clock, eight CPU workers, 64-proposal look-ahead

The launch clock and worker count are user-manager environment overrides
(`PIECEFARM_CYCLE_MS=100`, `PIECEFARM_WORKERS=8`,
`PIECEFARM_LOOKAHEAD=64`). They survive service restarts inside the current
user-manager lifetime, but not necessarily a logout or reboot. The source
defaults remain deliberately more conservative.

The service is running but deliberately **disabled** for login autostart. Do
not enable it or make the current launch/worker overrides permanent without a
keeper decision.
The separate fleet status cockpit is closed; its login autostart entry is
unchanged.

## Loopboy caretaker

- Canonical prox: `neo:tal#A2F2270F` (host machine `neo.local`)
- Agent: Codex on `neo.local`, advertised as `neo` by the fresh fleet ledger
- CWD: this experiment directory
- Private route: the existing Piecefarm `alex` Loopboy contact
- Delivery: isolated prox inbox; no Terminal, clipboard, pointer, or keyboard injection

The caretaker verifies service health, iteration movement, display assignment,
display rate, and the actual PipeWire sink/port. It should re-enter
`prox_loopboy_wait` after each handled message.

The caretaker may operate the already-authorized Piecefarm surface. It must not
reset or delete state, commit or push the source worktree, deploy, expose the
loopback API, stop the farm, change the CUDA miner, enable autostart, or edit
shared files unless an incoming request explicitly authorizes that action.

The earlier `neo:tupas#758ACE5E` rock disappeared from Neo's fresh ledger after
the client switched to a mobile network. Its first replacement inherited an
unrelated stale session and was allowed to exit. At 17:57 PDT the genuinely
fresh guarded `neo:tal#A2F2270F` session was launched in the same worktree and
bound to the same isolated `alex` Loopboy route; the autonomous jas-nzxt
service did not stop during either control-plane change.

## Capacity reading

The desktop is near its shared compute capacity, but not its RAM capacity.
With the 64-proposal look-ahead live, Piecefarm uses roughly 5–7 logical cores
in Node plus 1.1–1.4 cores in the native display/VM. The independent miner uses
about two more CPU cores, so the observed system load is roughly 11 on 12
logical CPUs. Piecefarm is about 1.12 GB current / 1.21 GB peak of its 2 GB
cgroup maximum and the host still has about 12 GiB available RAM. Sustained
two-display presentation is roughly 136–142 FPS. Under the co-resident miner,
mixed-resolution search completes roughly 38–45 evaluations/second, up from
20–25/second with the old eight-proposal barrier.

The RTX 3070 is effectively saturated: the latest process sample attributed
about 84% SM to `matador-miner` and 14% graphics work to Piecefarm. Piecefarm's OpenGL presentation remains smooth,
but GPU search kernels should not be introduced without an explicit
coexistence policy. The bounded look-ahead keeps work queued while admitting
results in proposal order; a 1-worker/4-worker replay test proves identical
archive state. Observed worker utilization is about 72%, with the remaining
loss concentrated in the heterogeneous tail of each 64-proposal window.
Proposal dispatch is now cost-aware: predicted-expensive profiles and stages
enter the fixed worker queue first, while archive admission remains in original
proposal order. A local 64-proposal comparison improved mean window time by
about 1.08×. An ordered rolling commit frontier remains a possible next step;
an unbounded worker increase is not.

## Deployed substrate

The successor substrate is deployed on the running season:

- `PixelGroove<160,RGB24,v1>` maps all 9,216 pixels of the 16-pixel margin to
  one 27,648-byte RGB memory image with protected header, sequence, function,
  body, projection, lifecycle, and source tracks plus mutable state, sprite,
  proposal, and fringe tracks.
- The native runtime decodes the groove, executes one bounded instruction slot
  per virtual tick, exposes PC/needle/pass/lifecycle telemetry, targets 240 VM
  ticks/second, and counts missed physical deadlines instead of changing the
  virtual computation budget.
- UTC authority and resident identity may perturb only the mutable fringe;
  protected code remains hash-checked.
- A bounded worker pool evaluates deterministic proposal batches in parallel
  and admits them to the archive in proposal order. The live setting is eight
  workers with a bounded 64-proposal queue under the unchanged 2 GB service
  limit.
- Four hardware profiles execute in genuinely distinct 32, 64, 128, and
  256-square native backing stores. Coordinates, neighborhoods, flood queues,
  sprites, boxes, temporal volumes, energy metrics, and lifecycle copies all
  obey the resident size. Only the final presentation boundary resamples to
  the fixed 128-square display wire.
- Ordinary JSON/SSE snapshots expose groove metadata only. The local native
  membrane receives full grooves, and `/api/groove/:id` retrieves one groove
  as JSON or pixel-perfect PPM for inspection.
- OpenAI vision considers retained visual-novelty fields plus residents with at
  least eight lifecycle samples that show sustained high health (mean ≥82 with a
  ≥.75 healthy ratio) or meaningful health variability (range ≥24 and standard
  deviation ≥7). Reviews use strict structured output and add a bounded
  criticism, capability class, and one-to-three mutation hints. Review events
  persist separately; the latest review is rehydrated into the archive. Its
  hints may bias a descendant toward a fixed recipe, but every descendant still
  passes the ordinary parser, fixed-memory evaluator, and archive admission.
  Reviews never rewrite HP or the parent. The top board shows persisted review
  and recommendation counts. Pre-deploy evidence: 23 reviews (14 retain, 7
  watch, 2 reject), with no current-session failures. Lifecycle candidates get
  first refusal on the bounded review lane; visual novelty is the fallback only
  after the current cohort has a complete eight-sample window.
- The raster language now has a nineteenth capability, `cellular`, with bounded
  9-bit birth/survival masks. It executes over the resident's true 32², 64²,
  128², or 256² field in both the JS verifier and native VM. Review recipes can
  also bake feedback, masking, symmetry, displacement, tiling, sprite-memory,
  box, contrast, color, and smoothing machinery.
- Bottom tiles reserve their imagery: the only text is one last-executed opcode
  at the lower edge, paired with a compact full-width health bar below it. The
  former address/profile badge, full pipeline, and four-line command history
  are not rendered.
- The tile rim is now an address display rather than a color summary. Every
  perimeter pixel maps to one fixed Groove cell: core pixels expose contiguous
  cells 0–555 (header, sequence, function table, bodies, projection, and early
  lifecycle), while fringe pixels expose contiguous mutable cells beginning at
  5,300. No address decimation, animated reader needle, tint, or interpolation
  remains at this boundary.
- The upper board carries the corresponding margin microscope for the selected
  resident. It follows the native reader and prints the exact Groove address,
  tile-rim coordinate, named track, `LOCK`/`MUT` protection class, literal RGB
  bytes, program counter, lifecycle vector, and decoded opcode. The lower rim
  therefore remains raw WYSIWYG memory while its meaning is readable above.
- A bounded probe membrane makes those addresses operable without granting raw
  writes. `bin/piecefarm-probe A1 64 loopboy` resolves the fixed display address,
  validates the requested core range `0..555` or fringe range `5300..5855`, and
  posts one loopback observation. Keyboard `[`/`]` (Page Up/Down steps eight)
  and Xbox shoulder buttons walk the same address set. Probes are persisted in
  SQLite with specimen, time, track, capability, and requester provenance.
  Only a protected cell that decodes to an allowlisted capability is focused in
  the nursery ecology; the resulting graft still needs normal behavioral
  change, JS proof, exact 64/128/256 native matches, and archive admission.
  An explicit probe remains `pending` for at most eight nursery batches. Each
  compatible proposal must attempt that focused capability before ordinary
  mutation; the durable outcome is `admitted` with descendant id or
  `no-admission`. General tile selection may continue changing independently,
  while the upper microscope stays pinned to the latest probe record. Once a
  graft is admitted, the first upper-board bar changes to `GRAFT <id>` and
  reports whether that exact descendant is still resident, retained in the
  bounded lineage, or historical, together with its generation and live
  child/descendant counts. This is ancestry read from the verified nursery,
  not decorative activity.
- Capability ancestry is now carried explicitly through later accepted
  mutations and through verified two-parent crossover donors. Each PieceVM
  record retains at most sixteen de-duplicated provenance atoms (donor,
  capability, probe time/address/track, requester); older archives reconstruct
  those atoms from their bounded parent/donor graph during replay. The upper
  board separates direct graft survival from propagation: `PROP` reports later
  descendants, current resident carriers, and the furthest generation. `SONIC
  PATH` counts the actual five musigraph voices whose currently executing
  PieceVM program carries that exact probe ancestry, and says whether the
  current champion carries it.
- The native PieceVM membrane no longer repeats one champion at all three
  resolutions. It deterministically assigns A1 to the current verified
  champion at 64², B1 to a high-scoring structurally distinct resident at
  128², and C1 to the furthest currently resident carrier of the latest exact
  probe at 256² (or another ecology resident when there is no carrier). The
  native row carries the PieceVM source id, role, and probe-carrier bit. Since
  the five-voice sampler includes C1, a living probe family now has an actual
  raster and sonic path rather than dashboard-only ancestry.
  Live evidence for probe `d488e9442435` / address 64 / `triangle`: the
  admitted root plus five later carriers are present in Git HEAD `fc0df41`;
  three carriers remain resident and the frontier is generation 46 specimen
  `bd0c6bf16825`. The current embodiment is three distinct programs: champion
  `4bafc78742a1` at A1/64², diversity resident `4838a753010d` at B1/128², and
  probe carrier `bd0c6bf16825` at C1/256². The measured sonic path is one of
  five voices from C1.
- Embodied PieceVMs now close a bounded phenotype loop. The SDL authority
  returns the exact PieceVM id/role/profile/probe bit and actual musigraph voice
  count alongside native HP, lifecycle, change, variance, spatial energy,
  coherence, noise, and muddiness. The server rejects identity/profile
  mismatches. A program needs twelve distinct reports spanning at least eight
  seconds before it can affect selection; each of at most 64 programs retains
  only its latest 60 samples. The resulting native bias is capped at ±0.06 and
  only changes which already verified resident supplies mutation parents. It
  cannot alter a stored score or bypass Lisp parsing, behavioral change, JS
  proof, exact 64/128/256 native framebuffer matching, or ordinary admission.
  A1 is the combined static/native phenotype lead, B1 leases a least-observed
  explorer for a two-minute nursery epoch, and C1 remains the exact probe
  carrier. These windows are nested in `pieceVm.phenotypes`, so the next farm
  Git edition preserves the native evidence used by later selection. The upper
  board line `PHENO` shows parent id, native phenotype score, bounded bias,
  report count, sonic voices, and `GATHER`/`READY`.
  Native snapshots also create a bounded 90-second lease for the exact
  raster/PieceVM id, role, profile, and probe-carrier bit the server served.
  That lets one delayed health envelope survive nursery reassignment without
  accepting an unserved or forged identity; at most 96 leases are retained.
  Live deployment advanced one selected phenotype from 19 to 60 reports under
  active churn with no further PieceVM identity rejection.
  Every admitted descendant now carries its own normalized `selectionEvidence`:
  the exact parent, allowlisted selection policy, static score, phenotype
  report/readiness/score state, ±0.06 native bias, recomputed combined score,
  audible voice count, and capture time. This record survives in both the
  bounded resident set and lineage, so a later Git reader can distinguish a
  phenotype-led child from a champion control, diversity branch, or focused
  probe without reconstructing mutable live state.
  Live Git commit `8e17faf` proves the first such causal descendant:
  native-valid champion `5b028b555a23`, generation 36, mutation `prune-child`,
  was cultivated from `28cc40f8277b` by `phenotype-lead` with 44 ready native
  reports, phenotype score 0.9155, bias +0.04986, combined score 0.9102, and
  one actual musigraph voice.
  Cultivation policy is now learned by a classic UCB1 bandit rather than an
  opaque heuristic. Every four-proposal batch retains one phenotype-led arm,
  one champion control, and one diversity arm; only the fourth proposal is
  allocated adaptively, and that selected arm receives the first scarce native
  verification position (capability ranking still orders candidates within
  the arm). A resolved trial's bounded reward is the mean of
  verified admission and measured structural capability gain, plus the native
  phenotype score once that child has a mature phenotype. The recent window is
  capped at 192 trials and stored under `pieceVm.policyBandit`. The top SDL
  board's `UCB` line shows the next bonus arm (`+P`, `+C`, or `+D`) and each
  arm's trial count, mean reward (`R`), and capability rate (`K`). UCB never
  bypasses compilation, behavioral change, or the three native framebuffer
  proofs, and mandatory control/diversity trials prevent monoculture.
  Live Git commit `35887ca` contains the first complete three-arm evidence
  window (11 resolved trials). UCB chose diversity for the scarce verifier and
  admitted native-valid child `e9cec66da3b4`, whose `memory-oscillator` is a
  measured structural capability gain. At that checkpoint phenotype/control/
  diversity had 5/5/1 trials, mean rewards 0.621/0.800/1.000, and capability
  gains 1/3/1. The one-sample diversity lead remains exploratory rather than a
  declaration of superiority.
  A second bounded UCB1 learner now measures the mutation operator as well as
  the parent-selection ecology. Its arms are `variation` (surface and control
  changes), `machinery` (functions, argument grafts, layouts, memory
  oscillators, and senses), and `exchange` (lineage crossover and environment
  grafts). Every four-proposal batch preserves one request from each family;
  only the fourth family is adaptive. The adaptive policy/family pair receives
  scarce native-verifier priority, while actual outcomes are attributed to the
  operator that really ran. Capability gain includes expanded persistent
  memory as well as functions, arguments, layouts, senses, transforms, and
  inherited capability lineage. This bandit has its own 192-trial window under
  `pieceVm.operatorBandit`, survives Git checkpoints, and uses the same bounded
  verified-admission/capability/native-phenotype reward. The upper ticker shows
  `OP +V`, `OP +M`, or `OP +X` and compact reward/capability rates beside the
  cultivation policy statistics. Neither learner may bypass behavioral or
  three-resolution native framebuffer proofs.
  Live Git commit `214274f` preserves the first balanced operator evidence:
  variation admitted one native-valid `color` child with no structural gain
  (reward .5); machinery admitted one native-valid `data-layout` child with a
  measured gain (reward 1.0); and exchange admitted native-valid
  `environment-graft` and `lineage-crossover` children with measured gains
  (reward 1.0 each). The one-sample/two-sample leads are exploration evidence,
  not a settled ranking. The next adaptive arm is machinery, while the three
  mandatory baseline proposals remain in every batch.
  Exact mutation requests now have a separate causal outcome model. It tracks
  all 16 allowlisted PieceVM operations, distinguishing the requested mutation
  from the operation that actually ran. Each bounded record contains native
  validity, admission, and positive deltas across functions, arguments,
  layouts, layout bytes, memory, senses, transforms, and inherited capability
  lineage; a mature native phenotype score is attached later by exact child
  identity. Unavailable requests receive no fallback credit. Per-family UCB1
  chooses the next exact request, so previously untried operations get first
  refusal while successful operations can be revisited. The model retains at
  most 384 causal trials in `pieceVm.outcomeModel` and is committed with the
  farm state. It only chooses what to propose: parsing, behavioral difference,
  native 64/128/256 proof, niche admission, and the mandatory family schedule
  remain authoritative. The top mission ticker names the exact next operation.
  Live Git commit `83157fe` preserves the first five request/outcome records.
  Three are exact native-valid admissions with measured gains:
  `data-layout` added one layout and 256 layout bytes;
  `lineage-crossover` added three transforms; and `environment-graft` added one
  layout, 256 layout bytes, and one inherited capability. A requested
  `argument-function-graft` and an earlier crossover were unavailable on their
  selected parents and correctly received no fallback credit. The current next
  exact machinery request is `function-graft`, visible on the upper board.
  Exact choice is now parent-compatible rather than a one-shot request followed
  by an unrelated random fallback. For each scheduled family, UCB1 supplies a
  complete ordered preference list. The nursery filters that list through the
  selected parent's compiled structural affordances and asks for the first
  feasible operation. The original top preference, feasible request, and
  actual operation are all retained. A compatibility miss counts as a zero
  exposure for the unavailable preference, while the feasible operation may
  receive its own verified outcome credit. This makes an incompatible
  parent/operation pair lose priority without hiding it, and it prevents an
  unseen but impossible operation from remaining at infinite UCB forever.
  Live Git commit `df01c08` proves the three-part compatibility record. On
  parent `0e34d521915e`, the model preferred `argument-function-graft`; the
  compiled affordance filter selected `data-layout`; `data-layout` actually
  executed, passed native verification, was admitted as `25697cc08559`, and
  added one layout plus 256 layout bytes. The record has
  `compatibilityFallback:true` and `honored:true`: the unavailable preference
  receives a miss while the feasible causal request receives outcome credit.
  Multi-step development is measured by five retained capabilities rather than
  mutation names: named memory (a nonempty data layout), temporal memory (at
  least four memory operations), abstraction (functions plus arguments),
  sensing, and ecological inheritance. Every candidate receives a before/after
  five-bit fingerprint with gained, lost, and retained capability sets. One
  evolution in four is a curriculum lane: a non-regressing breadth increase may
  outrank the one-step UCB pair. The other three evolutions keep UCB priority,
  so curriculum cannot erase exploration or controls. Native verification and
  ordinary admission remain mandatory. At most 192 curriculum outcomes are
  stored under `pieceVm.curriculum`; mature native phenotype evidence is joined
  later by exact child id. The upper board shows `CUR*` during the curriculum
  lane and `CUR Bn/5` plus the champion's five-bit signature.
  Git commit `c959f28` contains the first curriculum-led admission:
  `environment-graft` took parent `bcc546420072` from `00110` to `10111`,
  retaining abstraction+sensing while adding named memory+ecological
  inheritance. The first implementation revealed that an ordinary next
  admission could replace the visible champion with a narrower branch, so the
  lane now explicitly chains from the broadest resident. On each curriculum
  cycle only the fourth proposal becomes `curriculum-chain`; it uses that
  resident and targets its first missing capability. The first three family
  baselines remain variation, machinery, and exchange. Curriculum trials are
  excluded from parent/family UCB rewards, but still inform exact-operation
  outcomes and curriculum evidence.
  The chain parent is chosen by breadth, then target/register compatibility,
  then register and instruction headroom. This avoids selecting an older but
  saturated breadth-four branch when a breadth-four branch can actually afford
  its missing operation. During `CUR*`, the explicit `curriculum-chain`
  candidate owns the first native verification attempt; if it fails proof or
  admission, the remaining ordinary candidates may still proceed.
  Git commit `1e4bd75` proves the first complete chained specimen. Compatible
  parent `ba94a8f74025` began at `11101`; the reserved lane targeted sensing,
  requested and executed `sense-graft`, passed all three native resolutions on
  the first verification attempt, retained named memory+temporal memory+
  abstraction+ecology, and admitted `6afe3abc92ca` at `11111` (5/5). The
  complete resident remains the displayed curriculum champion even as ordinary
  UCB admissions continue on other branches.
  When no active margin probe reserves C1, the broadest verified curriculum
  resident now occupies that 256-square embodiment slot with role
  `curriculum`. This exposes the compound program to native health/phenotype
  sampling and musigraph sonification rather than leaving completion only in
  Git metadata. An active exact probe still has priority over C1.
  Live verification placed `6afe3abc92ca` at C1/256 with role `curriculum`.
  It matured to 15 exact native reports over 212 seconds, phenotype score
  0.5831, positive bounded selection bias +0.00998, and one audible musigraph
  voice while ordinary admissions continued.
- Press `I` or `Return` (Xbox south/A) to toggle a selected resident into a
  centered 1:1 view on the bottom display. Its 32, 64, 128, or 256 backing
  pixels are copied without scaling, cropping, labels, or filtering. Arrow keys
  and the Xbox d-pad select; `Escape` or Xbox east/B returns to the fixed wall.
- The native membrane rotates mutable Groove state back to the authority. A
  controlled post-scheduler restart replay preserved specimen `f0933877df42`,
  advanced its sequence-pass counter from 6,712 to 7,009, and retained
  protected hash `79aa74ef`.
- The top board reads actual history ancestry: edition count, abbreviated Git
  HEAD, and the last committed iteration. A live crossing advanced edition
  140 / `e54c5979` at iteration 194,768 to edition 141 / `c37f9bb2` at
  iteration 196,625; `git rev-parse` and the loopback snapshot agreed.

Latest local gate: 112/112 Node tests pass; the native build is warning-clean
under strict C11 `-Werror`, and its sonic/raster replay self-test passes. The
live native membrane consumes all 102 scoreboard fields.
The display authority uses a three-second stale-data grace window so a single
250 ms loopback poll miss cannot flash a false disconnect banner over a valid
board.

The musigraph now uses a deep-night Dorian map from a 6.875 Hz fundamental,
two octaves below its earlier voice map. Five voices derive one of four smooth timbral families from
resident memory, add separately phased sub and second-partial components, and
pan from stable tile position plus measured color axis. Frequency, gain, pan,
sub, overtone, and wavetable changes remain interpolated in the audio callback;
the existing soft saturation and click-free transient envelopes remain in
force. The top spectrum is measured from those actual synthesized partials.
The kiosk now defaults its own Piecefarm stream to 42% rather than 115%; the
current tower analog Line Out sink is 38% for the quieter night session.
AddressSanitizer and UndefinedBehaviorSanitizer pass locally (the Fedora host
does not have the ASan runtime installed).

## Quick verification

```sh
ssh jas-nzxt 'systemctl --user is-active piecefarm.service'
ssh jas-nzxt 'curl -fsS http://127.0.0.1:8788/api/state | jq "{iteration, coverage, runtime, memory}"'
ssh jas-nzxt 'journalctl --user -u piecefarm.service -n 20 --no-pager'
ssh jas-nzxt 'env XDG_RUNTIME_DIR=/run/user/1000 wpctl status -n'
```

Never print service environment values or secret-bearing configuration while
debugging. The OpenAI proposal/curation membrane remains capability-bounded and
loopback-only.
