# Face expressions — sleeping, breathing, listening, watching

A design for what the three faces do when they are *not* singing, so the trio
reads as three bodies in a room rather than three mouths that appear for a
line and vanish. Applies to the full-screen show face and the corner ghost /
sim tile (`SimSlot`; `corner=1|<pt>` and `sim=i/n` on the play payload).
Line numbers are for `slab/menuband/Sources/MenuBand/*` as of Sept 24
(working tree, with the corner-ghost edit in). Study only; nothing here is
implemented yet.

## 1. What the face can already do

Controller `SingerFace` (SingerFace.swift):
- `SimSlot` L20–68: sim tiles by index; `corner` (L26, `SimSlot.corner(_:)`
  L37) hangs a 280 pt tile under the menu-bar keys (`keyboardFrame` L46,
  `tile(on:)` L50).
- `configure(epoch:bpm:expression:)` L85 — the beat clock
  (`performanceEpoch`, `performanceBpm`) and `expression` from the score's
  `performance` key. `setExpression` L90.
- `inhale()` L95–103 — a 0.38 s sine ramp *before the first cue of a phrase*.
- `follow`/`mouthPose()` L105–123 — visemes from `SingerArticulation` on the
  player's sample clock (`MenuBandSingerVoice.swift` L107).
- `show(member:accent:skin:gaze:)` L128 (sets `feather`/`watchKeys` for the
  ghost L144–145), `onset(_:hold:)` L158, **`rest(_:)` L164** (new: between
  lines), `meter(rms:zcr:)` L170 (audio tap, `MenuBandSingerVoice.swift`
  L63), `hide()` L174.

View `SingerFaceView`, per-tick state (`tick()` L390–478):
- beat clock L395; `vitality` from `effort` (audio RMS) L397–398;
  `beatPulse` 1 on the bar, 0.36 on the beat L403; backbeat blinks L406.
- `livingPhase`/`boil`/`boilBlend` L409–413 — the boil, 10–14 Hz.
- idle blink L414 (**only when `expression == 0`**; L406 needs
  `vitality > 0.08` — a quiet face with a performance never blinks: bug).
- gaze L421–451: `wanderT` two-sine drift, `saccade` glance every 1.2–3 s
  L422–426, room person wins L430, **`watchKeys`** up at the keys L434,
  **`drowsy` ≥ 0.5** lazy look L438, `tau` 0.09/0.2/0.25/0.5.
- `squash`/`lean` L452, L374–382; jaw from meter L456–461; `effort` L465;
  `breath` (pre-phrase inhale) L466–467; **`drowsy` eased from `resting`**
  L470, plus a free sine rammed into `breath` while resting L471; `bob` L477.

`drawArtwork` L526–830: `life`/`motion`/`softBreath` L546–551; sway, BB nod,
frisbee bounce L581–583; translate/rotate/zoom L584–587; **`breathing`**
lift L593 (new: `sin(bob*1.3)*H*0.012*(0.3+0.7*drowsy)`); onset squash
L595–597; hair L600–655; cheeks L657–681; eyes L683–716 (blink shut curve
L693–696, pupil L700–703 with a `drowsy` droop, **lids for everyone** L704–716:
`lid = lidBase - drowsy*(lidBase + eh*0.72)`); brows and neo's fixed
`curiosity` L718–735; nose L738–752; mouth L754–818.

Payload (`bin/trio-payload.mjs`): `face=`, `faceAlpha=`, `sim=`, `corner=`;
`AppDelegate.swift`: ghost shown resting at `downbeatEpoch − 0.6` L5025–5032,
`rest(false)` 0.4 s before a line L5097, `rest(true)` 1.2 s after L5115,
`hide()` 1.8 s after the last syllable L5121.

What is still missing:
- The camera, brows and cheeks are multiplied by `mood`/`motion`/`life`
  (L584–587, L662, L720), i.e. by *audio effort*. `breathing` (L593) is the
  one term that survives silence, and it is a bare sine on `bob`, not a
  breath with an inhale and a settle, and it moves only y.
- `resting` is one bit. There is no asleep vs. drowsy vs. attentive, no
  waking, no "listening to a bandmate": the show face still appears 0.4 s
  before its first syllable (L5096) and hides after its last (L5121), so a
  member who waits 68 s in *Birth* is absent.
- The face knows nothing of the others. The payload carries only this
  member's part; nothing says who is singing now, or where they sit.

## 2. State machine

```swift
enum FaceState { case asleep, waking, idle, watching, listening, singing }
```
Resolved every tick from `beat` and a `FaceTimeline` (§5/§7). `resting`
stays as the coarse bit the conductor already flips; the timeline refines it.

| state | enters when | leaves |
|---|---|---|
| asleep | show → first line (own or other) ≥ 8 beats away; or nobody singing for ≥ 12 beats | 4 beats before any line (own → waking, other → listening) |
| waking | 4 beats before own first line after asleep | own line starts → singing |
| singing | own line span active (0.4 s lead, as `rest(false)` today) | span end + 0.3 s |
| listening | another member's span active and mine is not | its span ends |
| watching | nobody singing, gap < 12 beats, a bandmate sang within 4 beats or sings within 4 | timeout → idle |
| idle | nobody singing, no imminent line | ≥ 12 beats → asleep (3-beat `drowse` ramp) |

`drowse: CGFloat` 0…1 is continuous (replaces `drowsy` as the eased value:
idle 0 → asleep 1 over ~3 beats; waking runs it 1 → 0 with a stretch: brows
up, one slow blink, zoom +0.02). All three singing: singing wins, `groove`
(§5) still colors brows. In the corner ghost the face falls asleep after the
piece and stays (it already never leaves); the show face hides as today.

## 3. Breath loop (never stops)

New `breathPhase: Double` (0…1) and `breathe: CGFloat` (0…1); the L593
`breathing` sine and the L471 hack become this. Advance in `tick()`:
- free rate by state: asleep 0.16 Hz (6.2 s), idle/watching 0.22 Hz (4.5 s);
  listening/singing **beat-locked**, one breath per two bars —
  `phase = (beat / phraseBeats).fract`, `phraseBeats` 8 (4/4) or 6 (3/4;
  pass `meter=3/4` from `score.arrangement.meter`). Lock by slewing the free
  rate toward the beat rate over one cycle, never snapping, so *Wake*'s 69 bpm
  waltz and *Sums*' 96 bpm both settle.
- shape: inhale 40 % (smoothstep up), exhale 60 % (slow settle) — a quick
  lift and a long sag, old-cartoon.
- depth by state: asleep 1.0, idle 0.6, listening 0.8, singing 0.35 (the
  phrase `breath` inhale L466 rides on top for the attack).

Modulation — additive terms **outside** the `mood`/`motion` products:
- zoom L587: `+0.012*breathe`; y translate L585: `+H*0.006*breathe`
  (replacing L593).
- lids: `+0.06*breathe` toward closed on the exhale (§4).
- brows L720: `+H*0.004*breathe`.
- mouth rest: closed-smile corners L763 `+H*0.004*breathe`; asleep mouth =
  `SingerViseme.hum.pose` with `jaw = 0.04*breathe` (lips part on exhale).
- cheeks L660 height and L662 alpha: `breath` → `max(breath, 0.5*breathe)`.
- `bob` L477: advance at the breath rate when idle instead of 1/s.

## 4. Eyes

Promote the L704–716 lid to `lids: CGFloat` 0 open … 1 shut, smoothed with
`tau` 0.06 (blink) / 0.4 (drowse), and make the blink a `lids` pulse instead
of the binary `blinking` L684. Draw as now (accent rect clipped to the eye +
ink edge); at `lids ≥ 0.97` the shut curve L693–696 with control points
*below* `cy` (sleeping) rather than above (blink). Pupil hidden past 0.9.

Targets: asleep 1.0; drowse ramps 0.55→1 with nodding off (every 3–6 s
lids sag +0.2 for 0.8 s, recover); waking one 0.4 s slow blink then 0;
listening 0.15; watching/idle 0.05; singing 0 + backbeat blinks (L406 gated
on `beat >= 0`, not `vitality`); idle blink L414 loses `expression == 0`.

Gaze goal per state (feeds `gazeTarget`, keep the L449–451 smoothing):
- asleep: (0, −0.2) frozen; `saccade` scale 0. drowse: L438 as is.
- listening: `bandDirection(singer)`; a glance to the room person every 6–10 s.
- watching: last/next singer 70 %, room person 30 %.
- idle: wander + saccades as now L427; room person wins as now L430.
- corner ghost: keys are above — L434 already aims (x·0.35, 0.55). Add the
  lit column: `AppDelegate` computes `display` before `drumLitOn` (L5129);
  call `face.keyLit(column: (Double(display)-60)/23*2-1)` there, and let
  `keysGoal.x` ease to it (tau 0.15) while idle in the corner.

Where the others are — from the conductor: `band=neo,blueberry,frisbee;me=1`
(roster order = left→right on stage, `parts[].idx`, `trio.mjs` L330). In the
view: `bandDirection(j) = (x: 0.85*sign(j-me), y: 0.05)` plus a per-member
wobble; sim tiles use the same formula on `SimSlot.index` (they already sit
left→right, `tile(on:)` L64–65, `pan` L68).

## 5. Grooving / listening

The payload must say when the *others* sing. Add to `singerPayload`
(`bin/trio-payload.mjs`), computed for every other part from `voice.notes` +
`lyrics` the way `SungLine.splitLines()` does (`MenuBandSinger.swift` L77:
walk tokens, `r:N` advances, one sounding note per syllable, `/` ends a line):

```
band=neo,blueberry,frisbee;me=0
others=blueberry:0-6|6-12|…|54-60,frisbee:18-21|36-39|54-60
lineRoles=lead,lead,lead,lead,hum,lead,lead,hum,hum
```
Beats from `startEpoch`. Separators avoid `;`/`=` (the kv delimiters): `,`
between members, `|` between spans, `-` inside a span. `others` is the whole
per-member line schedule, so a face also knows who sings *next* and turns
early. Own spans come from `tracks[0]` and `lineOfSyllable` (L4995) — no key.

Reaction in the view, driven by `groove: CGFloat` (0→1 over one beat when
listening, decays over two):
- nod: blueberry's L582 becomes everyone's, `* max(life, groove)` with
  member amplitudes (BB 0.014, neo 0.010, frisbee 0.016 + her bounce L583).
- sway L584: `motion * sway` → `max(motion, 0.6*groove) * sway` (periods
  stay: BB 8, neo 6, frisbee 3 beats).
- brows L720: `beatPulse*life` → `beatPulse*max(life, groove)`; on a
  bandmate's line *start* a 0.3 s raise + blink (the "turn" cue).
- head tilt toward the singer: `rotate(by: 0.03*dir.x*groove)` after L586.
- eyes: §4 listening goal; cheeks `+0.08*groove` in the alpha L662.
- mouth: closed smile, corners `+H*0.01*groove`; role `hum` lines keep
  `.hum` (lips sealed) so a humming member reads as humming, not asleep.
`beatPulse` L399 is gated on `expression > 0`; gate on `beat >= 0` so a
score without `performance` still grooves.

## 6. Facial vocabulary (parameters)

| param | range | driven by |
|---|---|---|
| `breathe` | 0…1 loop | §3 rate/depth by state; bpm-locked when music plays |
| `lids` | 0…1 | state target + blink pulses + `0.06*breathe` |
| `browLift` | −1…1 | −0.6 asleep, −0.2 drowse, +0.5 waking, `beatPulse*groove`, `life` |
| `browTilt` | −1…1 | neo's `curiosity` L721 as a param: toward the singer when listening |
| `cheek` | 0…1 | `life` (L662) + `0.08*groove` + `0.05*breathe` |
| `tilt` | −1…1 roll | sway (L586) + `0.03*dir.x*groove` |
| `nod` | 0…1 | beat-locked cosine × `max(life, groove)` |
| `zoom` | 0.95…1.1 | L587 + `0.012*breathe` + waking stretch |
| `squash`/`lean` | existing | onset L374–382, unchanged |
| `mouthRest` | rest / hum / parted / smile | asleep hum-parted, listening smile, idle rest, `hum` role hum |
| `pupil` | 0…1 scale | 0 asleep, 0.9 drowse, 1 else (droop L701 stays) |
| `gazeGoal` | (−1…1, −0.6…0.6) | §4 priority per state |

Everything still passes through the boil (`j()` L482) and the ink widths;
only lids and gaze are tweened, the rest keys on the boil so it stays
hand-drawn.

## 7. First cut (one sitting, ~200 lines) vs. later

Order, all additive to the working tree:
1. `SingerFaceView`: `breathPhase`, `breathe`, `phraseBeats`; advance in
   `tick()`; §3 terms at L585/L587/L660/L720/L763; drop L471 and L593. (~30)
2. `lids` + `lidTarget` replacing the L684 binary and the L707 formula;
   blink = pulse; fix the two blink gates. (~35)
3. `struct FaceTimeline { var own: [(start: Double, end: Double, role: String)];
   var others: [(member: String, start: Double, end: Double)];
   var band: [String]; var me: Int; var phraseBeats: Double }`, `FaceState`,
   `state`, `drowse`, `groove`, `func resolveState(beat:)` in `tick()`;
   `resting` remains a floor (`resting && no timeline` → idle/drowse). (~55)
4. `bandDirection(_:)`, `keysGoal`, `func keyLit(column:)`, per-state goal in
   the L429–447 chain. (~25)
5. Groove terms at L582–586, L720, L662. (~20)
6. `SingerFace.schedule(_ timeline: FaceTimeline)` → view; `AppDelegate`
   after L5020: build `own` from the L5035 loop (`k == 0` start, last
   syllable's `offAt` end, `lineRoles[li]`), parse `others`/`band`/`me`/
   `meter`; show the *show* face too at `at(downbeatEpoch − 0.6)` like the
   ghost L5027 so a waiting member sleeps on stage; hide at
   `max(partBeats, othersEnd)*beat + 1.8` instead of L5121. (~30)
7. `trio-payload.mjs`: `band`, `me`, `others`, `lineRoles`, `meter` — pass
   `parts` in (`singerPayload({p, parts, …})`, `trio.mjs` L448). (~20)

Later polish: nodding-off sag; waking stretch and yawn (`.ah` pose 0.6 s,
lids 0.7); lit-key column in the corner; `SingerGaze` for the ghost (only
`slot == nil` starts the camera, L146); per-character sleep (BB one brow up,
frisbee's curls droop via `hop` L618); a glance at whoever *just* stopped;
`wake=` to rouse a face for its spoken intro line.
