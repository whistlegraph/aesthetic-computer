# pop / cult — "whistlegraph cult --- remix"

A remix of **`cult` — "The Three of Us Are in a Cult"** (Whistlegraph,
2022, 39.6M views), the whistlegraph whose caption was already the whole
idea:

> dot dot dot dash dash dash dot dot dot (jk)

SOS in morse, joke retracted. The three of us are Jeffrey Alan Scudder,
Camille Klein and Alex Freundlich.

**The hook, as @jeffrey specified it:**

> dash — i wanna — dash — i wanna — run it fast — dot dot

There are six versions in `out/`. **v6 is the current one.**

| cut | file | what it is |
| --- | ---- | ---------- |
| v1 | `out/cult-remix.mp3` | 4:18, B minor, 128 BPM. Club banger. Kept for reference. |
| v2 | `out/cult-remix-v2.mp3` | 4:00, 120 BPM. Chill techno, and the words are sung. |
| v3 | `out/cult-remix-v3.mp3` | Chorus-first: the dictated four-line chorus as the spine. |
| v4 | `out/cult-remix-v4.mp3` | v2 re-scored — "run REAL fast", a harmonised "cult". 0:50 cut. |
| v5 | `out/cult-remix-v5.mp3` | 4:03. The whole record given a narrative, in ten acts. |
| **v6** | **`out/cult-remix-v6.mp3`** | **4:03. v5's ten acts, with the polyrhythms rising as the voices sink and stair-step.** |
| | `out/cult-remix-v6-50s.mp3` | 0:48 → 1:38 — act III into the turn. |
| | `out/cult-remix-v6-extended.mp3` | 0:48 → 3:28 — acts III through VII. |

---

# v6 — "the descent"

@jeffrey heard v5 ("way better") and asked for three things:

> 1. as the track progresses can we bring up the polyrhythms a bit
> 2. and also bring down the vocals / so daaaaash daaaash gets lower in octaves
> 3. and can we like quantize it more like "bad" autotune it / eiffel 65 style

The acts, the narrative, the kick, the tube pump, the signal layer and the
skids are all v5's and are unchanged. What v6 adds is that the three notes
above are **one move seen from either side**: over four minutes the voices
recede — quieter *and* literally lower, F#4 down to D3 — and the
cross-rhythms rise into the room they vacate. It is a deliberate crossfade
of attention. By act VII the lead is a stair-stepped machine near the bottom
of its range and the thing carrying the record is five rhythms interlocking
over one 4/4 kick.

`bin/render5.mjs` and `bin/cut-v5.sh` are untouched; v6 lives in
`bin/render6.mjs`, `bin/cut-v6.sh` and `bin/sing6.mjs`.

## 1. The polyrhythm arc

`polyAmt(bar)` is a single piecewise-linear 0→1 curve across the 120 bars.
Five cross-rhythm layers switch themselves on as it crosses their own
thresholds — nothing is scored per act; **the arc is the score**.

| layer | period | enters at | made of |
| ----- | ------ | --------- | ------- |
| **L1 · 3:4** | 4⁄3 beat (0.667 s) | act II, one tick | clicks; a closed hat from `a ≥ 0.36`; a ride from `a ≥ 0.66`; **woodTaps** wherever the machines are off |
| **L2 · dotted eighth (3:16)** | 0.375 s | act III | taps — 5⅓ per bar, so the figure only re-meets the downbeat every **three bars** |
| **L3 · 5:4** | 4⁄5 beat (0.4 s) | act V | clicks; every other bar until act VII, then every bar |
| **L4 · 7:16** | 0.875 s | act VI | DTMF off the same C-U-L-T keypad, plus a bop — takes **seven bars** to come back round |
| **L5 · the interlock skid** | on the 3:4 grid | act VII | the TrackDrum friction voice, snare/rim band, cutoff 1900→1000, carrier 239→149 |

L2 and L4 are counted from **t = 0**, not from the downbeat. That is the
whole point of them, and it is why the figure lands somewhere new every bar.

The arc, act by act:

| act | `polyAmt` | hits/bar | what it is |
| --- | --------- | -------- | ---------- |
| I · carrier | — | 0 | nothing. A channel opening has no pulse, and the rise only reads as a rise if it starts from actual silence. |
| II · three voices | 0.10 → 0.12 | 1.9 | one 3:4 tick a bar, so the ear files it as colour |
| III · the message | 0.28 → 0.34 | 7.0 | 3:4 every bar, and the dotted eighth begins to precess |
| IV · the secret | 0.16 | 0.1 + woodTaps | the transmission has stopped: **3:4 in woodTaps only**, no beeps |
| V · the reply | 0.40 → 0.52 | 13.3 | 5:4 enters; 3:4 picks up a hat |
| VI · it spreads | 0.62 → 0.68 | 15.0 | the 7:16 phone figure joins |
| VII · the whole message | 0.78 → **0.92** | 17.3 | **all five, interlocking**, plus the skid on the 3:4 grid |
| VIII · recognition | 0.80 → 0.55 | 12.9 | the polyrhythms are the **last** thing to peel off, because by now they are what you are listening to |
| IX · the humans | 0.18 | 0.1 + woodTaps | under the raw recording, a hand keeping 3:4 and nothing else |
| X · carrier off | 0.30 → 0 | 2.6 | the beeps outlast the music, as v5's did |

**Why it stays chill techno.** The kick never leaves 4/4 and the hats never
leave straight eighths. A polyrhythm is hypnotic when the pulse is
unambiguous and the pattern disagrees with it; it becomes chaos when the
pulse itself is negotiable. So the 4/4 grid is never touched and the
disagreement lives entirely in voices small enough to be texture. No drops,
no risers, no crashes — v5's rule, unchanged.

**The levels were set by measurement, not taste.** The first pass sat 25 dB
under the bed (poly-only signal stem at −46 dBFS RMS against music at
−16.8) and simply was not a cross-rhythm you could follow. The shipped
levels put the layer where the record's *existing* phone voices already
live — the narrative clicks run 0.44–0.70, the taps 0.38–0.68, the dials
0.46 — so the polyrhythm is as present as the phone and no more:

| act | poly-only signal stem, RMS | full signal stem | music stem |
| --- | ------------------------- | ---------------- | ---------- |
| II | −55.1 dB | −42.4 | −21.0 |
| III | −39.9 | −36.4 | −16.8 |
| V | −37.4 | −27.6 | −16.6 |
| VI | −35.5 | −28.8 | −16.7 |
| VII | **−33.0** | −27.5 | −16.6 |
| VIII | −37.1 | −34.5 | −18.1 |

RMS under-reads a 4 ms click badly, so the honest companion figure is peak:
in act VII the poly-only signal stem **peaks at −1.6 dBFS**, against the
drums stem's −2.1. Per transient it is fully present; per second it is
texture. That is the definition of a tick layer.

Debug: `MUTE=poly3,poly5,polydot node pop/cult/bin/render6.mjs` subtracts
the whole arc; `ONLY=poly3` renders one layer alone. (v6 also added the
`allow()` gate to `noiseHit`, which was the one voice family `MUTE=` could
not previously reach.)

## 2. The descent

Two separate moves under one note.

**The ladder.** Every dash in the score names a *rung* — `fs4`, `d4`, `b3` —
and `rung()` walks that name down by however many steps the narrative has
taken by this bar. The same written figure sinks an octave and a fourth
between act III and act VIII without a single note being rewritten.

| stage | from | `fs4` → | `d4` → | `b3` → |
| ----- | ---- | ------- | ------ | ------ |
| 0 | bar 0 (acts I–IV) | fs4 | d4 | b3 |
| 1 | bar 48 (act V) | d4 | b3 | fs3 |
| 2 | bar 64 (acts VI, VII/1) | b3 | fs3 | d3 |
| 3 | bar 84 (acts VII/2–X) | fs3 | d3 | d3 |

The rungs below B3 are **real lower takes** of Camille and Alex, rendered
by `bin/sing6.mjs` — not the same take pitched down. A transposed voice
keeps the formants of the octave above it and reads as a machine, and the
machine in this record is supposed to be the autotune, not the descent.
Jeffrey already had the floor of the ladder from v5.

**The ride.** `VOXG` drops 1.42 → 1.22 and `TUBEG` 1.00 → 0.92 (a static
−1.3 dB and −0.7 dB), and on top of that both vocal buses ride a
piecewise-linear curve:

| bar | 0 | 24 | 48 | 64 | 76 | 88–96 | 104 | 120 |
| --- | - | -- | -- | -- | -- | ----- | --- | --- |
| gain | 1.00 | 1.00 | 0.94 | 0.90 | 0.86 | **0.83** | 0.96 | 0.90 |
| dB | 0 | 0 | −0.5 | −0.9 | −1.3 | **−1.6** | −0.4 | −0.9 |

It bottoms exactly where `polyAmt` peaks, and comes back up for act IX
because the humans are the reveal of the whole record and must not arrive
shy. The steepest segment moves 0.08 over 12 bars — 0.3 dB every 8 seconds
— so there is no step anywhere in it, and the side return is scaled by the
same curve so the space recedes with the voice rather than outliving it.

The tube bus deliberately takes **less** static trim than the vox bus: a
held dash that is also descending into the pad's register has already lost
presence before anything touches its fader. Measured on the act VII
"a—waaaay" window, `TUBEG` 0.86 put it 2.6 dB under the bed; 0.92 keeps it
where a lead belongs and still reads as quieter than v5.

## 3. Hard autotune — the Eiffel 65 stair-step

`bin/sing.py` grew a real DSP mode, `--autotune hard`. It is not a preset
name; it is the three settings that make "Blue" sound like "Blue", each
pushed to its stop.

1. **Snap.** Every frame's target is forced onto the nearest member of a
   scale (B natural minor: B C# D E F# G A). Nothing lands between scale
   degrees, ever, so the pitch alphabet is seven letters wide.
2. **Zero retune time.** No portamento, no legato kernel, no overshoot, no
   preparation, no vibrato — the four Saitou expression effects that
   `target_contour()` exists to add are all skipped. The target jumps from
   one scale tone to the next **inside a single 5 ms WORLD frame**, and
   that discontinuity is the artifact you hear.
3. **Rhythmic quantization.** Frames are bucketed onto a note grid
   (125 ms = a sixteenth at 120 BPM; 62.5 ms for the short syllables) and
   each bucket is held at ONE pitch, the median of its own contour, so the
   steps land *in time with the record* rather than wherever the syllable
   happened to move.

What gets stepped is the **performance's own pitch wander**, not a
sequenced melody: the source f0 is read along the same warp as the spectral
envelope, high-passed against its own ~350 ms moving average (keeping the
micro-wander, dropping the slow glide that is the melody's job), scaled by
`--autotune-drive`, clamped by `--autotune-range`, and added to the scored
note. That is why it reads as a *retune of a real take*.

**Formants are never touched.** `cheaptrick`'s envelope goes into
`pw.synthesize()` unmodified, so it is retuned, not chipmunked, and the
words stay legible — which was the condition on the ask.

Two failure modes were found by measurement and fixed:

| symptom | cause | fix |
| ------- | ----- | --- |
| "run real fast" landing on **A3** instead of D4 | the whole slow pitch fall of a spoken word was being fed to the snapper | high-pass the contour against its own moving average (`--autotune-smooth-ms`) |
| "i wanna" jumping a **fifth** off the note | a 0.20 s syllable spans 1½ cells at 1/16, so one cell's median decided the whole word | finer grid (1/32) for short words **and** a leash (`--autotune-range`, 2–4 semitones) applied before the snap |

Measured on `run-real-fast` at drive 3.2, pyin reports the notes at
**+8¢ / +13¢ / −2¢** — the melody is intact — while the frame-by-frame
contour steps F♯4 → G4 → F♯4 → G4 → F♯4 → G4 → D4 → C♯4 → D4 → C♯4 → D4,
every step landing on a 125 ms boundary. That is the sound.

`bin/sing6.mjs` renders the `at-` bank: the hook and chorus leads, and
Camille's and Alex's dashes at every rung of the descent. **Jeffrey and the
dots stay human** — he is the floor of the unison and the sub-octave under
it, a stair-stepped bass warbles, and leaving one real person under the
robot is what stops the stack reading as a synth. The crossover wiggle also
halves where the autotune is on, because hand vibrato under a hard retune
is two pitch modulators fighting and the retune wins anyway.

`AUTOTUNE=0 node pop/cult/bin/render6.mjs` renders the whole record with
the human takes throughout, which is the A/B to reach for if the robot is
ever too much.

## Run it — v6

```bash
node pop/cult/bin/slice.mjs             # the sample bank (v1's, unchanged)
node pop/cult/bin/sing.mjs              # v5's speech-to-singing bank (cached)
node pop/cult/bin/sing6.mjs             # v6's descent + `at-` bank (cached; 26 renders)
node pop/cult/bin/render6.mjs --stems   # score → out/cult-remix-v6-full.wav + bus stems
bash pop/cult/bin/cut-v6.sh             # master once → full + 0:50 + 2:40 mp3s
node pop/cult/bin/qc.mjs pop/cult/out/cult-remix-v6.mp3
```

`cut-v6.sh` is `cut-v5.sh`'s method exactly: measure the render **once**,
apply **one static dB of gain**, limit. Never a second `loudnorm` pass.

## Measured — v6

| check | full | 0:50 cut | 2:40 cut |
| ----- | ---- | -------- | -------- |
| duration | 243.2 s | 50.0 s | 160.0 s |
| integrated loudness | **−14.0 LUFS** | −12.5 | −13.3 |
| true peak | **−1.3 dBFS** | −1.7 | −1.6 |
| loudness range | 8.2 LU | 3.1 | 3.2 |
| max raw sample step | 0.520 | 0.357 | 0.514 |
| max 2 kHz-band step | **0.135** | 0.112 | 0.133 |
| full-scale runs ≥ 3 samples | **0** | 0 | 0 |
| mono fold-down, whole track | **−0.11 dB** | −0.19 | −0.10 |
| mono fold-down, worst window | −0.86 dB | −0.68 | −0.68 |

The raw-step figure is up on v5's 0.365 for the same reason v5's was up on
v4's: **there are more clicks in it on purpose** — about a thousand more.
Muting the three polyrhythm layers drops the count of steps > 0.35 from
27 to 11, which identifies them as the click voices themselves. The 2 kHz-band
step is what actually distinguishes a discontinuity from a fast transient
(a real signal in that band cannot move more than ~0.26 per sample), and it
is **0.135 against v5's 0.117** — the same order, nowhere near the bound.
No discontinuities were introduced.

Bus balance, from `--stems` (this is the check every earlier version failed
at least once, and v6 is the version where it needed the most care, because
the vocals are being pulled down **on purpose**):

| bus | v5 | v6 | Δ |
| --- | -- | -- | - |
| vox (sung words, dots, choir, grains) | −13.9 | **−15.9** | −2.0 |
| tube (the held dashes) | −16.5 | **−18.2** | −1.7 |
| drums (kick, hats, perc, skids, poly hats/ride) | −15.9 | −16.0 | — |
| music (bed, pad, stabs, bass, delay) | −17.5 | −17.5 | — |
| signal (beeps, bops, clicks, taps, poly ticks) | −25.4 | −25.6 | — |

Whole-file integrated loudness hides what matters, so the check that
actually answers "is the lead still proud of the bed" is measured **on the
phrase**, over the 2.4 s a lead line occupies:

| window | v5 vox | v6 vox | music | v6 lead over bed |
| ------ | ------ | ------ | ----- | ---------------- |
| "run real fast", act III (0:52) | −12.3 | −13.8 | −16.5 | **+2.6 dB** |
| "run real fast", act VII (2:32) | −10.8 | −14.3 | −16.4 | **+2.1 dB** |

Two dB rather than v5's four-to-five. That is the ask, and it is still the
loudest thing in the window.

---

# v5 — "the signal"

@jeffrey's note on v4 was five items, and the fifth reframes the other
four: *"lets add an overall narrative to the musical composition now"*.

## The narrative

The source is eight seconds of three friends chanting morse at a camera,
captioned **"dot dot dot dash dash dash dot dot dot (jk)"** — SOS,
disclaimed. So the story is already in the material:

> **A signal goes out, and it is answered, and the joke turns out to
> be true.**

The arrangement spends four minutes taking the *(jk)* away. Ten acts, and
nothing in the track exists that does not belong to one of them.

| act | at | what happens | what the listener learns |
| --- | -- | ------------ | ------------------------ |
| **I · CARRIER** | 0:00 | DTMF tones dial **C-U-L-T** (keypad 2-8-5-8) into empty air over a drone. No drums, no words. | there is a channel |
| **II · THREE VOICES** | 0:16 | The pulse arrives and the three registers enter **one at a time** — Jeffrey B2, Alex F#3, Camille C#4. At bar 14 they land on one syllable for the first time and *wiggle into unison*. Then SOS, twice. | there are three people, and the signal is SOS |
| **III · THE MESSAGE** | 0:48 | The hook, legible, over the thick kick. The dashes ring in a tube. | the message has words |
| **IV · THE SECRET** | 1:20 | **THE TURN.** Kick out. The beeps stop — nobody is transmitting — and one lone click is a receiver being picked up. The word arrives alone, harmonised: **cult**. | the word is "cult" |
| **V · THE REPLY** | 1:36 | The hook returns, and in the hole it leaves SOS comes back **beeped, in DTMF**, from the far side of the field. | something answered |
| **VI · IT SPREADS** | 2:08 | No hook. Held dashes at the widest wiggle, a switchboard of beeps, and **other people's "cult"** — six utterances harvested from six *other* whistlegraph posts across 2022, arriving from the edges. | it has spread, and it no longer agrees about pitch |
| **VII · THE WHOLE MESSAGE** | 2:32 | The complete four-line chorus the record has withheld — *run real fast / i wanna hide away / i wanna dash / dot dot dash* — and the harmony stops wandering and comes home to Bm·D·G·Em. | the message, intact |
| **VIII · RECOGNITION** | 3:12 | Elements peel off, one per bar. | |
| **IX · THE HUMANS** | 3:28 | Drums out. The **original recording**, unprocessed — plus somebody else, months later, *singing* it. | it was three friends and one take all along |
| **X · CARRIER OFF** | 3:44 | The drone leaves, the beeps outlast it, and the last sound is a hang-up click **after the music has stopped**. | the channel closed; the signal is still out there |

Everything below serves that table.

## 1. Pitch wiggle at the crossover

> *"when the dashes cross over we could definitely pitch wiggle with them"*

`dashStack()` puts three real people on one syllable 28 ms apart, and act VI
stacks two held dashes on top of each other. **Those are the crossovers, and
only there does the wiggle turn on** — it is deliberately not a global
vibrato. Each performer gets their own vibrato rate and phase (4.3 / 5.9 /
3.4 Hz, mutually irrational-ish so the beating never settles into a pattern
the ear can predict) plus a slow detune drift in the opposite direction from
their neighbour.

Two details make it work rather than just wobble:

- **the depth ramps in** over the first third of each note, so the consonant
  lands in tune and only the held vowel shimmers. Intelligibility survives
  the effect;
- **the depth grows with the story** — 4 cents when the three of them first
  meet in act II, 10 through the message, 16 once it has been answered, 22
  where it spreads, back to 9 for the last statements. The narrative is a
  unison coming apart, so the wiggle is that happening.

It is implemented in the sample player: `pos += step · 2^(cents/1200)` per
sample, which is a real varispeed read, not a post-hoc modulation.

## 2. The tubular pump

> *"and pumping them with more tubular vibes"*

The held dashes get **their own bus**, and it is the only thing on it:

1. **a tube.** Four feedback combs tuned to B2 · B3 · F#4 · D5 — the pitches
   the dashes and the Bm harmony actually live on — damped at 2.4 kHz in the
   loop. A tube *is* a delay line with feedback; that is physics, not
   metaphor. Each comb is scaled by `(1-fb)` so it resonates at unity instead
   of screaming, and the bank is mixed **in parallel at 0.26**, so the dry
   dash is still the thing you hear and the ring is what surrounds it.
2. **tube saturation.** `tanh(1.35·(x + 0.11x²))`, then a DC blocker. The x²
   term is what adds the even harmonics that read as *tube* rather than
   *fuzz*; the DC blocker stops an asymmetric shaper parking an offset on the
   bus and eating headroom. Drive 1.35 is colour, not distortion.
3. **the pump.** Depth **0.72**, keyed off **kick AND snare** — against the
   bed's 0.50 — with a 9 ms ramp and a 0.26 s smoothstep recovery. A quiet
   rim on 3 exists mostly so the pump has a second trigger, which is what
   makes it read as rhythm rather than as the kick's shadow.

Hollow and ringing. No presence or air boost anywhere in the render.

## 3. A thick electro kick

> *"want a thicker kick / POW / like a nice electro synth keek"*

v4's kick was deliberately soft (*"reads as a pulse rather than a punch"*).
v5 goes the other way, in five parts:

| part | what |
| ---- | ---- |
| sweep | 200 Hz → 47 Hz at 62/s, so the drop happens inside 20 ms and reads as one event rather than a bloop |
| envelope | two-stage — a 34/s slam over a 7/s tail — which is what makes a kick sound big and short at once |
| saturation | `tanh` at drive 2.4 **on the kick's own waveform**, folding in the harmonics that let a sine kick cut on a phone. This is the only tanh in the render and it is nowhere near the master |
| transient | two decaying blips at 1.6 k and 3.9 kHz, ~4 ms — there is now an attack to hear |
| sub | a separate 44 Hz layer with a slow decay, for the weight underneath |

The POW is per hit. There is still no drop, no riser, no crash, no
snare-roll build anywhere in the record.

## 4. Beeps, bops, clicks and taps

> *"and more phone beeps and bops / and clicks and taps"*

A fifth bus, ducked at half depth so it sits inside the groove rather than
on top of it. A beep reads as a **phone** when it is two tones and not one,
so these are **real DTMF pairs off the keypad** — and the record dials
**C-U-L-T**, which is 2-8-5-8. Plus UI bops that drop a fifth in 60 ms,
0.9 ms clicks and short woody taps.

Placed narratively, never as a wall:

- **act I** dials;
- **acts II–III** tick under the voices;
- **act IV goes completely silent** — the transmission stopping is the
  loudest thing in the record;
- **act V** beeps SOS back;
- **act VI** becomes a 3-against-4 switchboard that never lines up with
  the bar;
- **act X** has the last sound on the record.

## 5. Skids and slides — Menu Band's TrackDrum

> *"menu band's trackdrum sliding sound could be GREAT to add — some
> skidding and sliding in the perc"*

A direct port of the **"Continuous membrane friction"** block in
`slab/menuband/Sources/MenuBand/MenuBandPercussion.swift` (~1296–1338), not
an approximation. The parts that matter, kept exactly:

- two low-pass states over the **same** white sample (at `cut`, and at
  `max(35, cut·0.18)`), with the friction signal being their **difference** —
  a tight moving band, not broadband noise. That band *is* the skid;
- a sine carrier at `res`, frequency-modulated **by the band itself**
  (`1 + tanh(band·8)·0.055`). That 5.5 % self-modulation is the whole reason
  it sounds like a finger dragging rather than like filtered noise;
- nonlinear grip: `gnarl = tanh(band·(5 + rough·5))`, then
  `gnarl·0.44 + carrier·(0.08 + |gnarl|·(0.42 + rough·0.30))`;
- a level **target** through separate attack/release one-poles (0.0025 s
  physical, 0.006 s synthetic), so it swells and releases like a real drag
  instead of gating on. The gesture lives in that envelope.

v5 adds a sweep on `cut` and `res` across each gesture, because a hand that
is sliding is also moving and a static filter reads as texture rather than
motion. Both stay warm — cutoff 700–2600 Hz, carrier 90–420 Hz — and the
side send is held at ≤0.34 so friction never smears the mono fold-down.

Three gestures, 49 of them in the record: **drag** swells into a downbeat,
**skid** comes off the back of a hit, **slide** is a slow swell-and-fall
under a transition. Act IV is the showcase: with the beeps silent, a hand on
a drumhead is the only thing still moving — the analog counterweight to a
dead signal layer, and the sound of three people still in the room.

## Other people's cults

> *"get other samples from the other tiktoks / we have many 'cult' tiktok
> takes"*

True, and more so than expected. `system/public/whistlegraph.org/posts.json`
tags **twelve** posts with the `cult` work, not one. The other eleven were
pulled from the assets mirror, transcribed with whisper.cpp, burst-segmented
and chopped into `alt/samples/` — **81 excerpts, including ten fresh
utterances of the word "cult"** (and, checked honestly with `librosa.pyin`:
**no whistles at all** in any of the eleven — these posts are chanted,
spoken and sung, not whistled). `alt/harvest.json` is the receipt.

Six of the ten are used in **act VI**, one per bar, thrown to the edges —
the same word said by different people across 2022, which is literally what
"it spreads" means. The sung one (`alt-71448-cult`, 0.80 s, 298 Hz) and a
sung "the three of us are in a" go to **act IX**, and act X's last word is
somebody else's.

## The score receipt

`out/cult-remix-v5.events.json` is no longer just metadata. Every scored
hit is pushed into a flat `events[]` array **from inside the voice that
makes it**, so a video renderer can draw the exact same performance the ear
hears — **3158 events**, each with `t`, `voice`, `bus`, `dur`, `gain`, `pan`
and, where it applies, `midi`/`midis`/`hz`, `sample`, `wiggleCents`, DTMF
`digit`, skid `shape`/`cut`/`res`, and **`who`** (camille / alex / jeffrey)
on every performer-specific vocal hit — because three people on one pitch
28 ms apart is this track's signature and it should be visible, not only
audible. The receipt also carries the ten acts with absolute start/end
seconds and the narrative line for each.

---

## What the source gave us

The 8-second clip says exactly one sentence:

> Dash, dash, dash, dot, dot, dot. The three of us are in a cult.

Six chant hits, and an RMS/autocorrelation probe puts them in three
distinct registers — **one dash and one dot from each performer**:

| voice   | measured | note |
| ------- | -------- | ---- |
| Jeffrey | ~127 Hz  | B2   |
| Alex    | ~193 Hz  | F#3  |
| Camille | ~245 Hz  | C#4  |
| "cult"  | ~247 Hz  | B3   |

B / F# / C# is a B minor power chord with the ninth on top, and the chant
sits at **120 BPM** — which is why v2 runs at 120 and not v1's 128. At the
source's own tempo nothing has to be dragged to fit.

TikTok blocks this IP for that video, so `slice.mjs` pulls the mp4 from the
AC asset mirror that `whistlegraph.org`'s own `posts.json` already points
at. Same file, no substitution.

## v2 — the four notes

@jeffrey heard v1 and asked for four things. Each one is a section below.

### 1. "we should be extending their words — use our whole speech-to-singing pipeline"

The headline ask, and the centre of v2. **Every lead line in the track is a
sung note, not a chopped speech hit.**

`bin/sing.py` is the full **Saitou (WASPAA 2007)** recipe on the **WORLD**
vocoder (`pyworld`) — a thicker sibling of `pop/bin/pitchsnap_world.py`,
which replaces an f0 contour but does not lengthen phonemes. Saitou's
point is that speech is missing three things: pitched melody, sustained
vowels, and a singer's-formant ring. `sing.py` does all three:

```
harvest / stonemask → f0
cheaptrick          → spectral envelope   (= vowel identity, = who it is)
d4c                 → aperiodicity        (= breath, consonants)
  ↓ DURATION CONTROL — warp the frame axis so the VOWEL absorbs the hold
    and the consonants stay short. A spoken "dash" becomes "daaaash",
    not a dragged "d-a-a-a-sh". Weights are voiced × loud × 70 ms past
    the vowel onset, so nothing stretches before the vowel has started.
  ↓ F0 SUBSTITUTION — write the score's contour, don't shift the source's
    (shifting is why v1's pitched hits still had spoken prosody on top)
  ↓ SAITOU'S FOUR SUB-EFFECTS — 42¢ attack overshoot decaying over 200 ms,
    a 22¢ preparation dip before rising intervals, 5.4 Hz / 32¢ vibrato
    faded in after 260 ms on notes longer than 0.45 s, and ±7¢ of
    lowpassed fine fluctuation
  ↓ SINGER'S FORMANT — +3.2 dB gaussian at 2.8 kHz, in the envelope
    (Sundberg 1974). Deliberately modest: @jeffrey does not want presence.
synthesize
  ↓ re-impose the voiced/unvoiced structure in the time domain and
    composite the WARPED ORIGINAL back over the unvoiced regions, so
    "s"/"t"/"sh" stay real instead of being coloured by tone
  ↓ de-ess (5–9 kHz, sidechained), 14 ms attack / 110 ms release
```

Everything `pop/SPEECH-TO-SINGING-V2.md` flags is applied: f0 is continuous
through unvoiced gaps before synthesis (§2 — the word-start stutter),
`cheaptrick`'s `f0_floor` is set per source (§2 — held-note ring; 55 Hz for
Jeffrey's B2 dash, 110 Hz for Camille), unvoiced regions come from the
source (§6 — harsh sibilants), and verification is **`librosa.pyin`**, not
autocorrelation (§1 — octave errors).

One extra trick that isn't in either doc: a long hold on a frozen
`cheaptrick` envelope rings like a sample loop, so the read head gets a
±2.2-frame **shimmer at 0.85 Hz** inside stretched regions. That restores
the formant drift a real held vowel has.

`bin/sing.mjs` drives it — 33 renders, cached by an args hash. Every one
verified back with pyin:

| word | source | sung to | stretch | pyin |
| ---- | ------ | ------- | ------- | ---- |
| dash (Camille) | 0.31 s | F#4 / D4, **1.50 s** | 4.8× | +8¢ |
| dash (Alex) | 0.34 s | F#4 / D4, **1.50 s** | 4.5× | +8¢ |
| dash (Jeffrey) | 0.49 s | F#4 / D4, **1.50 s** | 3.0× | +8¢ |
| dash (Jeffrey, bass) | 0.49 s | E2/G2/A2/B2, **2.00 s** | 4.0× | −2¢ |
| **cult** | 0.54 s | 11 pitches B2→G4, **4.00 s** | **7.5×** | +8¢ |
| dot | 0.30 s | B3/D4/F#3/A3, 0.22 s | 0.7× | +8¢ |
| "the three of us are in a" | 2.17 s | a 6-note melody, 4.00 s | 1.9× | −12…+28¢ |
| "i wanna" | 0.40 s | D4→E4 / B3→C#4 | 1.3× | +8…+28¢ |
| "run it fast" | 0.59 s | G4→F#4→**D4 held 1.20 s** | 3.4× | −12…+18¢ |

Median absolute error **8 cents**, worst **28**. The residual is mostly the
vibrato and overshoot, which are supposed to be there.

**The choir is one word.** Camille's sung "cult" (B3), rendered at eleven
pitches and picked per chord — a whole section's pad out of a single
syllable. **The hook's dashes are one syllable sung by three people**:
Camille, Alex and Jeffrey's dashes are each rendered to the *same* pitch
and stacked 28 ms apart, so the unison is real performers, not a detune.

### 2. "the orchestration is weird" · 3. "can we like rethink the whole track melody"

Full rewrite. 120 BPM, 120 bars, 4:00. **No drops, no snare-roll builds,
no risers, no crashes** — v1 had all four. Chill techno is subtraction and
patience: the arrangement moves by adding or removing one element at a time
over a kick that mostly just stays.

| bars | time | section | what changes |
| ---- | ---- | ------- | ------------ |
| 0–8 | 0:00 | **air** | sung "cult" drone alone. no drums. |
| 8–16 | 0:16 | **pulse** | kick fades in, offbeat sine-bump bass, sparse hats |
| 16–24 | 0:32 | **morse** | the SOS figure, twice |
| 24–40 | 0:48 | **hookA** | the hook ×4, alternating full and reduced |
| 40–48 | 1:20 | **hollow** | kick out. "the three of us are in a" sung — rising, then answered falling |
| 48–64 | 1:36 | **hookB** | hook ×4, SOS dots answering in the hole the hook leaves |
| 64–76 | 2:08 | **drift** | no hook at all. harmonic travel, held sung dashes as pads, dub stabs, ride |
| 76–96 | 2:32 | **hookC** | fullest: hook ×5, 16th hats, clap, open hats |
| 96–104 | 3:12 | **descent** | elements peel off |
| 104–112 | 3:28 | **ebb** | drums out, the original spoken hook and chant return |
| 112–120 | 3:44 | **out** | the drone walks out the way it walked in |

Measured RMS: −19 dB in `air`, −10.6 through the hooks, −15.5 in `hollow`,
−22 in `out`. **LRA 6.8 LU** — the shape is in the arrangement, not the
limiter. The three hook sections measure the *same* level on purpose;
what changes between them is texture, not volume, because that is the
difference between hypnotic and drop-bait.

**The melody.** The hook is four bars now instead of two, because a held
word needs the room:

```
0.00  dash ─────────── F#4, 1.50 s   (three performers, one pitch)
1.50  i wanna           D4 → E4
2.00  dash ─────────── D4,  1.50 s
3.50  i wanna           B3 → C#4
4.00  run it fast       G4 → F#4 → D4 (D4 held 1.20 s)
6.00  dot               B3
6.50  dot               F#3
7.00  ─── rest ───                    (the chill needs the hole)
```

**The SOS figure.** The caption is morse, so it's a real four-bar riff:
three short sung notes (B3 F#3 D4), three long ones (D4 F#4 E4, 1.2 s
each), three short (D4 A3 B3). It earns its place because long-versus-short
is the contrast a chill techno lead lives on anyway — and because the words
being held *are* the dashes and the words being clipped *are* the dots.

### 4. "lets use some of our chord progression experiments"

The harmony is lifted from the repo's own research, not invented:
**`PROGRESSIONS_CHILL`** in `recap/bin/trance.mjs`, the eight-row pool
that `pop/dance/cutezenwaltzi.md` describes as the chill mode walking "an
8-progression pool ... so it loops far less and travels more
harmonically". Four of its rows, transposed to B minor, two bars a chord:

| bars | row | degrees | in B minor |
| ---- | --- | ------- | ---------- |
| 0–7 | `[0,5,2,6]` | i VI III VII | Bm G D A |
| 8–15 | `[0,6,3,5]` | i VII iv VI | Bm A Em G |
| 16–23 | `[0,4,5,3]` | i v VI iv | Bm F#m G Em |
| 24–31 | `[0,2,5,3]` | i III VI iv | Bm D G Em |

A 32-bar cycle — a full minute at 120 — which is long enough that the ear
reads travel rather than loop. `pop/dance/STUDY.md`'s rule that a dance
track wants a *simple* looped minor progression is respected inside each
row; the pool is what keeps four simple rows from becoming one boring one.

## Mixing

Follows `pop/teknull/c/taksmukkeklokken-smooch.c`, same as v1:

- **No clicks.** Every voice exits through a 10 ms raised-cosine tail. The
  sidechain ramps to its floor over ~9 ms and recovers over 310 ms — and
  it is shallower than v1's (0.50 vs 0.66) because chill wants a breath,
  not a pump.
- **No clipping.** No master `tanh`. Clean sum, linear trim to 0.92,
  loudness set only in mastering.
- **Sine bumps, not twang.** Bass is fundamental + sub octave + a whisper
  of the 2nd through a one-pole, with a 75 ms glide. Pads and stabs are
  detuned harmonized sines. Nothing plucked, no presence or air boost
  anywhere — including inside `sing.py`, where the singer's formant is
  held to +3.2 dB.
- **Special Sign space.** Dry equal-power pans keep it mono-safe; a cheap
  interaural model feeds a side bus band-limited 80 Hz–11.5 kHz and
  returned antisymmetrically (L=+, R=−) with a slewed send that breathes
  with the arrangement. Mono fold-down costs **0.12 dB** across the track
  and never more than **0.82 dB** in any 2-second window.
- **Dub delay** — new for v2, and the one genuinely new mix element: a
  dotted-eighth (0.375 s) ping-pong with a 2.6 kHz damp *in the loop* and
  a 180 Hz highpass on the return, so the tails never muddy the sub. This
  is chill techno's actual reverb.
- **Four buses.** Drums never duck, the harmonic bed takes the full
  sidechain, the sung voices take a quarter-depth duck (`d^0.25`) **and
  ride +3 dB**. That last number was measured, not guessed: `--stems`
  showed the lead sitting 6 dB *under* music + drums through the hooks,
  which is exactly the failure mode v1 hit. It now sits ~3 dB proud.

Mastering is **measure → static gain → true-peak limiter**, unchanged from
v1 and for v1's reason: at this target `loudnorm` silently abandons
`linear=true` and starts riding gain, which lifts the quiet intro ~20 dB
and manufactures sample-to-sample steps the renderer was careful not to
make (`qc.mjs` caught it at 8.79 s on v1).

## Samples

- **`samples/`** — cut by `bin/slice.mjs` from the whistlegraph itself and
  from the two ElevenLabs lines. Unchanged from v1.
- **`sung/`** — built by `bin/sing.mjs` from `samples/`. Derived; the
  `.manifest.json` (tracked) records what each word was sung to and what
  pyin measured back.
- **`vox/`** — the two ElevenLabs jeffrey-pvc renders, **reused from v1**,
  not regenerated. `/api/say` caches by content hash and v2 needed no new
  words, so this cut cost zero API calls.
- **`pop/demos/samples/perc-*.mp3`** — top-end percussion. The kick and all
  bass are synthesized here so the low end stays under control.
- v1's `whats-inside-your-heart` stabs are **dropped** in v2. They were
  texture in a busy club mix; in a quiet one they read as the jokes
  @jeffrey warned they'd be.

## Run it

```bash
node pop/cult/bin/slice.mjs        # rebuild the sample bank (v1's, unchanged)
node pop/cult/bin/sing.mjs         # speech-to-singing bank (cached)
bash pop/cult/bin/render-v2.sh     # score + master → out/cult-remix-v2.*
node pop/cult/bin/qc.mjs pop/cult/out/cult-remix-v2.mp3
node pop/cult/bin/render2.mjs --stems   # bus stems, for balance checks
```

`bin/sing.py` needs `pop/.venv`:

```bash
python3 -m venv pop/.venv
pop/.venv/bin/pip install numpy soundfile pyworld librosa "setuptools<81"
```

(`setuptools<81` is not optional — `pyworld` still imports `pkg_resources`,
which setuptools 81+ removed.)

`bash pop/cult/bin/render.sh` still builds v1 and does not touch v2.

## Measured — v2

| check | value |
| ----- | ----- |
| duration | 243.2 s (4:03 incl. tail) |
| integrated loudness | **−14.1 LUFS** |
| true peak | **−2.0 dBFS** |
| loudness range | 6.8 LU |
| max sample-to-sample step | 0.247 (0 steps > 0.35) |
| max 2 kHz-band step | 0.122 |
| full-scale runs ≥ 3 samples | 0 |
| mono fold-down | −0.12 dB overall, −0.82 dB worst window |
| pre-master max step | 0.196 (0 > 0.35) |
| sung-note pitch error (pyin) | median 8¢, max 28¢ |

---

## Run it — v5

```bash
node pop/cult/bin/slice.mjs             # the sample bank (v1's, unchanged)
node pop/cult/bin/sing.mjs              # speech-to-singing bank (cached; 93 renders)
node pop/cult/bin/render5.mjs --stems   # score → out/cult-remix-v5-full.wav + bus stems
bash pop/cult/bin/cut-v5.sh             # master once → full + 0:50 + 2:40 mp3s
node pop/cult/bin/qc.mjs pop/cult/out/cult-remix-v5.mp3
```

`cut-v5.sh` measures the render **once**, applies **one static dB of gain**,
and limits. There is never a second `loudnorm` pass: at loud targets ffmpeg's
loudnorm silently abandons `linear=true` and starts riding gain, which
manufactures sample-step artifacts that the click scan then finds.

Different windows: `START=80 DUR=40 SUFFIX=-secret bash pop/cult/bin/cut-v5.sh`.

`bin/render.mjs` (v1), `bin/render2.mjs` (v2), `bin/render3.mjs` (v3) and
`bin/render4.mjs` (v4) all still build their own outputs and are untouched.


---

## Measured — v5

| check | full | 0:50 cut | 2:40 cut |
| ----- | ---- | -------- | -------- |
| duration | 243.2 s | 50.0 s | 160.0 s |
| integrated loudness | **−14.0 LUFS** | −12.3 | −13.2 |
| true peak | **−1.7 dBFS** | −1.7 | −1.7 |
| loudness range | 8.2 LU | 3.6 | 4.3 |
| max raw sample step | 0.365 | 0.249 | 0.249 |
| max 2 kHz-band step | **0.117** | 0.106 | 0.106 |
| full-scale runs ≥ 3 samples | **0** | 0 | 0 |
| mono fold-down, whole track | **−0.13 dB** | −0.21 | −0.12 |
| mono fold-down, worst window | −0.79 dB | −0.64 | −0.64 |

The raw-step figure is higher than v4's 0.186 for one reason: **v5 has
clicks in it on purpose**. The 2 kHz-band step — which is what actually
distinguishes a discontinuity from a fast transient, since a real signal in
that band cannot move more than ~0.26 per sample — is **0.117 against v4's
0.125**, i.e. slightly cleaner. No discontinuities were introduced.

Bus balance, measured from `--stems` (this is the check every earlier
version failed at least once):

| bus | integrated |
| --- | ---------- |
| vox (sung words, dots, choir, grains) | −13.5 LUFS |
| tube (the held dashes) | −13.8 |
| drums (kick, hats, perc, skids) | −15.9 |
| music (bed, pad, stabs, bass, delay) | −17.5 |
| signal (beeps, bops, clicks, taps) | −25.4 |

The lead sits ~4 dB proud of the bed, which is the whole point of the
measurement.
