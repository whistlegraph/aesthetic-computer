# GM Synthesis Dossier 03 — Reed, Pipe, Synth Lead, Synth Pad

Real-time C synthesis algorithms for 32 General MIDI programs (65–96), targeting
the Aesthetic Computer native audio engine (`fedac/native/src/audio.c`,
`audio.h`). 32 voices, per-sample loop, up to 192 kHz, mono-per-voice with
post pan.

This dossier covers four GM families:

- **Reed** (65–72): Soprano/Alto/Tenor/Baritone Sax, Oboe, English Horn, Bassoon, Clarinet
- **Pipe** (73–80): Piccolo, Flute, Recorder, Pan Flute, Blown Bottle, Shakuhachi, Whistle, Ocarina
- **Synth Lead** (81–88): Square, Sawtooth, Calliope, Chiff, Charang, Voice, Fifths, Bass+Lead
- **Synth Pad** (89–96): New Age, Warm, Polysynth, Choir, Bowed, Metallic, Halo, Sweep

---

## 0. The engine substrate — what we build on

The AC engine already ships a **Perry Cook STK digital-waveguide flute model**
as `WAVE_WHISTLE` (`generate_whistle_sample`, audio.c:177). Its signal flow:

```
 breath ─►(+)─► jetDelay ─► NL(x·(x²−1)) ─► dcBlock ─►(+)─► boreDelay ─┬─► out
           ▲ −jetRefl·temp                            ▲ +endRefl·temp   │
           └──────────── 1-pole loop LPF ◄────────────┴─────────────────┘
```

Key existing state (per `ACVoice`, audio.h:88–104):

- `whistle_bore_buf[2048]` + `whistle_bore_w` — **bore delay line** (`= SR/freq`),
  the primary resonator. Fractional read via `whistle_frac_read()` (audio.c:140).
- `whistle_jet_buf[512]` + `whistle_jet_w` — **jet delay** (`0.32 × bore`), models
  air-jet travel across the embouchure.
- `whistle_lp1` — 1-pole loop LPF (bore losses → tone darkens).
- `whistle_hp_x1/y1` — DC blocker after the nonlinearity.
- `whistle_breath`, `whistle_vibrato_phase` — breath envelope + ~5 Hz vibrato.
- `noise_seed` — xorshift32 white noise for breath turbulence.

This **flute waveguide is the direct basis for the entire Pipe family** (retune
the jet ratio + breath noise) and, with the cubic nonlinearity swapped for a
**reed reflection table**, becomes the **entire Reed family**. Basic oscillators
(`WAVE_SINE/TRIANGLE/SAWTOOTH/SQUARE/NOISE`, audio.c:1469–1496) plus a biquad
LPF (`setup_noise_filter`, audio.c:1528) are the substrate for the **subtractive
Synth Lead and Synth Pad** families.

Repo convention: **phase-increment oscillators** (`phase += freq/SR`), not
`sin(TAU·f·t)`; one-pole/biquad filters; table or polyBLEP for band-limiting.

### The reed table — the one new primitive the Reed family needs

The flute uses a **cubic** nonlinearity `x·(x²−1)` (a limit-cycle generator that
turns DC breath into oscillation). A *reed* instrument instead uses a
**pressure-controlled reflection coefficient** — the McIntyre–Schumacher–
Woodhouse reed model, implemented in STK as a saturating affine map:

```
STK ReedTable:  reflection = offset + slope · pressureDiff
                offset = 0.6   slope = −0.8   (STK defaults)
                clamp reflection to [−1, +1]
```

The reed nearly closes as mouth-pressure rises (negative slope), then *slams
shut* at the clamp — that hard saturation is what gives reeds their buzzy,
harmonic-rich tone versus the flute's pure jet whistle. Source: STK
`ReedTable.h`; J.O. Smith *PASP* "Single-Reed Theory." The **clarinet's
cylindrical closed bore reflects with inversion → only odd harmonics survive**;
the **conical sax/oboe/bassoon bores reflect without inversion → full harmonic
series**. This single bore-reflection sign is the deepest physical distinction
in the whole Reed family.

A single shared `generate_reed_sample(v, sr, ReedParams *rp)` with a
`ReedParams` struct (bore ratio, conical flag, reed stiffness/offset/slope,
breath-noise gain, vibrato) covers all 8 reeds. Likewise one
`generate_pipe_sample()` covers all 8 pipes via a `PipeParams` struct. The two
synth families share `generate_subtractive_voice()`.

---

## Family A — Reed (GM 65–72)

**Method:** digital waveguide bore + STK reed reflection table nonlinearity.
Extend the existing whistle waveguide: keep `whistle_bore_buf`/`whistle_jet_buf`,
**replace the cubic `pd·(pd²−1)` with the reed table** and add a bore-reflection
sign that flips for cylindrical (clarinet) vs conical (sax/oboe/bassoon) bores.

**Core references (whole family):**

- M.E. McIntyre, R.T. Schumacher, J. Woodhouse (1983), "On the Oscillations of
  Musical Instruments," *J. Acoust. Soc. Am.* **74**(5):1325–1345. The foundational
  time-domain reed/bowed/jet oscillator model. <https://pubs.aip.org/asa/jasa/article/150/2/R3/615501/A-landmark-article-on-nonlinear-time-domain>
- J.O. Smith, *Physical Audio Signal Processing*, CCRMA Stanford (online):
  "Single-Reed Theory," "Clarinet" chapters. <https://ccrma.stanford.edu/~jos/pasp/Single_Reed_Theory.html>
- P.R. Cook & G.P. Scavone, *The Synthesis ToolKit in C++* (STK): `Clarinet`,
  `Saxofony`, `BlowHole` classes. <https://ccrma.stanford.edu/software/stk/classstk_1_1Clarinet.html>, <https://ccrma.stanford.edu/software/stk/classstk_1_1Saxofony.html>
- G.P. Scavone & P.R. Cook (1998), tonehole/register-hole BlowHole model.
- N.H. Fletcher & T.D. Rossing, *The Physics of Musical Instruments*, 2nd ed.,
  Springer 1998 (Ch. 13–15, reed instruments; conical vs cylindrical bores).
- V. Välimäki et al., woodwind digital-waveguide synthesis papers (fractional
  delay tuning, toneholes).
- Gordon Reid, "Synth Secrets — Synthesizing Wind Instruments," *Sound on Sound*.
  Closed pipe (clarinet) = square (odd harmonics); conical bore = sawtooth (full
  series). <https://www.soundonsound.com/techniques/synthesizing-wind-instruments>

**Shared reed-waveguide per-sample sketch:**

```c
typedef struct {
    double bore_ratio;     // jet/embouchure unused for reeds; bore = SR/freq
    int    conical;        // 1 = sax/oboe/bassoon (full harmonics),
                           // 0 = clarinet (odd-only, inverting reflection)
    double reed_offset;    // STK 0.6 nominal; lower = softer reed
    double reed_slope;     // STK −0.8 nominal; steeper = brighter/buzzier
    double loop_damp;      // 1-pole LPF coeff (bore loss; bigger bore = darker)
    double noise_gain;     // breath turbulence (sax > oboe > clarinet)
    double vib_rate, vib_depth;
} ReedParams;

static inline double generate_reed_sample(ACVoice *v, double sr, const ReedParams *rp) {
    double env = compute_envelope(v);
    // Breath pressure: DC drives the reed into self-oscillation.
    double pTarget = 0.55 + 0.45 * env;                 // mouth pressure Pm
    v->whistle_breath += (pTarget - v->whistle_breath) * 0.01;
    // vibrato + turbulent breath noise
    v->whistle_vibrato_phase += rp->vib_rate / sr;
    if (v->whistle_vibrato_phase >= 1.0) v->whistle_vibrato_phase -= 1.0;
    double vib = sin(2*M_PI*v->whistle_vibrato_phase) * rp->vib_depth;
    double white = ((double)xorshift32(&v->noise_seed)/UINT32_MAX)*2-1;
    double Pm = v->whistle_breath * (1.0 + rp->noise_gain*white + vib);

    double freq = clampd(v->frequency, 30.0, sr*0.20);
    double bore_delay = sr / freq;
    const int BORE_N = 2048;
    if (bore_delay > BORE_N-2) bore_delay = BORE_N-2;

    // Bore round-trip: read, damp (1-pole loop LPF = bore losses)
    double bore_out = whistle_frac_read(v->whistle_bore_buf, BORE_N,
                                        v->whistle_bore_w, bore_delay);
    v->whistle_lp1 = (1-rp->loop_damp)*bore_out + rp->loop_damp*v->whistle_lp1;
    // CLARINET: cylinder reflects with INVERSION → odd harmonics only.
    // SAX/OBOE/BASSOON: conical bore reflects WITHOUT inversion → full series.
    double refl = rp->conical ? v->whistle_lp1 : -v->whistle_lp1;

    // Pressure difference across the reed, then the STK reed table.
    double pDiff = Pm - refl;                            // Pd = Pm − Pb
    double reedRefl = rp->reed_offset + rp->reed_slope * pDiff; // affine
    if (reedRefl > 1.0) reedRefl = 1.0;                 // reed slams shut
    if (reedRefl < -1.0) reedRefl = -1.0;
    // Reflected pressure wave entering the bore (McIntyre et al.):
    double into_bore = refl + reedRefl * pDiff;

    // DC block + write back into the bore loop.
    double y = into_bore - v->whistle_hp_x1 + 0.995*v->whistle_hp_y1;
    v->whistle_hp_x1 = into_bore; v->whistle_hp_y1 = y;
    v->whistle_bore_buf[v->whistle_bore_w] = (float)y;
    v->whistle_bore_w = (v->whistle_bore_w + 1) % BORE_N;
    return 0.3 * y * env;
}
```

**Param-vs-variant table** (the timbre knobs that distinguish all 8):

| GM | Instrument | bore (octave) | conical | reed_slope | noise_gain | notes |
|----|-----------|---------------|---------|-----------|-----------|-------|
| 65 | Soprano Sax | short (high) | 1 | −0.85 | 0.10 | bright conical, B♭ soprano |
| 66 | Alto Sax    | medium | 1 | −0.80 | 0.12 | E♭, classic sax buzz |
| 67 | Tenor Sax   | long | 1 | −0.75 | 0.13 | B♭, warmer/breathier |
| 68 | Baritone Sax| longest | 1 | −0.70 | 0.15 | E♭, dark, big noise floor |
| 69 | Oboe        | short, narrow | 1 | −0.90 | 0.05 | double reed → very stiff (steep slope), thin nasal |
| 70 | English Horn| medium, narrow | 1 | −0.88 | 0.06 | alto oboe, F, rounder than oboe |
| 71 | Bassoon     | long, narrow | 1 | −0.82 | 0.07 | bass double reed, hollow low register |
| 72 | Clarinet    | medium | **0** | −0.80 | 0.04 | **cylindrical → odd harmonics**, woody hollow tone |

### 65–68 Saxophones
Conical bore (full harmonic series), single reed. STK `Saxofony` is the
reference: a "blowed-string" hybrid where excitation position morphs
clarinet↔sax. Bore length is the *only* primary distinguisher across the four
saxes (soprano shortest → baritone longest); set `bore_delay = SR/freq` from the
played pitch and let the longer/larger members get more `noise_gain` (breathier)
and a heavier `loop_damp` (darker). Reed `slope ≈ −0.8`, `offset 0.6`.

### 69–70 Oboe & English Horn
Conical narrow bore + **double reed** → model as a *stiffer* reed (steeper
`reed_slope ≈ −0.9`, smaller aperture) and lighter breath noise. Their thin,
nasal, harmonically dense timbre comes from the narrow cone emphasizing upper
formants — boost a fixed bandpass "singer's formant" ~1.4 kHz (oboe) /
~1.1 kHz (English horn) on the output for the characteristic reedy edge
(Fletcher & Rossing). English horn = oboe scaled down a fifth (F instrument).

### 71 Bassoon
Conical, long, narrow double reed. Same model as oboe with a long bore and a
mild low-pass on output (the bassoon's hollow lower register). A formant peak
around 440–500 Hz gives its woody "buzz."

### 72 Clarinet
**The odd-harmonic case.** Cylindrical closed bore → set `conical = 0` so the
loop reflection **inverts** (`refl = −whistle_lp1`). A closed cylinder resonates
at odd multiples only, producing the clarinet's hollow, square-wave-like spectrum
(Reid; Smith *PASP* Clarinet). This is the most physically distinct reed and
should A/B clearly against the saxes. Subtractive fallback: filtered **square
wave** (odd harmonics) confirms the target spectrum.

---

## Family B — Pipe (GM 73–80)

**Method:** the **existing Cook flute waveguide unchanged in structure** —
retune jet ratio, breath-noise level, and vibrato per instrument. These are
air-jet (no reed) instruments, so the **cubic nonlinearity stays**. This is the
cheapest family: it is literally `generate_whistle_sample` with a small
`PipeParams` struct.

**References:**

- P.R. Cook, STK `Flute` (jet-delay air-reed waveguide). <https://ccrma.stanford.edu/software/stk/>
- J.O. Smith, *PASP*, "Blown Bottles and Pipes" / flute waveguide.
- M.E. McIntyre, R.T. Schumacher, J. Woodhouse (1983), JASA 74:1325 (air-jet
  oscillation, flute family). <https://pubs.aip.org/asa/jasa/article/150/2/R3/615501>
- Fletcher & Rossing, *Physics of Musical Instruments*, Ch. 16–17 (flutes,
  organ flue pipes, jet drive).
- Gordon Reid, "Synthesizing Wind Instruments," *SoS* — open pipe = full
  harmonic series (sawtooth/triangle). <https://www.soundonsound.com/techniques/synthesizing-wind-instruments>

```c
typedef struct {
    double jet_ratio;    // jet delay / bore delay. Cook flute 0.32;
                         // pennywhistle 0.45; ocarina ~0.5 (Helmholtz)
    double noise_gain;   // breath chiff: pan flute/shakuhachi high, whistle low
    double vib_rate, vib_depth;
    double loop_damp;    // brightness: piccolo bright, recorder pure
    int    helmholtz;    // ocarina/bottle: vessel resonator, no overblow
} PipeParams;
```

**Param-vs-variant table:**

| GM | Instrument | jet_ratio | noise_gain | vib | notes |
|----|-----------|-----------|-----------|-----|-------|
| 73 | Piccolo   | 0.30 | 0.06 | 5 Hz/0.03 | flute up an octave; short bore, very bright |
| 74 | Flute     | 0.32 | 0.08 | 5 Hz/0.03 | the canonical Cook model (current default) |
| 75 | Recorder  | 0.32 | 0.04 | minimal | pure, low noise; open pipe, near-pure tone |
| 76 | Pan Flute | 0.40 | **0.22** | 4 Hz/0.05 | strong breathy chiff, prominent onset noise |
| 77 | Blown Bottle | n/a | 0.18 | slow | **Helmholtz** resonator: single-mode, no overblow |
| 78 | Shakuhachi| 0.38 | **0.25** | 6 Hz/0.08 expressive | very breathy, pitch-bend/meri-kari portamento |
| 79 | Whistle   | 0.45 | 0.05 | 6 Hz/0.04 | tin/penny whistle; higher jet ratio, bright |
| 80 | Ocarina   | 0.50 | 0.06 | gentle | **Helmholtz vessel**; pure, slightly hollow |

### 73 Piccolo / 74 Flute / 75 Recorder
Same waveguide; piccolo plays an octave higher (shorter `bore_delay`), recorder
drops vibrato and breath noise to near zero for its pure tone. Reid: recorder is
an open pipe → full harmonic series; subtractive fallback is sawtooth/triangle
through a gentle LPF.

### 76 Pan Flute / 78 Shakuhachi
The **breath-noise instruments**. Raise `noise_gain` to 0.22–0.25 and increase
attack-onset chiff (the engine already scales noise by `onset = 1 − env`,
audio.c:196). Shakuhachi additionally wants expressive vibrato/portamento and a
slightly higher `jet_ratio` for its airy edge — use the existing
`target_frequency` slew (audio.c:1514) for bends.

### 77 Blown Bottle / 80 Ocarina
**Helmholtz vessel resonators**, not pipes — a single resonant mode, no overblown
octave. Cheapest model: skip the bore delay loop, drive a single resonant
**bandpass biquad** (Q ~10) at the played pitch with breath noise + a touch of
the jet nonlinearity for the "edge tone." `helmholtz=1` branch:

```c
// Bottle/ocarina: resonant bandpass excited by breath noise + jet edge tone.
double exc = Pm * (1.0 + noise_gain*white);
double y = bp_b0*exc + bp_b1*x1 + bp_b2*x2 - bp_a1*y1 - bp_a2*y2; // biquad BPF
```

### 79 Whistle
Tin/penny whistle: `jet_ratio = 0.45` (per the existing code comment, audio.c:201),
bright, low breath noise, lively ~6 Hz vibrato. Essentially the current whistle
default tuned a hair brighter.

---

## Family C — Synth Lead (GM 81–88)

**Method:** classic **subtractive synthesis** — named waveform(s) → resonant
low-pass filter → amp+filter envelope, with detune/PWM/LFO per variant. No
physical modeling. Many GM leads are explicitly derived from the **Roland D-50 /
MT-32** factory sounds (Calliope = "Living Calliope," Chiff = "Breathy Chiffer").

**References:**

- Gordon Reid, "Synth Secrets" (63-part series), *Sound on Sound* — subtractive
  synthesis bible; wind/brass/lead patches. <https://www.soundonsound.com/series/synth-secrets-sound-sound>
- *Synth Secrets* on PWM/pulse leads and square-vs-saw harmonic content. <https://yamahasynth.com/learn/synth-programming/synth-basics-all-squares-pulse/>
- FreePats GM Synth Lead set (ZynAddSubFX/Surge recipes). <https://freepats.zenvoid.org/Synthesizer/synth-lead.html>
- GM spec / D-50 & MT-32 lineage. <https://en.wikipedia.org/wiki/General_MIDI>
- Minimoog 24 dB/oct ladder filter, detuned-oscillator lead practice. <https://www.vintagesynth.com/moog/minimoog>

**Shared subtractive voice + state-variable filter:**

```c
typedef struct {
    WaveType osc[3];       // up to 3 oscillators
    double   detune[3];    // cents offset (×2^(c/1200))
    int      n_osc;
    double   pwm_rate, pwm_depth;   // square→pulse PWM (lead 1)
    double   cutoff, res;           // SVF / ladder
    double   env_amount;            // filter envelope depth
    double   fa, fd, fs, fr;        // filter ADSR
    double   lfo_rate, lfo_depth;   // pitch/cutoff vibrato
    int      sub_octave;            // bass+lead: extra −1 oct osc
} LeadParams;
```

For band-limiting, use **polyBLEP** on saw/square (cheap, one branch per
discontinuity) so high leads don't alias at 192 kHz harmonics — the current
naive `2·phase−1` saw (audio.c:1483) is fine at the engine rate but polyBLEP is
worth it for bright leads. A **2-pole state-variable filter** (cheaper than the
existing RBJ biquad recompute, and trivially modulatable per-sample) gives the
resonant sweep:

```c
// Chamberlin SVF, per sample (f = 2·sin(π·cutoff/SR), q = 1/res):
lp += f*bp;  hp = in - lp - q*bp;  bp += f*hp;  // lp = low-pass out
```

| GM | Lead | oscillators | filter / motion | character |
|----|------|------------|-----------------|-----------|
| 81 | Square | 1× square + **PWM** (LFO ~0.3–6 Hz) | static LPF, mild res | hollow odd-harmonic lead, classic chiptune |
| 82 | Sawtooth | 1–2× saw, detuned ~7 cents | LPF + env sweep | bright buzzy "all-harmonics" lead |
| 83 | Calliope | triangle/sine + soft saw, slow attack | gentle LPF | **woodwind/steam-organ**; near-pure + breath; D-50 "Living Calliope" |
| 84 | Chiff | saw/pulse + **noise burst on attack** | LPF, fast env | **breathy chiff transient** then tone; D-50 "Breathy Chiffer" |
| 85 | Charang | 2× saw, hard detune + **drive/distortion** | resonant LPF | aggressive **guitar-like** lead; run through engine `drive_mix` |
| 86 | Voice | saw/pulse + **formant BPF bank** | vowel-shaped | synth-voice "aah" lead, fast attack |
| 87 | Fifths | **two oscillators a perfect fifth apart** (×1.5 freq) | LPF | parallel-5ths power lead (organum) |
| 88 | Bass+Lead | saw + **sub-octave square** | LPF, keytracked | split: fat bass low, lead high; sub_octave=1 |

**Variant recipes:**

- **81 Square:** one square osc; modulate pulse width with a slow LFO (PWM) for
  motion. Square = odd harmonics only → the "hollow" lead. Light filter.
- **82 Sawtooth:** one or two slightly detuned saws (≈7 cents) → fatter; LPF with
  a moderate envelope sweep. The textbook bright analog lead.
- **83 Calliope:** softer, woodwind-like — triangle or filtered saw, slow-ish
  attack, low resonance, gentle vibrato. Add a hint of breath noise (reuse the
  pipe `noise_gain`) for the "steam organ" air.
- **84 Chiff:** the defining feature is a **short filtered noise burst at note
  onset** (the "chiff") layered before the pitched tone settles — gate a
  decaying white-noise→BPF for ~30–50 ms via the envelope, then the saw/pulse
  carries the sustain.
- **85 Charang:** two hard-detuned saws through **soft saturation** (the engine's
  `audio_set_drive_mix`/tanh stage already exists) + resonant LPF → buzzy
  guitar-ish lead.
- **86 Voice:** rich oscillator (saw) shaped by a **3-band formant filter bank**
  (see Choir pad below) on an "aah/ooh" vowel; fast attack, slight vibrato.
- **87 Fifths:** run two copies of the lead oscillator, the second at **1.5×
  frequency** (a perfect fifth), mixed equally → the parallel-fifths
  "organum" lead.
- **88 Bass+Lead:** keyboard-split layering — a sub-octave square/saw for the
  bass register plus the saw lead, low notes weighted to bass. Use
  `sub_octave` to add a −1-octave oscillator.

---

## Family D — Synth Pad (GM 89–96)

**Method:** **detuned multi-oscillator** subtractive + **slow filter sweep** +
**chorus** for width, with per-variant character modules (formant for Choir,
FM/ring-mod for Metallic, filter-motion for Sweep/Halo). Pads are defined by
*slow attack/release envelopes* and *evolving motion*, not transients.

**References:**

- Gordon Reid, "Synth Secrets" — string-machine / ensemble / PWM-pad
  installments; "From Sound on Sound." <https://www.soundonsound.com/series/synth-secrets-sound-sound>
- *Synth Secrets* on PWM for lush string pads (synthesizing PWM when absent).
- *Sound on Sound*, "Formant Synthesis" — vowel formant frequencies + bandpass
  bank for choir pads. <https://www.soundonsound.com/techniques/formant-synthesis>
- Chowning FM (bell/metallic timbres) — inharmonic partials from
  non-integer modulator ratios; ring modulation for metallic spectra.

**Shared pad engine:** `LeadParams` extended with multiple detuned voices + a
**chorus** (3-tap modulated delay; the engine already has a flange/`wobble_buf`
that can be repurposed) and a **slow bipolar LFO** on filter cutoff.

```c
typedef struct {
    int    n_detune;       // 3–7 stacked detuned saw/PWM oscillators
    double spread_cents;    // ±detune for "supersaw" width
    double attack, release; // slow (0.3–2.0 s)
    double cutoff, res, sweep_rate, sweep_depth; // slow filter LFO
    double chorus_depth, chorus_rate;
    int    formant;         // choir pad
    int    ring_mod;        // metallic pad
    double fm_ratio, fm_index; // metallic/halo inharmonic content
} PadParams;
```

| GM | Pad | engine | character |
|----|-----|--------|-----------|
| 89 | New Age | detuned saws + long reverb + slow LPF sweep | shimmery, ethereal, slow swell |
| 90 | Warm | 2–3 detuned saws, **low cutoff**, soft attack | rounded analog warmth, no high edge |
| 91 | Polysynth | bright detuned saws/PWM, medium attack | classic "Jump"-style poly stack |
| 92 | Choir | saw/pulse + **formant BPF bank** ("ooh/aah") + chorus | vocal pad |
| 93 | Bowed | saw + **slow noisy attack + body resonance** | bowed-glass/string swell |
| 94 | Metallic | **FM or ring-mod** inharmonic + slow attack | bell-like shimmering metal |
| 95 | Halo | choir/saw + **bright formant + reverb + slow tremolo** | airy heavenly pad |
| 96 | Sweep | detuned saws + **dramatic resonant LPF sweep LFO** | the signature filter-sweep pad |

**Variant recipes:**

- **89 New Age / 90 Warm / 91 Polysynth:** all are the same detuned-saw stack
  (3–7 oscillators, `spread_cents` ±5–15) into a resonant LPF; they differ only
  in **filter cutoff** (warm = dark/low, polysynth = bright) and attack time.
  PWM on the pulses adds the slow internal motion (Reid's string-machine trick).
- **92 Choir:** harmonically rich oscillator → **3 parallel bandpass filters at
  vowel formants**, e.g. "ooh" F1=300/F2=870/F3=2250 Hz, "aah" F1=660/F2=1700/
  F3=2400 Hz (SoS formant table), + chorus + slow attack. Pitch-independent
  formants are what make it read as a voice.
- **93 Bowed (Bowed Glass):** slow noisy attack (filtered noise ramp) feeding a
  high-Q resonant body, then a steady saw/triangle sustain — the "bowed
  glass/string" swell. A bowed-string waveguide (friction nonlinearity, MSW
  1983) is the deluxe option, but the subtractive swell is adequate.
- **94 Metallic:** **inharmonic** — either 2-operator **FM** with a non-integer
  modulator ratio (e.g. ratio 1.4, moderate index → bell partials, Chowning) or
  **ring-modulate** two oscillators (`out = oscA·oscB`) to inject metallic sum/
  difference tones, with a slow attack and long decay.
- **95 Halo:** a bright, airy choir-ish pad — formant-shaped saws (bright vowel),
  heavy reverb (engine `room_mix`), and a slow tremolo LFO on amplitude for the
  "halo" shimmer.
- **96 Sweep:** the canonical **filter-sweep pad** — detuned saws into a resonant
  LPF whose cutoff is driven by a **slow triangle/sine LFO** (sweep_rate ~0.1–
  0.3 Hz) across a wide range with high resonance, so the harmonic content
  visibly sweeps up and down.

---

## Implementation roadmap for AC

1. **Reed family (8):** add `generate_reed_sample()` + `ReedParams` table — reuse
   `whistle_bore_buf`/`whistle_jet_buf`/`whistle_lp1`/`whistle_hp_*`. Only new
   logic: STK reed table (`offset + slope·Pd`, clamp) replacing the cubic, and
   the `conical` reflection-sign flip (clarinet = invert → odd harmonics).
2. **Pipe family (8):** add `generate_pipe_sample()` + `PipeParams` — structurally
   identical to the current whistle; per-instrument `jet_ratio`, `noise_gain`,
   `vib`, plus a `helmholtz` biquad-resonator branch for bottle/ocarina.
3. **Synth Lead (8) + Synth Pad (16):** add `generate_subtractive_voice()` with a
   Chamberlin SVF + polyBLEP saw/square + N detuned oscillators + slow LFOs +
   chorus (reuse `wobble_buf`); plus a small **formant BPF bank** shared by
   Voice lead, Choir pad, and Halo pad, and an **FM/ring-mod** branch for the
   Metallic pad. Extend `WaveType` enum and the `generate_sample` switch
   (audio.c:1468) accordingly.

All four families fit the existing per-voice/per-sample architecture; the reeds
and pipes need **zero new buffers** (they share the whistle/harp delay lines),
and the synth families add only small filter/LFO state.

---

## Consolidated source list

- McIntyre, Schumacher & Woodhouse (1983), "On the Oscillations of Musical
  Instruments," *JASA* 74(5):1325–1345. <https://pubs.aip.org/asa/jasa/article/150/2/R3/615501/A-landmark-article-on-nonlinear-time-domain>
- J.O. Smith, *Physical Audio Signal Processing*, CCRMA. <https://ccrma.stanford.edu/~jos/pasp/Single_Reed_Theory.html>
- P.R. Cook & G.P. Scavone, STK (Clarinet/Saxofony/BlowHole/Flute/ReedTable).
  <https://ccrma.stanford.edu/software/stk/classstk_1_1Clarinet.html> ·
  <https://ccrma.stanford.edu/software/stk/classstk_1_1Saxofony.html> ·
  <https://ccrma.stanford.edu/software/stk/classstk_1_1BlowHole.html>
- N.H. Fletcher & T.D. Rossing, *The Physics of Musical Instruments*, 2nd ed.,
  Springer 1998.
- Gordon Reid, "Synth Secrets," *Sound on Sound* (esp. "Synthesizing Wind
  Instruments"). <https://www.soundonsound.com/series/synth-secrets-sound-sound> ·
  <https://www.soundonsound.com/techniques/synthesizing-wind-instruments>
- "Formant Synthesis," *Sound on Sound*. <https://www.soundonsound.com/techniques/formant-synthesis>
- FreePats GM Synth Lead. <https://freepats.zenvoid.org/Synthesizer/synth-lead.html>
- General MIDI (D-50/MT-32 lineage), Wikipedia. <https://en.wikipedia.org/wiki/General_MIDI>
- Synthesis ToolKit `ReedTable` (offset 0.6, slope −0.8): thestk/stk source.

*Existing AC engine reference: `fedac/native/src/audio.c` (generate_whistle_sample
:177, generate_sample :1466, setup_noise_filter :1528), `audio.h` (ACVoice
waveguide state :88–104).*
