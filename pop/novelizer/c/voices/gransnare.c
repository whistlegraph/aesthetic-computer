// gransnare.c — grain-cloud snare: stochastic pulsaret bursts.
//
// TECHNIQUE
//   Every hit is a CLOUD of 50..300 pulsarets — short sine bursts under an
//   expodec envelope (fast cosine attack, exponential decay, cosine tail) —
//   scattered stochastically-uniform in time by a truncated-exponential
//   density envelope. Three cloud statistics follow fast envelopes across
//   the hit:
//     density   : dense at onset (the crack), thinning toward the tail
//     center    : formant center glides log-space from a velocity-scaled
//                 ceiling (2.2..9 kHz) down to the drum body frequency
//     spread    : formant scatter is wide at onset (±~1.6 octaves) and
//                 collapses to nearly zero in the tail
//   Tail grains lock their sine phase to a global body oscillator
//   (phase = frac(fw * onset)), so as the cloud narrows the overlapping
//   pulsarets fuse into a coherent pitched hum at the note frequency —
//   the snare "body" is a statistical condensation, not a resonator.
//
// CITATION / LINEAGE
//   - Curtis Roads, "Microsound" (MIT Press, 2001), ch. 3 ("Granular
//     Synthesis": clouds as statistical objects — density / bandwidth /
//     amplitude envelopes over grain populations) and ch. 4 ("Varieties of
//     Particle Synthesis": pulsarets, expodec envelopes).
//   - Declared batch-1 lineage: the pulsaret particle + expodec window are
//     shared with pulsar.c; there they form a PERIODIC train (pitch = train
//     rate), here they form an ASYNCHRONOUS cloud (pitch = statistical
//     convergence of the formant distribution).
//
// WHAT MAKES IT NOVEL HERE
//   The snare is a STATISTICAL object. Velocity does not just scale level:
//   it morphs the cloud's probability distribution — grain count (50..300),
//   formant ceiling, and hit length — so soft hits are papery sparse taps
//   and hard hits are a wall of micro-zaps. No filtered noise anywhere
//   (vs nullnoise / percussion.mjs / gm_synth: every sample here is a
//   windowed sinusoid); grains are scheduled stochastically-uniform, not
//   by a chaotic contact process (vs wiresnare) and not periodically
//   (vs pulsar). hellsine's kick is a single distorted sine — no cloud.
//
// PARAMETER MAP (per hit; all randomness = per-note hashed xorshift32)
//   fbody       note freq octave-folded up to >= 120 Hz (drum body pitch)
//   hitdur      0.16 + 0.24*vel seconds (envelopes own the decay; note dur
//               is ignored, grains are self-windowed so retrigger is safe)
//   N grains    50 + 250*vel^1.6
//   onset burst 6 + 30*vel grains forced into the first 2 ms (the crack)
//   onsets      truncated-exponential, tau = 0.25*hitdur
//   brightness  g(t) = exp(-t / (0.030 + 0.020*vel))
//   center      fc = fbody * (fmax/fbody)^g,  fmax = 2200 + 6800*vel^1.2
//   spread      ± (1.6*g + 0.06) octaves, triangular
//   body prob   (1 - g)^1.2 — tail grains lock to fbody (30% at 1.58*fbody,
//               the second membrane-ish partial), phase-coherent
//   grain len   cloud: 6..10 cycles of fw, clamped 1.5..8 ms (2..6 ms in
//               the crack band); body: 3..4.5 cycles, clamped 4..30 ms
//   grain amp   exp(-onset / (0.35*hitdur)) * jitter(0.75..1.25); body x1.4
//   hit amp     vel^1.35
//
// Self-contained C99, libm only, deterministic (no libc rand).
//   cc -O2 -std=c99 -Wall -o build/gransnare voices/gransnare.c -lm

#include "../novelizer.h"

/* ── per-note deterministic rng ───────────────────────────────────── */
static inline uint32_t gs_hash(uint32_t x) {
  x ^= x >> 16; x *= 0x7feb352du;
  x ^= x >> 15; x *= 0x846ca68bu;
  x ^= x >> 16; return x;
}
typedef struct { uint32_t s; } GsRng;
static inline uint32_t gs_next(GsRng *r) {
  uint32_t x = r->s;
  x ^= x << 13; x ^= x >> 17; x ^= x << 5;
  r->s = x; return x;
}
static inline double gs_frand(GsRng *r) { /* uniform 0..1 */
  return (double)(gs_next(r) >> 8) * (1.0 / 16777216.0);
}
static inline double gs_tri(GsRng *r) { /* triangular -1..1 */
  return gs_frand(r) + gs_frand(r) - 1.0;
}

/* ── pulsaret expodec window (batch-1 pulsar lineage, declared) ───── */
static inline double gs_win(double x) {
  const double att = 0.12, tail = 0.10, k = 5.0;
  if (x <= 0.0 || x >= 1.0) return 0.0;
  double a = (x < att) ? 0.5 - 0.5 * cos(M_PI * x / att)
                       : exp(-k * (x - att) / (1.0 - att));
  if (x > 1.0 - tail) a *= 0.5 + 0.5 * cos(M_PI * (x - (1.0 - tail)) / tail);
  return a;
}

static void render_hit(const NvNote *n, int idx, float *out, int nframes) {
  const double vel = n->vel;
  double fbody = n->freq;
  while (fbody < 120.0) fbody *= 2.0;

  GsRng rng;
  rng.s = gs_hash((uint32_t)idx * 0x9E3779B9u ^
                  (uint32_t)(n->start * NV_SR) ^
                  gs_hash((uint32_t)(n->freq * 100.0)));
  if (!rng.s) rng.s = 0xACu;

  const double hitdur = 0.16 + 0.24 * vel;
  const int ngrains = 50 + (int)(250.0 * pow(vel, 1.6));
  const int nburst = 6 + (int)(30.0 * vel);
  const double tau = 0.25 * hitdur;            /* onset density e-fold  */
  const double bright_tau = 0.030 + 0.020 * vel;
  const double fmax = 2200.0 + 6800.0 * pow(vel, 1.2);
  const double amp_tau = 0.35 * hitdur;
  const double hit_amp = 0.40 * pow(vel, 1.35);
  const double trunc = 1.0 - exp(-hitdur / tau); /* exp-draw truncation */
  const int s0 = (int)(n->start * NV_SR);

  for (int g = 0; g < ngrains; g++) {
    /* onset: forced crack burst, then truncated-exponential scatter */
    double onset = (g < nburst)
                       ? gs_frand(&rng) * 0.002
                       : -tau * log(1.0 - gs_frand(&rng) * trunc);

    double br = exp(-onset / bright_tau); /* cloud brightness statistic */
    double fw, phase, cycles, amp;
    int is_body = gs_frand(&rng) < pow(1.0 - br, 1.2);

    if (is_body) { /* tail condensation: phase-locked body pulsaret */
      fw = fbody * (gs_frand(&rng) < 0.30 ? 1.58 : 1.0);
      fw *= 1.0 + 0.004 * gs_tri(&rng);
      phase = fw * onset;
      phase -= floor(phase); /* coherent with a global body oscillator */
      cycles = 3.0 + 1.5 * gs_frand(&rng);
      amp = 1.4;
    } else {       /* crack: scattered micro-zap */
      double fc = fbody * pow(fmax / fbody, br);
      double oct = gs_tri(&rng) * (1.6 * br + 0.06);
      fw = fc * pow(2.0, oct);
      if (fw < fbody * 0.8) fw = fbody * 0.8;
      if (fw > 16000.0) fw = 16000.0;
      phase = gs_frand(&rng);
      cycles = 6.0 + 4.0 * gs_frand(&rng);
      amp = 1.0;
    }

    double glen = cycles / fw; /* seconds */
    double lo = is_body ? 0.004 : 0.0015;
    double hi = is_body ? 0.030 : 0.008;
    if (glen < lo) glen = lo;
    if (glen > hi) glen = hi;

    amp *= exp(-onset / amp_tau) * (0.75 + 0.5 * gs_frand(&rng)) * hit_amp;

    int gs0 = s0 + (int)(onset * NV_SR);
    int glen_s = (int)(glen * NV_SR);
    if (glen_s < 8) glen_s = 8;
    double inc = fw / NV_SR, ph = phase;
    for (int i = 0; i < glen_s && gs0 + i < nframes; i++) {
      double x = (double)(i + 1) / (double)(glen_s + 1);
      out[gs0 + i] += (float)(gs_win(x) * sin(NV_TAU * ph) * amp);
      ph += inc;
      if (ph >= 1.0) ph -= 1.0;
    }
  }
}

static void render(const NvMelody *m, float *out, int nframes) {
  for (int j = 0; j < m->count; j++) {
    const NvNote *n = &m->notes[j];
    if (n->freq <= 0) continue;
    render_hit(n, j, out, nframes);
  }
}

int main(int argc, char **argv) { return nv_main(argc, argv, "gransnare", render); }
