// pulsar.c — Pulsar synthesis voice for pop/novelizer.
//
// TECHNIQUE
//   Pulsar synthesis after Curtis Roads, "Microsound" (MIT Press, 2001),
//   ch. 4 ("Varieties of Particle Synthesis"), itself descended from the
//   Vosim/formant-pulse lineage (Kaegi & Tempelaars 1978) and Roads's own
//   PulsarGenerator (with Alberto de Campo, CREATE/UCSB, 2001).
//
//   A note is a TRAIN of pulsarets: short particles, each a dual-partial
//   sinusoid burst under an "expodec"-style envelope (fast cosine attack,
//   exponential decay, cosine tail), emitted at the fundamental rate fp.
//   The pulsaret's internal frequency (the FORMANT, fw) and its duty
//   cycle d/p are INDEPENDENT of fp — the train's spectrum is the
//   harmonic series of fp weighted by the pulsaret spectrum, i.e. a
//   movable resonance ridge over a perfectly periodic tone.
//
//   Per Roads, long tones use PULSAR MASKING: whole pulsarets are
//   deleted from the train — here a burst pattern (b on : r off, r slowly
//   varying) plus stochastic deletion, both fading in only after the
//   note has sustained — giving rhythmic/granular shimmer without ever
//   clicking (every particle carries its own envelope).
//
// PARAMETER MAP (all per note)
//   freq  -> pulsar train rate fp (the perceived pitch; phase 0..1
//            advanced by freq/NV_SR, house style)
//   vel   -> loudness (vel^1.4), formant brightness (onset formant
//            750..2850 Hz scales with vel), and duty (higher vel =
//            narrower pulsaret = buzzier attack)
//   time  -> formant f1 glides exponentially from bright onset down to a
//            settled vowel-like target (380..640 Hz); f2 = 2.6*f1 rides
//            along; duty widens from ~0.2 to ~0.7 of the period.
//            Long notes (>2.5 s) add slow sinusoidal formant/duty
//            wander and masking for internal evolution.
//
// WHAT MAKES IT NOVEL HERE
//   Nothing in the AC stable is particle-based. skrill's talking bass
//   sweeps a filter/FM index over a continuous oscillator; pulsar gets
//   its vowel from the geometry of enveloped micro-events, so the
//   formant ridge is sharper, duty-cycle morphing adds a second
//   independent spectral axis, and masking produces granular rhythm
//   INSIDE a held pitch — unavailable to sine bells, supersaw, hoover,
//   zitar, nullnoise, or the gm_synth waveguides.
//
// Self-contained C99, libm only.  cc -O2 -std=c99 -Wall -o build/pulsar
// voices/pulsar.c -lm

#include "../novelizer.h"

/* ── deterministic rng (xorshift32) ───────────────────────────────── */
static uint32_t nv_rng = 0x9E3779B9u;
static inline uint32_t xr32(void) {
  nv_rng ^= nv_rng << 13; nv_rng ^= nv_rng >> 17; nv_rng ^= nv_rng << 5;
  return nv_rng;
}
static inline double frand(void) { /* uniform 0..1 */
  return (double)(xr32() & 0xFFFFFFu) / (double)0x1000000u;
}

/* ── pulsaret envelope: expodec with smooth ends ──────────────────── */
static inline double pulsaret_win(double x) {
  const double att = 0.12, tail = 0.10, k = 4.0;
  if (x <= 0.0 || x >= 1.0) return 0.0;
  double a = (x < att) ? 0.5 - 0.5 * cos(M_PI * x / att)
                       : exp(-k * (x - att) / (1.0 - att));
  if (x > 1.0 - tail) a *= 0.5 + 0.5 * cos(M_PI * (x - (1.0 - tail)) / tail);
  return a;
}

/* ── per-note amplitude envelope (attack / gentle decay / release) ── */
#define NOTE_REL 0.10
static inline double note_env(double t, double dur) {
  const double att = 0.006;
  if (t < 0.0) return 0.0;
  double a = (t < att) ? 0.5 - 0.5 * cos(M_PI * t / att) : 1.0;
  double dec = 0.78 + 0.22 * exp(-2.5 * fmax(0.0, t - att) / fmax(dur, 0.05));
  double r = 1.0;
  if (t > dur) {
    double x = (t - dur) / NOTE_REL;
    if (x >= 1.0) return 0.0;
    r = 0.5 + 0.5 * cos(M_PI * x);
  }
  return a * dec * r;
}

static void render_note(const NvNote *n, float *out, int nframes) {
  const double fp = n->freq, vel = n->vel, dur = n->dur;
  const double amp = 0.35 * pow(vel, 1.4);
  const double period = (double)NV_SR / fp; /* samples per pulsar cycle */
  const int s0 = (int)(n->start * NV_SR);
  const int len = (int)((dur + NOTE_REL + 0.01) * NV_SR);

  /* formant + duty trajectories (independent of fp, per Roads) */
  const double glide_tau = fmax(0.06, dur * 0.35);
  const double f1_onset = 750.0 + 2100.0 * vel;  /* bright attack ridge  */
  const double f1_settle = 380.0 + 260.0 * vel;  /* vowel-ish rest state */
  const double duty_a = 0.38 - 0.18 * vel;       /* narrow = buzzy       */
  const double duty_b = 0.70 - 0.10 * vel;       /* wide = rounder       */
  const int longnote = dur > 2.5;
  const double wobble_ph = frand(); /* decorrelate drone LFOs per note */

  /* pulsar-train state: pulse phase 0..1 advanced by fp/NV_SR */
  double pp = 1.0;              /* forces a trigger at sample 0 */
  int p_on = 0, p_idx = 0;      /* active flag, pulse counter   */
  double p_t = 0, p_len = 1;    /* samples into / total of pulsaret */
  double ph1 = 0, ph2 = 0;      /* pulsaret partial phases 0..1 */
  double inc1 = 0, inc2 = 0, p_amp = 1.0;

  for (int i = 0; i < len && s0 + i < nframes; i++) {
    double t = (double)i / NV_SR;
    double env = note_env(t, dur);

    pp += fp / NV_SR;
    if (pp >= 1.0) {
      pp -= 1.0;
      p_idx++;
      /* latch this pulsaret's particle parameters at trigger time */
      double g = exp(-t / glide_tau);
      double f1 = f1_settle + (f1_onset - f1_settle) * g;
      double duty = duty_b + (duty_a - duty_b) * g;
      if (longnote) { /* slow internal evolution on drones */
        f1 *= 1.0 + 0.22 * sin(NV_TAU * (0.11 * t + wobble_ph));
        duty += 0.12 * sin(NV_TAU * (0.07 * t + 0.5 * wobble_ph));
      }
      if (f1 < fp * 1.25) f1 = fp * 1.25;
      if (f1 > 6000.0) f1 = 6000.0;
      double f2 = fmin(2.6 * f1, 12000.0);
      if (duty < 0.12) duty = 0.12;
      if (duty > 0.95) duty = 0.95;
      p_len = duty * period;
      /* keep >= 1.25 formant cycles inside the particle */
      double min_len = 1.25 * NV_SR / f1;
      if (p_len < min_len) p_len = fmin(min_len, 0.95 * period);
      /* pulsar masking: burst pattern + stochastic, fades in on the
         sustain of LONG notes only — melodic notes stay fully periodic */
      int masked = 0;
      double mdepth =
          (longnote && t > 0.55) ? fmin(1.0, (t - 0.55) / 0.9) : 0.0;
      if (mdepth > 0.0) {
        int rest = 1 + (int)(1.45 + 1.45 * sin(NV_TAU * 0.05 * t));
        int cycle = 5 + rest;
        if (p_idx % cycle >= 5) masked = 1;
        if (!masked && frand() < 0.30 * mdepth) masked = 1;
        p_amp = 1.0 - 0.10 * mdepth * frand();
      } else {
        p_amp = 1.0;
      }
      p_on = !masked;
      p_t = 0; ph1 = 0; ph2 = 0;
      inc1 = f1 / NV_SR; inc2 = f2 / NV_SR;
    }

    double s = 0.0;
    if (p_on) {
      double x = p_t / p_len;
      if (x < 1.0) {
        s = pulsaret_win(x) *
            (sin(NV_TAU * ph1) + 0.6 * sin(NV_TAU * ph2)) * p_amp;
        ph1 += inc1; if (ph1 >= 1.0) ph1 -= 1.0;
        ph2 += inc2; if (ph2 >= 1.0) ph2 -= 1.0;
        p_t += 1.0;
      } else {
        p_on = 0;
      }
    }
    out[s0 + i] += (float)(s * env * amp);
  }
}

static void render(const NvMelody *m, float *out, int nframes) {
  nv_rng = 0x9E3779B9u; /* deterministic per melody */
  for (int j = 0; j < m->count; j++) {
    const NvNote *n = &m->notes[j];
    if (n->freq <= 0) continue;
    render_note(n, out, nframes);
  }
}

int main(int argc, char **argv) { return nv_main(argc, argv, "pulsar", render); }
