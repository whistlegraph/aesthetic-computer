// cavikick.c — Helmholtz-resonator / bass-reflex-port kick drum.
//
// TECHNIQUE
//   The drum "shell" is modeled as a lumped Helmholtz resonator: the slug
//   of air in a port (mass) oscillates against the compliance of the air
//   in the cavity (spring). A beater hit is an impulsive pressure step on
//   the cavity, so per note we integrate
//
//       v' = -w0^2 x - ( d_lin + d_nl * |v|/w ) v + F(t)
//       x' = v
//
//   with symplectic Euler at 48 kHz (frequency-prewarped so the discrete
//   rotation lands exactly on the note pitch). The radiated sound is the
//   port volume velocity, y = v / w. The |v| * v term is the classic
//   NONLINEAR ORIFICE RESISTANCE: jet separation at the port makes losses
//   grow with air speed, so loud hits decay faster and rounder than soft
//   ones — an amplitude-dependent envelope no ADSR gives you for free.
//   Port turbulence is audible as a "chuff": deterministic noise, band-
//   limited to ~250 Hz..1.25 kHz, gated by u^2 (u = normalized port
//   velocity), so the breath pulses at 2*f with the flow peaks and dies
//   twice as fast as the tone. One fast-decaying inharmonic mode at
//   6.27 * f stands in for the first standing-wave mode of the cavity
//   (where the lumped approximation ends) and supplies the woody knock.
//   The cavity is tuned so the Helmholtz frequency IS the note frequency;
//   damping-induced flatting is compensated in w0.
//
// CITATION / LINEAGE
//   - H. von Helmholtz, "On the Sensations of Tone" (1863) — cavity
//     resonators: air mass in a neck against cavity compliance.
//   - U. Ingard & H. Ising, "Acoustic Nonlinearity of an Orifice",
//     JASA 42(1), 1967 — orifice resistance grows ~ |u| (velocity-squared
//     pressure loss), the d_nl * |v| damping term here.
//   - A. N. Thiele (1971) / R. H. Small (1972), JAES — the bass-reflex
//     (vented-box) lumped model: port output = Helmholtz mode of the box.
//   - Fletcher & Rossing, "The Physics of Musical Instruments", ch. on
//     ocarinas/jug resonators — blown/struck Helmholtz oscillators.
//
// WHAT MAKES IT NOVEL HERE
//   Every kick already in the AC stable synthesizes the MEMBRANE side:
//   hellsine's distorted pitch-swept sine, percussion.mjs's 808/909
//   envelopes, nullnoise's carved noise bursts, gm_synth's GM drums.
//   cavikick synthesizes the AIR side — the box, not the head: a struck
//   Helmholtz cavity whose pitch is the port resonance, whose dynamics
//   come from physical velocity-squared port loss (loud = shorter and
//   rounder), and whose attack breath is flow-gated turbulence rather
//   than an enveloped noise sample. No pitch sweep, no waveshaper, no
//   membrane: hollow, woody, breathy — air punched out of a box. No
//   batch-1 lineage (scanner/pulsar/chaosfm/frictus/vosim/twomass all
//   untouched).
//
// PARAMETER MAP
//   note freq  -> Helmholtz frequency f_H (cavity tuning), prewarped +
//                 damping-compensated so the ring is on pitch
//   note vel   -> pressure step P = vel^1.35, beater fall time
//                 6ms -> 3.5ms (harder beater), chuff level ~ vel^2
//                 (via u^2 gate), deeper nonlinear-loss rounding
//   d_lin      T60 ~ 300 ms small-signal decay (always < 400 ms)
//   d_nl       ~2.3x extra damping at full swing (Ingard-Ising loss)
//   chuff      noise * u^2 * exp(-t/70ms), BP 250..1250 Hz, gain 0.55
//   knock      2nd resonator at 6.27*f, T60 55 ms, gain 0.13 (cavity
//                 standing-wave mode; skipped if > 18 kHz)
//   per-note   FNV hash -> +/-0.3% cavity detune, chuff seed, beater jitter
//   dur        ignored: the resonator's own losses are the envelope
//              (capped at 0.85 s, 10 ms safety fade, never past nframes)

#include "../novelizer.h"

#define NOTE_MAX_SEC 0.85
#define FADE_SEC 0.010
#define D_LIN (2.0 * 6.9078 / 0.30) /* T60 = 300 ms small-signal */
#define D_NL 105.0                  /* extra damping per unit |u| */
#define KNOCK_RATIO 6.27
#define KNOCK_T60 0.055
#define KNOCK_GAIN 0.13
#define CHUFF_GAIN 0.55
#define CHUFF_TAU 0.070
#define CHUFF_LP_HZ 1250.0
#define CHUFF_HP_HZ 250.0

/* FNV-1a note hash -> deterministic per-hit variation */
static uint32_t note_hash(int idx, double freq, double start) {
  uint32_t h = 2166136261u;
  uint32_t a = (uint32_t)idx, b = (uint32_t)(freq * 64.0), c = (uint32_t)(start * 1000.0);
  h = (h ^ a) * 16777619u;
  h = (h ^ b) * 16777619u;
  h = (h ^ c) * 16777619u;
  return h;
}

static uint32_t xs32(uint32_t *s) { /* xorshift32, deterministic noise */
  uint32_t x = *s;
  x ^= x << 13; x ^= x >> 17; x ^= x << 5;
  return *s = x;
}

/* symplectic-Euler prewarp: discrete rotation of exactly w rad/s */
static double prewarp(double w, double dt) {
  double a = 0.5 * w * dt;
  if (a > 1.5) a = 1.5; /* clamp near Nyquist */
  return 2.0 * sin(a) / dt;
}

static void render_note(const NvNote *n, int idx, float *out, int nframes) {
  const double dt = 1.0 / NV_SR;
  int s0 = (int)(n->start * NV_SR);
  if (s0 >= nframes) return;
  int len = (int)(NOTE_MAX_SEC * NV_SR);
  if (s0 + len > nframes) len = nframes - s0;
  int fade = (int)(FADE_SEC * NV_SR);

  uint32_t h = note_hash(idx, n->freq, n->start);
  uint32_t rng = h | 1u;
  double detune = 1.0 + (((h >> 8) & 0xffff) / 65535.0 - 0.5) * 0.006;
  double jit = (((h >> 20) & 0xff) / 255.0 - 0.5) * 0.10; /* +/-5% beater */

  /* cavity tuning: target angular freq, damping-compensated + prewarped */
  double wt = NV_TAU * n->freq * detune;
  double w0 = prewarp(sqrt(wt * wt + 0.25 * D_LIN * D_LIN), dt);
  double w02 = w0 * w0;

  /* beater pressure step: fast rise, velocity-shortened fall */
  double tau_r = 0.0007;
  double tau_f = (0.006 - 0.0025 * n->vel) * (1.0 + jit);
  double P = pow(n->vel, 1.35) * wt * wt; /* scale so peak |x| ~ vel^1.35 */

  /* knock mode: first cavity standing wave beyond the lumped model */
  double fk = n->freq * KNOCK_RATIO;
  int knock_on = fk < 18000.0;
  double wk = knock_on ? prewarp(NV_TAU * fk, dt) : 0.0;
  double wk2 = wk * wk;
  double dk = 2.0 * 6.9078 / KNOCK_T60;

  /* chuff filters: one-pole LP x2 + one-pole HP */
  double klp = 1.0 - exp(-NV_TAU * CHUFF_LP_HZ * dt);
  double khp = 1.0 - exp(-NV_TAU * CHUFF_HP_HZ * dt);
  double lp1 = 0, lp2 = 0, hpt = 0;

  double x = 0, v = 0;   /* port air mass: displacement, velocity */
  double xk = 0, vk = 0; /* knock mode */

  for (int i = 0; i < len; i++) {
    double t = i * dt;

    /* beater force */
    double F = P * (1.0 - exp(-t / tau_r)) * exp(-t / tau_f);

    /* Helmholtz mode with Ingard-Ising nonlinear port loss */
    double u = v / w0; /* normalized port velocity (~ swing amplitude) */
    double au = fabs(u);
    double d = D_LIN + D_NL * (au > 1.4 ? 1.4 : au);
    v *= exp(-d * dt);
    v += dt * (-w02 * x + F);
    x += dt * v;

    double y = v / w0; /* radiated port volume velocity */

    /* knock */
    if (knock_on) {
      vk *= exp(-dk * dt);
      vk += dt * (-wk2 * xk + F);
      xk += dt * vk;
      y += KNOCK_GAIN * (vk / wk);
    }

    /* port turbulence chuff: noise gated by u^2, band-limited */
    double w = ((double)(xs32(&rng) >> 8) / 8388608.0) - 1.0; /* -1..1 */
    lp1 += klp * (w - lp1);
    lp2 += klp * (lp1 - lp2);
    hpt += khp * (lp2 - hpt);
    double band = lp2 - hpt;
    double gate = u * u;
    if (gate > 1.5) gate = 1.5;
    y += CHUFF_GAIN * band * gate * exp(-t / CHUFF_TAU);

    /* safety fade at the hard cap (tail is ~ -80 dB there anyway) */
    if (i >= len - fade) y *= (double)(len - i) / fade;

    out[s0 + i] += (float)(0.9 * y);
  }
}

static void render(const NvMelody *m, float *out, int nframes) {
  for (int j = 0; j < m->count; j++) {
    const NvNote *n = &m->notes[j];
    if (n->freq <= 0) continue;
    render_note(n, j, out, nframes);
  }
}

int main(int argc, char **argv) { return nv_main(argc, argv, "cavikick", render); }
