// memkick.c — nonlinear circular-membrane modal kick (tension-modulation glide).
//
// TECHNIQUE
//   The drumhead is a bank of circular-membrane modes at the ideal-membrane
//   Bessel-root frequency ratios 1 : 1.594 : 2.136 : 2.296 : 2.653 : 2.918
//   (modes 01, 11, 21, 02, 31, 12 of J_m roots — Fletcher & Rossing,
//   "The Physics of Musical Instruments", ch. 18). Each mode is an
//   exponentially decaying oscillator struck through a short raised-cosine
//   contact pulse (1.5–3 ms; harder hits = stiffer, shorter contact).
//
//   THE KEY NONLINEARITY: membrane tension is not constant. A displaced
//   head is stretched, so instantaneous tension rises with total
//   displacement energy and every modal frequency scales with sqrt(T):
//
//       f_k(t) = r_k · f0 · sqrt(1 + Γ · E(t)),
//       E(t)   = vel² · Σ_k (w_k · s(t) · e^(-t/τ_k))² / Σ_k w_k²
//
//   so a hard hit starts SHARP (≈2.6× f0 at vel=1) and glides down to the
//   note fundamental as the head relaxes — the kick's pitch-drop comes from
//   membrane physics (tension modulation in the timpani/drumhead literature,
//   cf. Bilbao, "Numerical Sound Synthesis", ch. 11–12), not a scripted
//   sine sweep. Velocity → strike force → loudness AND glide depth AND
//   brightness (mode-weight tilt) AND decay tightness (nonlinear losses).
//   A few ms of hash-noise "mallet contact" is bandpassed near the upper
//   mode region for the attack tick.
//
// CITATION / LINEAGE
//   - N. H. Fletcher & T. D. Rossing, "The Physics of Musical Instruments",
//     2nd ed., Springer 1998 — ch. 18 circular-membrane mode frequencies +
//     tension-dependent pitch of struck drumheads.
//   - S. Bilbao, "Numerical Sound Synthesis", Wiley 2009 — ch. 11–12,
//     nonlinear tension-modulated membranes/plates and their pitch glide.
//
// WHAT MAKES IT NOVEL HERE
//   hellsine's gabber kick and percussion.mjs's 808 both SCRIPT the pitch
//   drop as an explicit sine sweep; nullnoise carves filtered noise with no
//   modes at all; gm_synth percussion is a GM kit reimplementation. memkick
//   is the only AC percussion whose drop is an emergent property of a
//   physical model: the glide depth, glide speed, and spectrum all co-vary
//   with strike force through one energy variable, and the body is an
//   inharmonic Bessel stack rather than a lone sine. No lineage overlap
//   with novelizer batch 1 (scanner/pulsar/chaosfm/frictus/vosim/twomass).
//
// PARAMETER MAP
//   MODE_RATIO[6]  1, 1.594, 2.136, 2.296, 2.653, 2.918   Bessel ratios
//   mode weights   w_k = r_k^-(2.2 − 1.0·vel)   harder hit = brighter head
//   decay          τ0 = (0.060 − 0.026·vel)·(65.4/f0)^0.25  s (amp const);
//                  τ_k = τ0 / (1 + 0.9·(r_k − 1))  upper modes die fast
//   tension        Γ = 6.0 → vel=1 starts sqrt(7) ≈ 2.65× sharp, settles
//                  to f0 within ~80 ms; vel=0.5 starts only 1.58× sharp
//   contact        raised-cosine strike window, T = 3.2 − 1.6·vel ms
//   click          hash-noise burst, τ 1.2 ms, SVF bandpass at
//                  clamp(22·f0, 900, 3800) Hz, level 0.11·vel^1.6
//   per-note hash  ±0.2% mode detune + click seed (deterministic)
//   amp            0.9 · vel^1.4  (velocity accents)

#include "../novelizer.h"

#define NMODES 6
static const double MODE_RATIO[NMODES] = {
  1.0, 1.594, 2.136, 2.296, 2.653, 2.918
};

#define TENSION_GAMMA 6.0
#define CLICK_TAU 0.0012
#define CLICK_LEN 0.012

/* ── deterministic hashing (no libc rand) ─────────────────────────── */
static inline uint32_t mk_hash32(uint32_t x) {
  x ^= x >> 16; x *= 0x7feb352dU;
  x ^= x >> 15; x *= 0x846ca68bU;
  x ^= x >> 16;
  return x;
}
static inline double mk_hash01(uint32_t x) {
  return mk_hash32(x) / 4294967296.0;
}

static void render_note(const NvNote *n, int idx, float *out, int nframes) {
  int s0 = (int)(n->start * NV_SR);
  if (s0 >= nframes) return;
  double f0 = n->freq;
  double vel = n->vel;
  if (vel < 0.0) vel = 0.0;
  if (vel > 1.0) vel = 1.0;

  uint32_t seed = mk_hash32((uint32_t)idx * 2654435761u ^
                            (uint32_t)(n->start * 48000.0) ^
                            (uint32_t)(f0 * 64.0));

  /* register scaling: smaller (higher-pitched) heads ring shorter */
  double reg = pow(65.406 / f0, 0.25);
  if (reg > 1.6) reg = 1.6;
  if (reg < 0.25) reg = 0.25;

  /* harder hit -> more nonlinear loss -> tighter decay */
  double tau0 = (0.060 - 0.026 * vel) * reg;
  double bright = 2.2 - 1.0 * vel; /* mode-weight rolloff exponent */

  double w[NMODES], tau[NMODES], det[NMODES], ph[NMODES];
  double s0w = 0.0;
  for (int k = 0; k < NMODES; k++) {
    w[k] = pow(MODE_RATIO[k], -bright);
    tau[k] = tau0 / (1.0 + 0.9 * (MODE_RATIO[k] - 1.0));
    det[k] = 1.0 + 0.004 * (mk_hash01(seed + 7u * (uint32_t)k) - 0.5);
    ph[k] = 0.0;
    s0w += w[k] * w[k];
  }

  double tstrike = 0.0032 - 0.0016 * vel; /* raised-cosine contact time */
  double amp = 0.9 * pow(vel, 1.4);

  /* mallet-contact click: hash noise through an SVF bandpass */
  double fc = 22.0 * f0;
  if (fc < 900.0) fc = 900.0;
  if (fc > 3800.0) fc = 3800.0;
  double f1 = 2.0 * sin(M_PI * fc / NV_SR);
  double svf_lp = 0.0, svf_bp = 0.0;
  double click_amp = 0.11 * pow(vel, 1.6);
  uint32_t rng = seed ^ 0x9e3779b9u;

  /* render to -60 dB of the slowest mode (+ margin), never past nframes */
  int len = (int)(6.9 * tau0 * NV_SR) + (int)(0.06 * NV_SR);
  if (len > nframes - s0) len = nframes - s0;

  for (int i = 0; i < len; i++) {
    double t = (double)i / NV_SR;
    double s = (t < tstrike) ? 0.5 * (1.0 - cos(M_PI * t / tstrike)) : 1.0;

    /* modal envelopes + total displacement energy (drives tension) */
    double ek[NMODES], E = 0.0;
    for (int k = 0; k < NMODES; k++) {
      ek[k] = w[k] * s * exp(-t / tau[k]);
      E += ek[k] * ek[k];
    }
    E *= vel * vel / s0w;
    double g = sqrt(1.0 + TENSION_GAMMA * E); /* f_k scales with sqrt(T) */

    double y = 0.0;
    for (int k = 0; k < NMODES; k++) {
      y += ek[k] * sin(NV_TAU * ph[k]);
      ph[k] += MODE_RATIO[k] * det[k] * f0 * g / NV_SR;
      if (ph[k] >= 1.0) ph[k] -= 1.0;
    }

    if (t < CLICK_LEN) {
      double ce = (t < 0.0003) ? t / 0.0003
                               : exp(-(t - 0.0003) / CLICK_TAU);
      rng = rng * 1664525u + 1013904223u;
      double nz = ((double)(rng >> 9) / 4194304.0) - 1.0;
      svf_lp += f1 * svf_bp;
      double hp = nz - svf_lp - 0.8 * svf_bp;
      svf_bp += f1 * hp;
      y += click_amp * ce * svf_bp;
    }

    out[s0 + i] += (float)(amp * y);
  }
}

static void render(const NvMelody *m, float *out, int nframes) {
  for (int j = 0; j < m->count; j++) {
    const NvNote *n = &m->notes[j];
    if (n->freq <= 0) continue;
    render_note(n, j, out, nframes);
  }
}

int main(int argc, char **argv) { return nv_main(argc, argv, "memkick", render); }
