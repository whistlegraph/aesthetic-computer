// cracklesnare.c — chaotic spike-train crackle snare.
//
// TECHNIQUE
//   The snare's "noise" is not filtered white noise: it is a CHAOTIC SPIKE
//   TRAIN. A logistic map
//
//       x' = r · x · (1 − x)
//
//   is iterated once per spike; the orbit value x sets BOTH the amplitude
//   of the spike and the inter-onset interval to the next one (the
//   dripping-faucet reading of the map: chaotic drip timing). r is
//   enveloped across the hit — full chaos (r → 4) at the attack, relaxing
//   toward the periodic window (r → 3.2, a period-2 orbit) in the tail, so
//   every hit begins as fractal broadband sputter and collapses into an
//   audible buzzy comb as the spike train turns periodic. Each spike
//   excites (a) a very short high bandpass "wire" resonator — the crack —
//   and (b) a 3-mode body resonator seated at the note frequency — the
//   drum. Spike density decays across the hit like rain moving off a tin
//   roof.
//
// CITATION / LINEAGE
//   - R. M. May, "Simple mathematical models with very complicated
//     dynamics", Nature 261 (1976) — the logistic map, its bifurcation
//     cascade and periodic windows (r ≈ 3.2 period-2 vs r → 4 chaos).
//   - R. Shaw, "The Dripping Faucet as a Model Chaotic System" (Aerial
//     Press, 1984) — chaotic inter-onset intervals of discrete events,
//     the model for spike TIMING here.
//   - Curtis Roads, Microsound (MIT Press, 2001), ch. on noise particles /
//     trainlets — sound built from irregular particle emission rather than
//     from a continuous noise source; SuperCollider's Crackle UGen is the
//     same family (chaotic-map noise, Voss crackle lineage).
//
// WHAT MAKES IT NOVEL HERE
//   hellsine's kick is a distorted sine, nullnoise carves FILTERED
//   pink/white/brown/velvet bursts, percussion.mjs and gm_synth are
//   conventional 808/909/GM noise-plus-tone snares. None of them emit
//   discrete events from a chaotic orbit: cracklesnare's noise floor is a
//   countable spike train whose statistics are fractal, so the tail
//   sputters and rattles (fireworks / rain-on-tin) instead of hissing, and
//   the r-envelope makes each hit literally traverse the bifurcation
//   diagram — chaos at the crack, period-2 buzz at the tail. Lineage link
//   to batch-1 chaosfm is declared: both play a nonlinear map, but chaosfm
//   bends a continuous FM operator while cracklesnare quantizes chaos into
//   particle timing (Roads' microsound axis, untouched in batch 1).
//
// PARAMETER MAP
//   rate0        1400 + 5200·vel   spike rate at the attack (spikes/s)
//   rate env     ×(0.10 + 0.90·e^(−t/55ms))   density thins into the tail
//   r ceiling    3.62 + 0.38·vel   velocity buys chaos depth (≤ 4.0)
//   r floor      3.20              period-2 window = tail buzz comb
//   r env        e^(−t/80ms) blend from ceiling down to floor
//   spike amp    sign(x−xprev) · (0.15 + 0.85·x) · e^(−t/Tc)
//   Tc           90 + 70·vel ms    crackle amplitude decay
//   crack res    2-pole BP, fc = (2.6k + 2.6k·vel)·(±15% hashed), τ 0.7 ms
//   body res     modes f0·{1.0, 1.593, 2.258}, τ {200, 110, 60} ms,
//                amps {1.0, 0.62, 0.40} — membrane-ish inharmonic stack
//   strike       one full-strength impulse at t=0 (clean transient)
//   seeding      splitmix64 of (note index, start, freq); no libc rand
//   note dur     ignored — the envelope owns the decay (one-shot drum)

#include "../novelizer.h"

#define NOTE_SEC 1.0     /* render window per hit; tails die well before */
#define FADE_SEC 0.02    /* safety cosine fade at the window edge        */
#define R_FLOOR 3.2      /* periodic window: tail buzz                   */
#define T_RENV 0.080     /* r ceiling → floor time constant              */
#define T_RATE 0.055     /* spike-density decay time constant            */
#define BODY_MODES 3

/* ── deterministic hashing (splitmix64) ───────────────────────────── */
static uint64_t sm64(uint64_t z) {
  z += 0x9e3779b97f4a7c15ULL;
  z = (z ^ (z >> 30)) * 0xbf58476d1ce4e5b9ULL;
  z = (z ^ (z >> 27)) * 0x94d049bb133111ebULL;
  return z ^ (z >> 31);
}
static double h01(uint64_t h) { /* uniform in [0,1) */
  return (double)(h >> 11) * (1.0 / 9007199254740992.0);
}

/* ── 2-pole resonator (impulse-excited ringing mode) ──────────────── */
typedef struct {
  double a1, a2, y1, y2, g;
} Res2;

static void res_set(Res2 *r, double f, double tau) {
  double fmax = 0.45 * NV_SR;
  if (f > fmax) f = fmax;
  double th = NV_TAU * f / NV_SR;
  double R = exp(-1.0 / (tau * NV_SR));
  r->a1 = 2.0 * R * cos(th);
  r->a2 = -R * R;
  r->y1 = r->y2 = 0.0;
  double s = sin(th); /* peak of the ring is ~1/sin(th): pre-compensate */
  r->g = (s < 1e-4) ? 1e-4 : s;
}

static double res_tick(Res2 *r, double x) {
  double y = r->a1 * r->y1 + r->a2 * r->y2 + x * r->g;
  r->y2 = r->y1;
  r->y1 = y;
  return y;
}

/* ── one hit ──────────────────────────────────────────────────────── */
static const double body_ratio[BODY_MODES] = { 1.0, 1.593, 2.258 };
static const double body_tau[BODY_MODES] = { 0.200, 0.110, 0.060 };
static const double body_amp[BODY_MODES] = { 1.00, 0.62, 0.40 };

static void render_note(const NvNote *n, int idx, float *out, int nframes) {
  int s0 = (int)(n->start * NV_SR);
  if (s0 >= nframes) return;
  int len = (int)(NOTE_SEC * NV_SR);
  if (s0 + len > nframes) len = nframes - s0;
  int fade = (int)(FADE_SEC * NV_SR);

  /* deterministic per-note seed from index + start + freq bits */
  uint64_t fb, sb;
  memcpy(&fb, &n->freq, sizeof fb);
  memcpy(&sb, &n->start, sizeof sb);
  uint64_t seed = sm64(((uint64_t)idx << 32) ^ fb ^ sm64(sb));

  double vel = n->vel;
  double amp = pow(vel, 1.5) * 0.8;

  /* logistic map state: hashed x0 well inside (0,1) */
  double x = 0.15 + 0.70 * h01(sm64(seed ^ 0xA5A5A5A5ULL));
  double r_hi = 3.62 + 0.38 * vel;
  if (r_hi > 4.0) r_hi = 4.0;
  double rate0 = 1400.0 + 5200.0 * vel;
  double t_crackle = 0.090 + 0.070 * vel;

  /* crack: short bright bandpass, hashed ±15% so hits aren't clones */
  Res2 crack;
  double fc = (2600.0 + 2600.0 * vel) * (0.85 + 0.30 * h01(sm64(seed ^ 0x51EEULL)));
  res_set(&crack, fc, 0.0007);

  /* body: 3 inharmonic modes seated at the note frequency */
  Res2 body[BODY_MODES];
  for (int m = 0; m < BODY_MODES; m++)
    res_set(&body[m], n->freq * body_ratio[m], body_tau[m]);

  double countdown = 0.0; /* fire the first spike at i = 0 */
  uint64_t reseed = seed;

  for (int i = 0; i < len; i++) {
    double t = (double)i / NV_SR;
    double envc = exp(-t / t_crackle);

    double inj = 0.0;
    countdown -= 1.0;
    if (countdown <= 0.0) {
      /* r envelope: chaos at the attack, period-2 window in the tail */
      double r = R_FLOOR + (r_hi - R_FLOOR) * exp(-t / T_RENV);
      double xp = x;
      x = r * x * (1.0 - x);
      if (!(x > 1e-9 && x < 1.0 - 1e-9)) { /* escaped orbit: reinject */
        reseed = sm64(reseed);
        x = 0.20 + 0.60 * h01(reseed);
      }
      double sgn = (x > xp) ? 1.0 : -1.0;
      /* spike density thins across the hit (rain moving off the roof) */
      double rate = rate0 * (0.10 + 0.90 * exp(-t / T_RATE));
      countdown += (NV_SR / rate) * (0.35 + 1.30 * x);
      inj = sgn * (0.15 + 0.85 * x) * envc;
    }
    if (i == 0) inj += 1.2; /* the strike: clean full-strength transient */

    double cr = res_tick(&crack, inj);
    double bd = 0.0;
    for (int m = 0; m < BODY_MODES; m++)
      bd += body_amp[m] * res_tick(&body[m], inj);

    double y = 1.15 * cr + 0.90 * bd + 0.35 * inj;
    if (i >= len - fade) /* safety fade at the render-window edge */
      y *= 0.5 * (1.0 + cos(M_PI * (double)(i - (len - fade)) / fade));

    out[s0 + i] += (float)(y * amp);
  }
}

static void render(const NvMelody *m, float *out, int nframes) {
  for (int j = 0; j < m->count; j++) {
    const NvNote *n = &m->notes[j];
    if (n->freq <= 0) continue;
    render_note(n, j, out, nframes);
  }
}

int main(int argc, char **argv) {
  return nv_main(argc, argv, "cracklesnare", render);
}
