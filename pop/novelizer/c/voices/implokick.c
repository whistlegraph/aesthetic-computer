// implokick.c — chaotic-collapse feedback-FM kick: a bifurcation played
// backwards as a transient.
//
// TECHNIQUE
//   One Tomisawa averaged-feedback FM operator:
//
//       y[n] = sin(2π·phase[n] + β · lp(avg(y[n-1], y[n-2])))
//
//   For a melodic voice (batch-1 chaosfm) β blooms slowly INTO chaos and
//   back. implokick runs the bifurcation the other way and 100× faster:
//   every hit is born deep inside the chaotic regime (β ≈ 2–3, broadband
//   roar) and β COLLAPSES exponentially to zero over the kick's first
//   ~60–150 ms, while the operator frequency falls along a fast exponential
//   from ~2–3.5× the note frequency down to exactly 1×. The result is a
//   controlled implosion — noise → tone → sub in one gesture — with the
//   final sub phase pitch-locked to the note so chromatic kick lines track.
//   The operator is run 2× oversampled (96 kHz internally, averaged-pair
//   decimation) because at β ≈ 3 the chaotic sidebands otherwise fold.
//
// CITATION / LINEAGE
//   - Norio Tomisawa, "Tone production method for an electronic musical
//     instrument", US Patent 4,249,447 (1981) — Yamaha operator
//     self-feedback with the two-sample averaging stabilizer.
//   - Dan Slater, "Chaotic Sound Synthesis", Computer Music Journal 22(2),
//     1998 — feedback FM driven past its bifurcation point as material.
//   - Declared lineage: novelizer batch-1 chaosfm.c (same operator core);
//     implokick inverts its slow bloom into a percussive collapse.
//
// WHAT MAKES IT NOVEL HERE
//   hellsine's kick is a distorted sine sweep; nullnoise carves filtered
//   noise bursts; percussion.mjs and gm_synth do conventional 808/909-ish
//   recipes (sine drop + separate noise click). implokick has NO separate
//   noise source and NO waveshaper: the attack "click", the tonal knock,
//   and the sub tail are one operator crossing its own bifurcation. The
//   transient is deterministic chaos, not filtered randomness, so it stays
//   phase-coherent with the body — a signature none of the existing kit has.
//
// PARAMETER MAP (velocity = strike force)
//   β0        1.7 + 1.3·vel        initial chaos depth (up to 3.0)
//   τβ        18 + 32·vel  ms      β collapse time constant (implosion life)
//   ratio     2.0 + 1.5·vel        initial op-freq multiple of note freq
//   τp        9 + 11·vel   ms      pitch-drop time constant
//   τa        45 + 50·vel  ms      amplitude decay (tail < 400 ms always)
//   amp       vel^1.6              accent law
//   register  β0 ÷ (1 + f/1500)    pitched lines up high bloom shallower
//   hash      ±8% β0, ±5% ratio    deterministic per-hit variation (SplitMix)
//   fb loop   avg(y1,y2) → one-pole (k=0.5 @96k): Tomisawa zero + parasite damp
//   env       0.8 ms linear attack, exp(-t/τa), 20 ms cosine end-fade
//   DC block  y = x − x′ + 0.999·y′ (chaotic regime is asymmetric)

#include "../novelizer.h"

#define KICK_LEN_SEC 0.38   /* whole voice life incl. fade — decay stays tight */
#define END_FADE_SEC 0.020  /* cosine fade so retriggered tails never pop */
#define ATTACK_SEC 0.0008
#define OS 2                /* 2x oversampled operator */
#define FB_LP_K 0.5         /* one-pole in the feedback loop (at 96 kHz) */

/* SplitMix32-style hash for deterministic per-hit variation. */
static uint32_t ik_hash(uint32_t x) {
  x ^= x >> 16; x *= 0x7feb352dU;
  x ^= x >> 15; x *= 0x846ca68bU;
  x ^= x >> 16;
  return x;
}
static double ik_h01(uint32_t h) { return (double)(h & 0xFFFFFFu) / 16777216.0; }

static void render_note(const NvNote *n, int idx, float *out, int nframes) {
  int s0 = (int)(n->start * NV_SR);
  if (s0 >= nframes) return;
  int len = (int)(KICK_LEN_SEC * NV_SR);
  int fade = (int)(END_FADE_SEC * NV_SR);
  int atk = (int)(ATTACK_SEC * NV_SR);
  if (atk < 1) atk = 1;

  /* deterministic per-hit jitter */
  uint32_t h = ik_hash((uint32_t)idx * 2654435761u ^ (uint32_t)(n->freq * 97.0));
  double jbeta = 0.92 + 0.16 * ik_h01(h);
  double jratio = 0.95 + 0.10 * ik_h01(ik_hash(h));

  /* velocity → chaos depth, drop depth, and every time constant */
  double vel = n->vel;
  double beta0 = (1.7 + 1.3 * vel) * jbeta / (1.0 + n->freq / 1500.0);
  double ratio = (2.0 + 1.5 * vel) * jratio;
  double tau_b = 0.018 + 0.032 * vel; /* β collapse: ~60–150 ms life */
  double tau_p = 0.009 + 0.011 * vel; /* pitch drop */
  double tau_a = 0.045 + 0.050 * vel; /* amp decay */
  double amp = pow(vel, 1.6);

  /* operator state (96 kHz internal) */
  double phase = 0.0, y1 = 0.0, y2 = 0.0, lp = 0.0;
  double dc_x1 = 0.0, dc_y1 = 0.0;

  for (int i = 0; i < len && s0 + i < nframes; i++) {
    double t = (double)i / NV_SR;

    /* the collapse: chaos depth and operator frequency both implode */
    double beta = beta0 * exp(-t / tau_b);
    double f = n->freq * (1.0 + (ratio - 1.0) * exp(-t / tau_p));
    double inc = f / (OS * NV_SR);

    /* 2x oversampled Tomisawa operator, averaged-pair decimation */
    double s = 0.0;
    for (int k = 0; k < OS; k++) {
      lp += FB_LP_K * (0.5 * (y1 + y2) - lp);
      double y = sin(NV_TAU * phase + beta * lp);
      y2 = y1;
      y1 = y;
      phase += inc;
      if (phase >= 1.0) phase -= 1.0;
      s += y;
    }
    s *= 1.0 / OS;

    /* DC blocker — the chaotic regime is asymmetric */
    double o = s - dc_x1 + 0.999 * dc_y1;
    dc_x1 = s;
    dc_y1 = o;

    /* envelope: instant-feeling attack, exp body, cosine end-fade */
    double env = (i < atk) ? (double)i / atk : exp(-(t - ATTACK_SEC) / tau_a);
    if (i > len - fade) {
      double r = (double)(i - (len - fade)) / fade;
      env *= 0.5 * (1.0 + cos(M_PI * r));
    }

    out[s0 + i] += (float)(o * env * amp * 0.85);
  }
}

static void render(const NvMelody *m, float *out, int nframes) {
  for (int j = 0; j < m->count; j++) {
    const NvNote *n = &m->notes[j];
    if (n->freq <= 0) continue;
    render_note(n, j, out, nframes);
  }
}

int main(int argc, char **argv) { return nv_main(argc, argv, "implokick", render); }
