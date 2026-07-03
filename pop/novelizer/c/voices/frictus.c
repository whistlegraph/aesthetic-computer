// frictus.c — banded-waveguide bowed friction voice for pop/novelizer.
//
// TECHNIQUE + CITATION
//   Banded waveguides: Georg Essl & Perry R. Cook, "Banded Waveguides:
//   Towards Physical Modeling of Bowed Bar Percussion Instruments",
//   Proc. ICMC 1999; expanded in Essl, Serafin, Cook & Smith, "Theory of
//   Banded Waveguides", Computer Music Journal 28(1), 2004. Reference
//   implementation lineage: STK's BandedWG (Cook & Scavone).
//
//   One waveguide per structural mode of a uniform free bar: a fractional
//   delay line whose length is the mode's period (NV_SR / f_k) closed in a
//   loop with a bandpass filter centered on that mode (RBJ constant-peak
//   bandpass -> zero phase at center, so the loop resonates exactly at f_k
//   and chromatic tuning tracks the fundamental mode). All bands share one
//   bow point: the relative velocity between the bow and the SUM of band
//   velocities drives a reciprocal-power stick-slip friction table
//   (Smith/Cook bow table, f(x) = min(1, (|x|+0.75)^-4)), and the friction
//   force is injected back into every band. Mode stack uses uniform-bar
//   ratios 1 : 2.756 : 5.404 : 8.933 (transverse modes of a free bar).
//
// PARAMETER MAP
//   note.freq -> fundamental mode frequency; upper modes at bar ratios.
//   note.dur  -> bow stroke length: bow-pressure envelope (~50ms attack,
//                hold, 35ms bow lift) and ring time (held notes are
//                allowed to ring longer, like an undamped bar).
//   note.vel  -> bow speed + bow force + level: harder bowing is louder
//                AND brighter (higher force flattens the friction slope,
//                longer sticking, stronger upper-mode drive).
//   long notes (>= 2s) -> slow "working the rim" LFOs on pressure, bow
//                speed and upper-mode injection so the drone breathes
//                like a singing glass bowl.
//
// WHAT MAKES IT NOVEL IN THE AC STABLE
//   Nothing else here is a *bowed inharmonic* resonator bank. zitar is a
//   plucked harmonic Karplus-Strong; gm_synth's bowed strings are harmonic
//   waveguides; hellsine/sinepower are additive sines; nullnoise is carved
//   noise. frictus sustains an inharmonic bar-mode stack with a breathy
//   friction attack and a metallic ring-out — bowed vibraphone / singing
//   bowl territory. Signature to verify: strong f0 plus non-integer
//   partials near 2.76x / 5.40x / 8.93x f0, noise skirts around each mode
//   while the bow is on, sustained (non-decaying) level through the hold.

#include "../novelizer.h"

#define NMODES 4
#define MAXDELAY 4096

/* uniform free bar transverse-mode ratios (Essl & Cook 1999) */
static const double kRatio[NMODES] = { 1.0, 2.756, 5.404, 8.933 };
static const double kOutW[NMODES]  = { 1.0, 0.40, 0.22, 0.12 };
/* bow point favors the fundamental so stick-slip locks to mode 1
   (bowing a bar at an antinode of the first transverse mode) */
static const double kInj[NMODES]   = { 0.62, 0.20, 0.11, 0.07 };

/* ── RBJ constant-peak bandpass biquad (zero phase at center) ─────── */
typedef struct { double b0, b2, a1, a2, x1, x2, y1, y2; } BiQuad;

static void bp_set(BiQuad *q, double f, double Q) {
  double w0 = NV_TAU * f / NV_SR;
  double alpha = sin(w0) / (2.0 * Q);
  double a0 = 1.0 + alpha;
  q->b0 = alpha / a0;
  q->b2 = -alpha / a0;
  q->a1 = -2.0 * cos(w0) / a0;
  q->a2 = (1.0 - alpha) / a0;
  q->x1 = q->x2 = q->y1 = q->y2 = 0.0;
}

static inline double bp_tick(BiQuad *q, double x) {
  double y = q->b0 * x + q->b2 * q->x2 - q->a1 * q->y1 - q->a2 * q->y2;
  q->x2 = q->x1; q->x1 = x;
  q->y2 = q->y1; q->y1 = y;
  return y;
}

/* ── one banded waveguide: fractional delay + bandpass in a loop ──── */
typedef struct {
  double buf[MAXDELAY];
  int w;           /* write index */
  double len;      /* fractional loop length in samples = NV_SR / f_k */
  BiQuad bp;
  double fbgain;   /* per-pass loop gain -> mode T60 */
  double inj;      /* bow injection weight */
  double outw;     /* output tap weight */
  double y;        /* last band velocity (bandpass output) */
  int on;
} Band;

static inline double band_read(const Band *b) {
  double rp = (double)b->w - b->len;
  while (rp < 0.0) rp += MAXDELAY;
  int i0 = (int)rp;
  double fr = rp - (double)i0;
  int i1 = i0 + 1; if (i1 >= MAXDELAY) i1 -= MAXDELAY;
  return b->buf[i0] * (1.0 - fr) + b->buf[i1] * fr;
}

/* ── noise (xorshift32) for bow breath ────────────────────────────── */
static uint32_t nz_state = 0x9d2c5681u;
static inline double nz(void) {
  nz_state ^= nz_state << 13;
  nz_state ^= nz_state >> 17;
  nz_state ^= nz_state << 5;
  return (double)(int32_t)nz_state * (1.0 / 2147483648.0);
}

/* ── Smith/Cook bow friction table ────────────────────────────────── */
static inline double bowtable(double dv, double slope) {
  double x = fabs(dv * slope) + 0.75;
  double f = pow(x, -4.0);
  return f > 1.0 ? 1.0 : f;
}

/* ── render one bowed note additively into out[] ──────────────────── */
static void bow_note(const NvNote *n, float *out, int nframes, int seed) {
  static Band B[NMODES];
  memset(B, 0, sizeof B);

  double f0 = n->freq;
  double ringT = 0.35 + fmin(n->dur, 1.5); /* fundamental T60, seconds */
  int nb = 0;
  for (int k = 0; k < NMODES; k++) {
    double f = f0 * kRatio[k];
    if (f > 0.42 * NV_SR) break;
    Band *b = &B[k];
    b->on = 1;
    b->len = (double)NV_SR / f;
    if (b->len > MAXDELAY - 2) b->len = MAXDELAY - 2;
    bp_set(&b->bp, f, 8.0 + 4.0 * k);
    double t60 = ringT / (1.0 + 0.85 * k); /* higher bar modes die faster */
    b->fbgain = pow(0.001, 1.0 / (t60 * f));
    b->inj = kInj[k];
    b->outw = kOutW[k];
    nb++;
  }
  if (!nb) return;

  double vel = n->vel;
  double vbow0 = 0.10 + 0.28 * vel;  /* bow speed */
  double bowP  = 0.35 + 0.55 * vel;  /* bow force 0..1 */
  double amp   = 0.35 + 0.65 * vel;
  double atk   = fmin(0.050, n->dur * 0.5);
  int longnote = n->dur >= 2.0;
  double lfoRate = 0.11 + 0.023 * (double)(seed % 3);
  double lfoPh   = 0.61 * (double)seed;

  int s0 = (int)(n->start * NV_SR);
  int len = (int)((n->dur + ringT * 1.4 + 0.15) * NV_SR);
  if (s0 + len > nframes) len = nframes - s0;
  double fadeLen = 0.08 * NV_SR;

  for (int i = 0; i < len; i++) {
    double t = (double)i / NV_SR;

    /* bow pressure envelope: ramp on, hold, 35ms lift, then free ring */
    double press;
    if (t < atk) press = t / atk;
    else if (t < n->dur) press = 1.0;
    else {
      double r = (t - n->dur) / 0.035;
      press = r < 1.0 ? 1.0 - r : 0.0;
    }

    double vbow = vbow0, injHi = 1.0;
    if (longnote && press > 0.0) {
      /* working the bow around the rim: slow drifts in force + speed */
      double lf  = 0.5 + 0.5 * sin(NV_TAU * lfoRate * t + lfoPh);
      double lf2 = 0.5 + 0.5 * sin(NV_TAU * 0.071 * t + lfoPh * 1.7);
      press *= 0.70 + 0.30 * lf;
      vbow  *= 0.80 + 0.35 * lf2;
      injHi  = 0.35 + 0.65 * lf;
    }

    /* friction excitation at the shared bow point */
    double ex = 0.0;
    if (press > 0.0) {
      double vsum = 0.0;
      for (int k = 0; k < NMODES; k++) if (B[k].on) vsum += B[k].y;
      double dv = vbow - vsum;
      double slope = 5.0 - 4.0 * (bowP * (0.55 + 0.45 * press));
      double fric = bowtable(dv, slope);
      ex = dv * fric * press;
      ex += nz() * 0.06 * press * fric; /* breath rides the sticking bow */
    }

    /* tick every band: delay -> bandpass -> loop gain + bow injection */
    double sig = 0.0;
    for (int k = 0; k < NMODES; k++) {
      Band *b = &B[k];
      if (!b->on) continue;
      double d = band_read(b);
      double y = bp_tick(&b->bp, d);
      b->y = y;
      b->buf[b->w] = y * b->fbgain + ex * b->inj * (k ? injHi : 1.0);
      b->w++; if (b->w >= MAXDELAY) b->w = 0;
      sig += y * b->outw;
    }

    double g = amp;
    double left = (double)(len - i);
    if (left < fadeLen) g *= left / fadeLen; /* safety fade, no tail click */
    out[s0 + i] += (float)(sig * g);
  }
}

static void render(const NvMelody *m, float *out, int nframes) {
  nz_state = 0x9d2c5681u; /* deterministic renders */
  for (int j = 0; j < m->count; j++) {
    const NvNote *n = &m->notes[j];
    if (n->freq <= 0) continue;
    bow_note(n, out, nframes, j + 1);
  }
}

int main(int argc, char **argv) { return nv_main(argc, argv, "frictus", render); }
