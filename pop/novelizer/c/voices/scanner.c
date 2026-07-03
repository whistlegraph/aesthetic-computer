// scanner.c — SCANNED SYNTHESIS voice for pop/novelizer.
//
// Technique: Scanned Synthesis (Bill Verplank, Max Mathews, Rob Shaw,
// ~1999-2000; "Scanned Synthesis", ICMC 2000 / Interval Research).
// A closed string of 128 masses connected by tension springs, with
// centering springs to ground, uniform damping, and a viscosity term,
// is integrated at a LOW model rate (6 kHz steps, dynamics tuned so the
// string's own modes live in the HAPTIC band, ~2..60 Hz). At note-on a
// hammer strikes the string (raised-cosine displacement + velocity kick).
// While the note is held, a slow wandering "finger" keeps pressing the
// string (Verplank's haptic manipulation), so held tones never freeze.
// The audio is made by SCANNING the string's shape as a wavetable at the
// note's pitch rate (normalized phase 0..1, advanced by freq/NV_SR,
// Catmull-Rom interpolation, DC = string mean removed). Pitch is thus
// locked to the scan rate while the timbre crawls and breathes at the
// speed of the slow mechanical system.
//
// Parameter map:
//   TENSION      inter-mass spring (sets haptic mode ladder, mode1 ~6 Hz)
//   CENTERING    spring to rest position (kills DC drift, mode floor ~2 Hz)
//   DAMPING      uniform velocity loss (how fast motion dies, ~2 s ring)
//   VISCOSITY    lap(v) loss — high spatial modes darken first
//   hammer amp   ∝ velocity^2 (harder hit = more displacement = louder)
//   hammer width narrower when velocity is high, wider for high pitches
//                (harder = brighter attack; wide = fewer alias partials)
//   finger force slow 5 Hz push at a wandering position while held
//   tension LFO  ±12% at 0.17 Hz → brightness drift (stiffness knob)
//
// Novel vs the AC stable: nothing else here scans a live mechanical
// shape. Karplus/zitar's delay line IS the waveform at audio rate;
// here the mechanics run ~3 decades slower than the scan, so each note
// is a wavetable whose contents visibly morph over hundreds of ms —
// harmonic amplitudes crawl independently instead of decaying together.
//
// Signature to verify: clean equal-tempered f0 (scan-rate locked) with
// harmonic partials whose relative levels churn within a single note;
// drone shows nonzero centroidStdHz / fluxMean from the finger + LFO.

#include "../novelizer.h"

#define N_MASS 128
#define PHYS_DIV 8                       /* physics step every 8 samples  */
#define PHYS_DT ((double)PHYS_DIV / NV_SR) /* 6 kHz model rate            */

#define TENSION 6.2e3   /* mode ladder ~2.1..25 Hz — ALL dynamics haptic   */
#define CENTERING 160.0 /* ≈ (2π·2Hz)² floor, holds string around zero     */
#define DAMPING 1.2     /* 1/s — free crawl ~2 s                           */
#define VISCOSITY 6.0   /* lap(v) coefficient — treble fades first         */

typedef struct {
  double y[N_MASS];  /* mass displacement (the live wavetable)   */
  double v[N_MASS];  /* mass velocity                            */
  double yA[N_MASS]; /* previous physics frame, for scan xfade   */
  double meanA, meanB;
  double t;          /* model time since note-on (s)             */
  double fingerPh;   /* per-note phase offsets for finger/LFO    */
  double lfoPh;
} NvString;

/* deterministic per-note hash → 0..1 (no libc rand; renders reproduce) */
static double nv_hash01(unsigned h) {
  h ^= h >> 16; h *= 0x7feb352dU; h ^= h >> 15; h *= 0x846ca68bU; h ^= h >> 16;
  return (double)h / 4294967296.0;
}

/* raised-cosine bump of half-width w masses centered at c, height amp */
static void nv_bump(double *dst, double c, double w, double amp) {
  for (int i = 0; i < N_MASS; i++) {
    double d = i - c;
    if (d > N_MASS / 2.0) d -= N_MASS;   /* ring wrap */
    if (d < -N_MASS / 2.0) d += N_MASS;
    if (fabs(d) < w) dst[i] += amp * 0.5 * (1.0 + cos(M_PI * d / w));
  }
}

/* one symplectic-Euler physics step at 6 kHz; hold = note still pressed */
static void nv_step(NvString *s, double held, double vel) {
  double tLfo = 1.0 + 0.12 * sin(NV_TAU * (0.17 * s->t + s->lfoPh)); /* stiffness drift */
  double T = TENSION * tLfo;
  double acc[N_MASS];
  for (int i = 0; i < N_MASS; i++) {
    int im = (i + N_MASS - 1) % N_MASS, ip = (i + 1) % N_MASS;
    double lapY = s->y[im] + s->y[ip] - 2.0 * s->y[i];
    double lapV = s->v[im] + s->v[ip] - 2.0 * s->v[i];
    acc[i] = T * lapY - CENTERING * s->y[i] - DAMPING * s->v[i] + VISCOSITY * lapV;
  }
  if (held > 0.0) {
    /* wandering finger: slow 5 Hz push at a drifting position (haptic) */
    double pos = N_MASS * (0.5 + 0.27 * sin(NV_TAU * (0.13 * s->t + s->fingerPh)));
    double push = 260.0 * vel * held * sin(NV_TAU * 5.0 * s->t);
    double fAcc[N_MASS] = {0};
    nv_bump(fAcc, pos, N_MASS / 9.0, push);
    for (int i = 0; i < N_MASS; i++) acc[i] += fAcc[i];
  }
  double mean = 0;
  for (int i = 0; i < N_MASS; i++) {
    s->v[i] += PHYS_DT * acc[i];
    s->y[i] += PHYS_DT * s->v[i];
    mean += s->y[i];
  }
  s->meanB = mean / N_MASS;
  s->t += PHYS_DT;
}

/* Catmull-Rom scan of a ring table, phase 0..1 */
static double nv_scan(const double *y, double ph) {
  double x = ph * N_MASS;
  int i1 = (int)x;
  double f = x - i1;
  int i0 = (i1 + N_MASS - 1) & (N_MASS - 1);
  int i2 = (i1 + 1) & (N_MASS - 1);
  int i3 = (i1 + 2) & (N_MASS - 1);
  i1 &= (N_MASS - 1);
  double a = y[i0], b = y[i1], c = y[i2], d = y[i3];
  return b + 0.5 * f * (c - a + f * (2.0 * a - 5.0 * b + 4.0 * c - d + f * (3.0 * (b - c) + d - a)));
}

static void render(const NvMelody *m, float *out, int nframes) {
  for (int j = 0; j < m->count; j++) {
    const NvNote *n = &m->notes[j];
    if (n->freq <= 0) continue;

    NvString s;
    memset(&s, 0, sizeof s);
    unsigned seed = (unsigned)(j * 2654435761u) ^ (unsigned)(n->freq * 97.0);
    s.fingerPh = nv_hash01(seed);
    s.lfoPh = nv_hash01(seed ^ 0x9e3779b9u);

    /* hammer at note-on: displacement bump + velocity kick.
       harder hit = taller + narrower (brighter); high notes get wider
       bumps so their scanned spectrum stays inside Nyquist. */
    double vel = n->vel;
    double hamAmp = 0.25 + 0.75 * vel * vel;
    double wide = 1.0 + n->freq / 700.0;
    double hamW = N_MASS * (0.055 + 0.075 * (1.0 - vel)) * wide;
    double hamPos = N_MASS * (0.18 + 0.24 * nv_hash01(seed ^ 0x51ed270bu));
    nv_bump(s.y, hamPos, hamW, hamAmp);
    nv_bump(s.v, hamPos, hamW * 0.7, hamAmp * 35.0); /* strike kick */
    memcpy(s.yA, s.y, sizeof s.yA);
    double mean = 0;
    for (int i = 0; i < N_MASS; i++) mean += s.y[i];
    s.meanA = s.meanB = mean / N_MASS;

    int s0 = (int)(n->start * NV_SR);
    double rel = 0.16;                        /* release tail after note-off */
    int len = (int)((n->dur + 1.2) * NV_SR);
    double phase = 0.0, inc = n->freq / NV_SR;
    double att = 0.005;
    double noteGain = 0.30 + 0.70 * vel;

    for (int i = 0; i < len && s0 + i < nframes; i++) {
      if (i % PHYS_DIV == 0) {
        memcpy(s.yA, s.y, sizeof s.yA);
        s.meanA = s.meanB;
        double t = (double)i / NV_SR;
        double held = (t < n->dur) ? 1.0 : 0.0;
        nv_step(&s, held, vel);
      }
      double xf = (double)(i % PHYS_DIV) / PHYS_DIV;
      double sa = nv_scan(s.yA, phase) - s.meanA;
      double sb = nv_scan(s.y, phase) - s.meanB;
      double sig = sa + (sb - sa) * xf;

      double t = (double)i / NV_SR;
      double env;
      if (t < att) env = t / att;
      else if (t < n->dur) env = 1.0;
      else env = exp(-(t - n->dur) / rel);

      out[s0 + i] += (float)(sig * env * noteGain);
      phase += inc;
      if (phase >= 1.0) phase -= 1.0;
    }
  }
}

int main(int argc, char **argv) { return nv_main(argc, argv, "scanner", render); }
