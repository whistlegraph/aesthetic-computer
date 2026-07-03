// novelette.c — the novelizer's first track: every voice is a research
// keeper from pop/novelizer batch 1 (frictus bed, scanner groove, vosim
// lead), with kick/snare slots reserved for the batch-2 percussion
// winners. One C engine, one score, one WAV — the hellsine contract.
//
// Voice cores are adapted from pop/novelizer/c/voices/{frictus,scanner,
// vosim}.c (same DSP, same citations — see those headers), reshaped from
// harness renderers into per-note stereo bus writers:
//   - frictus: Band state moved off static storage (heap per note)
//   - scanner: unchanged physics, pan + gain params added
//   - vosim:   unchanged oscillator, extracted to a note function
//
// Form: D dorian, 96 BPM, ~1:35 + ring-out.
//   intro drone -> groove (Dm Am C G) -> vosim verse -> break -> lift
//   (verse melody up a 4th) -> outro.
//
// Build:  ./build.sh      Render: ./build/novelette [--out path.wav]

#define _POSIX_C_SOURCE 200809L
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <math.h>

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif
#define TAU (2.0 * M_PI)

static const int SR = 48000;
static const double BPM = 104.0;
#define BEAT (60.0 / BPM)              /* ~0.577 s */
static const double TRACK_BEATS = 142.0; /* 136 composed + ring-out */

static inline double mtof(double m) { return 440.0 * pow(2.0, (m - 69.0) / 12.0); }

/* deterministic per-note hash → 0..1 (shared by all voices) */
static double hash01(unsigned h) {
  h ^= h >> 16; h *= 0x7feb352dU; h ^= h >> 15; h *= 0x846ca68bU; h ^= h >> 16;
  return (double)h / 4294967296.0;
}

/* ── event list: the score is emitted programmatically ─────────────── */
typedef struct {
  double beat, beats; /* start + length in beats */
  double midi, vel;
  double pan;         /* -1..1, constant-power at render */
  int seed;
} Ev;

#define MAXEV 1024
typedef struct { Ev ev[MAXEV]; int n; } Part;

static void emit(Part *p, double beat, double beats, double midi, double vel, double pan) {
  if (p->n >= MAXEV) { fprintf(stderr, "novelette: part overflow\n"); exit(1); }
  Ev *e = &p->ev[p->n];
  e->beat = beat; e->beats = beats; e->midi = midi; e->vel = vel; e->pan = pan;
  e->seed = p->n * 2654435761u % 100000 + 7;
  p->n++;
}

/* ── stereo bus ─────────────────────────────────────────────────────── */
typedef struct { float *L, *R; int n; } Bus;

static Bus bus_new(int n) {
  Bus b = { calloc((size_t)n, sizeof(float)), calloc((size_t)n, sizeof(float)), n };
  if (!b.L || !b.R) { fprintf(stderr, "novelette: alloc failed\n"); exit(1); }
  return b;
}

static inline void bus_add(Bus *b, int i, double sig, double pan, double gain) {
  if (i < 0 || i >= b->n) return;
  double a = 0.25 * M_PI * (pan + 1.0) * 0.5; /* constant-power */
  b->L[i] += (float)(sig * gain * cos(a) * 1.414);
  b->R[i] += (float)(sig * gain * sin(a) * 1.414);
}

/* ════ FRICTUS — banded-waveguide bowed bar (Essl & Cook 1999) ═══════ */

#define FR_NMODES 4
#define FR_MAXDELAY 4096
static const double frRatio[FR_NMODES] = { 1.0, 2.756, 5.404, 8.933 };
static const double frOutW[FR_NMODES]  = { 1.0, 0.40, 0.22, 0.12 };
static const double frInj[FR_NMODES]   = { 0.62, 0.20, 0.11, 0.07 };

typedef struct { double b0, b2, a1, a2, x1, x2, y1, y2; } BiQuad;

static void bp_set(BiQuad *q, double f, double Q) {
  double w0 = TAU * f / SR, alpha = sin(w0) / (2.0 * Q), a0 = 1.0 + alpha;
  q->b0 = alpha / a0; q->b2 = -alpha / a0;
  q->a1 = -2.0 * cos(w0) / a0; q->a2 = (1.0 - alpha) / a0;
  q->x1 = q->x2 = q->y1 = q->y2 = 0.0;
}
static inline double bp_tick(BiQuad *q, double x) {
  double y = q->b0 * x + q->b2 * q->x2 - q->a1 * q->y1 - q->a2 * q->y2;
  q->x2 = q->x1; q->x1 = x; q->y2 = q->y1; q->y1 = y;
  return y;
}

typedef struct {
  double buf[FR_MAXDELAY];
  int w;
  double len;
  BiQuad bp;
  double fbgain, inj, outw, y;
  int on;
} FrBand;

static inline double fr_read(const FrBand *b) {
  double rp = (double)b->w - b->len;
  while (rp < 0.0) rp += FR_MAXDELAY;
  int i0 = (int)rp;
  double fr = rp - (double)i0;
  int i1 = i0 + 1; if (i1 >= FR_MAXDELAY) i1 -= FR_MAXDELAY;
  return b->buf[i0] * (1.0 - fr) + b->buf[i1] * fr;
}

static uint32_t nz_state = 0x9d2c5681u;
static inline double nz(void) {
  nz_state ^= nz_state << 13; nz_state ^= nz_state >> 17; nz_state ^= nz_state << 5;
  return (double)(int32_t)nz_state * (1.0 / 2147483648.0);
}

static inline double bowtable(double dv, double slope) {
  double f = pow(fabs(dv * slope) + 0.75, -4.0);
  return f > 1.0 ? 1.0 : f;
}

static void frictus_note(const Ev *e, Bus *bus, Bus *send, double gain, double sendg) {
  double f0 = mtof(e->midi), start = e->beat * BEAT, dur = e->beats * BEAT;
  double ringT = 0.35 + fmin(dur, 1.5);
  FrBand *B = calloc(FR_NMODES, sizeof(FrBand));
  if (!B) exit(1);
  int nb = 0;
  for (int k = 0; k < FR_NMODES; k++) {
    double f = f0 * frRatio[k];
    if (f > 0.42 * SR) break;
    FrBand *b = &B[k];
    b->on = 1;
    b->len = (double)SR / f;
    if (b->len > FR_MAXDELAY - 2) b->len = FR_MAXDELAY - 2;
    bp_set(&b->bp, f, 8.0 + 4.0 * k);
    double t60 = ringT / (1.0 + 0.85 * k);
    b->fbgain = pow(0.001, 1.0 / (t60 * f));
    b->inj = frInj[k]; b->outw = frOutW[k];
    nb++;
  }
  if (!nb) { free(B); return; }

  double vel = e->vel;
  double vbow0 = 0.10 + 0.28 * vel, bowP = 0.35 + 0.55 * vel, amp = 0.35 + 0.65 * vel;
  double atk = fmin(0.050, dur * 0.5);
  int longnote = dur >= 2.0;
  double lfoRate = 0.11 + 0.023 * (double)(e->seed % 3);
  double lfoPh = 0.61 * (double)e->seed;

  int s0 = (int)(start * SR);
  int len = (int)((dur + ringT * 1.4 + 0.15) * SR);
  if (s0 + len > bus->n) len = bus->n - s0;
  double fadeLen = 0.08 * SR;

  for (int i = 0; i < len; i++) {
    double t = (double)i / SR;
    double press;
    if (t < atk) press = t / atk;
    else if (t < dur) press = 1.0;
    else { double r = (t - dur) / 0.035; press = r < 1.0 ? 1.0 - r : 0.0; }

    double vbow = vbow0, injHi = 1.0;
    if (longnote && press > 0.0) {
      double lf = 0.5 + 0.5 * sin(TAU * lfoRate * t + lfoPh);
      double lf2 = 0.5 + 0.5 * sin(TAU * 0.071 * t + lfoPh * 1.7);
      press *= 0.70 + 0.30 * lf;
      vbow *= 0.80 + 0.35 * lf2;
      injHi = 0.35 + 0.65 * lf;
    }

    double ex = 0.0;
    if (press > 0.0) {
      double vsum = 0.0;
      for (int k = 0; k < FR_NMODES; k++) if (B[k].on) vsum += B[k].y;
      double dv = vbow - vsum;
      double slope = 5.0 - 4.0 * (bowP * (0.55 + 0.45 * press));
      double fric = bowtable(dv, slope);
      ex = dv * fric * press + nz() * 0.06 * press * fric;
    }

    double sig = 0.0;
    for (int k = 0; k < FR_NMODES; k++) {
      FrBand *b = &B[k];
      if (!b->on) continue;
      double d = fr_read(b);
      double y = bp_tick(&b->bp, d);
      b->y = y;
      b->buf[b->w] = y * b->fbgain + ex * b->inj * (k ? injHi : 1.0);
      b->w++; if (b->w >= FR_MAXDELAY) b->w = 0;
      sig += y * b->outw;
    }

    double g = amp;
    double left = (double)(len - i);
    if (left < fadeLen) g *= left / fadeLen;
    bus_add(bus, s0 + i, sig, e->pan, gain * g);
    bus_add(send, s0 + i, sig, e->pan, sendg * g);
  }
  free(B);
}

/* ════ SCANNER — scanned synthesis (Verplank/Mathews/Shaw 2000) ══════ */

#define SC_NMASS 128
#define SC_PHYSDIV 8
#define SC_PHYSDT ((double)SC_PHYSDIV / SR)
#define SC_TENSION 6.2e3
#define SC_CENTERING 160.0
#define SC_DAMPING 1.2
#define SC_VISCOSITY 6.0

typedef struct {
  double y[SC_NMASS], v[SC_NMASS], yA[SC_NMASS];
  double meanA, meanB, t, fingerPh, lfoPh;
} ScString;

static void sc_bump(double *dst, double c, double w, double amp) {
  for (int i = 0; i < SC_NMASS; i++) {
    double d = i - c;
    if (d > SC_NMASS / 2.0) d -= SC_NMASS;
    if (d < -SC_NMASS / 2.0) d += SC_NMASS;
    if (fabs(d) < w) dst[i] += amp * 0.5 * (1.0 + cos(M_PI * d / w));
  }
}

static void sc_step(ScString *s, double held, double vel) {
  double T = SC_TENSION * (1.0 + 0.12 * sin(TAU * (0.17 * s->t + s->lfoPh)));
  double acc[SC_NMASS];
  for (int i = 0; i < SC_NMASS; i++) {
    int im = (i + SC_NMASS - 1) % SC_NMASS, ip = (i + 1) % SC_NMASS;
    double lapY = s->y[im] + s->y[ip] - 2.0 * s->y[i];
    double lapV = s->v[im] + s->v[ip] - 2.0 * s->v[i];
    acc[i] = T * lapY - SC_CENTERING * s->y[i] - SC_DAMPING * s->v[i] + SC_VISCOSITY * lapV;
  }
  if (held > 0.0) {
    double pos = SC_NMASS * (0.5 + 0.27 * sin(TAU * (0.13 * s->t + s->fingerPh)));
    double push = 260.0 * vel * held * sin(TAU * 5.0 * s->t);
    double fAcc[SC_NMASS] = {0};
    sc_bump(fAcc, pos, SC_NMASS / 9.0, push);
    for (int i = 0; i < SC_NMASS; i++) acc[i] += fAcc[i];
  }
  double mean = 0;
  for (int i = 0; i < SC_NMASS; i++) {
    s->v[i] += SC_PHYSDT * acc[i];
    s->y[i] += SC_PHYSDT * s->v[i];
    mean += s->y[i];
  }
  s->meanB = mean / SC_NMASS;
  s->t += SC_PHYSDT;
}

static double sc_scan(const double *y, double ph) {
  double x = ph * SC_NMASS;
  int i1 = (int)x;
  double f = x - i1;
  int i0 = (i1 + SC_NMASS - 1) & (SC_NMASS - 1);
  int i2 = (i1 + 1) & (SC_NMASS - 1);
  int i3 = (i1 + 2) & (SC_NMASS - 1);
  i1 &= (SC_NMASS - 1);
  double a = y[i0], b = y[i1], c = y[i2], d = y[i3];
  return b + 0.5 * f * (c - a + f * (2.0 * a - 5.0 * b + 4.0 * c - d + f * (3.0 * (b - c) + d - a)));
}

static void scanner_note(const Ev *e, Bus *bus, Bus *send, double gain, double sendg) {
  double freq = mtof(e->midi), start = e->beat * BEAT, dur = e->beats * BEAT;
  ScString s;
  memset(&s, 0, sizeof s);
  unsigned seed = (unsigned)(e->seed * 2654435761u) ^ (unsigned)(freq * 97.0);
  s.fingerPh = hash01(seed);
  s.lfoPh = hash01(seed ^ 0x9e3779b9u);

  double vel = e->vel;
  double hamAmp = 0.25 + 0.75 * vel * vel;
  double wide = 1.0 + freq / 700.0;
  double hamW = SC_NMASS * (0.055 + 0.075 * (1.0 - vel)) * wide;
  double hamPos = SC_NMASS * (0.18 + 0.24 * hash01(seed ^ 0x51ed270bu));
  sc_bump(s.y, hamPos, hamW, hamAmp);
  sc_bump(s.v, hamPos, hamW * 0.7, hamAmp * 35.0);
  memcpy(s.yA, s.y, sizeof s.yA);
  double mean = 0;
  for (int i = 0; i < SC_NMASS; i++) mean += s.y[i];
  s.meanA = s.meanB = mean / SC_NMASS;

  int s0 = (int)(start * SR);
  double rel = 0.16;
  int len = (int)((dur + 1.2) * SR);
  double phase = 0.0, inc = freq / SR, att = 0.005;
  double noteGain = 0.30 + 0.70 * vel;

  for (int i = 0; i < len && s0 + i < bus->n; i++) {
    if (i % SC_PHYSDIV == 0) {
      memcpy(s.yA, s.y, sizeof s.yA);
      s.meanA = s.meanB;
      double t = (double)i / SR;
      sc_step(&s, (t < dur) ? 1.0 : 0.0, vel);
    }
    double xf = (double)(i % SC_PHYSDIV) / SC_PHYSDIV;
    double sa = sc_scan(s.yA, phase) - s.meanA;
    double sb = sc_scan(s.y, phase) - s.meanB;
    double sig = sa + (sb - sa) * xf;

    double t = (double)i / SR, env;
    if (t < att) env = t / att;
    else if (t < dur) env = 1.0;
    else env = exp(-(t - dur) / rel);

    bus_add(bus, s0 + i, sig * env, e->pan, gain * noteGain);
    bus_add(send, s0 + i, sig * env, e->pan, sendg * noteGain);
    phase += inc;
    if (phase >= 1.0) phase -= 1.0;
  }
}

/* ════ VOSIM — pulse-train formants (Kaegi & Tempelaars 1978) ════════ */

#define VO_VOWELS 6
static const double vowel_f[VO_VOWELS] = { 720.0, 470.0, 290.0, 520.0, 360.0, 640.0 };
static double smoothstep(double x) { return x * x * (3.0 - 2.0 * x); }

static void vosim_note(const Ev *e, Bus *bus, Bus *send, double gain, double sendg) {
  double f0base = mtof(e->midi), start = e->beat * BEAT, dur = e->beats * BEAT;
  double vel = e->vel;
  int s0 = (int)(start * SR);
  double rel = 0.14; /* softened from the harness voice's 60 ms guillotine */
  int len = (int)((dur + rel) * SR);

  int ndes = 2 + (int)floor(vel * 6.0 + 0.5);
  if (ndes < 1) ndes = 1;
  if (ndes > 8) ndes = 8;
  double bbase = 0.50 + 0.42 * vel;
  double vrate = (dur > 1.2) ? 0.55 : 1.0 / (dur + 0.05);
  double vstart = (double)(e->seed % VO_VOWELS);

  double p = 1.0, inc = f0base / SR, period = 1.0 / f0base;
  double T = 1.0 / 500.0;
  int N = ndes;
  double amps[9] = { 0 };
  double x1 = 0.0, y1 = 0.0;

  for (int i = 0; i < len && s0 + i < bus->n; i++) {
    double t = (double)i / SR;
    if (p >= 1.0) {
      p -= 1.0;
      double f0 = f0base;
      if (dur > 0.5 && t > 0.25) {
        double dep = fmin(1.0, (t - 0.25) / 0.4) * 0.0035;
        f0 *= 1.0 + dep * sin(TAU * 5.1 * t);
      }
      inc = f0 / SR;
      period = 1.0 / f0;
      double v = vstart + vrate * t;
      int i0 = (int)floor(v) % VO_VOWELS;
      int iB = (i0 + 1) % VO_VOWELS;
      double fr = smoothstep(v - floor(v));
      double F = vowel_f[i0] + (vowel_f[iB] - vowel_f[i0]) * fr;
      double b = bbase * (0.88 + 0.12 * sin(TAU * 0.09 * t + (double)e->seed));
      if (b > 0.97) b = 0.97;
      if (F < f0 / 0.92) F = f0 / 0.92;
      T = 1.0 / F;
      N = ndes;
      int nmax = (int)floor(0.92 * F / f0);
      if (N > nmax) N = nmax > 1 ? nmax : 1;
      double eE = 0.0, a = 1.0;
      for (int k = 0; k < N; k++) { eE += a * a; a *= b; }
      double gnorm = 1.0 / sqrt(eE > 1e-9 ? eE : 1e-9);
      a = gnorm;
      for (int k = 0; k < N; k++) { amps[k] = a; a *= b; }
    }

    double tin = p * period, x = 0.0;
    double kf = tin / T;
    int k = (int)kf;
    if (k < N) {
      double sv = sin(M_PI * (kf - (double)k));
      x = amps[k] * sv * sv;
    }
    double y = x - x1 + 0.995 * y1;
    x1 = x; y1 = y;

    double env;
    if (t < 0.006) env = t / 0.006;
    else if (t < dur) env = exp(-0.35 * (t - 0.006) / dur);
    else {
      double e0 = exp(-0.35 * (dur - 0.006) / dur);
      env = e0 * (1.0 - (t - dur) / rel);
      if (env < 0.0) env = 0.0;
    }

    double sig = y * env * (0.25 + 0.75 * vel) * 0.5;
    bus_add(bus, s0 + i, sig, e->pan, gain);
    bus_add(send, s0 + i, sig, e->pan, sendg);
    p += inc;
  }
}

/* ════ MEMKICK — tension-modulated membrane kick (batch 2) ═══════════ */
/* Fletcher & Rossing Bessel mode stack; pitch drop is emergent from
   displacement-energy tension modulation. Adapted from memkick.c. */

#define MK_NMODES 6
static const double MK_RATIO[MK_NMODES] = { 1.0, 1.594, 2.136, 2.296, 2.653, 2.918 };
#define MK_GAMMA 6.0
#define MK_CLICK_TAU 0.0012
#define MK_CLICK_LEN 0.012

static void memkick_note(const Ev *e, Bus *bus, Bus *send, double gain, double sendg) {
  double f0 = mtof(e->midi), vel = e->vel;
  int s0 = (int)(e->beat * BEAT * SR);
  if (s0 >= bus->n) return;
  unsigned seed = (unsigned)(e->seed * 2654435761u) ^ (unsigned)(f0 * 64.0);

  double reg = pow(65.406 / f0, 0.25);
  if (reg > 1.6) reg = 1.6;
  if (reg < 0.25) reg = 0.25;
  double tau0 = (0.060 - 0.026 * vel) * reg;
  double bright = 2.2 - 1.0 * vel;

  double w[MK_NMODES], tau[MK_NMODES], det[MK_NMODES], ph[MK_NMODES];
  double s0w = 0.0;
  for (int k = 0; k < MK_NMODES; k++) {
    w[k] = pow(MK_RATIO[k], -bright);
    tau[k] = tau0 / (1.0 + 0.9 * (MK_RATIO[k] - 1.0));
    det[k] = 1.0 + 0.004 * (hash01(seed + 7u * (unsigned)k) - 0.5);
    ph[k] = 0.0;
    s0w += w[k] * w[k];
  }

  double tstrike = 0.0032 - 0.0016 * vel;
  double amp = 0.9 * pow(vel, 1.4);
  double fc = 22.0 * f0;
  if (fc < 900.0) fc = 900.0;
  if (fc > 3800.0) fc = 3800.0;
  double f1 = 2.0 * sin(M_PI * fc / SR);
  double svf_lp = 0.0, svf_bp = 0.0;
  double click_amp = 0.11 * pow(vel, 1.6);
  uint32_t rng = seed ^ 0x9e3779b9u;

  int len = (int)(6.9 * tau0 * SR) + (int)(0.06 * SR);
  if (len > bus->n - s0) len = bus->n - s0;

  for (int i = 0; i < len; i++) {
    double t = (double)i / SR;
    double s = (t < tstrike) ? 0.5 * (1.0 - cos(M_PI * t / tstrike)) : 1.0;
    double ek[MK_NMODES], E = 0.0;
    for (int k = 0; k < MK_NMODES; k++) {
      ek[k] = w[k] * s * exp(-t / tau[k]);
      E += ek[k] * ek[k];
    }
    E *= vel * vel / s0w;
    double g = sqrt(1.0 + MK_GAMMA * E);
    double y = 0.0;
    for (int k = 0; k < MK_NMODES; k++) {
      y += ek[k] * sin(TAU * ph[k]);
      ph[k] += MK_RATIO[k] * det[k] * f0 * g / SR;
      if (ph[k] >= 1.0) ph[k] -= 1.0;
    }
    if (t < MK_CLICK_LEN) {
      double ce = (t < 0.0003) ? t / 0.0003 : exp(-(t - 0.0003) / MK_CLICK_TAU);
      rng = rng * 1664525u + 1013904223u;
      double nzv = ((double)(rng >> 9) / 4194304.0) - 1.0;
      svf_lp += f1 * svf_bp;
      double hp = nzv - svf_lp - 0.8 * svf_bp;
      svf_bp += f1 * hp;
      y += click_amp * ce * svf_bp;
    }
    bus_add(bus, s0 + i, amp * y, e->pan, gain);
    bus_add(send, s0 + i, amp * y, e->pan, sendg);
  }
}

/* ════ CAVIKICK — Helmholtz-port kick (batch 2), the shuffle ghost ═══ */
/* Ingard-Ising nonlinear orifice loss + flow-gated chuff. Adapted from
   cavikick.c. */

static void cavikick_note(const Ev *e, Bus *bus, Bus *send, double gain, double sendg) {
  const double dt = 1.0 / SR;
  double freq = mtof(e->midi), vel = e->vel;
  int s0 = (int)(e->beat * BEAT * SR);
  if (s0 >= bus->n) return;
  int len = (int)(0.85 * SR);
  if (s0 + len > bus->n) len = bus->n - s0;
  int fade = (int)(0.010 * SR);

  unsigned h = (unsigned)(e->seed * 2654435761u) ^ (unsigned)(freq * 64.0);
  uint32_t rng = h | 1u;
  double detune = 1.0 + (((h >> 8) & 0xffff) / 65535.0 - 0.5) * 0.006;
  double jit = (((h >> 20) & 0xff) / 255.0 - 0.5) * 0.10;

  const double D_LIN = 2.0 * 6.9078 / 0.30, D_NL = 105.0;
  double wt = TAU * freq * detune;
  double a = 0.5 * sqrt(wt * wt + 0.25 * D_LIN * D_LIN) * dt;
  if (a > 1.5) a = 1.5;
  double w0 = 2.0 * sin(a) / dt, w02 = w0 * w0;

  double tau_r = 0.0007, tau_f = (0.006 - 0.0025 * vel) * (1.0 + jit);
  double P = pow(vel, 1.35) * wt * wt;

  double fk = freq * 6.27;
  int knock_on = fk < 18000.0;
  double ak = 0.5 * TAU * fk * dt;
  if (ak > 1.5) ak = 1.5;
  double wk = knock_on ? 2.0 * sin(ak) / dt : 1.0;
  double wk2 = wk * wk, dk = 2.0 * 6.9078 / 0.055;

  double klp = 1.0 - exp(-TAU * 1250.0 * dt), khp = 1.0 - exp(-TAU * 250.0 * dt);
  double lp1 = 0, lp2 = 0, hpt = 0;
  double x = 0, v = 0, xk = 0, vk = 0;

  for (int i = 0; i < len; i++) {
    double t = i * dt;
    double F = P * (1.0 - exp(-t / tau_r)) * exp(-t / tau_f);
    double u = v / w0, au = fabs(u);
    double d = D_LIN + D_NL * (au > 1.4 ? 1.4 : au);
    v *= exp(-d * dt);
    v += dt * (-w02 * x + F);
    x += dt * v;
    double y = v / w0;
    if (knock_on) {
      vk *= exp(-dk * dt);
      vk += dt * (-wk2 * xk + F);
      xk += dt * vk;
      y += 0.13 * (vk / wk);
    }
    rng ^= rng << 13; rng ^= rng >> 17; rng ^= rng << 5;
    double wnz = ((double)(rng >> 8) / 8388608.0) - 1.0;
    lp1 += klp * (wnz - lp1);
    lp2 += klp * (lp1 - lp2);
    hpt += khp * (lp2 - hpt);
    double gate = u * u;
    if (gate > 1.5) gate = 1.5;
    y += 0.55 * (lp2 - hpt) * gate * exp(-t / 0.070);
    if (i >= len - fade) y *= (double)(len - i) / fade;
    bus_add(bus, s0 + i, 0.9 * y, e->pan, gain);
    bus_add(send, s0 + i, 0.9 * y, e->pan, sendg);
  }
}

/* ════ GRANSNARE — grain-cloud snare (batch 2) ═══════════════════════ */
/* Roads pulsaret cloud; the body is a statistical condensation of
   phase-locked tail grains. Adapted from gransnare.c. */

typedef struct { uint32_t s; } GsRng;
static inline uint32_t gs_next(GsRng *r) {
  uint32_t x = r->s;
  x ^= x << 13; x ^= x >> 17; x ^= x << 5;
  r->s = x; return x;
}
static inline double gs_frand(GsRng *r) { return (double)(gs_next(r) >> 8) * (1.0 / 16777216.0); }
static inline double gs_tri(GsRng *r) { return gs_frand(r) + gs_frand(r) - 1.0; }

static inline double gs_win(double x) {
  const double att = 0.12, tail = 0.10, k = 5.0;
  if (x <= 0.0 || x >= 1.0) return 0.0;
  double w = (x < att) ? 0.5 - 0.5 * cos(M_PI * x / att)
                       : exp(-k * (x - att) / (1.0 - att));
  if (x > 1.0 - tail) w *= 0.5 + 0.5 * cos(M_PI * (x - (1.0 - tail)) / tail);
  return w;
}

static void gransnare_note(const Ev *e, Bus *bus, Bus *send, double gain, double sendg) {
  double vel = e->vel, fbody = mtof(e->midi);
  while (fbody < 120.0) fbody *= 2.0;
  int s0 = (int)(e->beat * BEAT * SR);
  if (s0 >= bus->n) return;

  GsRng rng;
  rng.s = (uint32_t)(e->seed * 0x9E3779B9u) ^ (uint32_t)(e->beat * 977.0) ^ 0x5F3759DFu;
  if (!rng.s) rng.s = 0xACu;

  double hitdur = 0.16 + 0.24 * vel;
  int ngrains = 50 + (int)(250.0 * pow(vel, 1.6));
  int nburst = 6 + (int)(30.0 * vel);
  double tau = 0.25 * hitdur;
  double bright_tau = 0.030 + 0.020 * vel;
  double fmax = 2200.0 + 6800.0 * pow(vel, 1.2);
  double amp_tau = 0.35 * hitdur;
  double hit_amp = 0.40 * pow(vel, 1.35);
  double trunc = 1.0 - exp(-hitdur / tau);

  for (int g = 0; g < ngrains; g++) {
    double onset = (g < nburst) ? gs_frand(&rng) * 0.002
                                : -tau * log(1.0 - gs_frand(&rng) * trunc);
    double br = exp(-onset / bright_tau);
    double fw, phase, cycles, amp;
    int is_body = gs_frand(&rng) < pow(1.0 - br, 1.2);
    if (is_body) {
      fw = fbody * (gs_frand(&rng) < 0.30 ? 1.58 : 1.0);
      fw *= 1.0 + 0.004 * gs_tri(&rng);
      phase = fw * onset;
      phase -= floor(phase);
      cycles = 3.0 + 1.5 * gs_frand(&rng);
      amp = 1.4;
    } else {
      double fcg = fbody * pow(fmax / fbody, br);
      double oct = gs_tri(&rng) * (1.6 * br + 0.06);
      fw = fcg * pow(2.0, oct);
      if (fw < fbody * 0.8) fw = fbody * 0.8;
      if (fw > 16000.0) fw = 16000.0;
      phase = gs_frand(&rng);
      cycles = 6.0 + 4.0 * gs_frand(&rng);
      amp = 1.0;
    }
    double glen = cycles / fw;
    double lo = is_body ? 0.004 : 0.0015, hi = is_body ? 0.030 : 0.008;
    if (glen < lo) glen = lo;
    if (glen > hi) glen = hi;
    amp *= exp(-onset / amp_tau) * (0.75 + 0.5 * gs_frand(&rng)) * hit_amp;

    int gs0 = s0 + (int)(onset * SR);
    int glen_s = (int)(glen * SR);
    if (glen_s < 8) glen_s = 8;
    double inc = fw / SR, ph = phase;
    /* grains scatter slightly in the field around the part pan */
    double gpan = e->pan + 0.18 * gs_tri(&rng) * (1.0 - br * 0.5);
    for (int i = 0; i < glen_s && gs0 + i < bus->n; i++) {
      double xw = (double)(i + 1) / (double)(glen_s + 1);
      double sig = gs_win(xw) * sin(TAU * ph) * amp;
      bus_add(bus, gs0 + i, sig, gpan, gain);
      bus_add(send, gs0 + i, sig, gpan, sendg);
      ph += inc;
      if (ph >= 1.0) ph -= 1.0;
    }
  }
}

/* ════ CRACKLESNARE (STRETCHED) — chaotic wash (batch 2) ═════════════ */
/* Logistic-map spike train (May 1976 / Shaw's dripping faucet). Here the
   note LENGTH is the stretch: all time constants scale with e->beats, so
   a 6-beat note is a ~4 s crackle wash that traverses the bifurcation
   diagram in slow motion — chaos sputter condensing to a period-2 buzz.
   Adapted from cracklesnare.c (base window 1 s). */

typedef struct { double a1, a2, y1, y2, g; } Res2;

static void res_set(Res2 *r, double f, double tauR) {
  double fmaxr = 0.45 * SR;
  if (f > fmaxr) f = fmaxr;
  double th = TAU * f / SR;
  double R = exp(-1.0 / (tauR * SR));
  r->a1 = 2.0 * R * cos(th);
  r->a2 = -R * R;
  r->y1 = r->y2 = 0.0;
  double s = sin(th);
  r->g = (s < 1e-4) ? 1e-4 : s;
}
static double res_tick(Res2 *r, double x) {
  double y = r->a1 * r->y1 + r->a2 * r->y2 + x * r->g;
  r->y2 = r->y1; r->y1 = y;
  return y;
}

static uint64_t sm64(uint64_t z) {
  z += 0x9e3779b97f4a7c15ULL;
  z = (z ^ (z >> 30)) * 0xbf58476d1ce4e5b9ULL;
  z = (z ^ (z >> 27)) * 0x94d049bb133111ebULL;
  return z ^ (z >> 31);
}
static double h01_64(uint64_t h) { return (double)(h >> 11) * (1.0 / 9007199254740992.0); }

static void crackle_note(const Ev *e, Bus *bus, Bus *send, double gain, double sendg) {
  int s0 = (int)(e->beat * BEAT * SR);
  if (s0 >= bus->n) return;
  double S = e->beats * BEAT;          /* the stretch: window in seconds */
  if (S < 0.5) S = 0.5;
  int len = (int)(S * SR);
  if (s0 + len > bus->n) len = bus->n - s0;
  int fade = (int)(0.02 * S * SR) + 1;

  uint64_t seed = sm64(((uint64_t)e->seed << 32) ^ (uint64_t)(e->beat * 4096.0));
  double vel = e->vel;
  double amp = pow(vel, 1.5) * 0.8;

  double x = 0.15 + 0.70 * h01_64(sm64(seed ^ 0xA5A5A5A5ULL));
  double r_hi = 3.62 + 0.38 * vel;
  if (r_hi > 4.0) r_hi = 4.0;
  double rate0 = 1400.0 + 5200.0 * vel;
  /* stretched time constants (base window 1 s) */
  double t_crackle = (0.090 + 0.070 * vel) * S;
  double t_renv = 0.080 * S, t_rate = 0.055 * S;

  Res2 crack;
  double fcx = (2600.0 + 2600.0 * vel) * (0.85 + 0.30 * h01_64(sm64(seed ^ 0x51EEULL)));
  res_set(&crack, fcx, 0.0007);

  static const double body_ratio[3] = { 1.0, 1.593, 2.258 };
  static const double body_tau[3] = { 0.200, 0.110, 0.060 };
  static const double body_amp[3] = { 1.00, 0.62, 0.40 };
  double fbody = mtof(e->midi);
  double btau = S > 3.0 ? 3.0 : S; /* let the body ring with the stretch */
  Res2 body[3];
  for (int m = 0; m < 3; m++)
    res_set(&body[m], fbody * body_ratio[m], body_tau[m] * btau);

  double countdown = 0.0;
  uint64_t reseed = seed;

  for (int i = 0; i < len; i++) {
    double t = (double)i / SR;
    double envc = exp(-t / t_crackle);
    double inj = 0.0;
    countdown -= 1.0;
    if (countdown <= 0.0) {
      double r = 3.2 + (r_hi - 3.2) * exp(-t / t_renv);
      double xp = x;
      x = r * x * (1.0 - x);
      if (!(x > 1e-9 && x < 1.0 - 1e-9)) {
        reseed = sm64(reseed);
        x = 0.20 + 0.60 * h01_64(reseed);
      }
      double sgn = (x > xp) ? 1.0 : -1.0;
      double rate = rate0 * (0.10 + 0.90 * exp(-t / t_rate));
      countdown += (SR / rate) * (0.35 + 1.30 * x);
      inj = sgn * (0.15 + 0.85 * x) * envc;
    }
    if (i == 0) inj += 1.2;
    double cr = res_tick(&crack, inj);
    double bd = 0.0;
    for (int m = 0; m < 3; m++) bd += body_amp[m] * res_tick(&body[m], inj);
    double y = 1.15 * cr + 0.90 * bd + 0.35 * inj;
    if (i >= len - fade)
      y *= 0.5 * (1.0 + cos(M_PI * (double)(i - (len - fade)) / fade));
    bus_add(bus, s0 + i, y * amp, e->pan, gain);
    bus_add(send, s0 + i, y * amp, e->pan, sendg);
  }
}

static Part kickP, shufP, snareP, washP;

/* ── Schroeder reverb: mono send in, stereo out ─────────────────────── */
typedef struct { double *buf; int len, w; double fb, damp, lp; } Comb;
typedef struct { double *buf; int len, w; } Ap;

static void comb_init(Comb *c, int len, double fb, double damp) {
  c->buf = calloc((size_t)len, sizeof(double));
  c->len = len; c->w = 0; c->fb = fb; c->damp = damp; c->lp = 0;
}
static inline double comb_tick(Comb *c, double x) {
  double y = c->buf[c->w];
  c->lp = y * (1.0 - c->damp) + c->lp * c->damp;
  c->buf[c->w] = x + c->lp * c->fb;
  if (++c->w >= c->len) c->w = 0;
  return y;
}
static void ap_init(Ap *a, int len) {
  a->buf = calloc((size_t)len, sizeof(double));
  a->len = len; a->w = 0;
}
static inline double ap_tick(Ap *a, double x) {
  double b = a->buf[a->w];
  double y = -x + b;
  a->buf[a->w] = x + b * 0.5;
  if (++a->w >= a->len) a->w = 0;
  return y;
}

static void reverb(const Bus *send, Bus *out) {
  static const int clenL[4] = { 1557, 1617, 1491, 1422 };
  static const int clenR[4] = { 1601, 1667, 1533, 1474 };
  Comb cL[4], cR[4];
  Ap aL[2], aR[2];
  for (int k = 0; k < 4; k++) {
    comb_init(&cL[k], clenL[k], 0.82, 0.35);
    comb_init(&cR[k], clenR[k], 0.82, 0.35);
  }
  ap_init(&aL[0], 225); ap_init(&aL[1], 556);
  ap_init(&aR[0], 241); ap_init(&aR[1], 579);
  int pre = (int)(0.020 * SR);
  for (int i = 0; i < send->n; i++) {
    int j = i - pre;
    double xL = j >= 0 ? send->L[j] : 0.0;
    double xR = j >= 0 ? send->R[j] : 0.0;
    double sL = 0, sR = 0;
    for (int k = 0; k < 4; k++) { sL += comb_tick(&cL[k], xL); sR += comb_tick(&cR[k], xR); }
    sL = ap_tick(&aL[1], ap_tick(&aL[0], sL * 0.25));
    sR = ap_tick(&aR[1], ap_tick(&aR[0], sR * 0.25));
    out->L[i] += (float)sL;
    out->R[i] += (float)sR;
  }
  for (int k = 0; k < 4; k++) { free(cL[k].buf); free(cR[k].buf); }
  free(aL[0].buf); free(aL[1].buf); free(aR[0].buf); free(aR[1].buf);
}

/* ════ THE SCORE ═════════════════════════════════════════════════════ */
/* Quiet, emo, sad — with the mania living in tics, stammers and nervous
   subdivision, never in loudness. Each voice is introduced ALONE:
     I   0-16   frictus (back-left): the grief drone, a minor-3rd reveal
     II  16-32  scanner (mid-right): sparse broken arp with a stammer tic
     III 32-48  vosim (front-center): the sad melody, first statement
     IV  48-64  drums (center): restrained ghost shuffle
     V   64-96  the long build: everything ramps bar by bar
     VI  96-112 peak: melody up a 5th, babble undertow, full (still sad)
     VII 112-   collapse: sudden hush, last words, sputter to silence
   Harmony: cycle A = Dm Bb F C (the emo spiral), cycle B = Dm Bb Gm A
   (the A-major ache, C# against the D grief). One chord per bar. */

/* spatial corners: each voice owns a region of the stage */
#define PAN_FRICTUS (-0.55)
#define PAN_FRICTUS2 (-0.35)
#define PAN_SCANNER 0.42
#define PAN_VOSIM 0.0
#define PAN_KICK 0.0
#define PAN_SHUF (-0.18)
#define PAN_SNARE 0.18

static const int cycA[4] = { 50, 46, 41, 48 }; /* D3 Bb2 F2 C3 */
static const int minA[4] = { 1, 0, 0, 0 };
static const int cycB[4] = { 50, 46, 43, 45 }; /* D3 Bb2 G2 A2 */
static const int minB[4] = { 1, 0, 1, 0 };

static Part frictusP, scannerP, vosimP;

/* frictus bed for one 4-bar cycle: root + a sad companion (minor 3rd on
   minor bars, 5th on major), staggered so the bow strokes breathe */
static void bed_cycle(double c0, const int *roots, const int *mins, double vel) {
  for (int k = 0; k < 4; k++) {
    double b = c0 + 4.0 * k;
    emit(&frictusP, b, 3.6, roots[k], vel, PAN_FRICTUS);
    emit(&frictusP, b + 0.4, 3.2, roots[k] + (mins[k] ? 15 : 7), vel * 0.78, PAN_FRICTUS2);
  }
}

/* scanner arpeggio bar: 16th grid, nervous density (hash-gated rests),
   irregular 3+3+2+3+5 accents, pitch spread inside the right corner */
static void arp_bar(double b0, int root, int minor, double density, double velScale,
                    int barSeed) {
  static const int pat[16] = { 0, 3, 2, 5, 1, 4, 2, 0, 3, 5, 2, 4, 0, 2, 5, 3 };
  static const int acc[16] = { 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0 };
  int r = root + 12;
  int q3 = minor ? 3 : 4;
  int tones[6] = { r, r + q3, r + 7, r + 12, r + 12 + q3, r + 19 };
  for (int s = 0; s < 16; s++) {
    if (hash01((unsigned)(barSeed * 131 + s * 17 + 5)) > density) continue;
    double v = acc[s] ? 0.62 : (s % 2 ? 0.34 : 0.44);
    int m = tones[pat[s]];
    double pan = PAN_SCANNER - 0.14 + 0.30 * (m - 53) / 31.0; /* stay in corner */
    emit(&scannerP, b0 + 0.25 * s, 0.21, m, v * velScale, pan);
    /* the tic: rare 32nd double-strike stammer */
    if (acc[s] && hash01((unsigned)(barSeed * 37 + s)) > 0.82)
      emit(&scannerP, b0 + 0.25 * s + 0.125, 0.1, m, v * velScale * 0.55, pan);
  }
}

/* the sad melody: 16 beats over one cycle, tr semitones up, with tics */
static void melody(double v0, int tr, double velScale) {
  static const struct { double b, d; int m; double v; } ph[] = {
    /* bar 1 (Dm): rise to the flat six */
    { 0.00, 0.75, 57, 0.48 }, { 0.75, 1.25, 62, 0.60 },
    { 2.00, 0.50, 64, 0.50 }, { 2.50, 1.50, 65, 0.58 },
    /* bar 2 (Bb): the tic, then the fall */
    { 4.00, 0.25, 65, 0.52 }, { 4.25, 0.25, 65, 0.38 },
    { 4.50, 0.50, 64, 0.50 }, { 5.00, 1.00, 62, 0.55 }, { 6.00, 2.00, 58, 0.45 },
    /* bar 3 (F): reach, give up */
    { 8.00, 0.50, 57, 0.45 }, { 8.50, 0.50, 60, 0.50 }, { 9.00, 1.50, 65, 0.62 },
    { 10.50, 0.50, 64, 0.50 }, { 11.00, 1.00, 62, 0.52 },
    /* bar 4 (C or A): the long ache */
    { 12.00, 0.75, 64, 0.50 }, { 12.75, 0.25, 60, 0.40 }, { 13.00, 2.60, 62, 0.60 },
  };
  for (size_t i = 0; i < sizeof ph / sizeof ph[0]; i++)
    emit(&vosimP, v0 + ph[i].b, ph[i].d, ph[i].m + tr, ph[i].v * velScale, PAN_VOSIM);
}

/* the babble undertow: hushed deterministic 16th chatter around D4 —
   the robot talking to itself under the music */
static void babble(double b0, double beats, double density, double vel) {
  static const int scale[6] = { 57, 60, 62, 64, 65, 69 };
  int steps = (int)(beats * 4.0);
  for (int s = 0; s < steps; s++) {
    unsigned hs = (unsigned)(b0 * 64.0) + (unsigned)s * 29u;
    if (hash01(hs * 2654435761u) > density) continue;
    int m = scale[(int)(hash01(hs ^ 0xbabb1eu) * 6.0) % 6];
    double v = vel * (0.7 + 0.5 * hash01(hs ^ 0x51u));
    emit(&vosimP, b0 + 0.25 * s, 0.18, m, v, PAN_VOSIM + 0.12 * (hash01(hs) - 0.5));
  }
}

/* one bar of drums; intensity 0..1 scales density, velocity and rolls */
static void drums_bar(double b0, double t, int roll) {
  emit(&kickP, b0 + 0.0, 0.25, 36, 0.55 + 0.45 * t, PAN_KICK);
  emit(&kickP, b0 + 2.5, 0.25, 36, 0.45 + 0.40 * t, PAN_KICK);
  if (t > 0.7) emit(&kickP, b0 + 3.75, 0.25, 36, 0.35 + 0.35 * (t - 0.7), PAN_KICK);
  emit(&shufP, b0 + 1.75, 0.25, 36, 0.30 + 0.25 * t, PAN_SHUF);
  emit(&shufP, b0 + 3.25, 0.25, 36, 0.24 + 0.22 * t, PAN_SHUF);
  if (t > 0.4) emit(&shufP, b0 + 0.75, 0.25, 36, 0.18 + 0.25 * (t - 0.4), PAN_SHUF - 0.06);
  emit(&snareP, b0 + 1.0, 0.25, 50, 0.42 + 0.35 * t, PAN_SNARE);
  emit(&snareP, b0 + 3.0, 0.25, 50, 0.46 + 0.38 * t, PAN_SNARE);
  if (roll) /* six-16th roll climbing into the next bar */
    for (int s = 0; s < 6; s++)
      emit(&snareP, b0 + 2.5 + 0.25 * s, 0.2, 50, 0.20 + 0.10 * s + 0.2 * t,
           PAN_SNARE + 0.04 * s);
}

static void compose(void) {
  /* I — frictus alone: grief drone, then the minor-3rd reveal */
  emit(&frictusP, 0.0, 10.0, 38, 0.42, PAN_FRICTUS);          /* D2 */
  emit(&frictusP, 10.0, 5.5, 50, 0.38, PAN_FRICTUS);          /* D3 */
  emit(&frictusP, 10.4, 5.0, 53, 0.32, PAN_FRICTUS2);         /* F3: the reveal */

  /* II — scanner alone (frictus whispers underneath): a broken, sparse
     arp finding its footing, one stammer tic */
  emit(&frictusP, 16.0, 7.0, 50, 0.28, PAN_FRICTUS);
  emit(&frictusP, 24.0, 7.0, 46, 0.28, PAN_FRICTUS);
  emit(&scannerP, 16.0, 0.6, 62, 0.35, PAN_SCANNER);
  emit(&scannerP, 17.0, 0.6, 69, 0.40, PAN_SCANNER + 0.08);
  emit(&scannerP, 18.5, 0.6, 65, 0.36, PAN_SCANNER - 0.06);
  emit(&scannerP, 20.0, 0.9, 74, 0.44, PAN_SCANNER + 0.12);
  emit(&scannerP, 21.5, 0.5, 69, 0.34, PAN_SCANNER);
  emit(&scannerP, 22.5, 0.5, 65, 0.30, PAN_SCANNER - 0.08);
  emit(&scannerP, 24.0, 0.6, 62, 0.38, PAN_SCANNER);
  emit(&scannerP, 25.0, 0.6, 70, 0.42, PAN_SCANNER + 0.10);  /* Bb4 */
  emit(&scannerP, 26.0, 0.25, 74, 0.40, PAN_SCANNER + 0.12); /* the tic */
  emit(&scannerP, 26.25, 0.25, 74, 0.26, PAN_SCANNER + 0.12);
  emit(&scannerP, 27.5, 0.6, 70, 0.34, PAN_SCANNER);
  emit(&scannerP, 29.0, 1.2, 65, 0.36, PAN_SCANNER - 0.05);
  emit(&scannerP, 31.0, 1.0, 62, 0.30, PAN_SCANNER);

  /* III — vosim alone over a hushed bed: first statement of the melody */
  bed_cycle(32.0, cycA, minA, 0.30);
  for (int bar = 0; bar < 4; bar++)
    arp_bar(32.0 + 4.0 * bar, cycA[bar], minA[bar], 0.30, 0.75, bar + 800);
  melody(32.0, 0, 0.85);

  /* IV — drums introduced: restrained ghost shuffle, voice rests
     (one held sigh), the A-major ache appears */
  bed_cycle(48.0, cycB, minB, 0.34);
  for (int bar = 0; bar < 4; bar++) {
    arp_bar(48.0 + 4.0 * bar, cycB[bar], minB[bar], 0.42, 0.80, bar + 900);
    drums_bar(48.0 + 4.0 * bar, 0.15, 0);
  }
  emit(&vosimP, 60.0, 3.0, 62, 0.40, PAN_VOSIM);              /* the sigh */

  /* V — the long build: 8 bars, everything ramps bar by bar */
  bed_cycle(64.0, cycA, minA, 0.38);
  bed_cycle(80.0, cycB, minB, 0.46);
  melody(64.0, 0, 1.0);
  melody(80.0, 0, 1.05);
  for (int bar = 0; bar < 8; bar++) {
    double t = (double)bar / 7.0;
    const int *cy = bar < 4 ? cycA : cycB;
    const int *mi = bar < 4 ? minA : minB;
    arp_bar(64.0 + 4.0 * bar, cy[bar % 4], mi[bar % 4],
            0.45 + 0.45 * t, 0.85 + 0.25 * t, bar + 100);
    drums_bar(64.0 + 4.0 * bar, 0.25 + 0.60 * t, bar == 3 || bar == 7);
  }
  babble(80.0, 16.0, 0.22, 0.26);                             /* undertow in */
  emit(&washP, 72.0, 2.0, 50, 0.45, 0.5);                     /* small eruption */
  emit(&washP, 88.0, 3.0, 50, 0.60, -0.5);
  emit(&scannerP, 95.0, 0.5, 77, 0.80, PAN_SCANNER + 0.15);   /* F5 cry, door */

  /* VI — peak: melody up a 5th, babble dense, still grieving */
  bed_cycle(96.0, cycB, minB, 0.55);
  melody(96.0, 7, 1.1);
  babble(96.0, 16.0, 0.42, 0.34);
  for (int bar = 0; bar < 4; bar++) {
    arp_bar(96.0 + 4.0 * bar, cycB[bar], minB[bar], 0.95, 1.1, bar + 200);
    drums_bar(96.0 + 4.0 * bar, 1.0, bar == 3);
  }
  emit(&washP, 96.0, 4.0, 50, 0.85, 0.4);

  /* VII — collapse: sudden hush, last words, stumble, sputter out */
  emit(&frictusP, 112.0, 9.0, 50, 0.40, PAN_FRICTUS);
  emit(&frictusP, 112.4, 8.2, 53, 0.32, PAN_FRICTUS2);
  emit(&vosimP, 114.0, 0.25, 62, 0.45, PAN_VOSIM);            /* stammer... */
  emit(&vosimP, 114.25, 0.25, 62, 0.34, PAN_VOSIM);
  emit(&vosimP, 114.5, 0.25, 62, 0.26, PAN_VOSIM);
  emit(&vosimP, 116.0, 4.0, 62, 0.45, PAN_VOSIM);             /* ...the word */
  emit(&vosimP, 121.0, 5.0, 57, 0.34, PAN_VOSIM);
  emit(&kickP, 116.0, 0.25, 36, 0.50, PAN_KICK);              /* stumbles */
  emit(&kickP, 120.75, 0.25, 36, 0.38, PAN_KICK);
  emit(&kickP, 126.0, 0.25, 36, 0.44, PAN_KICK);
  emit(&snareP, 118.0, 0.2, 50, 0.28, PAN_SNARE);
  emit(&snareP, 127.0, 0.2, 50, 0.30, PAN_SNARE);
  emit(&scannerP, 120.0, 0.8, 69, 0.30, PAN_SCANNER);
  emit(&scannerP, 124.0, 0.8, 65, 0.28, PAN_SCANNER - 0.06);
  emit(&scannerP, 128.0, 3.0, 62, 0.30, PAN_SCANNER);
  emit(&frictusP, 124.0, 10.0, 38, 0.40, PAN_FRICTUS);        /* open fifth */
  emit(&frictusP, 124.5, 9.0, 45, 0.30, PAN_FRICTUS2);        /* no third: done */
  emit(&washP, 128.0, 8.0, 38, 0.42, -0.3);                   /* dying sputter */
}

/* ════ MIX + MASTER + WAV ════════════════════════════════════════════ */

static int write_wav(const char *path, const float *L, const float *R, int n) {
  FILE *f = fopen(path, "wb");
  if (!f) { fprintf(stderr, "novelette: cannot open %s\n", path); return 1; }
  uint32_t data = (uint32_t)n * 4, chunk = 36 + data, fmtlen = 16, sr = SR, br = SR * 4;
  uint16_t pcm = 1, ch = 2, bits = 16, block = 4;
  fwrite("RIFF", 1, 4, f); fwrite(&chunk, 4, 1, f); fwrite("WAVE", 1, 4, f);
  fwrite("fmt ", 1, 4, f); fwrite(&fmtlen, 4, 1, f);
  fwrite(&pcm, 2, 1, f); fwrite(&ch, 2, 1, f);
  fwrite(&sr, 4, 1, f); fwrite(&br, 4, 1, f);
  fwrite(&block, 2, 1, f); fwrite(&bits, 2, 1, f);
  fwrite("data", 1, 4, f); fwrite(&data, 4, 1, f);
  for (int i = 0; i < n; i++) {
    double l = L[i], r = R[i];
    if (l > 1) l = 1; if (l < -1) l = -1;
    if (r > 1) r = 1; if (r < -1) r = -1;
    int16_t s[2] = { (int16_t)lrint(l * 32767.0), (int16_t)lrint(r * 32767.0) };
    fwrite(s, 2, 2, f);
  }
  fclose(f);
  return 0;
}

int main(int argc, char **argv) {
  const char *outPath = "out/novelette.wav";
  for (int i = 1; i < argc; i++)
    if (!strcmp(argv[i], "--out") && i + 1 < argc) outPath = argv[++i];

  compose();
  int n = (int)(TRACK_BEATS * BEAT * SR);
  Bus master = bus_new(n), send = bus_new(n);

  /* staging: frictus deepest in the reverb (back-left), scanner mid-wet
     (mid-right), vosim nearly dry (front-center), drums dry and anchored,
     washes the wettest thing in the room */
  nz_state = 0x9d2c5681u; /* deterministic renders */
  fprintf(stderr, "novelette: frictus %d notes\n", frictusP.n);
  for (int i = 0; i < frictusP.n; i++) frictus_note(&frictusP.ev[i], &master, &send, 0.46, 0.62);
  fprintf(stderr, "novelette: scanner %d notes\n", scannerP.n);
  for (int i = 0; i < scannerP.n; i++) scanner_note(&scannerP.ev[i], &master, &send, 0.60, 0.32);
  fprintf(stderr, "novelette: vosim %d notes\n", vosimP.n);
  for (int i = 0; i < vosimP.n; i++) vosim_note(&vosimP.ev[i], &master, &send, 0.95, 0.12);
  fprintf(stderr, "novelette: memkick %d / cavikick %d / gransnare %d / washes %d\n",
          kickP.n, shufP.n, snareP.n, washP.n);
  for (int i = 0; i < kickP.n; i++) memkick_note(&kickP.ev[i], &master, &send, 0.85, 0.04);
  for (int i = 0; i < shufP.n; i++) cavikick_note(&shufP.ev[i], &master, &send, 0.65, 0.10);
  for (int i = 0; i < snareP.n; i++) gransnare_note(&snareP.ev[i], &master, &send, 0.70, 0.18);
  for (int i = 0; i < washP.n; i++) crackle_note(&washP.ev[i], &master, &send, 0.42, 0.50);

  fprintf(stderr, "novelette: reverb\n");
  reverb(&send, &master);

  /* master: gentle tanh glue, then peak-normalize to -1 dBFS */
  double peak = 0;
  for (int i = 0; i < n; i++) {
    master.L[i] = (float)(tanh(master.L[i] * 1.1) / 1.1);
    master.R[i] = (float)(tanh(master.R[i] * 1.1) / 1.1);
    double a = fabs(master.L[i]);
    if (a > peak) peak = a;
    a = fabs(master.R[i]);
    if (a > peak) peak = a;
  }
  if (peak > 0) {
    double g = pow(10.0, -1.0 / 20.0) / peak;
    for (int i = 0; i < n; i++) { master.L[i] *= (float)g; master.R[i] *= (float)g; }
  }

  if (write_wav(outPath, master.L, master.R, n)) return 1;
  fprintf(stderr, "novelette: wrote %s (%.1fs, peak %.3f)\n", outPath, (double)n / SR, peak);
  return 0;
}
