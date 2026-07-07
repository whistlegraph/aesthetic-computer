// goatshard.c — high-energy club for the blubclub. The name is the brief:
// GOAT = a real self-oscillating two-mass vocal-fold creature (novelizer
// batch-1 twomass) growling and bleating the lead; SHARD = glassy splinters
// from chaosfm bifurcation stabs and frictus bowed-metal ticks. Battery is
// the batch-2 percussion winners (memkick / implokick / cavikick /
// wiresnare / gransnare / stretched cracklesnare washes).
//
// SUBSTRATE MATERIALITY, ALL THE TRICKS:
//   - carved-noise residue (the nullnoise law: wet-minus-dry through a
//     peaking bell — the noise only EXISTS where carved), pink + velvet
//     groups, riser glides into each drop, a pump bell breathing with the
//     sidechain
//   - tape print on the master (americomputadora): tanh drive + tube bias
//     + hiss floor; FULL print in the intro (lo-fi), LIFTED at the drops
//     (the room turns real), settling partial in the outro
//   - sidechain pump: every kick ducks the whole music bus (12 ms dive,
//     ~130 ms recovery), drums ride their own dry bus
//   - crunch zone (bitcrush + self-flange) late in drop 2, self-scratch
//     gestures (the playhead dragged through the mix's own past)
//   - width story: narrow intro -> wide drops -> corridor breakdown
//
// Voice cores are adapted from pop/novelizer/c/voices/*.c and
// pop/novelette/c/novelette.c (same DSP, same citations — see those
// headers), reshaped into per-note stereo bus writers. twomass renders as
// ONE continuous monophonic throat over the whole track, so repeated
// short notes re-articulate a living larynx (that is the bleat).
//
// Form: G minor, 150 BPM, ~1:58 + ring-out.
//   intro -> build 1 -> DROP 1 -> breakdown (the goat sings clean) ->
//   build 2 -> DROP 2 (+5, C minor, implokick layer, crunch) -> outro
//
// Build:  ./build.sh      Render: ./build/goatshard [--out path.wav]

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
static const double BPM = 150.0;
#define BEAT (60.0 / BPM)                /* 0.4 s */
static const double TRACK_BEATS = 264.0; /* 256 composed + ring-out */

/* section doors, in beats (4-beat bars) — classic song structure:
   intro / verse / pre-chorus / CHORUS / verse 2 / bridge / build /
   FINAL CHORUS / outro. Voices are introduced one at a time and the
   full band only exists in the choruses. */
#define S_VERSE1 32.0
#define S_PRE 64.0
#define S_CHOR1 80.0
#define S_VERSE2 112.0
#define S_BRIDGE 144.0
#define S_BUILD2 160.0
#define S_CHOR2 176.0
#define S_OUTRO 224.0

static inline double mtof(double m) { return 440.0 * pow(2.0, (m - 69.0) / 12.0); }

/* deterministic per-note hash -> 0..1 (shared by all voices) */
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

#define MAXEV 4096
typedef struct { Ev ev[MAXEV]; int n; } Part;

/* the grid spine stays machine-tight; everything else gets the eager
   human drive below (tentative defs — the full roster lives mid-file) */
static Part kickP, impP, bassP, washP;

static void emit(Part *p, double beat, double beats, double midi, double vel, double pan) {
  if (p->n >= MAXEV) { fprintf(stderr, "goatshard: part overflow\n"); exit(1); }
  Ev *e = &p->ev[p->n];
  int tight = (p == &kickP || p == &impP || p == &bassP || p == &washP);
  if (!tight) {
    /* eager drive: players lean a hair AHEAD of the grid (up to 16 ms
       early, rarely late), and no two hits land at the same force */
    unsigned hh = (unsigned)(beat * 96.0) ^ (unsigned)(midi * 13.0)
                ^ ((unsigned)p->n * 101u);
    beat -= 0.040 * hash01(hh);
    beat += 0.012 * hash01(hh ^ 0x9e3779b9u);
    if (beat < 0.0) beat = 0.0;
    vel *= 0.95 + 0.09 * hash01(hh ^ 0x51u);
    if (vel > 1.0) vel = 1.0;
  }
  e->beat = beat; e->beats = beats; e->midi = midi; e->vel = vel; e->pan = pan;
  e->seed = p->n * 2654435761u % 100000 + 7;
  p->n++;
}

/* ── stereo bus ─────────────────────────────────────────────────────── */
typedef struct { float *L, *R; int n; } Bus;

static Bus bus_new(int n) {
  Bus b = { calloc((size_t)n, sizeof(float)), calloc((size_t)n, sizeof(float)), n };
  if (!b.L || !b.R) { fprintf(stderr, "goatshard: alloc failed\n"); exit(1); }
  return b;
}

static inline void bus_add(Bus *b, int i, double sig, double pan, double gain) {
  if (i < 0 || i >= b->n) return;
  double a = 0.25 * M_PI * (pan + 1.0) * 0.5; /* constant-power */
  b->L[i] += (float)(sig * gain * cos(a) * 1.414);
  b->R[i] += (float)(sig * gain * sin(a) * 1.414);
}

static uint32_t nz_state = 0x9d2c5681u;
static inline double nz(void) {
  nz_state ^= nz_state << 13; nz_state ^= nz_state >> 17; nz_state ^= nz_state << 5;
  return (double)(int32_t)nz_state * (1.0 / 2147483648.0);
}

/* ════ MEMKICK — tension-modulated membrane kick (batch 2) ═══════════ */
/* Fletcher & Rossing Bessel mode stack; pitch drop is emergent from
   displacement-energy tension modulation. */

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

/* ════ IMPLOKICK — chaotic-collapse feedback-FM kick (batch 2) ═══════ */
/* Born deep in the chaotic regime (β≈2-3), β collapses to zero while the
   operator frequency implodes onto the note: noise -> tone -> sub in one
   gesture, no separate noise source anywhere. */

static void implokick_note(const Ev *e, Bus *bus, Bus *send, double gain, double sendg) {
  int s0 = (int)(e->beat * BEAT * SR);
  if (s0 >= bus->n) return;
  double freq = mtof(e->midi), vel = e->vel;
  int len = (int)(0.38 * SR);
  int fade = (int)(0.020 * SR);
  int atk = (int)(0.0008 * SR);
  if (atk < 1) atk = 1;

  unsigned h = (unsigned)(e->seed * 2654435761u) ^ (unsigned)(freq * 97.0);
  double jbeta = 0.92 + 0.16 * hash01(h);
  double jratio = 0.95 + 0.10 * hash01(h ^ 0x51ed270bu);

  double beta0 = (1.7 + 1.3 * vel) * jbeta / (1.0 + freq / 1500.0);
  double ratio = (2.0 + 1.5 * vel) * jratio;
  double tau_b = 0.018 + 0.032 * vel;
  double tau_p = 0.009 + 0.011 * vel;
  double tau_a = 0.045 + 0.050 * vel;
  double amp = pow(vel, 1.6);

  double phase = 0.0, y1 = 0.0, y2 = 0.0, lp = 0.0;
  double dc_x1 = 0.0, dc_y1 = 0.0;

  for (int i = 0; i < len && s0 + i < bus->n; i++) {
    double t = (double)i / SR;
    double beta = beta0 * exp(-t / tau_b);
    double f = freq * (1.0 + (ratio - 1.0) * exp(-t / tau_p));
    double inc = f / (2.0 * SR);

    double s = 0.0; /* 2x oversampled Tomisawa operator */
    for (int k = 0; k < 2; k++) {
      lp += 0.5 * (0.5 * (y1 + y2) - lp);
      double y = sin(TAU * phase + beta * lp);
      y2 = y1; y1 = y;
      phase += inc;
      if (phase >= 1.0) phase -= 1.0;
      s += y;
    }
    s *= 0.5;

    double o = s - dc_x1 + 0.999 * dc_y1;
    dc_x1 = s; dc_y1 = o;

    double env = (i < atk) ? (double)i / atk : exp(-(t - 0.0008) / tau_a);
    if (i > len - fade) {
      double r = (double)(i - (len - fade)) / fade;
      env *= 0.5 * (1.0 + cos(M_PI * r));
    }
    double v = o * env * amp * 0.85;
    bus_add(bus, s0 + i, v, e->pan, gain);
    bus_add(send, s0 + i, v, e->pan, sendg);
  }
}

/* ════ CAVIKICK — Helmholtz-port kick (batch 2), the offbeat ghost ═══ */

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

/* ════ WIRESNARE — membrane + snare-wire contact model (batch 2) ═════ */
/* No noise generator: the rattle is 12 wires chattering against 7 head
   modes through impulsive restitution collisions (Rossing; Bilbao). */

#define WS_NMODES 7
#define WS_NWIRES 12
static const double ws_ratio[WS_NMODES] = { 1.0, 1.593, 2.135, 2.295, 2.653, 2.917, 3.155 };
static const double ws_gain[WS_NMODES] = { 1.0, 0.62, 0.46, 0.40, 0.31, 0.26, 0.21 };
static const double ws_strike[WS_NMODES] = { 1.0, 0.92, 0.85, 0.80, 0.74, 0.66, 0.58 };

static uint64_t sm64(uint64_t x) {
  x += 0x9E3779B97F4A7C15ULL;
  x = (x ^ (x >> 30)) * 0xBF58476D1CE4E5B9ULL;
  x = (x ^ (x >> 27)) * 0x94D049BB133111EBULL;
  return x ^ (x >> 31);
}
static double h01_64(uint64_t seed, int slot) {
  return (double)(sm64(seed + (uint64_t)slot * 0x632BE59BD9B4E019ULL) >> 11)
         / 9007199254740992.0;
}

static void wiresnare_note(const Ev *e, Bus *bus, Bus *send, double gain, double sendg) {
  int s0 = (int)(e->beat * BEAT * SR);
  if (s0 >= bus->n) return;
  int len = (int)(0.90 * SR);
  if (s0 + len > bus->n) len = bus->n - s0;
  int fade = (int)(0.010 * SR);

  double f0 = mtof(e->midi);
  while (f0 < 120.0) f0 *= 2.0;

  uint64_t seed = sm64((uint64_t)e->seed * 0x100000001B3ULL ^ (uint64_t)(f0 * 1024.0));

  struct { double cr, ci, rw, iw; } md[WS_NMODES];
  for (int k = 0; k < WS_NMODES; k++) {
    double jit = 1.0 + 0.012 * (h01_64(seed, 100 + k) - 0.5);
    double fk = f0 * ws_ratio[k] * jit;
    if (fk > 0.45 * SR) fk = 0.45 * SR;
    double tauk = 0.090 * pow(ws_ratio[k], -0.6);
    double r = exp(-1.0 / (tauk * SR));
    double w = TAU * fk / SR;
    md[k].cr = md[k].ci = 0.0;
    md[k].rw = r * cos(w);
    md[k].iw = r * sin(w);
  }

  struct { double pos, vel, k2, damp, rest; } wr[WS_NWIRES];
  for (int i = 0; i < WS_NWIRES; i++) {
    double u = ((double)i + h01_64(seed, 200 + i)) / WS_NWIRES;
    double fw = 320.0 * pow(1750.0 / 320.0, u);
    double ph = TAU * fw / SR;
    wr[i].k2 = ph * ph;
    wr[i].damp = exp(-1.0 / (0.060 * SR));
    wr[i].rest = -(0.04 + 0.26 * h01_64(seed, 300 + i));
    wr[i].pos = wr[i].rest;
    wr[i].vel = 0.0;
  }

  int sl = (int)(0.001 * SR);
  if (sl < 4) sl = 4;
  double force = pow(e->vel, 1.1);
  double amp = pow(e->vel, 1.15);

  double inj = 0.0, hp_lp = 0.0, dc_x1 = 0.0, dc_y1 = 0.0, hprev = 0.0;

  for (int i = 0; i < len; i++) {
    double strike = 0.0;
    if (i < sl)
      strike = -force * (2.0 / sl) * 0.5 * (1.0 - cos(TAU * i / sl));

    double hd = 0.0;
    for (int k = 0; k < WS_NMODES; k++) {
      double in = strike * ws_strike[k] + inj * 0.02;
      double nr = md[k].cr * md[k].rw - md[k].ci * md[k].iw;
      double ni = md[k].cr * md[k].iw + md[k].ci * md[k].rw;
      md[k].cr = nr;
      md[k].ci = ni + in;
      hd += ws_gain[k] * md[k].ci;
    }

    double hvel = hd - hprev;
    hprev = hd;
    inj = 0.0;
    double click = 0.0;
    for (int w = 0; w < WS_NWIRES; w++) {
      wr[w].vel += -wr[w].k2 * (wr[w].pos - wr[w].rest);
      wr[w].vel *= wr[w].damp;
      wr[w].pos += wr[w].vel;
      double p = wr[w].pos - hd;
      if (p > 0.0) {
        double vrel = wr[w].vel - hvel;
        if (vrel > 0.0) {
          double j = 1.5 * vrel; /* (1+e)·v_rel, e = 0.5 */
          if (j > 1.0) j = 1.0;
          wr[w].vel = hvel - 0.5 * vrel;
          inj += j;
          click += j;
        }
        wr[w].pos = hd;
      }
      if (wr[w].pos > 5.0) wr[w].pos = 5.0;
      else if (wr[w].pos < -5.0) wr[w].pos = -5.0;
      if (wr[w].vel > 3.0) wr[w].vel = 3.0;
      else if (wr[w].vel < -3.0) wr[w].vel = -3.0;
    }

    hp_lp += 0.12 * (click - hp_lp);
    double snap = click - hp_lp;
    double xx = 0.70 * hd + 4.0 * snap;

    double y = xx - dc_x1 + 0.9995 * dc_y1;
    dc_x1 = xx; dc_y1 = y;

    double g = amp * 0.35;
    if (i > len - fade) g *= (double)(len - i) / fade;
    bus_add(bus, s0 + i, y * g, e->pan, gain);
    bus_add(send, s0 + i, y * g, e->pan, sendg);
  }
}

/* ════ GRANSNARE — grain-cloud clap layer (batch 2) ══════════════════ */

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

/* ════ CRACKLESNARE (STRETCHED) — chaotic wash riser (batch 2) ═══════ */
/* Logistic-map spike train; note LENGTH scales every time constant, so a
   4-beat note is a ~1.6 s crackle wash traversing the bifurcation diagram
   in slow motion — the section-door riser of this track. */

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

static double h01u(uint64_t h) { return (double)(sm64(h) >> 11) * (1.0 / 9007199254740992.0); }

static void crackle_note(const Ev *e, Bus *bus, Bus *send, double gain, double sendg) {
  int s0 = (int)(e->beat * BEAT * SR);
  if (s0 >= bus->n) return;
  double S = e->beats * BEAT;
  if (S < 0.5) S = 0.5;
  int len = (int)(S * SR);
  if (s0 + len > bus->n) len = bus->n - s0;
  int fade = (int)(0.02 * S * SR) + 1;

  uint64_t seed = sm64(((uint64_t)e->seed << 32) ^ (uint64_t)(e->beat * 4096.0));
  double vel = e->vel;
  double amp = pow(vel, 1.5) * 0.8;

  double x = 0.15 + 0.70 * h01u(seed ^ 0xA5A5A5A5ULL);
  double r_hi = 3.62 + 0.38 * vel;
  if (r_hi > 4.0) r_hi = 4.0;
  double rate0 = 1400.0 + 5200.0 * vel;
  double t_crackle = (0.090 + 0.070 * vel) * S;
  double t_renv = 0.080 * S, t_rate = 0.055 * S;

  Res2 crack;
  double fcx = (2600.0 + 2600.0 * vel) * (0.85 + 0.30 * h01u(seed ^ 0x51EEULL));
  res_set(&crack, fcx, 0.0007);

  static const double body_ratio[3] = { 1.0, 1.593, 2.258 };
  static const double body_tau[3] = { 0.200, 0.110, 0.060 };
  static const double body_amp[3] = { 1.00, 0.62, 0.40 };
  double fbody = mtof(e->midi);
  double btau = S > 3.0 ? 3.0 : S;
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
        x = 0.20 + 0.60 * h01u(reseed);
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

/* ════ CHAOSFM — feedback-FM bifurcation stabs + rolling bass ════════ */
/* Tomisawa averaged-feedback operator; β blooms sine -> saw -> chaos ->
   back per note. Velocity rides the bifurcation depth: bass notes at
   vel ~0.5 are growling saws, stab chords at vel ~0.9 shatter. */

typedef struct {
  double phase, inc, y1, y2, lp;
} FbOp;

static void fbop_init(FbOp *o, double freq) {
  o->phase = 0.0;
  o->inc = freq / SR;
  o->y1 = o->y2 = o->lp = 0.0;
}

static double fbop_tick(FbOp *o, double beta) {
  o->lp += 0.5 * (0.5 * (o->y1 + o->y2) - o->lp);
  double y = sin(TAU * o->phase + beta * o->lp);
  o->y2 = o->y1;
  o->y1 = y;
  o->phase += o->inc;
  if (o->phase >= 1.0) o->phase -= 1.0;
  return y;
}

static void chaosfm_note(const Ev *e, Bus *bus, Bus *send, double gain, double sendg) {
  double freq = mtof(e->midi), dur = e->beats * BEAT;
  int s0 = (int)(e->beat * BEAT * SR);
  if (s0 >= bus->n) return;
  int hold = (int)(dur * SR);
  int rel = (int)(0.060 * SR);
  int len = hold + rel;
  int atk = (int)(0.005 * SR);
  if (atk < 1) atk = 1;

  double amp = pow(e->vel, 1.3);
  double beta_peak = (1.0 + 2.0 * e->vel) / (1.0 + freq / 2600.0);
  if (beta_peak < 0.35) beta_peak = 0.35;

  FbOp op1, op2, anchor;
  fbop_init(&op1, freq);
  fbop_init(&op2, freq * 1.00208);
  fbop_init(&anchor, freq);

  double dc_x1 = 0.0, dc_y1 = 0.0;

  for (int i = 0; i < len && s0 + i < bus->n; i++) {
    double t = (double)i / SR;
    double u = t / dur;
    if (u > 1.0) u = 1.0;
    double s = pow(sin(M_PI * u), 1.1);
    double beta = 0.35 + (beta_peak - 0.35) * s;

    double x = fbop_tick(&op1, beta) + 0.45 * fbop_tick(&op2, beta * 0.85);
    x /= 1.45;
    x += 0.12 * sin(TAU * anchor.phase);
    anchor.phase += anchor.inc;
    if (anchor.phase >= 1.0) anchor.phase -= 1.0;

    double y = x - dc_x1 + 0.9995 * dc_y1;
    dc_x1 = x; dc_y1 = y;

    double env;
    if (i < atk) env = (double)i / atk;
    else env = exp(-1.2 * (t - 0.005) / (dur + 0.060));
    if (i >= hold) {
      double r = (double)(i - hold) / rel;
      env *= 0.5 * (1.0 + cos(M_PI * r));
    }
    double v = y * env * amp * 0.5;
    bus_add(bus, s0 + i, v, e->pan, gain);
    bus_add(send, s0 + i, v, e->pan, sendg);
  }
}

/* ════ FRICTUS — banded-waveguide bowed metal (batch 1) ══════════════ */
/* Long low notes = the breathing drone; short high notes = glass ticks
   (the bow-bite attack IS the shard). */

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

/* ════ THROATBASS — sub sine + formant growl ═════════════════════════ */
/* A clean phase-increment sine holds the actual fundamental (the floor)
   while a Tomisawa feedback operator an octave up talks through two
   gliding formant bandpasses — the mouth closes over each note
   (wah -> oh). Lower AND throatier than a bare FM bass. */

static void bp_move(BiQuad *q, double f, double Q) { /* retune, keep state */
  double w0 = TAU * f / SR, alpha = sin(w0) / (2.0 * Q), a0 = 1.0 + alpha;
  q->b0 = alpha / a0; q->b2 = -alpha / a0;
  q->a1 = -2.0 * cos(w0) / a0; q->a2 = (1.0 - alpha) / a0;
}

static void throatbass_note(const Ev *e, Bus *bus, Bus *send, double gain, double sendg) {
  double f0 = mtof(e->midi), dur = e->beats * BEAT;
  int s0 = (int)(e->beat * BEAT * SR);
  if (s0 >= bus->n) return;
  int hold = (int)(dur * SR), rel = (int)(0.030 * SR);
  int len = hold + rel;
  int atk = (int)(0.004 * SR);
  if (atk < 1) atk = 1;
  double vel = e->vel;

  FbOp op;
  fbop_init(&op, f0 * 2.0);
  BiQuad F1b, F2b;
  bp_set(&F1b, 420.0, 5.0);
  bp_set(&F2b, 1050.0, 5.0);
  double ph = 0.0, amp = pow(vel, 1.2);

  for (int i = 0; i < len && s0 + i < bus->n; i++) {
    double t = (double)i / SR;
    double u = t / (dur + 1e-9);
    if (u > 1.0) u = 1.0;
    if ((i & 31) == 0) { /* the mouth closes over the note */
      bp_move(&F1b, 420.0 - 210.0 * u, 5.0);
      bp_move(&F2b, 1050.0 - 480.0 * u, 5.0);
    }
    double beta = (0.9 + 1.1 * vel) * exp(-t / 0.5);
    double g = fbop_tick(&op, beta);
    double growl = 1.7 * bp_tick(&F1b, g) + 1.2 * bp_tick(&F2b, g);
    double sub = sin(TAU * ph);
    ph += f0 / SR;
    if (ph >= 1.0) ph -= 1.0;
    sub = tanh(1.6 * sub) / tanh(1.6);
    double env;
    if (i < atk) env = (double)i / atk;
    else if (i < hold) env = 1.0;
    else env = 0.5 * (1.0 + cos(M_PI * (double)(i - hold) / rel));
    double v = (0.85 * sub + 0.55 * growl) * env * amp;
    bus_add(bus, s0 + i, v, e->pan, gain);
    bus_add(send, s0 + i, v, e->pan, sendg);
  }
}

/* ════ GONGBELL — two-modulator FM gong ══════════════════════════════ */
/* Chowning FM with an inharmonic modulator pair (1.4x clang + 3.53x
   strike glitter, indices decaying at different rates) over a beating
   carrier pair; the tone SWELLS after the mallet like a real gong and
   rings for seconds. Low notes are the gong, high notes the bell. */

static void gongbell_note(const Ev *e, Bus *bus, Bus *send, double gain, double sendg) {
  double f0 = mtof(e->midi), vel = e->vel;
  int s0 = (int)(e->beat * BEAT * SR);
  if (s0 >= bus->n) return;
  /* ring time: register-scaled — low gongs bloom for seconds, high
     bells ring tight enough to play MELODIES on */
  double tau = (1.8 + 2.2 * vel) * 1.6 / (1.0 + f0 / 500.0);
  int len = (int)(4.5 * tau * SR);
  if (s0 + len > bus->n) len = bus->n - s0;
  unsigned h = (unsigned)(e->seed * 2654435761u);
  double det = 1.0 + 0.0035 * (hash01(h) - 0.5);
  double amp = 0.9 * pow(vel, 1.3);
  double I1_0 = 4.5 + 3.5 * vel, I2_0 = 2.4;
  double p1 = 0, p2 = 0, m1 = 0, m2 = 0;

  for (int i = 0; i < len; i++) {
    double t = (double)i / SR;
    double I1 = I1_0 * exp(-t / (0.40 * tau));
    double I2 = I2_0 * exp(-t / (0.10 * tau));
    double mm1 = sin(TAU * m1), mm2 = sin(TAU * m2);
    double y = 0.62 * sin(TAU * p1 + I1 * mm1 + I2 * mm2)
             + 0.38 * sin(TAU * p2 + 0.8 * I1 * mm1);
    p1 += f0 / SR;               if (p1 >= 1.0) p1 -= 1.0;
    p2 += f0 * 1.006 * det / SR; if (p2 >= 1.0) p2 -= 1.0;
    m1 += f0 * 1.4 / SR;         if (m1 >= 1.0) m1 -= 1.0;
    m2 += f0 * 3.53 / SR;        if (m2 >= 1.0) m2 -= 1.0;
    double env = exp(-t / tau) * (1.0 - 0.35 * exp(-t / 0.12)); /* the swell */
    double aenv = (t < 0.002) ? t / 0.002 : 1.0;
    double v = y * env * aenv * amp;
    bus_add(bus, s0 + i, v, e->pan, gain);
    bus_add(send, s0 + i, v, e->pan, sendg);
  }
}

/* ════ CLICKRATTLE — the shaker layer ════════════════════════════════ */
/* Dry resonant clicks with Newton's-cradle rattle tails: each hit is
   1-5 bounces of a narrow bandpassed tick, quieter and closer together
   as they settle. Velocity buys bounce count — soft hits tick once,
   hard hits shake. */

static void click_note(const Ev *e, Bus *bus, Bus *send, double gain, double sendg) {
  int s0 = (int)(e->beat * BEAT * SR);
  if (s0 >= bus->n) return;
  double vel = e->vel;
  unsigned h = (unsigned)(e->seed * 2654435761u) ^ (unsigned)(e->beat * 512.0);
  int nb = 1 + (int)(vel * 4.2 * hash01(h ^ 0x77u));
  double fc = 3800.0 + 4200.0 * hash01(h ^ 0xC11Cu);
  double t0 = 0.0;
  uint32_t rng = h | 1u;

  for (int b = 0; b < nb; b++) {
    double bamp = pow(0.55, b) * pow(vel, 1.2) * (0.8 + 0.4 * hash01(h + 31u * b));
    int bs = s0 + (int)(t0 * SR);
    int blen = (int)(0.006 * SR);
    Res2 tick;
    res_set(&tick, fc * (1.0 + 0.06 * (hash01(h + 7u * b) - 0.5)), 0.0011);
    for (int i = 0; i < blen && bs + i < bus->n; i++) {
      double t = (double)i / SR;
      rng ^= rng << 13; rng ^= rng >> 17; rng ^= rng << 5;
      double w = ((double)(rng >> 8) / 8388608.0) - 1.0;
      double y = res_tick(&tick, w * exp(-t / 0.0009));
      bus_add(bus, bs + i, y * bamp * 2.2, e->pan, gain);
      bus_add(send, bs + i, y * bamp * 2.2, e->pan, sendg);
    }
    t0 += (0.016 + 0.020 * hash01(h + 13u * b)) * (1.0 - 0.3 * vel);
  }
}

/* ════ TWOMASS — THE GOAT (batch 1) ══════════════════════════════════ */
/* Ishizaka-Flanagan two-mass vocal folds (Steinecke-Herzel form) into a
   Kelly-Lochbaum tube. Rendered as ONE continuous monophonic throat over
   the whole track: the folds never reset, so rapid re-articulated notes
   are a living bleat, not retriggered samples. Hard-blown notes
   (vel > 0.86) roughen into subharmonic growl; growl also deepens and
   quickens the vibrato (the bleat tremor, ~6-8 Hz). Vowel map is shifted
   goat-ward: G3 lands on "aa", D4 up at "ii" — maa / meh / mii. */

#define GT_OS 2
#define GT_TR (SR * GT_OS)
#define GT_NSEC 24
#define GT_F0REF 150.3
#define GT_PSMAX 9000.0

typedef struct { double cp, cd; } GtPose;

static GtPose gt_pose(double b) {
  static const GtPose a0 = { 0.86, 0.45 };
  static const GtPose a1 = { 0.30, 1.75 };
  static const GtPose a2 = { 0.72, 0.50 };
  if (b < 0.0) b = 0.0; else if (b > 1.0) b = 1.0;
  GtPose p;
  if (b < 0.5) { double t = b / 0.5;         p.cp = a0.cp + (a1.cp - a0.cp) * t; p.cd = a0.cd + (a1.cd - a0.cd) * t; }
  else         { double t = (b - 0.5) / 0.5; p.cp = a1.cp + (a2.cp - a1.cp) * t; p.cd = a1.cd + (a2.cd - a1.cd) * t; }
  return p;
}

static void gt_area(GtPose p, double *area) {
  const double Drest = 1.9, cw = 0.17;
  for (int i = 0; i < GT_NSEC; i++) {
    double x = (i + 0.5) / GT_NSEC;
    double g = (x - p.cp) / cw;
    double d = Drest - (Drest - p.cd) * exp(-g * g);
    if (x > 0.88) d += (x - 0.88) * 2.0;
    if (d < 0.2) d = 0.2;
    area[i] = d * d;
  }
}

static uint32_t gt_rng = 0x1a2b3c4du;
static inline double gt_frand(void) {
  gt_rng = gt_rng * 1664525u + 1013904223u;
  return (double)(gt_rng >> 8) / 8388608.0 - 1.0;
}

static void twomass_part(const Part *p, Bus *bus, Bus *send, double gain, double sendg) {
  int n = bus->n;
  int *gov = malloc(sizeof(int) * (size_t)n);
  if (!gov) exit(1);
  for (int i = 0; i < n; i++) gov[i] = -1;
  double relSec = 0.055;
  for (int j = 0; j < p->n; j++) {
    const Ev *e = &p->ev[j];
    int s0 = (int)(e->beat * BEAT * SR);
    int s1 = (int)((e->beat + e->beats) * BEAT * SR + relSec * SR);
    if (s0 < 0) s0 = 0;
    if (s1 > n) s1 = n;
    for (int i = s0; i < s1; i++) gov[i] = j;
  }

  /* fold + tract state — persists across the whole track */
  double x1 = 0.010, v1 = 0, x2 = -0.006, v2 = 0;
  double Qs = 1.0, Ps = 0.0, growlWalk = 0.0;
  double R[GT_NSEC], L[GT_NSEC], area[GT_NSEC], refl[GT_NSEC];
  for (int i = 0; i < GT_NSEC; i++) { R[i] = L[i] = 0.0; area[i] = 1.0; refl[i] = 0.0; }
  double hp_x1 = 0.0, hp_y = 0.0, lp = 0.0;

  const double dt = 1.0 / GT_TR;
  const double M1_0 = 0.125, M2_0 = 0.025;
  const double K1_0 = 80000.0, K2_0 = 8000.0, KC_0 = 25000.0;
  const double X01 = 0.018, X02 = 0.018;
  const double GLOT_L = 1.4, D1 = 0.25, D2 = 0.05, RHO = 0.00114;
  const double r1_base = 2.0 * 0.10 * sqrt(M1_0 * K1_0);
  const double r2_base = 2.0 * 0.10 * sqrt(M2_0 * K2_0);
  const double r1_col  = 2.0 * 0.70 * sqrt(M1_0 * K1_0);
  const double r2_col  = 2.0 * 0.70 * sqrt(M2_0 * K2_0);

  double vibPhase = 0.0, lastPan = 0.0;

  for (int f = 0; f < n; f++) {
    double env = 0.0, vel = 0.0, targetQ = Qs, growlAmt = 0.0;
    GtPose poseNow = { 0.5, 1.0 };
    int gi = gov[f];
    if (gi >= 0) {
      const Ev *e = &p->ev[gi];
      double freq = mtof(e->midi);
      double start = e->beat * BEAT, dur = e->beats * BEAT;
      double t = (double)f / SR;
      vel = e->vel;
      lastPan = e->pan;
      double atk = dur * 0.4; if (atk > 0.02) atk = 0.02; if (atk < 0.002) atk = 0.002;
      double lt = t - start;
      if (lt < atk)      env = 0.5 - 0.5 * cos(M_PI * (lt / atk));
      else if (lt < dur) env = 1.0;
      else {
        double rr = (lt - dur) / relSec;
        env = (rr >= 1.0) ? 0.0 : 0.5 + 0.5 * cos(M_PI * rr);
      }
      if (env < 0.0) env = 0.0;

      targetQ = freq / GT_F0REF;
      if (targetQ > 2.5) targetQ *= 1.0 + 0.045 * (targetQ - 2.5);

      /* growl engages on hard-blown notes only */
      growlAmt = (vel - 0.86) / 0.14;
      if (growlAmt < 0.0) growlAmt = 0.0; else if (growlAmt > 1.0) growlAmt = 1.0;

      /* the bleat tremor: growl deepens + quickens the vibrato */
      double vibOnset = lt - (0.14 - 0.09 * growlAmt);
      if (vibOnset > 0.0) {
        double vd = (0.006 + 0.011 * growlAmt) * (vibOnset < 0.2 ? vibOnset / 0.2 : 1.0);
        targetQ *= 1.0 + vd * sin(TAU * vibPhase);
      }

      /* goat-ward vowel line: maa around G3, mii up at D4 */
      double b = (freq - 120.0) / (320.0 - 120.0);
      double mf = (dur > 0) ? lt / dur : 0.0;
      if (mf > 1.0) mf = 1.0; if (mf < 0.0) mf = 0.0;
      GtPose pS = gt_pose(b);
      GtPose pE = gt_pose(b + 0.28);
      poseNow.cp = pS.cp + (pE.cp - pS.cp) * mf;
      poseNow.cd = pS.cd + (pE.cd - pS.cd) * mf;
    }
    vibPhase += (5.6 + 2.2 * growlAmt) / SR;
    if (vibPhase >= 1.0) vibPhase -= 1.0;

    double PsTarget = env * GT_PSMAX * (0.45 + 0.55 * vel);
    Ps += (PsTarget - Ps) * 0.02;
    Qs += (targetQ - Qs) * 0.03;
    double Q = Qs; if (Q < 0.2) Q = 0.2;

    gt_area(poseNow, area);
    for (int i = 1; i < GT_NSEC; i++) {
      double s = area[i - 1] + area[i];
      refl[i] = (s > 1e-9) ? (area[i - 1] - area[i]) / s : 0.0;
    }

    double m1e = M1_0 / Q, m2e = M2_0 / Q;
    double k1e = K1_0 * Q, k2e = K2_0 * Q, kce = KC_0 * Q;

    growlWalk += gt_frand() * 0.04 - growlWalk * 0.002;
    if (growlWalk > 1.0) growlWalk = 1.0; if (growlWalk < -1.0) growlWalk = -1.0;
    double asym = 1.0 + growlAmt * 0.16 * growlWalk;

    double frameOut = 0.0;

    for (int os = 0; os < GT_OS; os++) {
      double a1 = 2.0 * GLOT_L * (X01 + x1);
      double a2 = 2.0 * GLOT_L * (X02 + x2);
      double amin = a1 < a2 ? a1 : a2;

      double P1, P2, Ug;
      if (a1 > 0.0 && a2 > 0.0) {
        Ug = amin * sqrt(2.0 * Ps / RHO);
        if (a1 < a2) {
          P1 = 0.0;
          double rr = a1 / a2; P2 = Ps * (1.0 - rr * rr);
        } else {
          double rr = a2 / a1; P1 = Ps * (1.0 - rr * rr);
          P2 = 0.0;
        }
      } else {
        Ug = 0.0;
        P1 = Ps;
        P2 = 0.0;
      }

      double turb = gt_frand() * Ps * 0.010;
      double F1 = GLOT_L * D1 * P1 + GLOT_L * D1 * turb;
      double F2 = GLOT_L * D2 * P2;

      double r1 = r1_base, r2 = r2_base;
      double Fc1 = 0.0, Fc2 = 0.0;
      if (a1 < 0.0) { Fc1 = -3.0 * k1e * (X01 + x1); r1 = r1_col; }
      if (a2 < 0.0) { Fc2 = -3.0 * k2e * (X02 + x2); r2 = r2_col; }

      double acc1 = (-k1e * x1 - kce * (x1 - x2) - r1 * v1 + F1 + Fc1) / m1e;
      double acc2 = (-k2e * asym * x2 - kce * (x2 - x1) - r2 * v2 + F2 + Fc2) / m2e;

      v1 += acc1 * dt; x1 += v1 * dt;
      v2 += acc2 * dt; x2 += v2 * dt;
      if (x1 > 0.4) { x1 = 0.4; if (v1 > 0) v1 = 0; } else if (x1 < -0.4) { x1 = -0.4; if (v1 < 0) v1 = 0; }
      if (x2 > 0.4) { x2 = 0.4; if (v2 > 0) v2 = 0; } else if (x2 < -0.4) { x2 = -0.4; if (v2 < 0) v2 = 0; }

      double drive = Ug * 0.0022 + gt_frand() * Ug * 1.3e-5;

      double jR0 = L[0] * 0.75 + drive;
      double jLN = R[GT_NSEC - 1] * (-0.85);
      double newR[GT_NSEC], newL[GT_NSEC];
      newR[0] = jR0;
      for (int i = 1; i < GT_NSEC; i++) {
        double w = refl[i] * (R[i - 1] + L[i]);
        newR[i] = R[i - 1] - w;
        newL[i - 1] = L[i] + w;
      }
      newL[GT_NSEC - 1] = jLN;
      for (int i = 0; i < GT_NSEC; i++) { R[i] = newR[i] * 0.9985; L[i] = newL[i] * 0.9985; }

      double lip = R[GT_NSEC - 1];
      double hp = lip - hp_x1 + 0.995 * hp_y;
      hp_x1 = lip; hp_y = hp;
      lp += (hp - lp) * 0.40;
      frameOut += lp;
    }

    double sig = frameOut / GT_OS * 0.5;
    bus_add(bus, f, sig, lastPan, gain);
    bus_add(send, f, sig, lastPan, sendg);
  }

  free(gov);
}

/* ════ VOSIM — the girl robot (batch 1) ══════════════════════════════ */
/* Kaegi & Tempelaars 1978: N sin² pulses of decaying amplitude + gap per
   period; one broad resonance glides a-e-i-o-u-ae. Feminized here: the
   vowel-formant table sits ~18% above the batch voice and the vibrato
   sings harder. She answers the goat — melody stays G4..E5, under the
   high-register collapse ceiling (~660 Hz). */

#define VO_VOWELS 6
static const double vowel_f[VO_VOWELS] = { 850.0, 555.0, 342.0, 614.0, 425.0, 755.0 };
static double smoothstep(double x) { return x * x * (3.0 - 2.0 * x); }

static void vosim_note(const Ev *e, Bus *bus, Bus *send, double gain, double sendg) {
  double f0base = mtof(e->midi), start = e->beat * BEAT, dur = e->beats * BEAT;
  double vel = e->vel;
  int s0 = (int)(start * SR);
  double rel = 0.14;
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
      if (dur > 0.4 && t > 0.18) { /* she sings: earlier, deeper vibrato */
        double dep = fmin(1.0, (t - 0.18) / 0.35) * 0.0060;
        f0 *= 1.0 + dep * sin(TAU * 5.4 * t);
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

    /* the singing envelope: lean into the note (a 220 ms swell), then
       HOLD — a singer sustains, she doesn't decay like a struck string */
    double env;
    if (t < 0.006) env = t / 0.006;
    else if (t < dur)
      env = exp(-0.12 * (t - 0.006) / dur) * (1.0 - 0.30 * exp(-t / 0.22));
    else {
      double e0 = exp(-0.12 * (dur - 0.006) / dur) * (1.0 - 0.30 * exp(-dur / 0.22));
      env = e0 * (1.0 - (t - dur) / rel);
      if (env < 0.0) env = 0.0;
    }

    double sig = y * env * (0.25 + 0.75 * vel) * 0.5;
    bus_add(bus, s0 + i, sig, e->pan, gain);
    bus_add(send, s0 + i, sig, e->pan, sendg);
    p += inc;
  }
}

/* ── Schroeder reverb: bus in, stereo out ───────────────────────────── */
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

/* ════ SIDECHAIN — the pump ══════════════════════════════════════════ */
/* Every kick (memkick + implokick parts) ducks the music bus: 12 ms dive
   to 15%, exponential recovery ~130 ms — mostly home before the next
   kick at 150. Drums ride their own dry bus, untouched. */

static void carve_duck(double *duck, int n, const Part *kicks, const Part *imps) {
  for (int i = 0; i < n; i++) duck[i] = 1.0;
  const Part *ps[2] = { kicks, imps };
  const double depth = 0.85, dive = 0.012, tau = 0.13, win = 0.40;
  for (int pi = 0; pi < 2; pi++) {
    for (int j = 0; j < ps[pi]->n; j++) {
      int s0 = (int)(ps[pi]->ev[j].beat * BEAT * SR);
      int len = (int)(win * SR);
      for (int i = 0; i < len && s0 + i < n; i++) {
        double t = (double)i / SR;
        double g;
        if (t < dive) g = 1.0 - depth * (t / dive);
        else g = 1.0 - depth * exp(-(t - dive) / tau);
        if (s0 + i >= 0 && g < duck[s0 + i]) duck[s0 + i] = g;
      }
    }
  }
}

/* ════ SUBSTRATE — carved-noise residue (the nullnoise law) ══════════ */
/* Two noise materials (pink + velvet), each a peaking-bell wet MINUS the
   same dry: the noise only exists where a bell breaks the cancellation.
   Bands run in PARALLEL (independent residues sum), so overlapping
   frequencies never dB-stack. Pump bands breathe with the sidechain. */

typedef struct {
  int group;                     /* 0 pink, 1 velvet */
  double t0, atk, hold, rel;     /* seconds */
  double f0, f1;                 /* f1 > 0 -> exponential glide */
  double db, q;
  int pump;                      /* residue rides duck[] */
} SubBand;

#define MAXSUB 24
static SubBand subBands[MAXSUB];
static int nSub = 0;

static void sub_add(int group, double t0, double atk, double hold, double rel,
                    double f0, double f1, double db, double q, int pump) {
  if (nSub >= MAXSUB) { fprintf(stderr, "goatshard: substrate overflow\n"); exit(1); }
  SubBand *b = &subBands[nSub++];
  b->group = group; b->t0 = t0; b->atk = atk; b->hold = hold; b->rel = rel;
  b->f0 = f0; b->f1 = f1; b->db = db; b->q = q; b->pump = pump;
}

/* pink: LCG -> Paul Kellett filter (nullnoise numerics) */
typedef struct {
  uint32_t s;
  double b0, b1, b2, b3, b4, b5, b6;
  int vPos, vAt;
  double vSign;
} NGen;

static void ngen_init(NGen *g, uint32_t seed) {
  memset(g, 0, sizeof *g);
  g->s = seed;
  g->vPos = 16;
}
static inline double ngen_lcg(NGen *g) {
  g->s = g->s * 1664525u + 1013904223u;
  return ((double)g->s / 4294967296.0) * 2.0 - 1.0;
}
static inline double ngen_pink(NGen *g) {
  double w = ngen_lcg(g);
  g->b0 = 0.99886 * g->b0 + w * 0.0555179;
  g->b1 = 0.99332 * g->b1 + w * 0.0750759;
  g->b2 = 0.969   * g->b2 + w * 0.153852;
  g->b3 = 0.8665  * g->b3 + w * 0.3104856;
  g->b4 = 0.55    * g->b4 + w * 0.5329522;
  g->b5 = -0.7616 * g->b5 - w * 0.016898;
  double out = (g->b0 + g->b1 + g->b2 + g->b3 + g->b4 + g->b5 + g->b6 + w * 0.5362) * 0.11;
  g->b6 = w * 0.115926;
  return out;
}
static inline double ngen_velvet(NGen *g) {
  if (g->vPos >= 16) {
    double r = ngen_lcg(g);
    g->vAt = (int)((r * 0.5 + 0.5) * 16.0) % 16;
    g->vSign = (g->s & 1u) ? 0.6 : -0.6;
    g->vPos = 0;
  }
  double v = (g->vPos == g->vAt) ? g->vSign : 0.0;
  g->vPos++;
  return v;
}

static double sub_env(const SubBand *b, double t) {
  double dt = t - b->t0;
  if (dt < 0 || dt >= b->atk + b->hold + b->rel) return 0.0;
  if (dt < b->atk) return dt / b->atk;
  if (dt < b->atk + b->hold) return 1.0;
  return 1.0 - (dt - b->atk - b->hold) / b->rel;
}

#define SUB_BLOCK 64

static void substrate(Bus *master, const double *duck) {
  int n = master->n;
  for (int ch = 0; ch < 2; ch++) {
    NGen pinkG, velvG;
    ngen_init(&pinkG, ch ? 0xC0FFEEu : 0xBEEFu);
    ngen_init(&velvG, ch ? 0x60A751u : 0x5A4Du);
    double detune = ch ? 1.012 : 1.0;
    double z1[MAXSUB], z2[MAXSUB];
    for (int b = 0; b < nSub; b++) { z1[b] = 0; z2[b] = 0; }
    float *out = ch ? master->R : master->L;

    double dryP[SUB_BLOCK], dryV[SUB_BLOCK], wet[SUB_BLOCK];
    for (int bs = 0; bs < n; bs += SUB_BLOCK) {
      int bn = n - bs < SUB_BLOCK ? n - bs : SUB_BLOCK;
      double time = (double)bs / SR;
      for (int i = 0; i < bn; i++) {
        dryP[i] = ngen_pink(&pinkG);
        dryV[i] = ngen_velvet(&velvG);
      }
      for (int b = 0; b < nSub; b++) {
        SubBand *sb = &subBands[b];
        double env = sub_env(sb, time);
        double db = sb->db * env;
        int ringing = fabs(z1[b]) + fabs(z2[b]) > 1e-9;
        if (db <= 0.01 && !ringing) continue;

        double freq = sb->f0;
        if (sb->f1 > 0) {
          double span = sb->atk + sb->hold + sb->rel;
          double pgl = (time - sb->t0) / span;
          if (pgl < 0) pgl = 0; if (pgl > 1) pgl = 1;
          freq = sb->f0 * pow(sb->f1 / sb->f0, pgl);
        }
        double A = pow(10.0, db / 40.0);
        double fd = freq * detune;
        double fmaxs = SR * 0.45;
        if (fd > fmaxs) fd = fmaxs;
        double w0 = TAU * fd / SR;
        double alpha = sin(w0) / (2.0 * sb->q);
        double cosw = cos(w0);
        double a0 = 1.0 + alpha / A;
        double b0c = (1.0 + alpha * A) / a0;
        double b1c = (-2.0 * cosw) / a0;
        double b2c = (1.0 - alpha * A) / a0;
        double a1c = (-2.0 * cosw) / a0;
        double a2c = (1.0 - alpha / A) / a0;
        const double *dry = sb->group ? dryV : dryP;
        double zz1 = z1[b], zz2 = z2[b];
        for (int i = 0; i < bn; i++) {
          double x = dry[i];
          double y = b0c * x + zz1;
          zz1 = b1c * x - a1c * y + zz2;
          zz2 = b2c * x - a2c * y;
          wet[i] = y;
        }
        z1[b] = zz1; z2[b] = zz2;
        for (int i = 0; i < bn; i++) {
          double res = (wet[i] - dry[i]) * 0.40;
          if (sb->pump) res *= duck[bs + i];
          out[bs + i] += (float)res;
        }
      }
    }
  }
}

static void substrate_score(void) {
  double B = BEAT;
  /* intro breath: a presence carved from pink, swelling in */
  sub_add(0, 0.0, 2.5, S_VERSE1 * B - 4.0, 1.5, 700.0, 0, 12.0, 5.0, 0);
  /* verses: a low quiet presence, mostly felt */
  sub_add(0, S_VERSE1 * B, 1.5, (S_PRE - S_VERSE1) * B - 2.5, 1.0,
          420.0, 0, 8.0, 4.0, 0);
  sub_add(0, S_VERSE2 * B, 1.5, (S_BRIDGE - S_VERSE2) * B - 2.5, 1.0,
          420.0, 0, 8.0, 4.0, 0);
  /* risers: last 8 beats into each chorus — a whistle, not wind */
  sub_add(0, (S_CHOR1 - 8.0) * B, 0.2, 8.0 * B - 0.7, 0.5,
          500.0, 5200.0, 17.0, 5.0, 0);
  sub_add(0, (S_CHOR2 - 8.0) * B, 0.2, 8.0 * B - 0.7, 0.5,
          420.0, 6000.0, 18.0, 5.0, 0);
  /* choruses: low pump bell + a whisper of velvet air, kick-breathing */
  sub_add(0, S_CHOR1 * B, 0.05, (S_VERSE2 - S_CHOR1) * B - 0.4, 0.4,
          130.0, 0, 23.0, 1.1, 1);
  sub_add(1, S_CHOR1 * B, 0.05, (S_VERSE2 - S_CHOR1) * B - 0.4, 0.4,
          8500.0, 0, 6.0, 0.9, 1);
  sub_add(0, S_CHOR2 * B, 0.05, (S_OUTRO - S_CHOR2) * B - 0.4, 0.4,
          130.0, 0, 24.0, 1.1, 1);
  sub_add(1, S_CHOR2 * B, 0.05, (S_OUTRO - S_CHOR2) * B - 0.4, 0.4,
          9000.0, 0, 7.0, 0.9, 1);
  /* bridge breath */
  sub_add(0, S_BRIDGE * B, 1.5, (S_BUILD2 - S_BRIDGE) * B - 2.5, 1.0,
          480.0, 0, 12.0, 4.0, 0);
  /* outro: the substrate is the last thing standing */
  sub_add(0, S_OUTRO * B, 1.5, (TRACK_BEATS - S_OUTRO) * B - 4.5, 3.0,
          560.0, 0, 11.0, 4.5, 0);
}

/* ════ THE SCORE ═════════════════════════════════════════════════════ */

static Part ghostP, snrP, clapP, goatP, stabP, shardP, droneP, robotP, gongP, clickP;

#define PAN_GOAT 0.0
#define PAN_KICK 0.0
#define PAN_SNARE 0.15
#define PAN_CLAP (-0.15)
#define PAN_BASS 0.0
#define PAN_ROBOT 0.18

static void bleat(double b0, double midi, int nrep, double vel);

/* diatonic third below, G-minor pitch classes (D/A/G take the wide step) */
static int third_below(int m) {
  switch (((m % 12) + 12) % 12) {
    case 2: case 9: case 7: return m - 4;
    default: return m - 3;
  }
}

/* the girl-robot hook: full 4-bar SUNG phrases — long tied notes, few
   rests, melisma turns. Each drop is a little song: statement (pIdx 0),
   rising ANSWER (pIdx 1), statement again with the harmony choir
   (pIdx 2). var 0 = G minor (drop 1), var 1 = C minor (drop 2).
   Ceiling is Eb5 — vosim collapses above ~660 Hz. */
typedef struct { double b, d; int m; double v; } HookN;

static const HookN hookA[13] = {
  { 0.00, 1.50, 74, 0.82 }, { 1.75, 0.22, 72, 0.66 }, { 2.00, 1.90, 70, 0.80 },
  { 4.00, 1.00, 72, 0.74 }, { 5.00, 0.50, 74, 0.72 }, { 5.75, 0.22, 72, 0.64 },
  { 6.00, 1.90, 69, 0.78 },
  { 8.00, 1.50, 70, 0.80 }, { 9.75, 0.22, 69, 0.62 }, { 10.00, 1.00, 67, 0.74 },
  { 11.00, 0.90, 69, 0.70 },
  { 12.00, 2.50, 74, 0.84 }, { 14.75, 1.20, 72, 0.72 },
};
static const HookN hookAans[13] = { /* the answer: starts low, climbs */
  { 0.00, 1.00, 67, 0.74 }, { 1.00, 0.50, 69, 0.70 }, { 1.50, 0.50, 70, 0.74 },
  { 2.00, 1.90, 72, 0.80 },
  { 4.00, 1.00, 74, 0.78 }, { 5.00, 0.50, 72, 0.68 }, { 5.75, 0.22, 70, 0.62 },
  { 6.00, 1.90, 74, 0.82 },
  { 8.00, 1.50, 75, 0.84 }, { 9.75, 0.22, 74, 0.64 }, { 10.00, 1.90, 70, 0.76 },
  { 12.00, 2.50, 69, 0.80 }, { 14.50, 1.40, 70, 0.72 },
};
static const HookN hookB[13] = {
  { 0.00, 1.50, 75, 0.84 }, { 1.75, 0.22, 74, 0.66 }, { 2.00, 1.90, 72, 0.82 },
  { 4.00, 1.00, 74, 0.74 }, { 5.00, 0.50, 75, 0.74 }, { 5.75, 0.22, 74, 0.64 },
  { 6.00, 1.90, 70, 0.78 },
  { 8.00, 1.50, 72, 0.80 }, { 9.75, 0.22, 70, 0.62 }, { 10.00, 1.00, 67, 0.74 },
  { 11.00, 0.90, 70, 0.70 },
  { 12.00, 2.50, 75, 0.86 }, { 14.75, 1.20, 72, 0.72 },
};
static const HookN hookBans[13] = {
  { 0.00, 1.00, 67, 0.74 }, { 1.00, 0.50, 70, 0.70 }, { 1.50, 0.50, 72, 0.74 },
  { 2.00, 1.90, 74, 0.80 },
  { 4.00, 1.00, 75, 0.80 }, { 5.00, 0.50, 74, 0.68 }, { 5.75, 0.22, 72, 0.62 },
  { 6.00, 1.90, 75, 0.84 },
  { 8.00, 1.50, 74, 0.82 }, { 9.75, 0.22, 72, 0.64 }, { 10.00, 1.90, 70, 0.76 },
  { 12.00, 2.50, 72, 0.80 }, { 14.50, 1.40, 70, 0.72 },
};

static void robot_hook(double b0, int var, int pIdx) {
  const HookN *ph = (pIdx == 1) ? (var ? hookBans : hookAans)
                                : (var ? hookB : hookA);
  int harm = (pIdx == 2);
  for (int i = 0; i < 13; i++) {
    emit(&robotP, b0 + ph[i].b, ph[i].d, ph[i].m, ph[i].v,
         PAN_ROBOT + 0.10 * ((i % 2) ? 1 : -1));
    if (harm)
      emit(&robotP, b0 + ph[i].b, ph[i].d, third_below(ph[i].m), ph[i].v * 0.68,
           -PAN_ROBOT - 0.10);
  }
}

/* the VERSE: his song, sung clean (no growl anywhere near 0.86) —
   8 bars over a i-VII-VI-VII bass. Verse 2 lifts the final line. */
static void goat_verse(double b0, int var) {
  static const HookN V[19] = {
    { 0.0, 1.5, 55, 0.62 },  { 2.0, 0.75, 58, 0.58 },  { 3.0, 0.9, 57, 0.55 },
    { 4.0, 1.5, 53, 0.60 },  { 6.0, 1.9, 55, 0.62 },
    { 8.0, 1.5, 58, 0.62 },  { 10.0, 0.75, 60, 0.60 }, { 11.0, 0.9, 58, 0.56 },
    { 12.0, 1.5, 62, 0.64 }, { 14.0, 1.9, 60, 0.60 },
    { 16.0, 1.5, 55, 0.62 }, { 18.0, 0.75, 53, 0.55 }, { 19.0, 0.9, 55, 0.58 },
    { 20.0, 1.5, 58, 0.62 }, { 22.0, 1.9, 57, 0.58 },
    { 24.0, 1.0, 55, 0.60 }, { 25.5, 0.5, 53, 0.52 },  { 26.0, 2.5, 50, 0.62 },
    { 29.0, 1.5, 55, 0.58 },
  };
  for (int i = 0; i < 19; i++) {
    int m = V[i].m + ((var && V[i].b >= 24.0) ? 3 : 0);
    emit(&goatP, b0 + V[i].b, V[i].d, m, V[i].v, PAN_GOAT);
  }
  bleat(b0 + 7.25, 55, 2, 0.62);   /* soft, clean bleats — his accent */
  bleat(b0 + 15.25, 58, 2, 0.62);
}

/* a bleat: rapid re-articulations of one held larynx (~11 Hz) */
static void bleat(double b0, double midi, int nrep, double vel) {
  for (int i = 0; i < nrep; i++)
    emit(&goatP, b0 + 0.22 * i, 0.16, midi, vel, PAN_GOAT);
}

/* the goat riff: one bar, hard-blown (growl territory), two variants */
static void goat_riff(double b0, int tr, int var) {
  emit(&goatP, b0 + 0.00, 0.55, 55 + tr, 0.97, PAN_GOAT);
  bleat(b0 + 1.00, 58 + tr, 2, 0.93);
  emit(&goatP, b0 + 1.50, 0.40, 53 + tr, 0.95, PAN_GOAT);
  emit(&goatP, b0 + 2.25, 0.60, 50 + tr, 0.96, PAN_GOAT);
  if (var) {
    bleat(b0 + 3.00, 55 + tr, 2, 0.92);
    emit(&goatP, b0 + 3.50, 0.35, 58 + tr, 0.90, PAN_GOAT);
  } else {
    emit(&goatP, b0 + 3.00, 0.80, 55 + tr, 0.94, PAN_GOAT);
  }
}

/* power chord: root + 5th + octave, no third — the shard wall. Each
   voice is its own chaosfm operator pair, spread across the field, so
   the chord blooms into chaos as one slab. */
static void power_chord(double b0, double len, int root, double vel) {
  static const int iv[3] = { 0, 7, 12 };
  for (int v = 0; v < 3; v++)
    emit(&stabP, b0, len, root + iv[v], vel, (v - 1) * 0.5);
}

/* melodic glass: the shards play a turning 16th arpeggio (rotated per
   bar), not random scatter — density gates how much of it speaks */
static void shards_bar(double b0, int seedBase, double density, int tr) {
  static const int seq[16] = { 79, 82, 86, 82, 91, 86, 82, 79,
                               86, 82, 79, 82, 91, 86, 82, 86 };
  for (int s = 0; s < 16; s++) {
    if (s % 4 == 0) continue; /* leave the downbeats to the kick */
    double h = hash01((unsigned)(seedBase * 131 + s * 17 + 5));
    if (h > density) continue;
    int m = seq[(s + seedBase) & 15] + tr;
    double v = (s % 4 == 2 ? 0.52 : 0.34) + 0.18 * h;
    double pan = (s % 2 ? 1.0 : -1.0) * (0.45 + 0.30 * h);
    emit(&shardP, b0 + 0.25 * s, 0.16, m, v, pan);
  }
}

/* the goat's counter-riff: a 2-bar CLIMB (G-Bb-C-D and back down) that
   answers two bars of the main riff — melodic contrast every cycle */
static void goat_riff2(double b0, int tr) {
  emit(&goatP, b0 + 0.00, 0.70, 55 + tr, 0.95, PAN_GOAT);
  emit(&goatP, b0 + 1.00, 0.70, 58 + tr, 0.94, PAN_GOAT);
  emit(&goatP, b0 + 2.00, 0.70, 60 + tr, 0.95, PAN_GOAT);
  emit(&goatP, b0 + 3.00, 0.90, 62 + tr, 0.96, PAN_GOAT);
  bleat(b0 + 4.25, 62 + tr, 2, 0.93);
  emit(&goatP, b0 + 5.00, 0.60, 60 + tr, 0.94, PAN_GOAT);
  emit(&goatP, b0 + 5.75, 0.45, 58 + tr, 0.92, PAN_GOAT);
  emit(&goatP, b0 + 6.50, 1.30, 55 + tr, 0.95, PAN_GOAT);
}

/* a bell run: four rising FM bells across the back half of a bar */
static void bell_run(double b0, int tr) {
  static const int m[4] = { 67, 70, 74, 79 };
  for (int i = 0; i < 4; i++)
    emit(&gongP, b0 + 2.0 + 0.5 * i, 0.5, m[i] + tr, 0.48 + 0.04 * i,
         (i % 2 ? 0.45 : -0.45));
}

/* one drop bar: full battery + bass + stabs */
static void drop_bar(double b0, int k, int tr, int drop2) {
  /* four on the floor */
  for (int bt = 0; bt < 4; bt++)
    emit(&kickP, b0 + bt, 0.25, 36, bt == 0 ? 0.97 : 0.93, PAN_KICK);
  if (drop2) { /* implokick chaos layer on 1 and 3 */
    emit(&impP, b0 + 0.0, 0.25, 36, 0.90, PAN_KICK);
    emit(&impP, b0 + 2.0, 0.25, 36, 0.86, PAN_KICK);
  }
  /* offbeat ghosts */
  for (int bt = 0; bt < 4; bt++)
    emit(&ghostP, b0 + bt + 0.5, 0.25, 36, 0.34, (bt % 2 ? 0.22 : -0.22));
  /* backbeat */
  emit(&snrP, b0 + 1.0, 0.25, 50, 0.82, PAN_SNARE);
  emit(&snrP, b0 + 3.0, 0.25, 50, 0.85, PAN_SNARE);
  if (k % 2) emit(&snrP, b0 + 3.75, 0.2, 50, 0.32, PAN_SNARE + 0.06);
  if (drop2) {
    emit(&clapP, b0 + 1.0, 0.25, 50, 0.72, PAN_CLAP);
    emit(&clapP, b0 + 3.0, 0.25, 50, 0.75, PAN_CLAP);
  }
  /* rolling offbeat throat bass: i - i - III - VII under the riff,
     with an anticipation note pulling into the next bar */
  static const int roots[4] = { 31, 31, 34, 29 }; /* G1 G1 Bb1 F1 */
  int root = roots[k % 4] + tr;
  for (int bt = 0; bt < 4; bt++)
    emit(&bassP, b0 + bt + 0.5, 0.38, root, 0.60, PAN_BASS);
  emit(&bassP, b0 + 3.75, 0.20, roots[(k + 1) % 4] + tr, 0.55, PAN_BASS);
  /* long drawn power chords: the bloom unfolds across two beats and
     each chord rings into the next — a wall, not a stab */
  int proot = root + 24;
  power_chord(b0 + 1.5, 1.8, proot, 0.90);
  power_chord(b0 + 3.5, 1.8, proot, 0.86);
  /* the shaker: 16ths with offbeat accents; hard hits rattle */
  for (int s = 0; s < 16; s++) {
    double cv = (s % 4 == 2) ? 0.58 : (s % 2 ? 0.30 : 0.22);
    emit(&clickP, b0 + 0.25 * s, 0.1, 60, cv, (s % 2 ? 0.35 : -0.35));
  }
  /* glass */
  shards_bar(b0, 700 + k + drop2 * 100, drop2 ? 0.80 : 0.60, tr);
}

static void compose(void) {
  /* ── INTRO (0-32): the voices meet ONE AT A TIME ─────────────────── */
  emit(&gongP, 0.0, 4.0, 43, 0.85, 0.0);              /* 1. the gong */
  emit(&droneP, 0.0, 15.0, 43, 0.34, -0.55);
  emit(&droneP, 0.4, 14.0, 50, 0.28, 0.55);
  for (int b = 8; b < 32; b++)                        /* 2. the shaker */
    emit(&clickP, b + 0.25, 0.1, 60, 0.20 + 0.08 * ((double)(b - 8) / 23.0),
         (b % 2 ? 0.35 : -0.35));
  emit(&goatP, 16.0, 2.0, 55, 0.58, PAN_GOAT);        /* 3. the goat */
  emit(&goatP, 19.0, 1.2, 58, 0.55, PAN_GOAT);
  bleat(21.5, 55, 2, 0.60);
  emit(&robotP, 24.0, 2.5, 70, 0.55, PAN_ROBOT);      /* 4. the girl */
  emit(&robotP, 27.0, 1.5, 74, 0.52, PAN_ROBOT);
  emit(&gongP, 28.0, 4.0, 50, 0.55, 0.25);
  emit(&washP, 30.0, 2.0, 50, 0.45, 0.5);

  /* ── VERSE 1 (32-64): kick + bass enter and carry HIS song ───────── */
  static const int vroots[4] = { 31, 29, 27, 29 };    /* Gm F Eb F */
  for (int b = 0; b < 32; b++) {
    emit(&kickP, S_VERSE1 + b, 0.25, 36, 0.78, PAN_KICK);
    emit(&clickP, S_VERSE1 + b + 0.5, 0.1, 60, 0.26, (b % 2 ? 0.35 : -0.35));
    if (b % 2) emit(&ghostP, S_VERSE1 + b + 0.5, 0.25, 36, 0.25,
                    (b % 4 == 1 ? 0.25 : -0.25));
  }
  for (int k = 0; k < 8; k++) {                       /* light offbeat bass */
    double bb = S_VERSE1 + 4.0 * k;
    emit(&bassP, bb + 0.5, 0.38, vroots[k % 4], 0.55, PAN_BASS);
    emit(&bassP, bb + 2.5, 0.38, vroots[k % 4], 0.52, PAN_BASS);
  }
  goat_verse(S_VERSE1, 0);
  emit(&droneP, S_VERSE1, 7.5, 43, 0.22, -0.5);       /* quiet mid pads */
  emit(&droneP, S_VERSE1 + 8.0, 7.5, 41, 0.22, 0.5);
  emit(&droneP, S_VERSE1 + 16.0, 7.5, 43, 0.22, -0.5);
  emit(&droneP, S_VERSE1 + 24.0, 7.5, 41, 0.22, 0.5);

  /* ── PRE-CHORUS (64-80): she wakes up, the band leans forward ────── */
  for (int b = 0; b < 14; b++)                        /* kick gasps at the end */
    emit(&kickP, S_PRE + b, 0.25, 36, 0.82, PAN_KICK);
  for (int s = 0; s < 16; s++)                        /* 8th roll, 2 bars */
    emit(&snrP, S_PRE + 0.5 * s, 0.2, 50, 0.24 + 0.20 * ((double)s / 15.0), PAN_SNARE);
  for (int s = 0; s < 24; s++) {                      /* 16th roll to the gasp */
    double t = (double)s / 23.0;
    emit(&snrP, S_PRE + 8.0 + 0.25 * s, 0.18, 50, 0.44 + 0.50 * t, -0.4 + 0.8 * t);
  }
  for (int k = 0; k < 4; k++) {                       /* chord swells */
    power_chord(S_PRE + 4.0 * k + 1.5, 2.2, 55, 0.66 + 0.08 * k);
    emit(&bassP, S_PRE + 4.0 * k + 0.5, 0.38, 31, 0.58, PAN_BASS);
    emit(&bassP, S_PRE + 4.0 * k + 1.5, 0.38, 31, 0.55, PAN_BASS);
    emit(&bassP, S_PRE + 4.0 * k + 2.5, 0.38, 31, 0.58, PAN_BASS);
    emit(&bassP, S_PRE + 4.0 * k + 3.5, 0.38, 31, 0.55, PAN_BASS);
  }
  bleat(S_PRE + 3.5, 55, 2, 0.90);                    /* the growl wakes */
  bleat(S_PRE + 7.5, 58, 2, 0.92);
  bleat(S_PRE + 11.5, 60, 3, 0.95);
  emit(&robotP, S_PRE + 2.5, 0.5, 67, 0.62, PAN_ROBOT); /* her fragments */
  emit(&robotP, S_PRE + 3.25, 0.5, 70, 0.64, PAN_ROBOT);
  emit(&robotP, S_PRE + 6.5, 0.5, 72, 0.66, PAN_ROBOT);
  emit(&robotP, S_PRE + 7.25, 0.5, 74, 0.68, PAN_ROBOT);
  emit(&robotP, S_PRE + 10.0, 5.5, 74, 0.78, PAN_ROBOT); /* the held cry */
  emit(&washP, S_CHOR1 - 2.0, 4.0, 50, 0.85, -0.5);

  /* ── CHORUS 1 (80-112, 8 bars): everything, statement + answer ───── */
  emit(&impP, S_CHOR1, 0.25, 36, 1.0, PAN_KICK);      /* door slam */
  for (int k = 0; k < 8; k++) {
    double b0 = S_CHOR1 + 4.0 * k;
    drop_bar(b0, k, 0, 0);
    if (k == 0) robot_hook(b0, 0, 0);                 /* statement */
    if (k == 4) robot_hook(b0, 0, 1);                 /* the answer */
    if (k % 4 == 0) emit(&gongP, b0, 4.0, 43, 0.85, (k ? 0.3 : -0.3));
    if (k % 4 == 2) { goat_riff2(b0, 0); bell_run(b0, 0); }
    else if (k % 4 != 3) goat_riff(b0, 0, k % 2);
  }
  emit(&washP, S_VERSE2 - 2.0, 4.0, 50, 0.65, 0.5);

  /* ── VERSE 2 (112-144): stripped back down — she echoes him now ──── */
  for (int b = 0; b < 32; b++) {
    emit(&kickP, S_VERSE2 + b, 0.25, 36, 0.78, PAN_KICK);
    emit(&clickP, S_VERSE2 + b + 0.5, 0.1, 60, 0.28, (b % 2 ? 0.35 : -0.35));
    if (b % 2) emit(&ghostP, S_VERSE2 + b + 0.5, 0.25, 36, 0.26,
                    (b % 4 == 1 ? 0.25 : -0.25));
  }
  for (int k = 0; k < 8; k++) {
    double bb = S_VERSE2 + 4.0 * k;
    emit(&snrP, bb + 1.0, 0.25, 50, 0.48, PAN_SNARE); /* backbeat joins */
    emit(&snrP, bb + 3.0, 0.25, 50, 0.50, PAN_SNARE);
    emit(&bassP, bb + 0.5, 0.38, vroots[k % 4], 0.56, PAN_BASS);
    emit(&bassP, bb + 2.5, 0.38, vroots[k % 4], 0.54, PAN_BASS);
  }
  goat_verse(S_VERSE2, 1);
  emit(&robotP, S_VERSE2 + 6.5, 1.2, 70, 0.50, PAN_ROBOT);  /* her echoes */
  emit(&robotP, S_VERSE2 + 14.5, 1.2, 72, 0.52, PAN_ROBOT);
  emit(&robotP, S_VERSE2 + 22.5, 1.2, 70, 0.50, PAN_ROBOT);
  emit(&robotP, S_VERSE2 + 30.0, 2.0, 74, 0.56, PAN_ROBOT);
  bell_run(S_VERSE2 + 12.0, 0);                       /* bell color */
  bell_run(S_VERSE2 + 28.0, 0);

  /* ── BRIDGE (144-160): no drums — the naked duet over bells ─────── */
  emit(&droneP, S_BRIDGE, 15.0, 46, 0.36, -0.5);      /* Bb2 turn */
  emit(&droneP, S_BRIDGE + 0.4, 14.0, 53, 0.28, 0.5);
  emit(&gongP, S_BRIDGE + 0.0, 4.0, 55, 0.55, -0.2);
  emit(&gongP, S_BRIDGE + 5.0, 4.0, 58, 0.50, 0.2);
  emit(&gongP, S_BRIDGE + 10.0, 4.0, 62, 0.60, 0.0);
  emit(&robotP, S_BRIDGE + 1.0, 2.5, 70, 0.62, PAN_ROBOT);   /* she leads */
  emit(&robotP, S_BRIDGE + 4.5, 1.5, 72, 0.58, PAN_ROBOT);
  emit(&robotP, S_BRIDGE + 6.5, 2.5, 74, 0.64, PAN_ROBOT);
  emit(&robotP, S_BRIDGE + 10.0, 4.0, 74, 0.68, PAN_ROBOT);  /* her big note */
  emit(&goatP, S_BRIDGE + 3.5, 1.4, 55, 0.58, PAN_GOAT);     /* he answers */
  emit(&goatP, S_BRIDGE + 8.5, 1.6, 58, 0.55, PAN_GOAT);
  emit(&goatP, S_BRIDGE + 13.0, 2.4, 62, 0.62, PAN_GOAT);

  /* ── BUILD (160-176): the band returns, all pistons ──────────────── */
  for (int b = 0; b < 16; b++) {
    double t = (double)b / 15.0;
    emit(&kickP, S_BUILD2 + b, 0.25, 36, 0.66 + 0.28 * t, PAN_KICK);
    emit(&ghostP, S_BUILD2 + b + 0.5, 0.25, 36, 0.28, (b % 2 ? 0.22 : -0.22));
    emit(&bassP, S_BUILD2 + b + 0.5, 0.38, 31, 0.56, PAN_BASS);
    emit(&clickP, S_BUILD2 + b + 0.25, 0.1, 60, 0.30 + 0.22 * t, (b % 2 ? 0.32 : -0.32));
  }
  for (int s = 0; s < 64; s++) {
    double t = (double)s / 63.0;
    emit(&snrP, S_BUILD2 + 0.25 * s, 0.16, 50, 0.30 + 0.60 * t, -0.5 + t);
  }
  for (int k = 0; k < 4; k++) {
    emit(&impP, S_BUILD2 + 4.0 * k, 0.25, 36, 0.72, PAN_KICK);
    power_chord(S_BUILD2 + 4.0 * k + 1.5, 1.8, k % 2 ? 58 : 55, 0.68 + 0.07 * k);
  }
  bleat(S_BUILD2 + 7.5, 58, 2, 0.93);
  bleat(S_BUILD2 + 14.5, 60, 3, 0.96);
  emit(&robotP, S_BUILD2 + 8.0, 6.0, 75, 0.80, PAN_ROBOT);  /* held cry */
  emit(&washP, S_CHOR2 - 2.0, 4.0, 50, 0.90, 0.5);

  /* ── FINAL CHORUS (176-224, 12 bars, +5 C minor): statement, answer,
     statement with the harmony choir; crunch zone late ─────────────── */
  emit(&impP, S_CHOR2, 0.25, 38, 1.0, PAN_KICK);
  for (int k = 0; k < 12; k++) {
    double b0 = S_CHOR2 + 4.0 * k;
    drop_bar(b0, k, 5, 1);
    if (k == 0) robot_hook(b0, 1, 0);
    if (k == 4) robot_hook(b0, 1, 1);
    if (k == 8) robot_hook(b0, 1, 2);                 /* the choir */
    if (k % 4 == 0) emit(&gongP, b0, 4.0, 48, 0.88, (k % 8) ? 0.3 : -0.3);
    if (k % 4 == 2) { goat_riff2(b0, 5); bell_run(b0, 5); }
    else if (k % 4 != 3) goat_riff(b0, 5, (k + 1) % 2);
  }
  emit(&washP, S_OUTRO - 2.0, 4.0, 55, 0.75, 0.5);

  /* ── OUTRO (224-256): the room empties, the gong has the last word ─ */
  emit(&gongP, S_OUTRO, 6.0, 43, 0.90, 0.0);
  for (int b = 0; b < 12; b++) {
    double t = 1.0 - (double)b / 11.0;
    emit(&kickP, S_OUTRO + b, 0.25, 36, 0.36 + 0.34 * t, PAN_KICK);
  }
  emit(&droneP, S_OUTRO, 15.0, 43, 0.36, -0.45);
  emit(&droneP, S_OUTRO + 0.4, 14.0, 31, 0.32, 0.3);  /* G1 floor */
  bleat(S_OUTRO + 3.0, 55, 3, 0.90);                  /* last growl */
  emit(&goatP, S_OUTRO + 5.5, 5.0, 55, 0.55, PAN_GOAT); /* long clean fall */
  emit(&robotP, S_OUTRO + 8.0, 4.0, 70, 0.52, PAN_ROBOT); /* goodnight */
  emit(&robotP, S_OUTRO + 14.0, 5.0, 67, 0.44, PAN_ROBOT);
  bell_run(S_OUTRO + 16.0, 0);
  emit(&gongP, S_OUTRO + 24.0, 4.0, 55, 0.45, 0.2);
  emit(&shardP, S_OUTRO + 26.0, 0.2, 91, 0.32, 0.7);  /* last splinter */
}

/* ════ BUS TOOLS — channel-strip EQ over whole buses ═════════════════ */

enum { EQ_HP, EQ_LP, EQ_PEAK, EQ_LSH, EQ_HSH };

/* one RBJ biquad applied across a stereo bus (Audio EQ Cookbook) */
static void bus_eq(Bus *b, int type, double f, double Q, double db) {
  double A = pow(10.0, db / 40.0);
  double w0 = TAU * f / SR, cw = cos(w0), sw = sin(w0);
  double alpha = sw / (2.0 * Q);
  double b0c, b1c, b2c, a0, a1c, a2c;
  switch (type) {
  case EQ_HP:
    b0c = (1 + cw) / 2; b1c = -(1 + cw); b2c = (1 + cw) / 2;
    a0 = 1 + alpha; a1c = -2 * cw; a2c = 1 - alpha;
    break;
  case EQ_LP:
    b0c = (1 - cw) / 2; b1c = 1 - cw; b2c = (1 - cw) / 2;
    a0 = 1 + alpha; a1c = -2 * cw; a2c = 1 - alpha;
    break;
  case EQ_PEAK:
    b0c = 1 + alpha * A; b1c = -2 * cw; b2c = 1 - alpha * A;
    a0 = 1 + alpha / A; a1c = -2 * cw; a2c = 1 - alpha / A;
    break;
  case EQ_LSH: {
    double sA = sqrt(A), t = 2 * sA * alpha;
    b0c = A * ((A + 1) - (A - 1) * cw + t);
    b1c = 2 * A * ((A - 1) - (A + 1) * cw);
    b2c = A * ((A + 1) - (A - 1) * cw - t);
    a0 = (A + 1) + (A - 1) * cw + t;
    a1c = -2 * ((A - 1) + (A + 1) * cw);
    a2c = (A + 1) + (A - 1) * cw - t;
    break;
  }
  default: { /* EQ_HSH */
    double sA = sqrt(A), t = 2 * sA * alpha;
    b0c = A * ((A + 1) + (A - 1) * cw + t);
    b1c = -2 * A * ((A - 1) + (A + 1) * cw);
    b2c = A * ((A + 1) + (A - 1) * cw - t);
    a0 = (A + 1) - (A - 1) * cw + t;
    a1c = 2 * ((A - 1) - (A + 1) * cw);
    a2c = (A + 1) - (A - 1) * cw - t;
    break;
  }
  }
  b0c /= a0; b1c /= a0; b2c /= a0; a1c /= a0; a2c /= a0;
  for (int ch = 0; ch < 2; ch++) {
    float *x = ch ? b->R : b->L;
    double x1 = 0, x2 = 0, y1 = 0, y2 = 0;
    for (int i = 0; i < b->n; i++) {
      double xx = x[i];
      double y = b0c * xx + b1c * x1 + b2c * x2 - a1c * y1 - a2c * y2;
      x2 = x1; x1 = xx; y2 = y1; y1 = y;
      x[i] = (float)y;
    }
  }
}

static void bus_wipe(Bus *b) {
  memset(b->L, 0, (size_t)b->n * sizeof(float));
  memset(b->R, 0, (size_t)b->n * sizeof(float));
}
static void bus_pour(const Bus *src, Bus *dst) {
  for (int i = 0; i < dst->n; i++) { dst->L[i] += src->L[i]; dst->R[i] += src->R[i]; }
}

/* ════ MASTER STORIES — width, tape amount ═══════════════════════════ */

static double width_of(double beats) {
  if (beats < S_VERSE1) return 0.35;
  if (beats < S_PRE)    return 0.55;
  if (beats < S_CHOR1)  return 0.55 + 0.40 * (beats - S_PRE) / (S_CHOR1 - S_PRE);
  if (beats < S_VERSE2) return 1.0;
  if (beats < S_BRIDGE) return 0.62;
  if (beats < S_BUILD2) return 0.48;                 /* the corridor */
  if (beats < S_CHOR2)  return 0.48 + 0.52 * (beats - S_BUILD2) / (S_CHOR2 - S_BUILD2);
  if (beats < S_OUTRO)  return 1.0;
  return 0.6 - 0.35 * (beats - S_OUTRO) / (TRACK_BEATS - S_OUTRO);
}

/* tape print amount: full print (lo-fi) in the intro, easing through
   the verses, LIFTED at the choruses (the room turns real) */
static double tape_of(double beats) {
  if (beats < S_VERSE1) return 1.0;
  if (beats < S_PRE)    return 0.55;
  if (beats < S_CHOR1)  return 0.65;
  if (beats < S_VERSE2) return 0.28;
  if (beats < S_BRIDGE) return 0.50;
  if (beats < S_BUILD2) return 0.75;
  if (beats < S_CHOR2)  return 0.60;
  if (beats < S_OUTRO)  return 0.22;
  return 0.85;
}

/* ════ MIX + MASTER + WAV ════════════════════════════════════════════ */

static int write_wav(const char *path, const float *L, const float *R, int n) {
  FILE *f = fopen(path, "wb");
  if (!f) { fprintf(stderr, "goatshard: cannot open %s\n", path); return 1; }
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
  const char *outPath = "../out/goatshard.wav";
  for (int i = 1; i < argc; i++)
    if (!strcmp(argv[i], "--out") && i + 1 < argc) outPath = argv[++i];

  compose();
  substrate_score();
  int n = (int)(TRACK_BEATS * BEAT * SR);
  Bus drum = bus_new(n), music = bus_new(n), send = bus_new(n), rvb = bus_new(n);
  Bus strip = bus_new(n);

  nz_state = 0x9d2c5681u; /* deterministic renders */
  fprintf(stderr, "goatshard: kick %d / implo %d / ghost %d / snare %d / clap %d / click %d / wash %d\n",
          kickP.n, impP.n, ghostP.n, snrP.n, clapP.n, clickP.n, washP.n);
  fprintf(stderr, "goatshard: goat %d / robot %d / bass %d / stabs %d / gongs %d / shards %d / drones %d\n",
          goatP.n, robotP.n, bassP.n, stabP.n, gongP.n, shardP.n, droneP.n);

  /* ── channel strips: each family renders alone onto the strip, gets
     its EQ slot, then pours into its bus — the spectrum is assigned,
     not hoped for ──────────────────────────────────────────────────── */
  fprintf(stderr, "goatshard: strips\n");

  /* kicks: sub + 62 Hz punch, nothing under 24 */
  for (int i = 0; i < kickP.n; i++) memkick_note(&kickP.ev[i], &strip, &send, 1.00, 0.03);
  for (int i = 0; i < impP.n; i++) implokick_note(&impP.ev[i], &strip, &send, 0.85, 0.05);
  bus_eq(&strip, EQ_HP, 24.0, 0.707, 0);
  bus_eq(&strip, EQ_PEAK, 62.0, 1.0, 2.0);
  bus_pour(&strip, &drum); bus_wipe(&strip);

  /* snares + ghosts + claps: 210 Hz body, 5.2 kHz snap */
  for (int i = 0; i < ghostP.n; i++) cavikick_note(&ghostP.ev[i], &strip, &send, 0.50, 0.08);
  for (int i = 0; i < snrP.n; i++) wiresnare_note(&snrP.ev[i], &strip, &send, 0.80, 0.14);
  for (int i = 0; i < clapP.n; i++) gransnare_note(&clapP.ev[i], &strip, &send, 0.60, 0.16);
  bus_eq(&strip, EQ_HP, 120.0, 0.707, 0);
  bus_eq(&strip, EQ_PEAK, 210.0, 1.0, 1.5);
  bus_eq(&strip, EQ_PEAK, 5200.0, 0.9, 2.0);
  bus_pour(&strip, &drum); bus_wipe(&strip);

  /* clicks: pure top, out of everyone's way */
  for (int i = 0; i < clickP.n; i++) click_note(&clickP.ev[i], &strip, &send, 0.42, 0.05);
  bus_eq(&strip, EQ_HP, 1800.0, 0.707, 0);
  bus_pour(&strip, &drum); bus_wipe(&strip);

  /* washes: band-limited so risers never eat the mix */
  for (int i = 0; i < washP.n; i++) crackle_note(&washP.ev[i], &strip, &send, 0.50, 0.50);
  bus_eq(&strip, EQ_HP, 300.0, 0.707, 0);
  bus_eq(&strip, EQ_LP, 9500.0, 0.707, 0);
  bus_pour(&strip, &music); bus_wipe(&strip);

  /* the goat: presence at 1.8 kHz, lows ceded to the bass */
  twomass_part(&goatP, &strip, &send, 0.90, 0.15);
  bus_eq(&strip, EQ_HP, 140.0, 0.707, 0);
  bus_eq(&strip, EQ_PEAK, 1800.0, 0.8, 3.0);
  bus_eq(&strip, EQ_LP, 11000.0, 0.707, 0);
  bus_pour(&strip, &music); bus_wipe(&strip);

  /* the girl: upper-mid presence + the air shelf — she owns the top */
  for (int i = 0; i < robotP.n; i++) vosim_note(&robotP.ev[i], &strip, &send, 0.85, 0.15);
  bus_eq(&strip, EQ_HP, 280.0, 0.707, 0);
  bus_eq(&strip, EQ_PEAK, 3200.0, 0.9, 2.5);
  bus_eq(&strip, EQ_HSH, 7000.0, 0.8, 2.0);
  bus_pour(&strip, &music); bus_wipe(&strip);

  /* throat bass: sub intact, low-mid mud dipped, top capped */
  for (int i = 0; i < bassP.n; i++) throatbass_note(&bassP.ev[i], &strip, &send, 0.75, 0.03);
  bus_eq(&strip, EQ_HP, 26.0, 0.707, 0);
  bus_eq(&strip, EQ_PEAK, 90.0, 1.0, 1.5);
  bus_eq(&strip, EQ_PEAK, 380.0, 1.2, -1.5);
  bus_eq(&strip, EQ_LP, 6500.0, 0.707, 0);
  bus_pour(&strip, &music); bus_wipe(&strip);

  /* power chords: 950 Hz body — the wall lives in the mids */
  for (int i = 0; i < stabP.n; i++) chaosfm_note(&stabP.ev[i], &strip, &send, 0.62, 0.25);
  bus_eq(&strip, EQ_HP, 170.0, 0.707, 0);
  bus_eq(&strip, EQ_PEAK, 950.0, 0.9, 2.0);
  bus_pour(&strip, &music); bus_wipe(&strip);

  /* gongs + bells: shimmer shelf */
  for (int i = 0; i < gongP.n; i++) gongbell_note(&gongP.ev[i], &strip, &send, 0.80, 0.35);
  bus_eq(&strip, EQ_HP, 85.0, 0.707, 0);
  bus_eq(&strip, EQ_HSH, 4500.0, 0.8, 1.5);
  bus_pour(&strip, &music); bus_wipe(&strip);

  /* glass: strictly above the vocals */
  for (int i = 0; i < shardP.n; i++) frictus_note(&shardP.ev[i], &strip, &send, 0.50, 0.35);
  bus_eq(&strip, EQ_HP, 1200.0, 0.707, 0);
  bus_pour(&strip, &music); bus_wipe(&strip);

  /* drones: the 200-600 Hz filler nobody else wants, capped on top */
  for (int i = 0; i < droneP.n; i++) frictus_note(&droneP.ev[i], &strip, &send, 0.42, 0.50);
  bus_eq(&strip, EQ_HP, 85.0, 0.707, 0);
  bus_eq(&strip, EQ_LP, 4200.0, 0.707, 0);
  bus_pour(&strip, &music); bus_wipe(&strip);

  fprintf(stderr, "goatshard: sidechain\n");
  double *duck = malloc(sizeof(double) * (size_t)n);
  if (!duck) exit(1);
  carve_duck(duck, n, &kickP, &impP);

  fprintf(stderr, "goatshard: reverb\n");
  reverb(&send, &rvb);

  /* the pump: drums dry, music ducked, reverb half-ducked */
  Bus master = bus_new(n);
  for (int i = 0; i < n; i++) {
    double d = duck[i];
    master.L[i] = drum.L[i] + (float)(music.L[i] * d + rvb.L[i] * (0.55 + 0.45 * d));
    master.R[i] = drum.R[i] + (float)(music.R[i] * d + rvb.R[i] * (0.55 + 0.45 * d));
  }

  fprintf(stderr, "goatshard: substrate (%d carved bands)\n", nSub);
  substrate(&master, duck);

  /* ── story pass: width -> crunch zone + self-scratch -> tape print ─ */
  fprintf(stderr, "goatshard: width / crunch / tape print\n");
  const double SDRV = 1.6, SBIAS = 0.22, SHISS = 0.0030, SNORM = tanh(1.6);
  uint32_t hss = 0x51ed2701u;
  double hlpL = 0, hlpR = 0;
  const double HLP = 0.22;
  /* crunch zone: late final chorus, bars 10-11 */
  const double ZC = 218.0 * BEAT, ZW = 2.6;
  #define ZMASK 16383
  float *zbufL = calloc(ZMASK + 1, 4), *zbufR = calloc(ZMASK + 1, 4);
  double zheldL = 0, zheldR = 0; int zhc = 0;
  double widS = 0.35, saS = 1.0;
  static const struct { double t, dur, freq, depth; } SCR[3] = {
    { 220.5 * BEAT, 0.35, 3.0, 0.15 },
    { 222.75 * BEAT, 0.30, 4.2, 0.11 },
    { 240.0 * BEAT, 0.50, 2.0, 0.22 },
  };

  for (int i = 0; i < n; i++) {
    double tsec = (double)i / SR, beats = tsec / BEAT;
    double L = master.L[i], R = master.R[i];

    /* width story (one-pole smoothed so doors never click) */
    widS += (width_of(beats) - widS) * 0.00015;
    double m = 0.5 * (L + R), sd = 0.5 * (L - R) * widS;
    L = m + sd; R = m - sd;

    /* crunch zone: sample-hold bitcrush + flange taps from the mix's own
       recent past, throbbing across the field */
    {
      double za = 1.0 - fabs(tsec - ZC) / ZW;
      if (za > 0) {
        za = za * za * (3.0 - 2.0 * za);
        if (--zhc <= 0) { zheldL = L; zheldR = R; zhc = 1 + (int)(6.0 * za); }
        const double qs = 18.0;
        double cl = floor(zheldL * qs + 0.5) / qs;
        double cr = floor(zheldR * qs + 0.5) / qs;
        long fdL = (long)((0.0012 + 0.0024 * (0.5 + 0.5 * sin(TAU * 0.27 * tsec))) * SR);
        long fdR = (long)((0.0012 + 0.0024 * (0.5 + 0.5 * sin(TAU * 0.27 * tsec + 1.5708))) * SR);
        double flL = i > fdL ? zbufL[(i - fdL) & ZMASK] : 0;
        double flR = i > fdR ? zbufR[(i - fdR) & ZMASK] : 0;
        double amt = 0.55 * za;
        L = L * (1.0 - amt) + (cl * 0.7 + flL * 0.5) * amt;
        R = R * (1.0 - amt) + (cr * 0.7 + flR * 0.5) * amt;
      }
      zbufL[i & ZMASK] = (float)L;
      zbufR[i & ZMASK] = (float)R;
    }

    /* self-scratch: the playhead dragged through the mix's own past */
    for (int sg = 0; sg < 3; sg++) {
      if (tsec < SCR[sg].t || tsec >= SCR[sg].t + SCR[sg].dur) continue;
      double ph = (tsec - SCR[sg].t) / SCR[sg].dur;
      double env2 = sin(M_PI * ph);
      double off = SCR[sg].depth * (0.5 - 0.5 * cos(TAU * SCR[sg].freq * (tsec - SCR[sg].t)));
      long j2 = i - (long)(off * SR);
      if (j2 > 0) {
        L = L * (1.0 - env2) + zbufL[j2 & ZMASK] * env2 * 1.1;
        R = R * (1.0 - env2) + zbufR[j2 & ZMASK] * env2 * 1.1;
      }
      break;
    }

    /* tape print: drive + bias vs clean, blended by the section amount,
       hiss floor riding the same knob */
    saS += (tape_of(beats) - saS) * 0.00015;
    double pl = (tanh(L * 0.95 * SDRV + SBIAS) - tanh(SBIAS)) / SNORM;
    double pr = (tanh(R * 0.95 * SDRV + SBIAS) - tanh(SBIAS)) / SNORM;
    double cl0 = tanh(L * 0.95), cr0 = tanh(R * 0.95);
    double vl = cl0 + (pl - cl0) * saS;
    double vr = cr0 + (pr - cr0) * saS;
    hss ^= hss << 13; hss ^= hss >> 17; hss ^= hss << 5;
    double n1 = ((double)(hss >> 8) / 8388608.0) - 1.0;
    hss ^= hss << 13; hss ^= hss >> 17; hss ^= hss << 5;
    double n2 = ((double)(hss >> 8) / 8388608.0) - 1.0;
    hlpL += (n1 - hlpL) * HLP;
    hlpR += (n2 - hlpR) * HLP;
    master.L[i] = (float)(vl + hlpL * SHISS * saS);
    master.R[i] = (float)(vr + hlpR * SHISS * saS);
  }

  /* ── MASTER, first pass: rumble filter -> smile EQ -> bus compressor
     -> tanh glue -> -0.5 dBFS ──────────────────────────────────────── */
  fprintf(stderr, "goatshard: master\n");
  bus_eq(&master, EQ_HP, 20.0, 0.707, 0);
  bus_eq(&master, EQ_LSH, 90.0, 0.8, 1.5);
  bus_eq(&master, EQ_HSH, 8500.0, 0.8, 1.5);

  /* stereo bus compressor: 2.5:1 over -14 dBFS, 12 ms / 180 ms */
  {
    double envc = 0.0;
    const double aA = exp(-1.0 / (0.012 * SR)), aR = exp(-1.0 / (0.180 * SR));
    const double thr = pow(10.0, -14.0 / 20.0), slope = 1.0 - 1.0 / 2.5;
    for (int i = 0; i < n; i++) {
      double L = master.L[i], R = master.R[i];
      double aL = fabs(L), aR2 = fabs(R);
      double x = aL > aR2 ? aL : aR2;
      envc = (x > envc) ? aA * envc + (1.0 - aA) * x
                        : aR * envc + (1.0 - aR) * x;
      double g = (envc > thr) ? pow(envc / thr, -slope) : 1.0;
      master.L[i] = (float)(L * g);
      master.R[i] = (float)(R * g);
    }
  }

  /* glue + normalize (a club master leaves little headroom) */
  double peak = 0;
  for (int i = 0; i < n; i++) {
    double vl = tanh(master.L[i] * 1.25) / 1.25;
    double vr = tanh(master.R[i] * 1.25) / 1.25;
    master.L[i] = (float)vl;
    master.R[i] = (float)vr;
    double a = fabs(vl); if (a > peak) peak = a;
    a = fabs(vr); if (a > peak) peak = a;
  }
  if (peak > 0) {
    double g = pow(10.0, -0.5 / 20.0) / peak;
    for (int i = 0; i < n; i++) { master.L[i] *= (float)g; master.R[i] *= (float)g; }
  }

  if (write_wav(outPath, master.L, master.R, n)) return 1;
  fprintf(stderr, "goatshard: wrote %s (%.1fs, pre-norm peak %.3f)\n",
          outPath, (double)n / SR, peak);
  return 0;
}
