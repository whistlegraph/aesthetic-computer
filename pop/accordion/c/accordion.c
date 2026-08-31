// accordion.c — air-powered physically-modeled accordion (free-reed synthesis).
//
// Build: ./build.sh   (cc -O2 -std=c11 -Wall -Wextra -o accordion accordion.c -lm)
//
// The sound source is a FREE REED: a cantilevered steel tongue beating through
// a close-fitting slot, driven into self-oscillation by bellows pressure. The
// reed is modeled as a van-der-Pol-type nonlinear oscillator (the standard
// reduction of the Fletcher/Rossing free-reed treatment): below a threshold
// pressure the airflow cannot overcome the reed's own damping and it stays
// silent; above it, the flow feeds energy into the swing each cycle and the
// oscillation GROWS from turbulence noise until the nonlinear damping
// saturates it. That growth-from-nothing is the accordion's breathing attack —
// it is not an amplitude envelope, it is the reed actually taking time to
// speak, faster at high pressure, slower at low.
//
// The radiated sound is not the reed displacement: it is the time derivative
// of the volume flow through the slot. The slot aperture nearly CLOSES once
// per swing as the tongue passes through the frame plane (|x - gate| kink),
// gating the flow into asymmetric pulses — that flow-gating nonlinearity is
// what makes a reed buzzy (dense odd+even harmonic comb), and pressure pushes
// a static deflection into the reed so the pulse asymmetry (and the evens)
// grow with how hard you pump.
//
// Sections:
//   1. RNG + note parsing + WAV writer
//   2. Bellows (pressure envelope, tremor, push/pull/swell/shake, release)
//   3. Reed (van-der-Pol self-oscillator + aperture flow gate)
//   4. Body (box + grille resonances, key click, exhale)
//   5. Render + CLI
//
// Physical touches, all pressure-coupled through ONE shared bellows:
//   - attack: air takes time to build (pressure RC) AND the reed takes time
//     to speak (self-oscillation growth rate ~ pressure above threshold)
//   - tremor: 4-6 Hz hand wobble on the pressure, depth = --tremor; because
//     pitch and level both follow pressure, the note breathes in both
//   - reeds flatten under pressure (a few cents at full pumping)
//   - musette: 2-3 reeds per note, detuned (wet tuning) -> slow beating
//   - --chord renders several notes off the SAME bellows: one attack, one
//     tremor, one release for the whole handful of reeds
//
// No dependencies beyond libm.

#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif
#define TAU (2.0 * M_PI)

// ===========================================================================
// 1. RNG + note parsing + WAV writer
// ===========================================================================

static uint32_t g_rng = 0x9e3779b9u;
static void rng_seed(uint32_t s) { g_rng = s ? s : 1u; }
static double frand(void) { // uniform [-1, 1)
  g_rng ^= g_rng << 13;
  g_rng ^= g_rng >> 17;
  g_rng ^= g_rng << 5;
  return ((double)g_rng / 4294967296.0) * 2.0 - 1.0;
}

// Accept "A4", "C#5", "Db3", or a bare number (Hz). Same contract as bell.c.
static double note_to_freq(const char *s) {
  char *end;
  double num = strtod(s, &end);
  if (end != s && *end == '\0') return num;
  int sem;
  switch (s[0]) {
    case 'C': sem = 0; break;
    case 'D': sem = 2; break;
    case 'E': sem = 4; break;
    case 'F': sem = 5; break;
    case 'G': sem = 7; break;
    case 'A': sem = 9; break;
    case 'B': sem = 11; break;
    default: return 440.0;
  }
  int i = 1;
  if (s[i] == '#') { sem++; i++; }
  else if (s[i] == 'b') { sem--; i++; }
  int oct = atoi(&s[i]);
  int midi = (oct + 1) * 12 + sem;
  return 440.0 * pow(2.0, (midi - 69) / 12.0);
}

static void write_wav_f32_stereo(const char *path, const float *L,
                                 const float *R, long n, int sr) {
  FILE *f = fopen(path, "wb");
  if (!f) { fprintf(stderr, "accordion: cannot write %s\n", path); return; }
  int ch = 2, bits = 32;
  int byteRate = sr * ch * bits / 8;
  int blockAlign = ch * bits / 8;
  long dataBytes = n * ch * (bits / 8);
  fwrite("RIFF", 1, 4, f);
  uint32_t riff = 36 + (uint32_t)dataBytes;
  fwrite(&riff, 4, 1, f);
  fwrite("WAVE", 1, 4, f);
  fwrite("fmt ", 1, 4, f);
  uint32_t fmtlen = 16;
  uint16_t fmt = 3; // IEEE float
  uint16_t chs = (uint16_t)ch;
  uint32_t srr = (uint32_t)sr;
  uint16_t ba = (uint16_t)blockAlign, bps = (uint16_t)bits;
  uint32_t br = (uint32_t)byteRate;
  fwrite(&fmtlen, 4, 1, f);
  fwrite(&fmt, 2, 1, f);
  fwrite(&chs, 2, 1, f);
  fwrite(&srr, 4, 1, f);
  fwrite(&br, 4, 1, f);
  fwrite(&ba, 2, 1, f);
  fwrite(&bps, 2, 1, f);
  fwrite("data", 1, 4, f);
  uint32_t dlen = (uint32_t)dataBytes;
  fwrite(&dlen, 4, 1, f);
  for (long i = 0; i < n; i++) {
    fwrite(&L[i], 4, 1, f);
    fwrite(&R[i], 4, 1, f);
  }
  fclose(f);
}

// ===========================================================================
// 2. Bellows — the shared air supply
// ===========================================================================
//
// p(t) in [0..1] is the pressure differential across the reed bank, built
// from: a shape envelope (push / pull / swell / shake), an attack RC (air
// takes time to build behind the pallets), hand tremor (a drifting 4-6 Hz
// wobble plus a slow random-walk unsteadiness), and a release (pallet closes,
// residual pressure bleeds off as a little exhale).

typedef enum { BELLOWS_PUSH, BELLOWS_PULL, BELLOWS_SWELL, BELLOWS_SHAKE } BellowsShape;

typedef struct {
  BellowsShape shape;
  double vel;       // 0..1 bellows force
  double dur;       // note duration (s) — release begins near the end
  double t_rel;     // when the pallet closes
  double tremor;    // 0..1 hand-tremor depth control
  // state
  double p;         // smoothed pressure (the RC "air builds up" pole)
  double trem_ph;   // tremor LFO phase
  double trem_f;    // current tremor frequency (drifts)
  double walk;      // slow random-walk pressure unsteadiness
  double walk_lp;
  double rel_p;     // pressure captured at release (drives the exhale)
  int released;
} Bellows;

static void bellows_init(Bellows *b, BellowsShape shape, double vel,
                         double dur, double tremor) {
  b->shape = shape;
  b->vel = vel;
  b->dur = dur;
  // Pallet closes a little before nominal end so the tail fits inside dur+tail.
  double lead = dur * 0.08;
  if (lead < 0.10) lead = 0.10;
  if (lead > 0.30) lead = 0.30;
  b->t_rel = (shape == BELLOWS_SWELL) ? dur : dur - lead;
  b->tremor = tremor;
  b->p = 0.0;
  b->trem_ph = frand() * 0.5 + 0.5; // random start phase
  b->trem_f = 5.0;
  b->walk = 0.0;
  b->walk_lp = 0.0;
  b->rel_p = 0.0;
  b->released = 0;
}

// Target (pre-RC) pressure from the gesture shape alone.
static double bellows_target(const Bellows *b, double t) {
  double v = b->vel;
  switch (b->shape) {
    case BELLOWS_PUSH:
      // Firm start, small sag as the arm settles.
      return v * (1.0 - 0.10 * (1.0 - exp(-t / 0.8)));
    case BELLOWS_PULL:
      // Opens gently, leans in: slight crescendo across the note.
      return v * (0.86 + 0.14 * (1.0 - exp(-t / (0.35 * b->dur + 1e-9))));
    case BELLOWS_SWELL: {
      // Hairpin < > : raised-cosine up to a peak ~55% in, back down.
      double u = t / b->dur;
      if (u < 0.0) u = 0.0;
      if (u > 1.0) u = 1.0;
      double peak = 0.55;
      double s = (u < peak) ? 0.5 - 0.5 * cos(M_PI * u / peak)
                            : 0.5 + 0.5 * cos(M_PI * (u - peak) / (1.0 - peak));
      return v * (0.04 + 0.96 * s);
    }
    case BELLOWS_SHAKE: {
      // Fast push/pull alternation (~3.6 Hz direction flips): pressure dips
      // toward zero at each turnaround, so the reeds re-speak every stroke.
      double fsh = 3.6;
      double c = sin(TAU * fsh * t);
      double mag = pow(fabs(c), 1.35); // real dwell near zero at each turnaround
      double dir = (c >= 0.0) ? 1.0 : 0.92; // pull strokes a touch weaker
      return v * (0.03 + 0.97 * mag) * dir;
    }
  }
  return 0.0;
}

// One control-rate step of the bellows. Returns pressure p in [0..~1].
static double bellows_step(Bellows *b, double t, double dt) {
  double target = bellows_target(b, t);

  // Attack RC: air takes time to build. Pull opens slower than push.
  double tau_a = (b->shape == BELLOWS_PULL) ? 0.10 : 0.055;
  if (b->shape == BELLOWS_SWELL) tau_a = 0.035; // the shape itself is slow
  if (b->shape == BELLOWS_SHAKE) tau_a = 0.020; // strokes must articulate
  double tau = (target > b->p) ? tau_a : 0.045; // vent-down slightly quick

  // Release: pallet closes, pressure bleeds fast.
  if (t >= b->t_rel) {
    if (!b->released) { b->released = 1; b->rel_p = b->p; }
    target = 0.0;
    tau = 0.050;
  }
  b->p += (target - b->p) * (1.0 - exp(-dt / tau));

  // Hand tremor: drifting 4-6 Hz LFO + slow random walk. Depth scales with
  // pressure (you can't wobble air you aren't moving).
  double depth = 0.20 * b->tremor;
  b->trem_f += (5.0 + 1.2 * b->walk_lp - b->trem_f) * dt * 2.0; // drift 4-6 Hz
  b->trem_ph += b->trem_f * dt;
  if (b->trem_ph >= 1.0) b->trem_ph -= 1.0;
  b->walk += frand() * dt * 8.0;
  b->walk -= b->walk * dt * 2.0; // leaky
  b->walk_lp += (b->walk - b->walk_lp) * dt * 6.0;
  double wob = sin(TAU * b->trem_ph) * depth + b->walk_lp * 0.06 * b->tremor;

  double p = b->p * (1.0 + wob);
  if (p < 0.0) p = 0.0;
  if (p > 1.25) p = 1.25;
  return p;
}

// ===========================================================================
// 3. Reed — van-der-Pol free reed + aperture flow gate
// ===========================================================================
//
//   x'' = -w(p)^2 (x - x_off(p)) + mu(p) (1 - x^2/xsat^2) x' + noise
//
//   mu(p)   = w * (drive * p - zeta0)      negative below threshold: silence;
//                                          positive above: grows to limit cycle
//   x_off(p)= static pressure deflection   -> pulse asymmetry, even harmonics
//   w(p)    = w0 * 2^(-flat_cents * p/1200)  reeds flatten under pressure
//
//   flow U  = (a_min + |x - gate|) * sqrt(p)   the slot nearly closes once per
//                                              swing: the buzz
//   out     ~ dU/dt / w0                       radiation is flow derivative;
//                                              /w0 levels loudness across pitch
//
// Integrated with semi-implicit Euler at OVERSAMPLE * sr.

#define OVERSAMPLE 4

typedef struct {
  double w0;        // natural angular frequency (rad/s), musette detune baked in
  double x, v;      // displacement, velocity (xsat units)
  double drive;     // per-reed pressure->negative-damping gain (small spread)
  double zeta0;     // intrinsic damping fraction of drive threshold
  double gate;      // aperture-minimum position in the swing
  double amin;      // leakage aperture at closest approach
  double xoff_max;  // static deflection at full pressure
  double flat_cents;// pitch drop at full pressure
  double u_prev;    // previous flow sample (for the derivative)
  double pan;       // -1..1
  double gain;
} Reed;

static void reed_init(Reed *r, double hz, double cents, double pan, double gain) {
  r->w0 = TAU * hz * pow(2.0, cents / 1200.0);
  r->x = frand() * 1e-4; // seed asymmetry; the real start-up comes from noise
  r->v = 0.0;
  r->drive = 0.30 * (1.0 + 0.05 * frand()); // per-reed spread: onsets desync
  r->zeta0 = 0.022;
  // The slot is at the frame plane, just below the rest position — any
  // speaking amplitude swings through it, so soft notes still buzz.
  r->gate = -0.15 * (1.0 + 0.25 * frand());
  r->amin = 0.10;
  r->xoff_max = 0.38;
  r->flat_cents = 6.5 * (1.0 + 0.15 * frand());
  r->u_prev = 0.0;
  r->pan = pan;
  r->gain = gain;
}

// One audio-rate sample of one reed under pressure p. Returns radiated signal.
static double reed_step(Reed *r, double p, double sr) {
  double dt = 1.0 / (sr * OVERSAMPLE);
  double w = r->w0 * pow(2.0, -(r->flat_cents * p) / 1200.0);
  double xoff = r->xoff_max * p;
  // Swing amplitude grows with pressure: the limit-cycle saturation point
  // moves out as you pump harder. This is what lets the bellows tremor and
  // the swell actually be HEARD in level, not just in the flow factor.
  double X = 0.35 + 0.65 * p;
  double iX2 = 1.0 / (X * X);
  // Damping split: the airflow term (pressure-driven, amplitude-limited) can
  // go negative and feed the swing; the intrinsic reed loss (zeta0) is ALWAYS
  // dissipative. Folding them into one van-der-Pol mu is unstable at release:
  // mu < 0 with |x| > X flips (1 - x^2/X^2) into anti-damping and explodes.
  double gain_air = w * r->drive;
  double na = 2e-3 * sqrt(p > 0 ? p : 0); // turbulence kicks the reed awake
  for (int k = 0; k < OVERSAMPLE; k++) {
    double xr = r->x;
    double damp = gain_air * (p * (1.0 - xr * xr * iX2) - r->zeta0);
    double acc = -w * w * (xr - xoff) + damp * r->v + w * w * na * frand();
    r->v += dt * acc;
    r->x += dt * r->v;
  }
  // Flow through the slot; kinked aperture = the buzz. Asymmetric: above the
  // frame plane the gap opens freely; below it the tongue sits INSIDE the
  // slot and the gap barely reopens — one real closure per cycle, so the
  // fundamental stays on top of a dense odd+even comb (a symmetric |x| fold
  // would put the octave above the fundamental).
  double d = r->x - r->gate;
  double ap = r->amin + (d > 0.0 ? d : -0.18 * d);
  double u = ap * sqrt(p > 0 ? p : 0);
  double y = (u - r->u_prev) * sr / r->w0; // radiation ~ dU/dt, leveled by w0
  r->u_prev = u;
  return y * r->gain;
}

// ===========================================================================
// 4. Body — wooden box + grille, key click, exhale
// ===========================================================================
//
// A light fixed-resonance coloration: a few 2-pole resonators (the cassotto /
// box / grille bumps) mixed with the direct reed sound, a DC-blocker, and a
// one-pole top-end rolloff (the grille cloth). Two independent copies, L/R.

typedef struct { double b0, b1, b2, a1, a2, z1, z2; } Biquad;

static void bp_set(Biquad *q, double f, double Q, double sr) {
  double w = TAU * f / sr;
  double alpha = sin(w) / (2.0 * Q);
  double a0 = 1.0 + alpha;
  q->b0 = alpha / a0;
  q->b1 = 0.0;
  q->b2 = -alpha / a0;
  q->a1 = -2.0 * cos(w) / a0;
  q->a2 = (1.0 - alpha) / a0;
  q->z1 = q->z2 = 0.0;
}

static double bq_run(Biquad *q, double in) {
  double out = q->b0 * in + q->z1;
  q->z1 = q->b1 * in - q->a1 * out + q->z2;
  q->z2 = q->b2 * in - q->a2 * out;
  return out;
}

#define BODY_RES 4

typedef struct {
  Biquad res[BODY_RES];
  double gains[BODY_RES];
  double hp_x1, hp_y1;  // DC blocker
  double lp;            // grille rolloff
  double lp_a;
} Body;

static void body_init(Body *b, double sr) {
  // f (Hz), Q, gain — small box thump, wooden midrange, grille presence, air.
  const double f[BODY_RES] = {215.0, 540.0, 1350.0, 2900.0};
  const double Q[BODY_RES] = {1.8, 2.6, 3.5, 4.0};
  const double g[BODY_RES] = {2.6, 2.0, 1.5, 0.9};
  for (int i = 0; i < BODY_RES; i++) {
    bp_set(&b->res[i], f[i], Q[i], sr);
    b->gains[i] = g[i];
  }
  b->hp_x1 = b->hp_y1 = 0.0;
  b->lp = 0.0;
  b->lp_a = 1.0 - exp(-TAU * 6500.0 / sr);
}

static double body_run(Body *b, double in) {
  double y = 0.55 * in;
  for (int i = 0; i < BODY_RES; i++) y += b->gains[i] * bq_run(&b->res[i], in);
  // DC block ~25 Hz.
  double hp = y - b->hp_x1 + 0.9967 * b->hp_y1;
  b->hp_x1 = y;
  b->hp_y1 = hp;
  // Grille cloth rolloff.
  b->lp += (hp - b->lp) * b->lp_a;
  return b->lp;
}

// ===========================================================================
// 5. Render + CLI
// ===========================================================================

#define MAX_NOTES 12
#define MAX_VOICES 3
#define MAX_REEDS (MAX_NOTES * MAX_VOICES)

static void usage(void) {
  fprintf(stderr,
    "accordion — air-powered physically-modeled free-reed instrument\n"
    "usage: accordion [options] --out file.wav\n"
    "  --note  N        note name (A4, C#3) or use --hz (default A4)\n"
    "  --hz    F        fundamental in Hz (overrides --note)\n"
    "  --chord \"A3,C#4,E4\"  several notes sharing ONE bellows\n"
    "  --dur   S        note length in seconds (default 4)\n"
    "  --vel   0..1     bellows force (default 0.8)\n"
    "  --voices 1|2|3   reeds per note, musette (default 2)\n"
    "  --detune C       musette detune in cents (default 12)\n"
    "  --bellows push|pull|swell|shake   gesture shape (default push)\n"
    "  --tremor 0..1    hand-tremor depth (default 0.35)\n"
    "  --sr    HZ       sample rate (default 48000)\n"
    "  --seed  N        RNG seed (default 1)\n"
    "  --out   FILE     output wav (float32 stereo)\n");
}

int main(int argc, char **argv) {
  const char *note = "A4", *chord = NULL, *out = NULL;
  double hz = 0.0, dur = 4.0, vel = 0.8, detune = 12.0, tremor = 0.35;
  double sr = 48000.0;
  int voices = 2;
  uint32_t seed = 1;
  BellowsShape shape = BELLOWS_PUSH;

  for (int i = 1; i < argc; i++) {
    if (strcmp(argv[i], "--note") == 0 && i + 1 < argc) note = argv[++i];
    else if (strcmp(argv[i], "--hz") == 0 && i + 1 < argc) hz = atof(argv[++i]);
    else if (strcmp(argv[i], "--chord") == 0 && i + 1 < argc) chord = argv[++i];
    else if (strcmp(argv[i], "--dur") == 0 && i + 1 < argc) dur = atof(argv[++i]);
    else if (strcmp(argv[i], "--vel") == 0 && i + 1 < argc) vel = atof(argv[++i]);
    else if (strcmp(argv[i], "--voices") == 0 && i + 1 < argc) voices = atoi(argv[++i]);
    else if (strcmp(argv[i], "--detune") == 0 && i + 1 < argc) detune = atof(argv[++i]);
    else if (strcmp(argv[i], "--tremor") == 0 && i + 1 < argc) tremor = atof(argv[++i]);
    else if (strcmp(argv[i], "--sr") == 0 && i + 1 < argc) sr = atof(argv[++i]);
    else if (strcmp(argv[i], "--seed") == 0 && i + 1 < argc) seed = (uint32_t)atoi(argv[++i]);
    else if (strcmp(argv[i], "--out") == 0 && i + 1 < argc) out = argv[++i];
    else if (strcmp(argv[i], "--bellows") == 0 && i + 1 < argc) {
      const char *s = argv[++i];
      if (strcmp(s, "push") == 0) shape = BELLOWS_PUSH;
      else if (strcmp(s, "pull") == 0) shape = BELLOWS_PULL;
      else if (strcmp(s, "swell") == 0) shape = BELLOWS_SWELL;
      else if (strcmp(s, "shake") == 0) shape = BELLOWS_SHAKE;
      else { fprintf(stderr, "accordion: unknown bellows '%s'\n", s); return 2; }
    }
    else if (strcmp(argv[i], "--help") == 0) { usage(); return 0; }
    else { fprintf(stderr, "accordion: unknown arg %s\n", argv[i]); usage(); return 2; }
  }
  if (!out) { usage(); return 2; }
  if (voices < 1) voices = 1;
  if (voices > MAX_VOICES) voices = MAX_VOICES;
  if (vel < 0.05) vel = 0.05;
  if (vel > 1.0) vel = 1.0;
  if (tremor < 0.0) tremor = 0.0;
  if (tremor > 1.0) tremor = 1.0;
  rng_seed(seed);

  // ---- collect the notes ----
  double freqs[MAX_NOTES];
  int nnotes = 0;
  if (chord) {
    char buf[256];
    snprintf(buf, sizeof(buf), "%s", chord);
    char *tok = strtok(buf, ", ");
    while (tok && nnotes < MAX_NOTES) {
      freqs[nnotes++] = note_to_freq(tok);
      tok = strtok(NULL, ", ");
    }
  } else {
    freqs[nnotes++] = (hz > 0.0) ? hz : note_to_freq(note);
  }
  if (nnotes == 0) { fprintf(stderr, "accordion: no notes\n"); return 2; }

  // ---- build the reed bank (musette: flat / on / sharp) ----
  Reed reeds[MAX_REEDS];
  int nreeds = 0;
  double note_gain = 1.0 / sqrt((double)(nnotes * voices));
  for (int n = 0; n < nnotes; n++) {
    double npan = (nnotes > 1) ? 0.3 * ((double)n / (nnotes - 1) - 0.5) : 0.0;
    for (int vv = 0; vv < voices; vv++) {
      double cents = 0.0, vpan = 0.0;
      if (voices == 2) { cents = (vv == 0) ? 0.0 : detune; vpan = (vv == 0) ? -0.22 : 0.22; }
      else if (voices == 3) { cents = (vv - 1) * detune; vpan = (vv - 1) * 0.26; }
      reed_init(&reeds[nreeds++], freqs[n], cents, npan + vpan, note_gain);
    }
  }

  // ---- allocate ----
  double tail = 0.5;
  long N = (long)((dur + tail) * sr);
  float *L = calloc((size_t)N, sizeof(float));
  float *R = calloc((size_t)N, sizeof(float));
  if (!L || !R) { fprintf(stderr, "accordion: alloc failed\n"); return 1; }

  Bellows bel;
  bellows_init(&bel, shape, vel, dur, tremor);
  Body bodyL, bodyR;
  body_init(&bodyL, sr);
  body_init(&bodyR, sr);

  // Key-click / valve-thump ingredients.
  double click_lp = 0.0;
  double thump_ph = 0.0;
  long click_len = (long)(0.012 * sr);
  // Exhale (release breath) state.
  double exhale_lp = 0.0;
  long rel_start = -1;
  double breath_lp = 0.0; // sustained airflow turbulence

  double dt = 1.0 / sr;
  for (long i = 0; i < N; i++) {
    double t = (double)i * dt;
    double p = bellows_step(&bel, t, dt);

    // Reed bank — everyone drinks from the same bellows.
    double sl = 0.0, srg = 0.0;
    for (int k = 0; k < nreeds; k++) {
      double y = reed_step(&reeds[k], p, sr);
      double gl = 0.5 * (1.0 - reeds[k].pan);
      double gr = 0.5 * (1.0 + reeds[k].pan);
      sl += y * gl;
      srg += y * gr;
    }

    // Airflow turbulence: soft LP noise scaled by pressure. The breath.
    breath_lp += (frand() - breath_lp) * 0.18; // ~1.5 kHz-ish shading
    double breath = breath_lp * 0.020 * p * sqrt(p);
    sl += breath;
    srg += breath;

    // Key click + valve thump at note start (mechanical, pre-air).
    if (i < click_len) {
      click_lp += (frand() - click_lp) * 0.45;
      double cenv = exp(-t / 0.0028);
      double click = 0.10 * vel * cenv * click_lp;
      thump_ph += 78.0 * dt;
      double thump = 0.05 * vel * exp(-t / 0.010) * sin(TAU * thump_ph);
      sl += click + thump;
      srg += click + thump;
    }

    // The little exhale when the pallet closes.
    if (bel.released && rel_start < 0) rel_start = i;
    if (rel_start >= 0) {
      double tr = (double)(i - rel_start) * dt;
      exhale_lp += (frand() - exhale_lp) * 0.22;
      double ex = 0.055 * bel.rel_p * exp(-tr / 0.10) * exhale_lp;
      sl += ex;
      srg += ex;
    }

    L[i] = (float)body_run(&bodyL, sl);
    R[i] = (float)body_run(&bodyR, srg);
  }

  // Peak normalize to 0.9.
  double pk = 0.0;
  for (long i = 0; i < N; i++) {
    if (fabs(L[i]) > pk) pk = fabs(L[i]);
    if (fabs(R[i]) > pk) pk = fabs(R[i]);
  }
  if (pk > 1e-9) {
    double g = 0.9 / pk;
    for (long i = 0; i < N; i++) { L[i] = (float)(L[i] * g); R[i] = (float)(R[i] * g); }
  }

  write_wav_f32_stereo(out, L, R, N, (int)sr);
  const char *shname[] = {"push", "pull", "swell", "shake"};
  fprintf(stderr,
          "accordion: %d note%s x %d voice%s (%d reeds), bellows %s, vel %.2f, "
          "tremor %.2f, detune %.1fc -> %s (%.2fs @ %d Hz)\n",
          nnotes, nnotes == 1 ? "" : "s", voices, voices == 1 ? "" : "s",
          nreeds, shname[shape], vel, tremor, detune, out, (double)N / sr,
          (int)sr);
  free(L);
  free(R);
  return 0;
}
