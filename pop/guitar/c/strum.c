// strum.c — a physically-modeled strummed six-string.
//
// Build: ./build.sh   (cc -O2 -std=c11 -Wall -Wextra -o strum strum.c -lm)
//
// The sound source is SIX STRINGS ON ONE BRIDGE, played by ONE HAND. Each
// string is an extended Karplus-Strong waveguide: a fractional-delay loop
// with a one-pole loss filter (the string forgetting its highs), excited by
// a pick-position comb (a pluck at 18 % of the length cannot put energy into
// the harmonics with a node there) whose brightness follows how hard the
// pick hit. The bridge couples the strings: energy leaks from the plucked
// string into the others, so a strum rings as a body of six and not as six
// files summed. The body is three modal peaks — the dreadnought's air
// resonance near 100 Hz and the top's first two modes — over a dry path.
//
// The HAND is the part the cult track's guitar() never had. A down-stroke
// rakes from the lowest sounding string up; an up-stroke starts at the top
// and catches only the upper strings, faster, because the arm is on its way
// back. Rake time shortens with force (a hard strum is a fast strum), the
// pick DAMPS each string for a few milliseconds before it lets go (so a
// ringing string is caught, not merely re-added to), and a chord change
// lifts the fret hand — the strings choke, retune, and the wound ones squeak.
//
// Sections:
//   1. RNG + note/chord parsing + WAV writer
//   2. String (waveguide, excitation, muting)
//   3. Bridge + body (acoustic) / drive (electric)
//   4. Hand (pattern → strokes → per-string plucks)
//   5. Render + CLI
//
// Deterministic: every noise draw comes from one seeded xorshift in a fixed
// order. Same flags, same bytes. No dependencies beyond libm.

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
// 1. RNG + note/chord parsing + WAV writer
// ===========================================================================

static uint32_t g_rng = 0x9e3779b9u;
static void rng_seed(uint32_t s) { g_rng = s ? s : 1u; }
static double frand(void) { // uniform [-1, 1)
  g_rng ^= g_rng << 13;
  g_rng ^= g_rng >> 17;
  g_rng ^= g_rng << 5;
  return ((double)g_rng / 4294967296.0) * 2.0 - 1.0;
}

static double midi_hz(double m) { return 440.0 * pow(2.0, (m - 69.0) / 12.0); }

#define NSTR 6
static const int OPEN[NSTR] = {40, 45, 50, 55, 59, 64}; // E2 A2 D3 G3 B3 E4
static const char *STRNAME[NSTR] = {"E2", "A2", "D3", "G3", "B3", "E4"};

// A voicing: one midi note per string, or -1 for a string that does not sound.
typedef struct { int midi[NSTR]; char name[24]; } Voicing;

// Open-position shapes the hand knows by heart. -1 = muted string.
static const struct { const char *name; int m[NSTR]; } OPEN_SHAPES[] = {
  {"E",     {40, 47, 52, 56, 59, 64}}, {"Em",    {40, 47, 52, 55, 59, 64}},
  {"E7",    {40, 47, 50, 56, 59, 64}}, {"Em7",   {40, 47, 50, 55, 59, 64}},
  {"A",     {-1, 45, 52, 57, 61, 64}}, {"Am",    {-1, 45, 52, 57, 60, 64}},
  {"A7",    {-1, 45, 52, 55, 61, 64}}, {"Am7",   {-1, 45, 52, 55, 60, 64}},
  {"Asus2", {-1, 45, 52, 57, 59, 64}}, {"Asus4", {-1, 45, 52, 57, 62, 64}},
  {"D",     {-1, -1, 50, 57, 62, 66}}, {"Dm",    {-1, -1, 50, 57, 62, 65}},
  {"D7",    {-1, -1, 50, 57, 60, 66}}, {"Dmaj7", {-1, -1, 50, 57, 61, 66}},
  {"Dsus2", {-1, -1, 50, 57, 62, 64}}, {"Dsus4", {-1, -1, 50, 57, 62, 67}},
  {"G",     {43, 47, 50, 55, 59, 67}}, {"G7",    {43, 47, 50, 55, 59, 65}},
  {"C",     {-1, 48, 52, 55, 60, 64}}, {"Cmaj7", {-1, 48, 52, 55, 59, 64}},
  {"Cadd9", {-1, 48, 52, 55, 62, 64}}, {"C7",    {-1, 48, 52, 58, 60, 64}},
  {"F",     {41, 48, 53, 57, 60, 65}}, {"Fmaj7", {-1, -1, 53, 57, 60, 64}},
  {"Bm",    {-1, 47, 54, 59, 62, 66}}, {"B",     {-1, 47, 54, 59, 63, 66}},
  {"B7",    {-1, 47, 51, 57, 59, 66}}, {"Bm7",   {-1, 47, 54, 57, 62, 66}},
  {"F#m",   {42, 49, 54, 57, 61, 66}}, {"F#",    {42, 49, 54, 58, 61, 66}},
  {"F#m7",  {42, 49, 52, 57, 61, 66}},
};

// Everything else is a barre: E-shape for roots E..B, A-shape for C..D#.
// These are voicing tables, not fingerings — a 9 on a barre is a stretch.
static int barre(const char *root_suffix, int rootpc, Voicing *v) {
  const char *s = root_suffix;
  int E[NSTR], A[NSTR];
  #define SET6(a, b, c, d, e, f) do { E[0]=a;E[1]=b;E[2]=c;E[3]=d;E[4]=e;E[5]=f; } while (0)
  #define SET5(b, c, d, e, f)    do { A[0]=-99;A[1]=b;A[2]=c;A[3]=d;A[4]=e;A[5]=f; } while (0)
  if (!strcmp(s, ""))          { SET6(0,7,12,16,19,24); SET5(0,7,12,16,19); }
  else if (!strcmp(s, "m"))    { SET6(0,7,12,15,19,24); SET5(0,7,12,15,19); }
  else if (!strcmp(s, "7"))    { SET6(0,7,10,16,19,24); SET5(0,7,10,16,19); }
  else if (!strcmp(s, "m7"))   { SET6(0,7,10,15,19,24); SET5(0,7,10,15,19); }
  else if (!strcmp(s, "maj7")) { SET6(0,7,11,16,19,24); SET5(0,7,11,16,19); }
  else if (!strcmp(s, "sus4")) { SET6(0,7,12,17,19,24); SET5(0,7,12,17,19); }
  else if (!strcmp(s, "sus2")) { SET6(0,7,12,14,19,24); SET5(0,7,12,14,19); }
  else if (!strcmp(s, "7sus4")) { SET6(0,7,10,17,19,24); SET5(0,7,10,17,19); }
  else if (!strcmp(s, "add9")) { SET6(0,7,12,16,19,26); SET5(0,7,12,16,26); }
  else if (!strcmp(s, "9"))    { SET6(0,7,10,14,19,24); SET5(0,7,10,14,19); }
  else if (!strcmp(s, "m9"))   { SET6(0,7,10,15,19,26); SET5(0,7,10,15,26); }
  else if (!strcmp(s, "5"))    { SET6(0,7,12,-99,-99,-99); SET5(0,7,12,-99,-99); }
  else return 0;
  #undef SET6
  #undef SET5
  int fE = ((rootpc - 4) + 12) % 12; // fret of the root on the low E
  if (fE <= 7) {
    int root = 40 + fE;
    for (int i = 0; i < NSTR; i++) v->midi[i] = E[i] < -50 ? -1 : root + E[i];
  } else {
    int root = 45 + ((rootpc - 9) + 12) % 12;
    for (int i = 0; i < NSTR; i++) v->midi[i] = A[i] < -50 ? -1 : root + A[i];
  }
  return 1;
}

// Assign a list of midi notes (sorted low→high) to strings, top note first,
// each on the highest free string that reaches it — the lowest fret, which
// is where a hand would put it. Notes that fit nowhere are dropped.
static void assign_midis(const int *notes, int n, Voicing *v) {
  for (int i = 0; i < NSTR; i++) v->midi[i] = -1;
  int next = NSTR - 1;
  for (int k = n - 1; k >= 0; k--) {
    int placed = 0;
    for (int s = next; s >= 0 && !placed; s--) {
      int fret = notes[k] - OPEN[s];
      if (fret >= 0 && fret <= 15) { v->midi[s] = notes[k]; next = s - 1; placed = 1; }
    }
    if (!placed) fprintf(stderr, "strum: midi %d fits no free string, dropped\n", notes[k]);
  }
}

// "Bm" | "F#m7" | "47,54,59,62" → voicing.
static int parse_chord(const char *txt, Voicing *v) {
  snprintf(v->name, sizeof(v->name), "%s", txt);
  if (txt[0] >= '0' && txt[0] <= '9') {
    int notes[NSTR], n = 0;
    char buf[128];
    snprintf(buf, sizeof(buf), "%s", txt);
    for (char *tok = strtok(buf, ", "); tok && n < NSTR; tok = strtok(NULL, ", "))
      notes[n++] = atoi(tok);
    // the hand wants them low to high
    for (int i = 1; i < n; i++)
      for (int j = i; j > 0 && notes[j] < notes[j - 1]; j--) {
        int t = notes[j]; notes[j] = notes[j - 1]; notes[j - 1] = t;
      }
    assign_midis(notes, n, v);
    return 1;
  }
  for (size_t i = 0; i < sizeof(OPEN_SHAPES) / sizeof(OPEN_SHAPES[0]); i++)
    if (!strcmp(OPEN_SHAPES[i].name, txt)) {
      memcpy(v->midi, OPEN_SHAPES[i].m, sizeof(v->midi));
      return 1;
    }
  int pc;
  switch (txt[0]) {
    case 'C': pc = 0; break; case 'D': pc = 2; break; case 'E': pc = 4; break;
    case 'F': pc = 5; break; case 'G': pc = 7; break; case 'A': pc = 9; break;
    case 'B': pc = 11; break; default: return 0;
  }
  int i = 1;
  if (txt[i] == '#') { pc = (pc + 1) % 12; i++; }
  else if (txt[i] == 'b') { pc = (pc + 11) % 12; i++; }
  return barre(txt + i, pc, v);
}

static void write_wav_f32_stereo(const char *path, const float *L,
                                 const float *R, long n, int sr) {
  FILE *f = fopen(path, "wb");
  if (!f) { fprintf(stderr, "strum: cannot write %s\n", path); return; }
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
// 2. String — extended Karplus-Strong waveguide
// ===========================================================================
//
//   loop:  y = allpass(line[idx])          fractional delay → exact pitch
//          lp = b·y + (1-b)·lp             the string forgets its highs
//          line[idx] = g·lp + c·(bridge - lp) + exc[k]
//
//   g      per-sample loss (from a T60 per string, blended toward the
//          muted T60 by `mute`: palm, fret-hand lift, or a chuck)
//   b      loop brightness — wound strings lower, plain strings higher,
//          every fret up shaving a little
//   exc    a one-period noise burst comb-filtered at the pick position and
//          lowpassed by force (a soft stroke is a dark stroke), ADDED into
//          whatever the string was already doing
//
// The pick also damps the string for ~3 ms before release (`contact`), so a
// re-strum catches the old note instead of stacking a new burst on it.

#define MAXLINE 4096

typedef struct {
  double line[MAXLINE];
  int N, idx;
  double ap_c, ap_x1, ap_y1;    // fractional-delay allpass
  double lp;                    // loop one-pole state
  double b;                     // loop brightness
  double g_open, g_mute;        // per-sample loss, open vs muted
  double mute, mute_base;       // 0..1, relaxes back to mute_base
  double mute_rel;              // relaxation coefficient
  double exc[MAXLINE];
  int exc_n, exc_i;
  int contact;                  // samples of pick contact left
  double pan, level;
  double y;                     // last loop output (feeds the bridge)
  int midi, fret, sounding;
} String;

static double t60_gain(double t60, double sr) { return exp(log(0.001) / (t60 * sr)); }

static void string_tune(String *s, int midi, double sr) {
  s->midi = midi;
  double D = sr / midi_hz(midi);
  double lp_delay = (1.0 - s->b) / s->b;       // one-pole group delay at DC
  double Dline = D - lp_delay;
  int N = (int)floor(Dline - 0.2);
  if (N < 2) N = 2;
  if (N > MAXLINE - 1) N = MAXLINE - 1;
  double frac = Dline - N;                       // in [0.2, 1.2)
  s->N = N;
  s->ap_c = (1.0 - frac) / (1.0 + frac);
}

static void string_init(String *s, int idx, int midi, double damp, int electric, double sr) {
  memset(s, 0, sizeof(*s));
  s->sounding = midi >= 0;
  s->fret = s->sounding ? midi - OPEN[idx] : 0;
  // gauge: wound low strings darker, longer; plain trebles brighter, shorter
  static const double T60[NSTR] = {5.5, 5.0, 4.5, 3.8, 3.2, 2.8};
  static const double BRI[NSTR] = {0.80, 0.83, 0.86, 0.90, 0.93, 0.95};
  static const double LVL[NSTR] = {1.15, 1.10, 1.00, 0.95, 0.90, 0.90};
  double t60 = T60[idx] * (1.0 - 0.85 * damp) * (electric ? 1.6 : 1.0);
  t60 *= 1.0 - 0.012 * s->fret;                  // shorter string, shorter ring
  s->b = BRI[idx] * (1.0 - 0.006 * s->fret) * (electric ? 1.03 : 1.0);
  if (s->b > 0.97) s->b = 0.97;
  s->g_open = t60_gain(t60, sr);
  s->g_mute = t60_gain(0.14, sr);
  s->mute_rel = 1.0 - exp(-1.0 / (0.040 * sr));
  s->level = LVL[idx];
  s->pan = -0.30 + 0.60 * idx / (NSTR - 1);
  s->idx = 0;
  if (s->sounding) string_tune(s, midi, sr);
  else s->N = 2;
}

// Load a pluck: force 0..1, pick position 0..1 of the length.
static void string_pluck(String *s, double force, double pick, double sr) {
  int n = s->N;
  double w[MAXLINE];
  for (int k = 0; k < n; k++) w[k] = frand();
  int P = (int)(pick * n + 0.5);
  if (P < 1) P = 1;
  // dynamic level: the pick's own lowpass opens with force
  double fc = 900.0 + 7000.0 * pow(force, 1.5);
  double a = 1.0 - exp(-TAU * fc / sr);
  double lp = 0.0, amp = 0.55 * pow(force, 1.3) * s->level;
  for (int k = 0; k < n; k++) {
    double e = w[k] - (k >= P ? w[k - P] : 0.0); // pick-position comb
    lp += (e - lp) * a;
    s->exc[k] = lp * amp;
  }
  // the pick leaving the string: a 1.5 ms click on top
  int clk = (int)(0.0015 * sr);
  for (int k = 0; k < clk && k < n; k++)
    s->exc[k] += frand() * 0.25 * force * exp(-k / (0.0004 * sr));
  s->exc_n = n;
  s->exc_i = 0;
}

static double string_step(String *s, double bridge) {
  double x = s->line[s->idx];
  double y = s->ap_c * x + s->ap_x1 - s->ap_c * s->ap_y1;
  s->ap_x1 = x;
  s->ap_y1 = y;
  s->lp += (y - s->lp) * s->b * (1.0 - 0.45 * s->mute);
  double g = s->g_open + (s->g_mute - s->g_open) * s->mute;
  if (s->contact > 0) { g *= 0.55; s->contact--; }
  double in = g * s->lp + 0.03 * (bridge - s->lp);
  if (s->exc_i < s->exc_n) in += s->exc[s->exc_i++];
  s->line[s->idx] = in;
  if (++s->idx >= s->N) s->idx = 0;
  s->mute += (s->mute_base - s->mute) * s->mute_rel;
  s->y = s->lp;
  return s->lp;
}

// ===========================================================================
// 3. Bridge + body (acoustic) / drive (electric)
// ===========================================================================

typedef struct { double b0, b1, b2, a1, a2, z1, z2; } Biquad;

static void bp_set(Biquad *q, double f, double Q, double sr) {
  double w = TAU * f / sr, alpha = sin(w) / (2.0 * Q), a0 = 1.0 + alpha;
  q->b0 = alpha / a0; q->b1 = 0.0; q->b2 = -alpha / a0;
  q->a1 = -2.0 * cos(w) / a0; q->a2 = (1.0 - alpha) / a0;
  q->z1 = q->z2 = 0.0;
}

static double bq_run(Biquad *q, double in) {
  double out = q->b0 * in + q->z1;
  q->z1 = q->b1 * in - q->a1 * out + q->z2;
  q->z2 = q->b2 * in - q->a2 * out;
  return out;
}

// Dreadnought: Helmholtz air ~100 Hz, top plate ~200 and ~400, a presence
// bump. Left and right sides carry slightly different modes so the box has
// a width the strings alone would not.
#define BODY_RES 4
typedef struct { Biquad res[BODY_RES]; double g[BODY_RES]; double hp_x1, hp_y1, lp, lp_a; } Body;

static void body_init(Body *b, double sr, double skew) {
  const double f[BODY_RES] = {101.0, 203.0, 398.0, 2300.0};
  const double Q[BODY_RES] = {9.0, 7.0, 5.5, 2.2};
  const double g[BODY_RES] = {3.2, 2.2, 1.4, 0.7};
  for (int i = 0; i < BODY_RES; i++) {
    bp_set(&b->res[i], f[i] * (1.0 + skew * (i & 1 ? -0.025 : 0.025)), Q[i], sr);
    b->g[i] = g[i];
  }
  b->hp_x1 = b->hp_y1 = b->lp = 0.0;
  b->lp_a = 1.0 - exp(-TAU * 9000.0 / sr);
}

static double body_run(Body *b, double in) {
  double y = 0.62 * in;
  for (int i = 0; i < BODY_RES; i++) y += b->g[i] * bq_run(&b->res[i], in);
  double hp = y - b->hp_x1 + 0.9967 * b->hp_y1; // DC block ~25 Hz
  b->hp_x1 = y; b->hp_y1 = hp;
  b->lp += (hp - b->lp) * b->lp_a;
  return b->lp;
}

// Electric: the strings sum at the pickup, THEN the amp drives them — the
// two-stage asymmetric tanh and dark one-pole of the cult engine, applied to
// the chord rather than per string so the strings intermodulate (that grind
// between the notes is the amp, and six separately-driven strings never
// make it). Then a cab: 2nd-order lowpass with a presence peak.
typedef struct { double lp, lp_a; Biquad cab; double cab_lp1, cab_lp2, cab_a; double drive; } Amp;

static void amp_init(Amp *a, double drive, double sr) {
  a->drive = drive;
  a->lp = 0.0;
  a->lp_a = 1.0 - pow(1.0 - 0.16, 48000.0 / sr); // 0.16 @ 48 k, as the cult engine
  bp_set(&a->cab, 2400.0, 1.4, sr);
  a->cab_lp1 = a->cab_lp2 = 0.0;
  a->cab_a = 1.0 - exp(-TAU * 4200.0 / sr);
}

static double amp_run(Amp *a, double v) {
  v *= 0.30; // pickup level: six open strings must not be a square wave
  double k = 1.0 + 9.0 * a->drive, bias = 0.35 * a->drive;
  double d1 = tanh(v * k + bias) - tanh(bias);
  double d2 = tanh(d1 * (1.0 + 0.8 * a->drive)) * 0.5;
  a->lp += (d2 - a->lp) * a->lp_a;
  a->cab_lp1 += (a->lp - a->cab_lp1) * a->cab_a;
  a->cab_lp2 += (a->cab_lp1 - a->cab_lp2) * a->cab_a;
  return a->cab_lp2 + 0.35 * bq_run(&a->cab, a->lp);
}

// ===========================================================================
// 4. Hand — pattern → strokes → per-string plucks
// ===========================================================================
//
// A pattern is one bar, one character per step (16 chars = 16ths, 8 = 8ths,
// 12 = triplet 8ths):  D  down, full     d  down, soft
//                      U  up, full       u  up, soft
//                      x  chuck (all strings muted, then scraped)
//                      .  rest
// Down-strokes rake low→high across every sounding string; up-strokes rake
// high→low across the top `up` strings only, 0.6× the rake time. The rake
// accelerates through the strings (t ∝ k^0.85): the wrist speeds up.

typedef struct { long t; int str; double force; int chuck; } Pluck;
#define MAXPLUCK 8192

static int pluck_cmp(const void *a, const void *b) {
  long ta = ((const Pluck *)a)->t, tb = ((const Pluck *)b)->t;
  return ta < tb ? -1 : ta > tb;
}

typedef struct {
  double rake;      // s, full-force down-stroke, low string to high
  int up;           // strings an up-stroke catches
  double force;     // overall hand force 0..1
  double human;     // 0..1 deterministic wobble on timing / force / rake
} Hand;

static int stroke(const Hand *h, const Voicing *v, double t0, int down, double force,
                  int chuck, double sr, Pluck *out) {
  int order[NSTR], n = 0;
  if (down) { for (int s = 0; s < NSTR; s++) if (v->midi[s] >= 0) order[n++] = s; }
  else {
    for (int s = NSTR - 1; s >= 0 && n < h->up; s--) if (v->midi[s] >= 0) order[n++] = s;
  }
  if (!n) return 0;
  double f = force * h->force * (1.0 + 0.08 * h->human * frand());
  if (f > 1.0) f = 1.0;
  if (f < 0.05) f = 0.05;
  double rake = h->rake * (1.30 - 0.40 * f) * (down ? 1.0 : 0.6)
              * (1.0 + 0.15 * h->human * frand());
  double t = t0 + 0.004 * h->human * frand();
  for (int k = 0; k < n; k++) {
    double u = n > 1 ? pow((double)k / (n - 1), 0.85) : 0.0;
    out[k].t = (long)((t + rake * u) * sr);
    out[k].str = order[k];
    // the later strings in a rake are hit a touch lighter; the top string
    // of an up-stroke a touch harder (the pick digs in first)
    out[k].force = f * (1.0 - 0.05 * k) * (!down && k == 0 ? 1.08 : 1.0);
    out[k].chuck = chuck;
  }
  return n;
}

// ===========================================================================
// 5. Render + CLI
// ===========================================================================

#define MAXCHORDS 64
#define DEFAULT_PATTERN "D..d..u.u.D..u.." // the cult STRUM table, in 16ths

static void usage(void) {
  fprintf(stderr,
    "strum — physically-modeled strummed six-string\n"
    "usage: strum [options] --out file.wav\n"
    "  --chord C        name (Bm, D, G, Em, F#m7, Cadd9 …) or midi list (47,54,59,62)\n"
    "                   several chords: \"Bm|D|G|Em\" — one per bar, cycling\n"
    "  --pattern P      one bar of D d U u x .  (default \"%s\")\n"
    "  --bpm B          tempo (default 120)\n"
    "  --bars N         bars to play (default 1)\n"
    "  --electric       pickup + two-stage tanh amp instead of the body\n"
    "  --acoustic       dreadnought body (default)\n"
    "  --drive 0..1     amp drive, electric only (default 0.7)\n"
    "  --rake MS        full-force down-stroke, low to high (default 16)\n"
    "  --up N           strings an up-stroke catches (default 4)\n"
    "  --force 0..1     hand force (default 0.8)\n"
    "  --pick 0..1      pick position along the string (default 0.18)\n"
    "  --mute palm|open|0..1  fret-hand damping (default open)\n"
    "  --damp 0..1      string damping / shorter sustain (default 0.15)\n"
    "  --human 0..1     seeded timing/force wobble (default 0.5)\n"
    "  --tail S         ring past the last bar (default 1.0)\n"
    "  --sr HZ          sample rate (default 48000)\n"
    "  --seed N         RNG seed (default 1)\n"
    "  --out FILE       output wav (float32 stereo)\n", DEFAULT_PATTERN);
}

int main(int argc, char **argv) {
  const char *chord = "Bm", *pattern = DEFAULT_PATTERN, *out = NULL, *mute_s = "open";
  double bpm = 120.0, drive = 0.7, rake_ms = 16.0, force = 0.8, pick = 0.18;
  double damp = 0.15, human = 0.5, tail = 1.0, sr = 48000.0;
  int bars = 1, electric = 0, up = 4;
  uint32_t seed = 1;

  for (int i = 1; i < argc; i++) {
    if (!strcmp(argv[i], "--chord") && i + 1 < argc) chord = argv[++i];
    else if (!strcmp(argv[i], "--pattern") && i + 1 < argc) pattern = argv[++i];
    else if (!strcmp(argv[i], "--bpm") && i + 1 < argc) bpm = atof(argv[++i]);
    else if (!strcmp(argv[i], "--bars") && i + 1 < argc) bars = atoi(argv[++i]);
    else if (!strcmp(argv[i], "--electric")) electric = 1;
    else if (!strcmp(argv[i], "--acoustic")) electric = 0;
    else if (!strcmp(argv[i], "--drive") && i + 1 < argc) drive = atof(argv[++i]);
    else if (!strcmp(argv[i], "--rake") && i + 1 < argc) rake_ms = atof(argv[++i]);
    else if (!strcmp(argv[i], "--up") && i + 1 < argc) up = atoi(argv[++i]);
    else if (!strcmp(argv[i], "--force") && i + 1 < argc) force = atof(argv[++i]);
    else if (!strcmp(argv[i], "--pick") && i + 1 < argc) pick = atof(argv[++i]);
    else if (!strcmp(argv[i], "--mute") && i + 1 < argc) mute_s = argv[++i];
    else if (!strcmp(argv[i], "--damp") && i + 1 < argc) damp = atof(argv[++i]);
    else if (!strcmp(argv[i], "--human") && i + 1 < argc) human = atof(argv[++i]);
    else if (!strcmp(argv[i], "--tail") && i + 1 < argc) tail = atof(argv[++i]);
    else if (!strcmp(argv[i], "--sr") && i + 1 < argc) sr = atof(argv[++i]);
    else if (!strcmp(argv[i], "--seed") && i + 1 < argc) seed = (uint32_t)atoi(argv[++i]);
    else if (!strcmp(argv[i], "--out") && i + 1 < argc) out = argv[++i];
    else if (!strcmp(argv[i], "--help")) { usage(); return 0; }
    else { fprintf(stderr, "strum: unknown arg %s\n", argv[i]); usage(); return 2; }
  }
  if (!out) { usage(); return 2; }
  if (bars < 1) bars = 1;
  if (up < 1) up = 1;
  if (up > NSTR) up = NSTR;
  #define CLAMP01(x) do { if ((x) < 0.0) (x) = 0.0; if ((x) > 1.0) (x) = 1.0; } while (0)
  CLAMP01(force); CLAMP01(drive); CLAMP01(damp); CLAMP01(human);
  if (pick < 0.03) pick = 0.03;
  if (pick > 0.5) pick = 0.5;
  double mute_base = !strcmp(mute_s, "palm") ? 0.75 : !strcmp(mute_s, "open") ? 0.0 : atof(mute_s);
  CLAMP01(mute_base);
  rng_seed(seed);

  // ---- chords: one per bar, cycling ----
  Voicing chords[MAXCHORDS];
  int nchords = 0;
  {
    char buf[512];
    snprintf(buf, sizeof(buf), "%s", chord);
    for (char *tok = strtok(buf, "|"); tok && nchords < MAXCHORDS; tok = strtok(NULL, "|")) {
      while (*tok == ' ') tok++;
      if (!parse_chord(tok, &chords[nchords])) {
        fprintf(stderr, "strum: unknown chord '%s'\n", tok);
        return 2;
      }
      nchords++;
    }
  }
  if (!nchords) { fprintf(stderr, "strum: no chord\n"); return 2; }

  // ---- pattern → plucks ----
  char pat[128];
  int steps = 0;
  for (const char *p = pattern; *p && steps < 127; p++)
    if (*p != ' ') pat[steps++] = *p;
  pat[steps] = 0;
  if (!steps) { fprintf(stderr, "strum: empty pattern\n"); return 2; }
  double BEAT = 60.0 / bpm, BAR = 4.0 * BEAT, step = BAR / steps;
  Hand hand = { rake_ms * 1e-3, up, force, human };
  static Pluck plucks[MAXPLUCK];
  int np = 0, nstrokes = 0;
  for (int bar = 0; bar < bars; bar++) {
    const Voicing *v = &chords[bar % nchords];
    for (int k = 0; k < steps && np < MAXPLUCK - NSTR; k++) {
      char c = pat[k];
      int down, chuck = 0;
      double f;
      switch (c) {
        case 'D': down = 1; f = 1.00; break;
        case 'd': down = 1; f = 0.60; break;
        case 'U': down = 0; f = 0.75; break;
        case 'u': down = 0; f = 0.45; break;
        case 'x': down = 1; f = 0.70; chuck = 1; break;
        case '.': case '-': continue;
        default: fprintf(stderr, "strum: bad pattern char '%c'\n", c); return 2;
      }
      np += stroke(&hand, v, bar * BAR + k * step, down, f, chuck, sr, &plucks[np]);
      nstrokes++;
    }
  }
  qsort(plucks, np, sizeof(Pluck), pluck_cmp);

  // ---- strings, bridge, body / amp ----
  long N = (long)((bars * BAR + tail) * sr);
  float *L = calloc((size_t)N, sizeof(float));
  float *R = calloc((size_t)N, sizeof(float));
  if (!L || !R) { fprintf(stderr, "strum: alloc failed\n"); return 1; }
  String str[NSTR];
  for (int s = 0; s < NSTR; s++) {
    string_init(&str[s], s, chords[0].midi[s], damp, electric, sr);
    str[s].mute_base = mute_base;
    str[s].mute = mute_base;
  }
  Body bodyL, bodyR;
  body_init(&bodyL, sr, 1.0);
  body_init(&bodyR, sr, -1.0);
  Amp amp;
  amp_init(&amp, drive, sr);

  int contact_len = (int)(0.003 * sr);
  int pc = 0, pp = 0;           // contact pointer, pluck pointer
  int cur_chord = 0;
  long bar_len = (long)(BAR * sr), lift = (long)(0.030 * sr);
  double bridge_lp = 0.0, bridge_a = 1.0 - exp(-TAU * 2500.0 / sr);
  Biquad sq;
  double squeak_env = 0.0, squeak_lp = 0.0;
  int squeak_left = 0, squeak_len = (int)(0.070 * sr), nsqueaks = 0;

  long choke = N - (long)(0.22 * sr);
  for (long i = 0; i < N; i++) {
    if (i == choke) for (int s = 0; s < NSTR; s++) str[s].mute_base = 1.0; // the hand lands
    // fret hand: choke before the bar line, retune on it, squeak if wound
    int bar = (int)(i / bar_len);
    if (bar < bars && nchords > 1) {
      int next = (bar + 1) % nchords;
      long into = i - bar * bar_len;
      if (bar + 1 < bars && into == bar_len - lift && next != cur_chord)
        for (int s = 0; s < NSTR; s++)
          if (chords[next].midi[s] != chords[cur_chord].midi[s]) str[s].mute = 1.0;
      if (into == 0 && bar % nchords != cur_chord) {
        const Voicing *nv = &chords[bar % nchords];
        int moved_wound = 0;
        for (int s = 0; s < NSTR; s++) {
          int old = str[s].midi;
          if (nv->midi[s] >= 0) {
            if (s < 3 && str[s].sounding && abs(nv->midi[s] - old) >= 2) moved_wound = 1;
            str[s].sounding = 1;
            str[s].fret = nv->midi[s] - OPEN[s];
            string_tune(&str[s], nv->midi[s], sr);
          } else {
            str[s].sounding = 0;
            str[s].mute = 1.0;
          }
        }
        cur_chord = bar % nchords;
        if (moved_wound) {
          squeak_left = squeak_len;
          bp_set(&sq, 2200.0 + 900.0 * (0.5 + 0.5 * frand()), 14.0, sr);
          squeak_env = 0.10 * (0.7 + 0.3 * frand());
          nsqueaks++;
        }
      }
    }
    // pick contact ahead of each pluck, then the pluck itself
    while (pc < np && plucks[pc].t - contact_len <= i) {
      String *s = &str[plucks[pc].str];
      s->contact = contact_len;
      if (plucks[pc].chuck) s->mute = 1.0;
      pc++;
    }
    while (pp < np && plucks[pp].t <= i) {
      String *s = &str[plucks[pp].str];
      if (plucks[pp].chuck) {
        for (int k = 0; k < NSTR; k++) str[k].mute = 1.0;
        string_pluck(s, plucks[pp].force * 0.7, pick * 0.6, sr);
      } else string_pluck(s, plucks[pp].force, pick, sr);
      pp++;
    }

    // strings share the bridge; the bridge is stiff above ~2.5 kHz
    double sl = 0.0, srg = 0.0, sum = 0.0;
    for (int s = 0; s < NSTR; s++) {
      double y = string_step(&str[s], bridge_lp);
      sum += y;
      sl += y * 0.5 * (1.0 - str[s].pan);
      srg += y * 0.5 * (1.0 + str[s].pan);
    }
    bridge_lp += (sum / NSTR - bridge_lp) * bridge_a;

    // the fret-hand squeak: a short whistling band of noise, in and out
    if (squeak_left > 0) {
      squeak_lp += (frand() - squeak_lp) * 0.5;
      double u = (double)(squeak_len - squeak_left) / squeak_len;
      double q = bq_run(&sq, squeak_lp) * squeak_env * sin(M_PI * u);
      sl += q; srg += q;
      squeak_left--;
    }

    if (electric) {
      double m = amp_run(&amp, sl + srg);
      L[i] = (float)(m + 0.15 * sl);
      R[i] = (float)(m + 0.15 * srg);
    } else {
      L[i] = (float)body_run(&bodyL, sl);
      R[i] = (float)body_run(&bodyR, srg);
    }
  }

  // Last 10 ms fade, then peak normalize to 0.9.
  long fade = (long)(0.010 * sr);
  for (long i = N - fade; i < N; i++) {
    float w = (float)(N - 1 - i) / fade;
    L[i] *= w; R[i] *= w;
  }
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

  fprintf(stderr, "strum: %s", electric ? "electric" : "acoustic");
  for (int c = 0; c < nchords; c++) {
    fprintf(stderr, "%s %s [", c ? " |" : "", chords[c].name);
    for (int s = 0; s < NSTR; s++) {
      if (chords[c].midi[s] < 0) fprintf(stderr, "%sx", s ? " " : "");
      else fprintf(stderr, "%s%s+%d", s ? " " : "", STRNAME[s], chords[c].midi[s] - OPEN[s]);
    }
    fprintf(stderr, "]");
  }
  fprintf(stderr, "\n       \"%s\" x %d bar%s @ %.0f bpm, %d strokes, %d plucks, %d squeak%s"
                  " -> %s (%.2fs @ %d Hz)\n",
          pat, bars, bars == 1 ? "" : "s", bpm, nstrokes, np, nsqueaks,
          nsqueaks == 1 ? "" : "s", out, (double)N / sr, (int)sr);
  free(L);
  free(R);
  return 0;
}
