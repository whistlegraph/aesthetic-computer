// emotrib.c — "emo trap." An emotional trap instrumental, rendered ENTIRELY
// in C (no JS, no samples): deep gliding 808 sub, a layered click kick, a
// halftime snare/clap, triplet hi-hat rolls, and a sad F-minor bell/piano
// loop over a soft pad, with a thread of vinyl crackle for the feels.
//
// House style (per pop/boombaboom/c/boombaboom.c + pop/hellsine/c): tonal
// voices are sums of sin() phases advanced by freq/SR, rounded with a
// memoryless tanh for grit; percussion (hats, snare noise, vinyl) is the one
// licensed exception — xorshift noise shaped by one-pole filters + envelopes,
// the same lane nullnoise.c lives in. Nothing here reads a WAV.
//
// Form (140 BPM, halftime feel — snare on beat 3): an 8-bar melody-only
// intro, then three full-beat sections (A1/A2/A3) of rising hat density and
// melody octaves, split by a stripped pad+melody break and a bridge, closing
// on a melody-fade outro.
//
// Build:  bash emotrib/c/build.sh
// Run:    ./emotrib --out out/emotrib-raw.wav [--bpm 140]

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
static double BPMV = 140, BEAT, BAR;
static long N; // total samples

// ── tiny rng (xorshift) — for noise perc + vinyl + humanized timing ────────
static uint32_t rng_s = 0x656d6f74; // "emot"
static inline double rnd(void) {
  rng_s ^= rng_s << 13; rng_s ^= rng_s >> 17; rng_s ^= rng_s << 5;
  return (double)rng_s / 4294967296.0;
}
static inline double rnd2(void) { return rnd() * 2.0 - 1.0; }
static inline double midi_hz(double m) { return 440.0 * pow(2.0, (m - 69.0) / 12.0); }
static inline double clampd(double x, double a, double b) { return x < a ? a : x > b ? b : x; }

// ── buses ──────────────────────────────────────────────────────────────────
//   bass  : 808 + kick (low end, gets a gentle kick-keyed pump)
//   mus   : bell/piano + pad melodic content (also pumps with the kick)
//   drum  : snare + hats (dry, punchy)
//   rev   : reverb send (bell + snare tail) — convolved-ish at mixdown
static float *bassL, *bassR, *musL, *musR, *drumL, *drumR, *revL, *revR;
static float *duck; // kick-keyed sidechain envelope (1 = open, dips on hits)

static inline void addB(long i, double l, double r) { if (i >= 0 && i < N) { bassL[i] += (float)l; bassR[i] += (float)r; } }
static inline void addM(long i, double l, double r) { if (i >= 0 && i < N) { musL[i] += (float)l; musR[i] += (float)r; } }
static inline void addD(long i, double l, double r) { if (i >= 0 && i < N) { drumL[i] += (float)l; drumR[i] += (float)r; } }
static inline void addR(long i, double l, double r) { if (i >= 0 && i < N) { revL[i] += (float)l; revR[i] += (float)r; } }

static int write_wav_f32_stereo(const char *path, const float *L, const float *R, long n) {
  FILE *f = fopen(path, "wb"); if (!f) return 0;
  uint32_t dsz = (uint32_t)(n * 8), riff = 36 + dsz, sr = SR, br = SR * 8, fsz = 16;
  uint16_t fmt = 3, ch = 2, ba = 8, bits = 32;
  fwrite("RIFF", 1, 4, f); fwrite(&riff, 4, 1, f); fwrite("WAVE", 1, 4, f);
  fwrite("fmt ", 1, 4, f); fwrite(&fsz, 4, 1, f);
  fwrite(&fmt, 2, 1, f); fwrite(&ch, 2, 1, f); fwrite(&sr, 4, 1, f);
  fwrite(&br, 4, 1, f); fwrite(&ba, 2, 1, f); fwrite(&bits, 2, 1, f);
  fwrite("data", 1, 4, f); fwrite(&dsz, 4, 1, f);
  for (long i = 0; i < n; i++) { fwrite(&L[i], 4, 1, f); fwrite(&R[i], 4, 1, f); }
  fclose(f); return 1;
}

// ════════════════════════════════════════════════════════════════════════
//  VOICES
// ════════════════════════════════════════════════════════════════════════

// 808 SUB — sine carrier with a fast pitch "click" at the attack, long
// exponential body, tanh saturation, and optional portamento glide IN from a
// previous pitch (the signature emo-trap slide). Writes to the bass bus and
// stamps the sidechain duck so everything breathes on the sub hits too.
static void e808(double t, double midi, double durBeats, double fromMidi, double gain) {
  long s0 = (long)(t * SR);
  double dur = durBeats * BEAT;
  long len = (long)((dur + 0.30) * SR);
  double f0 = midi_hz(midi);
  double fprev = fromMidi >= 0 ? midi_hz(fromMidi) : f0;
  double phase = 0, drift = rnd2() * 0.5; // tiny per-hit detune for life
  for (long i = 0; i < len; i++) {
    double tt = (double)i / SR;
    double gl = fromMidi >= 0 ? clampd(tt / 0.07, 0, 1) : 1.0; // 70ms glide
    double f = fprev * pow(f0 / fprev, gl) + drift;
    double click = 1.0 + 5.0 * exp(-tt / 0.010);               // attack chirp
    double amp = (1.0 - exp(-tt / 0.003)) * exp(-tt / (dur * 0.55 + 0.05));
    phase += f * click / SR;
    double x = sin(TAU * phase);
    x = tanh(x * 1.7) * 0.78;                                  // warm sat
    double s = x * amp * gain;
    addB(s0 + i, s, s);
  }
}

// KICK — short sine thump (pitch drop) layered under 808 attacks for punch.
// Also stamps the sidechain duck (the groove pump).
static void kick(double t, double gain) {
  long s0 = (long)(t * SR);
  long len = (long)(0.18 * SR);
  double phase = 0;
  for (long i = 0; i < len; i++) {
    double tt = (double)i / SR;
    double f = 52.0 + 150.0 * exp(-tt / 0.016);   // 200→52 Hz drop
    double amp = exp(-tt / 0.07);
    phase += f / SR;
    double x = tanh(sin(TAU * phase) * 1.5) * amp * gain;
    addB(s0 + i, x, x);
  }
  // stamp duck: a deep 1→0.30 dip recovering over ~260ms keyed at this onset —
  // the audible kick-keyed pump that makes the pad/melody breathe in groove.
  long dl = (long)(0.30 * SR);
  for (long i = 0; i < dl; i++) {
    long j = s0 + i; if (j < 0 || j >= N) continue;
    double tt = (double)i / SR;
    double d = 0.30 + 0.70 * (1.0 - exp(-tt / 0.11)); // deep dip, smooth recover
    if (d < duck[j]) duck[j] = (float)d;
  }
}

// HI-HAT — crisp metallic tick. White noise through TWO cascaded one-pole
// highpasses (cut the gushy body), a fast-decay env, and a snappy attack so
// it reads as a tick rather than a wash. Panned slightly per hit.
static void hat(double t, double vel, int open) {
  long s0 = (long)(t * SR);
  double dec = open ? 0.045 : 0.013;            // tight: was 0.12 / 0.028
  long len = (long)((dec * 3.5) * SR);
  double h1 = 0, h2 = 0, p1 = 0, p2 = 0, pan = rnd2() * 0.25;
  for (long i = 0; i < len; i++) {
    double tt = (double)i / SR;
    double w = rnd2();
    // two HP stages with a low coefficient → high cutoff, very little body left
    h1 = 0.55 * (h1 + w - p1); p1 = w;
    h2 = 0.55 * (h2 + h1 - p2); p2 = h1;
    double amp = exp(-tt / dec) * vel * 0.42;
    double s = h2 * amp;
    addD(s0 + i, s * (0.82 - 0.4 * pan), s * (0.82 + 0.4 * pan));
  }
}

// SNARE / CLAP — a noise body (bandpass-ish via HP+LP one-poles) plus a short
// tonal "ping" around 190 Hz, with a 3-tap clap stutter on the attack. Sends
// a tail to reverb. Lands on beat 3 (halftime).
static void snare(double t, double gain) {
  long s0 = (long)(t * SR);
  long len = (long)(0.34 * SR);
  double hp = 0, prevN = 0, lp = 0, pphase = 0;
  // clap stutter offsets (seconds) + relative levels
  double taps[4] = { 0.0, 0.011, 0.021, 0.030 };
  for (long i = 0; i < len; i++) {
    double tt = (double)i / SR;
    // layered noise from the clap taps
    double body = 0;
    for (int k = 0; k < 4; k++) {
      double dt = tt - taps[k];
      if (dt < 0) continue;
      body += rnd2() * exp(-dt / (k == 0 ? 0.13 : 0.05));
    }
    hp = 0.80 * (hp + body - prevN); prevN = body;  // brighten
    lp = lp + 0.55 * (hp - lp);                     // tame the very top
    double tone = sin(TAU * pphase); pphase += 190.0 / SR;
    double tamp = exp(-tt / 0.05);
    double s = (lp * 0.9 + tone * tamp * 0.35) * gain;
    addD(s0 + i, s, s);
    addR(s0 + i, s * 0.30, s * 0.30);               // reverb send
  }
}

// BELL / PIANO — the melodic voice. A struck tone: fundamental + a couple of
// inharmonic-ish partials, pluck envelope (fast attack, medium decay), light
// detune chorus across L/R. Sends to reverb for the emotional wash.
static void bell(double t, double midi, double durBeats, double vel) {
  long s0 = (long)(t * SR);
  double dur = durBeats * BEAT;
  long len = (long)((dur + 0.6) * SR);
  double f = midi_hz(midi);
  double pa = 0, pb = 0, pc = 0, pd = 0;
  double det = f * 0.0016;
  for (long i = 0; i < len; i++) {
    double tt = (double)i / SR;
    double amp = (1.0 - exp(-tt / 0.004)) * exp(-tt / (dur * 0.7 + 0.18)) * vel;
    pa += (f - det) / SR; pb += (f + det) / SR;     // detuned pair
    pc += (f * 2.01) / SR; pd += (f * 3.0) / SR;    // shimmer partials
    double l = (sin(TAU * pa) + 0.5 * sin(TAU * pc) + 0.22 * sin(TAU * pd)) * amp;
    double r = (sin(TAU * pb) + 0.5 * sin(TAU * pc) + 0.22 * sin(TAU * pd)) * amp;
    l = tanh(l * 1.1) * 0.32; r = tanh(r * 1.1) * 0.32;
    addM(s0 + i, l, r);
    addR(s0 + i, l * 0.45, r * 0.45);
  }
}

// PAD — soft sustained chord (stack of sine voices) under the melody. Slow
// attack/release, sits low in the mix on the music bus.
static void pad(double t, const int *midis, int nv, double durBeats, double gain) {
  long s0 = (long)(t * SR);
  double dur = durBeats * BEAT;
  long len = (long)((dur + 0.4) * SR);
  double atk = 0.25, rel = 0.5;
  double ph[8] = {0}, ph2[8] = {0};
  if (nv > 8) nv = 8;
  for (long i = 0; i < len; i++) {
    double tt = (double)i / SR;
    double env;
    if (tt < atk) env = tt / atk;
    else if (tt < dur) env = 1.0;
    else env = clampd(1.0 - (tt - dur) / rel, 0, 1);
    env *= gain;
    double l = 0, r = 0;
    for (int v = 0; v < nv; v++) {
      double f = midi_hz(midis[v]);
      ph[v] += f / SR; ph2[v] += (f * 1.004) / SR; // slight detune width
      l += sin(TAU * ph[v]);
      r += sin(TAU * ph2[v]);
    }
    double scale = env * 0.10 / (nv > 0 ? nv : 1);
    addM(s0 + i, l * scale, r * scale);
  }
}

// VINYL CRACKLE — sparse low-level pops + a noise floor, for emotional texture.
static void vinyl(double t0, double dur, double gain) {
  long s0 = (long)(t0 * SR), len = (long)(dur * SR);
  double lp = 0;
  for (long i = 0; i < len; i++) {
    double w = rnd2();
    lp = lp + 0.04 * (w - lp);                 // dark hiss floor
    double s = lp * 0.5;
    if (rnd() > 0.99955) s += rnd2() * 0.9;    // occasional pop
    s *= gain;
    addM(s0 + i, s, s);
  }
}

// ════════════════════════════════════════════════════════════════════════
//  COMPOSITION — F minor.  i–VI–III–VII  (Fm – Db – Ab – Eb), one bar each.
// ════════════════════════════════════════════════════════════════════════

// MIDI helpers (F minor scale tones used below)
enum { F2=41, Ab1=32, Db2=37, Eb2=39,            // 808 roots
       G3=55, Ab3=56, Bb3=58, C4=60, Db4=61, Eb4=63, F4=65, G4=67, Ab4=68 };

// 808 root per chord-bar (low octave) + a glide-target into the next bar.
static const int ROOTS[4]   = { F2, Db2, Ab1, Eb2 };
// melody — eighth-note grid (8 per bar; -1 = rest), one row per chord. FOUR
// variations so the line evolves across the form instead of looping forever:
//   0 = sad descending motif (intro / A1)
//   1 = more active, higher answer (A2)
//   2 = top-octave climax lead, denser (A3)
//   3 = sparse, breathing (break / outro)
static const int MEL[4][4][8] = {
  { // 0 — the core motif
    /* Fm */ { C4, -1, Ab3, -1,  F4, -1, Eb4, C4 },
    /* Db */ { Db4,-1, Ab3, -1,  F4, -1, Db4, -1 },
    /* Ab */ { C4, -1, Ab3, -1,  Eb4,-1, C4, -1 },
    /* Eb */ { Bb3,-1, G3,  -1,  Eb4,-1, Bb3, G3 },
  },
  { // 1 — active answer
    /* Fm */ { Ab3,C4, Eb4,-1,  F4, Eb4,C4, Ab3 },
    /* Db */ { F4, -1, Ab3,Db4, -1, F4, Eb4,Db4 },
    /* Ab */ { Eb4,C4, Ab3,C4,  Eb4,-1, G4, Eb4 },
    /* Eb */ { Bb3,Eb4,G4, -1,  F4, Eb4,Bb3,G3 },
  },
  { // 2 — top-octave climax
    /* Fm */ { C4, Eb4,F4, Ab4, G4, F4, Eb4,C4 },
    /* Db */ { Db4,F4, Ab4,-1,  F4, Ab3,F4, Db4 },
    /* Ab */ { C4, Eb4,Ab4,-1,  G4, Eb4,C4, Eb4 },
    /* Eb */ { Bb3,Eb4,G4, Bb3, Eb4,G4, F4, Eb4 },
  },
  { // 3 — sparse / breathing
    /* Fm */ { -1,-1, C4, -1,  -1,-1, Ab3,-1 },
    /* Db */ { -1,-1, Ab3,-1,  -1,-1, F4, -1 },
    /* Ab */ { -1,-1, Eb4,-1,  -1,-1, C4, -1 },
    /* Eb */ { -1,-1, Bb3,-1,  -1,-1, G3, -1 },
  },
};
// pad chord voicings (3-note triads, mid octave)
static const int PAD[4][3] = {
  /* Fm */ { 53, 56, 60 },   // F3 Ab3 C4
  /* Db */ { 49, 53, 56 },   // Db3 F3 Ab3
  /* Ab */ { 56, 60, 63 },   // Ab3 C4 Eb4
  /* Eb */ { 51, 55, 58 },   // Eb3 G3 Bb3
};

// swing/humanize: nudge a beat time by a few ms
static double hum(double t) { return t + rnd2() * 0.006; }

// place the 808 for one bar (root on the 1, a syncopated push on the "+3",
// gliding in from the previous bar's root on the downbeat).
static void bar808(double bt, int root, int prevRoot, double gain) {
  e808(bt + 0.0 * BEAT, root, 2.3, prevRoot, gain);          // glide-in downbeat
  e808(bt + 2.5 * BEAT, root, 1.3, -1, gain * 0.92);         // syncopated push
}

// hats for one bar. density 0 = none, 1 = eighths, 2 = sixteenths + a triplet
// roll on beat 4, 3 = busy (sixteenths + two rolls).
static void barHats(double bt, int density) {
  if (density <= 0) return;
  int steps = density >= 2 ? 16 : 8;
  for (int s = 0; s < steps; s++) {
    double t = bt + (double)s / steps * 4.0 * BEAT;
    double vel = 0.6 + 0.25 * ((s % 2) == 0);     // accent on-beats
    if (density >= 2 && (s % 4) == 0) vel += 0.12;
    hat(hum(t), vel, (s % 8) == 6);               // an open-ish hat now and then
  }
  // triplet rolls: subdivide the last beat (and beat 2 when busy)
  if (density >= 2) {
    double rb = bt + 3.0 * 4.0 / 4.0 * BEAT;       // start of beat 4
    int n = 6;
    for (int k = 0; k < n; k++)
      hat(rb + (double)k / n * BEAT, 0.45 + 0.5 * k / n, 0);
  }
  if (density >= 3) {
    double rb = bt + 1.0 * BEAT;                    // a roll on beat 2 as well
    int n = 4;
    for (int k = 0; k < n; k++)
      hat(rb + (double)k / n * BEAT, 0.4 + 0.5 * k / n, 0);
  }
}

// one full bar of the beat. flags pick which layers play. melVar = which of
// the four MEL variations this section's melody uses.
typedef struct { int drums, hats, oct808, melody, melOct, pad, snareTriplet, melVar; } Bar;

static void renderBar(int barIdx, double bt, int chord, int prevRootMidi, Bar cfg) {
  int root = ROOTS[chord];

  // 808 + kick
  if (cfg.drums) {
    int r = cfg.oct808 ? root + 12 : root;
    int pr = prevRootMidi >= 0 ? (cfg.oct808 ? prevRootMidi + 12 : prevRootMidi) : -1;
    bar808(bt, r, pr, 0.95);
    kick(hum(bt + 0.0), 0.9);
    kick(hum(bt + 2.5 * BEAT), 0.62);     // ghost kick with the 808 push
    // halftime snare on beat 3
    snare(bt + 2.0 * BEAT, 0.9);
    if (cfg.snareTriplet) {                // a triplet snare fill feel
      snare(bt + 3.66 * BEAT, 0.5);
      snare(bt + 3.83 * BEAT, 0.62);
    }
  }
  barHats(bt, cfg.hats);

  // pad
  if (cfg.pad) pad(bt, PAD[chord], 3, 4.0, 1.0);

  // melody
  if (cfg.melody) {
    for (int s = 0; s < 8; s++) {
      int m = MEL[cfg.melVar][chord][s];
      if (m < 0) continue;
      double t = bt + (double)s / 8.0 * 4.0 * BEAT;
      double dur = 0.9; // eighth-ish, let it ring a touch
      double vel = 0.85 + 0.1 * rnd();
      bell(hum(t), m + (cfg.melOct ? 12 : 0), dur, vel);
      if (cfg.melOct) bell(hum(t), m, dur, vel * 0.5); // double low for body
    }
  }
}

// the arrangement: a list of (bars, chordStartOffset, Bar cfg).
int main(int argc, char **argv) {
  const char *outPath = "out/emotrib-raw.wav";
  for (int i = 1; i < argc; i++) {
    if (!strcmp(argv[i], "--out") && i + 1 < argc) outPath = argv[++i];
    else if (!strcmp(argv[i], "--bpm") && i + 1 < argc) BPMV = atof(argv[++i]);
  }
  BEAT = 60.0 / BPMV; BAR = BEAT * 4.0;

  // section table — bar counts + per-section Bar config
  struct { int bars; Bar cfg; } sections[] = {
    //                          drums hats oct melody melOct pad snTrip melVar
    { 8,  (Bar){ 0, 0, 0, 1, 0, 1, 0, 0 } },   // intro: pad + melody (motif) + atmos
    { 16, (Bar){ 1, 1, 0, 1, 0, 1, 0, 0 } },   // A1: beat in, eighth hats, motif
    { 8,  (Bar){ 0, 0, 0, 1, 0, 1, 0, 3 } },   // break: strip drums, sparse melody
    { 16, (Bar){ 1, 2, 0, 1, 0, 1, 1, 1 } },   // A2: 16th hats + rolls, active melody
    { 8,  (Bar){ 1, 1, 0, 0, 0, 1, 0, 0 } },   // bridge: drums + pad, melody rest
    { 16, (Bar){ 1, 3, 1, 1, 1, 1, 1, 2 } },   // A3: climax — busy, octave 808+mel
    { 8,  (Bar){ 0, 0, 0, 1, 0, 1, 0, 3 } },   // outro: sparse melody fade
  };
  int nSec = (int)(sizeof(sections) / sizeof(sections[0]));

  int totalBars = 0;
  for (int s = 0; s < nSec; s++) totalBars += sections[s].bars;
  double dur = totalBars * BAR + 1.0;       // +1s tail for ring-out
  N = (long)(dur * SR);

  bassL = calloc(N, sizeof(float)); bassR = calloc(N, sizeof(float));
  musL  = calloc(N, sizeof(float)); musR  = calloc(N, sizeof(float));
  drumL = calloc(N, sizeof(float)); drumR = calloc(N, sizeof(float));
  revL  = calloc(N, sizeof(float)); revR  = calloc(N, sizeof(float));
  duck  = malloc(N * sizeof(float));
  for (long i = 0; i < N; i++) duck[i] = 1.0f;

  // atmospheric vinyl under the whole thing
  vinyl(0.0, dur, 0.5);

  // walk the arrangement
  int barIdx = 0; double bt = 0; int prevRoot = -1;
  for (int s = 0; s < nSec; s++) {
    for (int b = 0; b < sections[s].bars; b++) {
      int chord = barIdx % 4;
      int root = ROOTS[chord];
      renderBar(barIdx, bt, chord, prevRoot, sections[s].cfg);
      prevRoot = root;
      bt += BAR; barIdx++;
    }
  }

  // ── simple stereo reverb on the rev bus (3 combs + 1 allpass) ────────────
  {
    int combLen[3] = { 1789, 1999, 2237 }, apLen = 557;
    float *cb[3]; for (int k = 0; k < 3; k++) cb[k] = calloc(combLen[k], sizeof(float));
    float *ap = calloc(apLen, sizeof(float));
    int ci[3] = {0,0,0}, ai = 0;
    for (long i = 0; i < N; i++) {
      double in = (revL[i] + revR[i]) * 0.5;
      double acc = 0;
      for (int k = 0; k < 3; k++) {
        double y = cb[k][ci[k]];
        cb[k][ci[k]] = (float)(in + y * 0.78);
        ci[k] = (ci[k] + 1) % combLen[k];
        acc += y;
      }
      acc /= 3.0;
      double yo = ap[ai];
      double xo = acc + yo * 0.5;
      ap[ai] = (float)xo; ai = (ai + 1) % apLen;
      double wet = yo - acc * 0.5;
      revL[i] = (float)(wet * 0.9);
      revR[i] = (float)(wet * 1.0);
    }
    for (int k = 0; k < 3; k++) free(cb[k]); free(ap);
  }

  // ── mixdown: apply the kick-keyed pump to bass + music, sum buses ────────
  float *L = calloc(N, sizeof(float)), *R = calloc(N, sizeof(float));
  for (long i = 0; i < N; i++) {
    double d = duck[i];
    double l = bassL[i] * (0.6  + 0.4  * d)        // 808/kick breathe a little
             + musL[i]  * (0.18 + 0.82 * d)         // melody/pad pump hard
             + drumL[i] * 0.9
             + revL[i]  * (0.35 + 0.65 * d);        // wash pumps too
    double r = bassR[i] * (0.6  + 0.4  * d)
             + musR[i]  * (0.18 + 0.82 * d)
             + drumR[i] * 0.9
             + revR[i]  * (0.35 + 0.65 * d);
    L[i] = (float)l; R[i] = (float)r;
  }

  // normalize to -1.5 dBFS-ish peak (final loudness is the ffmpeg master's job)
  double peak = 0;
  for (long i = 0; i < N; i++) {
    if (fabs(L[i]) > peak) peak = fabs(L[i]);
    if (fabs(R[i]) > peak) peak = fabs(R[i]);
  }
  double g = peak > 0 ? 0.84 / peak : 1.0;
  long fi = (long)(0.004 * SR), fo = (long)(0.8 * SR);
  for (long i = 0; i < N; i++) { L[i] *= (float)g; R[i] *= (float)g; }
  for (long i = 0; i < fi && i < N; i++) { L[i] *= (float)i / fi; R[i] *= (float)i / fi; }
  for (long i = 0; i < fo && i < N; i++) { L[N-1-i] *= (float)i / fo; R[N-1-i] *= (float)i / fo; }

  if (!write_wav_f32_stereo(outPath, L, R, N)) { fprintf(stderr, "emotrib: cannot write %s\n", outPath); return 1; }
  fprintf(stderr, "emotrib: %d bars, %.1f BPM, %.1f s -> %s\n", totalBars, BPMV, (double)N / SR, outPath);
  return 0;
}
