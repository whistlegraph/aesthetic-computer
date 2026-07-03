// emotribtranse.c — "emo trance." The emotrib motif dragged onto a sweaty
// trance floor, rendered ENTIRELY in C (no JS, no samples): a harsh
// overdriven four-on-the-floor kick, offbeat bass stabs over a gliding 808
// under-layer, rolling sixteenth arps, open-hat offbeats, claps on 2 & 4,
// the sad F-minor bell melody doubled by a marimba with dotted-eighth echo
// trails — and then THE MELT: over the last twenty bars the entire mix pours
// through a drip-modulated delay into a triangle wavefolder whose drive
// climbs to the final sample, so the whole track dissolves into a drippy
// triangle-wave mess that gets sharper and sharper until the fade.
//
// House style (per pop/emotrib/c/emotrib.c + pop/hellsine/c): tonal voices
// are sums of sin() phases advanced by freq/SR, rounded with a memoryless
// tanh for grit; percussion (hats, claps, sweeps, vinyl) is the one licensed
// exception — xorshift noise shaped by one-pole filters + envelopes. Nothing
// here reads a WAV.
//
// Form (155 BPM, straight four): intro → build 1 → drop A1 → break →
// build 2 → drop A2 (climax) → MELT (climax keeps pounding while the fold
// takes over) → melted outro.
//
// Build:  bash emotribtranse/c/build.sh
// Run:    ./emotribtranse --out out/emotribtranse-raw.wav [--bpm 155]

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
static double BPMV = 155, BEAT, BAR;
static long N; // total samples

// ── tiny rng (xorshift) — for noise perc + vinyl + humanized timing ────────
static uint32_t rng_s = 0x7472616e; // "tran"
static inline double rnd(void) {
  rng_s ^= rng_s << 13; rng_s ^= rng_s >> 17; rng_s ^= rng_s << 5;
  return (double)rng_s / 4294967296.0;
}
static inline double rnd2(void) { return rnd() * 2.0 - 1.0; }
static inline double midi_hz(double m) { return 440.0 * pow(2.0, (m - 69.0) / 12.0); }
static inline double clampd(double x, double a, double b) { return x < a ? a : x > b ? b : x; }

// ── buses ──────────────────────────────────────────────────────────────────
//   bass  : kick + 808 + offbeat stabs (low end, breathes a little)
//   mus   : bell/marimba/arp/pad melodic content (pumps hard with the kick)
//   drum  : claps + hats + sweeps (dry, punchy)
//   rev   : reverb send (bell + marimba + clap tail)
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

// TRANCE KICK — the harsh one. Sine with a steep pitch drop driven way past
// polite (tanh × 4.5) so the body squares off, plus a 4 ms highpassed noise
// click for the beater edge. Stamps a DEEP sidechain duck — the pump is the
// sweat of the whole record.
static void tkick(double t, double gain) {
  long s0 = (long)(t * SR);
  long len = (long)(0.20 * SR);
  double phase = 0, h1 = 0, p1 = 0;
  for (long i = 0; i < len; i++) {
    double tt = (double)i / SR;
    double f = 46.0 + 198.0 * exp(-tt / 0.013);   // 244→46 Hz drop, steep
    double amp = (1.0 - exp(-tt / 0.0008)) * exp(-tt / 0.075);
    phase += f / SR;
    double body = tanh(sin(TAU * phase) * 4.5) * 0.92; // harsh, squared-off
    double click = 0;
    if (tt < 0.004) {
      double w = rnd2();
      h1 = 0.5 * (h1 + w - p1); p1 = w;           // HP the click noise
      click = h1 * (1.0 - tt / 0.004) * 0.85;
    }
    double s = (body + click) * amp * gain;
    addB(s0 + i, s, s);
  }
  // stamp duck: a brutal 1→0.22 dip recovering over ~240ms — the trance pump.
  long dl = (long)(0.28 * SR);
  for (long i = 0; i < dl; i++) {
    long j = s0 + i; if (j < 0 || j >= N) continue;
    double tt = (double)i / SR;
    double d = 0.22 + 0.78 * (1.0 - exp(-tt / 0.10));
    if (d < duck[j]) duck[j] = (float)d;
  }
}

// 808 SUB — kept from emotrib: sine carrier, attack chirp, portamento glide
// in from the previous root. Here it's the warm under-layer beneath the
// offbeat stabs (and the star of the builds), not the lead low voice.
static void e808(double t, double midi, double durBeats, double fromMidi, double gain) {
  long s0 = (long)(t * SR);
  double dur = durBeats * BEAT;
  long len = (long)((dur + 0.30) * SR);
  double f0 = midi_hz(midi);
  double fprev = fromMidi >= 0 ? midi_hz(fromMidi) : f0;
  double phase = 0, drift = rnd2() * 0.5;
  for (long i = 0; i < len; i++) {
    double tt = (double)i / SR;
    double gl = fromMidi >= 0 ? clampd(tt / 0.07, 0, 1) : 1.0; // 70ms glide
    double f = fprev * pow(f0 / fprev, gl) + drift;
    double click = 1.0 + 5.0 * exp(-tt / 0.010);
    double amp = (1.0 - exp(-tt / 0.003)) * exp(-tt / (dur * 0.55 + 0.05));
    phase += f * click / SR;
    double x = sin(TAU * phase);
    x = tanh(x * 1.7) * 0.78;
    double s = x * amp * gain;
    addB(s0 + i, s, s);
  }
}

// OFFBEAT STAB — the trance bass: a short saturated root hit on every "and",
// living in the pocket the kick's duck carves out. Slightly brighter than the
// 808 (second partial + more drive) so it reads as a stab, not a sub swell.
static void stab(double t, double midi, double gain) {
  long s0 = (long)(t * SR);
  long len = (long)(0.16 * SR);
  double f = midi_hz(midi);
  double pa = 0, pb = 0;
  for (long i = 0; i < len; i++) {
    double tt = (double)i / SR;
    double amp = (1.0 - exp(-tt / 0.002)) * exp(-tt / 0.055);
    pa += f / SR; pb += (f * 2.0) / SR;
    double x = tanh((sin(TAU * pa) + 0.35 * sin(TAU * pb)) * 2.6) * 0.7;
    double s = x * amp * gain;
    addB(s0 + i, s, s);
  }
}

// HI-HAT — crisp metallic tick (from emotrib): white noise through two
// cascaded one-pole highpasses, fast-decay env, panned slightly per hit.
static void hat(double t, double vel, int open) {
  long s0 = (long)(t * SR);
  double dec = open ? 0.060 : 0.013;
  long len = (long)((dec * 3.5) * SR);
  double h1 = 0, h2 = 0, p1 = 0, p2 = 0, pan = rnd2() * 0.25;
  for (long i = 0; i < len; i++) {
    double tt = (double)i / SR;
    double w = rnd2();
    h1 = 0.55 * (h1 + w - p1); p1 = w;
    h2 = 0.55 * (h2 + h1 - p2); p2 = h1;
    double amp = exp(-tt / dec) * vel * 0.42;
    double s = h2 * amp;
    addD(s0 + i, s * (0.82 - 0.4 * pan), s * (0.82 + 0.4 * pan));
  }
}

// CLAP — the emotrib snare/clap: noise body + 3-tap stutter + a 190 Hz ping,
// with a reverb send. Lands on 2 & 4 here (trance), plus the build rolls.
static void clap(double t, double gain) {
  long s0 = (long)(t * SR);
  long len = (long)(0.34 * SR);
  double hp = 0, prevN = 0, lp = 0, pphase = 0;
  double taps[4] = { 0.0, 0.011, 0.021, 0.030 };
  for (long i = 0; i < len; i++) {
    double tt = (double)i / SR;
    double body = 0;
    for (int k = 0; k < 4; k++) {
      double dt = tt - taps[k];
      if (dt < 0) continue;
      body += rnd2() * exp(-dt / (k == 0 ? 0.13 : 0.05));
    }
    hp = 0.80 * (hp + body - prevN); prevN = body;
    lp = lp + 0.55 * (hp - lp);
    double tone = sin(TAU * pphase); pphase += 190.0 / SR;
    double tamp = exp(-tt / 0.05);
    double s = (lp * 0.9 + tone * tamp * 0.35) * gain;
    addD(s0 + i, s, s);
    addR(s0 + i, s * 0.30, s * 0.30);
  }
}

// SWEEP — rising filtered-noise riser under the last bars of a build: white
// noise through a one-pole lowpass whose cutoff and level both climb, so the
// air itself tightens into the drop.
static void sweep(double t0, double durBeats, double gain) {
  long s0 = (long)(t0 * SR);
  double dur = durBeats * BEAT;
  long len = (long)(dur * SR);
  double lp = 0;
  for (long i = 0; i < len; i++) {
    double prog = (double)i / len;
    double w = rnd2();
    double coef = 0.015 + 0.45 * prog * prog;   // cutoff opens as it rises
    lp = lp + coef * (w - lp);
    double s = lp * prog * prog * gain;
    addD(s0 + i, s * 0.9, s);
    addR(s0 + i, s * 0.25, s * 0.25);
  }
}

// BELL — the emotrib melodic voice, unchanged: detuned struck tone with
// shimmer partials, pluck envelope, reverb send. This is the emo thread.
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
    pa += (f - det) / SR; pb += (f + det) / SR;
    pc += (f * 2.01) / SR; pd += (f * 3.0) / SR;
    double l = (sin(TAU * pa) + 0.5 * sin(TAU * pc) + 0.22 * sin(TAU * pd)) * amp;
    double r = (sin(TAU * pb) + 0.5 * sin(TAU * pc) + 0.22 * sin(TAU * pd)) * amp;
    l = tanh(l * 1.1) * 0.32; r = tanh(r * 1.1) * 0.32;
    addM(s0 + i, l, r);
    addR(s0 + i, l * 0.45, r * 0.45);
  }
}

// MARIMBA — the new wooden voice. Partials at 1×, 3.98× and 9.2× (the deep
// bar-arch tuning that makes a marimba sound like a marimba), each with its
// own fast decay, plus a 1.5 ms knock transient. Alternating pan per strike.
// Stays at melody pitch (no octave-up runs — laptop speakers get tangy).
static int marPanFlip = 0;
static void marimba(double t, double midi, double vel) {
  long s0 = (long)(t * SR);
  long len = (long)(0.50 * SR);
  double f = midi_hz(midi);
  double pa = 0, pb = 0, pc = 0;
  double pan = (marPanFlip++ & 1) ? 0.30 : -0.30;
  for (long i = 0; i < len; i++) {
    double tt = (double)i / SR;
    double atk = 1.0 - exp(-tt / 0.0015);
    double a1 = exp(-tt / 0.16), a2 = exp(-tt / 0.045), a3 = exp(-tt / 0.015);
    pa += f / SR; pb += (f * 3.98) / SR; pc += (f * 9.2) / SR;
    double x = (sin(TAU * pa) * a1
              + 0.42 * sin(TAU * pb) * a2
              + 0.15 * sin(TAU * pc) * a3) * atk * vel * 0.30;
    addM(s0 + i, x * (1.0 - pan * 0.5), x * (1.0 + pan * 0.5));
    addR(s0 + i, x * 0.35, x * 0.35);
  }
}

// ARP — the rolling sixteenth: a short bitey pluck (detuned pair + second
// partial, driven tanh) cycling chord tones. THE trance engine room; pumps
// hard against the kick via the duck.
static void arpn(double t, double midi, double vel) {
  long s0 = (long)(t * SR);
  long len = (long)(0.13 * SR);
  double f = midi_hz(midi);
  double det = f * 0.004;
  double pa = 0, pb = 0, pc = 0;
  for (long i = 0; i < len; i++) {
    double tt = (double)i / SR;
    double amp = (1.0 - exp(-tt / 0.0015)) * exp(-tt / 0.045) * vel;
    pa += (f - det) / SR; pb += (f + det) / SR; pc += (f * 2.0) / SR;
    double l = tanh((sin(TAU * pa) + 0.4 * sin(TAU * pc)) * 2.2) * 0.20 * amp;
    double r = tanh((sin(TAU * pb) + 0.4 * sin(TAU * pc)) * 2.2) * 0.20 * amp;
    addM(s0 + i, l, r);
    addR(s0 + i, l * 0.20, r * 0.20);
  }
}

// PAD — soft sustained chord (from emotrib), the emotional bed.
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
      ph[v] += f / SR; ph2[v] += (f * 1.004) / SR;
      l += sin(TAU * ph[v]);
      r += sin(TAU * ph2[v]);
    }
    double scale = env * 0.10 / (nv > 0 ? nv : 1);
    addM(s0 + i, l * scale, r * scale);
  }
}

// VINYL CRACKLE — the emo thread of texture, kept under everything.
static void vinyl(double t0, double dur, double gain) {
  long s0 = (long)(t0 * SR), len = (long)(dur * SR);
  double lp = 0;
  for (long i = 0; i < len; i++) {
    double w = rnd2();
    lp = lp + 0.04 * (w - lp);
    double s = lp * 0.5;
    if (rnd() > 0.99955) s += rnd2() * 0.9;
    s *= gain;
    addM(s0 + i, s, s);
  }
}

// ════════════════════════════════════════════════════════════════════════
//  COMPOSITION — F minor.  i–VI–III–VII  (Fm – Db – Ab – Eb), one bar each.
//  Melody tables copied verbatim from emotrib — same sad line, new floor.
// ════════════════════════════════════════════════════════════════════════

enum { F2=41, Ab1=32, Db2=37, Eb2=39,
       G3=55, Ab3=56, Bb3=58, C4=60, Db4=61, Eb4=63, F4=65, G4=67, Ab4=68 };

static const int ROOTS[4]   = { F2, Db2, Ab1, Eb2 };
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
static const int PAD[4][3] = {
  /* Fm */ { 53, 56, 60 },
  /* Db */ { 49, 53, 56 },
  /* Ab */ { 56, 60, 63 },
  /* Eb */ { 51, 55, 58 },
};

// swing/humanize: nudge a hit by a few ms (drums stay on the grid — trance).
static double hum(double t) { return t + rnd2() * 0.005; }

// hats for one bar. 1 = offbeat opens (the trance chk), 2 = + sixteenth
// ticks, 3 = busy + a 32nd roll into the next bar. Sweat scales with level.
static void barHats(double bt, int density) {
  if (density <= 0) return;
  for (int b = 0; b < 4; b++)
    hat(bt + (b + 0.5) * BEAT, 0.85, 1);                 // offbeat opens
  if (density >= 2)
    for (int s = 0; s < 16; s++) {
      if ((s % 4) == 2) continue;                        // opens own that slot
      hat(hum(bt + s / 16.0 * 4.0 * BEAT), 0.30 + 0.18 * ((s % 4) == 0), 0);
    }
  if (density >= 3) {
    double rb = bt + 3.5 * BEAT;                         // roll up the turn
    for (int k = 0; k < 8; k++)
      hat(rb + k / 8.0 * 0.5 * BEAT, 0.30 + 0.55 * k / 8.0, 0);
  }
}

// accelerating clap roll for build bars: quarters → eighths → sixteenths as
// the build progresses (prog 0..1 across the build section).
static void barBuildRoll(double bt, double prog) {
  int div = prog < 0.5 ? 4 : (prog < 0.8 ? 8 : 16);
  for (int s = 0; s < div; s++)
    clap(bt + (double)s / div * 4.0 * BEAT, 0.30 + 0.45 * prog);
}

// one full bar. flags pick the layers; melVar picks the emotrib melody row.
typedef struct {
  int kick, stabs, sub808, hats, claps, arp, arpOct,
      melody, melOct, pad, marimba, melVar, build;
} Bar;

static void renderBar(double bt, int chord, int prevRootMidi, Bar cfg, double buildProg) {
  int root = ROOTS[chord];

  if (cfg.kick)
    for (int b = 0; b < 4; b++) tkick(bt + b * BEAT, b == 0 ? 1.0 : 0.94);

  if (cfg.stabs)
    for (int b = 0; b < 4; b++) stab(bt + (b + 0.5) * BEAT, root + 12, 0.85);

  if (cfg.sub808)
    e808(bt, root, 3.6, prevRootMidi, cfg.stabs ? 0.45 : 0.90); // under-layer vs star

  if (cfg.claps) { clap(bt + 1.0 * BEAT, 0.8); clap(bt + 3.0 * BEAT, 0.9); }

  barHats(bt, cfg.hats);
  if (cfg.build) barBuildRoll(bt, buildProg);

  if (cfg.pad) pad(bt, PAD[chord], 3, 4.0, 1.0);

  // rolling sixteenth arp over the chord triad + octave root
  if (cfg.arp) {
    int oct = cfg.arpOct ? 12 : 0;
    int tones[4] = { PAD[chord][0] + oct, PAD[chord][1] + oct,
                     PAD[chord][2] + oct, PAD[chord][0] + 12 + oct };
    for (int s = 0; s < 16; s++) {
      double t = bt + s / 16.0 * 4.0 * BEAT;
      double vel = 0.55 + 0.30 * ((s % 4) == 0) + 0.06 * rnd();
      arpn(t, tones[s % 4], vel);
    }
  }

  // melody: emotrib bells + the marimba doubling with dotted-eighth echoes
  if (cfg.melody) {
    for (int s = 0; s < 8; s++) {
      int m = MEL[cfg.melVar][chord][s];
      if (m < 0) continue;
      double t = bt + (double)s / 8.0 * 4.0 * BEAT;
      double vel = 0.85 + 0.1 * rnd();
      bell(hum(t), m + (cfg.melOct ? 12 : 0), 0.9, vel);
      if (cfg.melOct) bell(hum(t), m, 0.9, vel * 0.5);
      if (cfg.marimba) {
        double sx = BEAT / 4.0;                       // one sixteenth
        marimba(hum(t), m, vel * 0.85);               // strike with the bell
        marimba(t + 3.0 * sx, m, vel * 0.48);         // dotted-eighth echo…
        marimba(t + 6.0 * sx, m, vel * 0.26);         // …and its echo's echo
      }
    }
  }
}

// triangle wavefolder — maps any amplitude onto a period-4 triangle through
// the origin, so hot signals fold back on themselves. Higher drive in →
// more folds → sharper. This is what the whole track melts into.
static inline double trifold(double x) {
  double p = (x - 1.0) * 0.25;
  p -= floor(p);
  return fabs(p * 4.0 - 2.0) - 1.0;
}

// the arrangement.
int main(int argc, char **argv) {
  const char *outPath = "out/emotribtranse-raw.wav";
  for (int i = 1; i < argc; i++) {
    if (!strcmp(argv[i], "--out") && i + 1 < argc) outPath = argv[++i];
    else if (!strcmp(argv[i], "--bpm") && i + 1 < argc) BPMV = atof(argv[++i]);
  }
  BEAT = 60.0 / BPMV; BAR = BEAT * 4.0;

  // section table. melt=1 marks where THE MELT starts eating the mix.
  struct { int bars; int melt; Bar cfg; } sections[] = {
    //            kick stab 808 hats clap arp aO mel mO pad mar mV bld
    { 8,  0, (Bar){ 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 0 } }, // intro: pad+motif+marimba
    { 8,  0, (Bar){ 1, 0, 1, 1, 0, 0, 0, 1, 0, 1, 1, 0, 1 } }, // build 1: kick + gliding 808
    { 16, 0, (Bar){ 1, 1, 0, 2, 1, 1, 0, 1, 0, 1, 1, 1, 0 } }, // A1: the floor arrives
    { 8,  0, (Bar){ 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 3, 0 } }, // break: bells + echoes breathe
    { 8,  0, (Bar){ 1, 0, 1, 2, 0, 1, 0, 1, 0, 1, 0, 0, 1 } }, // build 2: roll tightens
    { 16, 0, (Bar){ 1, 1, 0, 3, 1, 1, 1, 1, 1, 1, 1, 2, 0 } }, // A2: climax, everything up
    { 16, 1, (Bar){ 1, 1, 1, 3, 1, 1, 1, 1, 1, 1, 1, 2, 0 } }, // MELT: climax keeps pounding as it folds
    { 4,  0, (Bar){ 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 3, 0 } }, // outro: fully melted drips, fading
  };
  int nSec = (int)(sizeof(sections) / sizeof(sections[0]));

  int totalBars = 0, meltStartBar = -1, acc = 0;
  for (int s = 0; s < nSec; s++) {
    if (sections[s].melt && meltStartBar < 0) meltStartBar = acc;
    acc += sections[s].bars; totalBars += sections[s].bars;
  }
  double dur = totalBars * BAR + 1.5;         // +tail for melted ring-out
  N = (long)(dur * SR);

  bassL = calloc(N, sizeof(float)); bassR = calloc(N, sizeof(float));
  musL  = calloc(N, sizeof(float)); musR  = calloc(N, sizeof(float));
  drumL = calloc(N, sizeof(float)); drumR = calloc(N, sizeof(float));
  revL  = calloc(N, sizeof(float)); revR  = calloc(N, sizeof(float));
  duck  = malloc(N * sizeof(float));
  for (long i = 0; i < N; i++) duck[i] = 1.0f;

  vinyl(0.0, dur, 0.45);

  // walk the arrangement
  int barIdx = 0; double bt = 0; int prevRoot = -1;
  for (int s = 0; s < nSec; s++) {
    for (int b = 0; b < sections[s].bars; b++) {
      int chord = barIdx % 4;
      double buildProg = sections[s].bars > 1 ? (double)b / (sections[s].bars - 1) : 1.0;
      renderBar(bt, chord, prevRoot, sections[s].cfg, buildProg);
      // a rising sweep under the last 4 bars of each build section
      if (sections[s].cfg.build && b == sections[s].bars - 4)
        sweep(bt, 16.0, 0.55);
      prevRoot = ROOTS[chord];
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
      double accv = 0;
      for (int k = 0; k < 3; k++) {
        double y = cb[k][ci[k]];
        cb[k][ci[k]] = (float)(in + y * 0.78);
        ci[k] = (ci[k] + 1) % combLen[k];
        accv += y;
      }
      accv /= 3.0;
      double yo = ap[ai];
      double xo = accv + yo * 0.5;
      ap[ai] = (float)xo; ai = (ai + 1) % apLen;
      double wet = yo - accv * 0.5;
      revL[i] = (float)(wet * 0.9);
      revR[i] = (float)(wet * 1.0);
    }
    for (int k = 0; k < 3; k++) free(cb[k]); free(ap);
  }

  // ── mixdown: kick-keyed pump on bass + music, sum buses ──────────────────
  float *L = calloc(N, sizeof(float)), *R = calloc(N, sizeof(float));
  for (long i = 0; i < N; i++) {
    double d = duck[i];
    double l = bassL[i] * (0.55 + 0.45 * d)      // low end breathes
             + musL[i]  * (0.14 + 0.86 * d)      // melodic bed pumps HARD
             + drumL[i] * 0.9
             + revL[i]  * (0.35 + 0.65 * d);
    double r = bassR[i] * (0.55 + 0.45 * d)
             + musR[i]  * (0.14 + 0.86 * d)
             + drumR[i] * 0.9
             + revR[i]  * (0.35 + 0.65 * d);
    L[i] = (float)l; R[i] = (float)r;
  }

  // ── THE MELT — from meltStartBar to the last sample, the whole mix pours
  // through a drip-modulated delay (two incommensurate LFOs per side, speeding
  // up as it goes, with growing feedback = echoing drips) and then a triangle
  // wavefolder whose drive keeps climbing. w is how much of the mix has
  // melted; drive is how sharp the folds are. Both only ever increase.
  if (meltStartBar >= 0) {
    long meltS = (long)(meltStartBar * BAR * SR);
    if (meltS < N) {
      enum { DMAX = 4096 };
      float *dbL = calloc(DMAX, sizeof(float)), *dbR = calloc(DMAX, sizeof(float));
      double ph1 = 0.0, ph2 = 0.31, ph3 = 0.77;
      double meltLen = (double)(N - meltS);
      for (long i = meltS; i < N; i++) {
        double m = (double)(i - meltS) / meltLen;      // 0 → 1 to the end
        double w = pow(m, 0.6);                        // takeover (fast in)
        double drive = 1.0 + pow(m, 1.4) * 16.0;       // sharper and sharper
        ph1 += (0.55 + 1.1 * m) / SR;                  // drips speed up
        ph2 += 0.43 / SR;
        ph3 += 0.185 / SR;
        double depth = (0.0006 + 0.0075 * m) * SR;
        double base = 0.006 * SR;
        double dL = base + depth * (0.5 + 0.5 * sin(TAU * ph1)) * (0.6 + 0.4 * sin(TAU * ph3));
        double dR = base + depth * (0.5 + 0.5 * sin(TAU * ph1 + 1.7)) * (0.6 + 0.4 * sin(TAU * ph2));
        long widx = i % DMAX;
        double rp, fr; long r0, r1; double rdL, rdR;
        rp = (double)widx - dL; while (rp < 0) rp += DMAX;
        r0 = (long)rp; r1 = (r0 + 1) % DMAX; fr = rp - r0;
        rdL = dbL[r0] * (1 - fr) + dbL[r1] * fr;
        rp = (double)widx - dR; while (rp < 0) rp += DMAX;
        r0 = (long)rp; r1 = (r0 + 1) % DMAX; fr = rp - r0;
        rdR = dbR[r0] * (1 - fr) + dbR[r1] * fr;
        double fb = 0.34 * m;                          // echoing drips
        dbL[widx] = (float)(L[i] + rdL * fb);
        dbR[widx] = (float)(R[i] + rdR * fb);
        double sigL = L[i] * (1.0 - 0.8 * w) + rdL * (0.8 * w);  // the drip
        double sigR = R[i] * (1.0 - 0.8 * w) + rdR * (0.8 * w);
        double flL = trifold(sigL * drive) * 0.95;               // the triangle
        double flR = trifold(sigR * drive) * 0.95;
        L[i] = (float)(sigL * (1.0 - w) + flL * w);              // the mess
        R[i] = (float)(sigR * (1.0 - w) + flR * w);
      }
      free(dbL); free(dbR);
    }
  }

  // normalize to -1.5 dBFS-ish peak (final loudness is the ffmpeg master's job)
  double peak = 0;
  for (long i = 0; i < N; i++) {
    if (fabs(L[i]) > peak) peak = fabs(L[i]);
    if (fabs(R[i]) > peak) peak = fabs(R[i]);
  }
  double g = peak > 0 ? 0.84 / peak : 1.0;
  long fi = (long)(0.004 * SR), fo = (long)(2.5 * SR);
  for (long i = 0; i < N; i++) { L[i] *= (float)g; R[i] *= (float)g; }
  for (long i = 0; i < fi && i < N; i++) { L[i] *= (float)i / fi; R[i] *= (float)i / fi; }
  for (long i = 0; i < fo && i < N; i++) { L[N-1-i] *= (float)i / fo; R[N-1-i] *= (float)i / fo; }

  if (!write_wav_f32_stereo(outPath, L, R, N)) { fprintf(stderr, "emotribtranse: cannot write %s\n", outPath); return 1; }
  fprintf(stderr, "emotribtranse: %d bars, %.1f BPM, %.1f s (melt from bar %d) -> %s\n",
          totalBars, BPMV, (double)N / SR, meltStartBar, outPath);
  return 0;
}
