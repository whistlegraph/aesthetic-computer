// squeakysines.c — adversarial sine/HRTF test song for Special Sign.
//
// Uses the production procedural HRTF verbatim, then exposes the signal path as
// music: raw carrier -> clean moving field -> 24% release blend -> isolated
// pinna residue -> full moving receiver -> an exaggerated residue finale.
#define _POSIX_C_SOURCE 200809L
#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "../../c/ac_hrtf.h"

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif
#define TAU (2.0 * M_PI)
#define SR 48000
#define PROBE_DUR 8.0
#define SONG_DUR 48.0
#define EXPLOIT_DUR 72.0

enum Stage { STAGE_RAW, STAGE_CLEAN, STAGE_BLEND, STAGE_HRTF, STAGE_RESIDUE, STAGE_FULL };

typedef struct {
  double at, dur, hz, gain, attack, release, motion, phase;
  enum Stage stage;
} Note;

static float *busL, *busR;
static long sampleCount;
static double duration = SONG_DUR;

static double clamp(double x, double lo, double hi) { return x < lo ? lo : x > hi ? hi : x; }
static double smooth(double x) { x = clamp(x, 0, 1); return x * x * (3 - 2 * x); }

static enum Stage parse_stage(const char *s) {
  if (!strcmp(s, "raw")) return STAGE_RAW;
  if (!strcmp(s, "clean")) return STAGE_CLEAN;
  if (!strcmp(s, "blend")) return STAGE_BLEND;
  if (!strcmp(s, "hrtf")) return STAGE_HRTF;
  if (!strcmp(s, "residue")) return STAGE_RESIDUE;
  if (!strcmp(s, "full")) return STAGE_FULL;
  fprintf(stderr, "unknown stage: %s\n", s);
  exit(2);
}

// A deliberately smooth trajectory. Any bright gesture is therefore produced
// by the moving receiver filters/delay, not a discontinuous control signal.
static void trajectory(double u, double motion, double phase,
                       double *az, double *el, double *distance) {
  double ramp = smooth(sin(M_PI * clamp(u, 0, 1)));
  double turns = 0.55 + 2.95 * motion;
  double a = TAU * turns * u + phase;
  *az = ramp * (0.35 + 0.90 * motion) * sin(a);
  *el = ramp * (0.18 + 1.02 * motion) * sin(a * 1.73 + 0.6);
  *distance = 2.8 + ramp * (1.35 * sin(a * 0.71 - 0.4) - 0.95 * cos(a));
  *distance = fmax(0.72, *distance);
}

static double envelope(const Note *n, double local) {
  if (local < 0 || local >= n->dur) return 0;
  double a = n->attack > 0 ? smooth(local / n->attack) : 1;
  double r = n->release > 0 ? smooth((n->dur - local) / n->release) : 1;
  return fmin(a, r);
}

static void deposit(long at, double fraction, double l, double r) {
  // Same four-point cubic B-spline deposit used by spatial-sineabye.c.
  if (at < 1 || at + 2 >= sampleCount) return;
  double f2 = fraction * fraction, f3 = f2 * fraction;
  double w0 = (1 - 3 * fraction + 3 * f2 - f3) / 6;
  double w1 = (4 - 6 * f2 + 3 * f3) / 6;
  double w2 = (1 + 3 * fraction + 3 * f2 - 3 * f3) / 6;
  double w3 = f3 / 6;
  busL[at - 1] += (float)(l * w0); busR[at - 1] += (float)(r * w0);
  busL[at]     += (float)(l * w1); busR[at]     += (float)(r * w1);
  busL[at + 1] += (float)(l * w2); busR[at + 1] += (float)(r * w2);
  busL[at + 2] += (float)(l * w3); busR[at + 2] += (float)(r * w3);
}

static void render_note(const Note *n) {
  ACHrtf h;
  memset(&h, 0, sizeof(h));
  long frames = (long)llround(n->dur * SR);
  double oscillatorPhase = 0;
  for (long i = 0; i < frames; i++) {
    double local = i / (double)SR;
    double env = envelope(n, local);
    oscillatorPhase += TAU * n->hz / SR;
    double carrier = sin(oscillatorPhase) * env * n->gain;
    double az, el, distance;
    trajectory(local / n->dur, n->motion, n->phase, &az, &el, &distance);

    double pan = sin(az);
    double near = .012 + .988 / (1 + .18 * distance * distance);
    double cleanL = carrier * sqrt((1 - pan) * .5) * near;
    double cleanR = carrier * sqrt((1 + pan) * .5) * near;
    float hl, hr;
    ac_hrtf_process(&h, (float)carrier, az, el, distance, &hl, &hr);

    double l, r;
    switch (n->stage) {
      case STAGE_RAW:
        l = r = carrier * M_SQRT1_2;
        break;
      case STAGE_CLEAN:
        l = cleanL; r = cleanR;
        break;
      case STAGE_HRTF:
        l = hl; r = hr;
        break;
      case STAGE_RESIDUE:
        // Audition the precise detail introduced by the release's 24% HRTF
        // blend, with musical makeup gain so it can stand alone as a voice.
        l = (hl - cleanL) * .24 * 3.25;
        r = (hr - cleanR) * .24 * 3.25;
        break;
      case STAGE_BLEND:
      case STAGE_FULL:
      default:
        l = cleanL * .76 + hl * .24;
        r = cleanR * .76 + hr * .24;
        break;
    }

    double destination = (n->at * SR) + i;
    if (n->stage == STAGE_FULL) destination += distance * SR / 343.0;
    long at = (long)floor(destination);
    double fraction = destination - at;
    if (n->stage == STAGE_FULL) deposit(at, fraction, l, r);
    else if (at >= 0 && at < sampleCount) { busL[at] += (float)l; busR[at] += (float)r; }
  }
}

static void phrase(double start, enum Stage stage, double motion, double gain, int transpose) {
  // The first six lead pitches and timing profile of Special Sign's release
  // entrance, compacted into a repeatable four-second diagnostic phrase.
  static const double at[]  = {0.000, .773, 1.259, 1.558, 2.032, 2.841};
  static const double dur[] = {.357, .333, .318, .379, .735, .333};
  static const double hz[]  = {349.228231, 440.0, 523.251131, 440.0, 391.995436, 349.228231};
  static const double atk[] = {.030, .097, .040, .036, .024, .193};
  static const double rel[] = {.335, .377, .266, .296, .529, .232};
  for (int repeat = 0; repeat < 2; repeat++) for (int i = 0; i < 6; i++) {
    Note n = {
      start + repeat * 4.0 + at[i], dur[i], hz[i] * pow(2, transpose / 12.0),
      gain * (i == 3 ? 1.10 : 1), atk[i], rel[i], motion,
      .37 * i + repeat * 1.19, stage
    };
    render_note(&n);
  }
}

static void render_probe(enum Stage stage) {
  phrase(0, stage, stage == STAGE_RAW ? 0 : .72, .23, 0);
}

static void render_song(void) {
  phrase(0,  STAGE_RAW,     0,   .17, 0);
  phrase(8,  STAGE_CLEAN,   .48, .19, 0);
  phrase(16, STAGE_BLEND,   .72, .21, 0);
  phrase(24, STAGE_RESIDUE, .84, .23, 0);
  phrase(32, STAGE_FULL,    1.0, .22, 0);
  // The residue becomes the arrangement: two interlocking octave registers
  // drive the pinna taps harder, but all trajectories remain C1-smooth.
  phrase(40, STAGE_RESIDUE, 1.45, .22, 12);
  phrase(40, STAGE_RESIDUE, 1.20, .16, 0);
}

static void render_exploit(void) {
  // Residue only: nine matched passes make the moving pinna-comb mechanism
  // legible as motion rises from a slow bend to an octave-stacked squeak choir.
  static const double motion[] = {.20, .34, .50, .68, .86, 1.05, 1.25, 1.48, 1.72};
  for (int section = 0; section < 9; section++) {
    double at = section * 8.0;
    phrase(at, STAGE_RESIDUE, motion[section], .205, section >= 6 ? 12 : 0);
    if (section >= 7) phrase(at, STAGE_RESIDUE, motion[section] * .83, .125, 0);
    if (section == 8) phrase(at, STAGE_RESIDUE, motion[section] * .67, .072, -12);
  }
}

static void add_room(void) {
  const int delays[] = {3408, 5424, 8688, 14400};
  const double gains[] = {.075, .048, .030, .018};
  float *dryL = malloc((size_t)sampleCount * sizeof(float));
  float *dryR = malloc((size_t)sampleCount * sizeof(float));
  if (!dryL || !dryR) { free(dryL); free(dryR); return; }
  memcpy(dryL, busL, (size_t)sampleCount * sizeof(float));
  memcpy(dryR, busR, (size_t)sampleCount * sizeof(float));
  for (int tap = 0; tap < 4; tap++) for (long i = delays[tap]; i < sampleCount; i++) {
    // Only the full-stack song section gets the immutable FIR room image.
    double t = i / (double)SR;
    if (t >= 32 && t < 40.5) {
      busL[i] += dryR[i - delays[tap]] * (float)gains[tap];
      busR[i] += dryL[i - delays[tap]] * (float)gains[tap];
    }
  }
  free(dryL); free(dryR);
}

static int write_wav(const char *path) {
  double peak = 0;
  for (long i = 0; i < sampleCount; i++) peak = fmax(peak, fmax(fabs(busL[i]), fabs(busR[i])));
  double gain = peak > 0 ? .89 / peak : 1;
  long fade = (long)(.035 * SR);
  for (long i = 0; i < sampleCount; i++) {
    double edge = 1;
    if (i < fade) edge *= smooth(i / (double)fade);
    if (i >= sampleCount - fade) edge *= smooth((sampleCount - 1 - i) / (double)fade);
    busL[i] = (float)(busL[i] * gain * edge);
    busR[i] = (float)(busR[i] * gain * edge);
  }
  FILE *f = fopen(path, "wb");
  if (!f) return 0;
  uint32_t dataSize = (uint32_t)(sampleCount * 8), riffSize = 36 + dataSize;
  uint32_t sampleRate = SR, byteRate = SR * 8, fmtSize = 16;
  uint16_t format = 3, channels = 2, blockAlign = 8, bits = 32;
  fwrite("RIFF", 1, 4, f); fwrite(&riffSize, 4, 1, f); fwrite("WAVEfmt ", 1, 8, f);
  fwrite(&fmtSize, 4, 1, f); fwrite(&format, 2, 1, f); fwrite(&channels, 2, 1, f);
  fwrite(&sampleRate, 4, 1, f); fwrite(&byteRate, 4, 1, f);
  fwrite(&blockAlign, 2, 1, f); fwrite(&bits, 2, 1, f);
  fwrite("data", 1, 4, f); fwrite(&dataSize, 4, 1, f);
  for (long i = 0; i < sampleCount; i++) { fwrite(&busL[i], 4, 1, f); fwrite(&busR[i], 4, 1, f); }
  int ok = !ferror(f); fclose(f); return ok;
}

int main(int argc, char **argv) {
  const char *out = "squeakysines.wav", *stageName = NULL;
  int probe = 0, exploit = 0;
  for (int i = 1; i < argc; i++) {
    if (!strcmp(argv[i], "--out") && i + 1 < argc) out = argv[++i];
    else if (!strcmp(argv[i], "--probe") && i + 1 < argc) { probe = 1; stageName = argv[++i]; }
    else if (!strcmp(argv[i], "--exploit")) exploit = 1;
  }
  duration = probe ? PROBE_DUR : exploit ? EXPLOIT_DUR : SONG_DUR;
  sampleCount = (long)llround(duration * SR);
  busL = calloc((size_t)sampleCount, sizeof(float));
  busR = calloc((size_t)sampleCount, sizeof(float));
  if (!busL || !busR) return 1;
  if (probe) render_probe(parse_stage(stageName));
  else if (exploit) render_exploit();
  else { render_song(); add_room(); }
  if (!write_wav(out)) { fprintf(stderr, "could not write %s\n", out); return 1; }
  fprintf(stderr, "✓ %s · %.1fs · %s\n", out, duration,
          probe ? stageName : exploit ? "pinna-residue exploit" : "stack-as-song");
  free(busL); free(busR); return 0;
}
