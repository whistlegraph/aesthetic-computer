// taksmukkeklokken.c — orchestral, binaural Clock-language reinterpretation.
//
// The score and every synthesized voice are rendered here. The grand piano is
// the project-owned CC0 Salamander bank used by AC OS. Three optional WAV slots
// accept user-supplied, release-cleared arcade percussion; absent slots fall
// back to original synthesized impacts.
//
// Build: cc -O3 -std=c11 -Wall -Wextra -o taksmukkeklokken taksmukkeklokken.c -lm

#define _POSIX_C_SOURCE 200809L
#include <errno.h>
#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

#include "../../nullabye/c/ac_hrtf.h"

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif
#define TAU (2.0 * M_PI)
#define SR 48000
#define BPM 140.0
#define BEAT (60.0 / BPM)
#define BAR (4.0 * BEAT)
#define BARS 70
#define DURATION (BARS * BAR)
#define MAX_EVENTS 20000
#define PIANO_ANCHORS 32
#define SAMPLE_SLOTS 3

typedef enum {
  V_KICK, V_HAT_CLOSED, V_HAT_OPEN, V_CLAP, V_RIDE, V_CRASH, V_RISER,
  V_SUB, V_ACID, V_ACID_ECHO, V_STRING, V_PIZZ, V_PIANO, V_CLOCK, V_PAD,
  V_ARCADE_HIT
} Voice;

typedef struct {
  double at, dur, midi, gain, pan;
  Voice voice;
  int lane, articulation, sample_slot;
  uint32_t seed;
} Event;

typedef struct { float *data; long n; int midi; } PianoAnchor;
typedef struct { float *data; long n; char path[1024]; int loaded; } SampleSlot;

static Event events[MAX_EVENTS];
static int event_count;
static int counts[V_ARCADE_HIT + 1];
static uint32_t rng_state = 41203u;
static long N;
static float *musicL, *musicR, *drumsL, *drumsR;
static PianoAnchor piano[PIANO_ANCHORS];
static int piano_count;
static SampleSlot samples[SAMPLE_SLOTS];
static const char *piano_dir;
static const char *sample_dir;
static const char *out_path = "../out/taksmukkeklokken-c.raw.wav";
static const char *receipt_path = "../out/taksmukkeklokken-c.events.json";
static int score_only;
static double harshness;

static double clamp(double x, double lo, double hi) { return x < lo ? lo : x > hi ? hi : x; }
static double midi_hz(double midi) { return 440.0 * pow(2.0, (midi - 69.0) / 12.0); }
static uint32_t xorshift(uint32_t *s) { *s ^= *s << 13; *s ^= *s >> 17; *s ^= *s << 5; return *s; }
static double noise01(uint32_t *s) { return ((double)xorshift(s) / 4294967296.0) * 2.0 - 1.0; }
static double frand(void) { return ((double)xorshift(&rng_state) / 4294967296.0); }
static double ease(double u) { u = clamp(u, 0, 1); return u * u * u * (10 + u * (-15 + 6 * u)); }
static double smooth(double u) { u = clamp(u, 0, 1); return u * u * (3 - 2 * u); }
static double bar_time(int bar, double beat) { return bar * BAR + beat * BEAT; }

static const char *voice_name(Voice v) {
  static const char *names[] = {
    "kick", "closed-hat", "open-hat", "clap", "ride", "crash", "riser",
    "sub", "acid", "acid-echo", "strings", "pizzicato", "grand-piano",
    "clock-metal", "harmonic-pad", "arcade-percussion"
  };
  return names[v];
}

static const char *section_name(double at) {
  int b = (int)floor(at / BAR + 1e-8);
  if (b < 8) return "intro";
  if (b < 16) return "build";
  if (b < 24) return "acid";
  if (b < 40) return "drop-a";
  if (b < 48) return "breakdown";
  if (b < 64) return "drop-b";
  return "outro";
}

static void add_event(Voice voice, double at, double dur, double midi, double gain,
                      double pan, int lane, int articulation, int sample_slot) {
  if (event_count >= MAX_EVENTS) { fprintf(stderr, "event capacity exceeded\n"); exit(1); }
  if (at < 0 || at >= DURATION || dur <= 0) return;
  if (at + dur > DURATION) dur = DURATION - at;
  Event *e = &events[event_count++];
  *e = (Event){ at, dur, midi, gain, pan, voice, lane, articulation, sample_slot,
                xorshift(&rng_state) | 1u };
  counts[voice]++;
}

// Eight-bar harmonic sentence. The first half keeps the canonical Am/F/C/G
// identity; the second half develops it through predominant, inversion, leading
// tone, and dominant-b9 tension before returning to A minor.
static const int harmony[8][6] = {
  {45, 52, 57, 60, 64, 71}, // Am(add9)
  {45, 53, 57, 60, 64, 69}, // Fmaj7/A
  {43, 55, 60, 64, 71, 74}, // Cmaj9/G
  {43, 50, 55, 59, 64, 69}, // G6(add9)
  {41, 50, 57, 60, 64, 65}, // Dm9/F
  {40, 52, 57, 60, 64, 69}, // Am/E
  {41, 47, 53, 56, 62, 65}, // Bdim7/F
  {40, 47, 56, 62, 65, 68}, // E7(b9)
};
static const int roots[8] = {45, 41, 43, 43, 38, 40, 47, 40};

static void score_strings(void) {
  for (int b = 6; b < 68; b++) {
    int h = (b < 16) ? ((b - 8) / 2 & 3) : (b & 7);
    if (h < 0) h += 8;
    double at = bar_time(b, 0), breath = (b >= 40 && b < 48) ? .18 : .06;
    double g = b < 8 ? .10 : b < 16 ? .14 : b < 24 ? .17 : b < 40 ? .22 : b < 48 ? .18 : b < 64 ? .25 : .13;
    // Celli and basses carry a two-note line rather than a static pad.
    add_event(V_STRING, at + breath, BAR - .09, harmony[h][0] - 12, g * .92, 0, 0, 0, -1);
    add_event(V_STRING, at + 2 * BEAT + breath, 2 * BEAT - .08, harmony[h][1] - 12, g * .67, 0, 1, 0, -1);
    // Divisi viola/violin voicings preserve common tones across every bar.
    for (int k = 1; k < 6; k++) {
      int lane = 2 + k;
      int art = (b >= 48 && (k + b) % 3 == 0) ? 1 : 0;
      add_event(art ? V_PIZZ : V_STRING, at + breath + .011 * k,
                art ? .62 * BEAT : BAR - .10, harmony[h][k], g * (.67 + .055 * k),
                0, lane, art, -1);
    }
    // Drop B adds an independent high counterline in contrary motion.
    if (b >= 48 && b < 64) {
      static const int counter[16] = {76,79,81,83,81,79,76,74,72,74,76,79,77,76,74,71};
      for (int q = 0; q < 8; q++) {
        int note = counter[((b - 48) * 3 + q * 2) & 15];
        add_event(V_STRING, at + q * .5 * BEAT + .014, .46 * BEAT, note,
                  .105 + (q % 3 == 0 ? .025 : 0), 0, 9, 2, -1);
      }
    }
  }
}

static void score_piano(void) {
  static const int arp[16] = {0,2,4,1,3,5,2,4, 0,3,5,1,4,2,5,3};
  for (int b = 0; b < 70; b++) {
    int h = b & 7;
    double at = bar_time(b, 0);
    if (b < 8) {
      if (b == 0) add_event(V_PIANO, at + .035, 3.5 * BEAT, 45, .34, 0, 2, 0, -1);
      if (b >= 2) {
        int k = (b * 3) % 6;
        add_event(V_PIANO, at + (b & 1 ? 2.5 : 1.0) * BEAT, 1.6 * BEAT,
                  harmony[h][k] + 12, .20, 0, 4 + k, 0, -1);
      }
      continue;
    }
    int steps = b < 16 ? 8 : (b >= 40 && b < 48 ? 6 : 16);
    for (int s = 0; s < steps; s++) {
      double pos = 4.0 * s / steps;
      if (steps >= 8 && (s & 1)) pos += .08; // human swing against the clock grid
      int k = arp[(s + b * 3) & 15];
      int note = harmony[h][k] + ((s >= steps / 2 && b >= 24) ? 12 : 0);
      double gain = b < 16 ? .16 : b < 24 ? .18 : b < 40 ? .205 : b < 48 ? .19 : b < 64 ? .225 : .14;
      if (s == 0 || s == steps - 1) gain *= 1.18;
      add_event(V_PIANO, at + pos * BEAT + .010 + (frand() - .5) * .009,
                (steps == 16 ? .58 : .82) * BEAT, note, gain, 0, 3 + k, 0, -1);
    }
    // Left-hand octave and a rolled upper extension make phrase boundaries
    // pianistic rather than loop resets.
    add_event(V_PIANO, at + .006, 1.85 * BEAT, roots[h] - 12, .255, 0, 0, 1, -1);
    if ((b & 3) == 3) for (int k = 2; k < 6; k++)
      add_event(V_PIANO, at + 3.45 * BEAT + .018 * (k - 2), 1.4 * BEAT,
                harmony[h][k] + 12, .17 + .015 * k, 0, 5 + k, 1, -1);
  }
}

static void score_pads(void) {
  // Two-bar pedal fields expose extensions the piano and divisi strings only
  // touch in passing. Alternating low fifths and upper 7/9 tones keep the pad
  // harmonic, not merely atmospheric.
  for (int b = 0; b < 68; b += 2) {
    int h = b & 7;
    double at = bar_time(b, 0) + .035;
    double gain = b < 8 ? .105 : b < 24 ? .125 : b < 40 ? .145
                : b < 48 ? .175 : b < 64 ? .155 : .10;
    add_event(V_PAD, at, 2 * BAR - .09, harmony[h][0] - 12, gain * .72, 0, 40, 0, -1);
    add_event(V_PAD, at + .08, 2 * BAR - .17, harmony[h][1], gain * .80, 0, 41, 0, -1);
    add_event(V_PAD, at + .16, 2 * BAR - .25, harmony[h][4] + 12, gain, 0, 42, 1, -1);
    add_event(V_PAD, at + .24, 2 * BAR - .33, harmony[h][5] + 12, gain * .86, 0, 43, 2, -1);
  }
}

static void score_acid_and_clock(void) {
  static const int acid[16] = {45,-1,45,57,-1,45,-1,52,-1,45,60,-1,-1,45,55,57};
  static const int turn[16] = {45,48,50,52,55,57,60,64,69,67,64,60,57,55,52,47};
  for (int b = 16; b < 64; b++) {
    if (b >= 40 && b < 48) continue;
    for (int s = 0; s < 16; s++) {
      int note = ((b & 3) == 3 ? turn[s] : acid[(s + b) & 15]);
      if (note < 0 || ((s * 17 + b * 11) % 23) > (b >= 48 ? 19 : 17)) continue;
      add_event(V_ACID, bar_time(b, s / 4.0), .21 * BEAT, note, b >= 48 ? .26 : .23,
                0, 10, s == 0 || s == 10, -1);
      if (b >= 48 && (s % 5 == 2))
        add_event(V_ACID_ECHO, bar_time(b, s / 4.0) + .375 * BEAT, .18 * BEAT,
                  note + 12, .12, 0, 11, 0, -1);
    }
  }

  // Clock hands form a polymetric layer: seconds in 7, minutes in 5, hour
  // bells on harmonic arrivals. Their phase cycles realign only at long spans.
  static const int second_notes[7] = {81,88,84,91,88,83,86};
  static const int minute_notes[5] = {69,72,76,74,71};
  for (int b = 8; b < 68; b++) {
    for (int s = 0; s < 8; s++) if ((b * 8 + s) % 7 != 6)
      add_event(V_CLOCK, bar_time(b, s * .5), .12, second_notes[(b * 8 + s) % 7],
                b < 16 ? .09 : .13, 0, 12, 0, -1);
    if (b >= 16) for (int s = 0; s < 5; s++)
      add_event(V_CLOCK, bar_time(b, s * .8 + .04), .24, minute_notes[(b * 5 + s) % 5],
                b >= 48 ? .125 : .10, 0, 13, 1, -1);
    if ((b & 1) == 0)
      add_event(V_CLOCK, bar_time(b, 0), 1.1, harmony[b & 7][2], .105,
                0, 14, 2, -1);
  }
}

static int kick_active(int bar, int beat) {
  if (bar >= 40 && bar < 48) return 0;
  if (bar >= 68) return beat == 0;
  if (bar < 2) return 0;
  if (bar < 4) return beat == 0;
  if (bar < 6) return beat == 0 || beat == 2;
  if (bar == 6) return beat != 1;
  if (bar == 7) return beat != 3;
  if ((bar == 23 || bar == 39 || bar == 63) && beat == 3) return 0;
  return 1;
}

static void score_drums_and_hits(void) {
  for (int b = 0; b < BARS; b++) {
    double at = bar_time(b, 0);
    for (int beat = 0; beat < 4; beat++) if (kick_active(b, beat))
      add_event(V_KICK, at + beat * BEAT, .48, 36, b < 8 ? .50 + b * .045 : .88,
                0, 0, 0, -1);
    if (b >= 4 && !(b >= 40 && b < 44)) {
      for (int e = 0; e < 8; e++) {
        double swing = (e & 1) ? .075 * BEAT : 0;
        add_event(V_HAT_CLOSED, at + e * .5 * BEAT + swing, .065, 0,
                  b < 8 ? .20 : .31 + (e % 4 == 2 ? .055 : 0), 0,
                  20 + (e & 1), 0, -1);
      }
    }
    if (b >= 6 && !(b >= 40 && b < 44)) for (int beat = 1; beat < 4; beat += 2)
      add_event(V_CLAP, at + beat * BEAT + .009, .19, 0, .52, 0, 22, 0, -1);
    if (b >= 8 && b < 68 && !(b >= 40 && b < 48))
      add_event(V_HAT_OPEN, at + ((b & 3) == 2 ? 1.5 : 3.5) * BEAT, .21, 0,
                .29, 0, 23, 0, -1);
    if (b >= 48 && b < 64) for (int q = 0; q < 16; q++) if (q % 8 != 7)
      add_event(V_RIDE, at + q * .25 * BEAT + ((q & 1) ? .075 * BEAT : 0), .12,
                0, q % 4 == 2 ? .20 : .12, 0, 24, 0, -1);

    // Chord-following sub counter-rhythm: 3+3+2 instead of a copied four-hit loop.
    if (b >= 8 && b < 68 && !(b >= 40 && b < 48)) {
      static const double pulse[6] = {.5, 1.25, 2.0, 2.5, 3.25, 3.75};
      for (int k = 0; k < 6; k++) if (!((b + k) % 7 == 5))
        add_event(V_SUB, at + pulse[k] * BEAT, .31, roots[b & 7] - 12,
                  .39 + (k == 0 ? .06 : 0), 0, 0, 0, -1);
    }
  }
  static const int marks[] = {8,16,24,32,40,48,56,64};
  for (int i = 0; i < 8; i++) {
    int b = marks[i];
    add_event(V_CRASH, bar_time(b, 0), 1.15, 0, .40, 0, 25, 0, -1);
    add_event(V_ARCADE_HIT, bar_time(b, 0) + (i & 1 ? .08 : 0), .9, 0,
              i == 4 || i == 7 ? .48 : .37, 0, 26 + (i % 3), i % 3, i % 3);
  }
  for (int b = 14; b <= 62; b += 8)
    add_event(V_RISER, bar_time(b, 0), 2 * BAR, 0, .21, 0, 27, 0, -1);
}

static void score_breakdown_counterpoint(void) {
  // Four entries of one subject, displaced by 3/2 beats and alternately
  // inverted. This is a real canon inside the quiet span, not a texture label.
  static const int subject[8] = {57,60,64,67,64,62,60,55};
  for (int entry = 0; entry < 4; entry++) {
    double start = bar_time(40, 0) + entry * 1.5 * BEAT;
    for (int n = 0; n < 8; n++) {
      int d = subject[n] - 57;
      int note = 57 + (entry & 1 ? -d : d) + (entry >= 2 ? 12 : 0);
      add_event(entry & 1 ? V_STRING : V_PIANO, start + n * .75 * BEAT,
                .70 * BEAT, note, .18 - .018 * entry, 0, 30 + entry,
                entry & 1 ? 0 : 1, -1);
    }
  }
}

static void build_score(void) {
  memset(counts, 0, sizeof counts);
  event_count = 0;
  score_strings();
  score_piano();
  score_pads();
  score_acid_and_clock();
  score_drums_and_hits();
  score_breakdown_counterpoint();
}

static const char *find_piano_dir(void) {
  const char *candidates[] = {
    piano_dir, getenv("AC_PIANO_DIR"), "fedac/native/samples/piano",
    "../../../fedac/native/samples/piano", "../../fedac/native/samples/piano"
  };
  static char chosen[1024];
  for (size_t i = 0; i < sizeof candidates / sizeof *candidates; i++) {
    if (!candidates[i]) continue;
    char p[1200]; snprintf(p, sizeof p, "%s/60.raw", candidates[i]);
    FILE *f = fopen(p, "rb");
    if (f) { fclose(f); snprintf(chosen, sizeof chosen, "%s", candidates[i]); return chosen; }
  }
  return NULL;
}

static int load_piano(const char *dir) {
  piano_count = 0;
  for (int midi = 21; midi <= 96 && piano_count < PIANO_ANCHORS; midi += 3) {
    char path[1200]; snprintf(path, sizeof path, "%s/%d.raw", dir, midi);
    FILE *f = fopen(path, "rb"); if (!f) continue;
    fseek(f, 0, SEEK_END); long bytes = ftell(f); rewind(f);
    if (bytes <= 0 || bytes % 4) { fclose(f); continue; }
    float *data = malloc((size_t)bytes);
    if (!data || fread(data, 1, (size_t)bytes, f) != (size_t)bytes) {
      free(data); fclose(f); continue;
    }
    fclose(f);
    piano[piano_count++] = (PianoAnchor){data, bytes / 4, midi};
  }
  return piano_count;
}

static int nearest_piano(double midi) {
  int best = 0; double distance = 1e9;
  for (int i = 0; i < piano_count; i++) {
    double d = fabs(midi - piano[i].midi);
    if (d < distance) { best = i; distance = d; }
  }
  return best;
}

static uint32_t u32le(const uint8_t *p) {
  return (uint32_t)p[0] | (uint32_t)p[1] << 8 | (uint32_t)p[2] << 16 | (uint32_t)p[3] << 24;
}

static float *load_wav_mono(const char *path, long *out_n) {
  FILE *f = fopen(path, "rb"); if (!f) return NULL;
  fseek(f, 0, SEEK_END); long size = ftell(f); rewind(f);
  uint8_t *buf = malloc((size_t)size);
  if (!buf || fread(buf, 1, (size_t)size, f) != (size_t)size) { free(buf); fclose(f); return NULL; }
  fclose(f);
  if (size < 44 || memcmp(buf, "RIFF", 4) || memcmp(buf + 8, "WAVE", 4)) { free(buf); return NULL; }
  long p = 12, data_at = 0, data_len = 0;
  int format = 0, channels = 0, bits = 0; uint32_t source_sr = 0;
  while (p + 8 <= size) {
    uint32_t chunk = u32le(buf + p + 4);
    if (!memcmp(buf + p, "fmt ", 4) && p + 24 <= size) {
      format = buf[p+8] | buf[p+9] << 8;
      channels = buf[p+10] | buf[p+11] << 8;
      source_sr = u32le(buf + p + 12);
      bits = buf[p+22] | buf[p+23] << 8;
    } else if (!memcmp(buf + p, "data", 4)) { data_at = p + 8; data_len = chunk; }
    p += 8 + chunk + (chunk & 1);
  }
  if (!channels || !bits || !data_at || data_at + data_len > size) { free(buf); return NULL; }
  int bytes = bits / 8, stride = bytes * channels;
  long frames = data_len / stride;
  float *mono = calloc((size_t)frames, sizeof *mono);
  if (!mono) { free(buf); return NULL; }
  for (long i = 0; i < frames; i++) {
    double sum = 0;
    for (int c = 0; c < channels; c++) {
      const uint8_t *q = buf + data_at + i * stride + c * bytes;
      if (format == 3 && bits == 32) { float v; memcpy(&v, q, 4); sum += v; }
      else if (bits == 16) { int16_t v; memcpy(&v, q, 2); sum += v / 32768.0; }
      else if (bits == 24) { int32_t v = q[0] | q[1] << 8 | (int32_t)(int8_t)q[2] << 16; sum += v / 8388608.0; }
      else if (bits == 32) { int32_t v; memcpy(&v, q, 4); sum += v / 2147483648.0; }
    }
    mono[i] = (float)(sum / channels);
  }
  free(buf);
  if (source_sr && source_sr != SR) {
    long target_n = (long)llround((double)frames * SR / source_sr);
    float *target = calloc((size_t)target_n, sizeof *target);
    if (!target) { free(mono); return NULL; }
    for (long i = 0; i < target_n; i++) {
      double x = (double)i * source_sr / SR; long a = (long)x; double u = x - a;
      float va = mono[a < frames ? a : frames - 1];
      float vb = mono[a + 1 < frames ? a + 1 : frames - 1];
      target[i] = (float)(va + (vb - va) * u);
    }
    free(mono); mono = target; frames = target_n;
  }
  *out_n = frames;
  return mono;
}

static void load_sample_slots(void) {
  static const char *filenames[SAMPLE_SLOTS] = {"accent-1.wav", "accent-2.wav", "accent-3.wav"};
  for (int i = 0; i < SAMPLE_SLOTS; i++) {
    if (!sample_dir) continue;
    snprintf(samples[i].path, sizeof samples[i].path, "%s/%s", sample_dir, filenames[i]);
    samples[i].data = load_wav_mono(samples[i].path, &samples[i].n);
    samples[i].loaded = samples[i].data != NULL;
    fprintf(stderr, "# arcade slot %d · %s · %s\n", i + 1, filenames[i],
            samples[i].loaded ? "loaded" : "synth fallback");
  }
}

// The listener is fixed at the origin with one modeled head and two ears.
// Source bodies move; the master bus never fake-spins. Exact-turn bursts return
// to their incoming orientation, while the platter wobble has zero displacement
// and velocity at both ends.
static double motion_window(double t, double at, double dur, double turns) {
  if (t < at || t > at + dur) return 0;
  double raw = (t - at) / dur, w = sin(M_PI * raw); w *= w;
  return TAU * turns * ease(raw) + .42 * w * (.78 * sin(TAU * 1.37 * raw) + .22 * sin(TAU * .61 * raw + .4));
}

static double world_wobble(double t) {
  const double starts[] = {bar_time(18, .37), bar_time(30, .83), bar_time(40, .22), bar_time(58, .41)};
  static const double durations[] = {8.4, 7.1, 12.2, 9.6};
  static const double amps[] = {.24, .38, .52, .34};
  for (int k = 0; k < 4; k++) if (t >= starts[k] && t <= starts[k] + durations[k]) {
    double u = (t - starts[k]) / durations[k], w = sin(M_PI * u); w *= w;
    return amps[k] * w * (.81 * sin(TAU * (1.21 + .17 * k) * u) + .19 * sin(TAU * .47 * u + k));
  }
  return 0;
}

static void source_pose(const Event *e, double t, double *az, double *el, double *distance) {
  double phase = fmod(e->lane * 2.399963229728653 + e->seed * 1.0e-7, TAU);
  double rate = e->voice == V_CLOCK ? (e->lane == 12 ? 1.0 / (2 * BAR) : 1.0 / (8 * BAR))
              : e->voice == V_ACID || e->voice == V_ACID_ECHO ? 1.0 / (6 * BAR)
              : e->voice == V_PIANO ? -1.0 / (12 * BAR)
              : e->voice == V_PAD ? -1.0 / (26 * BAR)
              : e->voice == V_STRING || e->voice == V_PIZZ ? 1.0 / (18 * BAR)
              : 1.0 / (10 * BAR);
  double spin = motion_window(t, bar_time(24, 0), 8 * BAR, 2)
              + motion_window(t, bar_time(48, 0), 8 * BAR, -3)
              + motion_window(t, bar_time(62, 0), 5 * BAR, 1);
  *az = phase + TAU * rate * t + spin + world_wobble(t);
  double base = e->voice == V_CLOCK ? 2.1 : e->voice == V_PIANO ? 2.7 : e->voice == V_PAD ? 4.4
              : e->voice == V_STRING ? 3.5 : e->voice == V_ACID_ECHO ? 4.2 : 3.0;
  *distance = base * (1 + .18 * sin(*az * .73 + e->lane * .41)) + .32 * sin(TAU * t / (7.7 + e->lane * .13));
  *distance = clamp(*distance, .65, 6.5);
  *el = .34 * sin(*az * .57 + e->lane * .62) + (e->voice == V_CLOCK ? .24 : 0);
}

typedef struct { ACHrtf ear; double az, el, distance; } SpatialState;

static int centered_voice(Voice v) { return v == V_KICK || v == V_SUB; }
static int drum_voice(Voice v) {
  return v <= V_RISER || v == V_ARCADE_HIT;
}

static void emit(const Event *e, SpatialState *sp, long local_i, double mono) {
  long at = (long)llround(e->at * SR) + local_i;
  if (at < 0 || at >= N) return;
  float *L = drum_voice(e->voice) ? drumsL : musicL;
  float *R = drum_voice(e->voice) ? drumsR : musicR;
  if (centered_voice(e->voice)) {
    L[at] += (float)(mono * .70710678); R[at] += (float)(mono * .70710678); return;
  }
  if ((local_i & 31) == 0) source_pose(e, (double)at / SR, &sp->az, &sp->el, &sp->distance);
  float l, r;
  ac_hrtf_process(&sp->ear, (float)mono, sp->az, sp->el, sp->distance, &l, &r);
  // A small directionless direct component keeps mono fold robust; the audible
  // motion remains dominated by the binaural ear return.
  double mid = .5 * (l + r), side = .5 * (l - r) * 2.40;
  double dry = mono * .08;
  L[at] += (float)(mid * .96 + side + dry);
  R[at] += (float)(mid * .96 - side + dry);
}

static void render_piano(const Event *e) {
  if (!piano_count) return;
  int a = nearest_piano(e->midi);
  PianoAnchor *p = &piano[a];
  double step = pow(2.0, (e->midi - p->midi) / 12.0);
  long held = (long)(e->dur * SR), release = (long)((.98 + .28 * (e->articulation == 1)) * SR);
  long max_n = held + release;
  SpatialState sp = {0};
  double pos = 0, previous = 0, keyboard = clamp((e->midi - 60) / 42.0, -1, 1);
  for (long i = 0; i < max_n; i++, pos += step) {
    long q = (long)pos; if (q + 1 >= p->n) break;
    double u = pos - q, s = p->data[q] + (p->data[q + 1] - p->data[q]) * u;
    double edge = s - previous; previous = s;
    double env = i < .002 * SR ? i / (.002 * SR) : 1;
    if (i > held) env *= 1 - (double)(i - held) / release;
    double hammer = edge * harshness * 5.2 * exp(-(double)i / (SR * .047));
    double wood = tanh(s * (1 + 1.8 * harshness)) / (1 + .38 * harshness);
    emit(e, &sp, i, (wood + hammer) * env * e->gain * (1.12 + .08 * keyboard));
  }
}

static void render_string(const Event *e) {
  long n = (long)((e->dur + (e->voice == V_PIZZ ? .18 : .26)) * SR);
  double f = midi_hz(e->midi), phases[5];
  uint32_t rs = e->seed;
  for (int k = 0; k < 5; k++) phases[k] = frand() * TAU;
  SpatialState sp = {0};
  double bow = 0, body = 0;
  for (long i = 0; i < n; i++) {
    double t = (double)i / SR, p = t / (e->dur + .001);
    double attack = e->voice == V_PIZZ ? .003 : (e->articulation == 2 ? .025 : .085 + .018 * (e->lane & 3));
    double env;
    if (e->voice == V_PIZZ) env = fmin(1, t / attack) * exp(-t / .19);
    else {
      env = fmin(1, t / attack);
      if (t > e->dur - .15) env *= clamp((e->dur - t + .26) / .41, 0, 1);
    }
    double vib = e->voice == V_PIZZ ? 1 : 1 + .0027 * sin(TAU * (5.0 + .13 * (e->lane & 3)) * t + e->lane);
    double raw = 0;
    static const double detune[5] = {-.0042,-.0013,0,.0017,.0048};
    for (int k = 0; k < 5; k++) {
      phases[k] += TAU * f * vib * (1 + detune[k]) / SR;
      // Six harmonic bow spectrum, softened with 1/h^1.35 weighting.
      for (int h = 1; h <= 6 && f * h < SR * .45; h++)
        raw += sin(phases[k] * h + .07 * h * h) / pow(h, 1.35);
    }
    raw /= 5.8;
    double scrape = noise01(&rs) * (.035 + .13 * harshness + (.055 + .08 * harshness) * exp(-t / .028));
    double cutoff = e->voice == V_PIZZ ? .15 + .06 * harshness : .045 + .018 * env + .025 * harshness;
    bow += cutoff * (raw + scrape - bow);
    body += .028 * (bow - body);
    double bite = body * (1.18 - .15 * harshness) + bow * (.24 + .52 * harshness) + scrape * .16 * harshness;
    emit(e, &sp, i, tanh(bite * (1.3 + 2.1 * harshness)) * env * e->gain);
    (void)p;
  }
}

static void render_acid(const Event *e) {
  long n = (long)((e->dur + .035) * SR);
  double f = midi_hz(e->midi), phase = 0, lp = 0, bp = 0;
  SpatialState sp = {0};
  for (long i = 0; i < n; i++) {
    double t = (double)i / SR, p = (double)i / n;
    phase += f / SR; phase -= floor(phase);
    double saw = 2 * phase - 1, square = phase < .5 ? 1 : -1;
    saw = saw * (1 - .34 * harshness) + square * .34 * harshness;
    double cutoff = 340 + (e->articulation ? 5200 + 2600 * harshness : 3400 + 1900 * harshness) * exp(-(4.2 - .7 * harshness) * p);
    double k = 2 * sin(M_PI * fmin(cutoff, 15000) / SR);
    double high = saw - lp - (.34 - .13 * harshness) * bp; bp += k * high; lp += k * bp;
    double env = fmin(1, t / .004) * exp(-4.8 * p) * clamp((e->dur - t + .035) / .035, 0, 1);
    double steps = 64 - 28 * harshness;
    double crushed = round(bp * steps) / steps;
    double acid = bp * (1 - .28 * harshness) + crushed * .28 * harshness;
    emit(e, &sp, i, tanh(acid * (2.25 + 4.2 * harshness)) * env * e->gain);
  }
}

static void render_clock(const Event *e) {
  long n = (long)((e->dur + (e->articulation == 2 ? 1.3 : .38)) * SR);
  double f = midi_hz(e->midi), p1 = 0, p2 = 0, p3 = 0;
  SpatialState sp = {0};
  for (long i = 0; i < n; i++) {
    double t = (double)i / SR;
    p1 += TAU * f / SR; p2 += TAU * f * 2.006 / SR; p3 += TAU * f * 3.993 / SR;
    double decay = e->articulation == 2 ? .82 : e->articulation == 1 ? .28 : .095;
    double env = fmin(1, t / .0025) * exp(-t / decay);
    double bell = sin(p1 + (.13 + .10 * harshness) * sin(TAU * f * 1.414 * t) * exp(-t / .08))
                + (.43 + .17 * harshness) * sin(p2) * exp(-t / .19)
                + (.20 + .20 * harshness) * sin(p3) * exp(-t / .12)
                + .15 * harshness * sin(p1 * 6.71) * exp(-t / .065);
    emit(e, &sp, i, tanh(bell * (.82 + 1.25 * harshness)) * env * e->gain);
  }
}

static void render_pad(const Event *e) {
  long n = (long)((e->dur + .72) * SR);
  double f = midi_hz(e->midi), phases[4] = {0, .71, 1.93, 3.14};
  static const double detune[4] = {-.0038, -.0011, .0014, .0041};
  SpatialState sp = {0};
  double warm = 0;
  for (long i = 0; i < n; i++) {
    double t = (double)i / SR;
    double env = smooth(t / (.36 + .07 * e->articulation));
    if (t > e->dur - .18) env *= smooth((e->dur + .72 - t) / .90);
    double drift = 1 + .0016 * sin(TAU * (.19 + .017 * e->lane) * t + e->lane);
    double raw = 0;
    for (int k = 0; k < 4; k++) {
      phases[k] += TAU * f * drift * (1 + detune[k]) / SR;
      raw += sin(phases[k]) + .34 * sin(2 * phases[k] + .3) + .12 * sin(3 * phases[k] + 1.1);
    }
    raw *= .22;
    double cutoff = .012 + .014 * env + .008 * harshness;
    warm += cutoff * (raw - warm);
    double edge = (raw - warm) * (.10 + .17 * harshness);
    emit(e, &sp, i, tanh((warm + edge) * (1.15 + .75 * harshness)) * env * e->gain);
  }
}

static void render_sub(const Event *e) {
  long n = (long)(e->dur * SR); double phase = 0, f = midi_hz(e->midi);
  SpatialState sp = {0};
  for (long i = 0; i < n; i++) {
    double p = (double)i / n, env = fmin(1, i / (.008 * SR)) * pow(sin(M_PI * p), .42);
    phase += TAU * f / SR;
    emit(e, &sp, i, sin(phase) * env * e->gain);
  }
}

static void render_kick(const Event *e) {
  long n = (long)(e->dur * SR); double phase = 0; uint32_t rs = e->seed;
  SpatialState sp = {0};
  for (long i = 0; i < n; i++) {
    double t = (double)i / SR, f = 39 + 112 * exp(-t / .024);
    phase += TAU * f / SR;
    double body = sin(phase) * exp(-t / .17);
    double click = noise01(&rs) * exp(-t / .0048);
    emit(e, &sp, i, tanh(body * (1.85 + .9 * harshness) + click * (.15 + .24 * harshness)) * e->gain * .74);
  }
}

static void render_noise_drum(const Event *e) {
  long n = (long)(e->dur * SR); uint32_t rs = e->seed;
  double lp = 0, last = 0; SpatialState sp = {0};
  for (long i = 0; i < n; i++) {
    double t = (double)i / SR, white = noise01(&rs); lp += .075 * (white - lp);
    double v = white - lp, env = exp(-t / .035);
    if (e->voice == V_CLAP) {
      double burst = exp(-t / .025);
      if (t >= .027) burst += .8 * exp(-(t - .027) / .023);
      if (t >= .051) burst += .6 * exp(-(t - .051) / .040);
      v = white - .58 * last; env = burst;
    } else if (e->voice == V_CRASH) env = exp(-t / .31);
    else if (e->voice == V_HAT_OPEN) env = exp(-t / .085);
    else if (e->voice == V_RIDE) env = exp(-t / .050);
    last = white;
    if (harshness > 0) {
      double steps = 64 - 36 * harshness;
      v = v * (1 - .42 * harshness) + round(v * steps) / steps * .42 * harshness;
    }
    emit(e, &sp, i, v * env * e->gain * (e->voice == V_CLAP ? .40 : .26));
  }
}

static void render_riser(const Event *e) {
  long n = (long)(e->dur * SR); uint32_t rs = e->seed;
  double lp = 0; SpatialState sp = {0};
  for (long i = 0; i < n; i++) {
    double p = (double)i / n, white = noise01(&rs), a = .006 + p * .38;
    lp += a * (white - lp);
    emit(e, &sp, i, (white - .45 * lp) * p * p * e->gain * .28);
  }
}

static void render_arcade_fallback(const Event *e) {
  long n = (long)(e->dur * SR); uint32_t rs = e->seed;
  double p1 = 0, p2 = 0, lp = 0; SpatialState sp = {0};
  for (long i = 0; i < n; i++) {
    double t = (double)i / SR, u = (double)i / n;
    double f1 = 150 * exp(-t * 5.2) + 47, f2 = 1320 * exp(-t * 8.4) + 160;
    p1 += TAU * f1 / SR; p2 += TAU * f2 / SR;
    double white = noise01(&rs); lp += .12 * (white - lp);
    double impact = sin(p1) * exp(-t * 6.8) + .42 * sin(p2) * exp(-t * 12)
                  + .21 * (white - lp) * exp(-t * 19);
    double gate = fmod(t * (31 + e->sample_slot * 7), 1) < .58 ? 1 : .35;
    emit(e, &sp, i, tanh(impact * (2.1 + 3.2 * harshness)) * gate * (1 - smooth(u)) * e->gain);
  }
}

static void render_arcade_sample(const Event *e) {
  if (e->sample_slot < 0 || e->sample_slot >= SAMPLE_SLOTS || !samples[e->sample_slot].loaded) {
    render_arcade_fallback(e); return;
  }
  SampleSlot *slot = &samples[e->sample_slot];
  long n = slot->n < (long)(e->dur * SR) ? slot->n : (long)(e->dur * SR);
  SpatialState sp = {0};
  for (long i = 0; i < n; i++) {
    double edge = i < .002 * SR ? i / (.002 * SR) : 1;
    long remain = n - i; if (remain < .012 * SR) edge *= remain / (.012 * SR);
    emit(e, &sp, i, slot->data[i] * edge * e->gain);
  }
}

static void render_event(const Event *e) {
  switch (e->voice) {
    case V_PIANO: render_piano(e); break;
    case V_STRING: case V_PIZZ: render_string(e); break;
    case V_ACID: case V_ACID_ECHO: render_acid(e); break;
    case V_CLOCK: render_clock(e); break;
    case V_PAD: render_pad(e); break;
    case V_SUB: render_sub(e); break;
    case V_KICK: render_kick(e); break;
    case V_RISER: render_riser(e); break;
    case V_ARCADE_HIT: render_arcade_sample(e); break;
    default: render_noise_drum(e); break;
  }
}

static double kick_duck(double t) {
  int bar = (int)floor(t / BAR);
  if (bar < 0 || bar >= BARS || (bar >= 40 && bar < 48)) return 1;
  double in_beat = fmod(t, BEAT);
  if (in_beat < 0 || in_beat > .22) return 1;
  int beat = (int)floor(fmod(t, BAR) / BEAT + 1e-7);
  if (!kick_active(bar, beat)) return 1;
  return .40 + .60 * pow(in_beat / .22, 1.75);
}

static void room_and_master(void) {
  // Prime-ish early reflections preserve attacks while binding piano, strings,
  // acid, and the moving ear field into one room.
  static const double delay_sec[] = {.031, .047, .071, .103};
  static const double gain[] = {.105, .078, .052, .031};
  for (int tap = 0; tap < 4; tap++) {
    long d = (long)llround(delay_sec[tap] * SR);
    for (long i = d; i < N; i++) {
      float l = musicR[i - d] * gain[tap] * (1 - .22 * harshness);
      float r = musicL[i - d] * gain[tap] * (1 - .22 * harshness);
      musicL[i] += l; musicR[i] += r;
    }
  }
  double peak = 0;
  for (long i = 0; i < N; i++) {
    double t = (double)i / SR, duck = kick_duck(t);
    double fade_in = fmin(1, i / (.010 * SR));
    double fade_out = fmin(1, (N - 1 - i) / (1.10 * SR));
    double fade = fmax(0, fmin(fade_in, fade_out));
    double drive = .79 + .46 * harshness;
    double l = tanh((musicL[i] * duck + drumsL[i]) * drive) * fade;
    double r = tanh((musicR[i] * duck + drumsR[i]) * drive) * fade;
    musicL[i] = (float)l; musicR[i] = (float)r;
    if (fabs(l) > peak) peak = fabs(l); if (fabs(r) > peak) peak = fabs(r);
  }
  fprintf(stderr, "# pre-master peak %.6f\n", peak);
}

static int write_wav(const char *path) {
  FILE *f = fopen(path, "wb"); if (!f) return 0;
  uint32_t data_size = (uint32_t)(N * 2 * sizeof(float)), riff = 36 + data_size;
  uint32_t fmt_size = 16, rate = SR, bytes_sec = SR * 8;
  uint16_t format = 3, channels = 2, block = 8, bits = 32;
  fwrite("RIFF", 1, 4, f); fwrite(&riff, 4, 1, f); fwrite("WAVE", 1, 4, f);
  fwrite("fmt ", 1, 4, f); fwrite(&fmt_size, 4, 1, f); fwrite(&format, 2, 1, f);
  fwrite(&channels, 2, 1, f); fwrite(&rate, 4, 1, f); fwrite(&bytes_sec, 4, 1, f);
  fwrite(&block, 2, 1, f); fwrite(&bits, 2, 1, f);
  fwrite("data", 1, 4, f); fwrite(&data_size, 4, 1, f);
  for (long i = 0; i < N; i++) { fwrite(&musicL[i], 4, 1, f); fwrite(&musicR[i], 4, 1, f); }
  int ok = !ferror(f); fclose(f); return ok;
}

static int write_receipt(const char *path) {
  FILE *f = fopen(path, "w"); if (!f) return 0;
  fprintf(f, "{\n");
  fprintf(f, "  \"schema\": \"aesthetic.computer/pop-events/v1\",\n");
  fprintf(f, "  \"track\": \"taksmukkeklokken-c\",\n");
  fprintf(f, "  \"renderer\": \"pop/teknull/c/taksmukkeklokken.c\",\n");
  fprintf(f, "  \"deterministic\": true,\n");
  fprintf(f, "  \"harshness\": %.3f,\n", harshness);
  fprintf(f, "  \"transport\": {\"bpm\": 140, \"meter\": \"4/4\", \"key\": \"A minor\", \"bars\": 70, \"durationSec\": 120, \"sampleRate\": 48000},\n");
  fprintf(f, "  \"listener\": {\"count\": 1, \"ears\": 2, \"model\": \"procedural ITD + far-ear head shadow + elevation pinna combs\", \"maximumItdMicroseconds\": 650},\n");
  fprintf(f, "  \"motion\": {\"sourceBased\": true, \"rotationBursts\": [{\"bars\":[24,32],\"turns\":2},{\"bars\":[48,56],\"turns\":-3},{\"bars\":[62,67],\"turns\":1}], \"elasticWobbleWindows\": 4},\n");
  fprintf(f, "  \"orchestration\": {\"grandPiano\": \"Salamander Grand Piano V3 CC0 bank with extended pedal releases\", \"strings\": \"five-player bowed/pizzicato physical-style sections\", \"pads\": \"four-voice two-bar harmonic fields carrying low fifths and upper 7/9 tones\", \"counterpoint\": \"four-entry breakdown canon plus contrary-motion drop-B violin line\", \"harmonicCycle\": [\"Am(add9)\",\"Fmaj7/A\",\"Cmaj9/G\",\"G6(add9)\",\"Dm9/F\",\"Am/E\",\"Bdim7/F\",\"E7(b9)\"]},\n");
  fprintf(f, "  \"arcadePercussion\": {\"policy\": \"only user-supplied release-cleared WAVs are loaded; missing slots use original synthesized impacts\", \"sampleDirectory\": %s, \"slots\": [\n",
          sample_dir ? "\"provided\"" : "null");
  for (int i = 0; i < SAMPLE_SLOTS; i++)
    fprintf(f, "    {\"name\":\"accent-%d.wav\",\"loaded\":%s}%s\n", i + 1,
            samples[i].loaded ? "true" : "false", i == SAMPLE_SLOTS - 1 ? "" : ",");
  fprintf(f, "  ]},\n");
  fprintf(f, "  \"eventCount\": %d,\n  \"eventCountsByVoice\": {\n", event_count);
  for (int v = 0; v <= V_ARCADE_HIT; v++)
    fprintf(f, "    \"%s\": %d%s\n", voice_name((Voice)v), counts[v], v == V_ARCADE_HIT ? "" : ",");
  fprintf(f, "  },\n  \"events\": [\n");
  for (int i = 0; i < event_count; i++) {
    Event *e = &events[i];
    fprintf(f, "    {\"at\":%.6f,\"dur\":%.6f,\"voice\":\"%s\",\"section\":\"%s\",\"midi\":%.3f,\"gain\":%.4f,\"lane\":%d,\"articulation\":%d,\"sampleSlot\":%d}%s\n",
            e->at, e->dur, voice_name(e->voice), section_name(e->at), e->midi,
            e->gain, e->lane, e->articulation, e->sample_slot,
            i == event_count - 1 ? "" : ",");
  }
  fprintf(f, "  ]\n}\n");
  int ok = !ferror(f); fclose(f); return ok;
}

static void usage(const char *argv0) {
  fprintf(stderr, "usage: %s [--out raw.wav] [--receipt events.json] [--piano-dir dir] [--arcade-samples dir] [--harsh 0..1] [--score-only]\n", argv0);
}

int main(int argc, char **argv) {
  for (int i = 1; i < argc; i++) {
    if (!strcmp(argv[i], "--out") && i + 1 < argc) out_path = argv[++i];
    else if (!strcmp(argv[i], "--receipt") && i + 1 < argc) receipt_path = argv[++i];
    else if (!strcmp(argv[i], "--piano-dir") && i + 1 < argc) piano_dir = argv[++i];
    else if (!strcmp(argv[i], "--arcade-samples") && i + 1 < argc) sample_dir = argv[++i];
    else if (!strcmp(argv[i], "--harsh") && i + 1 < argc) harshness = clamp(atof(argv[++i]), 0, 1);
    else if (!strcmp(argv[i], "--score-only")) score_only = 1;
    else if (!strcmp(argv[i], "--help")) { usage(argv[0]); return 0; }
    else { usage(argv[0]); return 2; }
  }
  build_score();
  load_sample_slots();
  if (!write_receipt(receipt_path)) { fprintf(stderr, "cannot write %s: %s\n", receipt_path, strerror(errno)); return 1; }
  fprintf(stderr, "# score · %d events · 70 bars · 120.000 s\n", event_count);
  if (score_only) return 0;
  const char *found = find_piano_dir();
  if (!found || !load_piano(found)) {
    fprintf(stderr, "no Salamander piano bank; use --piano-dir (expected 21.raw..96.raw)\n"); return 1;
  }
  fprintf(stderr, "# grand piano · %d CC0 anchors · %s\n", piano_count, found);
  N = (long)llround(DURATION * SR);
  musicL = calloc((size_t)N, sizeof *musicL); musicR = calloc((size_t)N, sizeof *musicR);
  drumsL = calloc((size_t)N, sizeof *drumsL); drumsR = calloc((size_t)N, sizeof *drumsR);
  if (!musicL || !musicR || !drumsL || !drumsR) { fprintf(stderr, "audio allocation failed\n"); return 1; }
  for (int i = 0; i < event_count; i++) {
    render_event(&events[i]);
    if ((i + 1) % 500 == 0) fprintf(stderr, "# render · %d/%d events\n", i + 1, event_count);
  }
  room_and_master();
  if (!write_wav(out_path)) { fprintf(stderr, "cannot write %s: %s\n", out_path, strerror(errno)); return 1; }
  fprintf(stderr, "# wrote %s · 48 kHz stereo float32 · 120.000 s\n", out_path);
  return 0;
}
