// taksmukkeklokken-smooch.c — the klokken re-cut as smooth NYC house (né fcukers).
//
// Direction (@jeffrey, 2026-08-13): "more like Fcukers · more natural / real
// instruments · more dancey sexy bass · a little less MIDI." So: 122 BPM
// pocket instead of 140, a plucked sine-bump bass with slides
// riding a sidechain pump, dusty Salamander piano stabs on the offbeats
// instead of sixteenth arps, swung + humanized drums with a live shaker, and
// the clock bells kept sparse as the hook. The acid line, risers, and the
// 7-against-5 clock grid are gone — that was the MIDI.
//
// The canonical orchestral cut lives untouched in taksmukkeklokken.c.
//
// Revision (@jeffrey, 2026-08-13): kill the clicks (smoothed sidechain attack,
// every voice now lands on a ten-millisecond tail fade), no more master tanh
// (clean peak-normalized sum, loudness left to the mastering chain), the lift
// re-voiced around harmonized pure-sine choir tones entering one by one, and
// the mix spatialized the Special Sign way: dry direct voices on stable pans
// plus a band-limited antisymmetric ear-field return. Mono stays intact.
//
// Build: cc -O3 -std=c11 -Wall -Wextra -o taksmukkeklokken-smooch taksmukkeklokken-smooch.c -lm

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
#define BPM 122.0
#define BEAT (60.0 / BPM)
#define BAR (4.0 * BEAT)
#define BARS 96
#define DURATION (BARS * BAR)
#define MAX_EVENTS 20000
#define PIANO_ANCHORS 32

typedef enum {
  V_KICK, V_HAT_CLOSED, V_HAT_OPEN, V_CLAP, V_RIDE, V_CRASH, V_SHAKER,
  V_BASS, V_STRING, V_PIZZ, V_PIANO, V_CLOCK, V_PAD, V_VOX
} Voice;

typedef struct {
  double at, dur, midi, gain, pan;
  Voice voice;
  int lane, articulation;
  uint32_t seed;
} Event;

typedef struct { float *data; long n; int midi; } PianoAnchor;

static Event events[MAX_EVENTS];
static int event_count;
static int counts[V_VOX + 1];
static uint32_t rng_state = 87109u;
static long N;
static float *musicL, *musicR, *drumsL, *drumsR, *sideB;
static PianoAnchor piano[PIANO_ANCHORS];
static int piano_count;
static const char *piano_dir;
static const char *out_path = "../out/taksmukkeklokken-smooch.raw.wav";
static const char *receipt_path = "../out/taksmukkeklokken-smooch.events.json";
static int score_only;

static double clamp(double x, double lo, double hi) { return x < lo ? lo : x > hi ? hi : x; }
static double midi_hz(double midi) { return 440.0 * pow(2.0, (midi - 69.0) / 12.0); }
static uint32_t xorshift(uint32_t *s) { *s ^= *s << 13; *s ^= *s >> 17; *s ^= *s << 5; return *s; }
static double noise01(uint32_t *s) { return ((double)xorshift(s) / 4294967296.0) * 2.0 - 1.0; }
static double frand(void) { return ((double)xorshift(&rng_state) / 4294967296.0); }
static double ease(double u) { u = clamp(u, 0, 1); return u * u * u * (10 + u * (-15 + 6 * u)); }
static double smooth(double u) { u = clamp(u, 0, 1); return u * u * (3 - 2 * u); }
static double bar_time(int bar, double beat) { return bar * BAR + beat * BEAT; }

// Every voice ends through this ten-millisecond raised-cosine ramp so no
// renderer can truncate mid-amplitude — the click amnesty.
static double tail_fade(long i, long n) {
  double u = (double)(n - 1 - i) / (.010 * SR);
  return u >= 1 ? 1 : u <= 0 ? 0 : u * u * (3 - 2 * u);
}

// Swung sixteenth grid: odd sixteenths sit late (house shuffle), the "and"
// eighths a touch less. Every scored hit also carries a few milliseconds of
// human scatter and velocity spread — the anti-MIDI layer.
#define SWING16 .085
#define SWING8 .045
static double sw(int s) {
  double pos = s * .25;
  if (s & 1) pos += SWING16;
  else if (s % 4 == 2) pos += SWING8;
  return pos;
}

static const char *voice_name(Voice v) {
  static const char *names[] = {
    "kick", "closed-hat", "open-hat", "clap", "ride", "crash", "shaker",
    "bass", "strings", "pizzicato", "grand-piano", "clock-metal", "harmonic-pad", "jeffrey-vox"
  };
  return names[v];
}

static const char *section_name(double at) {
  int b = (int)floor(at / BAR + 1e-8);
  if (b < 8) return "intro";
  if (b < 16) return "lift";
  if (b < 32) return "groove-a";
  if (b < 36) return "bass-feature";
  if (b < 40) return "rebuild";
  if (b < 64) return "groove-b";
  if (b < 72) return "breakdown";
  if (b < 88) return "groove-c";
  return "outro";
}

static void add_event(Voice voice, double at, double dur, double midi, double gain,
                      double pan, int lane, int articulation) {
  if (event_count >= MAX_EVENTS) { fprintf(stderr, "event capacity exceeded\n"); exit(1); }
  if (at < 0 || at >= DURATION || dur <= 0) return;
  if (at + dur > DURATION) dur = DURATION - at;
  Event *e = &events[event_count++];
  *e = (Event){ at, dur, midi, gain, pan, voice, lane, articulation,
                xorshift(&rng_state) | 1u };
  counts[voice]++;
}

// Humanized add: milliseconds of scatter, velocity spread.
static void hadd(Voice voice, double at, double dur, double midi, double gain,
                 int lane, int articulation) {
  add_event(voice, at + (frand() - .5) * .013, dur, midi,
            gain * (.86 + .28 * frand()), 0, lane, articulation);
}

// Same eight-bar harmonic sentence as the canonical cut — the klokken identity.
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

static int bass_on(int b) {
  return (b >= 12 && b < 40) || (b >= 40 && b < 64) || (b >= 72 && b < 92);
}

static void score_bass(void) {
  // Two alternating syncopated shapes over the chord roots: held root on the
  // one, offbeat pickups, an octave pop, a b7 walk-down, and a slid return.
  // articulation = semitones the pluck slides up from (fingered slide).
  typedef struct { int step, offset, slide; double dur, accent; } Hit;
  static const Hit shapeA[] = {
    {0, 0, 0, .60, 1.12}, {3, 0, 0, .22, .78}, {6, 12, 0, .18, 1.00},
    {8, 0, 0, .48, .88}, {11, -2, 0, .22, .74}, {14, 0, 2, .34, .96},
  };
  static const Hit shapeB[] = {
    {0, 0, 0, .60, 1.12}, {3, 3, 0, .22, .80}, {6, 0, 0, .34, .86},
    {9, 7, 0, .18, 1.02}, {11, 5, 0, .22, .76}, {14, 0, -3, .38, .94},
  };
  for (int b = 0; b < BARS; b++) {
    if (!bass_on(b)) continue;
    int h = b & 7;
    int root = roots[h] - 12;
    const Hit *shape = (b & 1) ? shapeB : shapeA;
    int nhits = 6;
    double at = bar_time(b, 0);
    double g = b < 16 ? .30 : b < 36 ? .40 : b < 40 ? .34 : b < 64 ? .42 : .44;
    if (b >= 32 && b < 36) g = .48; // bass feature carries the room
    for (int k = 0; k < nhits; k++) {
      const Hit *hit = &shape[k];
      // Drop a hit now and then so the line breathes instead of looping.
      if (((b * 5 + k * 3) % 17) == 12 && hit->step != 0) continue;
      int note = root + hit->offset;
      int lift = (b >= 80 && b < 88 && hit->offset == 12) ? 12 : 0;
      int slide = hit->slide;
      if (slide < 0) slide = -slide, note -= slide, note += slide; // magnitude only, from below
      hadd(V_BASS, at + sw(hit->step) * BEAT, hit->dur * BEAT, note + lift,
           g * hit->accent, 0, hit->slide > 0 ? hit->slide : (hit->slide < 0 ? -hit->slide : 0));
    }
    // Ghost pluck before the next downbeat, one bar in four.
    if ((b & 3) == 2)
      hadd(V_BASS, at + sw(15) * BEAT + .02, .16 * BEAT, root, g * .38, 0, 0);
  }
}

static void score_piano(void) {
  for (int b = 0; b < BARS; b++) {
    int h = b & 7;
    double at = bar_time(b, 0);
    if (b < 8) {
      // Dusty held chords, rolled by hand — the record starts on the piano.
      if ((b & 1) == 0) {
        for (int k = 1; k < 6; k++)
          hadd(V_PIANO, at + .030 * k + .02, 6.4 * BEAT, harmony[h][k],
               .21 + .012 * k, 3 + k, 0);
        hadd(V_PIANO, at + .012, 6.8 * BEAT, roots[h] - 12, .27, 0, 1);
      }
      continue;
    }
    if (b >= 64 && b < 72) {
      // Breakdown: gentle broken-chord figure, no stabs.
      static const int order[6] = {0, 2, 4, 5, 3, 1};
      for (int s = 0; s < 6; s++)
        hadd(V_PIANO, at + s * .62 * BEAT + .01, 1.1 * BEAT,
             harmony[h][order[s]] + 12, .155, 4 + s, 0);
      hadd(V_PIANO, at + .01, 3.6 * BEAT, roots[h] - 12, .22, 0, 1);
      continue;
    }
    if (b < 12 || b >= 92) {
      if ((b & 1) == 0)
        hadd(V_PIANO, at + .02, 3.6 * BEAT, roots[h], .18, 1, 0);
      continue;
    }
    // The house move: chord stabs on the and-of-two and and-of-four,
    // occasionally anticipating on the and-of-three. Short, dusty, mid-keyboard.
    double g = b < 16 ? .17 : b < 40 ? .21 : b < 64 ? .225 : .235;
    int stabs[2] = {6, 14};
    if ((b & 3) == 3) stabs[1] = 11;
    for (int q = 0; q < 2; q++) {
      double pos = sw(stabs[q]) * BEAT;
      for (int k = 2; k < 6; k++)
        hadd(V_PIANO, at + pos + .009 * (k - 2), .42 * BEAT, harmony[h][k],
             g * (.72 + .09 * k), 3 + k, 0);
    }
    // Rolled upper chord to close each 8-bar phrase — a hand, not a loop reset.
    if ((b & 7) == 7)
      for (int k = 1; k < 6; k++)
        hadd(V_PIANO, at + 3.42 * BEAT + .026 * k, 1.5 * BEAT,
             harmony[h][k] + 12, .16 + .014 * k, 5 + k, 1);
  }
}

static void score_bells(void) {
  // The klokken hook, kept sparse: a four-note answer every fourth bar in the
  // grooves, hour-bell arrivals at section marks, faint tick pairs elsewhere.
  static const int riff[4][4] = {
    {81, 79, 76, 74}, {79, 76, 74, 72}, {81, 84, 81, 79}, {76, 74, 72, 69},
  };
  for (int b = 8; b < BARS; b++) {
    double at = bar_time(b, 0);
    int in_groove = (b >= 16 && b < 32) || (b >= 40 && b < 64) || (b >= 72 && b < 88);
    if (in_groove && (b & 3) == 3) {
      const int *line = riff[(b >> 2) & 3];
      for (int q = 0; q < 4; q++)
        hadd(V_CLOCK, at + sw(q * 2 + 8) * BEAT, .30, line[q],
             .115 + (q == 0 ? .025 : 0), 12, 1);
    } else if (!in_groove && (b & 1) == 0 && b < 92) {
      // Faint and low-mixed — ticks color the lift, they don't pierce it.
      hadd(V_CLOCK, at + sw(4) * BEAT, .14, 88, .030, 12, 0);
      hadd(V_CLOCK, at + sw(12) * BEAT, .14, 84, .022, 12, 0);
    }
  }
  static const int marks[] = {8, 16, 32, 40, 64, 72, 88};
  for (int i = 0; i < 7; i++)
    add_event(V_CLOCK, bar_time(marks[i], 0) + .01, 1.4,
              harmony[marks[i] & 7][2], .12, 0, 14, 2);
}

static void score_pads_and_strings(void) {
  for (int b = 0; b < 92; b += 2) {
    int h = b & 7;
    double at = bar_time(b, 0) + .04;
    double g = b < 8 ? .07 : b < 64 ? .075 : b < 72 ? .15 : .09;
    add_event(V_PAD, at, 2 * BAR - .12, harmony[h][0] - 12, g * .75, 0, 40, 0);
    add_event(V_PAD, at + .09, 2 * BAR - .2, harmony[h][4], g, 0, 41, 1);
  }
  // The lift sings: harmonized pure-sine choir tones enter one at a time from
  // bar 12, then hold softly under every groove — the smooth way in.
  for (int b = 12; b < 92; b += 2) {
    if (b >= 64 && b < 72) continue; // the breakdown belongs to piano + strings
    int h = b & 7;
    double at = bar_time(b, 0);
    double g = b < 14 ? .052 : b < 16 ? .060 : b < 64 ? .066 : .056;
    int tones = b < 14 ? 2 : b < 16 ? 3 : 4;
    for (int k = 0; k < tones; k++)
      add_event(V_PAD, at + k * .40 * BEAT, 2 * BAR - .3 - k * .12,
                harmony[h][k + 1] + 12, g * (1 - .12 * k), 0, 50 + k, 2);
  }
  // Strings only where the record opens up: breakdown swells and a low
  // sustained counterline through the last groove.
  for (int b = 64; b < 88; b++) {
    int h = b & 7;
    double at = bar_time(b, 0);
    double g = b < 72 ? .16 : .09;
    add_event(V_STRING, at + .12, BAR - .1, harmony[h][1] - 12, g, 0, 1, 0);
    if (b < 72)
      add_event(V_STRING, at + .16, BAR - .12, harmony[h][3], g * .8, 0, 5, 0);
  }
  // Breakdown keeps a two-entry echo of the old canon — piano asks, strings answer.
  static const int subject[8] = {57, 60, 64, 67, 64, 62, 60, 55};
  for (int entry = 0; entry < 2; entry++) {
    double start = bar_time(66, 0) + entry * 1.5 * BEAT;
    for (int n = 0; n < 8; n++) {
      int d = subject[n] - 57;
      int note = 57 + (entry ? -d : d) + 12;
      hadd(entry ? V_STRING : V_PIANO, start + n * .75 * BEAT, .70 * BEAT,
           note, .15 - .02 * entry, 30 + entry, entry ? 0 : 1);
    }
  }
}

static int kick_active(int bar, int beat) {
  if (bar < 6) return 0;
  if (bar < 8) return beat == 0 || beat == 2;
  if (bar >= 32 && bar < 36) return 0;
  if (bar >= 36 && bar < 38) return beat == 0 || beat == 2;
  if (bar >= 64 && bar < 72) return 0;
  if (bar >= 92) return bar < 94 ? beat == 0 : (bar == 94 && beat == 0);
  if (bar >= 88 && bar < 92) return beat == 0 || beat == 2;
  if ((bar == 31 || bar == 63 || bar == 87) && beat == 3) return 0;
  return 1;
}

static void score_drums(void) {
  for (int b = 0; b < BARS; b++) {
    double at = bar_time(b, 0);
    int in_groove = (b >= 8 && b < 32) || (b >= 36 && b < 64) || (b >= 72 && b < 92);
    for (int beat = 0; beat < 4; beat++) if (kick_active(b, beat))
      add_event(V_KICK, at + beat * BEAT + (frand() - .5) * .004, .42, 36,
                b < 12 ? .58 + (b - 6) * .05 : .86, 0, 0, 0);
    // Shaker sixteenths run nearly the whole record — the live wrist.
    if (b >= 2 && b < 94 && !(b >= 64 && b < 66)) {
      for (int s = 0; s < 16; s++)
        hadd(V_SHAKER, at + sw(s) * BEAT, .09, 0,
             (s % 4 == 2 ? .105 : .062) * (b < 8 ? .7 : 1.0), 21, 0);
    }
    if (!in_groove) continue;
    // Closed hats: swung offbeat eighths with pushed velocity, classic pocket.
    for (int s = 2; s < 16; s += 4)
      hadd(V_HAT_CLOSED, at + sw(s) * BEAT, .07, 0, .27 + (s == 10 ? .05 : 0), 20, 0);
    // Extra sixteenth chatter one bar in four.
    if ((b & 3) == 1) for (int s = 5; s < 16; s += 8)
      hadd(V_HAT_CLOSED, at + sw(s) * BEAT, .05, 0, .13, 20, 0);
    // Open hat rides the and-of-four; and-of-two too once the record peaks.
    hadd(V_HAT_OPEN, at + sw(14) * BEAT, .24, 0, .21, 23, 0);
    if (b >= 72) hadd(V_HAT_OPEN, at + sw(6) * BEAT, .20, 0, .16, 23, 0);
    // Clap layered loose on 2 and 4 — two hands, not one machine.
    for (int beat = 1; beat < 4; beat += 2) {
      hadd(V_CLAP, at + beat * BEAT + .006, .18, 0, .46, 22, 0);
      hadd(V_CLAP, at + beat * BEAT + .021, .15, 0, .20, 22, 0);
    }
    if ((b & 7) == 7)
      hadd(V_CLAP, at + sw(14) * BEAT, .15, 0, .26, 22, 0);
    // Ride eighths through the final groove.
    if (b >= 72 && b < 88) for (int s = 0; s < 16; s += 2)
      hadd(V_RIDE, at + sw(s) * BEAT, .13, 0, s % 8 == 4 ? .16 : .10, 24, 0);
  }
  static const int marks[] = {16, 40, 72};
  for (int i = 0; i < 3; i++)
    add_event(V_CRASH, bar_time(marks[i], 0), 1.0, 0, .30, 0, 25, 0);
}

static void score_vox(void);

static void build_score(void) {
  memset(counts, 0, sizeof counts);
  event_count = 0;
  score_bass();
  score_piano();
  score_bells();
  score_pads_and_strings();
  score_drums();
  score_vox();
}

// ── jeffrey vox one-shots ──────────────────────────────────────────────────
// Real @jeffrey takes from the hellsine bank, dropped in like a house record
// would: a label tag, a breakdown confession, and a clipped sung "money" on
// the turnarounds. Event articulation picks the sample; midi is repurposed
// as the chop's start offset in seconds.

enum { VOX_TAG, VOX_INEEDYOU, VOX_MONEY, VOX_PRUTTI, VOX_COUNT };
typedef struct { float *data; long n; } VoxSample;
static VoxSample vox_samples[VOX_COUNT];
static const char *vox_files[VOX_COUNT] = {
  "pop/hellsine/samples/aesthetic-dot-computer.wav",
  "pop/hellsine/samples/i-need-you.wav",
  "pop/hellsine/samples/jeffrey-vocal-money.wav",
  // Prutti (Goodiepal) saying the label — his klokkentales ElevenLabs voice,
  // regenerated by `marketing/klokkentales/bin/voice.mjs say`.
  "pop/teknull/samples/prutti-aesthetic-dot-computer.wav",
};

// Minimal RIFF reader for the bank's own files: 48 kHz mono, PCM16 or
// float32. Anything else is skipped with a warning rather than guessed at.
static int load_wav_mono_48k(const char *path, float **out_data, long *out_n) {
  FILE *f = fopen(path, "rb"); if (!f) return 0;
  fseek(f, 0, SEEK_END); long bytes = ftell(f); rewind(f);
  if (bytes < 44) { fclose(f); return 0; }
  unsigned char *raw = malloc((size_t)bytes);
  if (!raw || fread(raw, 1, (size_t)bytes, f) != (size_t)bytes) {
    free(raw); fclose(f); return 0;
  }
  fclose(f);
  if (memcmp(raw, "RIFF", 4) || memcmp(raw + 8, "WAVE", 4)) { free(raw); return 0; }
  uint16_t format = 0, channels = 0, bits = 0; uint32_t rate = 0;
  long data_off = -1, data_len = 0;
  for (long off = 12; off + 8 <= bytes;) {
    uint32_t size; memcpy(&size, raw + off + 4, 4);
    if (!memcmp(raw + off, "fmt ", 4) && off + 24 <= bytes) {
      memcpy(&format, raw + off + 8, 2);
      memcpy(&channels, raw + off + 10, 2);
      memcpy(&rate, raw + off + 12, 4);
      memcpy(&bits, raw + off + 22, 2);
    } else if (!memcmp(raw + off, "data", 4)) {
      data_off = off + 8; data_len = size;
    }
    off += 8 + size + (size & 1);
  }
  int pcm16 = format == 1 && bits == 16, f32 = format == 3 && bits == 32;
  if (data_off < 0 || channels != 1 || rate != 48000 || (!pcm16 && !f32)
      || data_off + data_len > bytes) { free(raw); return 0; }
  long frames = data_len / (bits / 8);
  float *data = malloc((size_t)frames * sizeof *data);
  if (!data) { free(raw); return 0; }
  for (long i = 0; i < frames; i++) {
    if (pcm16) {
      int16_t s; memcpy(&s, raw + data_off + i * 2, 2);
      data[i] = (float)s / 32768.0f;
    } else {
      memcpy(&data[i], raw + data_off + i * 4, 4);
    }
  }
  free(raw);
  *out_data = data; *out_n = frames;
  return 1;
}

static void load_vox(void) {
  for (int i = 0; i < VOX_COUNT; i++) {
    if (!load_wav_mono_48k(vox_files[i], &vox_samples[i].data, &vox_samples[i].n))
      fprintf(stderr, "# vox · missing or unsupported: %s (skipping)\n", vox_files[i]);
  }
}

static void score_vox(void) {
  // The label tag opens the record over the solo piano and re-stamps the
  // arrival of the last groove — spoken by Prutti, the klokken's own voice.
  add_event(V_VOX, bar_time(4, 0) + .02, 1.35, 0, .50, 0, 60, VOX_PRUTTI);
  add_event(V_VOX, bar_time(72, 0) + .05, 1.35, 0, .40, 0, 60, VOX_PRUTTI);
  // "i need you" is the featured voice, sung as a choir of himself: each
  // entry stacks the same take at chord intervals (root / third / fifth /
  // octave), staggered a few milliseconds and spread across its own spatial
  // lanes. One throat becomes a section — the angelic layer. A soft echo
  // trails 1.5 beats behind each call.
  // `stretch` draws the phrase out — the breakdown entries hold their vowel
  // for whole bars ("i neeeeed you") while the groove calls stay close to
  // spoken length.
  static const struct { int bar; double root, g, stretch, dur; int voices; } calls[] = {
    {40, 0, .34, 1.6, 2.0, 3}, {48, 3, .32, 1.6, 2.0, 3},
    {64, 0, .40, 3.2, 3.9, 4}, {66, 3, .38, 3.2, 3.9, 4},
    {68, -2, .38, 3.6, 4.3, 4}, {70, 5, .40, 4.0, 4.6, 4},
    {76, 0, .34, 2.0, 2.5, 3}, {80, 3, .32, 2.0, 2.5, 3},
  };
  // Major-triad stack with the octave on top; the upper voices sit back so
  // the root still carries the words.
  static const double chord_semis[4] = {0, 4, 7, 12};
  static const double chord_level[4] = {1, .52, .40, .30};
  for (int i = 0; i < 8; i++) {
    double at = bar_time(calls[i].bar, 2);
    for (int v = 0; v < calls[i].voices; v++) {
      double g = calls[i].g * chord_level[v];
      double semis = calls[i].root + chord_semis[v];
      // Upper voices stretch a touch further, so the chord blooms open.
      double stretch = calls[i].stretch * (1 + .06 * v);
      add_event(V_VOX, at + v * .014, calls[i].dur, semis, g, stretch,
                61 + i * 4 + v, VOX_INEEDYOU);
      add_event(V_VOX, at + 1.5 * BEAT + v * .014, calls[i].dur, semis, g * .32,
                stretch, 120 + i * 4 + v, VOX_INEEDYOU);
    }
  }
  // The sung "money" turnarounds, pitched around the cycle so they sing.
  static const int turns[] = {23, 31, 47, 55, 87};
  static const double money_semis[] = {0, 3, -4, 7, 0};
  for (int i = 0; i < 5; i++)
    add_event(V_VOX, bar_time(turns[i], 2.5), .62, money_semis[i], .40, 0, 62, VOX_MONEY);
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

// A calmer spatial field than the orchestral cut: the ear model stays, but the
// rotation bursts and platter wobble are gone. Sources drift slowly; kick,
// bass, and sub weight stay centered like a record.
static void source_pose(const Event *e, double t, double *az, double *el, double *distance) {
  double phase = fmod(e->lane * 2.399963229728653 + e->seed * 1.0e-7, TAU);
  double rate = e->voice == V_CLOCK ? 1.0 / (10 * BAR)
              : e->voice == V_PIANO ? -1.0 / (16 * BAR)
              : e->voice == V_PAD ? -1.0 / (28 * BAR)
              : 1.0 / (20 * BAR);
  *az = phase + TAU * rate * t;
  double base = e->voice == V_CLOCK ? 2.2 : e->voice == V_PIANO ? 2.4 : e->voice == V_PAD ? 4.2 : 3.0;
  *distance = clamp(base * (1 + .12 * sin(*az * .73 + e->lane * .41)), .8, 5.5);
  *el = .22 * sin(*az * .57 + e->lane * .62) + (e->voice == V_CLOCK ? .2 : 0);
}

typedef struct { ACHrtf ear; double az, el, distance, pan, near; int posed; } SpatialState;

static int centered_voice(Voice v) { return v == V_KICK || v == V_BASS; }
static int drum_voice(Voice v) { return v <= V_SHAKER; }

// Special Sign topology: the direct voice stays dry on a stable equal-power
// pan, and the ear model contributes only an antisymmetric side field,
// collected on its own bus and band-limited in mastering. The dry master is
// intact and mono-safe; the space is a return, not a replacement.
static void emit(const Event *e, SpatialState *sp, long local_i, double mono) {
  long at = (long)llround(e->at * SR) + local_i;
  if (at < 0 || at >= N) return;
  float *L = drum_voice(e->voice) ? drumsL : musicL;
  float *R = drum_voice(e->voice) ? drumsR : musicR;
  if (centered_voice(e->voice)) {
    L[at] += (float)(mono * .70710678); R[at] += (float)(mono * .70710678); return;
  }
  if ((local_i & 31) == 0 || !sp->posed) {
    source_pose(e, (double)at / SR, &sp->az, &sp->el, &sp->distance);
    if (!sp->posed) {
      sp->pan = .55 * sin(sp->az);
      sp->near = clamp(2.6 / sp->distance, .6, 1.1);
      sp->posed = 1;
    }
  }
  double a = M_PI * .25 * (1 + sp->pan);
  L[at] += (float)(mono * cos(a) * .96 * sp->near);
  R[at] += (float)(mono * sin(a) * .96 * sp->near);
  float l, r;
  ac_hrtf_process(&sp->ear, (float)mono, sp->az, sp->el, sp->distance, &l, &r);
  sideB[at] += (float)(.5 * (l - r));
}

static void render_piano(const Event *e) {
  if (!piano_count) return;
  int a = nearest_piano(e->midi);
  PianoAnchor *p = &piano[a];
  double step = pow(2.0, (e->midi - p->midi) / 12.0);
  long held = (long)(e->dur * SR), release = (long)((.9 + .3 * (e->articulation == 1)) * SR);
  long max_n = held + release;
  long avail = (long)((p->n - 2) / step);
  if (max_n > avail) max_n = avail;
  SpatialState sp = {0};
  double pos = 0, keyboard = clamp((e->midi - 60) / 42.0, -1, 1), dust = 0;
  for (long i = 0; i < max_n; i++, pos += step) {
    long q = (long)pos; if (q + 1 >= p->n) break;
    double u = pos - q, s = p->data[q] + (p->data[q + 1] - p->data[q]) * u;
    double env = i < .002 * SR ? i / (.002 * SR) : 1;
    if (i > held) env *= 1 - (double)(i - held) / release;
    // One-pole darkening = the dusty sampled-off-a-record tone.
    dust += .32 * (s - dust);
    double body = tanh(dust * 1.05);
    emit(e, &sp, i, body * env * e->gain * (1.10 + .08 * keyboard) * tail_fade(i, max_n));
  }
}

// Sine-bump bass: no string, no twang. Two soft sine layers (fundamental +
// sub octave, a whisper of second harmonic for note definition) under a
// rounded bump envelope, keeping the portamento slides between roots.
static void render_bass(const Event *e) {
  double f_target = midi_hz(e->midi);
  double f_from = e->articulation ? midi_hz(e->midi - e->articulation) : f_target;
  long n = (long)((e->dur + .16) * SR);
  double slide_len = .07 * SR;
  double p1 = 0, p2 = 0, p3 = 0;
  SpatialState sp = {0};
  for (long i = 0; i < n; i++) {
    double t = (double)i / SR;
    double f = i < slide_len ? f_from + (f_target - f_from) * smooth(i / slide_len) : f_target;
    p1 += TAU * f / SR;
    p2 += TAU * f * .5 / SR;
    p3 += TAU * f * 2.0 / SR;
    double env = smooth(t / .020);
    if (t > e->dur) env *= exp(-(t - e->dur) / .05);
    double s = sin(p1) * .74 + sin(p2) * .40 + sin(p3) * .07;
    emit(e, &sp, i, tanh(s * 1.1) * 1.25 * env * e->gain * tail_fade(i, n));
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
    double t = (double)i / SR;
    double attack = e->voice == V_PIZZ ? .003 : .095 + .02 * (e->lane & 3);
    double env;
    if (e->voice == V_PIZZ) env = fmin(1, t / attack) * exp(-t / .19);
    else {
      env = fmin(1, t / attack);
      if (t > e->dur - .15) env *= clamp((e->dur - t + .26) / .41, 0, 1);
    }
    double vib = e->voice == V_PIZZ ? 1 : 1 + .0027 * sin(TAU * (5.0 + .13 * (e->lane & 3)) * t + e->lane);
    double raw = 0;
    static const double detune[5] = {-.0042, -.0013, 0, .0017, .0048};
    for (int k = 0; k < 5; k++) {
      phases[k] += TAU * f * vib * (1 + detune[k]) / SR;
      for (int h = 1; h <= 6 && f * h < SR * .45; h++)
        raw += sin(phases[k] * h + .07 * h * h) / pow(h, 1.35);
    }
    raw /= 5.8;
    double scrape = noise01(&rs) * (.035 + .055 * exp(-t / .028));
    double cutoff = e->voice == V_PIZZ ? .15 : .045 + .018 * env;
    bow += cutoff * (raw + scrape - bow);
    body += .028 * (bow - body);
    emit(e, &sp, i, tanh((body * 1.18 + bow * .24 + scrape * .02) * 1.3) * env * e->gain * tail_fade(i, n));
  }
}

static void render_clock(const Event *e) {
  long n = (long)((e->dur + (e->articulation == 2 ? 1.3 : .38)) * SR);
  double f = midi_hz(e->midi), p1 = 0, p2 = 0, p3 = 0;
  SpatialState sp = {0};
  for (long i = 0; i < n; i++) {
    double t = (double)i / SR;
    p1 += TAU * f / SR; p2 += TAU * f * 2.006 / SR; p3 += TAU * f * 3.993 / SR;
    double decay = e->articulation == 2 ? .82 : e->articulation == 1 ? .30 : .095;
    double env = fmin(1, t / .0025) * exp(-t / decay);
    double bell = sin(p1 + .13 * sin(TAU * f * 1.414 * t) * exp(-t / .08))
                + .43 * sin(p2) * exp(-t / .19)
                + .20 * sin(p3) * exp(-t / .12);
    emit(e, &sp, i, tanh(bell * .82) * env * e->gain * tail_fade(i, n));
  }
}

// The choir: one smooth sine per chord tone (a whisper of octave), breath-slow
// attack, zero saturation — the harmonized way into the groove.
static void render_sine_choir(const Event *e) {
  long n = (long)((e->dur + 1.2) * SR);
  double f = midi_hz(e->midi);
  double p1 = (e->seed & 1023) / 1023.0 * TAU, p2 = p1 * 1.7;
  SpatialState sp = {0};
  for (long i = 0; i < n; i++) {
    double t = (double)i / SR;
    double env = smooth(t / .9);
    if (t > e->dur) env *= smooth((e->dur + 1.2 - t) / 1.2);
    double vib = 1 + .0019 * sin(TAU * .22 * t + e->lane);
    p1 += TAU * f * vib / SR; p2 += TAU * f * 2.0 * vib / SR;
    double s = sin(p1) + .10 * sin(p2);
    emit(e, &sp, i, s * env * e->gain * tail_fade(i, n));
  }
}

static void render_pad(const Event *e) {
  if (e->articulation == 2) { render_sine_choir(e); return; }
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
    warm += (.012 + .014 * env) * (raw - warm);
    emit(e, &sp, i, tanh(warm * 1.15) * env * e->gain * tail_fade(i, n));
  }
}

static void render_kick(const Event *e) {
  long n = (long)(e->dur * SR); double phase = 0; uint32_t rs = e->seed;
  SpatialState sp = {0};
  for (long i = 0; i < n; i++) {
    double t = (double)i / SR, f = 41 + 92 * exp(-t / .028);
    phase += TAU * f / SR;
    double body = sin(phase) * exp(-t / .19);
    double click = noise01(&rs) * exp(-t / .0032);
    emit(e, &sp, i, tanh(body * 1.55 + click * .08) * e->gain * .76 * tail_fade(i, n));
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
    else if (e->voice == V_SHAKER) {
      // Softer band: less top than the hats, a grain of husk.
      lp += .11 * (white - lp);
      v = (white - lp) * .7 + lp * .12;
      env = exp(-t / .028) * fmin(1, t / .003);
    }
    last = white;
    emit(e, &sp, i, v * env * e->gain * (e->voice == V_CLAP ? .40 : e->voice == V_SHAKER ? .30 : .26) * tail_fade(i, n));
  }
}

// Where the voice actually starts inside each take (leading-silence trim).
static const double vox_start[VOX_COUNT] = {0, .06, 0, 0};

// Granular time-stretch: overlapping raised-cosine grains read at the pitch
// rate but advanced more slowly, so a phrase can be drawn out — held, angelic
// — without dropping in pitch. `pan` carries the stretch factor (1 = as
// recorded, 2 = twice as long), `midi` the pitch shift in semitones.
static void render_vox(const Event *e) {
  int which = e->articulation;
  if (which < 0 || which >= VOX_COUNT) return;
  const VoxSample *sample = &vox_samples[which];
  if (!sample->data) return;
  long start = (long)(vox_start[which] * SR);
  double step = pow(2.0, e->midi / 12.0);
  double stretch = e->pan > 0 ? e->pan : 1.0;
  long n = (long)(e->dur * SR);
  long source_n = sample->n - start - 2;
  if (n <= 0 || source_n <= 0) return;

  const long grain = (long)(.055 * SR);          // 55 ms grains
  const long hop_out = grain / 2;                // 50% overlap on output
  const double hop_in = hop_out * step / stretch; // …read in more slowly
  double *acc = calloc((size_t)n + grain, sizeof *acc);
  if (!acc) return;
  double read = 0;
  for (long g = 0; g * hop_out < n; g++) {
    double base = read;
    if (base + grain * step >= source_n) break;   // ran out of take
    for (long k = 0; k < grain; k++) {
      long o = g * hop_out + k;
      if (o >= n + grain - 1) break;
      double pos = start + base + k * step;
      long q = (long)pos;
      double u = pos - q;
      double s = sample->data[q] + (sample->data[q + 1] - sample->data[q]) * u;
      // Hann window — overlapped at 50% these sum to unity.
      double w = .5 - .5 * cos(TAU * k / (grain - 1));
      acc[o] += s * w;
    }
    read += hop_in;
  }
  SpatialState sp = {0};
  double dust = 0;
  for (long i = 0; i < n; i++) {
    double t = (double)i / SR;
    double env = fmin(1, t / .006);
    // The same one-pole dusting as the piano, so the voice sits in the
    // record instead of on top of it.
    dust += .55 * (acc[i] - dust);
    emit(e, &sp, i, dust * env * e->gain * tail_fade(i, n));
  }
  free(acc);
}

static void render_event(const Event *e) {
  switch (e->voice) {
    case V_PIANO: render_piano(e); break;
    case V_BASS: render_bass(e); break;
    case V_STRING: case V_PIZZ: render_string(e); break;
    case V_CLOCK: render_clock(e); break;
    case V_PAD: render_pad(e); break;
    case V_KICK: render_kick(e); break;
    case V_VOX: render_vox(e); break;
    default: render_noise_drum(e); break;
  }
}

// The pump: everything musical (including the bass) ducks under the kick and
// swells back — the dance-floor breath.
static double kick_duck(double t) {
  int bar = (int)floor(t / BAR);
  if (bar < 0 || bar >= BARS) return 1;
  double in_beat = fmod(t, BEAT);
  if (in_beat < 0 || in_beat > .26) return 1;
  int beat = (int)floor(fmod(t, BAR) / BEAT + 1e-7);
  if (!kick_active(bar, beat)) return 1;
  double duck = .34 + .66 * pow(in_beat / .26, 1.6);
  // Reach the floor over 3.5 ms instead of instantly — the pump without the pop.
  if (in_beat < .0035) {
    double u = in_beat / .0035;
    duck = 1 + (duck - 1) * u * u * (3 - 2 * u);
  }
  return duck;
}

static void room_and_master(void) {
  static const double delay_sec[] = {.031, .047, .071, .103};
  static const double gain[] = {.095, .070, .046, .027};
  for (int tap = 0; tap < 4; tap++) {
    long d = (long)llround(delay_sec[tap] * SR);
    for (long i = d; i < N; i++) {
      float l = musicR[i - d] * gain[tap];
      float r = musicL[i - d] * gain[tap];
      musicL[i] += l; musicR[i] += r;
    }
  }
  // Special Sign return: band-limit the antisymmetric ear field (80 Hz –
  // 11.5 kHz) and hand it back L=+/R=- with a send that breathes with the
  // arrangement. The send is slewed (~.4 s) so section moves never step.
  double hp_rc = 1 / (TAU * 80.0), hp_a = hp_rc / (hp_rc + 1.0 / SR);
  double lp_k = 1 - exp(-TAU * 11500.0 / SR);
  double hp = 0, lp = 0, prev = 0, send = .45;
  for (long i = 0; i < N; i++) {
    double s = sideB[i];
    hp = hp_a * (hp + s - prev); prev = s;
    lp += lp_k * (hp - lp);
    int b = (int)((double)i / SR / BAR);
    double target = b < 8 ? .45 : b < 16 ? .60 : b < 64 ? .74 : b < 72 ? 1.0 : b < 92 ? .80 : .55;
    send += .00005 * (target - send);
    musicL[i] += (float)(lp * send);
    musicR[i] -= (float)(lp * send);
  }
  // Clean sum: duck, fade, measure, and normalize linearly. No master tanh —
  // loudness and the ceiling belong to the mastering chain.
  double peak = 0;
  for (long i = 0; i < N; i++) {
    double t = (double)i / SR, duck = kick_duck(t);
    double fade_in = fmin(1, i / (.010 * SR));
    double fade_out = fmin(1, (N - 1 - i) / (1.6 * SR));
    double fade = fmax(0, fmin(fade_in, fade_out));
    double l = (musicL[i] * duck + drumsL[i]) * fade;
    double r = (musicR[i] * duck + drumsR[i]) * fade;
    musicL[i] = (float)l; musicR[i] = (float)r;
    if (fabs(l) > peak) peak = fabs(l); if (fabs(r) > peak) peak = fabs(r);
  }
  double norm = peak > 1e-9 ? .92 / peak : 1;
  if (norm < 1) for (long i = 0; i < N; i++) { musicL[i] *= (float)norm; musicR[i] *= (float)norm; }
  fprintf(stderr, "# pre-master peak %.6f · linear trim %.3f\n", peak, norm < 1 ? norm : 1.0);
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
  fprintf(f, "  \"track\": \"taksmukkeklokken-smooch\",\n");
  fprintf(f, "  \"renderer\": \"pop/teknull/c/taksmukkeklokken-smooch.c\",\n");
  fprintf(f, "  \"deterministic\": true,\n");
  fprintf(f, "  \"direction\": \"scrappy NYC house: swung/humanized pocket, sine-bump bass with slides, dusty offbeat piano stabs, sparse clock-bell hook; sine-choir lift, Special Sign spatial return (dry direct + band-limited antisymmetric side field), clean un-clipped master\",\n");
  fprintf(f, "  \"transport\": {\"bpm\": 122, \"meter\": \"4/4\", \"key\": \"A minor\", \"bars\": %d, \"durationSec\": %.3f, \"sampleRate\": 48000},\n", BARS, DURATION);
  fprintf(f, "  \"groove\": {\"swing16Beats\": %.3f, \"swing8Beats\": %.3f, \"timingScatterMs\": 13, \"velocitySpread\": .28, \"sidechain\": \"kick duck to .34 over 260 ms\"},\n", SWING16, SWING8);
  fprintf(f, "  \"orchestration\": {\"bass\": \"fractional-delay Karplus-Strong pluck + soft sub octave + tanh drive, portamento slides\", \"grandPiano\": \"Salamander CC0 bank, one-pole dusted, offbeat house stabs + rolled phrase chords\", \"bells\": \"sparse klokken hook every 4th groove bar + hour-bell arrivals\", \"harmonicCycle\": [\"Am(add9)\",\"Fmaj7/A\",\"Cmaj9/G\",\"G6(add9)\",\"Dm9/F\",\"Am/E\",\"Bdim7/F\",\"E7(b9)\"]},\n");
  fprintf(f, "  \"eventCount\": %d,\n  \"eventCountsByVoice\": {\n", event_count);
  for (int v = 0; v <= V_VOX; v++)
    fprintf(f, "    \"%s\": %d%s\n", voice_name((Voice)v), counts[v], v == V_VOX ? "" : ",");
  fprintf(f, "  },\n  \"events\": [\n");
  for (int i = 0; i < event_count; i++) {
    Event *e = &events[i];
    fprintf(f, "    {\"at\":%.6f,\"dur\":%.6f,\"voice\":\"%s\",\"section\":\"%s\",\"midi\":%.3f,\"gain\":%.4f,\"lane\":%d,\"articulation\":%d}%s\n",
            e->at, e->dur, voice_name(e->voice), section_name(e->at), e->midi,
            e->gain, e->lane, e->articulation,
            i == event_count - 1 ? "" : ",");
  }
  fprintf(f, "  ]\n}\n");
  int ok = !ferror(f); fclose(f); return ok;
}

static void usage(const char *argv0) {
  fprintf(stderr, "usage: %s [--out raw.wav] [--receipt events.json] [--piano-dir dir] [--score-only]\n", argv0);
}

int main(int argc, char **argv) {
  for (int i = 1; i < argc; i++) {
    if (!strcmp(argv[i], "--out") && i + 1 < argc) out_path = argv[++i];
    else if (!strcmp(argv[i], "--receipt") && i + 1 < argc) receipt_path = argv[++i];
    else if (!strcmp(argv[i], "--piano-dir") && i + 1 < argc) piano_dir = argv[++i];
    else if (!strcmp(argv[i], "--score-only")) score_only = 1;
    else if (!strcmp(argv[i], "--help")) { usage(argv[0]); return 0; }
    else { usage(argv[0]); return 2; }
  }
  build_score();
  if (!write_receipt(receipt_path)) { fprintf(stderr, "cannot write %s: %s\n", receipt_path, strerror(errno)); return 1; }
  fprintf(stderr, "# score · %d events · %d bars · %.3f s\n", event_count, BARS, DURATION);
  if (score_only) return 0;
  const char *found = find_piano_dir();
  if (!found || !load_piano(found)) {
    fprintf(stderr, "no Salamander piano bank; use --piano-dir (expected 21.raw..96.raw)\n"); return 1;
  }
  fprintf(stderr, "# grand piano · %d CC0 anchors · %s\n", piano_count, found);
  load_vox();
  N = (long)llround(DURATION * SR);
  musicL = calloc((size_t)N, sizeof *musicL); musicR = calloc((size_t)N, sizeof *musicR);
  drumsL = calloc((size_t)N, sizeof *drumsL); drumsR = calloc((size_t)N, sizeof *drumsR);
  sideB = calloc((size_t)N, sizeof *sideB);
  if (!musicL || !musicR || !drumsL || !drumsR || !sideB) { fprintf(stderr, "audio allocation failed\n"); return 1; }
  for (int i = 0; i < event_count; i++) {
    render_event(&events[i]);
    if ((i + 1) % 500 == 0) fprintf(stderr, "# render · %d/%d events\n", i + 1, event_count);
  }
  room_and_master();
  if (!write_wav(out_path)) { fprintf(stderr, "cannot write %s: %s\n", out_path, strerror(errno)); return 1; }
  fprintf(stderr, "# wrote %s · 48 kHz stereo float32 · %.3f s\n", out_path, DURATION);
  return 0;
}
