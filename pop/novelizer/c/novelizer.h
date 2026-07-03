// novelizer.h — shared harness for pop/novelizer voices.
//
// A voice is one self-contained C99 file (libm only) that implements a
// full-melody renderer and hands it to nv_main(). The harness owns the
// test melodies, buffer allocation, peak normalization, and WAV output,
// so every voice renders the identical musical material and the results
// compare fairly.
//
// House style (matches hellsine.c / fedac/native/src/audio.c): keep a
// normalized phase 0..1 advanced by freq / NV_SR per tick.
//
// Usage in a voice file:
//
//   #include "../novelizer.h"
//   static void render(const NvMelody *m, float *out, int nframes) { ... }
//   int main(int argc, char **argv) {
//     return nv_main(argc, argv, "myvoice", render);
//   }
//
// CLI:  ./build/myvoice                  render all melodies -> out/
//       ./build/myvoice --melody stab    render one
//       ./build/myvoice --out somedir    override output dir

#ifndef NOVELIZER_H
#define NOVELIZER_H

#define _POSIX_C_SOURCE 200809L
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <math.h>
#include <sys/stat.h>

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif
#ifndef NV_TAU
#define NV_TAU (2.0 * M_PI)
#endif

enum { NV_SR = 48000 };
#define NV_TAIL_SEC 3.0 /* silence appended after the last note for decays */

typedef struct {
  double freq;  /* Hz; 0 = rest */
  double start; /* seconds from melody start */
  double dur;   /* seconds the note is held (voice decides its envelope) */
  double vel;   /* 0..1 */
} NvNote;

typedef struct {
  const char *name;
  const NvNote *notes;
  int count;
  double length; /* seconds, end of last note (tail added by harness) */
} NvMelody;

typedef void (*NvRenderFn)(const NvMelody *m, float *out, int nframes);

/* ── pitch helper ─────────────────────────────────────────────────── */
static inline double nv_mtof(double midi) {
  return 440.0 * pow(2.0, (midi - 69.0) / 12.0);
}

/* ── test melodies ────────────────────────────────────────────────── */
/* Note tables are authored in MIDI via NV_N and converted at init.     */
#define NV_N(midi, st, d, v) { (midi), (st), (d), (v) }

/* rising — A-minor-pentatonic ascent A2..A5, timbre across registers  */
static NvNote nv_mel_rising[] = {
  NV_N(45, 0.0, 0.45, 0.85), NV_N(48, 0.5, 0.45, 0.75),
  NV_N(50, 1.0, 0.45, 0.80), NV_N(52, 1.5, 0.45, 0.70),
  NV_N(55, 2.0, 0.45, 0.85), NV_N(57, 2.5, 0.45, 0.75),
  NV_N(60, 3.0, 0.45, 0.80), NV_N(62, 3.5, 0.45, 0.70),
  NV_N(64, 4.0, 0.45, 0.85), NV_N(67, 4.5, 0.45, 0.75),
  NV_N(69, 5.0, 0.45, 0.80), NV_N(72, 5.5, 0.45, 0.70),
  NV_N(74, 6.0, 0.45, 0.85), NV_N(76, 6.5, 0.45, 0.75),
  NV_N(79, 7.0, 0.45, 0.85), NV_N(81, 7.5, 1.60, 0.95),
};

/* lyrical — D dorian phrase with held tones, tests sustain + shape    */
static NvNote nv_mel_lyrical[] = {
  NV_N(62, 0.00, 0.90, 0.70), NV_N(65, 1.00, 0.40, 0.60),
  NV_N(67, 1.50, 0.90, 0.75), NV_N(69, 2.50, 1.40, 0.85),
  NV_N(67, 4.00, 0.40, 0.60), NV_N(65, 4.50, 0.40, 0.55),
  NV_N(67, 5.00, 1.90, 0.80), NV_N(62, 7.00, 0.90, 0.65),
  NV_N(60, 8.00, 0.90, 0.60), NV_N(62, 9.00, 2.40, 0.90),
};

/* stab — fast staccato 16ths at 140bpm (0.107s/16th), Em arpeggio     */
static NvNote nv_mel_stab[] = {
  NV_N(52, 0.000, 0.08, 0.95), NV_N(59, 0.107, 0.08, 0.70),
  NV_N(64, 0.214, 0.08, 0.80), NV_N(67, 0.321, 0.08, 0.65),
  NV_N(71, 0.429, 0.08, 0.90), NV_N(67, 0.536, 0.08, 0.60),
  NV_N(64, 0.643, 0.08, 0.75), NV_N(59, 0.750, 0.08, 0.60),
  NV_N(52, 0.857, 0.08, 0.95), NV_N(59, 0.964, 0.08, 0.70),
  NV_N(64, 1.071, 0.08, 0.80), NV_N(67, 1.179, 0.08, 0.65),
  NV_N(71, 1.286, 0.08, 0.90), NV_N(72, 1.393, 0.08, 0.75),
  NV_N(71, 1.500, 0.08, 0.85), NV_N(64, 1.607, 0.30, 1.00),
};

/* drone — two long low tones (root then fifth), internal evolution    */
static NvNote nv_mel_drone[] = {
  NV_N(38, 0.0, 8.0, 0.80),
  NV_N(45, 8.5, 8.0, 0.80),
};

/* chromatic — one octave up from C4, tuning sanity                    */
static NvNote nv_mel_chromatic[] = {
  NV_N(60, 0.0, 0.28, 0.75), NV_N(61, 0.3, 0.28, 0.75),
  NV_N(62, 0.6, 0.28, 0.75), NV_N(63, 0.9, 0.28, 0.75),
  NV_N(64, 1.2, 0.28, 0.75), NV_N(65, 1.5, 0.28, 0.75),
  NV_N(66, 1.8, 0.28, 0.75), NV_N(67, 2.1, 0.28, 0.75),
  NV_N(68, 2.4, 0.28, 0.75), NV_N(69, 2.7, 0.28, 0.75),
  NV_N(70, 3.0, 0.28, 0.75), NV_N(71, 3.3, 0.28, 0.75),
  NV_N(72, 3.6, 0.90, 0.85),
};

static NvMelody nv_melodies[] = {
  { "rising",    nv_mel_rising,    (int)(sizeof nv_mel_rising    / sizeof(NvNote)), 0 },
  { "lyrical",   nv_mel_lyrical,   (int)(sizeof nv_mel_lyrical   / sizeof(NvNote)), 0 },
  { "stab",      nv_mel_stab,      (int)(sizeof nv_mel_stab      / sizeof(NvNote)), 0 },
  { "drone",     nv_mel_drone,     (int)(sizeof nv_mel_drone     / sizeof(NvNote)), 0 },
  { "chromatic", nv_mel_chromatic, (int)(sizeof nv_mel_chromatic / sizeof(NvNote)), 0 },
};
enum { NV_MELODY_COUNT = (int)(sizeof nv_melodies / sizeof(NvMelody)) };

/* Convert authored MIDI numbers to Hz and compute lengths. Idempotent
   guard: freq fields start as MIDI (<128); after conversion they're Hz. */
static void nv_init_melodies(void) {
  for (int i = 0; i < NV_MELODY_COUNT; i++) {
    NvMelody *m = &nv_melodies[i];
    double end = 0;
    for (int j = 0; j < m->count; j++) {
      NvNote *n = (NvNote *)&m->notes[j];
      if (n->freq > 0 && n->freq < 128.0) n->freq = nv_mtof(n->freq);
      double e = n->start + n->dur;
      if (e > end) end = e;
    }
    m->length = end;
  }
}

/* ── WAV writer (mono 16-bit PCM) ─────────────────────────────────── */
static int nv_write_wav(const char *path, const float *buf, int nframes) {
  FILE *f = fopen(path, "wb");
  if (!f) { fprintf(stderr, "novelizer: cannot open %s\n", path); return 1; }
  uint32_t data_bytes = (uint32_t)nframes * 2;
  uint32_t chunk = 36 + data_bytes;
  uint16_t fmt = 1, ch = 1, bits = 16, block = 2;
  uint32_t sr = NV_SR, byterate = NV_SR * 2;
  fwrite("RIFF", 1, 4, f); fwrite(&chunk, 4, 1, f); fwrite("WAVE", 1, 4, f);
  fwrite("fmt ", 1, 4, f);
  uint32_t fmtlen = 16; fwrite(&fmtlen, 4, 1, f);
  fwrite(&fmt, 2, 1, f); fwrite(&ch, 2, 1, f);
  fwrite(&sr, 4, 1, f); fwrite(&byterate, 4, 1, f);
  fwrite(&block, 2, 1, f); fwrite(&bits, 2, 1, f);
  fwrite("data", 1, 4, f); fwrite(&data_bytes, 4, 1, f);
  for (int i = 0; i < nframes; i++) {
    double s = buf[i];
    if (s > 1.0) s = 1.0; else if (s < -1.0) s = -1.0;
    int16_t v = (int16_t)lrint(s * 32767.0);
    fwrite(&v, 2, 1, f);
  }
  fclose(f);
  return 0;
}

/* ── driver ───────────────────────────────────────────────────────── */
static int nv_main(int argc, char **argv, const char *voice, NvRenderFn render) {
  const char *only = NULL, *outdir = "out";
  for (int i = 1; i < argc; i++) {
    if (!strcmp(argv[i], "--melody") && i + 1 < argc) only = argv[++i];
    else if (!strcmp(argv[i], "--out") && i + 1 < argc) outdir = argv[++i];
    else { fprintf(stderr, "usage: %s [--melody name] [--out dir]\n", argv[0]); return 2; }
  }
  nv_init_melodies();
  mkdir(outdir, 0755);
  int rendered = 0;
  for (int i = 0; i < NV_MELODY_COUNT; i++) {
    const NvMelody *m = &nv_melodies[i];
    if (only && strcmp(only, m->name)) continue;
    int nframes = (int)((m->length + NV_TAIL_SEC) * NV_SR);
    float *buf = (float *)calloc((size_t)nframes, sizeof(float));
    if (!buf) { fprintf(stderr, "novelizer: alloc failed\n"); return 1; }
    render(m, buf, nframes);
    /* health check + peak normalize to -1 dBFS so voices compare fairly */
    double peak = 0; int bad = 0;
    for (int j = 0; j < nframes; j++) {
      if (!isfinite(buf[j])) { bad = 1; buf[j] = 0; }
      double a = fabs(buf[j]);
      if (a > peak) peak = a;
    }
    if (bad) fprintf(stderr, "novelizer: WARNING %s/%s produced NaN/inf (zeroed)\n", voice, m->name);
    if (peak > 0) {
      double g = pow(10.0, -1.0 / 20.0) / peak;
      for (int j = 0; j < nframes; j++) buf[j] = (float)(buf[j] * g);
    } else {
      fprintf(stderr, "novelizer: WARNING %s/%s is silent\n", voice, m->name);
    }
    char path[512];
    snprintf(path, sizeof path, "%s/%s-%s.wav", outdir, voice, m->name);
    if (nv_write_wav(path, buf, nframes)) { free(buf); return 1; }
    printf("%s  (%d frames, peak %.3f%s)\n", path, nframes, peak, bad ? ", HAD NaN" : "");
    free(buf);
    rendered++;
  }
  if (!rendered) { fprintf(stderr, "novelizer: no melody named '%s'\n", only ? only : ""); return 2; }
  return 0;
}

#endif /* NOVELIZER_H */
