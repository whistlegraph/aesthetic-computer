// main.c — standalone driver for the singer core.
//
// This file is the ONLY part that knows about an audio device or the CLI.
// Menu Band (AVAudioEngine) / fedac (ALSA) would replace just this file and
// keep singer.c untouched — that's the point of the split.
//
// The headline: CHUNKED ANALYSIS. We analyze a short front chunk, start
// playing, and stream the rest of the corpus in on a background thread.

#define MINIAUDIO_IMPLEMENTATION
#include "miniaudio.h"
#include "singer.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>
#include <time.h>
#include <math.h>

static double now_ms(void) {
  struct timespec ts; clock_gettime(CLOCK_MONOTONIC, &ts);
  return ts.tv_sec * 1000.0 + ts.tv_nsec / 1e6;
}

// ── minimal 16-bit mono WAV reader ───────────────────────────────────────
static double *load_wav(const char *path, int *n, int *fs) {
  FILE *f = fopen(path, "rb");
  if (!f) { fprintf(stderr, "cannot open %s\n", path); return NULL; }
  fseek(f, 0, SEEK_END); long sz = ftell(f); fseek(f, 0, SEEK_SET);
  unsigned char *b = (unsigned char *)malloc(sz);
  if (fread(b, 1, sz, f) != (size_t)sz) { fclose(f); free(b); return NULL; }
  fclose(f);
  int rate = 44100, bits = 16, ch = 1;
  long off = 12, doff = 0; unsigned dlen = 0;
  while (off + 8 < sz) {
    unsigned id = *(unsigned *)(b + off);
    unsigned csz = *(unsigned *)(b + off + 4);
    if (memcmp(&id, "fmt ", 4) == 0) {
      ch   = *(short *)(b + off + 10);
      rate = *(int *)(b + off + 12);
      bits = *(short *)(b + off + 22);
    } else if (memcmp(&id, "data", 4) == 0) { doff = off + 8; dlen = csz; break; }
    off += 8 + csz + (csz & 1);
  }
  if (!doff || bits != 16) { fprintf(stderr, "need 16-bit PCM wav\n"); free(b); return NULL; }
  int total = dlen / 2, frames = total / ch;
  double *x = (double *)malloc(frames * sizeof(double));
  short *pcm = (short *)(b + doff);
  for (int i = 0; i < frames; i++) {          // downmix to mono
    double a = 0; for (int c = 0; c < ch; c++) a += pcm[i * ch + c] / 32768.0;
    x[i] = a / ch;
  }
  free(b);
  *n = frames; *fs = rate;
  return x;
}

// ── words.json reader (the known shape: [{"text":..,"fromMs":N,"toMs":N}]) ─
static int load_words(const char *path, singer_word *w, int max) {
  FILE *f = fopen(path, "rb");
  if (!f) { fprintf(stderr, "cannot open %s\n", path); return 0; }
  fseek(f, 0, SEEK_END); long sz = ftell(f); fseek(f, 0, SEEK_SET);
  char *b = (char *)malloc(sz + 1);
  if (fread(b, 1, sz, f) != (size_t)sz) { fclose(f); free(b); return 0; }
  b[sz] = 0; fclose(f);
  int n = 0; char *p = b;
  while (n < max && (p = strstr(p, "\"text\""))) {
    char *q = strchr(p + 6, '"'); if (!q) break;
    char *e = strchr(q + 1, '"'); if (!e) break;
    int len = (int)(e - q - 1); if (len > 47) len = 47;
    memcpy(w[n].text, q + 1, len); w[n].text[len] = 0;
    char *fm = strstr(e, "\"fromMs\""); char *tm = strstr(e, "\"toMs\"");
    if (!fm || !tm) break;
    double from = atof(fm + 9), to = atof(tm + 7);
    w[n].a = (int)lround(from / SINGER_FP_MS);
    w[n].b = (int)lround(to   / SINGER_FP_MS);
    if (w[n].b <= w[n].a) w[n].b = w[n].a + 1;
    w[n].vs = w[n].a; w[n].ve = w[n].b;
    n++; p = tm;
  }
  free(b);
  return n;
}

// ── shared state ─────────────────────────────────────────────────────────
static singer *S;
static volatile int running = 1;
static double t_start;

static void audio_cb(ma_device *dev, void *out, const void *in, ma_uint32 n) {
  (void)dev; (void)in;
  singer_audio_block(S, (float *)out, (int)n);
}

// analysis thread — streams the corpus in BEHIND the playhead
static void *analyze_thread(void *arg) {
  (void)arg;
  int total = singer_total_frames(S);
  int step  = 800;                       // ~4s of frames per chunk
  while (running && singer_analyzed(S) < total) {
    int from = singer_analyzed(S);
    int to = from + step; if (to > total) to = total;
    double t0 = now_ms();
    singer_analyze_chunk(S, from, to);
    printf("  · analyzed %.1fs–%.1fs (%.0fms)  [%d%%]\n",
           from * SINGER_FP_MS / 1000.0, to * SINGER_FP_MS / 1000.0,
           now_ms() - t0, 100 * singer_analyzed(S) / total);
    fflush(stdout);
  }
  printf("  · corpus fully analyzed\n"); fflush(stdout);
  return NULL;
}

// render thread — keeps one phrase queued, ahead of the bar
static void *render_thread(void *arg) {
  (void)arg;
  int cursor = 0;
  while (running) {
    if (singer_pending(S)) {                      // queue full — wait
      struct timespec ts = {0, 15 * 1000000}; nanosleep(&ts, NULL); continue;
    }
    int ready = singer_words_ready(S);
    if (cursor >= ready) {
      if (ready >= singer_word_count(S)) cursor = 0;   // loop the corpus
      else { struct timespec ts = {0, 20*1000000}; nanosleep(&ts, NULL); continue; }
    }
    double t0 = now_ms();
    int len = 0, used = 0;
    double *buf = singer_render_bars(S, cursor, 2, &len, &used);   // 2 bars
    if (buf && len > 0 && used > 0) {
      double bar_ms = (60000.0 / singer_params_ptr(S)->bpm) * 4;
      printf("  ▸ 2 bars · %d words · %.0fms (%.0f%% of a bar)\n",
             used, now_ms() - t0, (now_ms() - t0) / bar_ms * 100);
      fflush(stdout);
      singer_queue_phrase(S, buf, len);
      cursor += used;
    } else { if (buf) free(buf); cursor = 0; }
  }
  return NULL;
}

int main(int argc, char **argv) {
  const char *wav   = argc > 1 ? argv[1] : "steyerl/vocal.wav";
  const char *wjson = argc > 2 ? argv[2] : "steyerl/words.json";
  double front_s    = argc > 3 ? atof(argv[3]) : 4.0;   // front chunk to start on
  double f0_floor   = argc > 4 ? atof(argv[4]) : 70.0;  // per-speaker (see floor.py)

  t_start = now_ms();

  int n, fs;
  double *x = load_wav(wav, &n, &fs);
  if (!x) return 1;
  printf("→ source: %s  (%.1fs @ %dHz)\n", wav, (double)n / fs, fs);

  S = singer_create(x, n, fs);
  singer_params_ptr(S)->f0_floor = f0_floor;
  printf("→ f0 floor: %.0f Hz\n", f0_floor);

  static singer_word W[SINGER_MAX_WORDS];
  int nw = load_words(wjson, W, SINGER_MAX_WORDS);
  singer_set_words(S, W, nw);
  printf("→ %d words → %d phrases\n", nw, singer_phrase_count(S));

  // ── THE TRICK: analyze only a short FRONT CHUNK before making a sound ──
  int front_frames = (int)(front_s * 1000.0 / SINGER_FP_MS);
  double t0 = now_ms();
  singer_analyze_chunk(S, 0, front_frames);
  printf("→ front chunk (%.1fs) analyzed in %.0fms — starting now, "
         "rest streams in behind\n", front_s, now_ms() - t0);

  // ── offline bounce: --out <file.wav> <seconds> ────────────────────────
  // Same core, no audio device. This is how /pop bounces a stem.
  const char *out_path = NULL; double out_secs = 30.0;
  for (int i = 1; i < argc - 1; i++)
    if (!strcmp(argv[i], "--out")) {
      out_path = argv[i + 1];
      if (i + 2 < argc) out_secs = atof(argv[i + 2]);
    }
  if (out_path) {
    printf("→ bouncing %.0fs → %s\n", out_secs, out_path);
    singer_analyze_chunk(S, 0, singer_total_frames(S));   // full analysis for a bounce
    singer_transport(S, 1);
    int total = (int)(out_secs * fs);
    float *buf = (float *)calloc(total, sizeof(float));
    int BL = 512, cursor = 0;
    for (int off = 0; off < total; off += BL) {
      int nblk = (off + BL <= total) ? BL : (total - off);
      while (!singer_pending(S)) {                 // keep the faucet primed
        int len = 0, used = 0;
        if (cursor >= singer_words_ready(S)) cursor = 0;
        double *pb = singer_render_bars(S, cursor, 2, &len, &used);
        if (pb && len > 0 && used > 0) { singer_queue_phrase(S, pb, len); cursor += used; }
        else { if (pb) free(pb); break; }
      }
      singer_audio_block(S, buf + off, nblk);
    }
    // write 16-bit mono wav
    FILE *o = fopen(out_path, "wb");
    int dlen = total * 2, hdr = 36 + dlen;
    fwrite("RIFF", 1, 4, o); fwrite(&hdr, 4, 1, o); fwrite("WAVE", 1, 4, o);
    fwrite("fmt ", 1, 4, o);
    int sixteen = 16; short one = 1, mono = 1, bps = 16;
    int byterate = fs * 2; short align = 2;
    fwrite(&sixteen, 4, 1, o); fwrite(&one, 2, 1, o); fwrite(&mono, 2, 1, o);
    fwrite(&fs, 4, 1, o); fwrite(&byterate, 4, 1, o);
    fwrite(&align, 2, 1, o); fwrite(&bps, 2, 1, o);
    fwrite("data", 1, 4, o); fwrite(&dlen, 4, 1, o);
    for (int i = 0; i < total; i++) {
      double v = buf[i]; if (v > 1) v = 1; if (v < -1) v = -1;
      short sv = (short)lround(v * 32767); fwrite(&sv, 2, 1, o);
    }
    fclose(o); free(buf);
    printf("✓ wrote %s\n", out_path);
    singer_destroy(S); free(x);
    return 0;
  }

  ma_device_config cfg = ma_device_config_init(ma_device_type_playback);
  cfg.playback.format   = ma_format_f32;
  cfg.playback.channels = 1;
  cfg.sampleRate        = fs;
  cfg.dataCallback      = audio_cb;
  ma_device dev;
  if (ma_device_init(NULL, &cfg, &dev) != MA_SUCCESS) {
    fprintf(stderr, "audio device failed\n"); return 1;
  }
  ma_device_start(&dev);
  printf("→ output: %s\n", dev.playback.name);

  pthread_t ta, tr;
  pthread_create(&ta, NULL, analyze_thread, NULL);
  pthread_create(&tr, NULL, render_thread, NULL);

  printf("\n╭─ singer (C) · %dHz ─────────────────────────────────╮\n", fs);
  printf("│ start·stop · morph N·snap N·depth N·level N·bpm N   │\n");
  printf("│ mode snap|melody · quit                            │\n");
  printf("╰────────────────────────────────────────────────────╯\n\n");
  fflush(stdout);

  char line[256];
  singer_params *P = singer_params_ptr(S);
  while (fgets(line, sizeof(line), stdin)) {
    char cmd[32]; double v = 0; char sv[32];
    if (sscanf(line, "%31s %lf", cmd, &v) < 1) continue;
    if      (!strcmp(cmd, "start")) {
      singer_transport(S, 1);
      printf("▶ playing  (ask→sound: %.0fms)\n", now_ms() - t_start);
    }
    else if (!strcmp(cmd, "stop"))  { singer_transport(S, 0); printf("■ stopped\n"); }
    else if (!strcmp(cmd, "morph")) { P->morph = v; printf("  morph=%.2f\n", v); }
    else if (!strcmp(cmd, "snap"))  { P->snap  = v; printf("  snap=%.2f\n", v); }
    else if (!strcmp(cmd, "depth")) { P->depth = v; printf("  depth=%.2f\n", v); }
    else if (!strcmp(cmd, "level")) { P->level = v; printf("  level=%.2f\n", v); }
    else if (!strcmp(cmd, "bpm"))   { P->bpm   = v; printf("  bpm=%.0f\n", v); }
    else if (!strcmp(cmd, "floor")) { P->f0_floor = v; printf("  f0_floor=%.0f\n", v); }
    else if (!strcmp(cmd, "mode") && sscanf(line, "%*s %31s", sv) == 1) {
      P->mode = strcmp(sv, "melody") ? SINGER_SNAP : SINGER_MELODY;
      printf("  mode=%s\n", sv);
    }
    else if (!strcmp(cmd, "quit")) break;
    fflush(stdout);
  }

  running = 0;
  pthread_join(ta, NULL); pthread_join(tr, NULL);
  ma_device_uninit(&dev);
  singer_destroy(S);
  free(x);
  printf("bye\n");
  return 0;
}
