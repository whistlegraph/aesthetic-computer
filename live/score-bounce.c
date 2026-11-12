// score-bounce.c — offline SCORE-mode bounce for the singer core: a spoken
// take (16-bit wav) + words.json (text/fromMs/toMs/nsyl) + a score (one note
// per syllable: "midi at16 dur16" per line) → one sung wav. Same core the
// MacNeoPolitan Trio sings through; this is how /pop bounces a scored line.
//
//   ./score-bounce take.wav words.json score.txt out.wav [--bpm 80.75]
//       [--f0-floor 70] [--lock 0.875] [--vib-hz 5] [--vib-cents 18]
//       [--sustain-db 5] [--gap-ms 40] [--hold-ms 0] [--presence-db 0]
//       [--legato-ms 0] [--shimmer 0] [--consonant-gain 1.25] [--to16 N]
#include "singer.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

static double *load_wav(const char *path, int *n, int *fs) {
  FILE *f = fopen(path, "rb"); if (!f) { fprintf(stderr, "cannot open %s\n", path); return NULL; }
  fseek(f, 0, SEEK_END); long sz = ftell(f); fseek(f, 0, SEEK_SET);
  unsigned char *b = (unsigned char *)malloc(sz);
  if (fread(b, 1, sz, f) != (size_t)sz) { fclose(f); free(b); return NULL; }
  fclose(f);
  int rate = 44100, bits = 16, ch = 1; long off = 12, doff = 0; unsigned dlen = 0;
  while (off + 8 < sz) {
    unsigned id = *(unsigned *)(b + off); unsigned csz = *(unsigned *)(b + off + 4);
    if (memcmp(&id, "fmt ", 4) == 0) { ch = *(short *)(b + off + 10); rate = *(int *)(b + off + 12); bits = *(short *)(b + off + 22); }
    else if (memcmp(&id, "data", 4) == 0) { doff = off + 8; dlen = csz; break; }
    off += 8 + csz + (csz & 1);
  }
  if (!doff || bits != 16) { fprintf(stderr, "need 16-bit PCM wav\n"); free(b); return NULL; }
  int total = dlen / 2, frames = total / ch; double *x = (double *)malloc(frames * sizeof(double));
  short *pcm = (short *)(b + doff);
  for (int i = 0; i < frames; i++) { double a = 0; for (int c = 0; c < ch; c++) a += pcm[i * ch + c] / 32768.0; x[i] = a / ch; }
  free(b); *n = frames; *fs = rate; return x;
}
static int load_words(const char *path, singer_word *w, int max) {
  FILE *f = fopen(path, "rb"); if (!f) { fprintf(stderr, "cannot open %s\n", path); return 0; }
  fseek(f, 0, SEEK_END); long sz = ftell(f); fseek(f, 0, SEEK_SET);
  char *b = (char *)malloc(sz + 1); if (fread(b, 1, sz, f) != (size_t)sz) { fclose(f); free(b); return 0; }
  b[sz] = 0; fclose(f); int n = 0; char *p = b;
  while (n < max && (p = strstr(p, "\"text\""))) {
    char *q = strchr(p + 6, '"'); if (!q) break; char *e = strchr(q + 1, '"'); if (!e) break;
    int len = (int)(e - q - 1); if (len > 47) len = 47; memcpy(w[n].text, q + 1, len); w[n].text[len] = 0;
    char *fm = strstr(e, "\"fromMs\""), *tm = strstr(e, "\"toMs\""), *ns = strstr(e, "\"nsyl\""), *nx = strstr(e + 1, "\"text\"");
    if (!fm || !tm) break;
    double from = atof(fm + 9), to = atof(tm + 7);
    w[n].a = (int)lround(from / SINGER_FP_MS); w[n].b = (int)lround(to / SINGER_FP_MS);
    if (w[n].b <= w[n].a) w[n].b = w[n].a + 1;
    w[n].vs = w[n].a; w[n].ve = w[n].b; w[n].fixed = 0;
    w[n].nsyl = (ns && (!nx || ns < nx)) ? atoi(ns + 7) : 1;
    n++; p = tm;
  }
  free(b); return n;
}
int main(int argc, char **argv) {
  if (argc < 5) { fprintf(stderr, "usage: score-bounce take.wav words.json score.txt out.wav [opts]\n"); return 1; }
  double bpm = 80.75, f0_floor = 70, lock = 0.875, vib_hz = 5, vib_cents = 18, sustain_db = 5, gap_ms = 40, hold_ms = 0;
  double presence_db = 0, legato_ms = 0, shimmer = 0, cgain = 1.25, to16 = -1;
  for (int i = 5; i + 1 < argc; i += 2) {
    double v = atof(argv[i + 1]);
    if (!strcmp(argv[i], "--bpm")) bpm = v; else if (!strcmp(argv[i], "--f0-floor")) f0_floor = v;
    else if (!strcmp(argv[i], "--lock")) lock = v; else if (!strcmp(argv[i], "--vib-hz")) vib_hz = v;
    else if (!strcmp(argv[i], "--vib-cents")) vib_cents = v; else if (!strcmp(argv[i], "--sustain-db")) sustain_db = v;
    else if (!strcmp(argv[i], "--gap-ms")) gap_ms = v; else if (!strcmp(argv[i], "--hold-ms")) hold_ms = v;
    else if (!strcmp(argv[i], "--presence-db")) presence_db = v; else if (!strcmp(argv[i], "--legato-ms")) legato_ms = v;
    else if (!strcmp(argv[i], "--shimmer")) shimmer = v; else if (!strcmp(argv[i], "--consonant-gain")) cgain = v;
    else if (!strcmp(argv[i], "--to16")) to16 = v;
  }
  int n, fs; double *x = load_wav(argv[1], &n, &fs); if (!x) return 1;
  singer *S = singer_create(x, n, fs);
  singer_params *P = singer_params_ptr(S);
  P->mode = SINGER_SCORE; P->bpm = bpm; P->f0_floor = f0_floor; P->lock = lock; P->vib_hz = vib_hz; P->vib_cents = vib_cents;
  P->sustain_db = sustain_db; P->gap_ms = gap_ms; P->hold_ms = hold_ms; P->presence_db = presence_db;
  P->legato_ms = legato_ms; P->shimmer_frames = shimmer; P->consonant_gain = cgain; P->morph = 1.0; P->depth = 0.0;
  static singer_word W[SINGER_MAX_WORDS]; int nw = load_words(argv[2], W, SINGER_MAX_WORDS);
  singer_set_words(S, W, nw);
  static singer_note N[SINGER_MAX_NOTES]; int nn = 0; double last16 = 0;
  FILE *sc = fopen(argv[3], "r"); if (!sc) { fprintf(stderr, "cannot open %s\n", argv[3]); return 1; }
  while (nn < SINGER_MAX_NOTES && fscanf(sc, "%lf %lf %lf", &N[nn].midi, &N[nn].at16, &N[nn].dur16) == 3) {
    if (N[nn].at16 + N[nn].dur16 > last16) last16 = N[nn].at16 + N[nn].dur16; nn++;
  }
  fclose(sc); singer_set_score(S, N, nn);
  int nsyl = 0; for (int i = 0; i < nw; i++) nsyl += W[i].nsyl > 1 ? W[i].nsyl : 1;
  fprintf(stderr, "→ %d words / %d syllables, %d notes · lock %.3f vib %.1f/%.0f sustain %.0f dB gap %.0f ms\n", nw, nsyl, nn, lock, vib_hz, vib_cents, sustain_db, gap_ms);
  singer_analyze_chunk(S, 0, singer_total_frames(S));
  if (to16 < 0) to16 = ceil(last16) + 4;
  int len = 0, used = 0; double *out = singer_render_score(S, 0, to16, &len, &used);
  fprintf(stderr, "→ rendered %d/%d notes, %.2fs\n", used, nn, (double)len / fs);
  FILE *o = fopen(argv[4], "wb"); int dlen = len * 2, hdr = 36 + dlen;
  fwrite("RIFF", 1, 4, o); fwrite(&hdr, 4, 1, o); fwrite("WAVE", 1, 4, o); fwrite("fmt ", 1, 4, o);
  int sixteen = 16; short one = 1, mono = 1, bps = 16; int byterate = fs * 2; short align = 2;
  fwrite(&sixteen, 4, 1, o); fwrite(&one, 2, 1, o); fwrite(&mono, 2, 1, o); fwrite(&fs, 4, 1, o); fwrite(&byterate, 4, 1, o);
  fwrite(&align, 2, 1, o); fwrite(&bps, 2, 1, o); fwrite("data", 1, 4, o); fwrite(&dlen, 4, 1, o);
  for (int i = 0; i < len; i++) { double v = out[i]; if (v > 1) v = 1; if (v < -1) v = -1; short sv = (short)lround(v * 32767); fwrite(&sv, 2, 1, o); }
  fclose(o); free(out); singer_destroy(S); free(x); return 0;
}
