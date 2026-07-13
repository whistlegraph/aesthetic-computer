// singer.c — speech-to-singing core in C. See singer.h for the contract.
//
// Ports the Python engine: WORLD f0-replacement + vowel-aware time warp +
// original-audio consonant composite, plus a sine bed and a vectorised FDN
// reverb — but with a realtime-safe audio path (no GIL, no GC, no allocation).
//
// The speed win is NOT from being C (libworld is the same C either way).
// It is from CHUNKED ANALYSIS: we sound the first phrase after analyzing a
// few seconds, and stream the rest of the corpus in behind the playhead.

#include "singer.h"
#include "world/harvest.h"
#include "world/dio.h"
#include "world/stonemask.h"
#include "world/cheaptrick.h"
#include "world/d4c.h"
#include "world/synthesis.h"

#include <stdlib.h>
#include <string.h>
#include <math.h>

#define NCOMB 4
static const int COMB_LEN[NCOMB] = {1687, 1601, 2053, 2251};

typedef struct { double *buf; int len; int pos; int active; } voice_t;

struct singer {
  // source
  const double *x; int nx; int fs;

  // WORLD streams, allocated for the WHOLE source, filled incrementally
  double *f0, *tpos;
  double **sp, **ap;
  int nframes, fft_size;
  int analyzed;                 // watermark: frames [0, analyzed) are valid

  // energy envelope (for vowel nuclei), filled alongside
  double *en;

  singer_word words[SINGER_MAX_WORDS];
  int nwords;
  int phrase_len;               // words per phrase

  singer_params p;

  // ── audio-thread state (no allocation past here) ──────────────────────
  volatile int playing;
  long long pos;                // the sample clock
  voice_t cur;                  // currently sounding phrase
  double *q[2]; int qlen[2]; volatile int qw, qr;   // 2-deep ready queue

  double comb[NCOMB][2304];     // reverb delay lines (>= max COMB_LEN)
  int    comb_i[NCOMB];
  double comb_lp[NCOMB];
  double vlp;                   // vocal lowpass state
};

// ═══ helpers ══════════════════════════════════════════════════════════════
static double note_hz_midi(double m) { return 440.0 * pow(2.0, (m - 69.0) / 12.0); }
static int    clampi(int v, int lo, int hi) { return v < lo ? lo : (v > hi ? hi : v); }

singer *singer_create(const double *pcm, int n, int fs) {
  singer *s = (singer *)calloc(1, sizeof(singer));
  s->x = pcm; s->nx = n; s->fs = fs;
  s->nframes = (int)(n / (fs * SINGER_FP_MS / 1000.0)) + 1;

  CheapTrickOption co; InitializeCheapTrickOption(fs, &co);
  co.f0_floor = 71.0;                       // provisional; reset in analyze
  s->fft_size = GetFFTSizeForCheapTrick(fs, &co);

  s->f0   = (double *)calloc(s->nframes, sizeof(double));
  s->tpos = (double *)calloc(s->nframes, sizeof(double));
  s->en   = (double *)calloc(s->nframes, sizeof(double));
  s->sp   = (double **)calloc(s->nframes, sizeof(double *));
  s->ap   = (double **)calloc(s->nframes, sizeof(double *));
  int spec = s->fft_size / 2 + 1;
  for (int i = 0; i < s->nframes; i++) {
    s->sp[i] = (double *)calloc(spec, sizeof(double));
    s->ap[i] = (double *)calloc(spec, sizeof(double));
  }

  s->analyzed = 0;
  s->phrase_len = 10;

  // sane defaults
  singer_params *p = &s->p;
  p->bpm = 124; p->morph = 1.0; p->snap = 0.9; p->depth = 0.15;
  p->level = 2.4; p->f0_floor = 70.0; p->consonant_gain = 1.25;
  p->mode = SINGER_SNAP; p->root_pc = 9;              // A
  int sc[5] = {0, 3, 5, 7, 10};                        // minor pentatonic
  memcpy(p->scale, sc, sizeof(sc)); p->n_scale = 5;
  int pat[8] = {2, 2, 2, 2, 2, 2, 2, 2};               // eighths
  memcpy(p->pattern, pat, sizeof(pat)); p->n_pattern = 8;
  return s;
}

void singer_destroy(singer *s) {
  if (!s) return;
  for (int i = 0; i < s->nframes; i++) { free(s->sp[i]); free(s->ap[i]); }
  free(s->sp); free(s->ap); free(s->f0); free(s->tpos); free(s->en);
  if (s->cur.buf) free(s->cur.buf);
  for (int i = 0; i < 2; i++) if (s->q[i]) free(s->q[i]);
  free(s);
}

singer_params *singer_params_ptr(singer *s) { return &s->p; }
int singer_analyzed(const singer *s)     { return s->analyzed; }
int singer_total_frames(const singer *s) { return s->nframes; }
int singer_phrase_count(const singer *s) {
  return s->nwords ? (s->nwords + s->phrase_len - 1) / s->phrase_len : 0;
}

// ═══ analysis — BACKGROUND thread. Chunked: this is the whole trick. ══════
int singer_analyze_chunk(singer *s, int from, int to) {
  from = clampi(from, 0, s->nframes);
  to   = clampi(to,   0, s->nframes);
  if (to <= from) return 0;

  double fp = SINGER_FP_MS;
  // WORLD needs raw samples spanning the frames, plus padding for its windows.
  int pad   = (int)(s->fs * 0.05);
  int s0    = clampi((int)(from * fp / 1000.0 * s->fs) - pad, 0, s->nx);
  int s1    = clampi((int)(to   * fp / 1000.0 * s->fs) + pad, 0, s->nx);
  int seglen = s1 - s0;
  if (seglen < 512) return 0;
  const double *seg = s->x + s0;

  HarvestOption ho; InitializeHarvestOption(&ho);
  ho.f0_floor = s->p.f0_floor; ho.f0_ceil = 600.0; ho.frame_period = fp;
  int nf = GetSamplesForHarvest(s->fs, seglen, fp);
  double *tp  = (double *)malloc(nf * sizeof(double));
  double *f0r = (double *)malloc(nf * sizeof(double));
  double *f0  = (double *)malloc(nf * sizeof(double));
  Harvest(seg, seglen, s->fs, &ho, tp, f0r);
  StoneMask(seg, seglen, s->fs, tp, f0r, nf, f0);

  CheapTrickOption co; InitializeCheapTrickOption(s->fs, &co);
  co.f0_floor = s->p.f0_floor; co.fft_size = s->fft_size;
  D4COption d4o; InitializeD4COption(&d4o);

  int spec = s->fft_size / 2 + 1;
  double **sp = (double **)malloc(nf * sizeof(double *));
  double **ap = (double **)malloc(nf * sizeof(double *));
  for (int i = 0; i < nf; i++) {
    sp[i] = (double *)malloc(spec * sizeof(double));
    ap[i] = (double *)malloc(spec * sizeof(double));
  }
  CheapTrick(seg, seglen, s->fs, tp, f0, nf, &co, sp);
  D4C(seg, seglen, s->fs, tp, f0, nf, s->fft_size, &d4o, ap);

  // splice the chunk's frames into the global streams at the right offset
  double off_ms = (double)s0 / s->fs * 1000.0;
  for (int i = 0; i < nf; i++) {
    int g = (int)lround((off_ms + i * fp) / fp);   // global frame index
    if (g < from || g >= to || g >= s->nframes) continue;
    s->f0[g]   = f0[i];
    s->tpos[g] = tp[i] + off_ms / 1000.0;
    memcpy(s->sp[g], sp[i], spec * sizeof(double));
    memcpy(s->ap[g], ap[i], spec * sizeof(double));
    double e = 0; for (int k = 0; k < spec; k++) e += sp[i][k];
    s->en[g] = log(e + 1e-10);
  }

  for (int i = 0; i < nf; i++) { free(sp[i]); free(ap[i]); }
  free(sp); free(ap); free(tp); free(f0r); free(f0);

  if (to > s->analyzed) s->analyzed = to;
  return to - from;
}

// ═══ words + vowel nuclei ═════════════════════════════════════════════════
void singer_set_words(singer *s, const singer_word *w, int n) {
  n = n > SINGER_MAX_WORDS ? SINGER_MAX_WORDS : n;
  memcpy(s->words, w, n * sizeof(singer_word));
  s->nwords = n;
}

int singer_phrase_ready(const singer *s, int p) {
  int i0 = p * s->phrase_len;
  int i1 = i0 + s->phrase_len; if (i1 > s->nwords) i1 = s->nwords;
  if (i0 >= s->nwords) return 0;
  return s->words[i1 - 1].b < s->analyzed;
}

// Vowel nucleus = longest voiced + energetic run inside the word.
static void nucleus(singer *s, int a, int b, int *vs, int *ve) {
  *vs = a; *ve = a < b ? a + 1 : a;
  if (b <= a) return;
  double emax = -1e18, emin = 1e18;
  for (int i = a; i < b; i++) { if (s->en[i] > emax) emax = s->en[i]; if (s->en[i] < emin) emin = s->en[i]; }
  double thr = emin + 0.55 * (emax - emin);
  int best = 0, run = -1;
  for (int i = a; i <= b; i++) {
    int on = (i < b) && (s->f0[i] > 0) && (s->en[i] > thr);
    if (on) { if (run < 0) run = i; }
    else if (run >= 0) { if (i - run > best) { best = i - run; *vs = run; *ve = i; } run = -1; }
  }
}

// ═══ render a phrase — RENDER thread. ~30ms. ══════════════════════════════
double *singer_render_bars(singer *s, int word_start, int bars,
                           int *out_len, int *words_used) {
  singer_params p = s->p;
  double fp = SINGER_FP_MS;
  double sixt = (60000.0 / p.bpm) / 4.0 / fp;   // frames per 16th
  double bar16 = 16.0;                           // sixteenths in a bar
  double target16 = bars * bar16;                // fill EXACTLY this many 16ths

  int ready = singer_words_ready(s);
  if (word_start >= ready) { *out_len = 0; *words_used = 0; return NULL; }

  // ── deal words onto the pattern until we've filled `bars` bars ─────────
  // A long word CONSUMES EXTRA STEPS rather than being squashed into one.
  // (Crushing a 900ms word into a 242ms eighth is what "squashed" sounds like.)
  int   idx[512]; double grid[512]; int slot[512];
  int   nw = 0; double cur = 0; int pi = 0; int w = word_start;
  while (cur < target16 && nw < 500 && w < ready) {
    int d16 = p.pattern[pi % p.n_pattern]; pi++;
    if (d16 < 0) { cur += -d16; continue; }              // rest
    // how much room does this word actually need?
    double nat16 = (s->words[w].b - s->words[w].a) / sixt;
    int steps = d16;
    while (steps < nat16 * 0.85 && cur + steps < target16) {   // grow, don't squash
      int nxt = p.pattern[pi % p.n_pattern];
      if (nxt < 0) break;
      pi++; steps += nxt;
      if (steps >= 16) break;                            // cap at a bar
    }
    if (cur + steps > target16) steps = (int)(target16 - cur);
    if (steps <= 0) break;
    idx[nw] = w; grid[nw] = cur * sixt; slot[nw] = steps;
    cur += steps; nw++; w++;
  }
  if (nw == 0) { *out_len = 0; *words_used = 0; return NULL; }

  int total = (int)lround(target16 * sixt);      // EXACTLY `bars` bars
  int spec = s->fft_size / 2 + 1;

  double  *o_f0  = (double *)calloc(total, sizeof(double));
  double  *o_sf0 = (double *)calloc(total, sizeof(double));
  double **o_sp  = (double **)malloc(total * sizeof(double *));
  double **o_ap  = (double **)malloc(total * sizeof(double *));
  double  *o_src = (double *)malloc(total * sizeof(double));
  char    *o_c   = (char *)calloc(total, 1);
  for (int i = 0; i < total; i++) {
    o_sp[i] = (double *)calloc(spec, sizeof(double));
    o_ap[i] = (double *)malloc(spec * sizeof(double));
    for (int k = 0; k < spec; k++) o_ap[i][k] = 1.0;
    o_src[i] = -1.0;
  }

  for (int i = 0; i < nw; i++) {
    singer_word *W = &s->words[idx[i]];
    int a = W->a, b = W->b, vs, ve;
    nucleus(s, a, b, &vs, &ve);
    int c_on = vs - a, c_co = b - ve;
    int vlen = ve - vs; if (vlen < 1) vlen = 1;

    double avail;
    if (i + 1 < nw) {
      singer_word *N = &s->words[idx[i + 1]];
      int n_vs, n_ve; nucleus(s, N->a, N->b, &n_vs, &n_ve);
      avail = (grid[i + 1] - (double)(n_vs - N->a) * p.morph) - grid[i] - c_co;
    } else {
      avail = slot[i] * sixt - c_on - c_co;
    }
    if (avail < 1) avail = 1;
    double full = avail / vlen;
    if (full > 6.0) full = 6.0;
    if (full < 0.85) full = 0.85;              // NEVER squash past 15%
    double st = 1.0 + (full - 1.0) * p.morph;
    int vout = (int)lround(vlen * st); if (vout < 1) vout = 1;

    int wlen = c_on + vout + c_co;
    int o0 = (int)lround(grid[i] - c_on * p.morph);
    for (int j = 0; j < wlen; j++) {
      int o = o0 + j;
      if (o < 0 || o >= total) continue;
      double srcf; int isc;
      if (j < c_on)           { srcf = a + j; isc = 1; }
      else if (j < c_on+vout) { srcf = vs + (double)(j - c_on) * (vlen - 1) / (vout > 1 ? vout - 1 : 1); isc = 0; }
      else                    { srcf = ve + (j - c_on - vout); isc = 1; }
      int l = clampi((int)floor(srcf), 0, s->nframes - 1);
      int hh = clampi(l + 1, 0, s->nframes - 1);
      double fr = srcf - l;
      for (int k = 0; k < spec; k++) {
        o_sp[o][k] = s->sp[l][k] * (1 - fr) + s->sp[hh][k] * fr;
        o_ap[o][k] = s->ap[l][k] * (1 - fr) + s->ap[hh][k] * fr;
      }
      o_src[o] = srcf; o_c[o] = (char)isc;
      o_sf0[o] = (s->f0[l] > 0) ? s->f0[l] : 0.0;
    }
  }

  for (int o = 0; o < total; o++) {
    if (o_sf0[o] <= 0) { o_f0[o] = 0; continue; }
    double midi = 69.0 + 12.0 * log2(o_sf0[o] / 440.0);
    double pc = fmod(midi, 12.0); if (pc < 0) pc += 12;
    double bestd = 1e9;
    for (int k = 0; k < p.n_scale; k++) {
      double deg = fmod((double)(p.root_pc + p.scale[k]), 12.0);
      double d = pc - deg;
      while (d >  6) d -= 12;
      while (d < -6) d += 12;
      if (fabs(d) < fabs(bestd)) bestd = d;
    }
    double snapped = midi - bestd;
    double tgt_m = (p.mode == SINGER_MELODY) ? snapped : midi + (snapped - midi) * p.snap;
    o_f0[o] = exp((1 - p.morph) * log(o_sf0[o]) + p.morph * log(note_hz_midi(tgt_m)));
  }

  int ylen = (int)(total * fp / 1000.0 * s->fs);
  double *y = (double *)calloc(ylen, sizeof(double));
  Synthesis(o_f0, total, (const double * const *)o_sp,
            (const double * const *)o_ap, s->fft_size, fp, s->fs, ylen, y);

  double spf = s->fs * fp / 1000.0;
  double *mask = (double *)calloc(ylen, sizeof(double));
  double *orig = (double *)calloc(ylen, sizeof(double));
  for (int n = 0; n < ylen; n++) {
    double fpos = n / spf;
    int fl = clampi((int)floor(fpos), 0, total - 1);
    int fh = clampi(fl + 1, 0, total - 1);
    if (o_src[fl] < 0 || o_src[fh] < 0) continue;
    if (!(o_c[fl] && o_sf0[fl] <= 0) || !(o_c[fh] && o_sf0[fh] <= 0)) continue;
    double ff = fpos - fl;
    double ss = (o_src[fl] * (1 - ff) + o_src[fh] * ff) * spf;
    int sl = clampi((int)floor(ss), 0, s->nx - 1);
    int sh = clampi(sl + 1, 0, s->nx - 1);
    double sfr = ss - sl;
    orig[n] = s->x[sl] * (1 - sfr) + s->x[sh] * sfr;
    mask[n] = 1.0;
  }
  int xf = (int)(0.012 * s->fs);
  double *msm = (double *)calloc(ylen, sizeof(double));
  double wsum = 0;
  for (int k = -xf; k <= xf; k += 4) wsum += 0.5 * (1 - cos(M_PI * (k + xf) / (double)xf));
  for (int n = 0; n < ylen; n++) {
    double acc = 0;
    for (int k = -xf; k <= xf; k += 4) {
      int m = n + k; if (m < 0 || m >= ylen) continue;
      acc += mask[m] * 0.5 * (1 - cos(M_PI * (k + xf) / (double)xf));
    }
    double v = wsum > 0 ? acc / wsum : 0;
    msm[n] = v > 1 ? 1 : v;
  }
  double rw = 0, ro = 0; int cnt = 0;
  for (int n = 0; n < ylen; n++) if (mask[n] > 0.5) { rw += y[n]*y[n]; ro += orig[n]*orig[n]; cnt++; }
  double g = (cnt && ro > 0) ? sqrt(rw / cnt) / sqrt((ro / cnt) + 1e-12) : 1.0;

  double pk = 0;
  for (int n = 0; n < ylen; n++) {
    double v = (1 - msm[n]) * y[n] + msm[n] * orig[n] * g * p.consonant_gain;
    if (!isfinite(v)) v = 0.0;
    y[n] = v;
    if (fabs(v) > pk) pk = fabs(v);
  }
  if (!isfinite(pk) || pk <= 0) pk = 1.0;
  for (int n = 0; n < ylen; n++) y[n] = y[n] / pk * 0.8;

  for (int i = 0; i < total; i++) { free(o_sp[i]); free(o_ap[i]); }
  free(o_sp); free(o_ap); free(o_f0); free(o_sf0); free(o_src); free(o_c);
  free(mask); free(orig); free(msm);

  *out_len = ylen;
  *words_used = nw;
  return y;
}

int singer_word_count(const singer *s) { return s->nwords; }
int singer_words_ready(const singer *s) {
  int n = 0;
  for (int i = 0; i < s->nwords; i++) if (s->words[i].b < s->analyzed) n = i + 1; else break;
  return n;
}

// ═══ audio thread — REALTIME SAFE. no malloc, no locks. ═══════════════════
void singer_transport(singer *s, int playing) { s->playing = playing; }
long long singer_pos(const singer *s) { return s->pos; }

void singer_queue_phrase(singer *s, double *buf, int len) {
  int nxt = (s->qw + 1) % 2;
  if (nxt == s->qr) { free(buf); return; }       // full
  s->q[s->qw] = buf; s->qlen[s->qw] = len;
  s->qw = nxt;                                    // publish last
}
int singer_pending(const singer *s) { return s->qw != s->qr; }

void singer_audio_block(singer *s, float *out, int frames) {
  if (!s->playing) { for (int i = 0; i < frames; i++) out[i] = 0.f; return; }
  singer_params p = s->p;
  double spb = s->fs * 60.0 / p.bpm;             // samples per beat
  double barlen = spb * 4;
  long long pos = s->pos;

  (void)barlen;

  double d = p.depth;
  double a_lp = 1.0 - 0.72 * d;
  double duckk, wet_amt = 0.06 + 0.7 * d, dry_amt = 1.0 - 0.55 * d;

  for (int i = 0; i < frames; i++) {
    double t   = (double)(pos + i) / s->fs;
    double bt  = (double)(pos + i) / spb;
    double inb = bt - floor(bt);

    // KICK — sine pitch sweep
    double ke = exp(-inb * 9.0);
    double kf = 52.0 + 105.0 * exp(-inb * 26.0);
    double kick = sin(2 * M_PI * kf * inb * spb / s->fs) * ke * 0.92;

    // SUB / LEAD / PAD — sines
    double root = 55.0 * pow(2.0, p.root_pc / 12.0);
    int step = ((int)(bt / 4)) % 4;
    static const int prog[4] = {0, 0, 5, 7};
    double fsub = root * pow(2.0, prog[step] / 12.0);
    double sub  = sin(2 * M_PI * fsub * t) * 0.30;
    double gate = (inb > 0.5) ? exp(-(inb - 0.5) * 8.0) : 0.0;
    double lead = sin(2 * M_PI * fsub * 4 * t) * gate * 0.10;
    double pad  = (sin(2 * M_PI * fsub * 3 * t) + sin(2 * M_PI * fsub * 4 * t)) * 0.0175;
    duckk = 1.0 - 0.75 * exp(-inb * 7.0);
    double synth = (sub + lead + pad) * duckk;

    // vocal
    // THE FAUCET: phrases are exact whole bars, so the moment one ends the
    // next starts on the very next sample — continuous singing, no dead air.
    double v = 0;
    if (!s->cur.active && s->qr != s->qw) {
      if (s->cur.buf) { free(s->cur.buf); s->cur.buf = NULL; }
      s->cur.buf = s->q[s->qr]; s->cur.len = s->qlen[s->qr];
      s->qr = (s->qr + 1) % 2;
      s->cur.pos = 0; s->cur.active = 1;
    }
    if (s->cur.active) {
      if (s->cur.pos < s->cur.len) v = s->cur.buf[s->cur.pos++];
      else s->cur.active = 0;
    }
    s->vlp += a_lp * (v - s->vlp);
    double vd = (d < 0.02) ? v : s->vlp;

    double kick_env = exp(-inb * 7.0);
    double duck_v = 1.0 - (0.15 + 0.6 * d) * kick_env;
    double send = vd * duck_v * wet_amt;

    // FDN reverb — 4 damped combs
    double wet = 0;
    for (int c = 0; c < NCOMB; c++) {
      int L = COMB_LEN[c], idx = s->comb_i[c];
      double dv = s->comb[c][idx];
      s->comb_lp[c] = s->comb_lp[c] * 0.62 + dv * 0.38;
      wet += s->comb_lp[c];
      s->comb[c][idx] = send + s->comb_lp[c] * 0.76;
      s->comb_i[c] = (idx + 1) % L;
    }
    wet *= 0.25;

    double vocal = (vd * duck_v * dry_amt + wet) * p.level;
    double bed_duck = 1.0 - 0.35 * (1.0 - d) * fmin(1.0, fabs(vd) * 3.0);
    double mix = kick * 0.85 + synth * bed_duck + vocal;
    out[i] = (float)(tanh(mix * 0.88) * 0.92);
  }
  s->pos = pos + frames;
}
