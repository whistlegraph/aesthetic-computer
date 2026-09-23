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
#include <stdio.h>

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
  singer_note notes[SINGER_MAX_NOTES];
  int nnotes;

  singer_articulation articulation[SINGER_MAX_NOTES];
  int narticulation;

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
  co.f0_floor = 50.0;   // sizes the FFT for the lowest floor any profile asks (a 55 Hz floor overflowed a 71 Hz-sized FFT → NaN → silence)
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
  p->sustain_db = 0.0;   // 0 = stretch the whole nucleus (the pre-Sept-21 behaviour)
  p->gap_ms = 0.0;       // 0 = legato right up to the next onset
  p->shimmer_frames = 0.0; p->legato_ms = 0.0;
  p->presence_db = 0.0;  // 0 = no consonant-band lift
  p->voiced_consonant_mix = 0.0;   // 0 = voiced consonants fully vocoded
  p->hold_ms = 0.0;      // 0 = a vowel may fill its whole note
  p->consonant_balance = 0.0;   // 0 = match the consonant region (old); 1 = keep the talker's consonant-to-vowel ratio
  p->consonant_stretch = 1.0;   // 1 = consonants at speaking rate
  p->sustain_band = 0;   // 0 = total energy picks the sustain zone; 1 = the 400 Hz–4 kHz formant band
  p->loop_sustain = 0;   // 0 = frozen-spectrum hold (eased); 1 = wander the nucleus at speaking rate
  p->mode = SINGER_SNAP; p->root_pc = 9;              // A
  int sc[5] = {0, 3, 5, 7, 10};                        // minor pentatonic
  memcpy(p->scale, sc, sizeof(sc)); p->n_scale = 5;
  int pat[8] = {2, 2, 2, 2, 2, 2, 2, 2};               // eighths
  memcpy(p->pattern, pat, sizeof(pat)); p->n_pattern = 8;
  p->lock = 0.875; p->vib_hz = 5.0; p->vib_cents = 18.0;  // spinging defaults
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

// ═══ sung units ═══════════════════════════════════════════════════════════
// A unit is one thing that gets a note: a word (SNAP/MELODY) or a syllable
// (SCORE). `grid` is where its vowel lands, `slot` how long it has, both in
// frames from the window start. `tgt` < 0 means "no target: snap to scale".
typedef struct { int a, b, vs, ve; double grid, slot, tgt; } unit_t;

// Collect the voiced + energetic runs inside [a,b) — the candidate nuclei.
static int runs_in(singer *s, int a, int b, int *rs, int *re, int max) {
  if (b <= a) return 0;
  double emax = -1e18, emin = 1e18;
  for (int i = a; i < b; i++) { if (s->en[i] > emax) emax = s->en[i]; if (s->en[i] < emin) emin = s->en[i]; }
  double thr = emin + 0.55 * (emax - emin);
  int n = 0, run = -1;
  for (int i = a; i <= b; i++) {
    int on = (i < b) && (s->f0[i] > 0) && (s->en[i] > thr);
    if (on) { if (run < 0) run = i; }
    else if (run >= 0) { if (n < max) { rs[n] = run; re[n] = i; n++; } run = -1; }
  }
  return n;
}

// Split word [a,b) into k syllable spans with their own nuclei: the k longest
// runs in time order; the cut between two nuclei falls on the quietest frame
// between them, so the consonants go to whichever side they lean.
static int split_syllables(singer *s, int a, int b, int k, unit_t *U) {
  int rs[64], re[64];
  int n = runs_in(s, a, b, rs, re, 64);
  if (k < 1) k = 1;
  if (n < k) {                                   // not enough nuclei: split evenly
    for (int i = 0; i < k; i++) {
      U[i].a = a + (int)((long long)(b - a) * i / k);
      U[i].b = a + (int)((long long)(b - a) * (i + 1) / k);
      nucleus(s, U[i].a, U[i].b, &U[i].vs, &U[i].ve);
    }
    return k;
  }
  // keep the k longest
  int keep[64]; for (int i = 0; i < n; i++) keep[i] = 0;
  for (int c = 0; c < k; c++) {
    int best = -1;
    for (int i = 0; i < n; i++) if (!keep[i] && (best < 0 || re[i] - rs[i] > re[best] - rs[best])) best = i;
    keep[best] = 1;
  }
  int m = 0;
  for (int i = 0; i < n; i++) if (keep[i]) { U[m].vs = rs[i]; U[m].ve = re[i]; m++; }
  for (int i = 0; i < m; i++) {
    if (i == 0) U[i].a = a;
    else {
      int lo = U[i - 1].ve, hi = U[i].vs, cut = lo;
      for (int f = lo; f < hi; f++) if (s->en[f] < s->en[cut]) cut = f;
      U[i - 1].b = cut; U[i].a = cut;
    }
    if (i == m - 1) U[i].b = b;
  }
  return m;
}

// Place units on the grid, warp vowels, write the f0 line, synthesize, and
// composite the original consonants back in. Shared by both render paths.
static double *synth_units(singer *s, const unit_t *U, int nu, int total, singer_params p, int *out_len) {
  s->narticulation = 0;
  double fp = SINGER_FP_MS;
  int spec = s->fft_size / 2 + 1;

  double  *o_f0  = (double *)calloc(total, sizeof(double));
  double  *o_sf0 = (double *)calloc(total, sizeof(double));
  double **o_sp  = (double **)malloc(total * sizeof(double *));
  double **o_ap  = (double **)malloc(total * sizeof(double *));
  double  *o_src = (double *)malloc(total * sizeof(double));
  char    *o_c   = (char *)calloc(total, 1);
  int     *o_u   = (int *)malloc(total * sizeof(int));
  for (int i = 0; i < total; i++) {
    o_sp[i] = (double *)malloc(spec * sizeof(double));
    o_ap[i] = (double *)malloc(spec * sizeof(double));
    // Uncovered frames get a whisper-quiet FLOOR, not zero: WORLD takes the
    // log of the envelope, and log(0) turns the frame — and, through the
    // overlap-add, its neighbours — into NaN.
    for (int k = 0; k < spec; k++) { o_sp[i][k] = 1e-12; o_ap[i][k] = 1.0; }
    o_src[i] = -1.0; o_u[i] = -1;
  }

  // Sustain zone: only the LOUD core of the nucleus stretches. A voiced run
  // ("born" = b + or + n) is one nucleus to the detector, but stretching the
  // quieter voiced edges — a b burst, an n murmur, an l — turns them into
  // syllables of their own ("horn"). Frames more than `sustain_db` below the
  // run's peak stay at speaking rate as (vocoded) onset/coda transitions.
  double sustain_db = getenv("SINGER_SUSTAIN_DB") ? atof(getenv("SINGER_SUSTAIN_DB")) : p.sustain_db;
  int sustain_band = getenv("SINGER_SUSTAIN_BAND") ? atoi(getenv("SINGER_SUSTAIN_BAND")) : p.sustain_band;
  double sustain_min_frac = getenv("SINGER_SUSTAIN_MINFRAC") ? atof(getenv("SINGER_SUSTAIN_MINFRAC")) : 0.4;
  double sustain_min_ms   = getenv("SINGER_SUSTAIN_MINMS")   ? atof(getenv("SINGER_SUSTAIN_MINMS"))   : 60.0;
  double cgain_env = getenv("SINGER_CGAIN") ? atof(getenv("SINGER_CGAIN")) : 0;
  if (cgain_env > 0) p.consonant_gain = cgain_env;
  double gap_fr = (getenv("SINGER_GAP_MS") ? atof(getenv("SINGER_GAP_MS")) : p.gap_ms) / fp;
  double hold_fr = (getenv("SINGER_HOLD_MS") ? atof(getenv("SINGER_HOLD_MS")) : p.hold_ms) / fp;
  double cstretch = getenv("SINGER_CSTRETCH") ? atof(getenv("SINGER_CSTRETCH")) : p.consonant_stretch;
  if (cstretch < 1.0) cstretch = 1.0;
  double cmix = getenv("SINGER_CMIX") ? atof(getenv("SINGER_CMIX")) : p.voiced_consonant_mix;
  int loop_sustain = getenv("SINGER_LOOP") ? atoi(getenv("SINGER_LOOP")) : p.loop_sustain;
  // Wannadash's sing.py: tiny movement through the spectral envelope keeps
  // a held vowel alive without moving its scored pitch. Opt-in for Chorus.
  double shimmer = getenv("SINGER_SHIMMER_FRAMES") ? atof(getenv("SINGER_SHIMMER_FRAMES")) : p.shimmer_frames;
  const int XF = getenv("SINGER_XF") ? atoi(getenv("SINGER_XF")) : 8;   // seam crossfade, frames (40 ms)
  for (int i = 0; i < nu; i++) {
    int a = U[i].a, b = U[i].b, vs = U[i].vs, ve = U[i].ve;
    if (sustain_db > 0 && ve - vs >= 6) {
      // Which energy tells a vowel from a voiced consonant? Not the total —
      // a d-murmur or an n hums as loud as the vowel below 400 Hz. The
      // formant band does: vowels carry 400 Hz–4 kHz, stops and nasals
      // barely. `sustain_band` measures the zone there.
      static double eb[65536];
      int k0 = (int)(400.0 * s->fft_size / s->fs), k1 = (int)(4000.0 * s->fft_size / s->fs);
      if (k1 > spec) k1 = spec;
      for (int f = vs; f < ve; f++) {
        if (sustain_band) { double e = 0; for (int k = k0; k < k1; k++) e += s->sp[f][k]; eb[f - vs] = log(e + 1e-10); }
        else eb[f - vs] = s->en[f];
      }
      int pk = vs;
      for (int f = vs; f < ve; f++) if (eb[f - vs] > eb[pk - vs]) pk = f;
      double floor_en = eb[pk - vs] - sustain_db * log(10.0) / 10.0;   // ln(power)
      int ss = pk, se = pk + 1;
      while (ss > vs && eb[ss - 1 - vs] >= floor_en) ss--;
      while (se < ve && eb[se - vs] >= floor_en) se++;
      // A core can't be a sliver: a 25 ms zone stretched 24x is one frozen
      // spectrum for a second (Allison's "Neo" → "n-n-n-neeel"). Widen it
      // around the peak to at least `sustain_min_frac` of the run and
      // `sustain_min_ms`, so trimming only removes long murmurs.
      int minlen = (int)lround(fmax((ve - vs) * sustain_min_frac, sustain_min_ms / fp));
      while (se - ss < minlen && (ss > vs || se < ve)) {
        int growLeft = ss > vs && (se >= ve || eb[ss - 1 - vs] >= eb[se - vs]);
        if (growLeft) ss--; else se++;
      }
      if (se - ss >= 4) { vs = ss; ve = se; }                        // keep ≥ 20 ms to sing on
    }
    int c_on = vs - a, c_co = b - ve;
    if (c_co > 36) { c_co = 36; b = ve + 36; }   // a coda is a release, not the silence after it (≤ 180 ms)
    if (c_on > 60) { a = vs - 60; c_on = 60; }   // likewise an onset (≤ 300 ms)
    int vlen = ve - vs; if (vlen < 1) vlen = 1;
    // Consonants keep speaking rate by default. But a 20 ms stop burst
    // between two two-second vowels is nothing to hear, and clear speech
    // works the other way: talkers lengthen consonants and hold closures
    // when they need to be understood. `cstretch` > 1 slows the onset and
    // coda regions (the original audio rides along through the composite).
    int c_on_out = (int)lround(c_on * cstretch); if (c_on && c_on_out < 1) c_on_out = 1;
    int c_co_out = (int)lround(c_co * cstretch); if (c_co && c_co_out < 1) c_co_out = 1;

    // The vowel may fill its note, and — legato — run up to the next unit's
    // consonant onset when the notes touch; it never sings through a REST.
    // (Filling to the next onset regardless held "night" through the two
    // beats of silence after it.)
    double end = U[i].grid + U[i].slot;
    if (i + 1 < nu) {
      // stop `gap_fr` before the next onset consonant so a stop's closure
      // (the silence before a b/d/k burst) is silence, not the held vowel
      double nextOn = U[i + 1].grid - (double)(U[i + 1].vs - U[i + 1].a) * cstretch * p.morph - gap_fr;
      if (nextOn < end) end = nextOn;
    }
    double avail = end - U[i].grid - c_co_out - (i + 1 < nu ? 0.0 : c_on_out);
    if (avail < 1) avail = 1;
    // The vowel fills what the note gives it: a long note sustains (frames
    // are interpolated through the nucleus, never tiled), a note shorter
    // than the spoken word compresses — down to half speed before we let
    // it overrun. Six-times was too low a ceiling: a two-beat "house" went
    // silent after 600 ms and read as cut off.
    // A singer does not hold one vowel for the whole of a long note and then
    // stop — they hold it, release it, and leave air. A 3-beat cadence at 100
    // bpm is 1.8 s, and 1.8 s of one neural voice's vowel reads as a drone
    // with its consonant lost ("we were here" came back as "weeeeeere").
    // `hold_ms` caps how long the vowel may sound; the coda still lands at
    // the end of what is sung, and the rest of the note is air.
    if (hold_fr > 0 && avail > hold_fr) avail = hold_fr;
    double full = avail / vlen;
    if (full > 24.0) full = 24.0;
    if (full < 0.5) full = 0.5;
    double st = 1.0 + (full - 1.0) * p.morph;
    int vout = (int)lround(vlen * st); if (vout < 1) vout = 1;

    int wlen = c_on_out + vout + c_co_out;
    int o0 = (int)lround(U[i].grid - c_on_out * p.morph);
    if (s->narticulation < SINGER_MAX_NOTES) {
      const double sec = SINGER_FP_MS / 1000.0;
      singer_articulation *aout = &s->articulation[s->narticulation++];
      aout->start = clampi(o0, 0, total) * sec;
      aout->vowel_start = clampi(o0 + c_on_out, 0, total) * sec;
      aout->vowel_end = clampi(o0 + c_on_out + vout, 0, total) * sec;
      aout->end = clampi(o0 + wlen, 0, total) * sec;
    }
    if (getenv("SINGER_TRACE")) {
      double nextOn = i + 1 < nu ? U[i + 1].grid - (double)(U[i + 1].vs - U[i + 1].a) * cstretch * p.morph : -1;
      fprintf(stderr, "  [unit %2d] src a=%d vs=%d ve=%d b=%d (on %d vow %d coda %d fr) → grid %.0f slot %.0f | avail %.0f full %.2f st %.2f vout %d | out %d..%d nextOn %.0f%s\n",
              i, a, vs, ve, b, c_on, vlen, c_co, U[i].grid, U[i].slot, avail, full, st, vout,
              o0, o0 + wlen, nextOn, (nextOn >= 0 && o0 + wlen > nextOn) ? "  OVERRUN" : "");
    }
    for (int j = 0; j < wlen; j++) {
      int o = o0 + j;
      if (o < 0 || o >= total) continue;
      double srcf; int isc;
      if (j < c_on_out)       { srcf = a + (double)j / cstretch; isc = 1; }
      else if (j < c_on_out+vout) {
        double u = (double)(j - c_on_out) / (vout > 1 ? vout - 1 : 1);
        if (loop_sustain && st > 2.0) {
          // LOOPED sustain: a held note is not one frozen spectrum. Play the
          // onset third of the nucleus once at speaking rate, then wander the
          // middle of the nucleus back and forth at speaking rate (its own
          // breath, jitter and formant drift come along), and finish with the
          // off-glide at speaking rate — "naaaa-it" with a living "aaaa".
          double head = 0.30 * (vlen - 1), tail = 0.25 * (vlen - 1);
          int jj = j - c_on_out;
          int headFr = (int)lround(head), tailFr = (int)lround(tail);
          if (jj < headFr) srcf = vs + jj;
          else if (jj >= vout - tailFr) srcf = vs + (vlen - 1) - (vout - 1 - jj);
          else {
            double lo = vs + head, span = (vlen - 1) - head - tail;
            if (span < 4) span = 4;
            double t = (double)(jj - headFr), per = 2 * span;
            double ph = fmod(t, per);
            srcf = lo + (ph <= span ? ph : per - ph);           // ping-pong, no seam
          }
        } else {
          // A singer holds the vowel's FIRST colour and saves the glide for
          // the end ("naaaa-it", not "naaiiii"): when the vowel is stretched
          // beyond 2x, ease the source position so most of the output time
          // sits on the early nucleus and the off-glide happens late.
          double gamma = st > 2.0 ? 2.2 : 1.0;
          srcf = vs + pow(u, gamma) * (vlen - 1);
        }
        if (shimmer > 0 && st > 2.0) {
          srcf += shimmer * sin(2 * M_PI * 0.8 * j * fp / 1000.0) * sin(M_PI * u);
          srcf = fmax(vs, fmin(ve - 1, srcf));
        }
        isc = 0;
      }
      else                    { srcf = ve + (double)(j - c_on_out - vout) / cstretch; isc = 1; }
      int l = clampi((int)floor(srcf), 0, s->nframes - 1);
      int hh = clampi(l + 1, 0, s->nframes - 1);
      double fr = srcf - l;
      double nf0 = (s->f0[l] > 0) ? s->f0[l] : 0.0;
      int owned = o_u[o] >= 0 && o_u[o] != i;
      double w = (owned && j < XF) ? (double)(j + 1) / (XF + 1) : 1.0;   // fade the newcomer in over the old
      for (int k = 0; k < spec; k++) {
        double nsp = s->sp[l][k] * (1 - fr) + s->sp[hh][k] * fr;
        double nap = s->ap[l][k] * (1 - fr) + s->ap[hh][k] * fr;
        o_sp[o][k] = owned ? o_sp[o][k] * (1 - w) + nsp * w : nsp;
        o_ap[o][k] = owned ? o_ap[o][k] * (1 - w) + nap * w : nap;
      }
      if (owned && w < 1.0 && o_sf0[o] > 0 && nf0 > 0) nf0 = exp(log(o_sf0[o]) * (1 - w) + log(nf0) * w);
      o_src[o] = srcf; o_c[o] = (char)isc;
      if (!owned || w >= 0.5) o_u[o] = i;        // the newcomer owns the frame once it dominates
      o_sf0[o] = nf0;
    }
  }

  // Per-unit mean spoken pitch, so SCORE mode can keep a fraction of the
  // speaker's own contour AROUND the note instead of on top of it.
  double *umean = (double *)calloc(nu > 0 ? nu : 1, sizeof(double));
  int    *ucnt  = (int *)calloc(nu > 0 ? nu : 1, sizeof(int));
  int    *uon   = (int *)malloc((nu > 0 ? nu : 1) * sizeof(int));
  for (int i = 0; i < nu; i++) uon[i] = -1;
  for (int o = 0; o < total; o++) {
    if (o_u[o] < 0 || o_sf0[o] <= 0) continue;
    int u = o_u[o];
    umean[u] += 69.0 + 12.0 * log2(o_sf0[o] / 440.0); ucnt[u]++;
    if (uon[u] < 0) uon[u] = o;
  }
  for (int i = 0; i < nu; i++) if (ucnt[i]) umean[i] /= ucnt[i];

  for (int o = 0; o < total; o++) {
    if (o_sf0[o] <= 0) { o_f0[o] = 0; continue; }
    double midi = 69.0 + 12.0 * log2(o_sf0[o] / 440.0);
    double tgt_m;
    int u = o_u[o];
    if (u >= 0 && U[u].tgt >= 0) {
      // Consonants can begin before their syllable's vowel/downbeat. The
      // speech fragment owning a frame must not move the melody early:
      // while a written note is active, its pitch owns the score clock.
      int pitch_u = u;
      if (p.mode == SINGER_SCORE) {
        for (int j = nu - 1; j >= 0; j--) {
          if (o >= U[j].grid && o < U[j].grid + U[j].slot) {
            pitch_u = j;
            break;
          }
        }
      }
      // SCORE: the note, plus (1-lock) of the spoken contour around its mean,
      // plus the profile's vibrato easing in over the first 150 ms.
      tgt_m = U[pitch_u].tgt + (midi - umean[u]) * (1.0 - p.lock);
      double tv = (o - uon[u]) * fp / 1000.0;
      double ramp = tv < 0.15 ? tv / 0.15 : 1.0;
      tgt_m += (p.vib_cents / 100.0) * ramp * sin(2 * M_PI * p.vib_hz * tv);
    } else {
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
      tgt_m = (p.mode == SINGER_MELODY) ? snapped : midi + (snapped - midi) * p.snap;
    }
    o_f0[o] = exp((1 - p.morph) * log(o_sf0[o]) + p.morph * log(note_hz_midi(tgt_m)));
  }
  free(umean); free(ucnt); free(uon);

  // Short log-frequency transitions, as in pop/cult/bin/sing.py.
  // No interpolation across breath/rest frames and no change to note centers.
  double legato_ms = getenv("SINGER_LEGATO_MS") ? atof(getenv("SINGER_LEGATO_MS")) : p.legato_ms;
  int radius = clampi((int)lround(legato_ms / fp), 0, 12);
  if (radius > 0) {
    double *smooth = (double *)calloc(total, sizeof(double));
    for (int o = 0; o < total; o++) {
      if (o_f0[o] <= 0) continue;
      double sum = log(o_f0[o]), weights = 1;
      for (int dir = -1; dir <= 1; dir += 2) {
        for (int k = 1; k <= radius; k++) {
          int j = o + dir * k;
          if (j < 0 || j >= total || o_f0[j] <= 0) break;
          double w = 0.5 * (1 + cos(M_PI * k / (radius + 1)));
          sum += w * log(o_f0[j]); weights += w;
        }
      }
      smooth[o] = exp(sum / weights);
    }
    memcpy(o_f0, smooth, total * sizeof(double)); free(smooth);
  }

  int ylen = (int)(total * fp / 1000.0 * s->fs);
  double *y = (double *)calloc(ylen, sizeof(double));
  Synthesis(o_f0, total, (const double * const *)o_sp,
            (const double * const *)o_ap, s->fft_size, fp, s->fs, ylen, y);
  for (int n = 0; n < ylen; n++) if (!isfinite(y[n])) y[n] = 0.0;   // never let one NaN reach the gain below
  if (getenv("SINGER_DEBUG")) {
    int nv = 0, nf = 0, nfill = 0, nnan = 0; double ypk = 0;
    for (int i = 0; i < s->analyzed; i++) if (s->f0[i] > 0) nv++;
    for (int o = 0; o < total; o++) { if (o_u[o] >= 0) nfill++; if (o_sf0[o] > 0) nf++; }
    for (int n = 0; n < ylen; n++) { if (!isfinite(y[n])) nnan++; else if (fabs(y[n]) > ypk) ypk = fabs(y[n]); }
    fprintf(stderr, "  [core] src voiced %d/%d frames · out filled %d/%d · out voiced %d · synth peak %.3f · nan %d/%d · fft %d\n",
            nv, s->analyzed, nfill, total, nf, ypk, nnan, ylen, s->fft_size);
    if (nu) fprintf(stderr, "  [core] unit0 a=%d b=%d vs=%d ve=%d grid=%.1f slot=%.1f tgt=%.1f\n", U[0].a, U[0].b, U[0].vs, U[0].ve, U[0].grid, U[0].slot, U[0].tgt);
  }

  double spf = s->fs * fp / 1000.0;
  double *mask = (double *)calloc(ylen, sizeof(double));
  double *orig = (double *)calloc(ylen, sizeof(double));
  for (int n = 0; n < ylen; n++) {
    double fpos = n / spf;
    int fl = clampi((int)floor(fpos), 0, total - 1);
    int fh = clampi(fl + 1, 0, total - 1);
    if (o_src[fl] < 0 || o_src[fh] < 0) continue;
    if (!o_c[fl] || !o_c[fh]) continue;                 // a consonant region (onset or coda, natural rate)
    int unvoiced = (o_sf0[fl] <= 0) && (o_sf0[fh] <= 0);
    // Unvoiced consonants are always the original recording (the vocoder
    // has nothing to say about a burst or a hiss). Voiced consonants — b d
    // g m n l r w — are vocoded at the note by default; `cmix` blends the
    // spoken original back in over them too: their short natural-pitch
    // moment is what tells "door" from "your" and "beat" from "eat".
    double m = unvoiced ? 1.0 : cmix;
    if (m <= 0) continue;
    double ff = fpos - fl;
    double ss = (o_src[fl] * (1 - ff) + o_src[fh] * ff) * spf;
    int sl = clampi((int)floor(ss), 0, s->nx - 1);
    int sh = clampi(sl + 1, 0, s->nx - 1);
    double sfr = ss - sl;
    orig[n] = s->x[sl] * (1 - sfr) + s->x[sh] * sfr;
    mask[n] = m;
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
  // How loud should a spliced-in consonant be? The old answer matched the
  // vocoded output's own level INSIDE the consonant region — but there the
  // vocoder has almost nothing to say (an unvoiced frame has no f0, so it
  // synthesises near-silence), so the match drove real consonants down and
  // `consonant_gain` was left to shove them back up by ear.
  // `cbalance` asks the better question: what was this consonant's level
  // NEXT TO ITS OWN VOWEL in the speech we started from? Match the vocoded
  // vowel's level and keep that ratio, and the words arrive at the balance
  // the talker gave them.
  double cbalance = getenv("SINGER_CBALANCE") ? atof(getenv("SINGER_CBALANCE")) : p.consonant_balance;
  double rw = 0, ro = 0; int cnt = 0;
  for (int n = 0; n < ylen; n++) if (mask[n] > 0.5) { rw += y[n]*y[n]; ro += orig[n]*orig[n]; cnt++; }
  double g = (cnt && ro > 0) ? sqrt(rw / cnt) / sqrt((ro / cnt) + 1e-12) : 1.0;
  if (cbalance > 0) {
    // the vowel regions: filled output frames the composite does NOT cover
    double vw = 0, vo = 0; int vc = 0;
    for (int n = 0; n < ylen; n++) {
      if (mask[n] > 0.5) continue;
      double fpos = n / spf;
      int fl = clampi((int)floor(fpos), 0, total - 1);
      if (o_u[fl] < 0 || o_src[fl] < 0) continue;
      double ss = o_src[fl] * spf;
      int sl = clampi((int)ss, 0, s->nx - 1);
      vw += y[n]*y[n]; vo += s->x[sl]*s->x[sl]; vc++;
    }
    if (vc && vo > 0) {
      double gv = sqrt(vw / vc) / sqrt((vo / vc) + 1e-12);
      g = g * (1 - cbalance) + gv * cbalance;
    }
  }

  double pk = 0;
  for (int n = 0; n < ylen; n++) {
    double v = (1 - msm[n]) * y[n] + msm[n] * orig[n] * g * p.consonant_gain;
    if (!isfinite(v)) v = 0.0;
    y[n] = v;
    if (fabs(v) > pk) pk = fabs(v);
  }
  // Presence: a peaking EQ around 3 kHz (RBJ biquad, Q 0.8) lifts the
  // consonant band that a vocoded, pitch-locked vowel line buries. 0 = off.
  double presence_db = getenv("SINGER_PRESENCE_DB") ? atof(getenv("SINGER_PRESENCE_DB")) : p.presence_db;
  if (presence_db != 0) {
    double A = pow(10.0, presence_db / 40.0), w0 = 2 * M_PI * 3000.0 / s->fs, Q = 0.8;
    double alpha = sin(w0) / (2 * Q), cw = cos(w0);
    double b0 = 1 + alpha * A, b1 = -2 * cw, b2 = 1 - alpha * A;
    double a0 = 1 + alpha / A, a1 = -2 * cw, a2 = 1 - alpha / A;
    b0 /= a0; b1 /= a0; b2 /= a0; a1 /= a0; a2 /= a0;
    double x1 = 0, x2 = 0, y1 = 0, y2 = 0;
    pk = 0;
    for (int n = 0; n < ylen; n++) {
      double x0 = y[n], v = b0 * x0 + b1 * x1 + b2 * x2 - a1 * y1 - a2 * y2;
      x2 = x1; x1 = x0; y2 = y1; y1 = v;
      if (!isfinite(v)) v = 0.0;
      y[n] = v; if (fabs(v) > pk) pk = fabs(v);
    }
  }
  if (!isfinite(pk) || pk <= 0) pk = 1.0;
  for (int n = 0; n < ylen; n++) y[n] = y[n] / pk * 0.8;

  for (int i = 0; i < total; i++) { free(o_sp[i]); free(o_ap[i]); }
  free(o_sp); free(o_ap); free(o_f0); free(o_sf0); free(o_src); free(o_c); free(o_u);
  free(mask); free(orig); free(msm);
  *out_len = ylen;
  return y;
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
  static unit_t U[512];
  int   nw = 0; double cur = 0; int pi = 0; int w = word_start;
  while (cur < target16 && nw < 500 && w < ready) {
    int d16 = p.pattern[pi % p.n_pattern]; pi++;
    if (d16 < 0) { cur += -d16; continue; }              // rest
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
    U[nw].a = s->words[w].a; U[nw].b = s->words[w].b;
    nucleus(s, U[nw].a, U[nw].b, &U[nw].vs, &U[nw].ve);
    U[nw].grid = cur * sixt; U[nw].slot = steps * sixt; U[nw].tgt = -1;
    cur += steps; nw++; w++;
  }
  if (nw == 0) { *out_len = 0; *words_used = 0; return NULL; }

  int total = (int)lround(target16 * sixt);      // EXACTLY `bars` bars
  double *y = synth_units(s, U, nw, total, p, out_len);
  *words_used = nw;
  return y;
}

// ═══ SCORE mode ═══════════════════════════════════════════════════════════
void singer_set_score(singer *s, const singer_note *n, int count) {
  count = count > SINGER_MAX_NOTES ? SINGER_MAX_NOTES : count;
  memcpy(s->notes, n, count * sizeof(singer_note));
  s->nnotes = count;
}
int singer_note_count(const singer *s) { return s->nnotes; }
int singer_articulation_count(const singer *s) { return s->narticulation; }
const singer_articulation *singer_articulations(const singer *s) { return s->articulation; }

double *singer_render_score(singer *s, double from16, double to16,
                            int *out_len, int *notes_used) {
  s->narticulation = 0;
  singer_params p = s->p;
  double fp = SINGER_FP_MS;
  double sixt = (60000.0 / p.bpm) / 4.0 / fp;
  int total = (int)lround((to16 - from16) * sixt);
  *notes_used = 0;
  if (total <= 0) { *out_len = 0; return NULL; }

  // Walk words → syllables → notes, keeping the units whose onset is inside
  // the window. Stop at the first unit whose speech is not analyzed yet.
  static unit_t U[512];
  int nu = 0, ni = 0;
  for (int w = 0; w < s->nwords && ni < s->nnotes && nu < 500; w++) {
    int k = s->words[w].nsyl > 1 ? s->words[w].nsyl : 1;
    if (ni + k > s->nnotes) k = s->nnotes - ni;
    if (s->words[w].b > s->analyzed) break;    // not yet analyzed (a word may END at the watermark)
    unit_t S[64]; int m;
    if (s->words[w].fixed && k == 1) {           // measured nucleus: use it as given
      S[0].a = s->words[w].a; S[0].b = s->words[w].b; S[0].vs = s->words[w].vs; S[0].ve = s->words[w].ve;
      if (S[0].ve <= S[0].vs) nucleus(s, S[0].a, S[0].b, &S[0].vs, &S[0].ve);
      m = 1;
    } else m = split_syllables(s, s->words[w].a, s->words[w].b, k > 64 ? 64 : k, S);
    for (int i = 0; i < k; i++, ni++) {
      const singer_note *N = &s->notes[ni];
      if (N->at16 < from16 || N->at16 >= to16) continue;
      unit_t u = S[i < m ? i : m - 1];
      u.grid = (N->at16 - from16) * sixt;
      u.slot = N->dur16 * sixt;
      u.tgt  = N->midi;
      U[nu++] = u;
      (*notes_used)++;
    }
  }
  if (nu == 0) {
    // silence for the window, so the caller's clock still advances
    int ylen = (int)(total * fp / 1000.0 * s->fs);
    *out_len = ylen;
    return (double *)calloc(ylen, sizeof(double));
  }
  return synth_units(s, U, nu, total, p, out_len);
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
