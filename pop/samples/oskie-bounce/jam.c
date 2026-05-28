/* jam.c — C port of jam.mjs. Granular remix of oskie's bounce: the
 * drum break stays as rhythm, all other voices are granularly sampled
 * from bass.wav (and a few drum-stem grains for the drum-into-bell
 * morph). Chord progression in E major (I–V–vi–IV; chorus 2 uses
 * vi–IV–I–V). Heartbeat written to ~/.ac-pop-renders/ so the Slab
 * menubar shows progress.
 *
 * Build:   cc -O2 -Wall -Wno-unused-result jam.c -o jam -lm
 * Run:     ./jam   (must be cwd of the sample dir so stems/ resolves)
 *
 * Output:  oskie-jam.wav (16-bit stereo, 44.1 kHz, 84.00 s)
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <math.h>
#include <time.h>
#include <unistd.h>
#include <sys/stat.h>

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

/* ── timing + analyzed params ──────────────────────────────────────── */
#define SR             44100
#define BPM            101.33
#define BEAT_S         (60.0 / BPM)
#define BAR_S          (BEAT_S * 4.0)
#define LOOP_START_S   0.813
#define LOOP_BARS      4
#define LOOP_S         (LOOP_BARS * BAR_S)
#define TARGET_S       84.0
#define TOTAL_BARS     36
/* totalN = 36 bars + 2 s tail */
#define TOTAL_N        ((size_t)((double)TOTAL_BARS * BAR_S * SR) + (size_t)(2.0 * SR))
#define BASS_F0_MIDI   28

/* ── chord progression ─────────────────────────────────────────────── */
typedef struct { int root_pc; int third; int fifth; } chord_t;
static const chord_t CH_E  = { 4,  4, 7 };
static const chord_t CH_B  = { 11, 4, 7 };
static const chord_t CH_Cs = { 1,  3, 7 };
static const chord_t CH_A  = { 9,  4, 7 };
static const chord_t CHORDS_A[4] = { CH_E, CH_B, CH_Cs, CH_A };
static const chord_t CHORDS_B[4] = { CH_Cs, CH_A, CH_E, CH_B };

static inline int pc_to_midi(int pc, int octave) { return pc + 12 * (octave + 1); }
static inline int chord_root(chord_t ch, int octave) { return pc_to_midi(ch.root_pc, octave); }
static inline double midi_to_hz(int m) { return 440.0 * pow(2.0, (m - 69) / 12.0); }
static inline double rate_for_midi(int m) {
  return pow(2.0, (m - BASS_F0_MIDI) / 12.0);
}

/* ── deterministic xorshift32 RNG ──────────────────────────────────── */
static uint32_t rng_state = 0xbeef;
static double rand01(void) {
  uint32_t s = rng_state;
  s ^= s << 13;
  s ^= s >> 17;
  s ^= s << 5;
  rng_state = s ? s : 1;
  return (double)rng_state / (double)0xffffffffu;
}

/* ── WAV I/O (16-bit PCM, 24-bit PCM, 32-bit float, mono down-mix) ── */
typedef struct {
  float *samples;
  size_t n;
  int sample_rate;
} wav_t;

static uint32_t le32(const uint8_t *p) {
  return (uint32_t)p[0] | ((uint32_t)p[1] << 8)
       | ((uint32_t)p[2] << 16) | ((uint32_t)p[3] << 24);
}
static uint16_t le16(const uint8_t *p) {
  return (uint16_t)p[0] | ((uint16_t)p[1] << 8);
}
static int32_t le24_signed(const uint8_t *p) {
  int32_t v = (int32_t)p[0] | ((int32_t)p[1] << 8) | ((int32_t)p[2] << 16);
  if (v & 0x800000) v |= 0xff000000;
  return v;
}

static wav_t read_wav_mono(const char *path) {
  wav_t w = { NULL, 0, 0 };
  FILE *f = fopen(path, "rb");
  if (!f) { fprintf(stderr, "cannot open %s\n", path); exit(1); }
  fseek(f, 0, SEEK_END);
  size_t fsz = (size_t)ftell(f);
  fseek(f, 0, SEEK_SET);
  uint8_t *buf = (uint8_t *)malloc(fsz);
  if (fread(buf, 1, fsz, f) != fsz) { fprintf(stderr, "short read %s\n", path); exit(1); }
  fclose(f);
  if (memcmp(buf, "RIFF", 4) != 0) {
    fprintf(stderr, "not RIFF: %s\n", path); exit(1);
  }
  int channels = 1, sr = SR, bits = 16, fmt = 1;
  size_t i = 12;
  while (i + 8 < fsz) {
    const uint8_t *id = buf + i;
    uint32_t size = le32(buf + i + 4);
    size_t body = i + 8;
    if (memcmp(id, "fmt ", 4) == 0) {
      fmt      = le16(buf + body);
      channels = le16(buf + body + 2);
      sr       = (int)le32(buf + body + 4);
      bits     = le16(buf + body + 14);
    } else if (memcmp(id, "data", 4) == 0) {
      int bps = bits / 8;
      size_t frames = size / (size_t)(bps * channels);
      float *out = (float *)calloc(frames, sizeof(float));
      for (size_t fi = 0; fi < frames; fi++) {
        double acc = 0;
        for (int c = 0; c < channels; c++) {
          const uint8_t *p = buf + body + (fi * channels + c) * bps;
          if (fmt == 3 && bits == 32) {
            float v; memcpy(&v, p, 4); acc += v;
          } else if (bits == 32) {
            int32_t v = (int32_t)le32(p); acc += (double)v / 2147483648.0;
          } else if (bits == 24) {
            acc += (double)le24_signed(p) / 8388608.0;
          } else {
            int16_t v = (int16_t)le16(p); acc += (double)v / 32768.0;
          }
        }
        out[fi] = (float)(acc / channels);
      }
      w.samples = out; w.n = frames; w.sample_rate = sr;
      free(buf);
      return w;
    }
    i = body + size + (size & 1);
  }
  fprintf(stderr, "no data chunk in %s\n", path); exit(1);
}

static void write_wav_stereo16(const char *path, const float *l, const float *r,
                               size_t n, int sr) {
  FILE *f = fopen(path, "wb");
  if (!f) { fprintf(stderr, "cannot write %s\n", path); exit(1); }
  uint32_t bytes = (uint32_t)(n * 4);
  uint8_t h[44];
  memcpy(h + 0,  "RIFF", 4);
  *(uint32_t *)(h + 4)  = 36 + bytes;
  memcpy(h + 8,  "WAVE", 4);
  memcpy(h + 12, "fmt ", 4);
  *(uint32_t *)(h + 16) = 16;
  *(uint16_t *)(h + 20) = 1;            /* PCM */
  *(uint16_t *)(h + 22) = 2;            /* channels */
  *(uint32_t *)(h + 24) = (uint32_t)sr;
  *(uint32_t *)(h + 28) = (uint32_t)(sr * 4);
  *(uint16_t *)(h + 32) = 4;            /* block align */
  *(uint16_t *)(h + 34) = 16;           /* bits */
  memcpy(h + 36, "data", 4);
  *(uint32_t *)(h + 40) = bytes;
  fwrite(h, 1, 44, f);
  int16_t *pcm = (int16_t *)malloc(bytes);
  for (size_t i = 0; i < n; i++) {
    float lf = l[i] > 1 ? 1 : (l[i] < -1 ? -1 : l[i]);
    float rf = r[i] > 1 ? 1 : (r[i] < -1 ? -1 : r[i]);
    pcm[i * 2]     = (int16_t)(lf * 32767.0f);
    pcm[i * 2 + 1] = (int16_t)(rf * 32767.0f);
  }
  fwrite(pcm, 1, bytes, f);
  free(pcm);
  fclose(f);
}

/* ── heartbeat: ~/.ac-pop-renders/<id>.json (Slab menubar reads it) ── */
static char hb_path[1024];
static int hb_pid = 0;
static int64_t hb_started_ms = 0;
static const char *hb_label = "oskie jam (c)";

static int64_t now_ms(void) {
  struct timespec ts; clock_gettime(CLOCK_REALTIME, &ts);
  return (int64_t)ts.tv_sec * 1000 + ts.tv_nsec / 1000000;
}

static void hb_write(int pct) {
  FILE *f = fopen(hb_path, "w");
  if (!f) return;
  fprintf(f,
    "{\"id\":\"audio-%d-%llx\",\"type\":\"audio\",\"label\":\"%s\","
    "\"pct\":%d,\"done\":null,\"total\":null,"
    "\"pid\":%d,\"startedAt\":%lld,\"updatedAt\":%lld}",
    hb_pid, (long long)hb_started_ms, hb_label,
    pct < 0 ? 0 : (pct > 100 ? 100 : pct),
    hb_pid, (long long)hb_started_ms, (long long)now_ms());
  fclose(f);
}

static void hb_begin(void) {
  hb_pid = (int)getpid();
  hb_started_ms = now_ms();
  const char *home = getenv("HOME");
  if (!home) home = "/tmp";
  char dir[1024];
  snprintf(dir, sizeof(dir), "%s/.ac-pop-renders", home);
  mkdir(dir, 0755);
  snprintf(hb_path, sizeof(hb_path), "%s/audio-%d-%llx.json",
           dir, hb_pid, (long long)hb_started_ms);
  hb_write(0);
}

static void hb_end(void) { unlink(hb_path); }

/* ── DSP helpers ───────────────────────────────────────────────────── */
static float *pitch_shift(const float *grain, size_t gn, double rate, size_t *out_n) {
  size_t outN = (size_t)((double)gn / rate);
  if (outN < 1) outN = 1;
  float *out = (float *)calloc(outN, sizeof(float));
  for (size_t i = 0; i < outN; i++) {
    double x = (double)i * rate;
    size_t i0 = (size_t)x;
    if (i0 + 1 >= gn) break;
    double f = x - (double)i0;
    out[i] = (float)(grain[i0] * (1.0 - f) + grain[i0 + 1] * f);
  }
  *out_n = outN;
  return out;
}

static void hann_in_place(float *buf, size_t n) {
  if (n < 2) return;
  for (size_t i = 0; i < n; i++) {
    buf[i] *= (float)(0.5 - 0.5 * cos(2.0 * M_PI * i / (double)(n - 1)));
  }
}

static void ar_envelope(float *buf, size_t n, double attack_s, double release_s) {
  size_t aN = (size_t)(attack_s * SR);
  size_t rN = (size_t)(release_s * SR);
  if (aN > n) aN = n;
  if (rN > n) rN = n;
  if (aN < 1) aN = 1;
  if (rN < 1) rN = 1;
  for (size_t i = 0; i < aN; i++) buf[i] *= (float)i / (float)aN;
  for (size_t i = 0; i < rN; i++) buf[n - rN + i] *= 1.0f - (float)i / (float)rN;
}

static void soft_sat(float *buf, size_t n, double drive) {
  double norm = tanh(drive);
  for (size_t i = 0; i < n; i++) {
    buf[i] = (float)(tanh(buf[i] * drive) / norm);
  }
}

static void lowpass(float *buf, size_t n, double cutoff_hz) {
  double a = exp(-2.0 * M_PI * cutoff_hz / SR);
  double y = 0.0;
  for (size_t i = 0; i < n; i++) {
    y = (1.0 - a) * buf[i] + a * y;
    buf[i] = (float)y;
  }
}

static void sweep_lp(float *buf, double start_s, double end_s,
                     double hz_start, double hz_end) {
  size_t a0 = (size_t)(start_s * SR);
  size_t a1 = (size_t)(end_s   * SR);
  if (a1 > TOTAL_N) a1 = TOTAL_N;
  if (a0 >= a1) return;
  size_t span = a1 - a0;
  double log0 = log(hz_start);
  double log1 = log(hz_end);
  double y = 0.0;
  /* 5ms prime to avoid click */
  size_t pre = (a0 > (size_t)(0.005 * SR)) ? a0 - (size_t)(0.005 * SR) : 0;
  double aStart = exp(-2.0 * M_PI * hz_start / SR);
  for (size_t p = pre; p < a0; p++) {
    y = (1.0 - aStart) * buf[p] + aStart * y;
  }
  for (size_t i = a0; i < a1; i++) {
    double t = (double)(i - a0) / (double)span;
    double fc = exp(log0 + (log1 - log0) * t);
    double a = exp(-2.0 * M_PI * fc / SR);
    y = (1.0 - a) * buf[i] + a * y;
    buf[i] = (float)y;
  }
}

static void sidechain(float *target, const float *source,
                      double attack_s, double release_s,
                      double threshold, double depth) {
  double aAtk = exp(-1.0 / (attack_s * SR));
  double aRel = exp(-1.0 / (release_s * SR));
  double env = 0.0;
  for (size_t i = 0; i < TOTAL_N; i++) {
    double v = fabs(source[i]);
    double a = (v > env) ? aAtk : aRel;
    env = a * env + (1.0 - a) * v;
    if (env > threshold) {
      double excess = env - threshold;
      double gain = 1.0 - excess * (depth * 6.0);
      if (gain < 1.0 - depth) gain = 1.0 - depth;
      target[i] = (float)(target[i] * gain);
    }
  }
}

/* ── shared sample data (loaded in main) ───────────────────────────── */
static float *g_bass = NULL;
static size_t g_bass_n = 0;
static float *g_drums = NULL;
static size_t g_drums_n = 0;
static size_t g_drum_start = 0;
static size_t g_loop_n = 0;

/* per-voice mono buffers */
static float *padBuf, *stabBuf, *bellBuf, *sineBellBuf, *drumBellBuf, *drumsBuf;

static void place_into(float *target, const float *src, size_t src_n, double start_s) {
  size_t off = (size_t)(start_s * SR);
  for (size_t i = 0; i < src_n && off + i < TOTAL_N; i++) target[off + i] += src[i];
}

/* ── granular voices ───────────────────────────────────────────────── */

/* PAD: overlap-add bass-stem grains, pitched to midi, filling dur_s.
 * Returns a malloc'd Float32 buffer of length n = dur_s * SR. */
static float *g_pad(int midi, double dur_s, double gain, size_t *out_n) {
  double rate = rate_for_midi(midi);
  double grainLen_s = 0.30;
  double advance_s = 0.15;
  size_t n = (size_t)(dur_s * SR);
  float *out = (float *)calloc(n, sizeof(float));
  size_t pos = 0;
  size_t grainSamples = (size_t)(grainLen_s * SR);
  double max_start = (double)g_bass_n / SR - grainLen_s - 2.0;
  if (max_start < 0.1) max_start = 0.1;
  while (pos < n) {
    double start = 1.0 + rand01() * max_start;
    size_t a = (size_t)(start * SR);
    if (a + grainSamples > g_bass_n) a = g_bass_n - grainSamples;
    float *g = (float *)malloc(grainSamples * sizeof(float));
    memcpy(g, g_bass + a, grainSamples * sizeof(float));
    hann_in_place(g, grainSamples);
    size_t sn = 0;
    float *s = pitch_shift(g, grainSamples, rate, &sn);
    for (size_t i = 0; i < sn && pos + i < n; i++) out[pos + i] += s[i] * (float)gain;
    free(g); free(s);
    pos += (size_t)(advance_s * SR);
  }
  ar_envelope(out, n, 0.05, 0.10);
  lowpass(out, n, 2200);
  *out_n = n;
  return out;
}

/* STAB: short pitched bass grain, soft-saturated industrial chord stab. */
static float *g_stab(int midi, double dur_s, double gain, size_t *out_n) {
  double rate = rate_for_midi(midi);
  double grainLen_s = 0.22;
  size_t grainSamples = (size_t)(grainLen_s * SR);
  double max_start = (double)g_bass_n / SR - grainLen_s - 1.0;
  if (max_start < 0.1) max_start = 0.1;
  double start = 0.5 + rand01() * max_start;
  size_t a = (size_t)(start * SR);
  if (a + grainSamples > g_bass_n) a = g_bass_n - grainSamples;
  float *g = (float *)malloc(grainSamples * sizeof(float));
  memcpy(g, g_bass + a, grainSamples * sizeof(float));
  size_t sn = 0;
  float *s = pitch_shift(g, grainSamples, rate, &sn);
  lowpass(s, sn, 2800);
  size_t n = (size_t)(dur_s * SR);
  size_t outN = (n > sn) ? n : sn;
  float *out = (float *)calloc(outN, sizeof(float));
  for (size_t i = 0; i < sn; i++) out[i] = s[i];
  double rel = dur_s * 0.6;
  if (rel > 0.08) rel = 0.08;
  ar_envelope(out, outN, 0.003, rel);
  soft_sat(out, outN, 1.8);
  for (size_t i = 0; i < outN; i++) out[i] *= (float)gain;
  free(g); free(s);
  /* trim to n */
  if (outN > n) {
    float *trim = (float *)malloc(n * sizeof(float));
    memcpy(trim, out, n * sizeof(float));
    free(out);
    out = trim;
    outN = n;
  } else if (outN < n) {
    float *pad = (float *)calloc(n, sizeof(float));
    memcpy(pad, out, outN * sizeof(float));
    free(out);
    out = pad;
    outN = n;
  }
  *out_n = outN;
  return out;
}

/* LOW SINE BELL: pure additive bell, inharmonic partials. */
static float *g_low_sine_bell(int midi, double dur_s, double gain, size_t *out_n) {
  double freq = midi_to_hz(midi);
  size_t n = (size_t)(dur_s * SR);
  float *out = (float *)calloc(n, sizeof(float));
  const double ratios[4] = { 1.0, 2.01, 2.76, 4.10 };
  const double amps[4]   = { 1.00, 0.42, 0.22, 0.11 };
  const double decays[4] = {
    dur_s * 0.55, dur_s * 0.30, dur_s * 0.18, dur_s * 0.10
  };
  for (size_t i = 0; i < n; i++) {
    double t = (double)i / SR;
    double s = 0.0;
    for (int p = 0; p < 4; p++) {
      s += amps[p] * sin(2.0 * M_PI * freq * ratios[p] * t) * exp(-t / decays[p]);
    }
    out[i] = (float)(s * gain * 0.5);
  }
  size_t aN = (size_t)(0.002 * SR);
  if (aN > n) aN = n;
  for (size_t i = 0; i < aN; i++) out[i] *= (float)i / (float)aN;
  *out_n = n;
  return out;
}

/* DRUM BELL: pitched drum grain with bell envelope. The drum-into-bell
 * morph: drums literally become pitched ringing. */
static float *g_drum_bell(int midi, double dur_s, double gain, size_t *out_n) {
  /* drum stem centroid ~1.2 kHz, treat as MIDI 84 (C6) */
  const int SOURCE_MIDI = 84;
  double rate = pow(2.0, (double)(midi - SOURCE_MIDI) / 12.0);
  double grainLen_s = 0.08;
  size_t grainSamples = (size_t)(grainLen_s * SR);
  /* draw from drumLoop window */
  double max_start = LOOP_S - grainLen_s - 0.1;
  if (max_start < 0.05) max_start = 0.05;
  double start = LOOP_START_S + rand01() * max_start;
  size_t a = (size_t)(start * SR);
  if (a + grainSamples > g_drums_n) a = g_drums_n - grainSamples;
  float *g = (float *)malloc(grainSamples * sizeof(float));
  memcpy(g, g_drums + a, grainSamples * sizeof(float));
  size_t sn = 0;
  float *s = pitch_shift(g, grainSamples, rate, &sn);
  lowpass(s, sn, 3500);
  size_t n = (size_t)(dur_s * SR);
  float *out = (float *)calloc(n, sizeof(float));
  for (size_t i = 0; i < sn && i < n; i++) out[i] = s[i];
  size_t head = (size_t)(0.04 * SR);
  if (head > n) head = n;
  double tau = dur_s * 0.38;
  for (size_t i = 0; i < n; i++) {
    double t = (double)i / SR;
    double hb = (i < head) ? 1.0 + 1.1 * (1.0 - (double)i / (double)head) : 1.0;
    out[i] = (float)(out[i] * hb * exp(-t / tau) * gain);
  }
  free(g); free(s);
  *out_n = n;
  return out;
}

/* BELL: pitched bass grain shaped like a marimba (fast attack, bright
 * head decay, exponential body). */
static float *g_bell(int midi, double dur_s, double gain, size_t *out_n) {
  double rate = rate_for_midi(midi);
  double grainLen_s = 0.18;
  size_t grainSamples = (size_t)(grainLen_s * SR);
  double max_start = (double)g_bass_n / SR - grainLen_s - 1.0;
  if (max_start < 0.1) max_start = 0.1;
  double start = 0.5 + rand01() * max_start;
  size_t a = (size_t)(start * SR);
  if (a + grainSamples > g_bass_n) a = g_bass_n - grainSamples;
  float *g = (float *)malloc(grainSamples * sizeof(float));
  memcpy(g, g_bass + a, grainSamples * sizeof(float));
  size_t sn = 0;
  float *s = pitch_shift(g, grainSamples, rate, &sn);
  lowpass(s, sn, 3200);
  size_t n = (size_t)(dur_s * SR);
  float *out = (float *)calloc(n, sizeof(float));
  for (size_t i = 0; i < sn && i < n; i++) out[i] = s[i];
  size_t head = (size_t)(0.06 * SR);
  if (head > n) head = n;
  double tau = dur_s * 0.32;
  for (size_t i = 0; i < n; i++) {
    double t = (double)i / SR;
    double hb = (i < head) ? 1.0 + 1.4 * (1.0 - (double)i / (double)head) : 1.0;
    out[i] = (float)(out[i] * hb * exp(-t / tau) * gain);
  }
  size_t aN = (size_t)(0.001 * SR);
  if (aN > n) aN = n;
  for (size_t i = 0; i < aN; i++) out[i] *= (float)i / (float)aN;
  free(g); free(s);
  *out_n = n;
  return out;
}

/* ── drum tiling with variants ─────────────────────────────────────── */
typedef enum {
  V_FULL, V_MUTE1ST, V_MUTELAST, V_HALFSILENT, V_STUTTER1, V_REVERSE, V_SILENT
} variant_t;

static void tile_drums(int startBar, int nBars, double gain,
                       const variant_t *variants, int nVariants) {
  size_t loopN = g_loop_n;
  size_t barN = (size_t)(BAR_S * SR);
  int loops = (nBars + LOOP_BARS - 1) / LOOP_BARS;
  for (int lp = 0; lp < loops; lp++) {
    variant_t v = (variants && nVariants > 0) ? variants[lp % nVariants] : V_FULL;
    if (v == V_SILENT) continue;
    size_t off = (size_t)(((double)(startBar + lp * LOOP_BARS)) * BAR_S * SR);
    size_t remaining = (size_t)((double)(nBars - lp * LOOP_BARS) * barN);
    size_t writeN = (loopN < remaining) ? loopN : remaining;
    const float *src = g_drums + g_drum_start;
    if (v == V_FULL) {
      for (size_t i = 0; i < writeN && off + i < TOTAL_N; i++)
        drumsBuf[off + i] += src[i] * (float)gain;
    } else if (v == V_MUTE1ST) {
      for (size_t i = barN; i < writeN && off + i < TOTAL_N; i++)
        drumsBuf[off + i] += src[i] * (float)gain;
    } else if (v == V_MUTELAST) {
      size_t stop = (writeN < 3 * barN) ? writeN : 3 * barN;
      for (size_t i = 0; i < stop && off + i < TOTAL_N; i++)
        drumsBuf[off + i] += src[i] * (float)gain;
    } else if (v == V_HALFSILENT) {
      size_t stop = (writeN < 2 * barN) ? writeN : 2 * barN;
      for (size_t i = 0; i < stop && off + i < TOTAL_N; i++)
        drumsBuf[off + i] += src[i] * (float)gain;
    } else if (v == V_STUTTER1) {
      for (int rep = 0; rep < 4; rep++) {
        size_t rOff = off + (size_t)rep * barN;
        for (size_t i = 0; i < barN && rOff + i < TOTAL_N; i++)
          drumsBuf[rOff + i] += src[i] * (float)gain;
      }
    } else if (v == V_REVERSE) {
      for (size_t i = 0; i < writeN && off + i < TOTAL_N; i++)
        drumsBuf[off + i] += src[loopN - 1 - i] * (float)gain;
    }
  }
}

/* ── arrangement ───────────────────────────────────────────────────── */
static int cursor = 0;

static void place_pad(int midi, double dur_s, double gain, double t) {
  size_t n; float *b = g_pad(midi, dur_s, gain, &n);
  place_into(padBuf, b, n, t); free(b);
}
static void place_stab(int midi, double dur_s, double gain, double t) {
  size_t n; float *b = g_stab(midi, dur_s, gain, &n);
  place_into(stabBuf, b, n, t); free(b);
}
static void place_low_sine(int midi, double dur_s, double gain, double t) {
  size_t n; float *b = g_low_sine_bell(midi, dur_s, gain, &n);
  place_into(sineBellBuf, b, n, t); free(b);
}
static void place_drum_bell(int midi, double dur_s, double gain, double t) {
  size_t n; float *b = g_drum_bell(midi, dur_s, gain, &n);
  place_into(drumBellBuf, b, n, t); free(b);
}
static void place_bell(int midi, double dur_s, double gain, double t) {
  size_t n; float *b = g_bell(midi, dur_s, gain, &n);
  place_into(bellBuf, b, n, t); free(b);
}

static void intro(int nBars) {
  variant_t v[1] = { V_HALFSILENT };
  tile_drums(cursor, nBars, 0.4, v, 1);
  for (int bar = 0; bar < nBars; bar++) {
    double t = (double)(cursor + bar) * BAR_S;
    chord_t ch = CHORDS_A[bar % 4];
    place_pad(chord_root(ch, 2), BAR_S, 0.35, t);
    place_low_sine(chord_root(ch, 3), BAR_S * 0.9, 0.40, t);
    int sparkle = chord_root(ch, 5) + ((bar & 1) ? ch.fifth : 0);
    place_bell(sparkle, BEAT_S * 2.5, 0.32, t + 1.5 * BEAT_S);
    if (bar == 1) place_drum_bell(chord_root(ch, 4) + ch.third, BEAT_S * 2, 0.35,
                                  t + 3 * BEAT_S);
    if (bar == 3) place_drum_bell(chord_root(ch, 4), BEAT_S * 1.5, 0.40,
                                  t + 2.5 * BEAT_S);
  }
  cursor += nBars;
}

static void verse(int nBars, const chord_t *prog) {
  variant_t v[2] = { V_FULL, V_MUTELAST };
  tile_drums(cursor, nBars, 0.9, v, 2);
  for (int bar = 0; bar < nBars; bar++) {
    double t = (double)(cursor + bar) * BAR_S;
    chord_t ch = prog[bar % 4];
    int phrase = (bar / 4) % 2;
    if (phrase == 0) {
      place_pad(chord_root(ch, 2), BAR_S, 0.38, t);
      place_low_sine(chord_root(ch, 3), BAR_S * 0.9, 0.38, t);
      if (bar % 2 == 1)
        place_low_sine(chord_root(ch, 3) + ch.fifth, BAR_S * 0.7, 0.22, t + BEAT_S);
      place_bell(chord_root(ch, 4), BEAT_S * 1.6, 0.28, t);
    } else {
      place_pad(chord_root(ch, 2), BAR_S, 0.42, t);
      place_pad(chord_root(ch, 3) + ch.third, BAR_S, 0.20, t);
      int r4 = chord_root(ch, 4);
      int echoes[4] = { r4, r4 + ch.third, r4 + ch.fifth, r4 };
      for (int i = 0; i < 4; i++) {
        if (rand01() < 0.55) {
          place_drum_bell(echoes[i], BEAT_S * 0.9, 0.30,
                          t + i * BEAT_S + BEAT_S * 0.5);
        }
      }
      place_low_sine(chord_root(ch, 3), BAR_S * 0.95, 0.34, t);
    }
  }
  cursor += nBars;
}

static void chorus(int nBars, double intensity, const chord_t *prog) {
  variant_t v[2] = { V_FULL, V_FULL };
  tile_drums(cursor, nBars, 1.0 * intensity, v, 2);
  for (int bar = 0; bar < nBars; bar++) {
    double t = (double)(cursor + bar) * BAR_S;
    chord_t ch = prog[bar % 4];
    int r4 = chord_root(ch, 4);
    int arpAsc[8] = {
      r4, r4 + ch.fifth, r4 + 12, r4 + ch.third,
      r4 + ch.fifth, r4 + 12, r4 + ch.third, r4 + ch.fifth
    };
    int arpDesc[8];
    for (int i = 0; i < 8; i++) arpDesc[i] = arpAsc[7 - i];
    int *dir = ((bar / 2) % 2 == 0) ? arpAsc : arpDesc;
    int triadRoot = chord_root(ch, 3);
    int triadThird = triadRoot + ch.third;
    int triadFifth = triadRoot + ch.fifth;
    place_pad(chord_root(ch, 2), BAR_S, 0.40 * intensity, t);
    place_pad(triadThird, BAR_S, 0.22 * intensity, t);
    place_pad(triadFifth, BAR_S, 0.22 * intensity, t);
    place_low_sine(chord_root(ch, 3), BAR_S * 0.95, 0.36 * intensity, t);
    double stabBeats[2];
    int nb;
    if (bar < nBars / 2) { stabBeats[0] = 1; stabBeats[1] = 3;   nb = 2; }
    else                 { stabBeats[0] = 0; stabBeats[1] = 2.5; nb = 2; }
    for (int b = 0; b < nb; b++) {
      place_stab(triadRoot, BEAT_S * 0.6, 0.50 * intensity, t + stabBeats[b] * BEAT_S);
      place_stab(triadFifth, BEAT_S * 0.6, 0.38 * intensity, t + stabBeats[b] * BEAT_S);
    }
    for (int i = 0; i < 8; i++) {
      place_bell(dir[i], BEAT_S * 0.85, 0.26 * intensity, t + i * (BEAT_S / 2));
    }
    if (bar == 3 || bar == 7) {
      chord_t nxt = prog[(bar + 1) % 4];
      int nr4 = chord_root(nxt, 4);
      int climb[4] = { nr4, nr4 + nxt.third, nr4 + nxt.fifth, chord_root(nxt, 5) };
      for (int i = 0; i < 4; i++) {
        place_drum_bell(climb[i], BEAT_S * 0.7, 0.38 * intensity, t + i * BEAT_S);
      }
    }
  }
  cursor += nBars;
}

static void bridge(int nBars, const chord_t *prog) {
  variant_t v[1] = { V_HALFSILENT };
  tile_drums(cursor, nBars, 0.55, v, 1);
  double t0 = (double)cursor * BAR_S;
  for (int bar = 0; bar < nBars; bar++) {
    double t = t0 + bar * BAR_S;
    chord_t ch = prog[bar % 4];
    int triadRoot = chord_root(ch, 3);
    place_pad(chord_root(ch, 2), BAR_S, 0.45, t);
    place_pad(triadRoot + ch.third, BAR_S, 0.30, t);
    place_pad(triadRoot + ch.fifth, BAR_S, 0.30, t);
    place_low_sine(chord_root(ch, 2), BAR_S, 0.42, t);
  }
  for (int bar = 0; bar < nBars; bar++) {
    chord_t ch = prog[bar % 4];
    int r = chord_root(ch, 4);
    int tones[4] = { r, r + ch.fifth, r + 12, r + ch.third };
    for (int s = 0; s < 4; s++) {
      place_drum_bell(tones[s], BEAT_S * 1.2, 0.38, t0 + bar * BAR_S + s * BEAT_S);
    }
  }
  for (int bar = 0; bar < nBars; bar++) {
    chord_t ch = prog[bar % 4];
    double t = (double)(cursor + bar) * BAR_S;
    place_bell(chord_root(ch, 5), BEAT_S * 3, 0.34, t);
    place_bell(chord_root(ch, 4) + ch.fifth, BEAT_S * 3, 0.28, t + 2 * BEAT_S);
  }
  cursor += nBars;
}

static void outro(int nBars, const chord_t *prog) {
  variant_t v[1] = { V_MUTELAST };
  tile_drums(cursor, nBars, 0.65, v, 1);
  for (int bar = 0; bar < nBars; bar++) {
    double t = (double)(cursor + bar) * BAR_S;
    chord_t ch = prog[bar % 4];
    int triadRoot = chord_root(ch, 3);
    place_pad(chord_root(ch, 2), BAR_S, 0.45, t);
    place_pad(triadRoot + ch.third, BAR_S, 0.28, t);
    place_pad(triadRoot + ch.fifth, BAR_S, 0.28, t);
    place_low_sine(chord_root(ch, 2), BAR_S, 0.48, t);
    place_low_sine(chord_root(ch, 3), BAR_S * 0.9, 0.30, t);
  }
  double tStart = (double)cursor * BAR_S;
  chord_t lastCh = prog[(nBars - 1) % 4];
  place_bell(chord_root(lastCh, 5), 4.0, 0.42, tStart);
  place_drum_bell(chord_root(lastCh, 4), BEAT_S * 4, 0.32, tStart + 3 * BAR_S);
  cursor += nBars;
}

/* ── main ──────────────────────────────────────────────────────────── */
int main(void) {
  hb_begin(); hb_write(0);

  printf("loading samples ...\n");
  wav_t bass = read_wav_mono("stems/htdemucs/bass.wav");
  wav_t drums = read_wav_mono("stems/htdemucs/drums.wav");
  if (bass.sample_rate != SR || drums.sample_rate != SR) {
    fprintf(stderr, "sample rate mismatch\n"); return 1;
  }
  g_bass = bass.samples; g_bass_n = bass.n;
  g_drums = drums.samples; g_drums_n = drums.n;
  g_drum_start = (size_t)(LOOP_START_S * SR);
  g_loop_n = (size_t)(LOOP_S * SR);
  hb_write(10);

  /* per-voice buffers */
  padBuf      = (float *)calloc(TOTAL_N, sizeof(float));
  stabBuf     = (float *)calloc(TOTAL_N, sizeof(float));
  bellBuf     = (float *)calloc(TOTAL_N, sizeof(float));
  sineBellBuf = (float *)calloc(TOTAL_N, sizeof(float));
  drumBellBuf = (float *)calloc(TOTAL_N, sizeof(float));
  drumsBuf    = (float *)calloc(TOTAL_N, sizeof(float));

  printf("rendering intro ...\n");    intro(4);                   hb_write(22);
  printf("rendering verse ...\n");    verse(8, CHORDS_A);         hb_write(38);
  printf("rendering chorus 1 ...\n"); chorus(8, 0.9, CHORDS_A);   hb_write(58);
  printf("rendering bridge ...\n");   bridge(4, CHORDS_A);        hb_write(70);
  printf("rendering chorus 2 ...\n"); chorus(8, 1.0, CHORDS_B);   hb_write(85);
  printf("rendering outro ...\n");    outro(4, CHORDS_A);         hb_write(92);

  /* filter sweeps */
  sweep_lp(drumsBuf, 0 * BAR_S,  4 * BAR_S,  250,   16000);
  sweep_lp(padBuf,   19 * BAR_S, 20 * BAR_S, 2200,  4500);
  sweep_lp(padBuf,   20 * BAR_S, 22 * BAR_S, 4500,  500);
  sweep_lp(padBuf,   22 * BAR_S, 24 * BAR_S, 500,   3800);
  sweep_lp(drumsBuf, 32 * BAR_S, 36 * BAR_S, 16000, 250);

  /* sidechain — multi-track awareness */
  sidechain(padBuf,      drumsBuf, 0.005, 0.18, 0.045, 0.45);
  sidechain(sineBellBuf, drumsBuf, 0.005, 0.16, 0.045, 0.40);
  sidechain(bellBuf,     drumsBuf, 0.004, 0.14, 0.050, 0.30);

  /* mix to stereo (per-voice pans) */
  float *left  = (float *)calloc(TOTAL_N, sizeof(float));
  float *right = (float *)calloc(TOTAL_N, sizeof(float));
  struct { float *src; double l; double r; } voices[] = {
    { drumsBuf,    1.00, 1.00 },
    { padBuf,      0.95, 0.95 },
    { sineBellBuf, 0.95, 0.95 },
    { stabBuf,     1.00, 0.85 },
    { bellBuf,     0.85, 1.00 },
    { drumBellBuf, 0.90, 0.95 },
  };
  for (size_t v = 0; v < sizeof(voices)/sizeof(voices[0]); v++) {
    for (size_t i = 0; i < TOTAL_N; i++) {
      left[i]  += voices[v].src[i] * (float)voices[v].l;
      right[i] += voices[v].src[i] * (float)voices[v].r;
    }
  }

  /* trim */
  size_t finalN = (size_t)(TARGET_S * SR);

  /* soft limit */
  float peak = 0;
  for (size_t i = 0; i < finalN; i++) {
    float m = fabsf(left[i]); if (m > peak) peak = m;
    m = fabsf(right[i]);      if (m > peak) peak = m;
  }
  if (peak > 0.95f) {
    float g = 0.95f / peak;
    for (size_t i = 0; i < finalN; i++) { left[i] *= g; right[i] *= g; }
  }

  /* fade in/out */
  size_t fi = (size_t)(0.3 * SR);
  size_t fo = (size_t)(2.0 * SR);
  for (size_t i = 0; i < fi; i++) {
    float g = (float)i / (float)fi;
    left[i] *= g; right[i] *= g;
  }
  for (size_t i = 0; i < fo; i++) {
    float g = (float)pow(1.0 - (double)i / (double)fo, 1.4);
    left[finalN - fo + i]  *= g;
    right[finalN - fo + i] *= g;
  }
  hb_write(97);

  write_wav_stereo16("oskie-jam.wav", left, right, finalN, SR);
  hb_write(100);
  hb_end();
  printf("\xe2\x9c\x93 oskie-jam.wav  (%.2fs)\n", (double)finalN / SR);

  free(left); free(right);
  free(padBuf); free(stabBuf); free(bellBuf);
  free(sineBellBuf); free(drumBellBuf); free(drumsBuf);
  free(g_bass); free(g_drums);
  return 0;
}
