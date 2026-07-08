// flatterbop180.c — C renderer for the flatterbop180 pop track (fluttabap360's
// fast 3/4 zoo-bop cousin): the SAME orchestra + a ZOO sample voice. Mirrors the
// hellsine / nullabye split: the JS composer (../bin/render-fluttabap360.mjs
// --bake) emits a flat score of already-humanized voice events; this engine
// replays each voice's DSP sample-for-sample into a per-voice mono bus, then
// runs the SAME stereo mixdown (place / reverb / delay / sends / finalize) the
// JS render does, and writes a pre-master stereo f32 raw. The ffmpeg master
// chain lives in run-c.mjs, NOT here.
//
// Parity is verified by c/compare.mjs (JS --keep-raw vs C, --solo per voice
// AND the full no-solo mix). No -ffast-math (see build.sh) so the libm calls
// match the JS reference.
//
// PORT STATUS — all voices + the full pre-master mix:
//   ✓ sub kick marimba                              (milestones 1-3)
//   ✓ snare hat shaker subperc scratch scream rise revkick  (milestone 4)
//   ✓ pad drop plink stream cave + mix-bus DSP + finalize   (milestone 5)
//
// Usage:  flatterbop180 <score.txt> --raw <out.f32> [--solo a,b]

#define _POSIX_C_SOURCE 200809L   // strtok_r under -std=c11 (strict ANSI hides POSIX)
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif
#define TWO_PI 6.283185307179586476925286766559

static int    SR = 48000;
static long   NS = 0;
static float *outL = NULL, *outR = NULL;   // stereo master (post-mixdown)

// ── per-voice mono buses — one Float32 lane per voice, exactly as the JS
// render. The mixdown places / reverbs / sends these into the stereo master,
// matching render-fluttabap360.mjs bar-for-bar. ────────────────────────────
static double *busLead, *busBass, *busSpark, *busBell, *busVib, *busDeep,
              *busRise, *busScr, *busScream, *busKick, *busSub, *busSnare,
              *busHat, *busShak, *busCave, *busPad, *busDrop, *busStream,
              *busFlute, *busZooL, *busZooR;   // zoo = stereo pair (per-event pan)

// ── score-header globals the mixdown needs (baked once by the JS render) ────
static double gBeat = 0.0;            // BEAT (s) — for the drop echo time
static double gTargetSec = 180.0;     // hard form clamp (flatterbop = 3:00)
static long   gTotalBars = 0;
static double gBar = 0.0;             // BAR (s)
static double gFinalLiftSec = -1.0;   // start of the final-F chorus (key==5), <0 = none

// SUBSTRATE print params (baked) — tape/tube saturation drive, even-harmonic
// (tube) bias, and the noise-floor hiss level. Defaults = the 'tape' substrate.
static double gSatDrive = 1.6, gSatBias = 0.22, gSatHiss = 0.0055;

// section map (baked) → drives the reverb-scene send automation at mixdown.
static struct { char name[24]; long bar0, bars; } gSec[256];
static int gSecN = 0;

// ── REVERB SCENES — the room as a compositional element. Per section, how much
// of each voice blooms into the shared space. Mirrors REVERB_SCENES in
// render-fluttabap360.mjs. Voices: lead bell spark vib pad scr scream rise. ──
// flatterbop's room is ETHEREAL — sends run ~1.6× the 360's and even the
// driving passes keep a wash (the 360 went bone-dry there; this one floats).
static double scene_amt(const char *sec, const char *v) {
  #define S(a) (!strcmp(sec, a))
  #define V(a) (!strcmp(v, a))
  if (S("intro"))           { if(V("bell"))return .48; if(V("spark"))return .30; if(V("vib"))return .32; if(V("pad"))return .48; if(V("lead"))return .22; if(V("rise"))return .28; if(V("scream"))return .24; if(V("flute"))return .52; }
  else if (S("butterfly"))  { if(V("bell"))return .30; if(V("spark"))return .22; if(V("lead"))return .14; if(V("scream"))return .22; if(V("flute"))return .38; if(V("vib"))return .18; }
  else if (S("palofmine"))  { if(V("bell"))return .28; if(V("spark"))return .18; if(V("lead"))return .14; if(V("flute"))return .38; if(V("vib"))return .18; }
  else if (S("mommywow"))   { if(V("bell"))return .52; if(V("pad"))return .48; if(V("vib"))return .30; if(V("lead"))return .20; if(V("spark"))return .26; if(V("flute"))return .55; }
  else if (S("slinky"))     { if(V("scream"))return .20; if(V("flute"))return .32; if(V("bell"))return .18; if(V("lead"))return .10; }
  else if (S("fly"))        { if(V("bell"))return .20; if(V("flute"))return .36; if(V("lead"))return .12; if(V("vib"))return .16; }
  else if (S("ride"))       { if(V("scream"))return .46; if(V("scr"))return .42; if(V("bell"))return .18; if(V("rise"))return .22; if(V("flute"))return .24; }
  else if (S("cave"))       { if(V("scream"))return .36; if(V("vib"))return .32; if(V("pad"))return .34; if(V("lead"))return .18; if(V("flute"))return .46; }
  else if (S("progression")){ if(V("bell"))return .26; if(V("spark"))return .18; if(V("lead"))return .12; if(V("flute"))return .30; }
  else if (S("land"))       { if(V("bell"))return .38; if(V("pad"))return .32; if(V("lead"))return .18; if(V("flute"))return .36; }
  else if (S("button"))     { if(V("bell"))return .44; if(V("pad"))return .36; }
  #undef S
  #undef V
  return 0.0;
}

// ── deterministic LCG noise, identical to perc.mjs makeNoise(seed) ──────────
// JS: s = (imul(s,1664525)+1013904223)>>>0; return (s/0xffffffff)*2-1
typedef struct { unsigned int s; } Noise;
static Noise noise_make(unsigned int seed) { Noise n; n.s = seed ? seed : 1u; return n; }
static double noise_next(Noise *n) {
  n->s = (unsigned int)(n->s * 1664525u + 1013904223u);
  return ((double)n->s / 4294967295.0) * 2.0 - 1.0;
}

static double midi_to_freq(double midi) { return 440.0 * pow(2.0, (midi - 69.0) / 12.0); }

// ── SUBSINE — the butter sine sub → busDeep (deepBuf). ──────────────────────
static void synth_sub(double start, double f, double dur, double g, double *bus) {
  const double dt = 1.0 / SR;
  long n0 = (long)floor(start * SR);
  long len = (long)ceil((dur * 1.2 + 0.05) * SR);
  long attS = (long)floor(0.008 * SR);
  double relA = log(1000.0) / (dur * 0.9);
  double phase = 0.0;
  for (long i = 0; i < len; i++) {
    long d = n0 + i;
    phase += f * dt;
    if (d < 0 || d >= NS) continue;
    double env = exp(-relA * ((double)i / SR));
    if (i < attS) env *= (double)i / (double)attS;
    bus[d] += sin(TWO_PI * phase) * env * g;
  }
}

// ── KICK — renderKick + mixInto → busKick (kickBuf). ───────────────────────
static void synth_kick(double start, double gain, double fStart, double fEnd,
                       double pitchT, double ampT60, double clickAmt, double drive,
                       double *bus) {
  const double dt = 1.0 / SR;
  double tail = ampT60 * 1.3 + 0.02;
  long nsK = (long)ceil(tail * SR);
  double ampA = log(1000.0) / ampT60;
  Noise noise = noise_make((unsigned int)((long)floor(start * 9173.0) | 1L));
  long startIdx = (long)floor(start * SR);
  double phase = 0.0, dnorm = tanh(drive);
  for (long i = 0; i < nsK; i++) {
    double t = i * dt;
    double f = fEnd + (fStart - fEnd) * exp(-t / pitchT);
    phase += f * dt;
    double body = tanh(sin(TWO_PI * phase) * drive) / dnorm;
    double ampEnv = exp(-ampA * t);
    double s = body * ampEnv;
    if (t < 0.006) s += noise_next(&noise) * clickAmt * exp(-t / 0.0012);
    long d = startIdx + i;
    if (d < 0 || d >= NS) continue;
    bus[d] += s * gain;
  }
}

// ── general equal-power pan (pan ∈ [-1,1]) → L/R gains, matching render's
// pan(p): a=(p+1)·π/4; [cos a, sin a]. ─────────────────────────────────────
static void pan_lr(double p, double *lg, double *rg) {
  double a = (p + 1.0) * M_PI / 4.0;
  *lg = cos(a); *rg = sin(a);
}

// ── ZOO — sampled animal one-shots (freesound, CC0/CC-BY, pre-baked to 48k
// mono f32 WAV by bin/fetch-zoo.mjs). The score registers slots with
// `zoosample <idx> <path>` and fires them with `zoo <start> <idx> <gain>
// <rate> <pan>` — rate repitches (2^(semis/12)), pan is per call, so the
// menagerie can answer from anywhere in the stereo park. ────────────────────
#define ZOO_SLOTS 8
static float *gZooBuf[ZOO_SLOTS] = {0};
static long   gZooLen[ZOO_SLOTS] = {0};

// minimal RIFF/WAVE reader: walks chunks, takes fmt 1 (PCM16) or 3 (float32),
// downmixes channels by average. Good for exactly what fetch-zoo.mjs writes.
static int load_wav_mono(const char *path, float **out, long *outLen) {
  FILE *w = fopen(path, "rb"); if (!w) return 0;
  unsigned char h[12];
  if (fread(h, 1, 12, w) != 12 || memcmp(h, "RIFF", 4) || memcmp(h + 8, "WAVE", 4)) { fclose(w); return 0; }
  int fmt = 0, ch = 0, bits = 0; long dataOff = 0, dataLen = 0;
  unsigned char ck[8];
  while (fread(ck, 1, 8, w) == 8) {
    long sz = (long)ck[4] | ((long)ck[5] << 8) | ((long)ck[6] << 16) | ((long)ck[7] << 24);
    if (!memcmp(ck, "fmt ", 4)) {
      unsigned char fb[16]; if (sz < 16 || fread(fb, 1, 16, w) != 16) break;
      fmt = fb[0] | (fb[1] << 8); ch = fb[2] | (fb[3] << 8); bits = fb[14] | (fb[15] << 8);
      if (fseek(w, sz - 16 + (sz & 1), SEEK_CUR)) break;
    } else if (!memcmp(ck, "data", 4)) {
      dataOff = ftell(w); dataLen = sz;
      if (fseek(w, sz + (sz & 1), SEEK_CUR)) break;
    } else if (fseek(w, sz + (sz & 1), SEEK_CUR)) break;
  }
  if (!dataOff || ch < 1 || (fmt != 1 && fmt != 3)) { fclose(w); return 0; }
  int bytes = bits / 8;
  long frames = dataLen / (bytes * ch);
  float *buf = malloc((size_t)frames * sizeof(float));
  unsigned char *raw = malloc((size_t)dataLen);
  if (!buf || !raw) { free(buf); free(raw); fclose(w); return 0; }
  fseek(w, dataOff, SEEK_SET);
  if (fread(raw, 1, (size_t)dataLen, w) != (size_t)dataLen) { free(buf); free(raw); fclose(w); return 0; }
  fclose(w);
  for (long i = 0; i < frames; i++) {
    double acc = 0.0;
    for (int c = 0; c < ch; c++) {
      const unsigned char *pp = raw + ((size_t)(i * ch + c)) * bytes;
      if (fmt == 3 && bits == 32) { float v; memcpy(&v, pp, 4); acc += v; }
      else if (fmt == 1 && bits == 16) { short v = (short)(pp[0] | (pp[1] << 8)); acc += v / 32768.0; }
    }
    buf[i] = (float)(acc / ch);
  }
  free(raw);
  *out = buf; *outLen = frames;
  return 1;
}

// fire a zoo one-shot: linear-interp resample at `rate`, tiny declick fades,
// equal-power panned into the zoo stereo pair.
static void synth_zoo(double start, int idx, double gain, double rate, double pan) {
  if (idx < 0 || idx >= ZOO_SLOTS || !gZooBuf[idx] || gZooLen[idx] < 2 || rate <= 0.0) return;
  double lg, rg; pan_lr(pan, &lg, &rg);
  long n0 = (long)floor(start * SR);
  long len = (long)floor((double)(gZooLen[idx] - 1) / rate);
  long attS = (long)(0.002 * SR), relS = (long)(0.02 * SR);
  if (attS < 1) attS = 1;
  if (relS < 1) relS = 1;
  for (long i = 0; i < len; i++) {
    long d = n0 + i; if (d < 0 || d >= NS) continue;
    double pos = i * rate; long p0 = (long)pos; double fr = pos - p0;
    double s = gZooBuf[idx][p0] * (1.0 - fr) + gZooBuf[idx][p0 + 1] * fr;
    double env = 1.0;
    if (i < attS) env = (double)i / attS;
    long rem = len - 1 - i;
    if (rem < relS) { double re = (double)rem / relS; if (re < env) env = re; }
    s *= env * gain;
    busZooL[d] += s * lg; busZooR[d] += s * rg;
  }
}

// ── MODAL MARIMBA — full port of synths/marimba.mjs renderMarimba → bus. ────
static void synth_marimba(double start, double midi, double durSec, double gain,
                          double decayMul, double attack, double release,
                          double mallet, double resQ, double resGain, double strike,
                          double noiseAmt, double tremHz, double tremDepth,
                          int M, const double *partials, const double *amps,
                          const double *decays, double *bus) {
  if (!(durSec > 0.0) || gain == 0.0 || M <= 0) return;
  const double dt = 1.0 / SR, LN1000 = log(1000.0);
  double ampSum = 0.0, maxDecay = 0.0;
  for (int n = 0; n < M; n++) { ampSum += amps[n]; if (decays[n] > maxDecay) maxDecay = decays[n]; }
  if (ampSum == 0.0) ampSum = 1.0;

  double freq[16], amp0[16], alpha[16], phase[16];
  for (int n = 0; n < M; n++) {
    freq[n] = midi_to_freq(midi) * partials[n];
    double shape = fabs(cos((n + 1) * M_PI * strike));
    amp0[n] = (amps[n] / ampSum) * (0.35 + 0.65 * shape);
    double dd = decays[n] * decayMul; if (dd < 0.01) dd = 0.01;
    alpha[n] = LN1000 / dd;
    phase[n] = 0.0;
  }
  double f0 = midi_to_freq(midi);
  long malletS = (long)floor((mallet > 0 ? mallet : 0.002) * SR); if (malletS < 1) malletS = 1;
  double fr = f0 < SR / 6.0 ? f0 : SR / 6.0;
  double resK = 2.0 * sin(M_PI * fr / SR);
  double resDamp = 1.0 / (resQ > 1.0 ? resQ : 1.0);
  double svfLp = 0.0, svfBp = 0.0;
  int tremActive = (tremHz > 0.0 && tremDepth > 0.0);
  double tremPhase = 0.0;
  double noiseDecaySec = 0.012, noiseAlpha = noiseAmt > 0.0 ? LN1000 / noiseDecaySec : 0.0;
  Noise mn = noise_make((unsigned int)((unsigned int)(start * 7919.0 + midi * 41.0) | 1u));

  double longestDecay = maxDecay * decayMul;
  double tailSec = (durSec > longestDecay * 1.2 ? durSec : longestDecay * 1.2) + release;
  long nsM = (long)ceil(tailSec * SR);
  long attS = (long)floor(attack * SR); if (attS < 1) attS = 1;
  long relS = (long)floor(release * SR); if (relS < 1) relS = 1;
  long durCeil = (long)ceil(durSec * SR);
  long releaseStart = (attS + 1 > durCeil) ? attS + 1 : durCeil;
  long startIdx = (long)floor(start * SR);

  for (long i = 0; i < nsM; i++) {
    double force = (i < malletS) ? 0.5 * (1.0 - cos((M_PI * 2.0 * i) / malletS)) : 0.0;
    double dry = 0.0;
    for (int n = 0; n < M; n++) {
      phase[n] += freq[n] * dt;
      if (phase[n] >= 1.0) phase[n] -= floor(phase[n]);
      double decayEnv = exp(-alpha[n] * i * dt);
      double s = sin(2.0 * M_PI * phase[n]);
      double drv = force > 0.0 ? (0.6 + 0.4 * force) : 1.0;
      dry += s * amp0[n] * decayEnv * drv;
    }
    if (noiseAmt > 0.0 && i * dt < 6.0 * noiseDecaySec) {
      double nEnv = exp(-noiseAlpha * i * dt);
      dry += noise_next(&mn) * noiseAmt * nEnv;
    }
    double hi = dry - svfLp - resDamp * svfBp;
    svfBp += resK * hi;
    svfLp += resK * svfBp;
    double resOut = svfBp * resGain;
    if (tremActive) {
      tremPhase += tremHz * dt;
      if (tremPhase >= 1.0) tremPhase -= 1.0;
      double lfo = 0.5 * (1.0 + sin(2.0 * M_PI * tremPhase));
      resOut *= 1.0 - tremDepth + tremDepth * lfo;
    }
    double wet = dry + resOut;
    double env = 1.0;
    if (i < attS) env = (double)i / attS;
    else if (i >= releaseStart) { double r = (double)(i - releaseStart) / relS; env = r >= 1.0 ? 0.0 : 1.0 - r; }
    if (env <= 0.0 && i > releaseStart) break;
    double v = wet * env * gain;
    long d = startIdx + i;
    if (d < 0 || d >= NS) continue;
    bus[d] += v;
  }
}

// ── SNARE — renderSnare → busSnare. Seed floor(start*6271)|1. ──────────────
static void synth_snare(double start, double gain, double toneT60, double noiseT60,
                        double toneAmt, double noiseAmt, double f1, double f2, double *bus) {
  if (gain == 0.0) return;
  const double dt = 1.0 / SR, LN1000 = log(1000.0);
  double tail = (toneT60 > noiseT60 ? toneT60 : noiseT60) * 1.4 + 0.01;
  long nsS = (long)ceil(tail * SR);
  double aT = LN1000 / toneT60, aN = LN1000 / noiseT60;
  Noise noise = noise_make((unsigned int)((long)floor(start * 6271.0) | 1L));
  double hp = 0.0, prevN = 0.0;
  long startIdx = (long)floor(start * SR);
  for (long i = 0; i < nsS; i++) {
    double t = i * dt;
    double tone = (sin(TWO_PI * f1 * t) * 0.6 + sin(TWO_PI * f2 * t) * 0.4) * exp(-aT * t) * toneAmt;
    double nRaw = noise_next(&noise);
    hp = 0.85 * (hp + nRaw - prevN); prevN = nRaw;
    double nz = hp * exp(-aN * t) * noiseAmt;
    long d = startIdx + i;
    if (d < 0 || d >= NS) continue;
    bus[d] += (tone + nz) * gain;
  }
}

// ── HAT — renderHat → busHat. Seed floor(start*5147)|1. ────────────────────
static void synth_hat(double start, double gain, double t60, double *bus) {
  if (gain == 0.0) return;
  const double dt = 1.0 / SR, LN1000 = log(1000.0);
  double tail = t60 * 1.5 + 0.005;
  long nsH = (long)ceil(tail * SR);
  double a = LN1000 / t60;
  Noise noise = noise_make((unsigned int)((long)floor(start * 5147.0) | 1L));
  double hp = 0.0, prevN = 0.0, hp2 = 0.0, prevH = 0.0;
  long startIdx = (long)floor(start * SR);
  for (long i = 0; i < nsH; i++) {
    double t = i * dt;
    double nRaw = noise_next(&noise);
    hp = 0.92 * (hp + nRaw - prevN); prevN = nRaw;
    hp2 = 0.92 * (hp2 + hp - prevH); prevH = hp;
    long d = startIdx + i;
    if (d < 0 || d >= NS) continue;
    bus[d] += hp2 * exp(-a * t) * gain;
  }
}

// renderHat into a freshly-zeroed segment (for reverse-hat). Returns length;
// caller frees *seg.
static long render_hat_seg(double gain, double t60, double **seg) {
  const double dt = 1.0 / SR, LN1000 = log(1000.0);
  double tail = t60 * 1.5 + 0.005;
  long nsH = (long)ceil(tail * SR);
  double a = LN1000 / t60;
  Noise noise = noise_make((unsigned int)((long)floor(0.0 * 5147.0) | 1L));  // startSec 0 in rhat
  double *out = malloc((size_t)nsH * sizeof(double));
  if (!out) { *seg = NULL; return 0; }
  double hp = 0.0, prevN = 0.0, hp2 = 0.0, prevH = 0.0;
  for (long i = 0; i < nsH; i++) {
    double t = i * dt;
    double nRaw = noise_next(&noise);
    hp = 0.92 * (hp + nRaw - prevN); prevN = nRaw;
    hp2 = 0.92 * (hp2 + hp - prevH); prevH = hp;
    out[i] = hp2 * exp(-a * t) * gain;
  }
  *seg = out; return nsH;
}

// ── SHAKER — renderShaker → busShak. Seed floor(start*4019)|1. ─────────────
static void synth_shaker(double start, double gain, double t60, double attack, double *bus) {
  if (gain == 0.0) return;
  const double dt = 1.0 / SR, LN1000 = log(1000.0);
  long attS = (long)floor(attack * SR); if (attS < 1) attS = 1;
  double tail = t60 * 1.6 + 0.01;
  long nsS = (long)ceil(tail * SR);
  double a = LN1000 / t60;
  Noise noise = noise_make((unsigned int)((long)floor(start * 4019.0) | 1L));
  double hp = 0.0, prevN = 0.0, lp = 0.0;
  double lpC = (TWO_PI * 6500.0) / SR; if (lpC > 1.0) lpC = 1.0;
  long startIdx = (long)floor(start * SR);
  for (long i = 0; i < nsS; i++) {
    double t = i * dt;
    double nRaw = noise_next(&noise);
    hp = 0.7 * (hp + nRaw - prevN); prevN = nRaw;
    lp += lpC * (hp - lp);
    double env = exp(-a * t);
    if (i < attS) env *= (double)i / (double)attS;
    long d = startIdx + i;
    if (d < 0 || d >= NS) continue;
    bus[d] += lp * env * gain;
  }
}

// ── BASS-PERC — renderBassPerc → busSub. Seed (floor(start*7919)+midi*53)|1. ─
static void synth_bassperc(double start, double midi, double durSec, double gain,
                           double pitchUp, double pitchT, double decay, double drive,
                           double sub, double clickAmt, double *bus) {
  if (gain == 0.0) return;
  const double dt = 1.0 / SR, LN1000 = log(1000.0);
  double f0 = midi_to_freq(midi);
  double tail = (durSec > decay ? durSec : decay) * 1.25 + 0.03;
  long nsB = (long)ceil(tail * SR);
  double ampA = LN1000 / decay;
  double fHi = f0 * pow(2.0, pitchUp / 12.0);
  Noise noise = noise_make((unsigned int)(((long)floor(start * 7919.0) + (long)(midi * 53.0)) | 1L));
  long relS = (long)floor(0.03 * SR);
  long gateS = (long)floor(durSec * SR);
  double phase = 0.0, phaseSub = 0.0, norm = tanh(drive);
  long startIdx = (long)floor(start * SR);
  for (long i = 0; i < nsB; i++) {
    double t = i * dt;
    double f = f0 + (fHi - f0) * exp(-t / pitchT);
    phase += f * dt;
    phaseSub += (f * 0.5) * dt;
    double body = sin(TWO_PI * phase) + sub * sin(TWO_PI * phaseSub);
    body = tanh(body * drive) / norm;
    double env = exp(-ampA * t);
    if (i > gateS) { double r = (double)(i - gateS) / relS; env *= r >= 1.0 ? 0.0 : 1.0 - r; }
    double s = body * env;
    if (t < 0.005) s += noise_next(&noise) * clickAmt * exp(-t / 0.0015);
    long d = startIdx + i;
    if (d >= 0 && d < NS) bus[d] += s * gain;
    if (env <= 0.0 && i > gateS) break;
  }
}

// ── SCRATCH — renderScratch → busScr. Seed (floor(start*3301)+midi*17)|1. ───
static void synth_scratch(double start, double midi, double gain, double dur,
                          double rate, double depth, double gateHz, double gateDuty,
                          double tone, double *bus) {
  const double dt = 1.0 / SR;
  double f0 = midi_to_freq(midi);
  Noise noise = noise_make((unsigned int)(((long)floor(start * 3301.0) + (long)(midi * 17.0)) | 1L));
  long srcLen = (long)ceil(0.4 * SR);
  double *src = malloc((size_t)srcLen * sizeof(double));
  if (!src) return;
  int nh = (int)floor(8.0 + tone * 22.0); if (nh < 4) nh = 4;
  double bp = 0.0, lp = 0.0;
  double fc = 900.0, k = 2.0 * sin(M_PI * fc / SR), damp = 0.22;
  for (long i = 0; i < srcLen; i++) {
    double tt = i * dt, s = 0.0;
    for (int h = 1; h <= nh; h++) s += sin(TWO_PI * f0 * h * tt) / h;
    s = s * 0.5 + noise_next(&noise) * 0.18 * tone;
    double hi = s - lp - damp * bp; bp += k * hi; lp += k * bp;
    src[i] = s * 0.5 + bp * 0.9;
  }
  double sp = 0.0; for (long i = 0; i < srcLen; i++) { double a = fabs(src[i]); if (a > sp) sp = a; }
  if (sp > 0.0) { double n = 0.9 / sp; for (long i = 0; i < srcLen; i++) src[i] *= n; }
  long nsSc = (long)ceil(dur * SR);
  double center = srcLen * 0.5;
  double amp = depth * SR / (TWO_PI * rate);
  long attS = (long)floor(0.004 * SR), relS = (long)floor(0.02 * SR);
  double gSmooth = 1.0;
  long startIdx = (long)floor(start * SR);
  for (long i = 0; i < nsSc; i++) {
    double tt = i * dt;
    double pos = center - amp * cos(TWO_PI * rate * tt);
    if (pos < 0.0) pos = 0.0; else if (pos >= srcLen - 1) pos = srcLen - 2;
    long i0 = (long)floor(pos); double frc = pos - i0;
    double smp = src[i0] * (1.0 - frc) + src[i0 + 1] * frc;
    if (gateHz > 0.0) {
      double ph = fmod(tt * gateHz, 1.0);
      gSmooth += ((ph < gateDuty ? 1.0 : 0.0) - gSmooth) * 0.02;
      smp *= gSmooth;
    }
    double env = 1.0;
    if (i < attS) env = (double)i / attS;
    else if (i > nsSc - relS) { double e = (double)(nsSc - i) / relS; env = e > 0.0 ? e : 0.0; }
    long d = startIdx + i;
    if (d >= 0 && d < NS) bus[d] += smp * env * gain;
  }
  free(src);
}

// ── SCREAM — renderScream → busScream. Seed (floor(start*2719)+midi*29)|1. ──
static void synth_scream(double start, double midi, double gain, double dur,
                         double bend, double rasp, double *bus) {
  const double dt = 1.0 / SR;
  if (rasp < 0.0) rasp = 0.0; else if (rasp > 1.0) rasp = 1.0;
  double f0 = midi_to_freq(midi);
  long nsSc = (long)ceil(dur * SR);
  Noise noise = noise_make((unsigned int)(((long)floor(start * 2719.0) + (long)(midi * 29.0)) | 1L));
  double bp1 = 0.0, lp1 = 0.0, bp2 = 0.0, lp2 = 0.0;
  double k1 = 2.0 * sin(M_PI * 950.0 / SR), k2 = 2.0 * sin(M_PI * 2500.0 / SR), dmp = 0.16;
  double ph = 0.0, phSub = 0.0, roughLP = 0.0;
  double drive = 1.0 + rasp * 3.0, norm = tanh(drive);
  long startIdx = (long)floor(start * SR);
  for (long i = 0; i < nsSc; i++) {
    double tt = i * dt, u = tt / dur;
    double um = u * 1.35; if (um > 1.0) um = 1.0;
    double rise = pow(2.0, (bend / 12.0) * sin(M_PI * um));
    double vibrato = 1.0 + 0.035 * sin(TWO_PI * 17.0 * tt);
    roughLP += (noise_next(&noise) - roughLP) * 0.4;
    double f = f0 * rise * vibrato * (1.0 + rasp * 0.05 * roughLP);
    ph += f * dt; phSub += f * 0.5 * dt;
    double s = 0.0; int nh = (int)floor(SR / 2.0 / f); if (nh > 24) nh = 24;
    for (int h = 1; h <= nh; h++) s += sin(TWO_PI * ph * h) / h;
    double u4 = u * 4.0; if (u4 > 1.0) u4 = 1.0;
    s = s * 0.5 + rasp * 0.45 * sin(TWO_PI * phSub) * u4;
    s += noise_next(&noise) * rasp * 0.22;
    double hi1 = s - lp1 - dmp * bp1; bp1 += k1 * hi1; lp1 += k1 * bp1;
    double hi2 = s - lp2 - dmp * bp2; bp2 += k2 * hi2; lp2 += k2 * bp2;
    double v = s * 0.35 + bp1 * 0.8 + bp2 * 0.5;
    v = tanh(v * drive) / norm;
    double u12 = u * 12.0; if (u12 > 1.0) u12 = 1.0;
    double u11 = u * 1.1; if (u11 > 1.0) u11 = 1.0;
    double env = u12 * (0.55 + 0.45 * sin(M_PI * u11));
    if (u > 0.85) { double e = (1.0 - u) / 0.15; env *= e > 0.0 ? e : 0.0; }
    long d = startIdx + i;
    if (d >= 0 && d < NS) bus[d] += v * env * gain;
  }
}

// ── REVERSE BELL — renderReverseBell, lands transient on landSec → bus. ────
static void synth_reverse_bell(double landSec, double dur, double gain, double decay,
                               double ratio, int nm, const double *midis, double *bus) {
  const double dt = 1.0 / SR;
  long nsR = (long)ceil(dur * SR);
  double *fwd = calloc((size_t)nsR, sizeof(double));
  if (!fwd) return;
  for (int mi = 0; mi < nm; mi++) {
    double fc = midi_to_freq(midis[mi]), fm = fc * ratio;
    double phc = 0.0, phm = 0.0;
    for (long i = 0; i < nsR; i++) {
      double tt = i * dt;
      double env = exp(-tt * decay);
      double miIdx = 6.0 * exp(-tt * 11.0);
      phm += TWO_PI * fm * dt;
      phc += TWO_PI * fc * dt + sin(phm) * miIdx * dt;
      fwd[i] += sin(phc) * env;
    }
  }
  double rt = sqrt((double)(nm > 1 ? nm : 1));
  double norm = gain / rt;
  long startIdx = (long)floor(landSec * SR) - nsR;
  for (long i = 0; i < nsR; i++) {
    long d = startIdx + i;
    if (d >= 0 && d < NS) bus[d] += fwd[nsR - 1 - i] * norm;
  }
  free(fwd);
}

// ── REVERSE KICK — renderReverseKick → busKick. Seed floor(landSec*8311)|1. ─
static void synth_reverse_kick(double landSec, double dur, double gain, double fStart,
                               double fEnd, double pitchT, double ampT60, double drive,
                               double *bus) {
  const double dt = 1.0 / SR, LN1000 = log(1000.0);
  long nsR = (long)ceil(dur * SR);
  double *fwd = calloc((size_t)nsR, sizeof(double));
  if (!fwd) return;
  double ampA = LN1000 / ampT60;
  Noise noise = noise_make((unsigned int)((long)floor(landSec * 8311.0) | 1L));
  double norm = tanh(drive), phase = 0.0;
  for (long i = 0; i < nsR; i++) {
    double t = i * dt;
    double f = fEnd + (fStart - fEnd) * exp(-t / pitchT);
    phase += f * dt;
    double body = tanh(sin(TWO_PI * phase) * drive) / norm;
    double s = body * exp(-ampA * t);
    if (t < 0.006) s += noise_next(&noise) * 0.5 * exp(-t / 0.0012);
    fwd[i] = s;
  }
  long startIdx = (long)floor(landSec * SR) - nsR;
  for (long i = 0; i < nsR; i++) {
    long d = startIdx + i;
    if (d >= 0 && d < NS) bus[d] += fwd[nsR - 1 - i] * gain;
  }
  free(fwd);
}

// ── HELD PAD — PAD() → busPad. midis is a chord; each writes the same shape. ─
static void synth_pad(double start, double dur, double g, double attFrac, int nm,
                      const double *midis, double *bus) {
  const double dt = 1.0 / SR;
  long n0 = (long)floor(start * SR);
  long len = (long)ceil((dur + 0.5) * SR);
  long att = (long)floor(dur * attFrac * SR); if (att < 1) att = 1;
  long rel = (long)floor(0.7 * SR);
  for (int mi = 0; mi < nm; mi++) {
    double f = midi_to_freq(midis[mi]); double phase = 0.0;
    for (long i = 0; i < len; i++) {
      long d = n0 + i; phase += f * dt;
      if (d < 0 || d >= NS) continue;
      double env = 1.0;
      if (i < att) env = (double)i / att;
      else if (i > len - rel) { double e = (double)(len - i) / rel; env = e > 0.0 ? e : 0.0; }
      double s = sin(TWO_PI * phase)
               + 0.5 * sin(TWO_PI * phase * 1.5)
               + 0.35 * sin(TWO_PI * phase * 0.5);
      bus[d] += s * env * g * 0.45;
    }
  }
}

// ── WATER DROP — drop() → busDrop. fast upward chirp + click attack. ───────
static void synth_drop(double t0, double f0, double g, double dur, double *bus) {
  const double dt = 1.0 / SR;
  long n0 = (long)floor(t0 * SR), ln = (long)ceil(dur * SR);
  double ph = 0.0;
  for (long i = 0; i < ln; i++) {
    long d = n0 + i; if (d < 0 || d >= NS) { ph += 0.0; }
    double u = (double)i / ln;
    double u3 = u * 3.4; if (u3 > 1.0) u3 = 1.0;
    double f = f0 * (1.0 + 1.7 * u3);
    ph += f * dt;
    if (d < 0 || d >= NS) continue;
    double env = exp(-u * 6.2);
    if (i < 20) env *= (double)i / 20.0;
    double s = sin(TWO_PI * ph) + 0.16 * sin(2.0 * TWO_PI * ph);
    bus[d] += s * env * g * 0.6;
  }
}

// ── DYNABELL — additive odd-harmonic bell whose waveform SHAPE morphs
// sine → triangle → square. Mirrors dynabell() in render-fluttabap360.mjs:
// odd-harmonic target spectra (sine=fundamental, tri=odd/n² alt-sign,
// square=odd/n same-sign), RMS-normalised, higher partials damp faster. ──
static void synth_dynabell(double start, double f, double dur, double g, double shape,
                           double bend, double bendTime, double vibD, double vibHz, double tail, double *bus) {
  int nH = (int)floor(0.45 * (double)SR / f); nH |= 1;          // odd, under Nyquist
  if (nH > 31) nH = 31; if (nH < 1) nH = 1;
  double amps[33]; for (int j = 0; j < 33; j++) amps[j] = 0.0;
  double seg = (shape < 0.0 ? 0.0 : (shape > 1.0 ? 1.0 : shape)) * 2.0;
  int k = 0;
  for (int n = 1; n <= nH; n += 2, k++) {
    double sine = (n == 1) ? 1.0 : 0.0;
    double tri = (8.0 / (M_PI * M_PI)) * (1.0 / ((double)n * (double)n)) * ((k % 2 == 0) ? 1.0 : -1.0);
    double sq  = (4.0 / M_PI) * (1.0 / (double)n);
    amps[n] = seg <= 1.0 ? sine + (tri - sine) * seg : tri + (sq - tri) * (seg - 1.0);
  }
  double e = 0.0; for (int n = 1; n <= nH; n += 2) e += amps[n] * amps[n];
  double norm = e > 0.0 ? 1.0 / sqrt(e) : 1.0;
  for (int n = 1; n <= nH; n += 2) amps[n] *= norm;
  long n0 = (long)floor(start * SR);
  long len = (long)ceil((dur * 1.35 * tail + 0.08) * SR);
  long attS = (long)floor(0.003 * SR); if (attS < 1) attS = 1;
  double relA = log(900.0) / (dur * 1.35 * tail), dt = 1.0 / SR;   // tail stretches the ring
  double bph = 0.0;   // fundamental phase in CYCLES (accumulated so pitch can move)
  for (long i = 0; i < len; i++) {
    long d = n0 + i; if (d < 0 || d >= NS) continue;
    double tsec = i * dt;
    double semis = 0.0;
    if (bend != 0.0 && tsec < bendTime) semis += bend * (1.0 - tsec / bendTime);
    if (vibD != 0.0) { double ein = tsec / 0.12; if (ein > 1.0) ein = 1.0; semis += vibD * sin(TWO_PI * vibHz * tsec) * ein; }
    bph += (f * pow(2.0, semis / 12.0)) * dt;
    double att = i < attS ? (double)i / attS : 1.0;
    double s = 0.0;
    for (int n = 1; n <= nH; n += 2) {
      double a = amps[n]; if (a == 0.0) continue;
      double env = exp(-relA * tsec * (1.0 + (n - 1) * 0.08));
      s += a * env * sin(TWO_PI * n * bph);
    }
    bus[d] += s * att * g * 0.62;
  }
}

// ── REVERSE DYNABELL ("boo") — render a dynabell forward into a temp buffer,
// then write it reversed so the loud attack lands at landSec (swell builds
// before it). Non-bendy closed-form phase → bit-exact with the JS revbell. ──
static void synth_revbell(double landSec, double f, double dur, double gain, double shape, double *bus) {
  int nH = (int)floor(0.45 * (double)SR / f); nH |= 1;
  if (nH > 31) nH = 31; if (nH < 1) nH = 1;
  double amps[33]; for (int j = 0; j < 33; j++) amps[j] = 0.0;
  double seg = (shape < 0.0 ? 0.0 : (shape > 1.0 ? 1.0 : shape)) * 2.0;
  int k = 0;
  for (int n = 1; n <= nH; n += 2, k++) {
    double sine = (n == 1) ? 1.0 : 0.0;
    double tri = (8.0 / (M_PI * M_PI)) * (1.0 / ((double)n * (double)n)) * ((k % 2 == 0) ? 1.0 : -1.0);
    double sq  = (4.0 / M_PI) * (1.0 / (double)n);
    amps[n] = seg <= 1.0 ? sine + (tri - sine) * seg : tri + (sq - tri) * (seg - 1.0);
  }
  double e = 0.0; for (int n = 1; n <= nH; n += 2) e += amps[n] * amps[n];
  double norm = e > 0.0 ? 1.0 / sqrt(e) : 1.0;
  for (int n = 1; n <= nH; n += 2) amps[n] *= norm;
  long len = (long)ceil((dur * 1.35 + 0.08) * SR);
  long attS = (long)floor(0.003 * SR); if (attS < 1) attS = 1;
  double relA = log(900.0) / (dur * 1.35), dt = 1.0 / SR;
  double *tmp = malloc((size_t)len * sizeof(double));
  if (!tmp) return;
  for (long i = 0; i < len; i++) {
    double tsec = i * dt;
    double att = i < attS ? (double)i / attS : 1.0;
    double s = 0.0;
    for (int n = 1; n <= nH; n += 2) {
      double a = amps[n]; if (a == 0.0) continue;
      double env = exp(-relA * tsec * (1.0 + (n - 1) * 0.08));
      s += a * env * sin(TWO_PI * f * n * tsec);
    }
    tmp[i] = s * att * 0.62;
  }
  long endIdx = (long)floor(landSec * SR);
  for (long i = 0; i < len; i++) { long d = endIdx - i; if (d >= 0 && d < NS) bus[d] += tmp[i] * gain; }
  free(tmp);
}

// ── VORTEX — performed turntable warble: a saw whose pitch is sucked
// down-and-back-up (the "throooop") through a resonant low-pass wobbled by a
// rhythmic LFO (the wub-wub). Mirrors vortex() in render-fluttabap360.mjs. ──
static void synth_vortex(double start, double f0, double dur, double g,
                         double wubHz, double depthOct, double q, double *bus) {
  long n0 = (long)floor(start * SR), ln = (long)ceil(dur * SR);
  double dt = 1.0 / SR, phase = 0.0, lp = 0.0, bp = 0.0;
  double damp = 1.0 / q; if (damp > 1.8) damp = 1.8;
  for (long i = 0; i < ln; i++) {
    long d = n0 + i; if (d < 0 || d >= NS) continue;
    double u = (double)i / (double)ln, tsec = i * dt;
    double dive = pow(sin(M_PI * u), 0.55);                       // fuller, lingering dip
    double snap = 0.12 * sin(M_PI * u) * sin(TWO_PI * u);         // tail overshoot
    double ripple = 0.22 * sin(TWO_PI * 3.0 * u);
    double pf = pow(2.0, depthOct * -dive + snap + ripple);
    phase += f0 * pf * dt;
    double ph = phase - floor(phase);
    double src = 2.0 * ph - 1.0;
    double wub = 0.5 + 0.5 * sin(TWO_PI * wubHz * tsec);
    double fc = (160.0 + 3200.0 * pf) * (0.42 + 0.58 * wub);      // cutoff TRACKS the pitch dive
    double fcm = fc < (double)SR * 0.45 ? fc : (double)SR * 0.45;
    double fco = 2.0 * sin(M_PI * fcm / SR); if (fco > 0.49) fco = 0.49;
    lp += fco * bp;
    double hp = src - lp - damp * bp;
    bp += fco * hp;
    double swell = sin(M_PI * u);
    bus[d] += lp * (0.35 + 0.65 * wub) * swell * g;
  }
}

// ── FEMBELL — a physically-modeled bell voice. The modal tables (ratio /
// amplitude / decay-tau) are FEM-derived by pop/bell (a finite-element shell
// model), captured at a 880 Hz reference strike. Retuned per note: mode freq =
// ratio·f0, decay scales ∝ 1/f0 (physical), amplitudes pre-normalised. Routed
// to busBell so it inherits the bell pan + reverb scene. 0=handbell 1=church
// 2=bowl. ──
#define FB_REF 880.0
static const int FB_N[3] = { 12, 12, 3 };
static const double FB_RATIO[3][12] = {
  { 1.00000, 3.47900, 3.55900, 4.81000, 5.55400, 6.36200, 6.55800, 7.09100, 7.48300, 8.57600, 8.68800, 10.62100 },  // handbell/bronze
  { 1.00000, 2.53600, 3.48400, 3.54900, 4.97600, 5.72600, 6.15900, 6.88400, 7.32100, 7.73500, 9.05700, 9.18600 },  // church/bronze
  { 0.46100, 0.68800, 1.00000, 0, 0, 0, 0, 0, 0, 0, 0, 0 },                                                          // bowl/glass
};
static const double FB_AMP[3][12] = {
  { 0.18428, 0.09880, 0.09768, 0.08402, 0.07819, 0.06824, 0.07196, 0.06920, 0.06737, 0.06293, 0.06252, 0.05479 },
  { 0.18239, 0.06963, 0.09772, 0.09682, 0.08176, 0.07622, 0.07217, 0.06951, 0.06741, 0.06558, 0.06061, 0.06018 },
  { 0.31478, 0.25772, 0.42749, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
};
static const double FB_TAU[3][12] = {
  { 1.8100, 0.5200, 0.5100, 0.3800, 0.3300, 0.2800, 0.2800, 0.2600, 0.2400, 0.2100, 0.2100, 0.1700 },
  { 1.8100, 0.7100, 0.5200, 0.5100, 0.3600, 0.3200, 0.2900, 0.2600, 0.2500, 0.2300, 0.2000, 0.2000 },
  { 13.0700, 8.7600, 6.0300, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
};
static void synth_fembell(double start, double f0, double gain, int preset, double tauScale, double *bus) {
  if (preset < 0 || preset > 2) preset = 0;
  int nm = FB_N[preset];
  long n0 = (long)floor(start * SR);
  double pitchTau = (FB_REF / f0) * tauScale;            // physical: higher note → shorter ring
  double maxTau = 0.0;
  for (int k = 0; k < nm; k++) { double tp = FB_TAU[preset][k] * pitchTau; if (tp > maxTau) maxTau = tp; }
  long len = (long)((maxTau * 6.0 + 0.05) * SR); if (len > NS) len = NS;
  long attS = (long)(0.002 * SR); if (attS < 1) attS = 1;
  double dt = 1.0 / SR;
  for (int k = 0; k < nm; k++) {
    double f = FB_RATIO[preset][k] * f0;
    if (f >= SR * 0.49) continue;
    double a = FB_AMP[preset][k], tau = FB_TAU[preset][k] * pitchTau;
    double inc = f * dt, ph = 0.0;
    for (long i = 0; i < len; i++) {
      long d = n0 + i;
      if (d >= 0 && d < NS) {
        double env = exp(-(double)i / (tau * SR));
        double att = i < attS ? (double)i / attS : 1.0;
        bus[d] += a * env * att * sin(TWO_PI * ph) * gain;
      }
      ph += inc; if (ph >= 1.0) ph -= 1.0;
    }
  }
}

// ── PHYSMAR — a physically-modeled MARIMBA BAR (modal, struck). Two endpoint
// mode tables — WOOD (morph=0): a rosewood bar arch-cut to the classic 1:4:10
// tuning (ratios ~[1, 3.98, 9.92, 14.9, 20.5]), amps falling off, SHORT decays
// (fundamental ~1.15 s at C4, uppers much faster) → warm. GLASS (morph=1):
// vibraphone-metallic — slightly stretched ratios [1, 4.08, 10.4, 16.1, 22.8],
// 2.5–4× longer decays, brighter uppers, and a slow amplitude BEAT per mode
// (two components detuned 0.5–1.5 Hz) so the ring stays alive. morph lerps
// ratios/taus/amps. strike = mallet hardness: soft (0) = fundamental-heavy,
// ~4 ms contact; hard (1) = strong uppers + a ~2.5 ms deterministic click,
// ~0.5 ms attack. Decays scale ∝ 1/sqrt(f0/C4) (bar physics, gentler than a
// shell); after `dur` the whole note releases 3× faster (gate), so short
// notes stay short but still ring a touch. Deterministic; → busLead. ──
#define PM_REF 261.63          /* C4 — the tau tables are voiced here */
#define PM_NM 5
static const double PM_R_WOOD[PM_NM]  = { 1.00, 3.98, 9.92, 14.90, 20.50 };
static const double PM_R_GLASS[PM_NM] = { 1.00, 4.08, 10.40, 16.10, 22.80 };
static const double PM_A_WOOD[PM_NM]  = { 1.000, 0.340, 0.130, 0.060, 0.030 };
static const double PM_A_GLASS[PM_NM] = { 1.000, 0.440, 0.230, 0.130, 0.080 };
static const double PM_T_WOOD[PM_NM]  = { 1.150, 0.300, 0.110, 0.065, 0.042 };  /* s at C4 */
static const double PM_T_GLASS[PM_NM] = { 3.600, 1.150, 0.440, 0.240, 0.150 };  /* ~3.1–3.9× wood */
static void synth_physmar(double start, double f0, double dur, double gain,
                          double morph, double strike, double *bus) {
  if (f0 <= 0.0 || dur <= 0.0) return;
  if (morph < 0.0) morph = 0.0; if (morph > 1.0) morph = 1.0;
  if (strike < 0.0) strike = 0.0; if (strike > 1.0) strike = 1.0;
  long n0 = (long)floor(start * SR);
  double pitchTau = sqrt(PM_REF / f0);                    // bar: ring ∝ 1/sqrt(f)
  double tilt = 0.30 + 0.70 * strike;                     // per-mode amp tilt base
  double maxTau = 0.0;
  for (int k = 0; k < PM_NM; k++) {
    double tp = (PM_T_WOOD[k] + (PM_T_GLASS[k] - PM_T_WOOD[k]) * morph) * pitchTau;
    if (tp > maxTau) maxTau = tp;
  }
  // gate: decay 1/tau until dur, then 3/tau — size the buffer for both legs.
  long len = (long)((dur + maxTau * 2.5 + 0.05) * SR); if (len > NS) len = NS;
  long gateS = (long)(dur * SR); if (gateS < 1) gateS = 1;
  long attS = (long)((0.0005 + 0.0035 * (1.0 - strike)) * SR); if (attS < 1) attS = 1;
  double dt = 1.0 / SR;
  for (int k = 0; k < PM_NM; k++) {
    double ratio = PM_R_WOOD[k] + (PM_R_GLASS[k] - PM_R_WOOD[k]) * morph;
    double f = ratio * f0;
    if (f >= SR * 0.45) continue;                         // anti-alias: drop mode
    double a = PM_A_WOOD[k] + (PM_A_GLASS[k] - PM_A_WOOD[k]) * morph;
    a *= pow(tilt, (double)k);                            // mallet hardness tilt
    double tau = (PM_T_WOOD[k] + (PM_T_GLASS[k] - PM_T_WOOD[k]) * morph) * pitchTau;
    // glass shimmer: split the mode into two components beating at 0.5–1.5 Hz.
    double beatHz = (0.5 + 0.25 * (double)k) * morph;     // wood → 0 (no beat)
    double bmix = 0.35 * morph;                           // beat component share
    double aA = a * (1.0 - bmix), aB = a * bmix;
    double incA = (f - beatHz * 0.5) * dt, incB = (f + beatHz * 0.5) * dt;
    double phA = 0.0, phB = 0.0;
    double d1 = exp(-1.0 / (tau * SR)), d2 = exp(-3.0 / (tau * SR));
    double e = 1.0;
    for (long i = 0; i < len; i++) {
      long d = n0 + i;
      if (d >= 0 && d < NS) {
        double att = i < attS ? (double)i / attS : 1.0;
        bus[d] += (aA * sin(TWO_PI * phA) + aB * sin(TWO_PI * phB)) * e * att * gain;
      }
      e *= (i < gateS) ? d1 : d2;
      phA += incA; if (phA >= 1.0) phA -= 1.0;
      phB += incB; if (phB >= 1.0) phB -= 1.0;
    }
  }
  // hard-mallet click: brief deterministic noise thock (~2.5 ms), lightly
  // lowpassed so it reads as contact, not hiss. Seeded like synth_kick.
  double clickAmt = 0.28 * strike * strike;
  if (clickAmt > 0.0) {
    Noise noise = noise_make((unsigned int)((long)floor(start * 9173.0) | 1L));
    long clk = (long)(0.0025 * SR);
    double lp = 0.0;
    for (long i = 0; i < clk; i++) {
      long d = n0 + i;
      double white = noise_next(&noise);
      lp += (white - lp) * 0.45;
      if (d >= 0 && d < NS)
        bus[d] += lp * clickAmt * exp(-(double)i / (0.0008 * SR)) * gain;
    }
  }
}

// ── GONG — a gnarly, long-blooming gong. Dense 24-mode inharmonic spectrum
// (FEM steel/glass shell, ref A3=220 Hz), but voiced as a GONG: the high
// shimmer modes SWELL IN after the strike (the characteristic gong bloom),
// paired modes are detuned a hair so they beat/shimmer, a bright noise CRASH
// rides the attack, and the whole thing rings LONG (tau capped ~14 s) so the
// wash can truly be felt. Routed to busCave → centered + drenched in reverb. ──
#define GONG_REF 220.0
static const double GONG_R[24] = {
  0.6140, 1.0000, 1.5960, 2.6630, 2.8810, 3.8970, 4.2330, 5.2660, 5.6530, 6.0150,
  6.5790, 6.7830, 7.6680, 8.5580, 8.9970, 9.6130, 9.8170, 9.8770, 10.8950, 11.1800,
  11.4680, 12.3570, 12.6280, 12.8780 };
static const double GONG_A[24] = {
  0.02192, 0.15469, 0.01543, 0.08958, 0.00100, 0.07836, 0.00692, 0.06741, 0.05420, 0.00076,
  0.06031, 0.05940, 0.00430, 0.05288, 0.01413, 0.04989, 0.04937, 0.04745, 0.04687, 0.04626,
  0.00196, 0.04401, 0.03182, 0.00108 };
static const double GONG_T[24] = {
  19.650, 12.060, 7.560, 4.530, 4.190, 3.090, 2.850, 2.290, 2.130, 2.000,
  1.830, 1.780, 1.570, 1.410, 1.340, 1.250, 1.230, 1.220, 1.110, 1.080,
  1.050, 0.980, 0.950, 0.940 };
static void synth_gong(double start, double f0, double gain, double tauScale, double *bus) {
  long n0 = (long)floor(start * SR);
  double pitchTau = (GONG_REF / f0) * tauScale;
  double dt = 1.0 / SR;
  unsigned int rng = (unsigned int)(start * 4129.0) * 2u + 1u;
  double maxTau = 0.0;
  for (int k = 0; k < 24; k++) { double tp = GONG_T[k] * pitchTau; if (tp > 14.0) tp = 14.0; if (tp > maxTau) maxTau = tp; }
  long len = (long)((maxTau * 4.0 + 0.1) * SR); if (len > NS) len = NS;
  for (int k = 0; k < 24; k++) {
    double f = GONG_R[k] * f0 * (1.0 + ((k % 2) ? 0.0015 : -0.0012));   // detune → shimmer/beat
    if (f >= SR * 0.49) continue;
    double a = GONG_A[k];
    double tau = GONG_T[k] * pitchTau; if (tau > 14.0) tau = 14.0;
    // bloom: body modes hit now; higher shimmer modes swell IN over ~0.1–1.2 s
    double bloom = GONG_R[k] <= 1.2 ? 0.0 : (0.06 + 0.09 * GONG_R[k]) * tauScale;
    if (bloom > 1.3) bloom = 1.3;
    double inc = f * dt, ph = 0.0;
    for (long i = 0; i < len; i++) {
      long d = n0 + i;
      if (d >= 0 && d < NS) {
        double t = i * dt;
        double env = exp(-t / tau);
        if (bloom > 0.0) env *= (1.0 - exp(-t / bloom));
        bus[d] += a * env * sin(TWO_PI * ph) * gain;
      }
      ph += inc; if (ph >= 1.0) ph -= 1.0;
    }
  }
  // CRASH — a short bright filtered-noise burst on the strike (the gnarly attack)
  double lp = 0.0; long clk = (long)(0.09 * SR);
  for (long i = 0; i < clk; i++) {
    long d = n0 + i; if (d < 0 || d >= NS) continue;
    rng = rng * 1664525u + 1013904223u;
    double white = ((double)rng / 4294967296.0) * 2.0 - 1.0;
    lp = 0.6 * white + 0.4 * lp;
    double env = exp(-(double)i / (0.025 * SR));
    bus[d] += (white - 0.5 * lp) * env * 0.12 * gain;
  }
}

// ── FLUTE — lifted from gm_synth.c's Cook jet-waveguide pipe model (the AC
// physically-modeled flute, GM 74 / pan-flute 76). A bore delay line excited by
// a jet (cubic limit-cycle) + blowing noise, with a breathy onset chiff and
// vibrato. Blowing pressure follows the note gate so it swells in/out — perfect
// for AMBIENT, sustained flute. preset 0=flute, 1=panflute (breathier). ──
static inline double gm_frac_read(const float *buf, int N, int w, double delay) {
  if (delay < 0.0) delay = 0.0;
  if (delay > (double)(N - 2)) delay = (double)(N - 2);
  double rd = (double)w - delay;
  while (rd < 0.0) rd += (double)N;
  int i0 = (int)rd, i1 = (i0 + 1) % N;
  double f = rd - (double)i0;
  return (double)buf[i0] * (1.0 - f) + (double)buf[i1] * f;
}
typedef struct { double loopDamp, breathMax, noise, vibHz, vibDepth, jetRatio, outScale; } FluteP;
static const FluteP FLUTE_P[2] = {
  { 0.16, 0.55, 0.08, 5.0, 0.005, 0.32, 1.5 },   // 0 flute — canonical Cook model
  { 0.18, 0.55, 0.22, 4.0, 0.006, 0.40, 1.5 },   // 1 pan flute — breathier, more chiff (ambient)
};
static void synth_flute(double start, double f0, double dur, double gain, int preset,
                        double attackSec, double relSec, double *bus) {
  if (preset < 0 || preset > 1) preset = 0;
  const FluteP *p = &FLUTE_P[preset];
  const int N = 4096;
  float *ks = calloc((size_t)N, sizeof(float));
  if (!ks) return;
  int w = 0;
  double loop_lp = 0.0, hp_x1 = 0.0, hp_y1 = 0.0, vibPhase = 0.0, vibInc = p->vibHz / SR;
  unsigned int rng = (unsigned int)(start * 9176.0) * 2u + 1u;
  long n0 = (long)floor(start * SR);
  long len = (long)((dur + relSec + 0.05) * SR);
  long ampAtt = (long)(attackSec * SR); if (ampAtt < 1) ampAtt = 1;
  long ampRel = (long)(relSec * SR);    if (ampRel < 1) ampRel = 1;
  long susEnd = (long)(dur * SR);
  for (long i = 0; i < len; i++) {
    long d = n0 + i;
    // note GATE — blowing pressure swells in, holds, releases (the ambient envelope)
    double gate;
    if (i < ampAtt) gate = (double)i / ampAtt;
    else if (i < susEnd) gate = 1.0;
    else { double r = (double)(i - susEnd) / ampRel; gate = r >= 1.0 ? 0.0 : 1.0 - r; }
    double onset = gate;
    double vib = p->vibDepth * sin(TWO_PI * vibPhase);
    vibPhase += vibInc; if (vibPhase >= 1.0) vibPhase -= 1.0;
    double delay = (SR / f0) * (1.0 + vib);
    if (delay > N - 2) delay = N - 2;
    double jet_delay = delay * p->jetRatio; if (jet_delay < 1.0) jet_delay = 1.0;
    rng = rng * 1664525u + 1013904223u;
    double white = ((double)rng / 4294967296.0) * 2.0 - 1.0;
    double breath = p->breathMax * gate;
    double turb = p->noise * white * (0.4 + 0.6 * (1.0 - onset));
    double excitation = breath + breath * turb;
    double bore_out = gm_frac_read(ks, N, w, delay);
    loop_lp = (1.0 - p->loopDamp) * bore_out + p->loopDamp * loop_lp;
    double feedback = loop_lp;
    double jet_in = excitation + feedback * 0.6;
    double jet = gm_frac_read(ks, N, w, jet_delay);
    double pd = jet_in - 0.5 * jet;
    if (pd > 1.0) pd = 1.0; else if (pd < -1.0) pd = -1.0;
    double nl = pd * (pd * pd - 1.0);
    double into_bore = nl + feedback;
    double y = into_bore - hp_x1 + 0.995 * hp_y1;   // DC block
    hp_x1 = into_bore; hp_y1 = y;
    ks[w] = (float)y;
    w = (w + 1) % N;
    double out = y * p->outScale;
    if (out > 2.0) out = 2.0; else if (out < -2.0) out = -2.0;
    // the Cook jet model is low-amplitude (gm_synth makes it up in the mixer);
    // bring it up to bus level here so the flute sits with the other voices.
    if (d >= 0 && d < NS) bus[d] += out * gate * gain * 11.0;
  }
  free(ks);
}

// ── REVERB — reverbInPlace (4 combs + 2 allpass) on a mono bus in place. ───
static void reverb_in_place(double *buf, long n, double wet, double decay) {
  const double combDelays[4] = {0.0297, 0.0371, 0.0411, 0.0437};
  const double apDelays[2] = {0.005, 0.0017};
  double *out = calloc((size_t)n, sizeof(double));
  if (!out) return;
  for (int c = 0; c < 4; c++) {
    long d = (long)floor(combDelays[c] * SR); if (d < 1) d = 1;
    double *ring = calloc((size_t)d, sizeof(double));
    if (!ring) { free(out); return; }
    for (long i = 0; i < n; i++) { double y = ring[i % d]; out[i] += y; ring[i % d] = buf[i] + y * decay; }
    free(ring);
  }
  for (long i = 0; i < n; i++) out[i] /= 4.0;
  for (int p = 0; p < 2; p++) {
    long d = (long)floor(apDelays[p] * SR); if (d < 1) d = 1;
    double *ring = calloc((size_t)d, sizeof(double)); double g = 0.7;
    if (!ring) { free(out); return; }
    for (long i = 0; i < n; i++) { double bd = ring[i % d], y = -g * out[i] + bd; ring[i % d] = out[i] + g * y; out[i] = y; }
    free(ring);
  }
  for (long i = 0; i < n; i++) buf[i] = buf[i] * (1.0 - wet) + out[i] * wet;
  free(out);
}

// ── DELAY — delayInPlace feedback echo on a mono bus in place. ─────────────
static void delay_in_place(double *buf, long n, double time, double fb, double wet) {
  long d = (long)floor(time * SR); if (d < 1) d = 1;
  double *out = calloc((size_t)n, sizeof(double));
  if (!out) return;
  for (long i = 0; i < n; i++) {
    double y = buf[i];
    if (i >= d) y += out[i - d] * fb;
    out[i] = y;
  }
  for (long i = 0; i < n; i++) buf[i] = buf[i] * (1.0 - wet) + out[i] * wet;
  free(out);
}

// --solo parity: restrict synthesis to named voice types (matches the JS
// render's --solo). NULL set = all voices.
static char soloBuf[256]; static char *soloNames[32]; static int soloCount = -1;
static int wanted(const char *name) {
  if (soloCount < 0) return 1;
  for (int i = 0; i < soloCount; i++) if (!strcmp(soloNames[i], name)) return 1;
  return 0;
}

// place a mono bus into the stereo master at (pan, gain).
static void place(const double *bus, double pan, double gain) {
  double lg, rg; pan_lr(pan, &lg, &rg);
  for (long i = 0; i < NS; i++) { double s = bus[i] * gain; outL[i] += (float)(s * lg); outR[i] += (float)(s * rg); }
}


static double *bus_alloc(void) { return calloc((size_t)NS, sizeof(double)); }

int main(int argc, char **argv) {
  const char *scorePath = NULL, *rawPath = NULL;
  int loopMode = 0;
  for (int i = 1; i < argc; i++) {
    if (!strcmp(argv[i], "--raw") && i + 1 < argc) rawPath = argv[++i];
    else if (!strcmp(argv[i], "--loop")) loopMode = 1;
    else if (!strcmp(argv[i], "--solo") && i + 1 < argc) {
      strncpy(soloBuf, argv[++i], sizeof soloBuf - 1);
      soloCount = 0;
      for (char *t = strtok(soloBuf, ","); t && soloCount < 32; t = strtok(NULL, ",")) soloNames[soloCount++] = t;
    }
    else if (argv[i][0] != '-') scorePath = argv[i];
  }
  if (!scorePath || !rawPath) { fprintf(stderr, "usage: flatterbop180 <score.txt> --raw <out.f32> [--loop] [--solo a,b]\n"); return 1; }

  FILE *f = fopen(scorePath, "r");
  if (!f) { fprintf(stderr, "cannot open score: %s\n", scorePath); return 1; }

  // revkick and kick share kickBuf in the JS render: when soloing 'revkick',
  // KICK still fills kickBuf so it gets placed too (and vice versa, EXCEPT the
  // JS riseKick is gated OFF when not soloing 'revkick' — so 'kick' solo has
  // no revkick). Mirror that: soloing revkick also renders kick; soloing kick
  // does NOT render revkick (the JS bake omits revkick lines unless soloed).
  int soloHasRevkick = 0; if (soloCount >= 0) for (int i = 0; i < soloCount; i++) if (!strcmp(soloNames[i], "revkick")) soloHasRevkick = 1;

  char line[1024];
  long evN = 0, skipN = 0;
  int allocated = 0;
  while (fgets(line, sizeof line, f)) {
    if (line[0] == '#' || line[0] == '\n') continue;
    if (!strncmp(line, "sr ", 3)) { SR = atoi(line + 3); continue; }
    if (!strncmp(line, "beat ", 5)) { gBeat = atof(line + 5); continue; }
    if (!strncmp(line, "bar ", 4)) { gBar = atof(line + 4); continue; }
    if (!strncmp(line, "totalbars ", 10)) { gTotalBars = atol(line + 10); continue; }
    if (!strncmp(line, "targetsec ", 10)) { gTargetSec = atof(line + 10); continue; }
    if (!strncmp(line, "finallift ", 10)) { gFinalLiftSec = atof(line + 10); continue; }
    if (!strncmp(line, "sat ", 4)) { sscanf(line + 4, "%lf %lf %lf", &gSatDrive, &gSatBias, &gSatHiss); continue; }
    if (!strncmp(line, "zoosample ", 10)) {
      int zi = -1; char zp[768];
      if (sscanf(line, "zoosample %d %767s", &zi, zp) == 2 && zi >= 0 && zi < ZOO_SLOTS) {
        if (!load_wav_mono(zp, &gZooBuf[zi], &gZooLen[zi]))
          fprintf(stderr, "[flatterbop180] ! zoosample %d failed to load: %s\n", zi, zp);
      }
      continue;
    }
    if (!strncmp(line, "ns ", 3)) {
      NS = atol(line + 3);
      busLead = bus_alloc(); busBass = bus_alloc(); busSpark = bus_alloc();
      busBell = bus_alloc(); busVib = bus_alloc(); busDeep = bus_alloc();
      busRise = bus_alloc(); busScr = bus_alloc(); busScream = bus_alloc();
      busKick = bus_alloc(); busSub = bus_alloc(); busSnare = bus_alloc();
      busHat = bus_alloc(); busShak = bus_alloc(); busCave = bus_alloc();
      busPad = bus_alloc(); busDrop = bus_alloc(); busStream = bus_alloc();
      busFlute = bus_alloc(); busZooL = bus_alloc(); busZooR = bus_alloc();
      outL = calloc((size_t)NS, sizeof(float)); outR = calloc((size_t)NS, sizeof(float));
      if (!busLead || !busStream || !outL || !outR) { fprintf(stderr, "OOM\n"); return 1; }
      allocated = 1;
      continue;
    }
    if (!strncmp(line, "section ", 8)) {
      if (gSecN < 256) { char nm[24]; long b0, bn;
        if (sscanf(line, "section %23s %ld %ld", nm, &b0, &bn) == 3) {
          strncpy(gSec[gSecN].name, nm, 23); gSec[gSecN].name[23] = '\0';
          gSec[gSecN].bar0 = b0; gSec[gSecN].bars = bn; gSecN++;
        }
      }
      continue;
    }
    if (!allocated) continue;
    double a, b, c, d, e, g, h, k;
    if (sscanf(line, "sub %lf %lf %lf %lf", &a, &b, &c, &d) == 4) {
      if (wanted("sub")) { synth_sub(a, b, c, d, busDeep); evN++; }
    } else if (sscanf(line, "kick %lf %lf %lf %lf %lf %lf %lf %lf", &a, &b, &c, &d, &e, &g, &h, &k) == 8) {
      if (wanted("kick") || soloHasRevkick) { synth_kick(a, b, c, d, e, g, h, k, busKick); evN++; }
    } else if (!strncmp(line, "snare ", 6)) {
      double v[8]; if (sscanf(line, "snare %lf %lf %lf %lf %lf %lf %lf %lf", &v[0],&v[1],&v[2],&v[3],&v[4],&v[5],&v[6],&v[7]) == 8) {
        if (wanted("snare")) { synth_snare(v[0],v[1],v[2],v[3],v[4],v[5],v[6],v[7], busSnare); evN++; } } else skipN++;
    } else if (!strncmp(line, "hat ", 4)) {
      double v[3]; if (sscanf(line, "hat %lf %lf %lf", &v[0],&v[1],&v[2]) == 3) {
        if (wanted("hat")) { synth_hat(v[0],v[1],v[2], busHat); evN++; } } else skipN++;
    } else if (!strncmp(line, "shaker ", 7)) {
      double v[4]; if (sscanf(line, "shaker %lf %lf %lf %lf", &v[0],&v[1],&v[2],&v[3]) == 4) {
        if (wanted("shaker")) { synth_shaker(v[0],v[1],v[2],v[3], busShak); evN++; } } else skipN++;
    } else if (!strncmp(line, "subperc ", 8)) {
      double v[10]; if (sscanf(line, "subperc %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf", &v[0],&v[1],&v[2],&v[3],&v[4],&v[5],&v[6],&v[7],&v[8],&v[9]) == 10) {
        if (wanted("subperc")) { synth_bassperc(v[0],v[1],v[2],v[3],v[4],v[5],v[6],v[7],v[8],v[9], busSub); evN++; } } else skipN++;
    } else if (!strncmp(line, "scratch ", 8)) {
      double v[9]; if (sscanf(line, "scratch %lf %lf %lf %lf %lf %lf %lf %lf %lf", &v[0],&v[1],&v[2],&v[3],&v[4],&v[5],&v[6],&v[7],&v[8]) == 9) {
        if (wanted("scratch")) { synth_scratch(v[0],v[1],v[2],v[3],v[4],v[5],v[6],v[7],v[8], busScr); evN++; } } else skipN++;
    } else if (!strncmp(line, "scream ", 7)) {
      double v[6]; if (sscanf(line, "scream %lf %lf %lf %lf %lf %lf", &v[0],&v[1],&v[2],&v[3],&v[4],&v[5]) == 6) {
        if (wanted("scream")) { synth_scream(v[0],v[1],v[2],v[3],v[4],v[5], busScream); evN++; } } else skipN++;
    } else if (!strncmp(line, "dynabell ", 9)) {
      double v[10]; if (sscanf(line, "dynabell %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf", &v[0],&v[1],&v[2],&v[3],&v[4],&v[5],&v[6],&v[7],&v[8],&v[9]) == 10) {
        if (wanted("bell")) { synth_dynabell(v[0],v[1],v[2],v[3],v[4],v[5],v[6],v[7],v[8],v[9], busBell); evN++; } } else skipN++;
    } else if (!strncmp(line, "flute ", 6)) {
      double v[7]; if (sscanf(line, "flute %lf %lf %lf %lf %lf %lf %lf", &v[0],&v[1],&v[2],&v[3],&v[4],&v[5],&v[6]) == 7) {
        if (wanted("flute")) { synth_flute(v[0],v[1],v[2],v[3],(int)v[4],v[5],v[6], busFlute); evN++; } } else skipN++;
    } else if (!strncmp(line, "fembell ", 8)) {
      double v[5]; if (sscanf(line, "fembell %lf %lf %lf %lf %lf", &v[0],&v[1],&v[2],&v[3],&v[4]) == 5) {
        if (wanted("bell")) { synth_fembell(v[0],v[1],v[2],(int)v[3],v[4], busBell); evN++; } } else skipN++;
    } else if (!strncmp(line, "gong ", 5)) {
      double v[4]; if (sscanf(line, "gong %lf %lf %lf %lf", &v[0],&v[1],&v[2],&v[3]) == 4) {
        if (wanted("cave")) { synth_gong(v[0],v[1],v[2],v[3], busCave); evN++; } } else skipN++;
    } else if (!strncmp(line, "revbell ", 8)) {
      double v[5]; if (sscanf(line, "revbell %lf %lf %lf %lf %lf", &v[0],&v[1],&v[2],&v[3],&v[4]) == 5) {
        if (wanted("bell")) { synth_revbell(v[0],v[1],v[2],v[3],v[4], busBell); evN++; } } else skipN++;
    } else if (!strncmp(line, "vortex ", 7)) {
      double v[7]; if (sscanf(line, "vortex %lf %lf %lf %lf %lf %lf %lf", &v[0],&v[1],&v[2],&v[3],&v[4],&v[5],&v[6]) == 7) {
        if (wanted("scratch")) { synth_vortex(v[0],v[1],v[2],v[3],v[4],v[5],v[6], busScr); evN++; } } else skipN++;
    } else if (!strncmp(line, "rise ", 5)) {
      // rise <landSec> <dur> <gain> <decay> <ratio> <nm> m0 m1 ...
      char *save, *tok = strtok_r(line, " \n", &save); (void)tok;
      double ls = atof(strtok_r(NULL," \n",&save)), du = atof(strtok_r(NULL," \n",&save));
      double gn = atof(strtok_r(NULL," \n",&save)), dc = atof(strtok_r(NULL," \n",&save));
      double ra = atof(strtok_r(NULL," \n",&save)); int nm = atoi(strtok_r(NULL," \n",&save));
      double midis[16]; int ok = 1; if (nm < 1 || nm > 16) ok = 0;
      for (int j = 0; ok && j < nm; j++) { char *tt = strtok_r(NULL," \n",&save); if (!tt) { ok = 0; break; } midis[j] = atof(tt); }
      if (ok) { if (wanted("rise")) { synth_reverse_bell(ls,du,gn,dc,ra,nm,midis, busRise); evN++; } } else skipN++;
    } else if (!strncmp(line, "revkick ", 8)) {
      double v[8]; if (sscanf(line, "revkick %lf %lf %lf %lf %lf %lf %lf %lf", &v[0],&v[1],&v[2],&v[3],&v[4],&v[5],&v[6],&v[7]) == 8) {
        if (wanted("revkick")) { synth_reverse_kick(v[0],v[1],v[2],v[3],v[4],v[5],v[6],v[7], busKick); evN++; } } else skipN++;
    } else if (!strncmp(line, "pad ", 4)) {
      // pad <start> <dur> <g> <attFrac> <nm> <midis…> — attFrac is flatterbop's
      // envelope knob (the 360 hardcoded 0.42): fast blooms in the dawn, slow
      // swells at the key changes.
      char *save, *tok = strtok_r(line, " \n", &save); (void)tok;
      double st = atof(strtok_r(NULL," \n",&save)), du = atof(strtok_r(NULL," \n",&save)), gn = atof(strtok_r(NULL," \n",&save));
      double at = atof(strtok_r(NULL," \n",&save));
      int nm = atoi(strtok_r(NULL," \n",&save)); double midis[16]; int ok = (nm >= 1 && nm <= 16);
      for (int j = 0; ok && j < nm; j++) { char *tt = strtok_r(NULL," \n",&save); if (!tt) { ok = 0; break; } midis[j] = atof(tt); }
      if (ok) { if (wanted("pad")) { synth_pad(st,du,gn,at,nm,midis, busPad); evN++; } } else skipN++;
    } else if (!strncmp(line, "drop ", 5)) {
      double v[4]; if (sscanf(line, "drop %lf %lf %lf %lf", &v[0],&v[1],&v[2],&v[3]) == 4) {
        if (wanted("drop")) { synth_drop(v[0],v[1],v[2],v[3], busDrop); evN++; } } else skipN++;
    } else if (!strncmp(line, "plink ", 6)) {
      // plink is a marimba voice → busDrop (glockenspiel). Reuse marimba parser.
      char *save, *tok = strtok_r(line, " \n", &save); (void)tok;
      double fld[15]; int nf = 0;
      for (; nf < 15; nf++) { char *tt = strtok_r(NULL," \n",&save); if (!tt) break; fld[nf] = atof(tt); }
      int M = (nf >= 15) ? (int)fld[14] : 0;
      if (M < 1 || M > 16) { skipN++; continue; }
      double partials[16], amps[16], decays[16]; int ok = 1;
      for (int j = 0; j < M; j++) { char *tt = strtok_r(NULL," \n",&save); if (!tt) { ok = 0; break; } partials[j] = atof(tt); }
      for (int j = 0; ok && j < M; j++) { char *tt = strtok_r(NULL," \n",&save); if (!tt) { ok = 0; break; } amps[j] = atof(tt); }
      for (int j = 0; ok && j < M; j++) { char *tt = strtok_r(NULL," \n",&save); if (!tt) { ok = 0; break; } decays[j] = atof(tt); }
      if (!ok) { skipN++; continue; }
      if (wanted("drop")) {
        synth_marimba(fld[0],fld[1],fld[2],fld[3],fld[4],fld[5],fld[6],fld[7],fld[8],fld[9],fld[10],fld[11],fld[12],fld[13], M, partials, amps, decays, busDrop);
        evN++;
      }
    } else if (!strncmp(line, "cavem ", 6)) {
      // cave marimba voice → busCave. cavem <fields...> like marimba but no lane.
      char *save, *tok = strtok_r(line, " \n", &save); (void)tok;
      double fld[15]; int nf = 0;
      for (; nf < 15; nf++) { char *tt = strtok_r(NULL," \n",&save); if (!tt) break; fld[nf] = atof(tt); }
      int M = (nf >= 15) ? (int)fld[14] : 0;
      if (M < 1 || M > 16) { skipN++; continue; }
      double partials[16], amps[16], decays[16]; int ok = 1;
      for (int j = 0; j < M; j++) { char *tt = strtok_r(NULL," \n",&save); if (!tt) { ok = 0; break; } partials[j] = atof(tt); }
      for (int j = 0; ok && j < M; j++) { char *tt = strtok_r(NULL," \n",&save); if (!tt) { ok = 0; break; } amps[j] = atof(tt); }
      for (int j = 0; ok && j < M; j++) { char *tt = strtok_r(NULL," \n",&save); if (!tt) { ok = 0; break; } decays[j] = atof(tt); }
      if (!ok) { skipN++; continue; }
      if (wanted("cave")) {
        synth_marimba(fld[0],fld[1],fld[2],fld[3],fld[4],fld[5],fld[6],fld[7],fld[8],fld[9],fld[10],fld[11],fld[12],fld[13], M, partials, amps, decays, busCave);
        evN++;
      }
    } else if (!strncmp(line, "cavekick ", 9)) {
      double v[8]; if (sscanf(line, "cavekick %lf %lf %lf %lf %lf %lf %lf %lf", &v[0],&v[1],&v[2],&v[3],&v[4],&v[5],&v[6],&v[7]) == 8) {
        if (wanted("cave")) { synth_kick(v[0],v[1],v[2],v[3],v[4],v[5],v[6],v[7], busCave); evN++; } } else skipN++;
    } else if (!strncmp(line, "caverise ", 9)) {
      char *save, *tok = strtok_r(line, " \n", &save); (void)tok;
      double ls = atof(strtok_r(NULL," \n",&save)), du = atof(strtok_r(NULL," \n",&save));
      double gn = atof(strtok_r(NULL," \n",&save)), dc = atof(strtok_r(NULL," \n",&save));
      double ra = atof(strtok_r(NULL," \n",&save)); int nm = atoi(strtok_r(NULL," \n",&save));
      double midis[16]; int ok = (nm >= 1 && nm <= 16);
      for (int j = 0; ok && j < nm; j++) { char *tt = strtok_r(NULL," \n",&save); if (!tt) { ok = 0; break; } midis[j] = atof(tt); }
      if (ok) { if (wanted("cave")) { synth_reverse_bell(ls,du,gn,dc,ra,nm,midis, busCave); evN++; } } else skipN++;
    } else if (!strncmp(line, "cavesub ", 8)) {
      double v[4]; if (sscanf(line, "cavesub %lf %lf %lf %lf", &v[0],&v[1],&v[2],&v[3]) == 4) {
        if (wanted("cave")) { synth_sub(v[0],v[1],v[2],v[3], busCave); evN++; } } else skipN++;
    } else if (!strncmp(line, "cavehat ", 8)) {
      // reverse-hat into busCave: render forward hat, write reversed at land−len.
      double land = 0.0, gn = 0.32; sscanf(line, "cavehat %lf %lf", &land, &gn);
      if (wanted("cave")) {
        double *seg; long sl = render_hat_seg(gn, 0.26, &seg);
        if (seg) {
          long startIdx = (long)floor(land * SR) - sl;
          for (long i = 0; i < sl; i++) { long dd = startIdx + i; if (dd >= 0 && dd < NS) busCave[dd] += seg[sl - 1 - i]; }
          free(seg);
        }
        evN++;
      }
    } else if (!strncmp(line, "rhat ", 5)) {
      // reverse-hat into busHat (the button's open-hat tail landing on a beat).
      double land = 0.0, gn = 0.32; sscanf(line, "rhat %lf %lf", &land, &gn);
      if (wanted("hat")) {
        double *seg; long sl = render_hat_seg(gn, 0.26, &seg);
        if (seg) {
          long startIdx = (long)floor(land * SR) - sl;
          for (long i = 0; i < sl; i++) { long dd = startIdx + i; if (dd >= 0 && dd < NS) busHat[dd] += seg[sl - 1 - i]; }
          free(seg);
        }
        evN++;
      }
    } else if (!strncmp(line, "physmar ", 8)) {
      // physmar <start> <f0Hz> <dur> <gain> <morph 0=wood..1=glass> <strike 0=soft..1=hard>
      double v[6]; if (sscanf(line, "physmar %lf %lf %lf %lf %lf %lf", &v[0],&v[1],&v[2],&v[3],&v[4],&v[5]) == 6) {
        if (wanted("lead")) { synth_physmar(v[0],v[1],v[2],v[3],v[4],v[5], busLead); evN++; } } else skipN++;
    } else if (!strncmp(line, "zoo ", 4)) {
      double v[5]; if (sscanf(line, "zoo %lf %lf %lf %lf %lf", &v[0],&v[1],&v[2],&v[3],&v[4]) == 5) {
        if (wanted("zoo")) { synth_zoo(v[0], (int)v[1], v[2], v[3], v[4]); evN++; } } else skipN++;
    } else if (!strncmp(line, "stream ", 7)) {
      // a single pre-summed stream sample run is impractical to bake per-sample;
      // instead the JS bakes the WHOLE streamBuf as a raw side-file. Handled
      // below via 'streamraw <path>'. This token is unused.
      skipN++;
    } else if (!strncmp(line, "streamraw ", 10)) {
      // load the pre-rendered streamBuf (mono f32, NS samples) the JS baked.
      char path[768]; sscanf(line, "streamraw %767s", path);
      if (wanted("stream")) {
        FILE *sf = fopen(path, "rb");
        if (sf) {
          float *tmp = malloc((size_t)NS * sizeof(float));
          if (tmp) { size_t got = fread(tmp, sizeof(float), (size_t)NS, sf); for (long i = 0; i < (long)got; i++) busStream[i] += tmp[i]; free(tmp); }
          fclose(sf); evN++;
        } else skipN++;
      }
    } else if (!strncmp(line, "marimba ", 8)) {
      char *save, *tok = strtok_r(line, " \n", &save); (void)tok;
      char *lane = strtok_r(NULL, " \n", &save);
      double fld[15]; int nf = 0;
      for (; nf < 15; nf++) { char *tt = strtok_r(NULL, " \n", &save); if (!tt) break; fld[nf] = atof(tt); }
      int M = (nf >= 15) ? (int)fld[14] : 0;
      if (M < 1 || M > 16) { skipN++; continue; }
      double partials[16], amps[16], decays[16]; int ok = 1;
      for (int j = 0; j < M; j++) { char *tt = strtok_r(NULL, " \n", &save); if (!tt) { ok = 0; break; } partials[j] = atof(tt); }
      for (int j = 0; ok && j < M; j++) { char *tt = strtok_r(NULL, " \n", &save); if (!tt) { ok = 0; break; } amps[j] = atof(tt); }
      for (int j = 0; ok && j < M; j++) { char *tt = strtok_r(NULL, " \n", &save); if (!tt) { ok = 0; break; } decays[j] = atof(tt); }
      if (!ok) { skipN++; continue; }
      double *bus = NULL;
      if      (!strcmp(lane, "lead"))  bus = busLead;
      else if (!strcmp(lane, "bass"))  bus = busBass;
      else if (!strcmp(lane, "spark")) bus = busSpark;
      else if (!strcmp(lane, "bell"))  bus = busBell;
      else if (!strcmp(lane, "vib"))   bus = busVib;
      else { skipN++; continue; }
      if (!wanted(lane)) continue;
      synth_marimba(fld[0],fld[1],fld[2],fld[3],fld[4],fld[5],fld[6],fld[7],fld[8],fld[9],fld[10],fld[11],fld[12],fld[13], M, partials, amps, decays, bus);
      evN++;
    } else {
      skipN++;
    }
  }
  fclose(f);
  if (!allocated) { fprintf(stderr, "score had no 'ns' header\n"); return 1; }

  // ══ STEREO MIXDOWN — mirrors render-fluttabap360.mjs. ══
  if (soloCount >= 0) {
    // SOLO/parity: place each soloed voice DRY at its canonical pan/gain.
    // (revkick shares kickBuf with kick → both place via the kick lane.)
    for (int i = 0; i < soloCount; i++) {
      const char *nm = soloNames[i];
      if      (!strcmp(nm, "sub"))     place(busDeep, 0.00, 0.92);
      else if (!strcmp(nm, "kick"))    place(busKick, 0.00, 1.02);
      else if (!strcmp(nm, "revkick")) place(busKick, 0.00, 1.02);
      else if (!strcmp(nm, "lead"))    place(busLead, 0.00, 1.00);
      else if (!strcmp(nm, "bass"))    place(busBass, 0.00, 0.66);
      else if (!strcmp(nm, "spark"))   place(busSpark, 0.44, 0.86);
      else if (!strcmp(nm, "bell"))    place(busBell, -0.42, 0.84);
      else if (!strcmp(nm, "vib"))     place(busVib, -0.24, 0.88);
      else if (!strcmp(nm, "snare"))   place(busSnare, 0.04, 0.80);
      else if (!strcmp(nm, "hat"))     place(busHat, 0.34, 0.56);
      else if (!strcmp(nm, "shaker"))  place(busShak, -0.34, 0.48);
      else if (!strcmp(nm, "subperc")) place(busSub, 0.00, 1.00);
      else if (!strcmp(nm, "scratch")) place(busScr, 0.34, 0.60);
      else if (!strcmp(nm, "scream"))  place(busScream, -0.48, 0.48);
      else if (!strcmp(nm, "rise"))    place(busRise, 0.00, 0.85);
      else if (!strcmp(nm, "pad"))     place(busPad, 0.00, 0.60);
      else if (!strcmp(nm, "drop"))    place(busDrop, 0.00, 0.84);
      else if (!strcmp(nm, "stream"))  place(busStream, -0.30, 0.30);   // JS solo places ONCE (full mix doubles)
      else if (!strcmp(nm, "cave"))    place(busCave, 0.00, 0.92);
      else if (!strcmp(nm, "flute"))   place(busFlute, 0.16, 0.90);
      else if (!strcmp(nm, "zoo"))     { place(busZooL, -1.00, 0.92); place(busZooR, 1.00, 0.92); }
    }
  } else {
    // FULL MIX — flatterbop's OWN voicing priorities (deliberately NOT the
    // 360's): bells + vib + flute carry the front, the beepy lead sits just
    // behind them, the techno kick anchors, and the zoo is LOUD and lives in
    // its own room (a real reverb, not dry accents). Everything more ethereal.
    place(busLead,   0.00, 0.90);
    place(busFlute,  0.12, 1.00);
    place(busBass,   0.00, 0.62);
    place(busVib,   -0.28, 0.98);
    place(busDeep,   0.00, 0.94);
    place(busRise,   0.00, 0.88);
    place(busScr,    0.34, 0.52);
    place(busScream,-0.48, 0.42);
    place(busSpark,  0.48, 0.90);
    place(busBell,  -0.46, 1.00);
    place(busKick,   0.00, 1.06);
    place(busSub,    0.00, 0.98);
    place(busSnare,  0.04, 0.70);
    place(busHat,    0.34, 0.50);
    place(busShak,  -0.34, 0.44);
    // the ZOO — loud, wide, and in its own airy room
    reverb_in_place(busZooL, NS, 0.32, 0.66);
    reverb_in_place(busZooR, NS, 0.32, 0.66);
    place(busZooL,  -1.00, 1.10);
    place(busZooR,   1.00, 1.10);
    reverb_in_place(busCave, NS, 0.56, 0.80);
    place(busCave,   0.00, 0.94);
    reverb_in_place(busStream, NS, 0.36, 0.62);
    place(busStream, -0.30, 0.32);
    place(busStream,  0.30, 0.32);
    place(busPad,    0.00, 0.60);
    delay_in_place(busDrop, NS, gBeat * 0.75, 0.42, 0.48);
    reverb_in_place(busDrop, NS, 0.52, 0.74);
    place(busDrop,   0.00, 0.84);
    // SPACE — automated REVERB SCENES (the room as a compositional element).
    // Per voice, a coarse send envelope across the timeline (section scene
    // values), smoothed both directions, then summed into the shared reverb.
    double *spaceSend = bus_alloc();
    const char *SV[9] = {"lead","bell","spark","vib","pad","scr","scream","rise","flute"};
    double *SB[9] = {busLead,busBell,busSpark,busVib,busPad,busScr,busScream,busRise,busFlute};
    const int RVZ = 100;
    long rvSpan = (long)ceil(((double)NS / SR) * RVZ) + 1;
    double *env = malloc((size_t)rvSpan * sizeof(double));
    if (env) {
      for (int vi = 0; vi < 9; vi++) {
        for (long k = 0; k < rvSpan; k++) {
          double tt = (double)k / RVZ, amt = 0.0;
          for (int si = 0; si < gSecN; si++) {
            double t0 = gSec[si].bar0 * gBar, t1 = (gSec[si].bar0 + gSec[si].bars) * gBar;
            if (tt >= t0 && tt < t1) { amt = scene_amt(gSec[si].name, SV[vi]); break; }
          }
          env[k] = amt;
        }
        double a2 = 0.04;
        for (long k = 1; k < rvSpan; k++) env[k] = env[k] * a2 + env[k - 1] * (1 - a2);
        for (long k = rvSpan - 2; k >= 0; k--) env[k] = env[k] * a2 + env[k + 1] * (1 - a2);
        for (long i = 0; i < NS; i++) {
          long ki = (long)floor(((double)i / SR) * RVZ); if (ki > rvSpan - 1) ki = rvSpan - 1;
          double g = env[ki]; if (g > 0.0004) spaceSend[i] += SB[vi][i] * g;
        }
      }
      free(env);
    }
    reverb_in_place(spaceSend, NS, 1.0, 0.78);
    place(spaceSend, 0.00, 0.50);
    free(spaceSend);
  }

  // ── FINALIZE (full mix only) — matches the JS exactly. ──
  long outN = NS;
  if (soloCount < 0) {
    // SEAMLESS LOOP fold — fold the tail (past the 187-bar loopN) onto the head
    // so the button ring + reverb wrap into the dawn (mirrors the JS LOOP fold).
    if (loopMode) {
      long loopN = (long)llround((double)gTotalBars * gBar * SR);
      for (long i = loopN; i < NS; i++) { long d = i - loopN; if (d < loopN) { outL[d] += outL[i]; outR[d] += outR[i]; } }
      outN = loopN;
    }
    // scrub non-finite
    for (long i = 0; i < NS; i++) { if (!isfinite(outL[i])) outL[i] = 0.0f; if (!isfinite(outR[i])) outR[i] = 0.0f; }
    // peak normalize to 0.84
    double peak = 0.0;
    for (long i = 0; i < outN; i++) { double a = fabs(outL[i]); if (a > peak) peak = a; double bb = fabs(outR[i]); if (bb > peak) peak = bb; }
    if (peak > 0.0) { double nrm = 0.84 / peak; for (long i = 0; i < outN; i++) { outL[i] *= (float)nrm; outR[i] *= (float)nrm; } }
    // FINAL-CHORUS LIFT (key==5 boundary), +2.4 dB over a 2 s ramp — LINEAR only
    // (skipped on the loop, or the lifted tail would jump at the seam).
    if (gFinalLiftSec >= 0.0 && !loopMode) {
      long s0 = (long)floor(gFinalLiftSec * SR), ramp = (long)floor(2.0 * SR);
      double gMax = pow(10.0, 2.4 / 20.0);
      for (long i = s0; i < outN; i++) {
        double rr = (double)(i - s0) / ramp; if (rr > 1.0) rr = 1.0;
        double gg = 1.0 + (gMax - 1.0) * rr;
        outL[i] *= (float)gg; outR[i] *= (float)gg;
      }
    }
    // trim: last-loud + 0.6 s, hard-clamped to TARGET_SEC — LINEAR only
    // (the loop uses the exact loopN; no trim).
    long trimN = outN;
    if (!loopMode) {
      long lastLoud = outN - 1;
      while (lastLoud > 0 && fabs(outL[lastLoud]) < 0.004 && fabs(outR[lastLoud]) < 0.004) lastLoud--;
      trimN = lastLoud + (long)floor(0.6 * SR);
      if (trimN > outN) trimN = outN;
      long clamp = (long)llround(gTargetSec * SR);
      if (trimN > clamp) trimN = clamp;
    }
    // SUBSTRATE PRINT — the per-sample print character: tape/tube saturation
    // (tanh drive + an even-harmonic TUBE bias = asymmetric warmth, DC removed)
    // plus the substrate's low-passed hiss floor. Drive/bias/hiss come from the
    // baked `sat` line (tape vs vinyl). Applied to linear master AND the loop.
    unsigned int hss = 0x51ed2701u;
    double hlpL = 0.0, hlpR = 0.0; const double HLP = 0.22, HISS = gSatHiss;
    double drv = gSatDrive, bias = gSatBias, dnorm = tanh(drv);
    for (long i = 0; i < trimN; i++) {
      double l = (tanh(outL[i] * drv + bias) - tanh(bias)) / dnorm;
      double r = (tanh(outR[i] * drv + bias) - tanh(bias)) / dnorm;
      // _hr(): two independent draws per sample (L then R)
      hss = (hss + 0x6d2b79f5u); unsigned int x = (hss ^ (hss >> 15)) * (1u | hss);
      x = (x + (x ^ (x >> 7)) * (61u | x)) ^ x; double n1 = ((double)((x ^ (x >> 14))) / 4294967296.0) * 2.0 - 1.0;
      hss = (hss + 0x6d2b79f5u); x = (hss ^ (hss >> 15)) * (1u | hss);
      x = (x + (x ^ (x >> 7)) * (61u | x)) ^ x; double n2 = ((double)((x ^ (x >> 14))) / 4294967296.0) * 2.0 - 1.0;
      hlpL += (n1 - hlpL) * HLP; hlpR += (n2 - hlpR) * HLP;
      outL[i] = (float)(l + hlpL * HISS); outR[i] = (float)(r + hlpR * HISS);
    }
    // FADES / LOOP BREATH — mirrors render-fluttabap360.mjs.
    if (!loopMode) {
      // linear master — long eased intro swell from silence + clean outro fade.
      long fadeIn = (long)floor(1.5 * SR), fadeOut = (long)floor(1.6 * SR);
      for (long i = 0; i < fadeIn && i < trimN; i++) { double gg = pow((double)i / fadeIn, 1.4); outL[i] *= (float)gg; outR[i] *= (float)gg; }
      for (long i = 0; i < fadeOut && i < trimN; i++) { long idx = trimN - 1 - i; double gg = (double)i / fadeOut; outL[idx] *= (float)gg; outR[idx] *= (float)gg; }
    } else {
      // seamless-but-still-an-intro loop — gentle breath (dip to a floor, swell
      // back over ~3.5 s) + a ~60 ms anti-click micro-fade on the seam edge.
      const double FLOOR = 0.45; long swell = (long)floor(2.5 * SR);
      for (long i = 0; i < swell && i < trimN; i++) { double gg = FLOOR + (1.0 - FLOOR) * pow((double)i / swell, 1.4); outL[i] *= (float)gg; outR[i] *= (float)gg; }
      long edge = (long)floor(0.06 * SR);
      for (long i = 0; i < edge && i < trimN; i++) { double gg = (double)i / edge; long j = trimN - 1 - i; outL[j] *= (float)gg; outR[j] *= (float)gg; }
    }
    outN = trimN;
  }

  FILE *o = fopen(rawPath, "wb");
  if (!o) { fprintf(stderr, "cannot open out: %s\n", rawPath); return 1; }
  for (long i = 0; i < outN; i++) { fwrite(&outL[i], 4, 1, o); fwrite(&outR[i], 4, 1, o); }
  fclose(o);

  fprintf(stderr, "[flatterbop180] %ld events (%ld unported skipped) → %ld frames\n", evN, skipN, outN);
  return 0;
}
