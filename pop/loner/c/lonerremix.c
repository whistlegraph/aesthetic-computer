// lonerremix.c — "whistlegraph loner --- remix (v4, regulated)", the C engine.
//
// @jeffrey (2026-08-17): "i wanna go from bedroom ballad to nice dance
// track" · "keep a pretty strict beat that the lyrics can now regulate
// around, with a kick and stuff, sidechained into the lyrics" · "i want
// this all to be in c code". So v4 is the lane's first C-native score —
// no Node render to port from; this file IS the score — following the
// fleet-standard single-file renderer the other /pop lanes carry
// (pop/cult/c, pop/boombaboom/c, pop/hellsine/c).
//
// The deal with the Python side: bin/halo3.py owns everything WORLD —
// it bakes the regulated vocal bank (snap 0.92, per-word frame warp
// onto the 122 BPM chart, octave halos, full-word low-3rd/low-5th
// backup) into vox4/, and emits c/loner-chart.h: per phrase, per word,
// its beat slot and its measured semitone. This engine reads that
// header, so THE BAND'S MELODY IS HER MELODY — the pluck doubles the
// chart, answers with it in the gaps, and closes the track with it.
//
//   FLOOR     Four on it. Kick every beat from bar 0; clap 2 & 4; an
//             offbeat air-hat; velocity-shaped 16th ticks. Strict — the
//             one thing that never breathes.
//   PUMP      The kick sidechains INTO the lyrics: every beat ducks the
//             vox bus 0.34 and the music bus 0.52 (10 ms pre-open,
//             smooth recovery inside the beat). Drums never duck — the
//             cult bus law survives the genre change.
//   NOW       Her first word lands ON beat 0 of bar 0 (the chart's
//             lead-in consonant runs ahead of the downbeat).
//   HER NOTES the pluck (a music-box pair: sine + 2.7× partial, 6 ms
//             attack, exponential decay, dotted-8th delay send) plays
//             only chart notes. Nothing in this file invents a melody.
//   SPACE     v3's rooms, ported intact: the dotted-8th dub delay
//             (damped cross-feedback at 0.38) and the decorrelated
//             Schroeder pair on the vox bus (4 combs/side 44.6–54.2 ms,
//             RT60 ≈ 3.2 s, damped in-loop, two allpasses, 40 ms
//             pre-delay, 180 Hz high-passed return).
//
// Form, 76 bars at 122 (2:29 + tail):
//
//   V1      0:00  0–16   f- phrases on the chart, pluck answering
//   HOOK    0:31  16–24  of-a-stone + hk unison + stone-long canon
//   V2      0:47  24–32  the "not again!" phrases, backup 3rds in
//   BREAK   1:03  32–40  kick out: naked regulated line, stone-long,
//                        riser — the one breath
//   DROP    1:18  40–60  everything: both backups, harps, ens crowds,
//                        the stone-long-17 crown
//   OUT     1:58  60–76  the pluck alone finishes the sentence
//
// Mixing rules survive from v1→v3: raised-cosine tails, no master tanh,
// one linear trim, mono-safe pans with band-limited antisymmetric side.
//
// Build:  bash pop/loner/c/build.sh
// Run:    pop/loner/c/lonerremix        # → pop/loner/out/loner-remix-v4-full.wav
//         (from the repo root; cut-v4.sh masters it to mp3)

#include <dirent.h>
#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "loner-chart.h"

#define SR 48000
#define TAU 6.283185307179586
static const double BPM = CHART_BPM;      // 122 — the chart's clock
static const double TONIC = CHART_TONIC;  // 237 Hz — Camille's frame
// @jeffrey: "instrumental intro" → "can the intro be like just 1 bar" →
// "can we start it as sitting from the first beat?". None, in the end, and
// it is the right answer for this song: three minutes of one sentence does
// not need announcing. The first sound on the record is her /s/.
#define INTRO 0
#define BARS (INTRO + 48)   // Lonerclub v4pid — three passes, one long build
// the passes, addressed by number rather than by remembering an offset
#define L(n) (INTRO + (n) * 16)
#define TAIL_S 6.0
static double BEAT, BAR, STEP;            // set in main
static long N;
#define LANE "pop/loner"

// ── buses ──────────────────────────────────────────────────────────────
static float *drumsL, *drumsR, *musicL, *musicR, *voxL, *voxR;
static float *sideV, *sideB, *dlySend, *rvbSend;
// @jeffrey: "could we have the vocals be deeper in the mix". 1.42 put her
// right at the front of the speaker. Depth is not just a smaller number
// though — a quiet close voice is still a close voice — so the fader comes
// down AND the sends go up AND the chest comes out (see THE VOCAL CHAIN).
static const double VOXG = 0.82;

// ── helpers ────────────────────────────────────────────────────────────
static double clampd(double v, double a, double b) { return v < a ? a : v > b ? b : v; }
static double smoothstep(double u) { return u <= 0 ? 0 : u >= 1 ? 1 : u * u * (3 - 2 * u); }
static double tail_fade(long i, long n) {
    double u = (double)(n - 1 - i) / (0.010 * SR);
    return u >= 1 ? 1 : u <= 0 ? 0 : u * u * (3 - 2 * u);
}
// THE CONSONANT RUNWAY NEEDS SOMEWHERE TO LIVE. voice_line places a phrase
// at at(bar) − leadIn, so her VOWEL lands on the beat and the consonant
// runs 1:1 before it, the way a singer leans in. At bar 0 that is NEGATIVE
// TIME, and emit() drops anything before sample 0 — so the 220 ms /s/ that
// opens "sitting" was not buried under the kick, it was never written to
// the file at all. Only the first pass lost it; bars 16, 48 and 80 have
// their runway. @jeffrey, twice: "the opening 's' is not hearable".
//
// @jeffrey: "can we start it as sitting from the first beat?" — with no
// intro at all, which puts the problem straight back: her /s/ has to run
// before the downbeat and there is nothing before the downbeat.
//
// A whole BEAT of pre-roll was the crude fix and it left 492 ms of silence
// at the head of the record. The exact fix is the phrase's own leadIn:
// pre-roll by precisely the runway that phrase needs, so the file opens ON
// the first sample of her /s/, her vowel lands on beat 1, and there is no
// silence anywhere. Nothing is trimmed and nothing is padded.
static double PREROLL = 0.0;
static double at(double bar) { return PREROLL + bar * BAR; }
static double hz_of(double st) { return TONIC * pow(2.0, st / 12.0); }

static uint32_t seed = 20260817u;
static double rnd(void) { seed = seed * 1664525u + 1013904223u; return seed / 4294967296.0; }
static double jit(double ms) { return ((rnd() - 0.5) * 2 * ms) / 1000.0; }

// ── BOP ───────────────────────────────────────────────────────────────
// @jeffrey: "any way we can add more bop / more jazz and humanization to
// the beat too?"
//
// SWING is the whole of the first half. A straight 16th grid is a machine
// counting; pushing every odd 16th late is what makes a beat walk. 0.58
// is a light shuffle — 0.5 is dead straight, 0.667 is full triplet swing —
// and it applies to everything that lands off the beat, so the kit, the
// bassline and the stabs all lean the same way rather than fighting.
#define SWING 0.575
static double sw(double step16) {          // a 16th step index → seconds
    double beat = floor(step16 / 2.0), odd = fmod(step16, 2.0);
    return (beat + (odd ? 2.0 * SWING : 0.0)) * BEAT;
}

// …and LEAN is the second half. A player does not vary each hit at
// random, they push through one bar and drag through the next: the error
// is CORRELATED, and that is what random jitter can never sound like. Two
// slow incommensurate waves plus a little noise, in milliseconds.
static double lean(double bar, double beat) {
    double x = bar + beat / 4.0;
    return (0.0075 * sin(TAU * x / 7.0) + 0.0042 * sin(TAU * x / 2.6)
            + 0.0018 * sin(TAU * x / 1.37));
}
static uint32_t nseed = 20210725u;
static double nrnd(void) {
    nseed ^= nseed << 13; nseed ^= nseed >> 17; nseed ^= nseed << 5;
    return (nseed / 4294967296.0) * 2 - 1;
}

typedef struct { long itd; double gl, gr; } Spatial;
static Spatial spatial(double az) {
    Spatial s;
    s.itd = lround(0.00027 * SR * sin(az));
    double shadow = 0.35 * sin(az);
    s.gl = 1 - shadow; s.gr = 1 + shadow;
    return s;
}
static void emit(int bus, long i, double mono, double pan, const Spatial *sp,
                 double sideAmt, double dly, double rvb) {
    if (i < 0 || i >= N) return;
    double a = (M_PI / 4) * (1 + pan), cl = cos(a), cr = sin(a);
    if (bus == 0) { drumsL[i] += mono * cl; drumsR[i] += mono * cr; }
    else if (bus == 2) { voxL[i] += mono * cl; voxR[i] += mono * cr; }
    else { musicL[i] += mono * cl; musicR[i] += mono * cr; }
    if (dly) dlySend[i] += mono * dly;
    if (rvb) rvbSend[i] += mono * rvb;
    if (sp && sideAmt) {
        long li = i + sp->itd, ri = i - sp->itd;
        double l = (li >= 0 && li < N) ? mono * sp->gl : 0;
        double r = (ri >= 0 && ri < N) ? mono * sp->gr : 0;
        double s = 0.5 * (l - r) * sideAmt;
        if (bus == 2) sideV[i] += s; else sideB[i] += s;
    }
}
#define BUS_DRUMS 0
#define BUS_MUSIC 1
#define BUS_VOX 2

// ── WAV IO (the fleet loader, cultremix.c's) ───────────────────────────
static float *load_wav_mono(const char *path, long *out_n, int *out_sr) {
    FILE *f = fopen(path, "rb"); if (!f) return NULL;
    fseek(f, 0, SEEK_END); long sz = ftell(f); fseek(f, 0, SEEK_SET);
    uint8_t *buf = (uint8_t *)malloc(sz);
    if (!buf || (long)fread(buf, 1, sz, f) != sz) { fclose(f); free(buf); return NULL; }
    fclose(f);
    if (sz < 12 || memcmp(buf, "RIFF", 4) || memcmp(buf + 8, "WAVE", 4)) { free(buf); return NULL; }
    long p = 12; int fmt = 1, channels = 1, bits = 16; uint32_t rate = SR;
    long dOff = 0, dLen = 0;
    while (p + 8 <= sz) {
        uint32_t s = (uint32_t)buf[p+4] | (uint32_t)buf[p+5] << 8 | (uint32_t)buf[p+6] << 16 | (uint32_t)buf[p+7] << 24;
        if (!memcmp(buf + p, "fmt ", 4)) {
            fmt = buf[p+8] | (buf[p+9] << 8); channels = buf[p+10] | (buf[p+11] << 8);
            rate = (uint32_t)buf[p+12] | (uint32_t)buf[p+13] << 8 | (uint32_t)buf[p+14] << 16 | (uint32_t)buf[p+15] << 24;
            bits = buf[p+22] | (buf[p+23] << 8);
        } else if (!memcmp(buf + p, "data", 4)) { dOff = p + 8; dLen = s; break; }
        p += 8 + s + (s & 1);
    }
    if (!channels || !bits || !dOff) { free(buf); return NULL; }
    const int bps = bits / 8, fb = bps * channels;
    const long frames = dLen / fb;
    float *mono = (float *)malloc(frames * sizeof(float));
    for (long i = 0; i < frames; i++) {
        double acc = 0;
        for (int c = 0; c < channels; c++) {
            const long o = dOff + (i * channels + c) * bps;
            if (fmt == 3 && bits == 32) { float v; memcpy(&v, buf + o, 4); acc += v; }
            else if (bits == 32) { int32_t v; memcpy(&v, buf + o, 4); acc += (double)v / 2147483648.0; }
            else if (bits == 24) { int32_t v = buf[o] | (buf[o+1] << 8) | ((int32_t)(int8_t)buf[o+2] << 16); acc += (double)v / 8388608.0; }
            else { int16_t v; memcpy(&v, buf + o, 2); acc += (double)v / 32768.0; }
        }
        mono[i] = (float)(acc / channels);
    }
    free(buf);
    *out_n = frames; *out_sr = (int)rate;
    return mono;
}
static void write_wav_f32_stereo(const char *path, const float *L, const float *R, long n) {
    FILE *f = fopen(path, "wb");
    if (!f) { fprintf(stderr, "! cannot write %s\n", path); exit(1); }
    uint32_t dsz = (uint32_t)(n * 8), riff = 36 + dsz, sr = SR, br = SR * 8, fsz = 16;
    uint16_t fmt = 3, ch = 2, ba = 8, bits = 32;
    fwrite("RIFF", 1, 4, f); fwrite(&riff, 4, 1, f); fwrite("WAVE", 1, 4, f);
    fwrite("fmt ", 1, 4, f); fwrite(&fsz, 4, 1, f);
    fwrite(&fmt, 2, 1, f); fwrite(&ch, 2, 1, f); fwrite(&sr, 4, 1, f);
    fwrite(&br, 4, 1, f); fwrite(&ba, 2, 1, f); fwrite(&bits, 2, 1, f);
    fwrite("data", 1, 4, f); fwrite(&dsz, 4, 1, f);
    float *inter = (float *)malloc(n * 2 * sizeof(float));
    for (long i = 0; i < n; i++) { inter[i*2] = L[i]; inter[i*2+1] = R[i]; }
    fwrite(inter, sizeof(float), n * 2, f);
    free(inter); fclose(f);
}

// ── sample bank — vox4/ (the chart bank) + vox3/ (arps, ens, longs) ────
#define MAX_BANK 512
typedef struct { char name[64]; float *s; long n; } Sample;
static Sample BANK[MAX_BANK];
static int bankN = 0;
static Sample *bank_get(const char *name) {
    for (int i = 0; i < bankN; i++) if (!strcmp(BANK[i].name, name)) return &BANK[i];
    return NULL;
}
static void bank_load(const char *name, const char *path) {
    long n = 0; int sr = 0;
    float *raw = load_wav_mono(path, &n, &sr);
    if (!raw) { fprintf(stderr, "  ! missing sample %s\n", path); return; }
    long a = 0;
    while (a < n - 1 && fabs((double)raw[a]) < 0.008) a++;
    long from = a - lround(0.002 * SR); if (from < 0) from = 0;
    long tn = n - from;
    double peak = 0;
    for (long i = 0; i < tn; i++) { double v = fabs((double)raw[from + i]); if (v > peak) peak = v; }
    double g = peak > 1e-6 ? 0.95 / peak : 1.0;
    float *out = (float *)malloc(tn * sizeof(float));
    for (long i = 0; i < tn; i++) out[i] = (float)((double)raw[from + i] * g);
    free(raw);
    Sample *slot = bank_get(name);
    if (!slot) {
        if (bankN >= MAX_BANK) { fprintf(stderr, "! bank full\n"); exit(1); }
        slot = &BANK[bankN++];
        snprintf(slot->name, sizeof slot->name, "%s", name);
    } else free(slot->s);
    slot->s = out; slot->n = tn;
}
static void bank_load_dir(const char *rel) {
    char dirp[512]; snprintf(dirp, sizeof dirp, "%s/%s", LANE, rel);
    DIR *d = opendir(dirp);
    if (!d) { fprintf(stderr, "! cannot open %s\n", dirp); exit(1); }
    struct dirent *e;
    while ((e = readdir(d))) {
        size_t len = strlen(e->d_name);
        if (len < 5 || strcmp(e->d_name + len - 4, ".wav") || e->d_name[0] == '.') continue;
        char name[64]; snprintf(name, sizeof name, "%.*s", (int)(len - 4), e->d_name);
        char path[1024]; snprintf(path, sizeof path, "%s/%s", dirp, e->d_name);
        bank_load(name, path);
    }
    closedir(d);
}
static int missingN = 0;

// ── the one-shot player (render3's shot(), ported) ─────────────────────
// `rate` is the playback speed and `rate_to` where it ends up: 1.0 plays
// the sample as recorded, 0.7 plays it slow AND low — which is the whole
// of "screwed", one number. Ramping between the two is a tape stop.
typedef struct {
    double gain, pan, side, dark, dur, dly, rvb, off, attack, rate, rate_to;
    int bus, rev;
} Shot;
static Shot shot_defaults(void) {
    Shot o = { 1, 0, 0.35, 0, 0, 0, 0, 0, 0.0015, 1.0, 0.0, BUS_VOX, 0 };
    return o;
}
static void shot(const char *name, double t, const Shot *o) {
    Sample *s = bank_get(name);
    if (!s) { fprintf(stderr, "  ! missing %s\n", name); missingN++; return; }
    long start = lround(o->off * SR);
    if (start < 0) start = 0;
    if (start > s->n - 2) start = s->n - 2;
    long avail = o->rev ? (start ? start : s->n - 2) : (s->n - 2 - start);
    double r0 = o->rate > 0 ? o->rate : 1.0;
    double r1 = o->rate_to > 0 ? o->rate_to : r0;
    // slower playback covers less tape in the same time, so a screwed slice
    // of a given SOURCE length occupies more output than it used to
    double rmid = 0.5 * (r0 + r1);
    long n = o->dur > 0 ? (long)fmin(avail / fmax(rmid, 1e-6), o->dur * SR)
                        : (long)(avail / fmax(rmid, 1e-6));
    if (n <= 4) return;
    long i0 = lround(t * SR);
    Spatial sp = spatial(o->pan * 1.2);
    double lp = 0;
    double pos = o->rev ? (start ? start : s->n - 2) : start;
    for (long i = 0; i < n; i++) {
        long q = (long)pos;
        if (q + 1 >= s->n || q < 0) break;
        double f = pos - q;
        double v = s->s[q] + (s->s[q + 1] - s->s[q]) * f;
        if (o->dark > 0) { lp += (1 - o->dark) * (v - lp); v = lp; }
        double env = smoothstep((i / (double)SR) / o->attack);
        emit(o->bus, i0 + i, v * env * o->gain * tail_fade(i, n),
             o->pan, &sp, o->side, o->dly, o->rvb);
        double r = r0 + (r1 - r0) * (i / (double)n);
        pos += o->rev ? -r : r;
    }
}

// lead + halo + optional backup, all locked to the same chart warp
static void sung(const char *name, double t, double gain, double pan, double dly) {
    Shot o = shot_defaults();
    // more room, more repeat: what puts a voice BEHIND the band is the
    // ratio of what you hear directly to what you hear off the walls.
    o.gain = gain; o.pan = pan; o.side = 0.5;
    o.dly = dly > 0 ? dly + 0.14 : 0.0; o.rvb = 0.58;
    shot(name, t, &o);
}
// the chart placer: subtracts the phrase's consonant lead-in so word 0
// lands ON the beat
static const ChartPhrase *phrase_of(const char *name) {
    for (int i = 0; i < CHART_N; i++)
        if (!strcmp(CHART[i].name, name)) return &CHART[i];
    fprintf(stderr, "  ! no chart phrase %s\n", name); exit(1);
}

// ONE VOICE. The halo and the backup 3rd/5th used to hang off every call
// here; @jeffrey: "lets not have background vocals anymore". There is
// nothing to fade down to zero — the layers are not built and not sung.
//
// THE TRIM. The take banks in vox4/ are halo3's renders, and they arrive
// from different rooms: measured against the f- spine's −16.2 LUFS, cp and
// o sit almost 7 dB under it, the rest one or two. stage-takes.sh's law —
// MEASURE, then one static dB — applied at the point of use, so a gain in
// this file means the same thing whichever take it is applied to.
static double take_trim_db(const char *name) {
    static const struct { const char *take; double db; } TRIM[] = {
        { "w-lg", 1.7 }, { "w-cp", 6.7 }, { "w-pf", 1.0 }, { "w-o", 6.3 },
        { "w-s", 1.8 },  { "w-rq", 3.8 }, { "w-sh", 5.6 }, { "w-rd", 2.7 },
    };
    for (unsigned i = 0; i < sizeof TRIM / sizeof *TRIM; i++)
        if (!strcmp(TRIM[i].take, name)) return TRIM[i].db;
    return 0.0;
}
static void voice_line(const char *name, double bar, double gain) {
    const ChartPhrase *p = phrase_of(name);
    double g = gain * pow(10.0, take_trim_db(name) / 20.0);
    sung(name, at(bar) - p->leadIn, g, 0, 0.13);
}
// ── the vibraphone ────────────────────────────────────────────────────
// @jeffrey: "maybe we should add more instruments · more vibes". Taken
// literally, and it is the right instrument for this record: the bank is
// all struck-and-decaying already (pluck, fembell) or all sustain (pads),
// and a vibraphone sits exactly between them. What makes one recognisable
// is not its spectrum, it is the MOTOR — rotating discs over the
// resonators chopping the sound about six times a second — so the tremolo
// is the point and the partials are just a bar being hit.
static void vibe(double t, double st, double dur, double gain, double pan) {
    double ring = fmin(dur * 1.6 + 0.9, 4.2);
    long n = lround(ring * SR), i0 = lround(t * SR);
    Spatial sp = spatial(pan * 1.2);
    double f = hz_of(st);
    double p1 = 0, p2 = 0, p3 = 0, trem = 0;
    for (long i = 0; i < n; i++) {
        double u = i / (double)SR;
        p1 += (TAU * f) / SR;
        p2 += (TAU * f * 4.0) / SR;          // the bar's strong 4th partial
        p3 += (TAU * f * 9.2) / SR;          // and the metallic one, brief
        trem += (TAU * 5.6) / SR;
        double body = sin(p1) + 0.30 * sin(p2) * exp(-u * 2.6)
                             + 0.10 * sin(p3) * exp(-u * 11.0);
        double env = smoothstep(u / 0.006) * exp(-u * (1.5 + 0.9 / fmax(dur, 0.2)));
        double motor = 1.0 - 0.42 * (0.5 - 0.5 * cos(trem));
        emit(BUS_MUSIC, i0 + i, body * 0.30 * env * motor * gain * tail_fade(i, n),
             pan, &sp, 0.7, 0.14, 0.44);
    }
}

// ── a word, on its own ────────────────────────────────────────────────
// @jeffrey: "can 'patiently' especially 'pa' be cooler". Every unit of the
// chart knows its label and its beat, and shot() already takes an offset
// and a length, so any single word can be lifted straight out of the
// rendered line and played again. `pa` is the one big gesture in the take
// — an 8.4-semitone scoop the glide guard deliberately leaves unsnapped —
// and it goes past in half a bar. This gives it the treatment it deserves.
static const ChartNote *unit_named(const ChartPhrase *p, const char *label) {
    for (int i = 0; i < p->n; i++)
        if (!strcmp(p->notes[i].t, label)) return &p->notes[i];
    fprintf(stderr, "  ! no unit %s in %s\n", label, p->name); exit(1);
}
// THE THROW — the word again, an octave up, thrown at the delay and gone.
// Nothing is doubled underneath it: the line has already moved on, so what
// answers back is the scoop alone, arriving off the beat.
static void word_throw(const char *phrase, const char *label, double bar,
                       double gain, double late, double pan) {
    const ChartPhrase *p = phrase_of(phrase);
    const ChartNote *n = unit_named(p, label);
    Shot o = shot_defaults();
    o.off = p->leadIn + n->beat * BEAT;
    o.dur = n->dur * BEAT * 1.15;
    o.gain = gain; o.pan = pan; o.side = 0.85;
    o.dly = 0.62; o.rvb = 0.55; o.dark = 0.30; o.attack = 0.010;
    shot(phrase, at(bar) - p->leadIn + n->beat * BEAT + late, &o);
}
// THE STUTTER — the first 80 ms of the word, three times on 16ths, walking
// up in level so the word itself is the fourth and loudest hit. It runs
// BEFORE the word, so the line is not interrupted; the scoop is announced.
static void word_stutter(const char *phrase, const char *label, double bar,
                         double gain, double pan) {
    const ChartPhrase *p = phrase_of(phrase);
    const ChartNote *n = unit_named(p, label);
    double t0 = at(bar) - p->leadIn + n->beat * BEAT;
    for (int k = 0; k < 3; k++) {
        Shot o = shot_defaults();
        o.off = p->leadIn + n->beat * BEAT;
        o.dur = 0.080;
        o.gain = gain * (0.42 + 0.24 * k);
        o.pan = pan * ((k % 2) ? -1 : 1);
        o.side = 0.75; o.dly = 0.28; o.rvb = 0.22; o.attack = 0.004;
        shot(phrase, t0 - (3 - k) * (BEAT / 4.0), &o);
    }
}

// ── A GHOST OF A WORD ─────────────────────────────────────────────────
// One word, slowed and soaked, at the level of something remembered
// rather than sung. The intro is built out of these: @jeffrey, "can the
// intro be more / have more previews". A preview is not a quieter copy of
// the section it announces, it is a GLIMPSE — the right length is one
// word, and the right level is under the band.
static void ghost_word(const char *phrase, const char *label, double t,
                       double rate, double gain, double pan) {
    const ChartPhrase *p = phrase_of(phrase);
    const ChartNote *nn = unit_named(p, label);
    Shot o = shot_defaults();
    o.off = p->leadIn + nn->beat * BEAT;
    o.dur = nn->dur * BEAT * 1.4;
    o.rate = rate;
    o.gain = gain; o.pan = pan; o.side = 0.9;
    o.dly = 0.52; o.rvb = 0.66; o.dark = 0.42; o.attack = 0.06;
    shot(phrase, t, &o);
}

// ── THE SCRATCH ───────────────────────────────────────────────────────
// @jeffrey: "and scratches and stuff". A scratch is not a sound, it is a
// GESTURE: the same fragment of record dragged forward and back under the
// hand, so what you hear is one slice played at a shifting rate in
// alternating directions. shot() has both now, so this is the technique
// itself rather than an imitation of it — a baby scratch is `n` passes,
// each one faster and shorter than the last.
static void scratch(const char *phrase, const char *label, double t,
                    int passes, double gain, double pan) {
    const ChartPhrase *p = phrase_of(phrase);
    const ChartNote *nn = unit_named(p, label);
    double cur = t;
    for (int k = 0; k < passes; k++) {
        double span = 0.13 / (1.0 + 0.42 * k);      // tightening
        double rate = 0.85 + 0.55 * k;              // …and speeding up
        Shot o = shot_defaults();
        o.off = p->leadIn + nn->beat * BEAT + 0.05;
        o.dur = span;
        o.rate = rate; o.rate_to = rate * (k % 2 ? 0.72 : 1.28);
        o.rev = k % 2;                              // …dragged back
        o.gain = gain * (0.68 + 0.32 * (k / (double)passes));
        o.pan = pan * ((k % 2) ? -0.7 : 1.0);
        o.side = 0.9; o.dly = 0.24; o.rvb = 0.16; o.attack = 0.002;
        shot(phrase, cur, &o);
        cur += span;
    }
}

// ── THE RISER ─────────────────────────────────────────────────────────
// Deleted with the rest of the ornaments when the kit was stripped, and
// wanted back now — @jeffrey: "and more epic drop". Noise through a
// bandpass that climbs, with the whole thing swelling: the oldest way of
// making a bar feel like it is about to end.
static void riser(double t, double dur, double gain) {
    long n = lround(dur * SR), i0 = lround(t * SR);
    double bp = 0, bp2 = 0;
    for (long i = 0; i < n; i++) {
        double u = i / (double)SR, f = u / dur;
        double hz = 300.0 * pow(38.0, f);
        double k = 1 - exp((-TAU * hz) / SR);
        double k2 = 1 - exp((-TAU * hz * 0.55) / SR);
        double w = nrnd();
        bp += k * (w - bp); bp2 += k2 * (w - bp2);
        double env = f * f * (0.35 + 0.65 * (0.5 - 0.5 * cos(TAU * 5.0 * u)));
        emit(BUS_DRUMS, i0 + i, (bp - bp2) * 1.5 * env * gain * tail_fade(i, n),
             0.1 * sin(TAU * 0.7 * u), NULL, 0, 0.10, 0.42);
    }
}

// ── THE SCREW DOWN ────────────────────────────────────────────────────
// The tape stopping. One long slice whose rate ramps to a crawl, so the
// pitch falls with it — which is the whole trick of "screwed", and the
// reason it belongs in front of a drop: everything sags, and then the
// floor comes back at full speed.
static void screw_down(const char *phrase, const char *label, double t,
                       double dur, double gain) {
    const ChartPhrase *p = phrase_of(phrase);
    const ChartNote *nn = unit_named(p, label);
    Shot o = shot_defaults();
    o.off = p->leadIn + nn->beat * BEAT;
    o.dur = dur;
    o.rate = 1.0; o.rate_to = 0.34;
    o.gain = gain; o.side = 0.8; o.dly = 0.40; o.rvb = 0.52;
    o.dark = 0.30; o.attack = 0.02;
    shot(phrase, t, &o);
}

// ── THE VOCAL BREAK ───────────────────────────────────────────────────
// @jeffrey: "cooler vocal breaks". Two bars where the band steps out of
// the way and the words come back chopped: 16th slices of a handful of
// units, accelerating, each one thrown further into the delay than the
// last, so the section is a voice disintegrating rather than a gap. The
// kick keeps running underneath — @jeffrey, earlier: "once the beat /
// kick starts it shouldn't stop it should run through the whole time" —
// and it is the hats and claps that clear out, which is what makes the
// return of the full kit land.
static void vocal_break(const char *phrase, double bar,
                        const char **words, int nw, double gain) {
    const ChartPhrase *p = phrase_of(phrase);
    // sixteen slices over two bars, tightening from 8ths to 16ths
    for (int k = 0; k < 16; k++) {
        double frac = k / 15.0;
        double step = (k < 6) ? (BEAT / 2.0) : (BEAT / 4.0);
        double t = at(bar) + (k < 6 ? k * (BEAT / 2.0)
                                    : 3.0 * BEAT + (k - 6) * (BEAT / 4.0));
        const ChartNote *n = unit_named(p, words[k % nw]);
        Shot o = shot_defaults();
        o.off = p->leadIn + n->beat * BEAT;
        // slices shorten as the figure tightens, so it stutters rather
        // than overlapping into mush
        o.dur = fmin(step * 0.92, 0.055 + 0.16 * (1.0 - frac));
        o.gain = gain * (0.42 + 0.58 * frac);
        o.pan = ((k % 2) ? 0.34 : -0.34) * (0.4 + 0.6 * frac);
        o.side = 0.85;
        o.dly = 0.22 + 0.46 * frac;
        o.rvb = 0.30 + 0.34 * frac;
        o.dark = 0.42 * (1.0 - frac);       // opens up as it accelerates
        o.attack = 0.004;
        // SCREWED, not merely chopped — @jeffrey: "can we make it even
        // more chopped and screwed". Every third slice drags: played at
        // two-thirds speed it is also six semitones down, which is the
        // technique in one number. Every fourth runs backwards. The rest
        // stay square so the figure keeps its pulse.
        if (k % 3 == 2) { o.rate = 0.66; o.dur *= 1.5; o.gain *= 1.15; }
        if (k % 4 == 3) { o.rev = 1; o.rate = 0.84; }
        shot(phrase, t, &o);
        // …and a double-trigger under the tightest half: the same slice
        // again a 32nd later, quieter, which is the hand slipping
        if (k >= 10 && (k % 2 == 0)) {
            Shot d = o;
            d.gain *= 0.5; d.pan = -o.pan; d.rev = 0; d.rate = 1.28;
            d.dur = fmin(o.dur, 0.05);
            shot(phrase, t + BEAT / 8.0, &d);
        }
    }
}

// A DIFFERENT TAKE OF THE SAME SENTENCE. bin/singdub.py warps s- and o-
// onto this chart and writes exactly its 60 beats, starting at beat 0 —
// so there is no consonant runway to subtract the way voice_line does for
// the take the chart was measured FROM. bin/stage-takes.sh has already
// matched the level, so `gain` here means the same thing it means above
// and the swap reads as a change of room, not of volume.
__attribute__((unused))
static void alt_line(const char *name, double bar, double gain, double side) {
    Shot o = shot_defaults();
    o.gain = gain; o.side = side; o.dly = 0.15; o.rvb = 0.36;
    shot(name, at(bar), &o);
}

// ── the kit — four on the floor, synthesized ───────────────────────────
#define MAX_KICKS 2048
static double kickT[MAX_KICKS]; static int kickN = 0;
static const double KSAT_24 = 0.983674092938487; // tanh(2.4)
// A HARDER KICK, for the speakers it will actually be heard on.
// @jeffrey: "can the kick be harder · so i can hear it better on laptop
// speakers with the AC on lol". A laptop cannot reproduce 50 Hz at all,
// so more sub would only eat headroom inaudibly. What reads on a small
// speaker is MIDRANGE: the beater click, a short 300 Hz knock, and the
// harmonics that saturation folds up out of the fundamental. So the
// drive doubles (1.9 → 3.8, which is nearly square), the attack is
// tighter, the pitch envelope drops faster, and the click is louder,
// longer and two-toned instead of a 2 ms blip at 0.07.
static void kick(double t, double gain) {
    if (kickN < MAX_KICKS) kickT[kickN++] = t;
    long n = lround(0.46 * SR), i0 = lround(t * SR);   // room for the tail
    double ph = 0, sub = 0, knk = 0;
    for (long i = 0; i < n; i++) {
        double u = i / (double)SR;
        // DEEPER — @jeffrey: "maybe a deeper kick". The body used to bottom
        // out at 50 Hz and get there in 17 ms; it now falls to 38 and takes
        // its time about it, which is what a big kick actually is — not
        // more level down low but longer SPENT down low. The sub under it
        // drops from 45 to 36 Hz and rings twice as long. The knock and the
        // click are untouched: they are what carries the kick on a laptop
        // speaker, and none of this is audible there at all.
        // …AND DEEP FROM THE FIRST SAMPLE — @jeffrey: "it should be deeper
        // especially at the beginnign". Measured, the old attack was not
        // deep at all: over its first 25 ms only 5% of the energy sat
        // under 60 Hz, with 63% at 60–150 and 31% above that. All the low
        // end was in the TAIL. A pitch envelope starting at 180 Hz means
        // the sound you hear land is a midrange thump and the depth only
        // catches up afterwards, which reads as sharp however gently the
        // click is treated. It now starts at 112 and settles to 34.
        double f = 34 + 78 * exp(-u * 30);
        ph += (TAU * f) / SR;
        sub += (TAU * 36) / SR;
        knk += (TAU * 300) / SR;
        // TOO SHARP — @jeffrey. Three things were making the edge, and all
        // three were deliberate once: the drive was pushed to 3.8 (nearly
        // a square wave) and the click to 0.30 back when the brief was
        // "can the kick be harder · so i can hear it better on laptop
        // speakers with the AC on lol". That kick had to fight; this one
        // does not — it is deep now, and a deep kick with a square top is
        // just a click sitting on a note.
        //
        // So the drive comes back to 2.4, where the saturation is adding
        // harmonics rather than corners; the click drops to a third and
        // moves down an octave, from 2.4/4.3 kHz to 1.5/2.7; and the
        // attack takes 2.2 ms instead of 0.8, which is the difference
        // between a beater and a spike. The knock stays — that is the part
        // a laptop can actually reproduce.
        double env = (0.6 * exp(-u * 30) + 0.5 * exp(-u * 9)) * fmin(1, u / 0.0022);
        double body = tanh(sin(ph) * env * 2.4) / KSAT_24;
        // the sub is at full from sample zero — it is the only part of the
        // kick that is already low when the hit arrives
        double low = sin(sub) * exp(-u * 3.2) * 0.52;
        double knock = sin(knk) * exp(-u * 95) * 0.17;   // ~10 ms at 300 Hz
        double click = exp(-u * 150) * 0.10 *
                       (sin(TAU * 1500 * u) + 0.7 * sin(TAU * 2700 * u));
        emit(BUS_DRUMS, i0 + i,
             (body + low + knock + click) * 0.86 * gain * tail_fade(i, n),
             0, NULL, 0, 0, 0);
    }
}
// clap — three noise bursts 11 ms apart through a 1.1–3 kHz band, for
// the 2-and-4 (the dance answer to v3's snare, which stays for ghosts)
static void clap(double t, double gain, double pan) {
    long n = lround(0.22 * SR), i0 = lround(t * SR);
    Spatial sp = spatial(pan * 1.2);
    double bp = 0, bp2 = 0;
    double k = 1 - exp((-TAU * 3000) / SR), k2 = 1 - exp((-TAU * 1100) / SR);
    for (long i = 0; i < n; i++) {
        double u = i / (double)SR;
        double w = nrnd();
        bp += k * (w - bp); bp2 += k2 * (w - bp2);
        double env = 0;
        for (int b = 0; b < 3; b++) {
            double dt = u - b * 0.011;
            if (dt > 0) env += exp(-dt * (b == 2 ? 26 : 90)) * (b == 2 ? 1.0 : 0.6);
        }
        double s = tanh((bp - bp2) * 2.1 * env);
        emit(BUS_DRUMS, i0 + i, s * 0.46 * gain * fmin(1, u / 0.001) * tail_fade(i, n),
             pan, &sp, 0.35, 0, 0.06);
    }
}
// airhat — the offbeat open hat, longer and breathier
// @jeffrey: "can we make it cooler? more up beat". The kit stays simple —
// kick, one offbeat hat, clap on 2 and 4 — but a closed hat on every
// offbeat is a metronome. `open` lets the hat RING, and an open hat on the
// offbeat is the oldest lift in house: it pulls the ear onto the "and"
// instead of onto the beat, and the track starts pushing forward rather
// than marking time.
static void airhat(double t, double gain, double pan, int open) {
    double ring = open ? 0.38 : 0.14;
    double decay = open ? 7.0 : 24.0;
    long n = lround(ring * SR), i0 = lround(t * SR);
    Spatial sp = spatial(pan * 1.2);
    double bp = 0, bp2 = 0;
    double k = 1 - exp((-TAU * (open ? 9200 : 10500)) / SR);
    double k2 = 1 - exp((-TAU * 7200) / SR);
    for (long i = 0; i < n; i++) {
        double u = i / (double)SR;
        double w = nrnd();
        bp += k * (w - bp); bp2 += k2 * (w - bp2);
        emit(BUS_DRUMS, i0 + i,
             (bp - bp2) * exp(-u * decay) * 1.1 * gain * (open ? 0.86 : 1.0)
             * tail_fade(i, n),
             pan, &sp, 0.45, 0, open ? 0.10 : 0.03);
    }
}

// ── the snare — brushed, not cracked ──────────────────────────────────
// @jeffrey: "snares too". The old one was deleted with the rest of the
// ornaments when the kit was stripped, and it was a crack: noise through a
// high band with a fast spike, which is the wrong instrument for a record
// that has just spent an evening getting its kick to stop being sharp.
//
// This is a brush instead. The band sits LOW — 190 Hz to 1.4 kHz rather
// than up where a rimshot lives — there is a tuned shell tone under it at
// 185 Hz, and the attack takes 4 ms, so it arrives rather than snapping.
// `wire` is how much snare-wire rattle rides on top; at 0 it is a tom, at
// 1 it is a backbeat, and the ghosts in between are most of the groove.
static void snare(double t, double gain, double wire, double pan) {
    long n = lround((0.16 + 0.10 * wire) * SR), i0 = lround(t * SR);
    Spatial sp = spatial(pan * 1.2);
    double bp = 0, bp2 = 0, sh = 0, sh2 = 0;
    double k = 1 - exp((-TAU * 1400.0) / SR), k2 = 1 - exp((-TAU * 190.0) / SR);
    for (long i = 0; i < n; i++) {
        double u = i / (double)SR;
        double w = nrnd();
        bp += k * (w - bp); bp2 += k2 * (w - bp2);
        double brush = (bp - bp2) * exp(-u * (26.0 - 9.0 * wire));
        sh += (TAU * 185.0) / SR; sh2 += (TAU * 262.0) / SR;
        double shell = (sin(sh) + 0.45 * sin(sh2)) * exp(-u * 30.0) * 0.42;
        double env = fmin(1.0, u / 0.004);
        emit(BUS_DRUMS, i0 + i,
             (brush * (0.35 + 0.65 * wire) + shell) * 0.55 * env * gain
             * tail_fade(i, n),
             pan, &sp, 0.5, 0.05, 0.20 + 0.16 * wire);
    }
}

// ── the stab — a chord on the offbeat ──────────────────────────────────
// The other half of "more up beat", and it is musical rather than
// percussive: a short filtered chord landing on the "and", which is what
// makes a house record feel like it is leaning forward. Sharp attack, a
// decay under a beat, and a lowpass that opens with the arrangement, so
// the same figure reads as muffled in an early pass and bright in a late
// one.
static void stab(double t, const double *tones, int n_t, double dur,
                 double gain, double open, double pan) {
    long n = lround((dur + 0.10) * SR), i0 = lround(t * SR);
    Spatial sp = spatial(pan * 1.2);
    double ph[4] = {0, 0, 0, 0}, det[4] = {1.0, 1.0032, 0.9971, 1.0018};
    double lp = 0, lp2 = 0;
    double cut = 520.0 + 3400.0 * open;
    double k = 1 - exp((-TAU * cut) / SR);
    for (long i = 0; i < n; i++) {
        double u = i / (double)SR;
        double env = smoothstep(u / 0.004) * exp(-u * (3.4 / fmax(dur, 0.05)));
        double v = 0;
        for (int j = 0; j < n_t && j < 4; j++) {
            double f = hz_of(tones[j]);
            ph[j] += (TAU * f * det[j]) / SR;
            // a saw, so the filter has something to bite on
            double x = fmod(ph[j] / TAU, 1.0) * 2.0 - 1.0;
            v += x;
        }
        v /= (double)(n_t ? n_t : 1);
        lp += k * (v - lp); lp2 += k * (lp - lp2);      // 12 dB/oct
        emit(BUS_MUSIC, i0 + i, lp2 * 0.5 * env * gain * tail_fade(i, n),
             pan, &sp, 0.6, 0.16, 0.30);
    }
}

// ── the bed — pad with wow, bass, and the pluck (her melody) ───────────
static void pad(double t, const double *sts, int nst, double dur, double gain,
                double attack, double pan, double side, double dly, double dark) {
    long n = lround((dur + 0.9) * SR), i0 = lround(t * SR);
    Spatial sp = spatial(pan * 1.2);
    double ph[6][2], drift[6];
    for (int v = 0; v < nst; v++) {
        ph[v][0] = fmod(t * 7 + v * 2.39, 1.0) * TAU;
        ph[v][1] = fmod(t * 3 + v * 1.17, 1.0) * TAU;
        drift[v] = 0.055 + 0.02 * v;
    }
    double lp = 0;
    for (long i = 0; i < n; i++) {
        double u = i / (double)SR;
        double wall = (i0 + i) / (double)SR;
        double wow = 0.0023 * sin(TAU * 0.38 * wall) + 0.0011 * sin(TAU * 0.11 * wall + 1.7);
        double s = 0;
        for (int v = 0; v < nst; v++) {
            double f = hz_of(sts[v]) * (1 + wow + 0.0007 * sin(TAU * drift[v] * wall + v * 2.1));
            ph[v][0] += (TAU * f) / SR;
            ph[v][1] += (TAU * f * 1.0028) / SR;
            s += sin(ph[v][0]) + 0.62 * sin(ph[v][1]);
        }
        s /= nst * 1.62;
        lp += (1 - dark) * 0.32 * (s - lp);
        double env = smoothstep(u / attack);
        if (u > dur) env *= fmax(0, 1 - (u - dur) / 0.9);
        emit(BUS_MUSIC, i0 + i, lp * env * gain * tail_fade(i, n), pan, &sp, side, dly, 0);
    }
}
static void bass(double t, double st, double dur, double gain, double attack) {
    long n = lround((dur + 0.16) * SR), i0 = lround(t * SR);
    double f = hz_of(st);
    double p1 = 0, p2 = 0, p3 = 0, lp = 0;
    for (long i = 0; i < n; i++) {
        double u = i / (double)SR;
        p1 += (TAU * f) / SR; p2 += (TAU * f * 0.5) / SR; p3 += (TAU * f * 2) / SR;
        double env = smoothstep(u / attack);
        if (u > dur) env *= fmax(0, 1 - (u - dur) / 0.16);
        double s = sin(p1) + 0.48 * sin(p2) + 0.07 * sin(p3);
        lp += 0.42 * (s - lp);
        emit(BUS_MUSIC, i0 + i, lp * 0.38 * env * gain * tail_fade(i, n), 0, NULL, 0, 0, 0);
    }
}
// pluck — the music box that plays only her notes: a detuned sine pair
// plus a 2.7× partial, 6 ms attack, decay scaled to the note length
static void pluck(double t, double st, double dur, double gain, double pan) {
    double ring = fmin(dur * 1.15 + 0.25, 1.6);
    long n = lround(ring * SR), i0 = lround(t * SR);
    Spatial sp = spatial(pan * 1.2);
    double f = hz_of(st);
    double p1 = 0, p2 = 0, p3 = 0;
    for (long i = 0; i < n; i++) {
        double u = i / (double)SR;
        p1 += (TAU * f) / SR;
        p2 += (TAU * f * 1.003) / SR;
        p3 += (TAU * f * 2.7) / SR;
        double env = fmin(1, u / 0.006) * exp(-u * (3.4 / ring));
        double s = (sin(p1) + 0.85 * sin(p2)) * 0.5 + 0.22 * sin(p3) * exp(-u * 11);
        emit(BUS_MUSIC, i0 + i, s * 0.30 * env * gain * tail_fade(i, n),
             pan, &sp, 0.55, 0.22, 0);
    }
}
// play a chart phrase's melody as pluck notes — the band doubling her,
// or answering in the gaps (octave 0 doubles; +12 answers up high)
static void pluck_line(const char *name, double bar, double gain, int oct, double pan) {
    const ChartPhrase *p = phrase_of(name);
    for (int i = 0; i < p->n; i++) {
        const ChartNote *nn = &p->notes[i];
        pluck(at(bar) + nn->beat * BEAT + jit(3), nn->st + oct,
              nn->dur * BEAT, gain * (1 - 0.03 * i), pan * ((i % 2) ? -1 : 1));
    }
}

// ── FEM bells — the /pop bell voice, in this engine ────────────────────
// @jeffrey: "lets extend this now a bit and add fem bells · bring in our
// usual /pop sounds". pop/bell/ solves a real shell by finite elements and
// bakes wavs; that is the right tool for a bell record and the wrong one
// for a voice inside a 2-minute dance cut, so this is its OUTPUT shape: a
// modal sum on the partial ratios a struck bell actually has, each partial
// with its own decay and a little beating between a detuned pair.
//
// The house rules from pop/wattajetta come with it, and they are rules
// because breaking them made that record tangy on a laptop:
//   · runs stay at or under E5 — see BELL_CEIL_ST
//   · bells NEVER pass through saturation; this writes to BUS_MUSIC clean
//   · a choked strike gets a real fade, never a truncation
static const double BELL_PARTIAL[6] = { 0.5, 1.0, 1.19, 1.5, 2.0, 2.51 };
static const double BELL_GAIN[6]    = { 0.42, 1.0, 0.55, 0.38, 0.30, 0.14 };
static const double BELL_DECAY[6]   = { 1.6, 2.6, 3.4, 4.2, 5.4, 8.0 };
// THE CEILING, RAISED. @jeffrey: "lets bring up the fem bel pitch". The
// E5 line came from pop/wattajetta, where E5–E6 runs read "too high and
// tangy on my macbook speakers" — that lesson is about MELODIC bell runs
// carrying the tune, and these are answering a vocal rather than carrying
// it. So the ceiling goes to A5 and the runs start an octave higher, with
// the compensation the same memory prescribes: the sixth octave is for
// SPARKLE, so anything above E5 loses gain rather than keeping it. Bells
// still never touch saturation — that is the part of the rule that is not
// negotiable, and it is why they go straight to BUS_MUSIC.
#define BELL_CEIL_ST 23.0   // A5 in the take's frame
#define BELL_TANG_ST 18.0   // …but above E5 it pays for the altitude

static void fembell(double t, double st, double dur, double gain, double pan) {
    while (st > BELL_CEIL_ST) st -= 12;          // the ceiling is not optional
    if (st > BELL_TANG_ST)                       // sparkle, not melody
        gain *= 1.0 - 0.42 * ((st - BELL_TANG_ST) / (BELL_CEIL_ST - BELL_TANG_ST));
    double ring = fmin(dur * 1.4 + 0.5, 3.2);
    long n = lround(ring * SR), i0 = lround(t * SR);
    Spatial sp = spatial(pan);
    double f = hz_of(st);
    double ph[6][2] = {{0}};
    // the strike: a short bright contact before the modes take over
    long nc = lround(0.006 * SR);
    for (long i = 0; i < n; i++) {
        double u = i / (double)SR;
        double s = 0;
        for (int k = 0; k < 6; k++) {
            double fk = f * BELL_PARTIAL[k];
            ph[k][0] += (TAU * fk) / SR;
            ph[k][1] += (TAU * fk * 1.0018) / SR;   // the beat in a real bell
            s += BELL_GAIN[k] * exp(-u * BELL_DECAY[k])
                 * (sin(ph[k][0]) + 0.8 * sin(ph[k][1]));
        }
        s /= 3.4;
        if (i < nc) s += 0.30 * (rnd() * 2 - 1) * (1 - i / (double)nc);
        // a truncated ring clicks — fade the last 80 ms, always
        double env = 1.0;
        double left = (n - i) / (double)SR;
        if (left < 0.080) env = left / 0.080;
        emit(BUS_MUSIC, i0 + i, s * env * gain * tail_fade(i, n), pan, &sp,
             0.45, 0.18, 0.42);
    }
}



// ── harmony — the v3 chords on a one-bar dance rhythm ──────────────────
// …and the SEVENTH. @jeffrey: "more jazz". A triad is a fact; a seventh is
// an opinion. These are the same four chords the record has always used —
// nothing about the harmony moves — but each now carries the note that
// makes it lean somewhere: minor sevenths on i and VI, a major seventh on
// III where the tune is already at its brightest, and a ninth on VII
// because it is the turnaround and wants to be unresolved. Only the stab
// and the pad reach for it; the bass and the bells stay on the triad, so
// the colour arrives without the bottom getting muddy.
typedef struct { double root; double tones[3]; double sev; } Chord;
static const Chord CH_i   = { 0,  { 0, 3, 7 },   10 };   // i7
static const Chord CH_III = { 3,  { 3, 7, 10 },  14 };   // III maj7 → 9
static const Chord CH_VI  = { -4, { -4, 0, 3 },   6 };   // VI7
static const Chord CH_VII = { -2, { -2, 2, 5 },   9 };   // VII add9
// per section: 8-bar rows (dance harmonic rhythm: one chord per bar)
static const Chord *ROW_VERSE[8] = { &CH_i, &CH_i, &CH_VI, &CH_VI, &CH_III, &CH_III, &CH_VII, &CH_VII };
static const Chord *ROW_HOOK[8]  = { &CH_VI, &CH_VII, &CH_i, &CH_i, &CH_VI, &CH_VII, &CH_III, &CH_VII };
static const Chord *ROW_BREAK[8] = { &CH_i, &CH_i, &CH_VI, &CH_VI, &CH_i, &CH_i, &CH_VII, &CH_VII };
// LONERCLUB, the narrative — @jeffrey: "i want our mix to have drops and
// arepggios · and like nice overall narrative · lets call the track
// Lonerclub". Every block that holds the whole take is SIXTEEN bars now:
// the charted line runs 60 beats, and the old 12-bar blocks made her
// sing straight through the section after her.
//
// SIX PASSES, AND THE LEAD CHANGES HANDS. @jeffrey: "lets work on swap
// lead and also use / bring in group takes · but the idea is we start
// small with camille's softest take then we build up each one · maybe
// even alonging / getting things longer a bit". The takes were MEASURED
// and they already form the arc: f −22.1 LUFS (the Feral spine, softest,
// solo), s −16.8 (roomier and lower, solo), o −12.2 (the ensemble, with
// @jeffrey and Alex). So the record starts on the quietest thing she ever
// sang and ends with a room full of people singing it.
//
//   L1  0–16   0:00  f- alone. No harmony at all.
//   L2 16–32   0:31  f- and her low 3rd
//   L3 32–48   1:03  the S TAKE takes the lead — floor thins, kick stays
//   L4 48–64   1:34  f- back, 3rd and 5th, bells arpeggiating
//   L5 64–80   2:06  the GROUP take leads. The club sings it.
//   L6 80–96   2:37  the ghost, thinning to the pluck alone
static const Chord *chord_at(int bar) {
    int b = bar - INTRO;                    // the intro rides L1's changes
    if (b < 0) b += 16;
    if (b < 16) return ROW_VERSE[b % 8];
    if (b < 32) return ROW_HOOK[b % 8];
    if (b < 48) return ROW_BREAK[b % 8];
    if (b < 64) return ROW_VERSE[b % 8];
    if (b < 80) return ROW_HOOK[b % 8];
    return ROW_BREAK[b % 8];
}
static int kick_on(int bar) {           // the strict floor: where it runs
    // v4pid: there is always a beat — the walking bass runs to the tail
    return bar < BARS - 4;
}

// A bell RUN — chord tones climbing from the tonic, swung a little so it
// rolls rather than marches. This is the arpeggio @jeffrey asked the mix
// for ("i want our mix to have drops and arepggios"), played by the bell.
static void bell_run(double t, const Chord *c, int count, double step,
                     double gain, int up, double pan) {
    for (int k = 0; k < count; k++) {
        int idx = up ? k : count - 1 - k;
        double st = c->tones[idx % 3] + 12 * (idx / 3) + 12;
        fembell(t + k * step + ((k % 2) ? step * 0.08 : 0) + jit(3),
                st, step * 1.6, gain * (1 - 0.05 * k),
                pan * ((k % 2) ? -1 : 1));
    }
}

// ── THE HOOK ──────────────────────────────────────────────────────────
// @jeffrey: "our instrumentation should be smarter compositionally / more
// structured". What it was doing was texture: every part played all the
// time inside its section, the pluck doubled her line in unison, and the
// bells ran the same arpeggio whatever bar they were in. Nothing was ever
// STATED, so nothing could be answered or returned to.
//
// This is the figure the record quotes. It is the first four notes of her
// own melody — 7 5 3 2, the descent under "sit-ting curled up" — because a
// hook the band invents is decoration, and a hook lifted from the singer
// is the song. It is stated at the head of a phrase and ANSWERED four bars
// later by its inversion, which is the oldest structure there is:
// antecedent, then consequent.
static const int HOOK[4]     = { 7, 5, 3, 2 };
static const int HOOK_ANS[4] = { 2, 3, 5, 7 };
enum { VOICE_PLUCK, VOICE_VIBE, VOICE_BELL };

static void hook_say(double t, const int *fig, int oct, int voice,
                     double gain, double pan) {
    for (int k = 0; k < 4; k++) {
        double u = t + k * (BEAT / 2.0) + jit(4);
        double st = fig[k] + oct;
        double p = pan * ((k % 2) ? -0.6 : 1.0);
        switch (voice) {
        case VOICE_VIBE:  vibe(u, st, 0.9, gain, p); break;
        case VOICE_BELL:  fembell(u, st, 1.1, gain * 0.8, p); break;
        default:          pluck(u, st, 0.75, gain, p); break;
        }
    }
}

// THE TURNAROUND. Eight-bar rows that simply repeat have no edge to them;
// this marks bar 7 of each row so the ear can hear a phrase close and
// another begin. It is the VII chord walking up into the downbeat — the
// one place in the harmony that is already unresolved.
static void turnaround(double bar, double gain) {
    static const int UP[3] = { -2, 2, 5 };
    for (int k = 0; k < 3; k++)
        pluck(at(bar) + (2.0 + k * 0.5) * BEAT + jit(4), UP[k] + 12,
              0.5, gain * (0.7 + 0.15 * k), (k % 2) ? 0.30 : -0.26);
    fembell(at(bar) + 3.5 * BEAT, 9, 0.9, gain * 0.5, 0.2);
}


// ── the backing voice — one sine, one note at a time, under her ────────
// @jeffrey: "canwe start to bring in piano / sine pad accompaniment ·
// just a single voice to back the vocal". Deliberately NOT the chord
// stack the bed already plays: one voice, held, moving as little as the
// harmony will let it and re-articulating only when it has to move. It
// lives in the octave under her lowest sung note (G#3), so it holds the
// chord up without ever arriving in the same place as a word — and it is
// `pad()` with nst = 1, which is already a detuned sine pair under a
// tape-wow lid, rather than a second synth doing the same job.
// The register. It was A#2–G3 (103–195 Hz) — genuinely under her, and
// genuinely inaudible on anything without a woofer: @jeffrey, "im not
// hearing those instruments". Moved up a fifth into the range a laptop
// and a phone actually reproduce, still below her sung line (her lowest
// is G#3, −5, and she spends the song above it).
#define BACK_LO  (-7.0)                 // F3 up to C4
#define BACK_HI   (3.0)
static double backing_note(const Chord *c, double from, int first) {
    double best = BACK_LO, bestd = 1e9;
    for (int k = 0; k < 3; k++)
        for (int oct = -2; oct <= 1; oct++) {
            double st = c->tones[k] + 12 * oct;
            if (st < BACK_LO || st > BACK_HI) continue;
            // voice-leading: the nearest tone to where the voice already
            // is. Only the first note is placed by register instead.
            double d = first ? fabs(st + 9) : fabs(st - from);
            if (d < bestd) { bestd = d; best = st; }
        }
    return best;
}
// One call per HELD note, not per bar — a bar whose chord keeps the
// voice's note just lets it ring rather than striking it again.
static void backing_line(int bar0, int bars, double off, double gain) {
    double cur = 0;
    int start = -1;
    for (int bar = 0; bar <= bars; bar++) {
        double st = 0;
        int have = bar < bars;
        if (have) st = backing_note(chord_at(bar0 + bar), cur, start < 0);
        if (start >= 0 && (!have || st != cur)) {
            pad(off + at(bar0 + start), &cur, 1, (bar - start) * BAR - 0.06,
                gain, 0.55, 0.0, 0.35, 0.08, 0.26);
            start = -1;
        }
        if (have && start < 0) { cur = st; start = bar; }
    }
}

// ── the floor — one bar of the dance kit ───────────────────────────────
// @jeffrey: "simple perc". Kick on the four, one hat on the offbeat, a
// clap on 2 and 4. The 16th-note tick bed, the snare backbeat, the fills
// and the risers are gone. They were the difference between a track that
// moves and a track that is busy, and every one of them lived in the same
// 4–8 kHz band as her consonants.
// VELOCITY, as a player would place it. Four identical kicks a bar is the
// machine; a drummer leans on 1, lets 2 breathe, half-leans on 3. The
// pattern repeats over two bars so it is a groove and not a wobble.
static const double KV[8] = { 1.00, 0.90, 0.96, 0.88, 0.98, 0.87, 0.94, 0.91 };
static const double HV[8] = { 0.92, 1.00, 0.86, 0.97, 0.95, 1.00, 0.84, 0.99 };

static void floor_bar(int bar, double kickG, double hatG, int claps) {
    double t = at(bar);
    for (int b = 0; b < 4; b++) {
        double d = lean(bar, b);
        kick(t + b * BEAT + d, kickG * KV[(bar * 4 + b) % 8]);
    }
    if (claps) {
        // the clap lands a hair LATE, always — it is the one thing in a
        // house bar that is allowed to be behind the beat, and it is most
        // of what makes the two and four feel like a person
        clap(t + 1 * BEAT + lean(bar, 1) + 0.010, 0.66, 0.08);
        clap(t + 3 * BEAT + lean(bar, 3) + 0.013, 0.64, -0.06);
    }
    // the "and" of 2 and 4 rings; the other two stay closed — and every
    // one of them is SWUNG, so the offbeat arrives late the way a
    // shuffle does rather than exactly halfway
    for (int b = 0; b < 4; b++)
        airhat(t + sw(b * 2 + 1) + lean(bar, b + 0.5),
               hatG * HV[(bar * 4 + b) % 8], (b % 2) ? 0.28 : -0.24, b % 2);
    // GHOST HATS on the last 16th of every other beat: barely there,
    // swung with everything else, and the reason the bar has an inside.
    for (int b = 0; b < 4; b++) {
        if ((bar + b) % 2) continue;
        airhat(t + sw(b * 2 + 1.5) + lean(bar, b + 0.75),
               hatG * 0.34, (b % 2) ? -0.36 : 0.34, 0);
    }
    // GHOST SNARES — the brush, under everything, on the 16ths a drummer
    // fills with their left hand. Never on 2 and 4: the clap owns those,
    // and doubling them would just make the backbeat louder rather than
    // making the bar breathe.
    if (claps) {
        snare(t + sw(2.5) + lean(bar, 1.25), 0.16, 0.30, -0.22);
        snare(t + sw(6.5) + lean(bar, 3.25), 0.13, 0.26, 0.24);
        if (bar % 4 == 3) {
            snare(t + sw(6) + lean(bar, 3.0), 0.22, 0.55, 0.10);
            snare(t + sw(7) + lean(bar, 3.5), 0.30, 0.75, -0.12);
        }
    }

    // ── POLYRHYTHM ────────────────────────────────────────────────────
    // @jeffrey: "and polythyrhtms!". Two of them, both against the same
    // 4/4 floor and neither of them agreeing with it:
    //
    //   THREE against four — a brush every 3 sixteenths. Sixteen 16ths in
    //   a bar and a period of 3 means the pattern only lands on the
    //   downbeat again every THREE bars, so the same figure keeps
    //   arriving somewhere new.
    //   FIVE against four — a closed hat every 5 sixteenths, which takes
    //   FIVE bars to come round. The two cycles agree once every fifteen.
    //
    // Both are quiet. A polyrhythm you can pick out is a competing beat;
    // one you can only feel is a groove.
    {
        long step = (long)bar * 16;
        for (int q = 0; q < 16; q++) {
            long g = step + q;
            if (g % 3 == 0)
                snare(t + sw(q) + lean(bar, q / 4.0), 0.085, 0.16,
                      (g % 6) ? 0.40 : -0.40);
            if (g % 5 == 0)
                airhat(t + sw(q) + lean(bar, q / 4.0), hatG * 0.30,
                       (g % 10) ? -0.42 : 0.42, 0);
        }
    }
}

// ── sidechain — the pump, keyed by every kick ──────────────────────────
static float *duck_env(double depth, double atk, double rel) {
    float *e = (float *)malloc(N * sizeof(float));
    for (long i = 0; i < N; i++) e[i] = 1;
    long pre = lround(0.010 * SR);
    for (int k = 0; k < kickN; k++) {
        long i0 = lround(kickT[k] * SR) - pre;
        long span = pre + lround((atk + rel) * SR);
        for (long i = 0; i < span; i++) {
            long j = i0 + i;
            if (j < 0) continue;
            if (j >= N) break;
            double dt = i / (double)SR - 0.010;
            double g;
            if (dt < atk) g = 1 - depth * clampd((dt + 0.010) / (0.010 + atk), 0, 1);
            else { double u = clampd((dt - atk) / rel, 0, 1); g = (1 - depth) + depth * smoothstep(u); }
            if (g < e[j]) e[j] = (float)g;
        }
    }
    return e;
}

// ═══ main ══════════════════════════════════════════════════════════════
static int minimal_bars(void) {
    // the line once, plus a bar to breathe — and never past bar 16
    // (@jeffrey: "no need to go past bar 16"); the line ends inside 15.
    int b = (int)(ceil(phrase_of("w-whole-line")->beats / 4.0) + 2);
    return b < 16 ? b : 16;
}

int main(void) {
    // MINIMAL=1 → the study pass: kick + the unbroken vocal, nothing
    // else — "lets start with just kick and vocals and get that right".
    int minimal = getenv("MINIMAL") != NULL;
    BEAT = 60.0 / BPM; BAR = 4 * BEAT; STEP = BEAT / 4;
    // exactly the runway the first sung phrase needs, and not a sample
    // more — see the note above at().
    PREROLL = INTRO > 0 ? 0.0 : phrase_of("w-whole-line")->leadIn;
    // the study stops at bar 16 and needs only enough tail for the last
    // note to ring — six seconds of nothing was six seconds of watching
    N = lround((PREROLL + (minimal ? minimal_bars() : BARS) * BAR
                + (minimal ? 2.0 : TAIL_S)) * SR);
    drumsL = calloc(N, 4); drumsR = calloc(N, 4);
    musicL = calloc(N, 4); musicR = calloc(N, 4);
    voxL = calloc(N, 4); voxR = calloc(N, 4);
    sideV = calloc(N, 4); sideB = calloc(N, 4);
    dlySend = calloc(N, 4); rvbSend = calloc(N, 4);

    printf("→ scoring %d bars @ %.0f BPM · A# minor @ %.0f Hz · four on the floor\n",
           BARS, BPM, TONIC);
    bank_load_dir("vox4");
    bank_load_dir("vox3");

    if (minimal) {
        // ONE run of the line — @jeffrey: "lets only render a single run
        // of the vocals · no need to do anything other than the first
        // loop". The study is for scrutinising the alignment, and a
        // second pass only doubles the sitting-through.
        // NO COUNT-IN — the file opens on her pickup. The render starts
        // at the /s/ of "sitting"; the phrase's beat 0, and the first
        // kick with it, land one lead-in later, so the downbeat IS the
        // first word rather than the third bar of waiting.
        // WHOSE CHART GOVERNS THE STUDY. Each take now has one of its own
        // (bin/takechart.py), and its leadIn is its own measured consonant
        // runway — 0.37 beats for s-, 0.84 for sh-. Reading f-'s here
        // would put every other take's pickup in the wrong place, which
        // is the borrowed-envelope problem the takecharts exist to end.
        const char *takeEnv = getenv("TAKE");
        const char *takePhrase = (takeEnv && *takeEnv) ? takeEnv : "w-whole-line";
        const ChartPhrase *p = phrase_of(takePhrase);
        double lineBars = ceil(p->beats / 4.0);
        int kickBars = (int)(lineBars + 1);
        if (kickBars > 16) kickBars = 16;
        double off = p->leadIn;                 // the pickup sits ahead of beat 0
        printf("  MINIMAL — kick + vocals, ONE pass, no count-in, line %.0f beats\n",
               p->beats);
        // hats too — @jeffrey: "can we add little hi hats so we can get a
        // better head on the beat in this monitor video?" Four on the
        // floor alone tells you where the bar is but not where you are
        // inside it; offbeat air-hats give the 8ths, and quiet 16th ticks
        // give the subdivision the words are actually being placed on.
        // no pickup kick — there is no pickup beat any more
        for (int bar = 0; bar < kickBars; bar++) {
            double t = off + at(bar);
            for (int b = 0; b < 4; b++) kick(t + b * BEAT, 0.62);
            for (int b = 0; b < 4; b++)
                airhat(t + (b + 0.5) * BEAT, 0.14, (b % 2) ? 0.3 : -0.3, b % 2);
        }
        // the backing voice, under the words — one held note at a time
        backing_line(0, kickBars, off, 0.55);
        // …and the bed under THAT — @jeffrey: "and also lets add the synth
        // pads / sine accompaniment now!". The full mix's voicing, at study
        // level: a low pair on two-bar chords, and the mid triad a beat
        // behind it so the chord arrives rather than lands.
        for (int bar = 0; bar < kickBars; bar += 2) {
            const Chord *c = chord_at(bar);
            double lows[3] = { c->root - 24, c->root - 12, c->tones[1] - 12 };
            pad(off + at(bar) + 0.02, lows, 3, 2 * BAR - 0.1, 0.42, 1.1, 0, 0.55, 0, 0.30);
            double mids[3] = { c->tones[0], c->tones[1], c->tones[2] };
            pad(off + at(bar) + 0.15, mids, 3, 2 * BAR - 0.3, 0.55, 1.5, 0.18, 0.7, 0.10, 0.36);
        }
        // HER MELODY, on the music box — @jeffrey: "lets start working on
        // more accompaniment". The pluck plays ONLY charted notes, so the
        // band's line IS her line: it doubles under every word and rings
        // on through the gaps she leaves.
        {
            const ChartPhrase *pp = phrase_of("w-whole-line");
            for (int i = 0; i < pp->n; i++) {
                const ChartNote *nn = &pp->notes[i];
                // IN OCTAVES — @jeffrey: "i was hoping for notes in the
                // octaves · like so we could have the lyrics and melody
                // being more closely mapped". Her note, the octave under
                // it for weight, and the octave over it for the mapping
                // to be unmissable. The low octave leads slightly so the
                // top one reads as the melody rather than as a chord.
                double t0 = off + nn->beat * BEAT + jit(3);
                double d = nn->dur * BEAT;
                double pn = (i % 2) ? -0.12 : 0.12;
                pluck(t0,          nn->st,      d, 0.62, pn);
                pluck(t0 - 0.006,  nn->st - 12, d, 0.30, -pn * 0.5);
                pluck(t0 + 0.010,  nn->st + 12, d, 0.36, pn * 1.6);
            }
        }
        // and the sub, offbeat, answering each kick
        for (int bar = 0; bar < kickBars; bar++) {
            const Chord *c = chord_at(bar);
            for (int b = 0; b < 4; b++)
                bass(off + at(bar) + (b + 0.5) * BEAT, c->root - 24,
                     0.30, 0.55, 0.012);
        }
        // …and TAKE_WAV is what it actually sings, so the study can play a
        // level-matched copy of a take while still reading that take's
        // own chart. @jeffrey: "see how they sound solo · with mp4 vocal
        // tests again?"
        const char *wavEnv = getenv("TAKE_WAV");
        sung((wavEnv && *wavEnv) ? wavEnv : takePhrase, 0.0, 0.98, 0, 0.0);
        goto mixdown;
    }

    // THE BED — pads on one-bar chords everywhere the floor runs; the
    // break gets the slow-attack ballad voicing.
    for (int bar = 0; bar < BARS - 4; bar++) {
        const Chord *c = chord_at(bar);
        double lows[3] = { c->root - 24, c->root - 12, c->tones[1] - 12 };
        int brk = 0, outro = 0;   // v4pid: no break, no outro — one build
        if (bar % 2 == 0) {
            double g = brk ? 0.15 : outro ? 0.13 : 0.16;
            pad(at(bar) + 0.02, lows, 3, 2 * BAR - 0.1, g,
                brk ? 2.4 : 1.1, 0, 0.55, 0, 0.30);
            if (!brk && !outro) {
                double mids[4] = { c->tones[0], c->tones[1], c->tones[2], c->sev };
                pad(at(bar) + 0.15, mids, 4, 2 * BAR - 0.3, g * 0.68,
                    1.5, 0.18, 0.7, 0.10, 0.36);
            }
        }
        // THE BASS — @jeffrey: "can we make it cooler? more up beat".
        // It was four identical offbeat subs a bar, at root−24, which is
        // a pad with a rhythm rather than a bassline: nothing in it moves,
        // so nothing in it pushes. Now it walks. Root on the offbeats, the
        // fifth under beat 3 so the bar turns over, and a 16th PUSH into
        // the next downbeat — the note that arrives early is what makes a
        // house bar feel like it is falling forward. An octave up on the
        // last offbeat of every other bar keeps it from settling.
        if (kick_on(bar)) {
            for (int b = 0; b < 4; b++) {
                double st = c->root - 24;
                if (b == 2) st = c->tones[2] - 24;         // the fifth, mid-bar
                if (b == 3 && (bar % 2)) st += 12;         // …and a lift out
                // swung with the hats: if the bass sat square while the kit
                // shuffled they would fight, and the groove would read as
                // sloppy rather than as swung
                bass(at(bar) + sw(b * 2 + 1) + lean(bar, b + 0.5), st,
                     0.30, 0.92 * KV[(bar * 4 + b) % 8], 0.012);
            }
            // the push: a 16th before the next bar, short and quiet, so the
            // downbeat is arrived AT rather than merely landed on
            bass(at(bar) + sw(7.5) + lean(bar, 3.75), c->root - 24,
                 0.13, 0.62, 0.008);
        } else {
            bass(at(bar), c->root - 24, BAR - 0.12, 0.5, 0.045);
        }
    }

    // THE FLOOR
    // THE FLOOR NEVER STOPS. @jeffrey: "once the beat / kick starts it
    // shouldn't stop it should run through the whole time". The BREAK used
    // to take the kick out entirely for sixteen bars — a dance-record
    // reflex, and the wrong one here: this is a club cut and the floor is
    // the thing you are standing on. So the break still empties, but it
    // empties AROUND the kick: no claps, no hats, no ticks, and the kick
    // itself pulled back to 0.72 so the room opens without the ground
    // disappearing. Everything that made the section an event — the
    // harmony thinning, the riser back in — is still there.
    // THE VOCAL BREAKS clear the kit for their two bars — no claps, hats
    // right down — while the kick keeps running. See vocal_break().
    // v4pid: @jeffrey — "i prefer there to always be a beat". The floor runs
    // every bar of the record, and the build lives in its WEIGHT: each pass
    // brings the kick up a step, the vocal breaks clear the kit for two bars
    // while the kick keeps running, and claps arrive with the second pass.
    for (int bar = 0; bar < BARS; bar++) {
        int vb = (bar >= L(1) - 2 && bar < L(1)) || (bar >= L(2) - 2 && bar < L(2));
        double kg = vb ? 0.82
                  : bar < L(1) ? 0.90 : bar < L(2) ? 0.96 : 1.0;
        double hg = vb ? 0.03 : bar < L(1) ? 0.11 : 0.15;
        int claps = (bar >= L(1)) && !vb;
        floor_bar(bar, kg, hg, claps);
    }
    {   // the words each break is built from — the scoop, the low held
        // note, and the last word of the sentence
        static const char *W1[] = { "pa", "stone" };
        static const char *W2[] = { "pa", "stone", "pass" };
        static const char *W3[] = { "pa", "think", "stone", "pass" };
        vocal_break("w-whole-line", L(1) - 2, W1, 2, 0.34);
        vocal_break("w-whole-line", L(2) - 2, W3, 4, 0.48);
        (void)W2;
    }
    // ── LONERCLUB — SIX PASSES OF ONE SENTENCE, ONE VOICE ─────────────
    // @jeffrey: "i want to try and just get a real clean track for now...
    // simple perc simple vocals... and lets not have background vocals
    // anymore".
    //
    // So every halo, every backup 3rd and 5th, every "ah"/"oh" arp and
    // every ensemble texture shot is gone. What is left is HER LINE, sung
    // whole, six times, and a band that never sings. The passes are told
    // apart by what the band does — the pluck thickening, the bells
    // arriving, the floor thinning — and nothing else.
    //
    // THE TAKES ROTATE — @jeffrey (2026-08-24): voice the passes with the
    // auditioned takes, each pass a different performance of the sentence.
    // ("Stick with our original mapped take" was the rule while the others
    // were still borrowing f-'s chart; takechart.py gave each its own, the
    // batch was auditioned in out/takes/, and now they earn passes.)
    //
    // The order is the club filling and emptying. f- opens — hers is the
    // hand-charted take the PREROLL math is built around. Then the sentence
    // is handed around the room: lg, the stranger nearest her register; cp,
    // Camille an octave down for the bare pass; pf carries the drop at her
    // octave; o — the group take, jeffrey and alex in it — sings the widest
    // pass; and s, her roomier lower take, is the ghost. The band and every
    // sampled gesture (scratches, throws, the breaks) stay f- throughout:
    // the club changes singers, the thread stays hers.
    //
    //   L1  0–16  f-  her pickup opens the file, kick from bar 0, lean band
    //   L2 16–32  lg  claps arrive, the stab still muffled — the build
    //   L3 32–48  o   the drop lands everything at once, widest, bells every bar
    //
    // (v4pid, final shape: @jeffrey — start on the sung "sitting", always a
    // beat, one build, ~1:40. Three passes; cp/pf/s sit this cut out.)

    // THE DOWNBEAT still gets its bell, but it rings UNDER her first word
    // now rather than in front of it — the record opens on a note and a
    // voice at the same instant.
    fembell(at(0), 0, 2.6, 0.26, 0.0);
    fembell(at(0) + 0.012, 12, 2.2, 0.14, 0.28);

    // ── THE ROTA ─────────────────────────────────────────────────────
    // Who says the hook, pass by pass. Handing one figure between
    // instruments is what makes an arrangement read as structured rather
    // than merely layered: the same four notes arrive in a different
    // voice each time, so the ear tracks a thread instead of a texture.
    // The statement lands on bar 1 of the pass; the ANSWER — the same
    // figure inverted — lands on bar 5, and again on bar 13, so every
    // pass is two four-bar sentences and a repeat.
    //
    // The turnaround closes each eight-bar row underneath all of it.
    {
        static const struct { int voice, oct; double gain; } SAYS[3] = {
            { VOICE_PLUCK, 0,   0.30 },     // L1  the music box opens it
            { VOICE_VIBE,  0,   0.34 },     // L2  the vibraphone takes it
            { VOICE_PLUCK, 12,  0.28 },     // L3  widest — pluck and vibe
        };
        for (int pass = 0; pass < 3; pass++) {
            double b0 = L(pass);
            double g = SAYS[pass].gain;
            hook_say(at(b0 + 1),  HOOK,     SAYS[pass].oct, SAYS[pass].voice, g, 0.32);
            hook_say(at(b0 + 5),  HOOK_ANS, SAYS[pass].oct, SAYS[pass].voice, g * 0.9, -0.32);
            hook_say(at(b0 + 13), HOOK_ANS, SAYS[pass].oct, SAYS[pass].voice, g * 0.8, 0.28);
            if (pass == 2)      // the widest pass says it in two voices
                hook_say(at(b0 + 5) + 0.5 * BEAT, HOOK_ANS, 0, VOICE_VIBE,
                         g * 0.7, -0.36);
            for (int row = 0; row < 2; row++)
                turnaround(b0 + row * 8 + 7, 0.26);
        }
    }

    // ── L1 0–16 — her pickup, the kick, and a lean band ──────────────
    // The file opens on the sung "sitting" (PREROLL is exactly her lead-in)
    // and the kick is under her from beat one. Lean on purpose — this is
    // the bottom of the build, not the record being shy.
    voice_line("w-whole-line", L(0), 0.90);
    pluck_line("w-whole-line", L(0), 0.48, 0, 0.3);
    bell_run(at(L(0) + 6), &CH_VI, 3, 0.30, 0.13, 1, 0.35);
    bell_run(at(L(0) + 13), &CH_VII, 3, 0.28, 0.12, 0, -0.35);

    // THE VIBRAPHONE answers her held notes. Every unit four beats or
    // longer is a place where she stops moving, and the vibe fills it with
    // a falling three-note figure off the bar's chord — so the new
    // instrument is not another layer running in parallel, it plays in the
    // gaps the singer leaves.
    {
        const ChartPhrase *pp = phrase_of("w-whole-line");
        for (int pass = 1; pass < 3; pass++) {
            double base = L(pass);
            double g = (pass == 1) ? 0.26 : 0.34;
            for (int i = 0; i < pp->n; i++) {
                const ChartNote *nn = &pp->notes[i];
                if (nn->dur < 4.0) continue;
                double t0 = at(base) + (nn->beat + nn->dur * 0.55) * BEAT;
                const Chord *c = chord_at((int)(base + nn->beat / 4.0));
                for (int k = 0; k < 3; k++)
                    vibe(t0 + k * 0.5 * BEAT, c->tones[2 - k] + 12,
                         1.2, g * (1.0 - 0.18 * k), (k % 2 ? 0.34 : -0.30));
            }
        }
    }

    // ── L2 16–32 — the clap, and her melody in octaves ────────────────
    voice_line("w-whole-line", L(1), 0.88);
    pluck_line("w-whole-line", L(1), 0.52, 0, 0.3);
    pluck_line("w-whole-line", L(1), 0.24, 12, -0.3);
    for (int bar = L(1) + 2; bar < L(2); bar += 4)
        bell_run(at(bar) + 2.5 * BEAT, chord_at(bar), 4, 0.26, 0.16,
                 (bar / 4) % 2 == 0, 0.4);
    // THE STAB arrives with the clap, still muffled — the filter opens
    // pass by pass, so one figure carries the whole build.
    for (int bar = L(1) + 4; bar < L(2); bar++) {
        const Chord *c = chord_at(bar);
        double v[4] = { c->tones[0], c->tones[1], c->tones[2], c->sev };
        stab(at(bar) + sw(3) + lean(bar, 1.5), v, 4, 0.34, 0.26, 0.22, 0.30);
        stab(at(bar) + sw(7) + lean(bar, 3.5), v, 4, 0.30, 0.22, 0.22, -0.30);
    }

    // ── THE DROP ─────────────────────────────────────────────────────
    // @jeffrey: "and more epic drop". Four bars of preparation instead of
    // none: the riser runs the whole way, the scratches answer each other
    // across the stereo, and in the last bar the tape STOPS — screw_down
    // drags "stone" from full speed to a third of it, so the pitch sags
    // out from under the room. Then the floor returns at tempo, and a bell
    // is struck on the downbeat itself so the drop arrives on a note and
    // not only on a kick.
    riser(at(L(2) - 4), 4 * BAR, 0.20);
    scratch("w-whole-line", "pa", at(L(2) - 4) + 2 * BEAT, 4, 0.34, 0.40);
    scratch("w-whole-line", "pass", at(L(2) - 3) + 2 * BEAT, 5, 0.36, -0.40);
    scratch("w-whole-line", "pa", at(L(2) - 2) + 1 * BEAT, 6, 0.40, 0.42);
    screw_down("w-whole-line", "stone", at(L(2) - 1), 1.5 * BAR, 0.62);
    fembell(at(L(2)), 0, 3.4, 0.62, 0);
    fembell(at(L(2)) + 0.012, 12, 3.4, 0.34, 0.25);
    // the floor already puts a kick on this downbeat; these are the three
    // 16ths AFTER it, so the drop stutters in rather than doubling a hit
    for (int b = 1; b < 4; b++)
        kick(at(L(2)) + b * (BEAT / 4.0), 0.54 - 0.12 * b);

    // ── L3 32–48 — the drop lands everything at once ─────────────────
    // THE STAB RESTS WHERE THE HOOK SPEAKS. Two ideas on the offbeat at
    // once is not counterpoint, it is a pile; the arrangement reads as
    // deliberate the moment something stops to let something else through.
    voice_line("w-whole-line", L(2), 0.98);
    pluck_line("w-whole-line", L(2), 0.52, 0, 0.3);
    pluck_line("w-whole-line", L(2), 0.30, 12, -0.3);
    pluck_line("w-whole-line", L(2), 0.18, -12, 0.15);
    for (int bar = L(2); bar < L(3); bar += 2)
        bell_run(at(bar) + 1.0 * BEAT, chord_at(bar), 5, 0.22, 0.22,
                 (bar / 2) % 2, (bar % 4) ? 0.44 : -0.44);
    for (int bar = L(2); bar < L(3); bar++) {
        int said = (bar == L(2) + 1 || bar == L(2) + 5 || bar == L(2) + 13);
        if (said) continue;
        const Chord *c = chord_at(bar);
        double v[4] = { c->tones[0], c->tones[1], c->tones[2], c->sev };
        stab(at(bar) + sw(3) + lean(bar, 1.5), v, 4, 0.36, 0.36, 0.86, 0.34);
        stab(at(bar) + sw(7) + lean(bar, 3.5), v, 4, 0.32, 0.32, 0.86, -0.34);
        if (bar % 2 == 1)
            stab(at(bar) + sw(5.5) + lean(bar, 2.75), v, 4, 0.18, 0.26, 0.95, 0.0);
    }
    // PA — announced, then answered. @jeffrey: "can 'patiently' especially
    // 'pa' be cooler". The stutter runs three 16ths into it so the scoop is
    // heard coming; the throws are the same word an octave later, off the
    // beat, gone into the delay.
    word_stutter("w-whole-line", "pa", L(2), 0.46, 0.36);
    word_throw("w-whole-line", "pa", L(2), 0.58, 0.75 * BEAT, 0.38);
    word_throw("w-whole-line", "pa", L(2), 0.30, 2.25 * BEAT, -0.42);
    // the record's last word gets a low bell under it, and the tail rings
    fembell(at(L(3) - 1), -4, 3.2, 0.30, -0.2);

mixdown:
    if (missingN) fprintf(stderr, "  ! %d missing samples\n", missingN);

    // ── dub delay — dotted 8th, damped cross-feedback (v1's) ───────────
    {
        long D = lround(0.75 * BEAT * SR);
        double FB = 0.38;
        double damp = 1 - exp((-TAU * 2200) / SR);
        double hpRc = 1 / (TAU * 160), hpA = hpRc / (hpRc + 1.0 / SR);
        float *bL = calloc(N + D + 1, 4), *bR = calloc(N + D + 1, 4);
        double dL = 0, dR = 0, hpL = 0, hpR = 0, pL = 0, pR = 0;
        for (long i = 0; i < N; i++) {
            double tapL = i >= D ? bL[i - D] : 0;
            double tapR = i >= D ? bR[i - D] : 0;
            dL += damp * (tapR - dL);
            dR += damp * (tapL - dR);
            bL[i] = (float)(dlySend[i] + dR * FB);
            bR[i] = (float)(dL * FB);
            hpL = hpA * (hpL + bL[i] - pL); pL = bL[i];
            hpR = hpA * (hpR + bR[i] - pR); pR = bR[i];
            musicL[i] += (float)(hpL * 0.56);
            musicR[i] += (float)(hpR * 0.56);
        }
        free(bL); free(bR);
    }

    // ── the diffuse tail — decorrelated Schroeder pair (v3's) ──────────
    {
        static const double combMsL[4] = { 44.6, 47.9, 51.5, 54.2 };
        static const int combOffR[4] = { 23, 29, 31, 37 };
        double rt60 = 3.2, dampHz = 3400.0;
        double kDamp = 1 - exp((-TAU * dampHz) / SR);
        long pre = lround(0.040 * SR);
        float *retL = calloc(N, 4), *retR = calloc(N, 4);
        for (int side = 0; side < 2; side++) {
            float *ret = side ? retR : retL;
            for (int c = 0; c < 4; c++) {
                long D = lround(combMsL[c] * 0.001 * SR) + (side ? combOffR[c] : 0);
                double g = pow(10.0, -3.0 * (D / (double)SR) / rt60);
                float *buf = calloc(N + D + 1, 4);
                double lp = 0;
                for (long i = 0; i < N; i++) {
                    double in = (i >= pre) ? rvbSend[i - pre] : 0;
                    double tap = i >= D ? buf[i - D] : 0;
                    lp += kDamp * (tap - lp);
                    buf[i] = (float)(in + lp * g);
                    ret[i] += (float)(tap * 0.25);
                }
                free(buf);
            }
            // two series allpasses, g 0.7
            static const double apMs[2] = { 22.2, 9.0 };
            for (int a = 0; a < 2; a++) {
                long D = lround(apMs[a] * 0.001 * SR) + (side ? 5 : 0);
                float *buf = calloc(N + D + 1, 4);
                for (long i = 0; i < N; i++) {
                    double x = ret[i];
                    double tap = i >= D ? buf[i - D] : 0;
                    double v = x + 0.7 * tap;
                    buf[i] = (float)v;
                    ret[i] = (float)(tap - 0.7 * v);
                }
                free(buf);
            }
            // 180 Hz high-pass on the return
            double rc = 1 / (TAU * 180), ka = rc / (rc + 1.0 / SR);
            double h = 0, prev = 0;
            for (long i = 0; i < N; i++) {
                h = ka * (h + ret[i] - prev); prev = ret[i];
                ret[i] = (float)h;
            }
        }
        for (long i = 0; i < N; i++) { voxL[i] += retL[i] * 0.34f; voxR[i] += retR[i] * 0.34f; }
        free(retL); free(retR);
    }

    // ── the pump + the mix ─────────────────────────────────────────────
    // "side chained into the lyrics": every kick ducks the vox 0.34 and
    // the bed 0.52; drums never duck.
    // harsher sidechain — deeper on both buses and a faster grab, so
    // the pump is something you feel rather than infer
    // The STUDY's kick lands on all four, so the dance mix's 0.68 duck
    // holds the bed down almost continuously and the pads never surface
    // — @jeffrey: "also the pads arent sounding". A monitor wants to hear
    // what it is judging; the dance depth stays on the real cut.
    // @jeffrey: "and side chainnig!" — it was already here, doing its job
    // quietly. The point of asking for it out loud is that you want to
    // HEAR it, and what makes a pump audible is not depth, it is the
    // RELEASE: a duck that recovers in 260 ms is most of the way back
    // before the next kick and reads as a level change. Shortening it to
    // 190 ms and grabbing faster makes the bed breathe in and out between
    // every beat, which is the sound. Depth up a little too, but the
    // release is what you notice.
    float *envBed = duck_env(minimal ? 0.28 : 0.76, 0.005, 0.19);
    // …and the duck on the voice comes back to the 0.34 the comment above
    // always claimed. 0.52 is six dB of movement on the lead four times a
    // bar, which reads as pumping rather than as a floor once the 16ths
    // are gone and there is nothing else covering it.
    float *envVox = duck_env(0.38, 0.006, 0.20);
    // SOLO=music (or drums / vox, comma-separated) mutes the rest —
    // @jeffrey: "im not hearing those instruments · are they toggled
    // off?". A bus you cannot solo can only be argued about; this makes
    // the question answerable in one render.
    const char *soloEnv = getenv("SOLO");
    double gD = 1, gM = 1, gV = 1;
    if (soloEnv && *soloEnv) {
        gD = strstr(soloEnv, "drums") ? 1 : 0;
        gM = strstr(soloEnv, "music") ? 1 : 0;
        gV = strstr(soloEnv, "vox")   ? 1 : 0;
        printf("  SOLO=%s — drums %.0f · music %.0f · vox %.0f\n", soloEnv, gD, gM, gV);
    }
    // ── THE VOCAL CHAIN — @jeffrey: "we need to master / treat each
    // vocal separate right?". Right. Until now the vox bus got a static
    // gain and a sidechain duck and nothing else, and her line swings 23
    // dB across one sentence — "sitting" peaks at −6, "waiting" sits at
    // −29. A single fader cannot serve both: set it for the loud words
    // and the quiet ones fall under the floor; set it for the quiet ones
    // and she shouts. So the lead gets its own compressor, linked across
    // the pair so the image does not wander, ahead of the mix and ahead
    // of the master.
    {
        const double th = 0.10;      // ≈ −20 dBFS on this bus
        const double ratio = 3.0, mk = 2.1;
        const double aA = 1 - exp(-1.0 / (0.012 * SR));   // 12 ms
        const double aR = 1 - exp(-1.0 / (0.140 * SR));   // 140 ms
        // …and a scoop where PROXIMITY lives. A voice close to a mic is
        // thick between about 150 and 800 Hz; taking some of that out is
        // what makes it read as further away, and unlike a lowpass it
        // leaves the sibilant restore alone — the /s/ that took all day to
        // find stays exactly as loud as it was.
        const double kLo = 1 - exp((-TAU * 150.0) / SR);
        const double kHi = 1 - exp((-TAU * 800.0) / SR);
        // …further back again — @jeffrey, twice now: "i think the voice
        // should be deeper in the [m]ix". Each of the three levers moves
        // together, because only moving the fader makes a quiet close
        // voice rather than a distant one: 1.02 → 0.82 direct, 0.46 → 0.58
        // room, and the proximity scoop from a third to nearly half.
        const double chest = 0.44;
        double lo[2] = {0, 0}, hi[2] = {0, 0};
        double env = 0, gr = 1, worst = 1;
        for (long i = 0; i < N; i++) {
            double d = fmax(fabs((double)voxL[i]), fabs((double)voxR[i]));
            env += (d > env ? aA : aR) * (d - env);
            double want = env > th ? (th + (env - th) / ratio) / env : 1.0;
            gr += (want < gr ? aA : aR) * (want - gr);
            if (gr < worst) worst = gr;
            double l = voxL[i] * gr * mk, r = voxR[i] * gr * mk;
            lo[0] += kLo * (l - lo[0]); hi[0] += kHi * (l - hi[0]);
            lo[1] += kLo * (r - lo[1]); hi[1] += kHi * (r - hi[1]);
            voxL[i] = (float)(l - chest * (hi[0] - lo[0]));
            voxR[i] = (float)(r - chest * (hi[1] - lo[1]));
        }
        printf("  vox comp — max %.1f dB down\n", 20 * log10(worst));
    }
    {   // bus peaks, so a silent bus is visible rather than argued about
        double pd = 0, pm = 0, pv = 0;
        for (long i = 0; i < N; i++) {
            pd = fmax(pd, fabs((double)drumsL[i]));
            pm = fmax(pm, fabs((double)musicL[i]));
            pv = fmax(pv, fabs((double)voxL[i]));
        }
        printf("  bus peaks — drums %.3f · music %.3f · vox %.3f\n", pd, pm, pv);
    }
    float *L = calloc(N, 4), *R = calloc(N, 4);
    // band-limited antisymmetric side (one-pole at 6 kHz)
    double kSide = 1 - exp((-TAU * 6000) / SR);
    double sV = 0, sB = 0;
    for (long i = 0; i < N; i++) {
        sV += kSide * (sideV[i] - sV);
        sB += kSide * (sideB[i] - sB);
        double bed = envBed[i], vx = envVox[i] * VOXG;
        double l = drumsL[i] * gD + (musicL[i] + sB) * bed * gM + (voxL[i] + sV) * vx * gV;
        double r = drumsR[i] * gD + (musicR[i] - sB) * bed * gM + (voxR[i] - sV) * vx * gV;
        L[i] = (float)l; R[i] = (float)r;
    }
    double peak = 0;
    for (long i = 0; i < N; i++) {
        double v = fmax(fabs((double)L[i]), fabs((double)R[i]));
        if (v > peak) peak = v;
    }
    double trim = peak > 1e-9 ? 0.92 / peak : 1.0;
    printf("# pre-master peak %.6f · linear trim %.3f\n", peak, trim);
    for (long i = 0; i < N; i++) { L[i] = (float)(L[i] * trim); R[i] = (float)(R[i] * trim); }

    char outp[512];
    snprintf(outp, sizeof outp, "%s/out/%s", LANE,
             minimal ? "loner-kickvox-full.wav" : "loner-remix-v4-full.wav");
    write_wav_f32_stereo(outp, L, R, N);
    printf("✓ %s\n  %.1f s scored · master with: bash %s/c/cut-v4.sh\n",
           outp, N / (double)SR, LANE);
    return 0;
}
