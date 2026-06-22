// pianotrax.c — glasstrax, arranged for a REAL PIANO. Same song (A major, 124
// BPM, the airy I–IV–vi–V loop, the same rising-then-cresting LEAD motif, the
// same 8-section form) but two big changes: TIMBRE and PLAYABILITY.
//
// TIMBRE: a REAL SAMPLED GRAND — the exact same piano AC OS plays. The audio
// engine in fedac/native uses the Salamander Grand Piano V3 (CC0 by Alexander
// Holm), decimated to 26 anchor samples every 3 semitones (raw f32 mono 48 kHz,
// 3 s each) in fedac/native/samples/piano/<midi>.raw. We load that same bank and
// voice every note the same way audio.c does: pick the nearest anchor, pitch-
// shift it by resampling (step = 2^((note−anchor)/12)) with linear interpolation,
// and apply an amp + damper-release envelope. So pianotrax sounds like the OS.
//
// PLAYABILITY: glasstrax's unplayable layers (random droplet mist, 8-note
// 3-octave shimmer sweeps, the busy independent 16th arp bed) are gone. In
// their place: a left-hand bass + broken-chord/Alberti accompaniment and a
// right-hand melody (the LEAD) with tasteful pentatonic eighths, rolled chords
// and grace notes. ≤ ~10 notes sounding at once, reaches ≤ an octave per hand.
// A natural room reverb (drier than glasstrax's crystalline hall).
//
// Samples are read from fedac/native/samples/piano (override with --piano-dir
// or $AC_PIANO_DIR). render-c.mjs --engine pianotrax passes the path and
// masters on the sine-family chain.
//
// Build:  cc -O3 -std=c11 -o pianotrax pianotrax.c -lm
// Run:    ./pianotrax --out out/pianotrax-raw.wav --piano-dir fedac/native/samples/piano

#define _POSIX_C_SOURCE 200809L
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <math.h>

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif
#define TAU (2.0 * M_PI)

static const int SR = 48000;
static double BPMV = 124, BEAT, BAR, SX;
static double PIANO_GAIN = 1.35;     // overall press level (extra volume, vs the beat)
static double PIANO_RELEASE = 0.42;  // damper release in seconds (sustain / pedal bloom)

// ── swing / sway ────────────────────────────────────────────────────────────
// A gentle pianist's swing (0.56) — the "&"s lean just slightly, the way a
// player phrases broken-chord eighths. 16ths interpolate inside the swung 8th.
static double SWING = 0.56;
static double swing_beats(double beats) {
    double e8 = beats * 2.0;                 // position in 8ths
    long idx = (long)floor(e8 + 1e-9);
    double frac = e8 - idx;
    double pos = (idx / 2) * BEAT + ((idx & 1) ? SWING * BEAT : 0.0);
    return pos + frac * (BEAT / 2.0);        // interpolate 16ths inside the 8th
}

static long N;
static float *busL, *busR, *revL, *revR;
static inline void adds(float *L, float *R, long i, double l, double r) { if (i >= 0 && i < N) { L[i] += (float)l; R[i] += (float)r; } }

static int write_wav_f32_stereo(const char *path, const float *L, const float *R, long n) {
    FILE *f = fopen(path, "wb"); if (!f) return 0;
    uint32_t dsz = (uint32_t)(n * 8), riff = 36 + dsz, sr = SR, br = SR * 8, fsz = 16;
    uint16_t fmt = 3, ch = 2, ba = 8, bits = 32;
    fwrite("RIFF", 1, 4, f); fwrite(&riff, 4, 1, f); fwrite("WAVE", 1, 4, f);
    fwrite("fmt ", 1, 4, f); fwrite(&fsz, 4, 1, f);
    fwrite(&fmt, 2, 1, f); fwrite(&ch, 2, 1, f); fwrite(&sr, 4, 1, f);
    fwrite(&br, 4, 1, f); fwrite(&ba, 2, 1, f); fwrite(&bits, 2, 1, f);
    fwrite("data", 1, 4, f); fwrite(&dsz, 4, 1, f);
    for (long i = 0; i < n; i++) { fwrite(&L[i], 4, 1, f); fwrite(&R[i], 4, 1, f); }
    fclose(f); return 1;
}

// ── Salamander Grand Piano sample bank (the AC OS piano) ──────────────────────
// Anchor samples are raw f32 mono 48 kHz, named "<midi>.raw" (MIDI 21..96 every
// 3 semitones). Same on-disk format + filename convention as fedac/native
// /samples/piano, loaded the same way audio.c's load_piano_bank() does.
#define PIANO_ANCHOR_MAX 64
static float *pn_data[PIANO_ANCHOR_MAX];
static long   pn_len[PIANO_ANCHOR_MAX];
static int    pn_midi[PIANO_ANCHOR_MAX];
static int    pn_count = 0;
static const char *PIANO_DIR = NULL;            // set from --piano-dir / $AC_PIANO_DIR

// Probe candidate dirs (CLI/env first, then paths relative to repo root and to
// pop/maytrax/c) by looking for the middle-C anchor 60.raw. Returns NULL if none.
static const char *find_piano_dir(void) {
    const char *cands[] = {
        PIANO_DIR, getenv("AC_PIANO_DIR"),
        "fedac/native/samples/piano",
        "../../../fedac/native/samples/piano",
        "../../fedac/native/samples/piano",
    };
    for (int i = 0; i < (int)(sizeof cands / sizeof *cands); i++) {
        if (!cands[i]) continue;
        char probe[1024]; snprintf(probe, sizeof probe, "%s/60.raw", cands[i]);
        FILE *f = fopen(probe, "rb");
        if (f) { fclose(f); return cands[i]; }
    }
    return NULL;
}

// Load the 26 anchors (MIDI 21,24,…,96) from `dir` into the bank.
static int load_piano_bank(const char *dir) {
    pn_count = 0;
    for (int m = 21; m <= 96 && pn_count < PIANO_ANCHOR_MAX; m += 3) {
        char path[1024]; snprintf(path, sizeof path, "%s/%d.raw", dir, m);
        FILE *f = fopen(path, "rb"); if (!f) continue;
        fseek(f, 0, SEEK_END); long sz = ftell(f); rewind(f);
        if (sz <= 0 || sz % (long)sizeof(float) != 0) { fclose(f); continue; }
        long cnt = sz / (long)sizeof(float);
        float *b = malloc((size_t)sz);
        if (!b) { fclose(f); continue; }
        if (fread(b, 1, (size_t)sz, f) != (size_t)sz) { free(b); fclose(f); continue; }
        fclose(f);
        pn_data[pn_count] = b; pn_len[pn_count] = cnt; pn_midi[pn_count] = m; pn_count++;
    }
    return pn_count;
}

// Nearest anchor to a target MIDI note (same as audio.c's pick_piano_anchor).
static int pick_anchor(double note) {
    int best = 0; double bd = 1e18;
    for (int i = 0; i < pn_count; i++) { double d = fabs(note - pn_midi[i]); if (d < bd) { bd = d; best = i; } }
    return best;
}

// ── the piano voice — sampled grand ───────────────────────────────────────────
// Pick the nearest Salamander anchor, pitch-shift it by reading at a fractional
// step (2^((note−anchor)/12)) with linear interpolation, and shape it with a
// tiny click-suppressing attack + a damper RELEASE after `dur` (so staccato
// notes damp and held/pedalled notes ring out their natural 3 s decay). The
// keyboard pan (low→left, high→right) is blended with the caller's pan, exactly
// as the synth voice did.
//
//  note  MIDI pitch        dur   sounding length (sec) before the damper falls
//  g     velocity/level    pan   stereo placement      vel  mild loudness tilt
static void piano(float *L, float *R, double note, double t, double dur, double g, double pan, double vel) {
    if (pn_count == 0 || dur <= 0) return;
    int a = pick_anchor(note);
    const float *src = pn_data[a]; long slen = pn_len[a];
    double step = pow(2.0, (note - (double)pn_midi[a]) / 12.0);     // resample ratio

    double kbpan = (note - 60.0) / 40.0; if (kbpan > 1) kbpan = 1; if (kbpan < -1) kbpan = -1;
    double P = pan * 0.5 + kbpan * 0.35;
    double lg = 1 - P * 0.5, rg = 1 + P * 0.5;
    // wider velocity → loudness map: soft notes sit back, accents punch — good
    // dynamic range, and a hotter overall level so the presses carry.
    double amp = g * (0.70 + 0.85 * vel) * PIANO_GAIN;

    long s0 = (long)(t * SR);
    long att = (long)(0.0015 * SR); if (att < 1) att = 1;          // click-suppress onset
    long noteN = (long)(dur * SR);                                 // key held this long
    // SUSTAIN: a long damper release so presses ring and bloom into each other
    // (the pedal-down, hazy wash the track wants) instead of clipping dry at dur.
    long relN = (long)(PIANO_RELEASE * SR); if (relN < 1) relN = 1;

    double pos = 0.0;
    for (long i = 0; ; i++) {
        long idx = (long)pos;
        if (idx >= slen - 1) break;                                // sample exhausted (natural end)
        double frac = pos - (double)idx;
        double s = (1.0 - frac) * (double)src[idx] + frac * (double)src[idx + 1];
        double env = 1.0;
        if (i < att) env = (double)i / att;
        if (i >= noteN) { long r = i - noteN; if (r >= relN) break; env *= 1.0 - (double)r / relN; }
        double v = s * amp * env;
        adds(L, R, s0 + i, v * lg, v * rg);
        pos += step;
    }
}

// ── humanize — a hazy, lazy hand ──────────────────────────────────────────────
// A real pianist drags a hair behind the beat and never strikes two keys at the
// same velocity. LAZY pushes every press slightly LATE (behind the dead-on-grid
// kick → the laid-back feel); HUM_MS jitters the timing; HUM_VEL wobbles the
// loudness. Chord notes humanize independently → a natural micro-roll/spread.
// Deterministic xorshift so renders stay identical run-to-run.
static double LAZY = 0.016;        // seconds behind the beat (the drag)
static double HUM_MS = 12.0;       // ± timing jitter (ms)
static double HUM_VEL = 0.17;      // ± velocity wobble (fraction)
static uint32_t hrs = 0x515A4953;  // humanize rng state
static inline double hr2(void) { hrs ^= hrs << 13; hrs ^= hrs >> 17; hrs ^= hrs << 5; return ((double)hrs / 4294967296.0) * 2.0 - 1.0; }

// piano note that also feeds a little into the room reverb (for sustained / lead
// notes we want a touch of tail). `wet` is the reverb-send level. The hazy-lazy
// humanize is applied HERE so every press inherits it (the kick/scratch grid
// stays tight, so the piano drags against it).
static void pkey(double note, double t, double dur, double g, double pan, double vel, double wet) {
    double tt = t + LAZY + hr2() * HUM_MS / 1000.0; if (tt < 0) tt = 0;
    double gg = g * (1.0 + hr2() * HUM_VEL); if (gg < 0) gg = 0;
    piano(busL, busR, note, tt, dur, gg, pan, vel);
    if (wet > 0) piano(revL, revR, note, tt, dur, gg * wet, 0, vel);
}

// a rolled (arpeggiated) chord — notes struck in quick succession bottom-up,
// the way a pianist rolls a wide voicing the hand can't grab flat. ~18 ms apart.
static void roll(const int *midis, int nm, double t, double g, double vel, double wet) {
    for (int k = 0; k < nm; k++) pkey(midis[k], t + 0.018 * k, BEAT * 3.0, g * (0.92 + 0.08 * k / nm), 0.0, vel, wet);
}

// a block chord — struck together (a small hand-grab, ≤ an octave span).
static void block(const int *midis, int nm, double t, double dur, double g, double vel, double wet) {
    for (int k = 0; k < nm; k++) pkey(midis[k], t + 0.002 * k, dur, g, 0.0, vel, wet);
}

// ── harmony — A major, the airy I–IV–vi–V loop (same as glasstrax) ────────────
// per-bar chords A, D, F#m, E — the same loop glasstrax walks (its A_ROOTS were
// A D F# E); the left-hand chord-tone sets below carry those roots in the bass.
// Left-hand chord tones (root-position triads, low register). The LH plays
// broken-chord / Alberti patterns over these — root, 5th, octave, 3rd — all
// within an octave so one hand reaches them comfortably.
//                                root  5th  oct  3rd
static const int LH_A[4]  = { 45, 52, 57, 49 };        // A2 E3 A3 C#3
static const int LH_D[4]  = { 38, 45, 50, 42 };        // D2 A2 D3 F#2
static const int LH_FM[4] = { 42, 49, 54, 45 };        // F#2 C#3 F#3 A2
static const int LH_E[4]  = { 40, 47, 52, 44 };        // E2 B2 E3 G#2
static const int *LH[4] = { LH_A, LH_D, LH_FM, LH_E };

// Right-hand block voicings (close, ≤ an octave, mid register) for sustained
// chords / hush passages — the kind a relaxed right hand holds.
static const int RH_A[3]  = { 61, 64, 69 };            // C#4 E4 A4
static const int RH_D[3]  = { 57, 62, 66 };            // A3 D4 F#4
static const int RH_FM[3] = { 57, 61, 66 };            // A3 C#4 F#4
static const int RH_E[3]  = { 56, 59, 64 };            // G#3 B3 E4
static const int *RH[4] = { RH_A, RH_D, RH_FM, RH_E };

// A major pentatonic (A B C# E F#) for right-hand runs/ornaments — pianistic,
// the hand stays in one position.
static const int PENT[5] = { 69, 71, 73, 76, 78 };     // A4 B4 C#5 E5 F#5

// section map — IDENTICAL to glasstrax so the form/length match exactly. At 124
// BPM a bar is ~1.935s, so this 56-bar form lands ~114s.
static const char *ORDER[8] = { "intro", "bloom", "waveA", "hushA", "waveB", "hushB", "waveC", "outro" };
static const int SECBARS[8] = { 4, 4, 8, 4, 8, 4, 12, 12 };
static int START[8];
static int sec_index(const char *k) { for (int i = 0; i < 8; i++) if (!strcmp(ORDER[i], k)) return i; return 0; }

// ── left-hand patterns ────────────────────────────────────────────────────────
// Alberti-ish broken chord across one bar: low–high–mid–high in eighths, the
// classic supporting figure a left hand runs without thinking. Uses the 4 LH
// chord tones; pattern indices keep the bass on the down-beats.
static void lh_broken(double t, const int *ch, double g, double vel) {
    // eighths over a 4/4 bar (8 hits): root, 5th, oct, 5th | 3rd, 5th, oct, 5th
    static const int pat[8] = { 0, 1, 2, 1, 3, 1, 2, 1 };
    for (int e = 0; e < 8; e++) {
        double tt = t + swing_beats(e * 0.5);
        double gg = (e % 2 == 0) ? g : g * 0.7;          // on-beats fuller
        pkey(ch[pat[e]], tt, BEAT * 0.85, gg, -0.10, vel * (e % 2 == 0 ? 1.0 : 0.8), 0.12);
    }
}

// simpler half-time LH for spacious sections: a low root on 1, an open 5th/oct
// on beat 3 — lets the bar breathe.
static void lh_half(double t, const int *ch, double g, double vel) {
    pkey(ch[0], t, BEAT * 2.0, g, -0.12, vel, 0.18);
    int two[2] = { ch[1], ch[2] };
    block(two, 2, t + BEAT * 2, BEAT * 2.0, g * 0.85, vel * 0.85, 0.18);
}

// ── turntable scratch (synthesized; ported from pop/marimba/synths/perc.mjs) ──
// No vinyl sampled — the record is synthetic. Build a buzzy vowel source (saw
// harmonics + grit through a resonant formant), then "scrub" it: read it back at
// a position that oscillates with the DJ's hand. The read velocity is depth·sin
// (2π·rate·t) — on the back-swing the tone plays in REVERSE; that bend is the
// scratch. A crossfader gate (gateHz>0) chops it for transform/chirp styles.
//   rate ~5 baby · ~14 scribble   depth = throw → pitch range   gateHz = chop
static void scratch(double t, double dur, double smidi, double rate, double depth,
                    double gateHz, double gateDuty, double tone, double g, double pan, double wet) {
    long s0 = (long)(t * SR), ns = (long)(dur * SR);
    if (ns <= 0) return;
    long srcLen = (long)(0.4 * SR);
    float *src = malloc((size_t)srcLen * sizeof(float));
    if (!src) return;
    double f0 = 440.0 * pow(2.0, (smidi - 69.0) / 12.0);
    int nh = (int)(8 + tone * 22); if (nh < 4) nh = 4;
    double bp = 0, lp = 0, fc = 900.0, k = 2.0 * sin(M_PI * fc / SR), damp = 0.22;
    uint32_t r = (uint32_t)(s0 * 2654435761u) | 1u;        // per-event noise
    for (long i = 0; i < srcLen; i++) {
        double tt = (double)i / SR, s = 0;
        for (int h = 1; h <= nh; h++) s += sin(TAU * f0 * h * tt) / h;
        r ^= r << 13; r ^= r >> 17; r ^= r << 5;
        s = s * 0.5 + (((double)r / 4294967296.0) * 2.0 - 1.0) * 0.18 * tone;
        double hi = s - lp - damp * bp; bp += k * hi; lp += k * bp;  // resonant formant
        src[i] = (float)(s * 0.5 + bp * 0.9);
    }
    double sp = 0; for (long i = 0; i < srcLen; i++) { double a = fabs(src[i]); if (a > sp) sp = a; }
    if (sp > 0) { double n = 0.9 / sp; for (long i = 0; i < srcLen; i++) src[i] *= (float)n; }

    double center = srcLen * 0.5, amp = depth * SR / (TAU * rate);
    long att = (long)(0.004 * SR), rel = (long)(0.02 * SR);
    double lgp = 1 - pan * 0.5, rgp = 1 + pan * 0.5, gSmooth = 1.0;
    for (long i = 0; i < ns; i++) {
        double tt = (double)i / SR;
        double pos = center - amp * cos(TAU * rate * tt);
        if (pos < 0) pos = 0; else if (pos >= srcLen - 1) pos = srcLen - 2;
        long i0 = (long)pos; double fr = pos - i0;
        double smp = (1.0 - fr) * src[i0] + fr * src[i0 + 1];
        if (gateHz > 0) {                                  // crossfader chop (~1 ms smoothed)
            double ph = fmod(tt * gateHz, 1.0);
            gSmooth += ((ph < gateDuty ? 1.0 : 0.0) - gSmooth) * 0.02;
            smp *= gSmooth;
        }
        double env = 1.0;
        if (i < att) env = (double)i / att;
        else if (i > ns - rel) env = fmax(0.0, (double)(ns - i) / rel);
        double v = smp * env * g;
        adds(busL, busR, s0 + i, v * lgp, v * rgp);
        if (wet > 0) adds(revL, revR, s0 + i, v * wet, v * wet);
    }
    free(src);
}

// ── kick drum (synthesized; ported from pop/marimba/synths/perc.mjs) ──────────
// Pitch-enveloped sine body (click pitch → sub fundamental) driven through a
// tanh for punch, plus a short broadband beater click. Centered + dry. Sits
// dead on the swung grid so the hazy-lazy piano drags behind it.
static void kick(double t, double g) {
    long s0 = (long)(t * SR);
    double fStart = 150, fEnd = 48, pitchT = 0.055, ampT60 = 0.34, drive = 1.6, click = 0.55;
    double ampA = log(1000.0) / ampT60, nrm = tanh(drive);
    long tail = (long)((ampT60 * 1.3 + 0.02) * SR);
    uint32_t r = (uint32_t)(s0 * 2246822519u) | 1u;
    double phase = 0;
    for (long i = 0; i < tail; i++) {
        double tt = (double)i / SR;
        double f = fEnd + (fStart - fEnd) * exp(-tt / pitchT);
        phase += f / SR;
        double body = tanh(sin(TAU * phase) * drive) / nrm;
        double s = body * exp(-ampA * tt);
        if (tt < 0.006) { r ^= r << 13; r ^= r >> 17; r ^= r << 5; s += (((double)r / 4294967296.0) * 2.0 - 1.0) * click * exp(-tt / 0.0012); }
        adds(busL, busR, s0 + i, s * g, s * g);
    }
}

int main(int argc, char **argv) {
    const char *out_path = "out/pianotrax-raw.wav";
    for (int i = 1; i < argc; i++) { if (!strcmp(argv[i], "--out") && i + 1 < argc) out_path = argv[++i]; else if (!strcmp(argv[i], "--bpm") && i + 1 < argc) BPMV = atof(argv[++i]); else if (!strcmp(argv[i], "--swing") && i + 1 < argc) SWING = atof(argv[++i]); else if (!strcmp(argv[i], "--piano-dir") && i + 1 < argc) PIANO_DIR = argv[++i]; }
    BEAT = 60.0 / BPMV; BAR = BEAT * 4; SX = BEAT / 4;

    // load the Salamander grand bank — the same samples AC OS plays.
    const char *pdir = find_piano_dir();
    if (!pdir || load_piano_bank(pdir) == 0) {
        fprintf(stderr, "✗ no piano samples found (looked for <dir>/60.raw; set --piano-dir or $AC_PIANO_DIR). "
                        "Expected fedac/native/samples/piano\n");
        return 1;
    }
    fprintf(stderr, "# piano bank · %d Salamander anchors from %s\n", pn_count, pdir);
    { int c = 0; for (int i = 0; i < 8; i++) { START[i] = c; c += SECBARS[i]; } }
    int TB = 0; for (int i = 0; i < 8; i++) TB += SECBARS[i];
    double totalSec = TB * BAR + 6.0; N = (long)(totalSec * SR);
    busL = calloc(N, 4); busR = calloc(N, 4); revL = calloc(N, 4); revR = calloc(N, 4);
    fprintf(stderr, "# pianotrax.c · %g BPM · %d bars · %.1fs · A major solo piano (glasstrax arranged)\n", BPMV, TB, totalSec);

    // the lead, as {phrase-bar, beat, note} — the SAME bright rising-then-
    // cresting glasstrax motif, dropped an octave into a comfortable right-hand
    // range (originals were 76..90 = E5..F#6; here ~ A4..F#5, where the hand
    // lives and the melody sings rather than tinkles).
    double LEAD[8][3] = { {0,0,69},{0,2.5,73},{1,0,76},{1,1.5,78},{2,0,81},{2,1.5,78},{3,0,76},{3,2,83} };

    // ── INTRO — a single line: sparse RH chord glints over a held LH root ──────
    { int idx = sec_index("intro"); int c = START[idx], nb = SECBARS[idx], h = nb / 2; double t0 = c * BAR;
        // held low roots, hand resting, pedal down — long natural ring.
        pkey(45, t0, BAR * h - 0.05, 0.20, -0.15, 0.35, 0.22);              // A2
        pkey(50, t0 + BAR * h, BAR * (nb - h) - 0.05, 0.20, -0.15, 0.35, 0.22); // D2→hand to IV
        // a few descending RH glints (a player feeling out the melody), from bar 2.
        int mel[4] = { 81, 78, 76, 73 };                                    // A5 F#5 E5 C#5
        for (int b = 0; b < 4 && 2 + b < nb; b++) pkey(mel[b], t0 + (2 + b) * BAR, BAR * 0.9, 0.16, 0.20, 0.45, 0.28);
        // one soft rolled A chord at the top of the back half (a breath in).
        roll(RH_A, 3, t0 + BAR * h + BEAT * 2, 0.12, 0.40, 0.25);
    }

    // ── BLOOM — LH broken chords arrive; RH answers with a gentle pentatonic run ──
    { int idx = sec_index("bloom"); int c = START[idx], nb = SECBARS[idx]; double t0 = c * BAR;
        // chord per bar follows vi, vi, V, V (the glasstrax bloom climb).
        const int *prog[4] = { LH_FM, LH_FM, LH_E, LH_E };
        for (int b = 0; b < nb; b++) {
            lh_broken(t0 + b * BAR, prog[b], 0.16, 0.42);
            // RH: a 4-note ascending pentatonic eighth-figure in the back half of
            // each bar — a hand-shaped flourish, not a sweep.
            if (b >= 1) for (int s = 0; s < 4; s++) {
                double tt = t0 + b * BAR + swing_beats(2.0 + s * 0.5);
                pkey(PENT[(s + b) % 5], tt, BEAT * 0.6, 0.10 + 0.012 * b, 0.25, 0.5, 0.28);
            }
        }
        // a rising 3-note grace pickup into wave A (RH).
        for (int s = 0; s < 3; s++) pkey(74 + s * 2, t0 + BAR * nb - BEAT * 0.9 + 0.07 * s, BEAT * 0.5, 0.10, 0.3, 0.5, 0.3);
    }

    // ── the three waves — the body. LH broken-chord accompaniment + RH melody. ──
    const char *waves[3] = { "waveA", "waveB", "waveC" };
    for (int d = 0; d < 3; d++) {
        int wi = sec_index(waves[d]); int c = START[wi], nbw = SECBARS[wi]; int big = (d == 2);
        for (int b = 0; b < nbw; b++) {
            double t0 = (c + b) * BAR; int ci = b % 4; int phrase = (ci == 0);
            const int *lhc = LH[ci];

            // LEFT HAND — broken-chord accompaniment running the bar. In the big
            // wave it's fuller; elsewhere a touch lighter.
            lh_broken(t0, lhc, big ? 0.165 : 0.14, big ? 0.46 : 0.40);

            // a sustained RH chord pad on each phrase downbeat (held ~2 bars),
            // rolled so it sounds played, not punched.
            if (phrase) roll(RH[ci], 3, t0, big ? 0.085 : 0.07, 0.42, 0.30);

            // RIGHT HAND — the LEAD melody (4-bar phrase), singing, with a single
            // grace-note before high notes (idiomatic, one finger) instead of the
            // glasstrax frill sweep.
            for (int z = 0; z < 8; z++) if ((int)LEAD[z][0] == ci) {
                double note = LEAD[z][2], tt = t0 + swing_beats(LEAD[z][1]);
                if (note >= 78) pkey(note - 2, tt - 0.05, BEAT * 0.25, big ? 0.07 : 0.055, 0.25, 0.55, 0.2); // grace
                pkey(note, tt, BEAT * 1.3, big ? 0.135 : 0.11, 0.22, big ? 0.7 : 0.6, 0.34);
            }

            // a light RH connecting figure between melody notes — two pentatonic
            // eighths mid-bar (the "ornaments" energy that replaces the 16th bed),
            // only when the hand is free (not on a phrase-busy big-wave crest).
            if (!(big && ci == 3)) for (int s = 0; s < 2; s++) {
                double tt = t0 + swing_beats(2.5 + s * 0.5);
                int p = PENT[(s + b + 2) % 5];
                pkey(p, tt, BEAT * 0.45, 0.06 + 0.01 * (s), 0.28, 0.45, 0.3);
            }

            // crest of each big-wave phrase end: a rolled high A-add chord (both
            // hands meet) — the recognizable glasstrax "crest", made playable.
            if (big && ci == 3) { int crest[3] = { 76, 81, 85 }; roll(crest, 3, t0 + BEAT * 2, 0.09, 0.7, 0.34); }
        }
    }

    // ── HUSHES — spacious: half-time LH + a held RH chord + a lone melody line ──
    const char *hush[2] = { "hushA", "hushB" };
    for (int z2 = 0; z2 < 2; z2++) {
        int hi = sec_index(hush[z2]); int c = START[hi], nb = SECBARS[hi], h = nb / 2; double t0 = c * BAR;
        // IV then V, the glasstrax hush harmony.
        for (int b = 0; b < nb; b++) {
            const int *lhc = (b < h) ? LH_D : LH_E;
            lh_half(t0 + b * BAR, lhc, 0.15, 0.36);
            // a sparse high RH note on alternating bars — the melody catching its breath.
            if (b % 2 == 0) {
                int rhn = (b < h) ? RH_D[2] : RH_E[2];
                pkey(rhn, t0 + b * BAR + BEAT * 1, BAR * 0.8, 0.10, 0.25, 0.45, 0.34);
            }
        }
        // a held RH chord across the back half (pedal ringing).
        roll((z2 == 0) ? RH_D : RH_E, 3, t0 + BAR * h, 0.075, 0.4, 0.36);
        // a short rising pentatonic pickup into the next wave (RH), z2==1 stronger.
        if (z2 == 1) for (int s = 0; s < 4; s++) pkey(PENT[s % 5] + (s / 5) * 12, t0 + BAR * (nb - 1) + swing_beats(s * 0.5), BEAT * 0.5, 0.085, 0.3, 0.5, 0.32);
    }

    // ── OUTRO — thinning LH, a last statement of the melody, a final rolled chord ──
    { int idx = sec_index("outro"); int c = START[idx], nb = SECBARS[idx], h = nb / 2; double t0 = c * BAR;
        for (int b = 0; b < nb; b++) {
            double e = fmax(0.12, 1.0 - (double)b / nb);          // gentle decrescendo
            const int *lhc = (b < h) ? LH_A : LH_E;
            if (b < nb - 3) lh_half(t0 + b * BAR, lhc, 0.14 * e, 0.34); // LH fades out
            // a soft RH chord glint every other bar, also fading.
            if (b % 2 == 0 && b < nb - 3) roll((b < h) ? RH_A : RH_E, 3, t0 + b * BAR + BEAT * 2, 0.065 * e, 0.38, 0.36);
        }
        // a final, long, rolled A-major chord (both hands) ringing into the fade —
        // low A, A, C#, E, A: the home chord, pedal held.
        int fin[5] = { 33, 45, 61, 64, 69 };                       // A1 A2 C#4 E4 A4
        for (int k = 0; k < 5; k++) pkey(fin[k], t0 + BAR * (nb - 3) + 0.03 * k, 5.5, 0.16 * (k == 0 ? 0.8 : 1.0), 0, 0.5, 0.4);
    }

    // ── BEAT LAYER — kick + rhythmic scratches, through the three waves only ──
    // The groove drops in for the waves and pulls out for intro / bloom / hushes
    // / outro — that contrast IS the dynamic range. Kick + scratches sit dead on
    // the swung grid; the humanized piano drags lazily behind them. Scratches are
    // rhythmic and timely (a stab on the &-of-4 every bar, chirps on the &-of-2),
    // not just one-shots at the seams.
    {
        const char *wv[3] = { "waveA", "waveB", "waveC" };
        for (int d = 0; d < 3; d++) {
            int wi = sec_index(wv[d]); int c = START[wi], nb = SECBARS[wi]; int big = (d == 2);
            scratch(c * BAR, 0.44, 50, big ? 7 : 5, big ? 3.0 : 2.2, big ? 7 : 12, 0.55, 0.58, big ? 0.14 : 0.12, 0.05, 0.18); // drop-in
            for (int b = 0; b < nb; b++) {
                double t0 = (c + b) * BAR;
                // kick — laid-back boom-bap: strong 1, syncopated &-of-2 (+ beat 3 in the big wave)
                kick(t0 + swing_beats(0),   0.18);
                kick(t0 + swing_beats(2.5), 0.13);
                if (big && b % 2 == 0) kick(t0 + swing_beats(3), 0.10);
                // rhythmic scratch on the &-of-4 every bar (baby ↔ scribble), chirp on &-of-2
                double sm = 47 + (b % 4) * 3;
                scratch(t0 + swing_beats(3.5), 0.28, sm, (b % 2) ? 14 : 5.5, (b % 2) ? 1.6 : 2.0, 0, 0, 0.5, big ? 0.12 : 0.10, 0.22, 0.10);
                if (b % 2 == 1) scratch(t0 + swing_beats(1.5), 0.24, 52, 7, 3.0, 7, 0.55, 0.55, 0.10, -0.18, 0.10);
                if (big)        scratch(t0 + swing_beats(0.5), 0.22, 50, 14, 1.5, 0, 0, 0.6, 0.09, 0.26, 0.08); // extra scribble
            }
        }
    }

    // ── REVERB (Schroeder) — a NATURAL ROOM, drier & warmer than glasstrax's hall.
    // Lower wet, more damping (darker tail) — a piano in a room, not a glass cave.
    {
        double decay = 0.80, wet = 0.30, damp = 0.45;
        int CD[6]; double cds[6] = { 0.0297, 0.0371, 0.0411, 0.0437, 0.0497, 0.0581 };
        for (int k = 0; k < 6; k++) CD[k] = (int)(cds[k] * SR);
        int AD[2] = { (int)(0.005 * SR), (int)(0.0017 * SR) }; double apFb = 0.5;
        float *cbL[6], *cbR[6]; int ciL[6] = {0}, ciR[6] = {0}; double cLPL[6] = {0}, cLPR[6] = {0};
        for (int k = 0; k < 6; k++) { cbL[k] = calloc(CD[k], 4); cbR[k] = calloc(CD[k], 4); }
        float *abL[2], *abR[2]; int aiL[2] = {0}, aiR[2] = {0};
        for (int k = 0; k < 2; k++) { abL[k] = calloc(AD[k], 4); abR[k] = calloc(AD[k], 4); }
        for (long i = 0; i < N; i++) {
            double inL = revL[i], inR = revR[i], cL = 0, cR = 0;
            for (int k = 0; k < 6; k++) {
                double dL = cbL[k][ciL[k]], dR = cbR[k][ciR[k]]; cL += dL; cR += dR;
                cLPL[k] = dL * (1 - damp) + cLPL[k] * damp; cLPR[k] = dR * (1 - damp) + cLPR[k] * damp;
                cbL[k][ciL[k]] = (float)(inL + cLPL[k] * decay); cbR[k][ciR[k]] = (float)(inR + cLPR[k] * decay);
                ciL[k] = (ciL[k] + 1) % CD[k]; ciR[k] = (ciR[k] + 1) % CD[k];
            }
            cL /= 6; cR /= 6;
            for (int k = 0; k < 2; k++) {
                double dL = abL[k][aiL[k]], dR = abR[k][aiR[k]];
                double oL = -apFb * cL + dL, oR = -apFb * cR + dR;
                abL[k][aiL[k]] = (float)(cL + apFb * oL); abR[k][aiR[k]] = (float)(cR + apFb * oR);
                aiL[k] = (aiL[k] + 1) % AD[k]; aiR[k] = (aiR[k] + 1) % AD[k];
                cL = oL; cR = oR;
            }
            busL[i] += (float)(cL * wet); busR[i] += (float)(cR * wet);
        }
        for (int k = 0; k < 6; k++) { free(cbL[k]); free(cbR[k]); }
        for (int k = 0; k < 2; k++) { free(abL[k]); free(abR[k]); }
    }

    // ── normalize + gentle in/out fades ──
    double peak = 0; for (long i = 0; i < N; i++) { double a = fmax(fabs(busL[i]), fabs(busR[i])); if (a > peak) peak = a; }
    fprintf(stderr, "# pre-norm peak %.3f\n", peak);
    if (peak > 0) { double g = 0.85 / peak; for (long i = 0; i < N; i++) { busL[i] *= (float)g; busR[i] *= (float)g; } }
    long fin = (long)(1.5 * SR), fout = (long)(4.5 * SR);
    for (long i = 0; i < fin && i < N; i++) { double g = 0.5 - 0.5 * cos(M_PI * (double)i / fin); busL[i] *= (float)g; busR[i] *= (float)g; }
    for (long i = 0; i < fout && i < N; i++) { double g = 0.5 - 0.5 * cos(M_PI * (double)i / fout); long idx = N - 1 - i; busL[idx] *= (float)g; busR[idx] *= (float)g; }

    if (!write_wav_f32_stereo(out_path, busL, busR, N)) { fprintf(stderr, "✗ write failed\n"); return 1; }
    fprintf(stderr, "✓ %s · %.1fs\n", out_path, (double)N / SR);
    return 0;
}
