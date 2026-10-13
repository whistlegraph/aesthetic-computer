// compare.c — offline loudness comparison of Menu Band's two engines.
//
// Reproduces the app's normalization exactly (one 350 ms note per program at
// measure_key, velocity 100, RMS after 30 ms, gains clamped 0.25–4 toward the
// SoundFont median), then plays every program in both engines across keys
// C2–C6 and velocities 64/100/127 and measures BS.1770 K-weighted loudness
// (LUFS-style) over the first 500 ms. Writes compare.csv and a listening WAV
// (C4 phrase per program: SoundFont, then AC).
#define TSF_IMPLEMENTATION
#include "tsf.h"
#include "gm_synth.h"
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

#define SR 48000.0
static const double MASTER = 0.5;   // menuband.c g_masterGain

static int measure_key(int p) {
    if ((p >= 32 && p <= 39) || p == 43 || p == 58 || p == 70 || p == 87) return 48;
    if (p == 72 || p == 9 || p == 10 || p == 112) return 72;
    return 60;
}
static double mfreq(int m) { return 440.0 * pow(2.0, (m - 69) / 12.0); }

// BS.1770 K-weighting at 48 kHz, applied to a mono or stereo buffer.
typedef struct { double x1, x2, y1, y2; } BQ;
static double bq(BQ *s, const double *b, const double *a, double x) {
    double y = b[0] * x + b[1] * s->x1 + b[2] * s->x2 - a[1] * s->y1 - a[2] * s->y2;
    s->x2 = s->x1; s->x1 = x; s->y2 = s->y1; s->y1 = y; return y;
}
static const double KB1[3] = { 1.53512485958697, -2.69169618940638, 1.19839281085285 }, KA1[3] = { 1, -1.69065929318241, 0.73248077421585 };
static const double KB2[3] = { 1, -2, 1 }, KA2[3] = { 1, -1.99004745483398, 0.99007225036621 };
static double lufs_stereo(const float *buf, int frames, int from, int to) {
    BQ s[2][2]; memset(s, 0, sizeof s); double acc = 0; int n = 0;
    for (int i = 0; i < frames; i++) for (int c = 0; c < 2; c++) {
        double y = bq(&s[c][1], KB2, KA2, bq(&s[c][0], KB1, KA1, buf[2 * i + c]));
        if (i >= from && i < to) { acc += y * y; if (c == 0) n++; }
    }
    double ms = n ? acc / n : 0;            // sum of both channels' mean squares
    return ms > 1e-12 ? -0.691 + 10 * log10(ms) : -120;
}
static double rms_stereo(const float *buf, int frames, int skip) {
    double acc = 0; int n = 0;
    for (int i = skip; i < frames; i++) { acc += 0.5 * (buf[2 * i] * buf[2 * i] + buf[2 * i + 1] * buf[2 * i + 1]); n++; }
    return n ? sqrt(acc / n) : 0;
}

static tsf *g_t; static GMVoice g_v;
static float g_sfGain[128], g_acGain[128];
// v2 normalization: gain in dB per program at three anchor keys, interpolated per note.
static const int ANCH[3] = { 48, 60, 72 };
static float g_sfDb[128][3], g_acDb[128][3]; static int V2 = 0;
static float note_db(float t[3], int key) {
    if (key <= ANCH[0]) return t[0]; if (key >= ANCH[2]) return t[2];
    int i = key < ANCH[1] ? 0 : 1; float f = (key - ANCH[i]) / 12.0f; return t[i] + (t[i + 1] - t[i]) * f;
}

// Render one note into buf (stereo, frames), held for `hold` frames.
static void sf_note(int p, int key, int vel, float *buf, int frames, int hold, float gain) {
    tsf_channel_set_volume(g_t, p, gain);
    memset(buf, 0, sizeof(float) * 2 * frames);
    tsf_channel_note_on(g_t, p, key, vel / 127.0f);
    tsf_render_float(g_t, buf, hold, 0);
    tsf_channel_note_off(g_t, p, key);
    if (frames > hold) tsf_render_float(g_t, buf + 2 * hold, frames - hold, 0);
    tsf_channel_sounds_off_all(g_t, p);
}
static int ac_note(int p, int key, int vel, float *buf, int frames, int hold, float gain) {
    memset(buf, 0, sizeof(float) * 2 * frames);
    double f = mfreq(key);
    if (!gm_program_implemented(p) || gm_voice_init(&g_v, p, f, SR, 0x9E3779B9u ^ (uint32_t)p) != 0) return 0;
    double env = 0, att = 1.0 / (SR * 0.004), rel = 1.0 / (SR * 0.18);
    for (int i = 0; i < frames; i++) {
        if (i < hold) { env += att; if (env > 1) env = 1; } else { env -= rel; if (env <= 0) break; }
        double s = gm_voice_render(&g_v, SR, env, f);
        if (!isfinite(s)) return 0;
        float a = (float)(s * (vel / 127.0) * gain * MASTER * 0.70710678);
        buf[2 * i] = a; buf[2 * i + 1] = a;
    }
    return 1;
}
static int cmpd(const void *a, const void *b) { double x = *(const double *)a, y = *(const double *)b; return x < y ? -1 : x > y; }

static const char *NAMES[128];

int main(int argc, char **argv) {
    const char *sfpath = argc > 1 ? argv[1] : "GeneralUser-GS.sf2";
    g_t = tsf_load_filename(sfpath); if (!g_t) { fprintf(stderr, "load %s failed\n", sfpath); return 1; }
    tsf_set_output(g_t, TSF_STEREO_INTERLEAVED, (int)SR, 0);
    for (int p = 0; p < 128; p++) { tsf_channel_set_presetnumber(g_t, p, p, 0); NAMES[p] = tsf_bank_get_presetname(g_t, 0, p); }
    gm_synth_init();

    // 1. the app's normalization, verbatim
    const int mf = (int)(SR * 0.35), skip = (int)(SR * 0.03);
    float *buf = malloc(sizeof(float) * 2 * (int)(SR * 2));
    double rms[128], sorted[128]; int found = 0;
    for (int p = 0; p < 128; p++) { sf_note(p, measure_key(p), 100, buf, mf, mf, 1.0f); rms[p] = rms_stereo(buf, mf, skip); if (rms[p] > 1e-6) sorted[found++] = rms[p]; }
    qsort(sorted, found, sizeof(double), cmpd);
    double target = sorted[found / 2];
    for (int p = 0; p < 128; p++) { float g = rms[p] > 1e-6 ? (float)(target / rms[p]) : 1; g_sfGain[p] = g < 0.25f ? 0.25f : g > 4 ? 4 : g; }
    for (int p = 0; p < 128; p++) {
        g_acGain[p] = 1;
        if (!ac_note(p, measure_key(p), 100, buf, mf, mf, 1.0f)) continue;
        double r = 0; int n = 0; for (int i = skip; i < mf; i++) { r += buf[2 * i] * buf[2 * i] * 2; n++; }   // the app's mono-equivalent (both channels)
        r = sqrt(r / n / 2.0 * 2.0);   // app: a = s*vel*master*0.7071, rms of a  → per-channel rms
        r = 0; n = 0; for (int i = skip; i < mf; i++) { r += buf[2 * i] * buf[2 * i]; n++; } r = sqrt(r / n);
        float g = r > 1e-6 ? (float)(target / r) : 1; g_acGain[p] = g < 0.25f ? 0.25f : g > 4 ? 4 : g;
    }

    // 1b. v2: K-weighted loudness over 50 ms–1 s at C3/C4/C5, target = the
    // SoundFont's median at C4, gains clamped to ±18 dB.
    if (argc > 2 && (!strcmp(argv[2], "v2") || !strcmp(argv[2], "v3"))) {
        int v3 = !strcmp(argv[2], "v3");
        V2 = 1;
        const int mfr = (int)(SR * 1.0), w0 = (int)(SR * 0.05);
        static double sfL[128][3], acL[128][3]; double med[128];
        for (int p = 0; p < 128; p++) for (int a = 0; a < 3; a++) {
            // v3: the mean (in dB) of a short-note window (10–510 ms) and a
            // held-note window (50 ms–1 s), so both ways of playing even out.
            sf_note(p, ANCH[a], 100, buf, mfr, mfr, 1.0f);
            sfL[p][a] = v3 ? 0.5 * (lufs_stereo(buf, mfr, (int)(SR * 0.01), (int)(SR * 0.51)) + lufs_stereo(buf, mfr, w0, mfr)) : lufs_stereo(buf, mfr, w0, mfr);
            if (ac_note(p, ANCH[a], 100, buf, mfr, mfr, 1.0f))
                acL[p][a] = v3 ? 0.5 * (lufs_stereo(buf, mfr, (int)(SR * 0.01), (int)(SR * 0.51)) + lufs_stereo(buf, mfr, w0, mfr)) : lufs_stereo(buf, mfr, w0, mfr);
            else acL[p][a] = -120;
        }
        for (int p = 0; p < 128; p++) med[p] = sfL[p][1];
        qsort(med, 128, sizeof(double), cmpd);
        double tgt = med[64];
        for (int p = 0; p < 128; p++) for (int a = 0; a < 3; a++) {
            double ds = sfL[p][a] > -100 ? tgt - sfL[p][a] : 0, da = acL[p][a] > -100 ? tgt - acL[p][a] : 0;
            g_sfDb[p][a] = (float)fmax(-18, fmin(18, ds)); g_acDb[p][a] = (float)fmax(-18, fmin(18, da));
        }
        fprintf(stderr, "v2 target %.1f LUFS\n", tgt);
    }

    // 2. the grid
    const int keys[5] = { 36, 48, 60, 72, 84 }, vels[3] = { 64, 100, 127 };
    int longWin = argc > 3 && !strcmp(argv[3], "long");
    const int frames = (int)(SR * (longWin ? 1.2 : 0.8)), hold = (int)(SR * (longWin ? 1.0 : 0.6));
    const int win0 = (int)(SR * (longWin ? 0.05 : 0.01)), win1 = (int)(SR * (longWin ? 1.0 : 0.51));
    FILE *csv = fopen("compare.csv", "w");
    fprintf(csv, "program,name,engine,key,vel,lufs\n");
    static double sfC4[128], acC4[128], sfSpread[128], acSpread[128], sfMin[128], sfMax[128], acMin[128], acMax[128];
    for (int p = 0; p < 128; p++) {
        sfMin[p] = acMin[p] = 1e9; sfMax[p] = acMax[p] = -1e9;
        for (int k = 0; k < 5; k++) for (int v = 0; v < 3; v++) {
            sf_note(p, keys[k], vels[v], buf, frames, hold, V2 ? powf(10, note_db(g_sfDb[p], keys[k]) / 20) : g_sfGain[p]);
            double ls = lufs_stereo(buf, frames, win0, win1);
            fprintf(csv, "%d,\"%s\",sf,%d,%d,%.2f\n", p + 1, NAMES[p] ? NAMES[p] : "", keys[k], vels[v], ls);
            if (vels[v] == 100) { if (ls > -100) { if (ls < sfMin[p]) sfMin[p] = ls; if (ls > sfMax[p]) sfMax[p] = ls; } if (keys[k] == 60) sfC4[p] = ls; }
            double la = ac_note(p, keys[k], vels[v], buf, frames, hold, V2 ? powf(10, note_db(g_acDb[p], keys[k]) / 20) : g_acGain[p]) ? lufs_stereo(buf, frames, win0, win1) : -120;
            fprintf(csv, "%d,\"%s\",ac,%d,%d,%.2f\n", p + 1, NAMES[p] ? NAMES[p] : "", keys[k], vels[v], la);
            if (vels[v] == 100) { if (la > -100) { if (la < acMin[p]) acMin[p] = la; if (la > acMax[p]) acMax[p] = la; } if (keys[k] == 60) acC4[p] = la; }
        }
        sfSpread[p] = sfMax[p] - sfMin[p]; acSpread[p] = acMax[p] - acMin[p];
    }
    fclose(csv);

    // 3. summary: C4 loudness relative to each engine's median, register spread, engine gap
    double sm[128], am[128]; for (int p = 0; p < 128; p++) { sm[p] = sfC4[p]; am[p] = acC4[p]; }
    qsort(sm, 128, sizeof(double), cmpd); qsort(am, 128, sizeof(double), cmpd);
    double sfMed = sm[64], acMed = am[64];
    printf("medians at C4 v100: SoundFont %.1f LUFS, AC %.1f LUFS (gap %.1f dB)\n", sfMed, acMed, acMed - sfMed);
    int sfOut = 0, acOut = 0, gapOut = 0;
    printf("\n%-4s %-22s %7s %7s %7s | %6s %6s | %s\n", "#", "name", "SF C4", "AC C4", "AC-SF", "SFspr", "ACspr", "flags");
    for (int p = 0; p < 128; p++) {
        double ds = sfC4[p] - sfMed, da = acC4[p] - sfMed, gap = acC4[p] - sfC4[p];
        char flags[64] = "";
        if (fabs(ds) > 3) { strcat(flags, "SF-level "); sfOut++; }
        if (fabs(da) > 3) { strcat(flags, "AC-level "); acOut++; }
        if (fabs(gap) > 4) { strcat(flags, "engine-gap "); gapOut++; }
        if (sfSpread[p] > 12) strcat(flags, "SF-register ");
        if (acSpread[p] > 12) strcat(flags, "AC-register ");
        printf("%-4d %-22.22s %+7.1f %+7.1f %+7.1f | %6.1f %6.1f | %s\n", p + 1, NAMES[p] ? NAMES[p] : "?", ds, da, gap, sfSpread[p], acSpread[p], flags);
    }
    printf("\nprograms off the median by >3 dB at C4: SoundFont %d, AC %d; engines >4 dB apart: %d\n", sfOut, acOut, gapOut);

    // 4. listening file: per program, C-E-G-C at C4 in SoundFont, then AC
    FILE *w = fopen("compare-listen.wav", "wb");
    int noteF = (int)(SR * 0.22), gapF = (int)(SR * 0.05), seg = 4 * (noteF + gapF);
    long total = 128L * (2 * seg + (int)(SR * 0.25));
    int16_t *pcm = calloc((size_t)total * 2, sizeof(int16_t)); long pos = 0;
    const int phrase[4] = { 60, 64, 67, 72 };
    for (int p = 0; p < 128; p++) for (int e = 0; e < 2; e++) {
        for (int n = 0; n < 4; n++) {
            int len = noteF + gapF;
            if (e == 0) sf_note(p, phrase[n], 100, buf, len, noteF, V2 ? powf(10, note_db(g_sfDb[p], phrase[n]) / 20) : g_sfGain[p]);
            else ac_note(p, phrase[n], 100, buf, len, noteF, V2 ? powf(10, note_db(g_acDb[p], phrase[n]) / 20) : g_acGain[p]);
            for (int i = 0; i < len; i++) for (int c = 0; c < 2; c++) { float x = buf[2 * i + c]; x = x > 1 ? 1 : x < -1 ? -1 : x; pcm[2 * (pos + i) + c] = (int16_t)(x * 32767); }
            pos += len;
        }
        if (e == 1) pos += (int)(SR * 0.25);
    }
    uint32_t dataBytes = (uint32_t)(pos * 4), r32; uint16_t r16;
    fwrite("RIFF", 1, 4, w); r32 = 36 + dataBytes; fwrite(&r32, 4, 1, w); fwrite("WAVEfmt ", 1, 8, w);
    r32 = 16; fwrite(&r32, 4, 1, w); r16 = 1; fwrite(&r16, 2, 1, w); r16 = 2; fwrite(&r16, 2, 1, w);
    r32 = (uint32_t)SR; fwrite(&r32, 4, 1, w); r32 = (uint32_t)SR * 4; fwrite(&r32, 4, 1, w); r16 = 4; fwrite(&r16, 2, 1, w); r16 = 16; fwrite(&r16, 2, 1, w);
    fwrite("data", 1, 4, w); fwrite(&dataBytes, 4, 1, w); fwrite(pcm, 4, (size_t)pos, w); fclose(w);
    fprintf(stderr, "wrote compare.csv and compare-listen.wav (%.0f s)\n", pos / SR);
    return 0;
}
