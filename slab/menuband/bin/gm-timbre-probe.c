// gm-timbre-probe.c — measure all 128 gm_synth programs into a timbre space.
//
// Wessel, "Timbre Space as a Musical Control Structure" (CMJ 3(2), 1979),
// gives a five-step method: select and EQUALIZE the material, collect
// dissimilarity judgments, scale them into a geometry, interpret the axes
// psychoacoustically, verify in a musical situation. This probe runs the
// halves a machine can run honestly — step 1 and step 4 — and skips 2 and 3.
//
// There are no listeners here, so there is no MDS and no measured geometry.
// What there IS: Wessel validated his vertical axis by correlating point
// projections with the centroid of a Zwicker loudness-model excitation
// pattern, and his horizontal axis with the "bite" of the onset. Those two
// acoustic correlates are computable, so we compute them directly and use
// them as coordinates. That is a proxy for his space, not his space. Say so
// wherever the numbers are used.
//
// Equalization (his §B — "the tones should be equalized with respect to the
// properties that are not to influence the judgments"):
//   · pitch    — every program rendered at the same fundamental (C4 default)
//   · duration — identical gate and release for all
//   · loudness — every rendered tone RMS-normalized before analysis, so a
//                bright voice cannot buy brightness with level
//   · organic  — gm_set_organic(0), so the space is reproducible
//
// Axes:
//   brightness — loudness-weighted mean centroid, in Bark, of a Schroeder
//                spread-of-masking excitation pattern. Higher = brighter.
//   bite       — fast onset and synchronous partials. Built from the 10-90%
//                rise time (log-scaled; attack rate is perceived that way)
//                and the spread of per-band onset times, which is Wessel's
//                "extent of synchronicity among the various components".
//
// The analysis itself lives in bin/timbre-analysis.c, shared with the
// Fluoddity path harness so both measure on one scale.
//
// Build + run:
//   cc -O2 -I Sources/CGMSynth/include -I bin bin/gm-timbre-probe.c \
//      Sources/CGMSynth/gm_synth.c bin/timbre-analysis.c -lm \
//      -o /tmp/gm-timbre-probe
//   /tmp/gm-timbre-probe > bin/gm-timbre-space.json
//   /tmp/gm-timbre-probe --pitch 130.81            # C3, for a stability check
//
// Output is JSON on stdout; progress and warnings go to stderr.

#include "gm_synth.h"
#include "timbre-analysis.h"

#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define SR          48000.0
#define GATE_S      0.60    // key held
#define RELEASE_S   0.18    // matches MenuBandGMSynth's outer AR
#define ATTACK_S    0.004   // matches MenuBandGMSynth's outer AR
#define TAIL_S      0.60    // let self-decaying voices ring out past release
#define TOTAL_S     (GATE_S + RELEASE_S + TAIL_S)
#define NSAMP       ((long)(SR * TOTAL_S))
#define DUMP_N      2048    // window for the --dump spectrum (diagnostic only)

// ── Rendering ──

// One note through gm_voice_render with Menu Band's production outer AR.
static void render_program(int program, double freq, float *out, long n) {
    GMVoice v;
    memset(&v, 0, sizeof v);
    if (gm_voice_init(&v, program, freq, SR, 0x5EEDu + (uint32_t)program) != 0) {
        for (long i = 0; i < n; i++) out[i] = 0.0f;
        return;
    }
    long gate = (long)(SR * GATE_S);
    double attack_inc = 1.0 / (SR * ATTACK_S);
    double release_dec = 1.0 / (SR * RELEASE_S);
    double env = 0.0;
    for (long i = 0; i < n; i++) {
        if (i < gate) { env += attack_inc; if (env > 1.0) env = 1.0; }
        else          { env -= release_dec; if (env < 0.0) env = 0.0; }
        double s = gm_voice_render(&v, SR, env, freq);
        if (!isfinite(s)) s = 0.0;
        out[i] = (float)s;
    }
}

// ── Analysis ──

typedef struct {
    double brightness;    // Bark centroid of the excitation pattern
    double rise_ms;       // 10→90% of peak
    double async_ms;      // spread of per-band onset times
    double bite;          // combined, normalized later
    double rms;
    int    silent;
} Measure;

static Measure measure_program(int program, double freq) {
    Measure m;
    memset(&m, 0, sizeof m);
    float *buf = malloc(sizeof(float) * NSAMP);
    render_program(program, freq, buf, NSAMP);

    double sum = 0.0;
    for (long i = 0; i < NSAMP; i++) sum += (double)buf[i] * buf[i];
    m.rms = sqrt(sum / NSAMP);
    if (m.rms < 1e-7) {
        m.silent = 1;
        free(buf);
        return m;
    }
    // Wessel §B: equalize loudness so the timbral contrast is what is left.
    // RMS is the crudest possible loudness match — he says as much, and
    // resorts to empirical matching. For a machine axis it is enough, and
    // it runs BEFORE the brightness measurement so a hot voice cannot read
    // as a bright one.
    timbre_loudness_equalize(buf, NSAMP, 0.1);

    m.brightness = timbre_brightness(buf, NSAMP, SR);
    m.rise_ms    = timbre_rise_ms(buf, NSAMP, SR);
    m.async_ms   = timbre_async_ms(buf, NSAMP, SR);
    free(buf);
    return m;
}

// ── Main ──

int main(int argc, char **argv) {
    double freq = 261.6255653;   // C4 — MIDI 60, AC's reference pitch
    const char *pitch_name = "C4";
    for (int i = 1; i < argc; i++) {
        if (!strcmp(argv[i], "--pitch") && i + 1 < argc) {
            freq = atof(argv[++i]);
            pitch_name = "custom";
        }
    }

    timbre_analysis_init();
    gm_synth_init();
    gm_set_organic(0.0);   // reproducible: no per-trigger stochastic spread

    // --selftest: run the brightness axis over signals whose ordering is not
    // in dispute. A sine must land near its own Bark band, a sawtooth above
    // it, white noise near the top. If this ordering ever breaks, the axis is
    // broken and nothing measured with it means anything.
    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "--selftest")) continue;
        float *buf = malloc(sizeof(float) * NSAMP);
        uint32_t rng = 12345u;
        struct { const char *name; int kind; } cases[] = {
            { "sine 261 Hz",     0 },
            { "sawtooth 261 Hz", 1 },
            { "square 261 Hz",   2 },
            { "white noise",     3 },
        };
        fprintf(stderr, "brightness self-test (Bark centroid, higher = brighter)\n");
        for (int c = 0; c < 4; c++) {
            double ph = 0.0, inc = freq / SR;
            for (long k = 0; k < NSAMP; k++) {
                double s = 0.0;
                switch (cases[c].kind) {
                    case 0: s = sin(2.0 * M_PI * ph); break;
                    case 1: s = 2.0 * ph - 1.0; break;
                    case 2: s = ph < 0.5 ? 1.0 : -1.0; break;
                    case 3: rng ^= rng << 13; rng ^= rng >> 17; rng ^= rng << 5;
                            s = (double)rng / 2147483648.0 - 1.0; break;
                }
                ph += inc; if (ph >= 1.0) ph -= 1.0;
                buf[k] = (float)(s * 0.3);
            }
            fprintf(stderr, "  %-16s %.3f\n", cases[c].name, timbre_brightness(buf, NSAMP, SR));
        }
        fprintf(stderr, "  (sine < square < sawtooth < noise expected)\n");
        free(buf);
        return 0;
    }

    // --dump N: print program N's mean excitation profile to stderr. Kept in
    // the tool because "the axis disagrees with my ears" is the question this
    // probe will be asked most often, and the profile is the answer.
    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "--dump") || i + 1 >= argc) continue;
        int p = atoi(argv[i + 1]);
        float *buf = malloc(sizeof(float) * NSAMP);
        render_program(p, freq, buf, NSAMP);
        double sum = 0.0;
        for (long k = 0; k < NSAMP; k++) sum += (double)buf[k] * buf[k];
        double rms = sqrt(sum / NSAMP);
        if (rms > 1e-9) { double g = 0.1 / rms; for (long k = 0; k < NSAMP; k++) buf[k] *= (float)g; }
        // One long window over the sustain, straight power per Bark band.
        static double re[DUMP_N], im[DUMP_N];
        double band[TIMBRE_NBARK] = {0};
        long start = (long)(SR * 0.20);
        for (int k = 0; k < DUMP_N; k++) {
            double w = 0.5 * (1.0 - cos(2.0 * M_PI * k / (DUMP_N - 1)));
            re[k] = buf[start + k] * w; im[k] = 0.0;
        }
        timbre_fft(re, im, DUMP_N);
        for (int k = 1; k < DUMP_N / 2; k++) {
            double hz = k * SR / DUMP_N;
            int b = (int)timbre_hz_to_bark(hz);
            if (b >= 0 && b < TIMBRE_NBARK) band[b] += re[k] * re[k] + im[k] * im[k];
        }
        double tot = 0.0;
        for (int b = 0; b < TIMBRE_NBARK; b++) tot += band[b];
        fprintf(stderr, "program %d @ %.2f Hz — power per Bark band (%% of total)\n", p, freq);
        for (int b = 0; b < TIMBRE_NBARK; b++) {
            double pct = tot > 0 ? 100.0 * band[b] / tot : 0.0;
            fprintf(stderr, "  z%-2d %6.0f Hz %6.2f%% ", b, timbre_bark_to_hz(b), pct);
            for (int s = 0; s < (int)(pct + 0.5) && s < 60; s++) fputc('#', stderr);
            fputc('\n', stderr);
        }
        free(buf);
        return 0;
    }

    Measure m[128];
    for (int p = 0; p < 128; p++) {
        m[p] = measure_program(p, freq);
        if (m[p].silent) fprintf(stderr, "program %d rendered silent\n", p);
        if ((p + 1) % 16 == 0) fprintf(stderr, "  measured %d/128\n", p + 1);
    }

    // Normalize each axis to 0..1 across the measured set. The space is
    // stimulus-set-relative (Wessel is explicit); these numbers describe
    // THIS catalog at THIS pitch and warp if the set changes.
    double bmin = 1e9, bmax = -1e9, rmin = 1e9, rmax = -1e9, amin = 1e9, amax = -1e9;
    for (int p = 0; p < 128; p++) {
        if (m[p].silent) continue;
        double lr = log10(m[p].rise_ms + 0.5);   // attack rate is heard log-wise
        double la = log10(m[p].async_ms + 0.5);
        if (m[p].brightness < bmin) bmin = m[p].brightness;
        if (m[p].brightness > bmax) bmax = m[p].brightness;
        if (lr < rmin) rmin = lr;  if (lr > rmax) rmax = lr;
        if (la < amin) amin = la;  if (la > amax) amax = la;
    }

    printf("{\n");
    printf("  \"generator\": \"slab/menuband/bin/gm-timbre-probe.c\",\n");
    printf("  \"after\": \"Wessel 1979, Timbre Space as a Musical Control Structure\",\n");
    printf("  \"method\": \"acoustic correlates only — no listeners, no MDS\",\n");
    printf("  \"pitch_hz\": %.4f,\n", freq);
    printf("  \"pitch_name\": \"%s\",\n", pitch_name);
    printf("  \"sample_rate\": %.0f,\n", SR);
    printf("  \"organic\": 0,\n");
    printf("  \"loudness_equalized\": true,\n");
    printf("  \"axes\": {\n");
    printf("    \"brightness\": \"Bark centroid of a Schroeder-spread excitation pattern, loudness-weighted over the note\",\n");
    printf("    \"bite\": \"fast + synchronous onset; from log 10-90%% rise time and per-band onset spread\"\n");
    printf("  },\n");
    printf("  \"programs\": [\n");
    for (int p = 0; p < 128; p++) {
        double bn = 0.0, bite = 0.0;
        if (!m[p].silent) {
            bn = (bmax > bmin) ? (m[p].brightness - bmin) / (bmax - bmin) : 0.0;
            double lr = log10(m[p].rise_ms + 0.5);
            double la = log10(m[p].async_ms + 0.5);
            double fast  = (rmax > rmin) ? 1.0 - (lr - rmin) / (rmax - rmin) : 0.0;
            double sync  = (amax > amin) ? 1.0 - (la - amin) / (amax - amin) : 0.0;
            // Rise time is the dominant cue; onset asynchrony is the refinement
            // Wessel names but never quantifies.
            bite = 0.7 * fast + 0.3 * sync;
        }
        printf("    { \"program\": %d, \"brightness\": %.5f, \"bite\": %.5f, "
               "\"bark_centroid\": %.4f, \"rise_ms\": %.3f, \"async_ms\": %.3f, "
               "\"rms\": %.6f, \"silent\": %s }%s\n",
               p, bn, bite, m[p].brightness, m[p].rise_ms, m[p].async_ms,
               m[p].rms, m[p].silent ? "true" : "false",
               p == 127 ? "" : ",");
    }
    printf("  ]\n}\n");
    return 0;
}
