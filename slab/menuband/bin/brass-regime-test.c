// brass-regime-test.c — the two things the brass voices must do at once.
//
// A digital waveguide brass voice has a standing tension. Drive the lip valve
// hard enough to generate a real harmonic spectrum and the loop can jump to a
// higher regime — overblowing, which on a GM synth means a note lands a
// twelfth or two sharp. Damp it until that can never happen and you get a sine
// at the right pitch. gm_synth has been on both sides of that line:
//
//   · before 2026-06, a quadratic valve fed straight back into the loop, and
//     pitch wandered +/-300-600 cents note to note;
//   · the rebuild that fixed the pitch throttled the loop to a 0.4 round-trip
//     gain and added a unity-gain bandpass AT f0 into the loop signal, which
//     is a fundamental booster. Trumpet came out with 90% of its power in the
//     fundamental band and wg_loop_damp — the per-instrument brightness knob —
//     had no measurable effect at all.
//
// So neither property alone is a passing grade, and eyeballing one note is not
// a test: the stable region has a chaotic boundary, and single lucky points sit
// right next to configurations where 6 notes in 792 jump 15 semitones. This
// sweeps every semitone of a 44-note range across several per-voice seeds with
// production jitter ON, and reports both numbers together.
//
//   upper%  fraction of sustained power ABOVE the fundamental. A sine is ~0.
//   worstc  largest pitch error in cents over the whole sweep.
//   bad     notes off by more than 50 cents — must be ZERO.
//
// Build + run:
//   cc -O2 -I Sources/CGMSynth/include -I bin bin/brass-regime-test.c \
//      Sources/CGMSynth/gm_synth.c bin/timbre-analysis.c -lm \
//      -o /tmp/brass-regime-test
//   /tmp/brass-regime-test            # exits non-zero if any voice regresses
//   /tmp/brass-regime-test --seeds 12 # slower, more confidence

#include "gm_synth.h"
#include "timbre-analysis.h"

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define SR        48000.0
#define NOTE_S    1.2
#define NSAMP     ((long)(SR * NOTE_S))
#define MIDI_LO   36
#define MIDI_HI   79

// Thresholds. `upper%` is not held to a real trumpet's ~80% — the model does
// not get there without overblowing — but it must stay clear of the sine the
// broken version produced (9.8-12.5%).
#define MIN_UPPER   0.15
#define MAX_CENTS   50.0

static void render(int prog, double f0, float *x, uint32_t seed) {
    GMVoice v;
    memset(&v, 0, sizeof v);
    gm_voice_init(&v, prog, f0, SR, seed);
    long gate = (long)(SR * 0.9);
    double env = 0, ai = 1.0 / (SR * 0.004), rd = 1.0 / (SR * 0.18);
    for (long i = 0; i < NSAMP; i++) {
        if (i < gate) { env += ai; if (env > 1) env = 1; }
        else          { env -= rd; if (env < 0) env = 0; }
        double s = gm_voice_render(&v, SR, env, f0);
        x[i] = isfinite(s) ? (float)s : 0.f;
    }
}

static double upper_fraction(const float *x, double f0) {
    int n = 4096;
    static double re[4096], im[4096];
    long a = (long)(SR * 0.4);
    for (int i = 0; i < n; i++) {
        double w = 0.5 * (1 - cos(2 * M_PI * i / (n - 1)));
        re[i] = x[a + i] * w; im[i] = 0;
    }
    timbre_fft(re, im, n);
    double fund = 0, tot = 0;
    for (int k = 1; k < n / 2; k++) {
        double hz = k * SR / n, p = re[k] * re[k] + im[k] * im[k];
        if (hz > 12000) break;
        tot += p;
        if (hz < f0 * 1.5) fund += p;
    }
    return tot > 0 ? 1.0 - fund / tot : 0.0;
}

// Autocorrelation with octave-error protection: take the SHORTEST lag whose
// correlation is within 5% of the best. A near-sine correlates almost as well
// at twice its period, so a plain argmax invents octave drops — which is what
// the first version of this test reported before the guard existed.
static double cents_err(const float *x, double f0) {
    long a = (long)(SR * 0.5), n = 8192;
    double best = -2; int bl = 0;
    int lo = (int)(SR / (f0 * 2.6)), hi = (int)(SR / (f0 * 0.38));
    if (lo < 20) lo = 20;
    if (hi > 4000) hi = 4000;
    static double r[4096];
    for (int lag = lo; lag < hi; lag++) {
        double s = 0, e1 = 0, e2 = 0;
        for (long i = 0; i < n; i++) {
            s += x[a + i] * x[a + i + lag];
            e1 += x[a + i] * x[a + i];
            e2 += x[a + i + lag] * x[a + i + lag];
        }
        r[lag] = s / (sqrt(e1 * e2) + 1e-12);
        if (r[lag] > best) { best = r[lag]; bl = lag; }
    }
    if (best < 0.2) return 9999;   // not periodic enough to judge
    for (int lag = lo + 1; lag < bl; lag++)
        if (r[lag] >= 0.95 * best && r[lag] > r[lag - 1] && r[lag] >= r[lag + 1]) { bl = lag; break; }
    if (bl <= lo || bl >= hi - 1) return 9999;
    double d = (r[bl - 1] - r[bl + 1]) / (2 * (r[bl - 1] - 2 * r[bl] + r[bl + 1]) + 1e-12);
    return 1200.0 * log2((SR / ((double)bl + d)) / f0);
}

int main(int argc, char **argv) {
    int seeds = 6;
    for (int i = 1; i < argc; i++)
        if (!strcmp(argv[i], "--seeds") && i + 1 < argc) seeds = atoi(argv[++i]);
    if (seeds < 1) seeds = 1;

    timbre_analysis_init();
    gm_synth_init();
    gm_set_organic(0.6);   // production default — the jitter must not break it

    const int progs[6] = { 56, 57, 58, 59, 60, 61 };
    const char *nm[6] = { "Trumpet", "Trombone", "Tuba", "MutedTpt", "FrenchHn", "BrassSec" };
    float *x = malloc(sizeof(float) * NSAMP);
    int failures = 0;
    int per = (MIDI_HI - MIDI_LO + 1) * seeds;

    printf("brass regime test — %d notes x %d seeds per voice, organic 0.6\n", MIDI_HI - MIDI_LO + 1, seeds);
    printf("%-9s %8s %9s %8s %6s\n", "voice", "meanUp%", "worstc", "bad", "");
    for (int i = 0; i < 6; i++) {
        double sum = 0, worst = 0;
        int bad = 0, cnt = 0;
        for (int m = MIDI_LO; m <= MIDI_HI; m++) {
            double f = 440.0 * pow(2.0, (m - 69) / 12.0);
            for (int s = 1; s <= seeds; s++) {
                render(progs[i], f, x, (uint32_t)(0x100 * s + m));
                sum += upper_fraction(x, f);
                cnt++;
                double c = cents_err(x, f);
                if (fabs(c) > 9000) continue;
                if (fabs(c) > MAX_CENTS) bad++;
                if (fabs(c) > fabs(worst)) worst = c;
            }
        }
        double mean = sum / cnt;
        int fail = (bad > 0) || (mean < MIN_UPPER);
        if (fail) failures++;
        printf("%-9s %7.1f%% %+9.1f %5d/%-4d %s\n",
               nm[i], 100 * mean, worst, bad, per, fail ? "FAIL" : "ok");
    }
    free(x);
    if (failures) {
        printf("\n%d voice(s) regressed. A sine at the right pitch and a rich tone at\n"
               "the wrong one are both failures; the fix has to hold both.\n", failures);
        return 1;
    }
    printf("\nall voices hold pitch and carry harmonics.\n");
    return 0;
}
