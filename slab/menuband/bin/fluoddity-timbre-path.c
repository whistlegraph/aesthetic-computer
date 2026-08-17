// fluoddity-timbre-path.c — does blending two genomes give a smooth timbral
// path, or does it fall off a cliff in the middle?
//
// This is the question `fluod_rule_lerp` raises and cannot answer by itself.
// Grey (1975) interpolated envelope BREAKPOINTS between two tones and got
// sequences that were "perceptually smooth and did not exhibit abrupt changes
// in timbre" — and when he re-scaled the enlarged set, the interpolated tones
// landed between their parents, as the geometry predicted. A Fluoddity genome
// is not a breakpoint list. It is 80 parameters of a nonlinear particle
// ecosystem, and nothing guarantees that halfway between two rules sounds
// halfway between two instruments.
//
// So: render the path, measure each step, and print the answer.
//
//   · brightness  — Bark centroid of a Zwicker-style excitation pattern
//   · jump        — brightness change from the previous step
//
// A smooth path has small, similar jumps. A cliff shows up as one big jump
// with small ones either side. The tool reports the worst jump against the
// mean so the verdict does not depend on reading the column by eye.
//
// Build + run:
//   cc -O2 -I Sources/CFluoddity/include -I bin bin/fluoddity-timbre-path.c \
//      Sources/CFluoddity/fluoddity_voice.c bin/timbre-analysis.c -lm \
//      -o /tmp/fluod-path
//   /tmp/fluod-path                 # blend seed 7 → seed 42 in 11 steps
//   /tmp/fluod-path --a 3 --b 9 --steps 21
//   /tmp/fluod-path --analogy       # check the parallelogram closes

#include "fluoddity_voice.h"
#include "timbre-analysis.h"

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define SR        48000.0
#define NOTE_S    1.4
#define NSAMP     ((long)(SR * NOTE_S))
#define GATE_S    0.9
#define ATTACK_S  0.008
#define RELEASE_S 0.22
#define FREQ      220.0    // A3 — the pitch bin/fluoddity-audition.c auditions at

// One held note from an explicit genome, with the voice's own outer AR.
static void render_rule(const FluodRule *rule, float *out, long n) {
    static FluodVoice v;   // ~36 KB of field and table buffers — not a stack local
    memset(&v, 0, sizeof v);
    fluod_voice_init_rule(&v, rule, 0xA11CE, FREQ, SR);
    long gate = (long)(SR * GATE_S);
    double attack_inc = 1.0 / (SR * ATTACK_S);
    double release_dec = 1.0 / (SR * RELEASE_S);
    double env = 0.0;
    for (long i = 0; i < n; i++) {
        if (i < gate) { env += attack_inc; if (env > 1.0) env = 1.0; }
        else          { env -= release_dec; if (env < 0.0) env = 0.0; }
        double s = fluod_voice_render(&v, SR, env, FREQ);
        out[i] = isfinite(s) ? (float)s : 0.0f;
    }
}

static double brightness_of(const FluodRule *rule, float *buf) {
    render_rule(rule, buf, NSAMP);
    // Wessel §B: equalize loudness before asking about timbre. The C voice
    // already AGCs toward a target, but a genome that drives the swarm into a
    // corner still comes out quieter, and quieter must not read as darker.
    if (!timbre_loudness_equalize(buf, NSAMP, 0.1)) return -1.0;
    return timbre_brightness(buf, NSAMP, SR);
}

int main(int argc, char **argv) {
    unsigned seed_a = 7, seed_b = 42, seed_c = 19;
    int steps = 11, analogy = 0;
    for (int i = 1; i < argc; i++) {
        if (!strcmp(argv[i], "--a") && i + 1 < argc) seed_a = (unsigned)atoi(argv[++i]);
        else if (!strcmp(argv[i], "--b") && i + 1 < argc) seed_b = (unsigned)atoi(argv[++i]);
        else if (!strcmp(argv[i], "--c") && i + 1 < argc) seed_c = (unsigned)atoi(argv[++i]);
        else if (!strcmp(argv[i], "--steps") && i + 1 < argc) steps = atoi(argv[++i]);
        else if (!strcmp(argv[i], "--analogy")) analogy = 1;
    }
    if (steps < 3) steps = 3;

    timbre_analysis_init();
    float *buf = malloc(sizeof(float) * NSAMP);

    FluodRule a, b, c;
    fluod_rule_from_seed(&a, seed_a);
    fluod_rule_from_seed(&b, seed_b);
    fluod_rule_from_seed(&c, seed_c);

    if (analogy) {
        // The parallelogram must at least be arithmetically sound: applying
        // A→B at A returns B, and the D it produces sits the same genome
        // distance from C that B sits from A. Whether it sounds like the same
        // move is the open question, and the brightness column is the hint.
        FluodRule d, check;
        fluod_rule_analogy(&d, &a, &b, &c);
        fluod_rule_analogy(&check, &a, &b, &a);
        printf("parallelogram A(%u) : B(%u) :: C(%u) : D\n", seed_a, seed_b, seed_c);
        printf("  |B-A| = %.4f   |D-C| = %.4f   (must match)\n",
               fluod_rule_distance(&a, &b), fluod_rule_distance(&c, &d));
        printf("  A+(B-A) recovers B: residual %.6f  (must be ~0)\n",
               fluod_rule_distance(&check, &b));
        printf("\n  genome    brightness (Bark)\n");
        const FluodRule *set[4] = { &a, &b, &c, &d };
        const char *nm[4] = { "A", "B", "C", "D" };
        double bz[4];
        for (int i = 0; i < 4; i++) {
            bz[i] = brightness_of(set[i], buf);
            printf("  %-8s  %7.3f\n", nm[i], bz[i]);
        }
        // If the analogy transposed the same timbral move, C→D should shift
        // brightness roughly the way A→B did. Reported, not asserted.
        printf("\n  A→B brightness shift %+.3f Bark\n", bz[1] - bz[0]);
        printf("  C→D brightness shift %+.3f Bark\n", bz[3] - bz[2]);
        printf("  (a transposed move would put these close; they may not be —\n"
               "   the genome is not a perceptual space, which is the point)\n");
        free(buf);
        return 0;
    }

    printf("blending genome seed %u → seed %u over %d steps\n", seed_a, seed_b, steps);
    printf("  genome distance |B-A| = %.4f\n\n", fluod_rule_distance(&a, &b));
    printf("     t   brightness   jump\n");

    double prev = 0.0, worst = 0.0, total = 0.0;
    int njumps = 0;
    for (int i = 0; i < steps; i++) {
        float t = (float)i / (float)(steps - 1);
        FluodRule mid;
        fluod_rule_lerp(&mid, &a, &b, t);
        double z = brightness_of(&mid, buf);
        if (i == 0) {
            printf("  %.2f   %8.3f      -\n", t, z);
        } else {
            double jump = fabs(z - prev);
            if (jump > worst) worst = jump;
            total += jump;
            njumps++;
            printf("  %.2f   %8.3f  %7.3f\n", t, z, jump);
        }
        prev = z;
    }

    double mean = njumps ? total / njumps : 0.0;
    printf("\n  mean step %.3f Bark, worst step %.3f Bark", mean, worst);
    if (mean > 0.0) printf(" (%.1fx mean)", worst / mean);
    printf("\n  %s\n", worst > 4.0 * mean && worst > 0.5
           ? "→ CLIFF: the blend is not perceptually smooth at this pair"
           : "→ no cliff: steps are comparable, the path reads as a path");

    free(buf);
    return 0;
}
