// fluoddity_voice.c — see fluoddity_voice.h for the design story.
//
// Ported from aphid91/Fluoddity's entity_update.glsl + fourier4_4.glsl +
// canvas.frag (commit 809d6b3). The particle math keeps Fluoddity's exact
// structure — local sensor frames, the 80-parameter Fourier black box, the
// chiral mirror average, drag/strafe, the (c·K + n+s+e+w)/(4+K) diffusion
// kernel — while the absolute scales are re-tuned for a 128×16 torus with a
// 40-particle swarm ticking at ~1 kHz instead of 600k particles at 60 fps.
// Trail persistence/diffusion sliders are wall-clock compensated so their
// values mean the same thing they mean in visual Fluoddity.

#include "fluoddity_voice.h"

#include <math.h>
#include <string.h>

#define FLUOD_PI 3.14159265358979323846f

// ── Tuning constants (torus units per tick unless noted) ──
#define FLUOD_SENSOR_BASE 0.02f  // sensor reach at sensor_distance = 1
#define FLUOD_FORCE_SCALE 0.010f // black-box force → velocity units
#define FLUOD_STRAFE_SCALE (FLUOD_FORCE_SCALE * 20.0f) // Fluoddity's /20 vs /400
#define FLUOD_TARGET_SPEED 0.005f // deposit normalization speed
#define FLUOD_MAX_SPEED 0.025f    // per-tick speed clamp
#define FLUOD_SENSE_SCALE 1.0f    // field → black-box input gain
#define FLUOD_VISUAL_FPS 60.0f    // Fluoddity's frame rate, for slider compensation
#define FLUOD_WARMUP_TICKS 32     // sim ticks pre-run at note-on
#define FLUOD_OUT_TARGET 0.42f    // output RMS anchor (see FLUOD_AGC_EXP)
#define FLUOD_AGC_FLOOR 0.02f     // table RMS below this fades toward silence
// Partial normalization: gain = (target/(floor+rms))^EXP. EXP 1 would be a
// hard leveler that irons out the ecosystem's own swells; 0.65 leaves a
// ±4 dB-ish window of real dynamics while still taming outliers.
#define FLUOD_AGC_EXP 0.65f
#define FLUOD_AGC_SMOOTH_S 0.25   // gain one-pole (s); slow enough to let
                                  // the swarm's 1–8 Hz amplitude life through
#define FLUOD_VIB_CENTS 4.0f      // max swarm-drift pitch micro-bend
#define FLUOD_BRIGHT_ATTACK 0.75f // raw-layer mix at note birth…
#define FLUOD_BRIGHT_TAU 0.10     // …decaying with this time constant (s)
#define FLUOD_BRIGHT_AGIT 0.22f   // sustained raw-layer mix per unit agitation
#define FLUOD_SIDE_WIDTH 0.45f    // y-flow side layer gain into L/R

// ── Hashing (PCG, uint-domain cousin of fourier4_4.glsl's) ──

static inline uint32_t fluod_pcg(uint32_t seed) {
    uint32_t state = seed * 747796405u + 2891336453u;
    uint32_t word = ((state >> ((state >> 28u) + 4u)) ^ state) * 277803737u;
    return (word >> 22u) ^ word;
}

static inline float fluod_u01(uint32_t seed, uint32_t idx) {
    return (float)fluod_pcg(seed ^ fluod_pcg(idx * 2654435769u)) *
           (1.0f / 4294967295.0f);
}

static inline uint32_t fluod_xorshift(uint32_t *s) {
    uint32_t x = *s ? *s : 0x6d2b79f5u;
    x ^= x << 13;
    x ^= x >> 17;
    x ^= x << 5;
    return *s = x;
}

static inline float fluod_rand01(uint32_t *s) {
    return (float)fluod_xorshift(s) * (1.0f / 4294967295.0f);
}

// ── Rule (genome) ──

void fluod_rule_from_seed(FluodRule *r, uint32_t seed) {
    for (int i = 0; i < FLUOD_CENTERS; i++) {
        // Same shape as generate_random_centers: frequencies in [-scale,
        // scale] with scale = 1 + 2u², biased low for smooth base behavior;
        // the x component reuses the scale draw (a Fluoddity quirk, kept).
        float u0 = fluod_u01(seed, (uint32_t)(i * 8 + 0));
        float freq_scale = 1.0f + 2.0f * u0 * u0;
        r->centers[i].freq[0] = (u0 * 2.0f - 1.0f) * freq_scale;
        for (int k = 1; k < 4; k++) {
            float u = fluod_u01(seed, (uint32_t)(i * 8 + k));
            r->centers[i].freq[k] = (u * 2.0f - 1.0f) * freq_scale;
        }
        for (int k = 0; k < 4; k++) {
            float u = fluod_u01(seed, (uint32_t)(i * 8 + 4 + k));
            r->centers[i].amp[k] = u * 2.0f - 1.0f;
        }
    }
}

void fluod_rule_mutate(FluodRule *r, float amount, uint32_t seed) {
    for (int i = 0; i < FLUOD_CENTERS; i++) {
        // Per-center scalar frequency stretch, per-component amplitude nudge —
        // mirrors mutate_rule in entity_update.glsl.
        float fs = 1.0f + amount * 0.5f *
                              (fluod_u01(seed, (uint32_t)(i * 16 + 15)) - 0.5f);
        for (int k = 0; k < 4; k++) {
            float u = fluod_u01(seed, (uint32_t)(i * 16 + k));
            r->centers[i].amp[k] += amount * (u * 2.0f - 1.0f);
            r->centers[i].freq[k] *= fs;
        }
    }
}

void fluod_rule_from_floats(FluodRule *r, const float flat[80]) {
    memcpy(r, flat, sizeof(FluodRule));
}

void fluod_rule_to_floats(const FluodRule *r, float flat[80]) {
    memcpy(flat, r, sizeof(FluodRule));
}

// ── Moving THROUGH the genome instead of only jumping around it ──
//
// Seeding and mutating are the two gestures Fluoddity came with: teleport to
// a random point, or take a random step from where you are. Neither can
// answer "put me halfway between these two" or "do to this one what you did
// to that one" — which is what a space is FOR (Wessel 1979).
//
// The genome is 80 floats laid out center-major, so all three operations are
// component-wise. Frequencies are drawn signed in [-scale, scale], so the
// interpolation has to be linear: a geometric blend cannot cross zero, and
// the sign of a frequency is meaningful. Linear is also what Grey (1975)
// used on envelope breakpoints, where interpolated tones came out
// perceptually smooth and landed where the geometry predicted.

void fluod_rule_lerp(FluodRule *out, const FluodRule *a, const FluodRule *b,
                     float t) {
    for (int i = 0; i < FLUOD_CENTERS; i++) {
        for (int k = 0; k < 4; k++) {
            out->centers[i].freq[k] = a->centers[i].freq[k] +
                (b->centers[i].freq[k] - a->centers[i].freq[k]) * t;
            out->centers[i].amp[k] = a->centers[i].amp[k] +
                (b->centers[i].amp[k] - a->centers[i].amp[k]) * t;
        }
    }
}

// Rumelhart & Abramson's parallelogram, the model Ehresman & Wessel (1978)
// tested on timbre: A is to B as C is to D, so D = C + (B - A).
//
// A caveat that belongs in the code and not only in a paper: Wessel's
// parallelogram predicted listener rankings because his coordinates came out
// of dissimilarity JUDGMENTS. These coordinates are a genome — the distance
// between two rules is not known to be the distance between two sounds. So
// this is a compositional lever with a defensible shape, not a perceptual
// claim. `bin/fluoddity-timbre-path.c` is where you check what it does.
void fluod_rule_analogy(FluodRule *out, const FluodRule *a, const FluodRule *b,
                        const FluodRule *c) {
    for (int i = 0; i < FLUOD_CENTERS; i++) {
        for (int k = 0; k < 4; k++) {
            out->centers[i].freq[k] = c->centers[i].freq[k] +
                (b->centers[i].freq[k] - a->centers[i].freq[k]);
            out->centers[i].amp[k] = c->centers[i].amp[k] +
                (b->centers[i].amp[k] - a->centers[i].amp[k]);
        }
    }
}

// Euclidean distance over the 80 parameters. Genome distance, NOT timbral
// distance — useful for "how far did that mutation actually move me", which
// was previously unanswerable.
float fluod_rule_distance(const FluodRule *a, const FluodRule *b) {
    float sum = 0.0f;
    for (int i = 0; i < FLUOD_CENTERS; i++) {
        for (int k = 0; k < 4; k++) {
            float df = a->centers[i].freq[k] - b->centers[i].freq[k];
            float da = a->centers[i].amp[k] - b->centers[i].amp[k];
            sum += df * df + da * da;
        }
    }
    return sqrtf(sum);
}

void fluod_physics_default(FluodPhysics *p) {
    p->axial_force = 0.371f;
    p->lateral_force = -0.707f;
    p->sensor_gain = 1.0f;
    p->sensor_angle = 0.45f;
    p->sensor_distance = 1.0f;
    p->global_force_mult = 0.399f;
    p->drag = 0.504f;
    p->strafe_power = 0.224f;
    p->mutation_scale = 0.137f;
    p->trail_persistence = 0.938f;
    p->trail_diffusion = 1.0f;
    p->deposit = 1.0f;
    p->tap_blur = 2;
    p->disable_symmetry = 0;
}

// ── The black box (fourier4_4.glsl's fourier_noise, verbatim math) ──

static void fluod_black_box(const FluodRule *r, const float in[4],
                            float out[4]) {
    out[0] = out[1] = out[2] = out[3] = 0.0f;
    for (int i = 0; i < FLUOD_CENTERS; i++) {
        const FluodCenter *c = &r->centers[i];
        float phase = in[0] * c->freq[0] + in[1] * c->freq[1] +
                      in[2] * c->freq[2] + in[3] * c->freq[3];
        float po = 2.0f * (float)i * 0.6283f + c->amp[3] * 3.14159f;
        out[0] += c->amp[0] * sinf(phase + po);
        out[1] += c->amp[1] * cosf(phase + po * 0.7f);
        out[2] += c->amp[2] * sinf(phase * 2.0f + po * 1.3f);
        out[3] += c->amp[3] * cosf(phase * 2.0f + po * 0.5f);
    }
}

// ── Field access (bilinear, torus-wrapped both axes) ──

static inline float fluod_wrap01(float x) {
    x -= floorf(x);
    return x;
}

static void fluod_field_sample(const FluodVoice *v, float px, float py,
                               float out[2]) {
    float fx = fluod_wrap01(px) * FLUOD_FIELD_W;
    float fy = fluod_wrap01(py) * FLUOD_FIELD_H;
    int x0 = (int)fx, y0 = (int)fy;
    float tx = fx - (float)x0, ty = fy - (float)y0;
    if (x0 >= FLUOD_FIELD_W) x0 = 0;
    if (y0 >= FLUOD_FIELD_H) y0 = 0;
    int x1 = (x0 + 1) & (FLUOD_FIELD_W - 1);
    int y1 = (y0 + 1) & (FLUOD_FIELD_H - 1);
    for (int k = 0; k < 2; k++) {
        float a = v->field[y0][x0][k] * (1 - tx) + v->field[y0][x1][k] * tx;
        float b = v->field[y1][x0][k] * (1 - tx) + v->field[y1][x1][k] * tx;
        out[k] = a * (1 - ty) + b * ty;
    }
}

static void fluod_field_splat(FluodVoice *v, float px, float py,
                              const float val[2]) {
    float fx = fluod_wrap01(px) * FLUOD_FIELD_W;
    float fy = fluod_wrap01(py) * FLUOD_FIELD_H;
    int x0 = (int)fx, y0 = (int)fy;
    float tx = fx - (float)x0, ty = fy - (float)y0;
    if (x0 >= FLUOD_FIELD_W) x0 = 0;
    if (y0 >= FLUOD_FIELD_H) y0 = 0;
    int x1 = (x0 + 1) & (FLUOD_FIELD_W - 1);
    int y1 = (y0 + 1) & (FLUOD_FIELD_H - 1);
    for (int k = 0; k < 2; k++) {
        v->field[y0][x0][k] += val[k] * (1 - tx) * (1 - ty);
        v->field[y0][x1][k] += val[k] * tx * (1 - ty);
        v->field[y1][x0][k] += val[k] * (1 - tx) * ty;
        v->field[y1][x1][k] += val[k] * tx * ty;
    }
}

// ── Swarm reset ──

static void fluod_scatter(FluodVoice *v) {
    for (int i = 0; i < FLUOD_PARTICLES; i++) {
        v->p[i].px = fluod_rand01(&v->rng);
        v->p[i].py = fluod_rand01(&v->rng);
        float ang = fluod_rand01(&v->rng) * 2.0f * FLUOD_PI;
        float spd = FLUOD_TARGET_SPEED * (0.25f + 0.75f * fluod_rand01(&v->rng));
        v->p[i].vx = cosf(ang) * spd;
        v->p[i].vy = sinf(ang) * spd;
    }
    memset(v->field, 0, sizeof(v->field));
    memset(v->tab_prev, 0, sizeof(v->tab_prev));
    memset(v->tab_cur, 0, sizeof(v->tab_cur));
    memset(v->tabr_prev, 0, sizeof(v->tabr_prev));
    memset(v->tabr_cur, 0, sizeof(v->tabr_cur));
    memset(v->taby_prev, 0, sizeof(v->taby_prev));
    memset(v->taby_cur, 0, sizeof(v->taby_cur));
}

// Circular 1-2-1 binomial passes over a scan table.
static void fluod_blur_table(float *t, int passes) {
    for (int pass = 0; pass < passes; pass++) {
        float prev = t[FLUOD_FIELD_W - 1];
        float first = t[0];
        for (int x = 0; x < FLUOD_FIELD_W; x++) {
            float next = x + 1 < FLUOD_FIELD_W ? t[x + 1] : first;
            float c = t[x];
            t[x] = 0.25f * prev + 0.5f * c + 0.25f * next;
            prev = c;
        }
    }
}

// Remove a table's DC in place; return its RMS.
static float fluod_dc_rms(float *t) {
    float mean = 0.0f;
    for (int x = 0; x < FLUOD_FIELD_W; x++) mean += t[x];
    mean /= (float)FLUOD_FIELD_W;
    float rms = 0.0f;
    for (int x = 0; x < FLUOD_FIELD_W; x++) {
        t[x] -= mean;
        rms += t[x] * t[x];
    }
    return sqrtf(rms / (float)FLUOD_FIELD_W);
}

// ── One simulation tick ──

static void fluod_tick(FluodVoice *v) {
    const FluodPhysics *ph = &v->phys;

    // Particles: sense → black box (chiral pair) → force/strafe → move →
    // deposit. Same order as entity_update.glsl's main().
    float sd = FLUOD_SENSOR_BASE * ph->sensor_distance;
    float sa = ph->sensor_angle * FLUOD_PI;
    float cs = cosf(sa), sn = sinf(sa);
    float sense_gain = ph->sensor_gain * FLUOD_SENSE_SCALE;
    float fmult = ph->global_force_mult * FLUOD_FORCE_SCALE;
    float smult = ph->global_force_mult * FLUOD_STRAFE_SCALE * ph->strafe_power;
    float dep = ph->deposit / FLUOD_TARGET_SPEED;

    for (int i = 0; i < FLUOD_PARTICLES; i++) {
        FluodParticle *e = &v->p[i];
        const FluodRule *rule = &v->cohort_rule[i % FLUOD_COHORTS];

        float speed = sqrtf(e->vx * e->vx + e->vy * e->vy);
        float fwx = speed > 1e-9f ? e->vx / speed : 1.0f;
        float fwy = speed > 1e-9f ? e->vy / speed : 0.0f;
        // left = (forward.y, -forward.x), matching pR's handedness
        float lx = fwy, ly = -fwx;

        // Sensor offsets: forward·sd rotated ±sensor_angle (pR rotation)
        float ox = fwx * sd, oy = fwy * sd;
        float lox = cs * ox + sn * oy, loy = cs * oy - sn * ox;
        float rox = cs * ox - sn * oy, roy = cs * oy + sn * ox;

        float L[2], R[2];
        fluod_field_sample(v, e->px + lox, e->py + loy, L);
        fluod_field_sample(v, e->px + rox, e->py + roy, R);
        L[0] *= sense_gain; L[1] *= sense_gain;
        R[0] *= sense_gain; R[1] *= sense_gain;

        // Decompose into the local frame: (axial, lateral)
        float La = L[0] * fwx + L[1] * fwy, Ll = L[0] * lx + L[1] * ly;
        float Ra = R[0] * fwx + R[1] * fwy, Rl = R[0] * lx + R[1] * ly;

        float base[4], mir[4];
        float bin[4] = {La, Ll, Ra, Rl};
        fluod_black_box(rule, bin, base);
        if (ph->disable_symmetry) {
            mir[0] = mir[1] = mir[2] = mir[3] = 0.0f;
        } else {
            // black_box(y_reflect(R), y_reflect(L)): swap sensors, negate
            // lateral components.
            float min_[4] = {Ra, -Rl, La, -Ll};
            fluod_black_box(rule, min_, mir);
        }

        // base + y_reflect(mirror), per Fluoddity's chiral average
        float f_ax = base[0] + mir[0], f_lat = base[1] - mir[1];
        float s_ax = base[2] + mir[2], s_lat = base[3] - mir[3];

        float fx = (fwx * f_ax * ph->axial_force + lx * f_lat * ph->lateral_force) * fmult;
        float fy = (fwy * f_ax * ph->axial_force + ly * f_lat * ph->lateral_force) * fmult;
        float sx = (fwx * s_ax * ph->axial_force + lx * s_lat * ph->lateral_force) * smult;
        float sy = (fwy * s_ax * ph->axial_force + ly * s_lat * ph->lateral_force) * smult;

        e->vx = e->vx * ph->drag + fx;
        e->vy = e->vy * ph->drag + fy;
        speed = sqrtf(e->vx * e->vx + e->vy * e->vy);
        if (speed > FLUOD_MAX_SPEED) {
            float s = FLUOD_MAX_SPEED / speed;
            e->vx *= s; e->vy *= s;
        }
        e->px = fluod_wrap01(e->px + e->vx + sx);
        e->py = fluod_wrap01(e->py + e->vy + sy);

        float trail[2] = {e->vx * dep, e->vy * dep};
        fluod_field_splat(v, e->px, e->py, trail);
    }

    // Field decay + diffusion, wall-clock compensated to visual Fluoddity.
    float tick_rate = (float)(v->sr / FLUOD_TICK_SAMPLES);
    if (tick_rate < 1.0f) tick_rate = 1.0f;
    float persist = powf(fminf(ph->trail_persistence, 0.999f),
                         FLUOD_VISUAL_FPS / tick_rate);
    float d = ph->trail_diffusion;
    if (d < 0.001f) d = 0.001f;
    if (d > 1.0f) d = 1.0f;
    d = d * d;
    float K60 = 4.0f / (powf(5.0f, d) - 1.0f);        // canvas.frag's kernel K
    float leak60 = 4.0f / (4.0f + K60);               // neighbor share per frame
    float leak = leak60 * FLUOD_VISUAL_FPS / tick_rate; // per-tick share
    if (leak > 0.8f) leak = 0.8f;
    float K = 4.0f / leak - 4.0f;
    float inv = 1.0f / (4.0f + K);
    for (int y = 0; y < FLUOD_FIELD_H; y++) {
        int yn = (y + 1) & (FLUOD_FIELD_H - 1);
        int ys = (y - 1 + FLUOD_FIELD_H) & (FLUOD_FIELD_H - 1);
        for (int x = 0; x < FLUOD_FIELD_W; x++) {
            int xe = (x + 1) & (FLUOD_FIELD_W - 1);
            int xw = (x - 1 + FLUOD_FIELD_W) & (FLUOD_FIELD_W - 1);
            for (int k = 0; k < 2; k++) {
                float c = v->field[y][x][k];
                float nb = v->field[yn][x][k] + v->field[ys][x][k] +
                           v->field[y][xe][k] + v->field[y][xw][k];
                v->swap[y][x][k] = (c * K + nb) * inv * persist;
            }
        }
    }
    memcpy(v->field, v->swap, sizeof(v->field));

    // Scan tables: column-summed flow, DC removed; the audio scanner
    // crossfades prev→cur across the tick interval. Three flavors — the
    // blurred x-scan is the tone's body, the raw x-scan is the bright
    // attack/agitation layer, the y-scan is the stereo side layer.
    memcpy(v->tab_prev, v->tab_cur, sizeof(v->tab_prev));
    memcpy(v->tabr_prev, v->tabr_cur, sizeof(v->tabr_prev));
    memcpy(v->taby_prev, v->taby_cur, sizeof(v->taby_prev));
    for (int x = 0; x < FLUOD_FIELD_W; x++) {
        float sx = 0.0f, sy = 0.0f;
        for (int y = 0; y < FLUOD_FIELD_H; y++) {
            sx += v->field[y][x][0];
            sy += v->field[y][x][1];
        }
        v->tabr_cur[x] = sx;
        v->tab_cur[x] = sx;
        v->taby_cur[x] = sy;
    }
    // Tone: each circular 1-2-1 pass rolls off high spatial harmonics
    // (splats are spatial impulses — raw, they scan buzzy on high notes).
    // The raw table keeps them: that fizz is the attack.
    int blur = ph->tap_blur;
    if (blur > 6) blur = 6;
    fluod_blur_table(v->tab_cur, blur);
    fluod_blur_table(v->taby_cur, blur);
    float rms = fluod_dc_rms(v->tab_cur);
    float rms_raw = fluod_dc_rms(v->tabr_cur);
    float rms_y = fluod_dc_rms(v->taby_cur);
    // Level-match the layers against the body so their MIX ratios (not the
    // field's arbitrary magnitudes) decide the sound.
    v->raw_gain = fminf(2.0f, rms / (rms_raw + 1e-9f));
    v->side_gain = fminf(2.0f, rms / (rms_y + 1e-9f));

    // Swarm statistics for the render thread: mean x-drift micro-bends the
    // scan pitch (the swarm literally sings a few cents sharp or flat as it
    // travels), mean speed vs cruise is "agitation" and keeps some bright
    // layer in the sustain of a busy ecosystem.
    float mvx = 0.0f, mspd = 0.0f;
    for (int i = 0; i < FLUOD_PARTICLES; i++) {
        mvx += v->p[i].vx;
        mspd += sqrtf(v->p[i].vx * v->p[i].vx + v->p[i].vy * v->p[i].vy);
    }
    v->drift = mvx / (float)FLUOD_PARTICLES;
    v->agitation = mspd / (float)FLUOD_PARTICLES / FLUOD_TARGET_SPEED;

    // Partial normalization reference: loud ecosystems are tamed, quiet
    // ones fade naturally — but FLUOD_AGC_EXP < 1 keeps their swells real.
    v->agc_rms = (double)rms;

    // NaN/divergence trap: one poisoned cell would spread through diffusion
    // and the scanner. Re-scatter the swarm, keep the genome.
    if (!isfinite(rms) || !isfinite(v->p[0].px)) fluod_scatter(v);
}

// ── Lifecycle ──

int fluod_voice_init_rule(FluodVoice *v, const FluodRule *genome,
                          uint32_t seed, double freq, double sample_rate) {
    (void)freq;
    memset(v, 0, sizeof(*v));
    v->genome = *genome;
    fluod_physics_default(&v->phys);
    v->rng = seed ? seed : 0x9E3779B9u;
    v->sr = sample_rate > 0 ? sample_rate : 48000.0;
    for (int c = 0; c < FLUOD_COHORTS; c++) {
        v->cohort_rule[c] = v->genome;
        fluod_rule_mutate(&v->cohort_rule[c], v->phys.mutation_scale,
                          seed + (uint32_t)c * 0x9E37u);
    }
    fluod_scatter(v);
    for (int t = 0; t < FLUOD_WARMUP_TICKS; t++) fluod_tick(v);
    return 0;
}

int fluod_voice_init(FluodVoice *v, uint32_t seed, double freq,
                     double sample_rate) {
    FluodRule r;
    fluod_rule_from_seed(&r, seed);
    return fluod_voice_init_rule(v, &r, seed ^ 0xA5A5A5A5u, freq, sample_rate);
}

// ── Render ──

// Spatial (x0/x1, tx) + temporal (tf) interpolated read of a prev/cur pair.
static inline float fluod_scan(const float *tp, const float *tc,
                               int x0, int x1, float tx, float tf) {
    float cur = tc[x0] * (1 - tx) + tc[x1] * tx;
    float prev = tp[x0] * (1 - tx) + tp[x1] * tx;
    return prev * (1 - tf) + cur * tf;
}

void fluod_voice_render_stereo(FluodVoice *v, double sample_rate, double env,
                               double frequency, float *out_l, float *out_r) {
    *out_l = 0.0f;
    *out_r = 0.0f;
    if (sample_rate <= 0) return;
    v->sr = sample_rate;
    v->age += 1.0 / sample_rate;
    v->tick_phase++;
    if (v->tick_phase >= FLUOD_TICK_SAMPLES) {
        fluod_tick(v);
        v->tick_phase = 0;
    }

    // Swarm vibrato: the ecosystem's net drift along the scan axis bends
    // pitch by up to ±FLUOD_VIB_CENTS. Smoothed ~60 ms so per-tick jumps in
    // the mean become a slow organic wobble, not FM hash.
    double drift_n = v->drift / FLUOD_TARGET_SPEED;
    if (drift_n > 1.0) drift_n = 1.0;
    if (drift_n < -1.0) drift_n = -1.0;
    v->drift_smooth += (drift_n - v->drift_smooth) *
                       (1.0 - exp(-1.0 / (sample_rate * 0.06)));
    double vib = 1.0 + v->drift_smooth * (FLUOD_VIB_CENTS / 1200.0) * 0.6931472;

    v->phase += frequency * vib / sample_rate;
    v->phase -= floor(v->phase);

    float fx = (float)v->phase * FLUOD_FIELD_W;
    int x0 = (int)fx;
    float tx = fx - (float)x0;
    if (x0 >= FLUOD_FIELD_W) x0 = 0;
    int x1 = (x0 + 1) & (FLUOD_FIELD_W - 1);
    float tf = (float)v->tick_phase / (float)FLUOD_TICK_SAMPLES;

    float body = fluod_scan(v->tab_prev, v->tab_cur, x0, x1, tx, tf);
    float bright = fluod_scan(v->tabr_prev, v->tabr_cur, x0, x1, tx, tf)
                   * v->raw_gain;
    float side = fluod_scan(v->taby_prev, v->taby_cur, x0, x1, tx, tf)
                 * v->side_gain * FLUOD_SIDE_WIDTH;

    // Spectral envelope over time: hard bright layer at note birth decaying
    // fast (a pluck/breath transient), plus a sustained remainder that
    // tracks how agitated the swarm is — busy ecosystems shimmer, calm ones
    // mellow. This is what separates an instrument from an organ patch.
    double agit = (double)v->agitation - 0.8;
    if (agit < 0) agit = 0;
    if (agit > 1.5) agit = 1.5;
    v->bright_smooth += (agit - v->bright_smooth) *
                        (1.0 - exp(-1.0 / (sample_rate * 0.08)));
    float bmix = FLUOD_BRIGHT_ATTACK * (float)exp(-v->age / FLUOD_BRIGHT_TAU)
                 + FLUOD_BRIGHT_AGIT * (float)v->bright_smooth;
    if (bmix > 0.9f) bmix = 0.9f;
    float mid = body + bright * bmix;

    // Partial normalization, eased slowly enough that the ecosystem's own
    // 1–8 Hz amplitude life survives. First-call state 0 = short fade-in.
    double target = pow(FLUOD_OUT_TARGET / (FLUOD_AGC_FLOOR + v->agc_rms),
                        FLUOD_AGC_EXP);
    v->gain_smooth += (target - v->gain_smooth) *
                      (1.0 - exp(-1.0 / (sample_rate * FLUOD_AGC_SMOOTH_S)));
    float g = (float)(v->gain_smooth * env);
    float l = tanhf((mid + side) * g);
    float r = tanhf((mid - side) * g);
    if (!isfinite(l) || !isfinite(r)) {
        fluod_scatter(v);
        return;
    }
    *out_l = l;
    *out_r = r;
}

float fluod_voice_render(FluodVoice *v, double sample_rate, double env,
                         double frequency) {
    float l, r;
    fluod_voice_render_stereo(v, sample_rate, env, frequency, &l, &r);
    return 0.5f * (l + r);
}
