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
#define FLUOD_OUT_TARGET 0.50f    // normalized output RMS target
#define FLUOD_AGC_FLOOR 0.02f     // table RMS below this fades toward silence

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

    // Scan table: column-summed x-flow, DC removed. The audio scanner
    // crossfades tab_prev→tab_cur across the tick interval.
    memcpy(v->tab_prev, v->tab_cur, sizeof(v->tab_prev));
    for (int x = 0; x < FLUOD_FIELD_W; x++) {
        float s = 0.0f;
        for (int y = 0; y < FLUOD_FIELD_H; y++) s += v->field[y][x][0];
        v->tab_cur[x] = s;
    }
    // Tone: each circular 1-2-1 pass rolls off the table's high spatial
    // harmonics (the splats are spatial impulses — raw, they scan buzzy and
    // alias on high notes).
    int blur = ph->tap_blur;
    if (blur > 6) blur = 6;
    for (int pass = 0; pass < blur; pass++) {
        float prev = v->tab_cur[FLUOD_FIELD_W - 1];
        float first = v->tab_cur[0];
        for (int x = 0; x < FLUOD_FIELD_W; x++) {
            float next = x + 1 < FLUOD_FIELD_W ? v->tab_cur[x + 1] : first;
            float c = v->tab_cur[x];
            v->tab_cur[x] = 0.25f * prev + 0.5f * c + 0.25f * next;
            prev = c;
        }
    }
    float mean = 0.0f;
    for (int x = 0; x < FLUOD_FIELD_W; x++) mean += v->tab_cur[x];
    mean /= (float)FLUOD_FIELD_W;
    float rms = 0.0f;
    for (int x = 0; x < FLUOD_FIELD_W; x++) {
        v->tab_cur[x] -= mean;
        rms += v->tab_cur[x] * v->tab_cur[x];
    }
    rms = sqrtf(rms / (float)FLUOD_FIELD_W);

    // Saturating normalization: loud ecosystems are leveled, near-silent
    // ones fade naturally instead of being boosted into noise.
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

float fluod_voice_render(FluodVoice *v, double sample_rate, double env,
                         double frequency) {
    if (sample_rate <= 0) return 0.0f;
    v->sr = sample_rate;
    v->tick_phase++;
    if (v->tick_phase >= FLUOD_TICK_SAMPLES) {
        fluod_tick(v);
        v->tick_phase = 0;
    }

    v->phase += frequency / sample_rate;
    v->phase -= floor(v->phase);

    float fx = (float)v->phase * FLUOD_FIELD_W;
    int x0 = (int)fx;
    float tx = fx - (float)x0;
    if (x0 >= FLUOD_FIELD_W) x0 = 0;
    int x1 = (x0 + 1) & (FLUOD_FIELD_W - 1);
    float cur = v->tab_cur[x0] * (1 - tx) + v->tab_cur[x1] * tx;
    float prev = v->tab_prev[x0] * (1 - tx) + v->tab_prev[x1] * tx;
    float tf = (float)v->tick_phase / (float)FLUOD_TICK_SAMPLES;
    float raw = prev * (1 - tf) + cur * tf;

    // Saturating normalization, eased with a ~30 ms one-pole so the leveling
    // never steps at tick rate. First-call state 0 just means a short fade-in.
    double target = FLUOD_OUT_TARGET / (FLUOD_AGC_FLOOR + v->agc_rms);
    v->gain_smooth += (target - v->gain_smooth) *
                      (1.0 - exp(-1.0 / (sample_rate * 0.03)));
    float out = tanhf(raw * (float)v->gain_smooth) * (float)env;
    if (!isfinite(out)) {
        fluod_scatter(v);
        return 0.0f;
    }
    return out;
}
