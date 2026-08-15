// fluoddity_voice.h — Standalone, dependency-free "Fluoddity" scanned-swarm voice.
//
// An audio port of aphid91/Fluoddity (github.com/aphid91/Fluoddity), the
// generalized physarum transport model after Sage Jenson's physarum page.
// Fluoddity has no audio of its own — this module carries its *algorithm*
// into a playable instrument:
//
//   · Each note owns a small toroidal trail field (a 2-component flow field,
//     FLUOD_FIELD_W × FLUOD_FIELD_H) and a swarm of particles that sense the
//     field through two rotated sensors, act through an 80-parameter
//     sum-of-sines "Rule" (10 Fourier centers × 4D frequency + 4D amplitude —
//     the exact black_box of Fluoddity's entity_update.glsl, including the
//     chiral mirror-average), deposit velocity trails, and ride drag/strafe.
//   · The field decays and diffuses each simulation tick with Fluoddity's
//     (center·K + 4 neighbors)/(4+K) kernel and persistence multiply.
//   · Audio comes out by SCANNED SYNTHESIS (Mathews/Verplank): the X axis of
//     the field is a wavetable — each column of flow is summed, DC-removed,
//     and an audio-rate scanner reads the table at the note fundamental.
//     Pitch is exact and harmonic by construction; the timbre is whatever the
//     ecosystem is currently doing. The instrument is *alive* while held.
//
// The Rule is the instrument: 80 floats, generated from a seed, mutated per
// cohort (and per note-on if the host wants lineage evolution). It is
// layout-compatible with the `rule` array in Fluoddity's saved configs, so a
// visual Fluoddity patch can be loaded directly as a timbre.
//
// API and threading contract mirror gm_synth.h so MenuBandGMSynth's
// AVAudioSourceNode pattern hosts this with no new plumbing: init is called
// from the render thread (cheap, no allocation — all state inline in
// FluodVoice), render is one sample per call with host-supplied envelope and
// (glidable) frequency.

#ifndef FLUODDITY_VOICE_H
#define FLUODDITY_VOICE_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

// ── Compile-time sizes ──
#define FLUOD_CENTERS 10   // Fourier centers per Rule (10 × 8 = 80 params)
#define FLUOD_FIELD_W 128  // trail-field columns == wavetable length
#define FLUOD_FIELD_H 16   // trail-field rows (the transverse dimension)
#define FLUOD_PARTICLES 40 // swarm size per voice
#define FLUOD_COHORTS 4    // rule-mutation cohorts (particle i → cohort i%4)
#define FLUOD_TICK_SAMPLES 48 // sim tick every N rendered samples (1 kHz @ 48k)

// One Fourier feature center: 4D input frequency + 4D output amplitude.
typedef struct {
    float freq[4];
    float amp[4];
} FluodCenter;

// The genome. Flat-float layout matches Fluoddity's saved `rule` arrays:
// center 0 freq xyzw, center 0 amp xyzw, center 1 freq xyzw, …
typedef struct {
    FluodCenter centers[FLUOD_CENTERS];
} FluodRule;

typedef struct {
    float px, py; // torus position, [0,1)²  (px is the scan/wavetable axis)
    float vx, vy; // velocity, torus units per tick
} FluodParticle;

// The Fluoddity physics sliders, same names and (where meaningful) the same
// default values as physics_configs/Core/_Default.json — these are the knobs
// a "dynamic instrument" UI would expose.
typedef struct {
    float axial_force;       // force/strafe gain along heading   (default 0.371)
    float lateral_force;     // force/strafe gain across heading  (default -0.707)
    float sensor_gain;       // sensed-flow input gain            (default 1.0)
    float sensor_angle;      // sensor rotation, ×π               (default 0.45)
    float sensor_distance;   // sensor reach multiplier           (default 1.0)
    float global_force_mult; // overall output scale              (default 0.399)
    float drag;              // velocity retained per tick        (default 0.504)
    float strafe_power;      // position-hop gain                 (default 0.224)
    float mutation_scale;    // per-cohort rule mutation          (default 0.137)
    float trail_persistence; // field kept per tick               (default 0.938)
    float trail_diffusion;   // blur slider 0..1                  (default 1.0)
    float deposit;           // trail splat gain (audio-side knob, default 1.0)
    int tap_blur;            // audio-side tone: 1-2-1 blur passes over the
                             // scan table, 0 (raw/buzzy) … 6 (dark). Default 2.
    int disable_symmetry;    // 1 = skip the chiral mirror term   (default 0)
} FluodPhysics;

typedef struct {
    FluodRule genome;                     // the instrument
    FluodRule cohort_rule[FLUOD_COHORTS]; // genome + per-cohort mutation
    FluodPhysics phys;
    FluodParticle p[FLUOD_PARTICLES];
    float field[FLUOD_FIELD_H][FLUOD_FIELD_W][2]; // flow (x,y) per cell
    float swap[FLUOD_FIELD_H][FLUOD_FIELD_W][2];  // diffusion scratch
    // Column-summed, DC-removed scan tables; audio interpolates cur↔prev
    // across the tick interval so table evolution never steps audibly.
    float tab_prev[FLUOD_FIELD_W];
    float tab_cur[FLUOD_FIELD_W];
    double phase;      // scanner phase, 0..1
    int tick_phase;    // samples rendered since last sim tick
    uint32_t rng;      // xorshift state (splat jitter, init scatter)
    double agc_rms;    // table RMS measured each tick (normalization target)
    double gain_smooth; // per-sample smoothed normalization gain
    double sr;         // sample rate at init (render recomputes per call)
} FluodVoice;

// ── Rule (genome) utilities — control thread OK, pure functions ──
void fluod_rule_from_seed(FluodRule *r, uint32_t seed);
// Fluoddity's mutate_rule: amplitude += amount·U(-1,1)⁴, frequency ×= 1 +
// 0.5·amount·U(-.5,.5). Deterministic in (rule contents, seed).
void fluod_rule_mutate(FluodRule *r, float amount, uint32_t seed);
// Interop with Fluoddity saved configs: 80 floats, center-major.
void fluod_rule_from_floats(FluodRule *r, const float flat[80]);
void fluod_rule_to_floats(const FluodRule *r, float flat[80]);

void fluod_physics_default(FluodPhysics *p);

// ── Voice lifecycle (render thread; no allocation, memsets the struct) ──
// Seeded init: genome from seed, cohorts mutated by phys defaults.
int fluod_voice_init(FluodVoice *v, uint32_t seed, double freq,
                     double sample_rate);
// Explicit-genome init (preset / lineage play). `seed` still drives the
// swarm scatter and cohort mutations.
int fluod_voice_init_rule(FluodVoice *v, const FluodRule *genome,
                          uint32_t seed, double freq, double sample_rate);
// One output sample. `env` is the host's amplitude envelope (0..1);
// `frequency` may glide per sample like gm_voice_render's. Output is
// AGC-normalized and soft-clipped; NaN/Inf is trapped internally (the
// swarm re-scatters rather than poisoning the mix).
float fluod_voice_render(FluodVoice *v, double sample_rate, double env,
                         double frequency);

#ifdef __cplusplus
}
#endif

#endif // FLUODDITY_VOICE_H
