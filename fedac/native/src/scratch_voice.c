// scratch_voice.c — see scratch_voice.h for what this is and why it is
// dependency-free.
//
// The sound is two low-passed copies of ONE noise source subtracted from each
// other — a band whose centre and width follow the material under the finger —
// pushed through a tanh so the grip bites rather than hisses, plus a sine
// "head" carrier that the friction itself frequency-modulates a little.
//
// That last detail is the one worth not losing in a rewrite. Without it this
// is a filtered noise sweep and sounds like one. With it the head wobbles
// because the rubbing is uneven, which is why the gesture reads as a finger
// dragging on a skin rather than a knob being turned.

#include "scratch_voice.h"

#include <math.h>
#include <string.h>

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

static inline uint32_t scratch_xorshift(uint32_t *s) {
    uint32_t x = *s ? *s : 0x51a7c4d3u;
    x ^= x << 13;
    x ^= x >> 17;
    x ^= x << 5;
    return *s = x;
}

void scratch_voice_init(ScratchVoice *v) {
    if (!v) return;
    memset(v, 0, sizeof *v);
    v->seed = 0x51a7c4d3u;      // same stream Menu Band starts from
    v->p.cutoff = 1600.0;
    v->p.resonance = 150.0;
    v->p.roughness = 0.5;
    v->p.release = 0.014;
}

void scratch_voice_set(ScratchVoice *v, const ScratchParams *p) {
    if (!v || !p) return;
    v->p = *p;
    if (v->p.target < 0.0) v->p.target = 0.0;
    if (v->p.roughness < 0.0) v->p.roughness = 0.0;
    if (v->p.roughness > 1.0) v->p.roughness = 1.0;
    if (v->p.release <= 0.0) v->p.release = 0.014;
    if (v->p.pan < -1.0) v->p.pan = -1.0;
    if (v->p.pan > 1.0) v->p.pan = 1.0;
    if (v->seed == 0) v->seed = 0x51a7c4d3u;
}

void scratch_voice_stop(ScratchVoice *v) {
    if (!v) return;
    v->p.target = 0.0;          // the release ramp does the rest
}

int scratch_voice_active(const ScratchVoice *v) {
    if (!v) return 0;
    return (v->p.target > 0.0) || (v->level >= 1e-5);
}

void scratch_voice_render(ScratchVoice *v, double sample_rate,
                          double *out_l, double *out_r) {
    if (!v || sample_rate <= 0.0) return;

    double attack_s = v->p.synthetic ? 0.006 : 0.0025;
    double attack_a = 1.0 - exp(-1.0 / (sample_rate * attack_s));
    double release_a = 1.0 - exp(-1.0 / (sample_rate * v->p.release));

    double cutoff = v->p.cutoff;
    if (cutoff < 20.0) cutoff = 20.0;
    if (cutoff > sample_rate * 0.45) cutoff = sample_rate * 0.45;
    double slow_hz = cutoff * 0.18;
    if (slow_hz < 35.0) slow_hz = 35.0;
    double filter_a = 1.0 - exp(-2.0 * M_PI * cutoff / sample_rate);
    double slow_a = 1.0 - exp(-2.0 * M_PI * slow_hz / sample_rate);

    double smoothing = (v->p.target > v->level) ? attack_a : release_a;
    v->level += smoothing * (v->p.target - v->level);

    double white = ((double)scratch_xorshift(&v->seed) / (double)UINT32_MAX) * 2.0 - 1.0;
    v->noise += filter_a * (white - v->noise);
    v->slow_noise += slow_a * (white - v->slow_noise);
    double friction = v->noise - v->slow_noise;

    // Physical surface only: the friction nudges the head's pitch. The electro
    // surface keeps a steady carrier because its character is the ring
    // modulation, not the grip.
    double pitch_motion = v->p.synthetic ? 1.0
                                         : 1.0 + tanh(friction * 8.0) * 0.055;
    double resonance = v->p.resonance;
    if (resonance < 0.0) resonance = 0.0;
    if (resonance > sample_rate * 0.45) resonance = sample_rate * 0.45;
    v->phase += resonance * pitch_motion / sample_rate;
    if (v->phase >= 1.0) v->phase -= floor(v->phase);
    double carrier = sin(2.0 * M_PI * v->phase);

    double texture;
    if (v->p.synthetic) {
        texture = v->noise * carrier * 1.35;
    } else {
        double gnarl = tanh(friction * (5.0 + v->p.roughness * 5.0));
        texture = gnarl * 0.44
                  + carrier * (0.08 + fabs(gnarl)
                               * (0.42 + v->p.roughness * 0.30));
    }

    double s = texture * v->level;
    double theta = (v->p.pan + 1.0) * 0.25 * M_PI;
    if (out_l) *out_l = s * cos(theta);
    if (out_r) *out_r = s * sin(theta);
}
