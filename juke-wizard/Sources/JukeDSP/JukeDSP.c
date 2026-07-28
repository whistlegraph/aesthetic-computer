#include "JukeDSP.h"
#include <math.h>
#include <string.h>

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif
#define TAU (2.0 * M_PI)

static float soft(float x) { return tanhf(x * 1.12f); }

static uint32_t hash32(uint32_t x) {
    x ^= x >> 16; x *= 0x7feb352du;
    x ^= x >> 15; x *= 0x846ca68bu;
    return x ^ (x >> 16);
}

static float hash_bipolar(uint32_t x) {
    return ((float)(hash32(x) & 0xffffu) / 32767.5f) - 1.0f;
}

void ac_scratch_init(ACScratchState *state) {
    memset(state, 0, sizeof(*state));
    state->velocity = 1.0;
}

double ac_scratch_motion(ACScratchState *state, double hand_velocity,
                         double position_error, int scratching,
                         double sample_rate) {
    // A light platter has fast attack but retains enough inertia for a throw.
    const double follow = scratching ? 0.075 : 0.018;
    state->velocity += (hand_velocity - state->velocity) * follow;
    if (!scratching) return state->velocity;

    // The stylus follows the hand through a bounded spring instead of jumping
    // to every pointer event. This is the click-prevention layer.
    const double throw_limit = fmax(2.0, fabs(state->velocity) * 2.5);
    double spring = tanh(position_error / fmax(1.0, sample_rate * 0.012)) * throw_limit;
    return state->velocity + spring;
}

float ac_scratch_cubic(float xm1, float x0, float x1, float x2, float t) {
    // Catmull-Rom: local, reversible, and continuous through sample boundaries.
    float a = -0.5f*xm1 + 1.5f*x0 - 1.5f*x1 + 0.5f*x2;
    float b = xm1 - 2.5f*x0 + 2.0f*x1 - 0.5f*x2;
    float c = -0.5f*xm1 + 0.5f*x1;
    return ((a*t + b)*t + c)*t + x0;
}

static float pixel_groove(double position, uint32_t channel) {
    // The noise coordinate travels with the record. Crossing a cell in reverse
    // returns the identical grain—the tactile repeatability unique to digital.
    const double cell_position = position / 24.0;
    const long long cell = (long long)floor(cell_position);
    float t = (float)(cell_position - floor(cell_position));
    t = t * t * (3.0f - 2.0f * t);
    uint32_t akey = (uint32_t)cell ^ (channel * 0x9e3779b9u);
    uint32_t bkey = (uint32_t)(cell + 1) ^ (channel * 0x9e3779b9u);
    return hash_bipolar(akey) + (hash_bipolar(bkey) - hash_bipolar(akey)) * t;
}

float ac_scratch_material(ACScratchState *state, float sample, int channel,
                          double sample_position, double motion, int scratching) {
    int c = channel & 1;
    float x = sample;
    if (scratching) {
        float grain = pixel_groove(sample_position, (uint32_t)c);
        float grain_gain = (float)fmin(0.017, 0.0025 + fabs(motion) * 0.0014);
        state->body[c] += (x - state->body[c]) * 0.12f;
        float edge = x - state->body[c];
        x = soft(x + edge * 0.18f + grain * grain_gain);
        state->output[c] += (x - state->output[c]) * 0.70f;
    } else {
        state->body[c] = x;
        state->output[c] = x;
    }
    return state->output[c];
}

void ac_practice_render(int variant, float *left, float *right, size_t frames,
                        double sample_rate, double bpm) {
    const double beat = 60.0 / bpm;
    float previous_noise = 0.0f;
    for (size_t i = 0; i < frames; i++) {
        double t = (double)i / sample_rate;
        long beat_index = (long)floor(t / beat);
        double beat_time = t - beat_index * beat;
        long half_index = (long)floor(t / (beat * 0.5));
        double half_time = t - half_index * beat * 0.5;

        // minitek's pitch-enveloped sine kick, expressed analytically so its
        // phase remains sample-exact without an event allocator.
        double kick_phase = TAU * (48.0 * beat_time
            + 72.0 * (1.0 - exp(-42.0 * beat_time)) / 42.0);
        double kick = tanh((sin(kick_phase) + exp(-beat_time * 360.0) * 0.7) * 1.9)
            * exp(-beat_time * 8.5);

        uint32_t key = (uint32_t)i ^ (variant ? 0x57415645u : 0x48415453u);
        float noise = hash_bipolar(key);
        float high = noise - previous_noise;
        previous_noise = noise;
        double hat_decay = (half_index % 4 == 3) ? 42.0 : 130.0;
        double hat = high * exp(-half_time * hat_decay) * 0.20;

        int beat_in_bar = (int)(beat_index & 3);
        double clap = 0.0;
        if ((beat_in_bar == 1 || beat_in_bar == 3) && beat_time < 0.20) {
            double spits = beat_time < 0.028 ? exp(-fmod(beat_time, 0.010) * 600.0) : 0.0;
            clap = high * (spits * 0.8 + exp(-beat_time * 16.0)) * 0.22;
        }

        double sidechain = 0.38 + 0.62 * fmin(1.0, beat_time / 0.16);
        double wave;
        if (variant == 0) {
            double note = (beat_index % 4 == 3) ? 65.406 : 55.0;
            wave = tanh((sin(TAU * note * t) + 0.14 * sin(TAU * note * 2.0 * t)) * 1.25)
                * 0.18 * sidechain;
        } else {
            static const double notes[8] = {55.0, 65.406, 73.416, 82.407, 73.416, 65.406, 49.0, 55.0};
            double hz = notes[half_index & 7];
            double phase = fmod(hz * t, 1.0);
            double saw = phase * 2.0 - 1.0;
            wave = tanh((saw + 0.35 * sin(TAU * hz * t)) * 1.1) * 0.13 * sidechain;
        }

        double drums = (variant == 0 ? kick * 0.72 : kick * 0.58) + hat + (variant ? clap : 0.0);
        double pan = (half_index & 1) ? 0.16 : -0.16;
        left[i] = soft((float)(drums * (1.0 - pan * 0.5) + wave));
        right[i] = soft((float)(drums * (1.0 + pan * 0.5) + wave));
    }
}
