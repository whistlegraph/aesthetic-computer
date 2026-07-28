#ifndef JUKE_DSP_H
#define JUKE_DSP_H

#include <stddef.h>
#include <stdint.h>

typedef struct {
    double velocity;
    float body[2];
    float output[2];
} ACScratchState;

void ac_scratch_init(ACScratchState *state);

// Hand inertia + stylus compliance. position_error is measured in samples.
double ac_scratch_motion(ACScratchState *state, double hand_velocity,
                         double position_error, int scratching,
                         double sample_rate);

// Four-point interpolation avoids the brittle edge of linear resampling.
float ac_scratch_cubic(float xm1, float x0, float x1, float x2, float fraction);

// "Pixel groove": reversible coordinate-bound grain, hysteresis, and a soft
// output slew. Texture belongs to the record position rather than wall time.
float ac_scratch_material(ACScratchState *state, float sample, int channel,
                          double sample_position, double motion, int scratching);

// Two deterministic practice loops: sine kick/hats and waves/claps.
void ac_practice_render(int variant, float *left, float *right, size_t frames,
                        double sample_rate, double bpm);

#endif
