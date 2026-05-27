// 1176 — FET compressor model.
//
// Physically-shaped (not generic VCA): per-sample peak detector with
// sub-millisecond attack, FET asymmetric saturation whose depth scales
// with the instantaneous gain-reduction amount (the FET is driven harder
// the more it conducts), gentle A12-style line-amp sat on the way out.
// Threshold is internal (-10 dB, soft-knee 6 dB); the user gets the same
// "input/output" knobs as the original — push more signal at the FET to
// get more reduction, then make up at the output.
//
// Knob ranges mirror the unit:
//   attack_us   : 20..800   (real 1176 sweep, exponential by attack knob 7..1)
//   release_ms  : 50..1100  (release knob 7..1)
//   ratio       : 4 / 8 / 12 / 20
//   iron        : 0..2, depth of FET + A12 nonlinearity (default 0.5).

#ifndef ACDSP_COMP_1176_H
#define ACDSP_COMP_1176_H

typedef struct {
  float input_db, output_db;
  float ratio;
  float attack_us, release_ms;
  float iron;
  float sr;
  float threshold_db, knee_db;
  float a_coef, r_coef;
  float gr_db;
  float gr_peak_db;       // most-negative GR seen since last reset (telemetry)
} comp_1176_t;

void comp_1176_init   (comp_1176_t *c, float sr);
void comp_1176_update (comp_1176_t *c);  // call after changing knobs
void comp_1176_process(comp_1176_t *c, float *buf, int n_frames, int channels);

// Knob 1..7 mapped to time, matching the unit's exponential sweep.
float comp_1176_knob_to_attack_us (float knob);
float comp_1176_knob_to_release_ms(float knob);

#endif
