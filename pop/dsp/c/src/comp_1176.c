#include "comp_1176.h"
#include "util.h"
#include <math.h>
#include <string.h>

float comp_1176_knob_to_attack_us(float knob) {
  // knob 1 → 800 μs, knob 7 → 20 μs (exponential).
  float t = (knob - 1.0f) / 6.0f;
  if (t < 0.0f) t = 0.0f; else if (t > 1.0f) t = 1.0f;
  return 800.0f * powf(20.0f / 800.0f, t);
}

float comp_1176_knob_to_release_ms(float knob) {
  // knob 1 → 1100 ms, knob 7 → 50 ms (exponential).
  float t = (knob - 1.0f) / 6.0f;
  if (t < 0.0f) t = 0.0f; else if (t > 1.0f) t = 1.0f;
  return 1100.0f * powf(50.0f / 1100.0f, t);
}

void comp_1176_init(comp_1176_t *c, float sr) {
  memset(c, 0, sizeof(*c));
  c->sr           = sr;
  c->input_db     = 0.0f;
  c->output_db    = 0.0f;
  c->ratio        = 4.0f;
  c->attack_us    = comp_1176_knob_to_attack_us(4.0f);   // ~127 μs
  c->release_ms   = comp_1176_knob_to_release_ms(4.0f);  // ~234 ms
  c->iron         = 0.5f;
  c->threshold_db = -10.0f;
  c->knee_db      = 6.0f;
  c->gr_db        = 0.0f;
  c->gr_peak_db   = 0.0f;
  comp_1176_update(c);
}

void comp_1176_update(comp_1176_t *c) {
  // Single-pole smoothing: y[n] = target + coef*(y[n-1] - target)
  // Time constant τ → coef = exp(-1 / (τ * sr)). Smaller τ ⇒ smaller coef ⇒ faster.
  float a_tau = c->attack_us  * 1e-6f;
  float r_tau = c->release_ms * 1e-3f;
  c->a_coef = expf(-1.0f / (a_tau * c->sr));
  c->r_coef = expf(-1.0f / (r_tau * c->sr));
}

// Soft-knee static curve (RBJ-style quadratic).
// Returns the *gain reduction* in dB (≤ 0) the curve would apply to a
// signal currently at `in_db`.
static inline float static_curve_db(float in_db, float th, float ratio, float knee) {
  float over  = in_db - th;
  float slope = 1.0f - 1.0f / ratio;
  float half  = knee * 0.5f;
  if (over <= -half) return 0.0f;
  if (over >=  half) return -over * slope;
  float t    = (over + half) / knee;            // 0..1 across the knee
  float full = -half * slope;                   // GR at top of knee
  return full * t * t;
}

// FET asymmetric saturation. Drive bias toward positive → 2nd-harmonic
// dominance, the audible FET fingerprint. `drive` modulates depth.
static inline float fet_sat(float x, float drive) {
  if (drive <= 0.0f) return x;
  float bias = 0.05f * drive;
  float k    = 1.0f + 4.0f * drive;
  float y    = tanhf((x + bias) * k) - tanhf(bias * k);
  return y / k;
}

// A12 line-amp gentle symmetric tanh (the post-FET stage adds warmth).
static inline float a12_sat(float x, float drive) {
  if (drive <= 0.0f) return x;
  float k = 1.0f + 0.5f * drive;
  return tanhf(x * k) / k;
}

void comp_1176_process(comp_1176_t *c, float *buf, int n_frames, int channels) {
  float in_lin  = acdsp_db2lin(c->input_db);
  float out_lin = acdsp_db2lin(c->output_db);
  float fet_d_base = 0.6f * c->iron;
  float a12_d      = 0.3f * c->iron;

  for (int i = 0; i < n_frames; i++) {
    float L = buf[i * channels] * in_lin;
    float R = (channels > 1) ? buf[i * channels + 1] * in_lin : L;

    // Stereo-linked peak detector (max abs across both channels).
    float det = fabsf(L);
    if (channels > 1) { float ar = fabsf(R); if (ar > det) det = ar; }
    float det_db = acdsp_lin2db(det);
    float target = static_curve_db(det_db, c->threshold_db, c->ratio, c->knee_db);

    // Attack when target wants MORE reduction (more negative); release otherwise.
    float coef = (target < c->gr_db) ? c->a_coef : c->r_coef;
    c->gr_db = target + coef * (c->gr_db - target);
    if (c->gr_db < c->gr_peak_db) c->gr_peak_db = c->gr_db;

    float g = acdsp_db2lin(c->gr_db);
    L *= g;
    if (channels > 1) R *= g;

    // FET drive scales with current GR amount (the physical FET conducts
    // harder ⇒ more nonlinearity). 0 GR ⇒ 30% of base; 18 dB GR ⇒ 100%.
    float gr_amt = fminf(-c->gr_db, 18.0f) / 18.0f;
    float fd     = fet_d_base * (0.3f + 0.7f * gr_amt);
    L = fet_sat(L, fd);
    if (channels > 1) R = fet_sat(R, fd);

    L = a12_sat(L, a12_d);
    if (channels > 1) R = a12_sat(R, a12_d);

    L *= out_lin;
    if (channels > 1) R *= out_lin;

    buf[i * channels] = L;
    if (channels > 1) buf[i * channels + 1] = R;
  }
}
