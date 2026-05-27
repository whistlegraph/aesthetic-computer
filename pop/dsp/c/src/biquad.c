#include "biquad.h"
#include "util.h"
#include <math.h>
#include <string.h>

void biquad_clear(biquad_t *bq) { bq->z1 = bq->z2 = 0.0f; }

static void set_coeffs(biquad_t *bq, float b0, float b1, float b2,
                       float a0, float a1, float a2) {
  float inv = 1.0f / a0;
  bq->b0 = b0 * inv;
  bq->b1 = b1 * inv;
  bq->b2 = b2 * inv;
  bq->a1 = a1 * inv;
  bq->a2 = a2 * inv;
}

void biquad_peak(biquad_t *bq, float f0, float Q, float gain_db, float sr) {
  float A    = powf(10.0f, gain_db / 40.0f);
  float w0   = 2.0f * ACDSP_PI * f0 / sr;
  float cosw = cosf(w0), sinw = sinf(w0);
  float alpha = sinw / (2.0f * Q);
  float b0 =  1.0f + alpha * A;
  float b1 = -2.0f * cosw;
  float b2 =  1.0f - alpha * A;
  float a0 =  1.0f + alpha / A;
  float a1 = -2.0f * cosw;
  float a2 =  1.0f - alpha / A;
  set_coeffs(bq, b0, b1, b2, a0, a1, a2);
}

void biquad_lowshelf(biquad_t *bq, float f0, float Q, float gain_db, float sr) {
  float A    = powf(10.0f, gain_db / 40.0f);
  float w0   = 2.0f * ACDSP_PI * f0 / sr;
  float cosw = cosf(w0), sinw = sinf(w0);
  float alpha = sinw / (2.0f * Q);
  float two_sqrtA_alpha = 2.0f * sqrtf(A) * alpha;
  float b0 =      A*((A+1) - (A-1)*cosw + two_sqrtA_alpha);
  float b1 =  2.0f*A*((A-1) - (A+1)*cosw);
  float b2 =      A*((A+1) - (A-1)*cosw - two_sqrtA_alpha);
  float a0 =         (A+1) + (A-1)*cosw + two_sqrtA_alpha;
  float a1 = -2.0f *((A-1) + (A+1)*cosw);
  float a2 =         (A+1) + (A-1)*cosw - two_sqrtA_alpha;
  set_coeffs(bq, b0, b1, b2, a0, a1, a2);
}

void biquad_highshelf(biquad_t *bq, float f0, float Q, float gain_db, float sr) {
  float A    = powf(10.0f, gain_db / 40.0f);
  float w0   = 2.0f * ACDSP_PI * f0 / sr;
  float cosw = cosf(w0), sinw = sinf(w0);
  float alpha = sinw / (2.0f * Q);
  float two_sqrtA_alpha = 2.0f * sqrtf(A) * alpha;
  float b0 =      A*((A+1) + (A-1)*cosw + two_sqrtA_alpha);
  float b1 = -2.0f*A*((A-1) + (A+1)*cosw);
  float b2 =      A*((A+1) + (A-1)*cosw - two_sqrtA_alpha);
  float a0 =         (A+1) - (A-1)*cosw + two_sqrtA_alpha;
  float a1 =  2.0f *((A-1) - (A+1)*cosw);
  float a2 =         (A+1) - (A-1)*cosw - two_sqrtA_alpha;
  set_coeffs(bq, b0, b1, b2, a0, a1, a2);
}

void biquad_highpass(biquad_t *bq, float f0, float Q, float sr) {
  float w0   = 2.0f * ACDSP_PI * f0 / sr;
  float cosw = cosf(w0), sinw = sinf(w0);
  float alpha = sinw / (2.0f * Q);
  float b0 =  (1.0f + cosw) * 0.5f;
  float b1 = -(1.0f + cosw);
  float b2 =  (1.0f + cosw) * 0.5f;
  float a0 =  1.0f + alpha;
  float a1 = -2.0f * cosw;
  float a2 =  1.0f - alpha;
  set_coeffs(bq, b0, b1, b2, a0, a1, a2);
}

void biquad_lowpass(biquad_t *bq, float f0, float Q, float sr) {
  float w0   = 2.0f * ACDSP_PI * f0 / sr;
  float cosw = cosf(w0), sinw = sinf(w0);
  float alpha = sinw / (2.0f * Q);
  float b0 =  (1.0f - cosw) * 0.5f;
  float b1 =   1.0f - cosw;
  float b2 =  (1.0f - cosw) * 0.5f;
  float a0 =  1.0f + alpha;
  float a1 = -2.0f * cosw;
  float a2 =  1.0f - alpha;
  set_coeffs(bq, b0, b1, b2, a0, a1, a2);
}
