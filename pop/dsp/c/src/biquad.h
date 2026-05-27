// RBJ Audio EQ Cookbook biquad — Direct Form II Transposed (numerically
// stable for fixed-point and float, low coefficient sensitivity).

#ifndef ACDSP_BIQUAD_H
#define ACDSP_BIQUAD_H

typedef struct {
  float b0, b1, b2, a1, a2;
  float z1, z2;
} biquad_t;

void biquad_clear(biquad_t *bq);

void biquad_peak     (biquad_t *bq, float f0, float Q, float gain_db, float sr);
void biquad_lowshelf (biquad_t *bq, float f0, float Q, float gain_db, float sr);
void biquad_highshelf(biquad_t *bq, float f0, float Q, float gain_db, float sr);
void biquad_highpass (biquad_t *bq, float f0, float Q, float sr);
void biquad_lowpass  (biquad_t *bq, float f0, float Q, float sr);

static inline float biquad_process(biquad_t *bq, float x) {
  float y = bq->b0 * x + bq->z1;
  bq->z1  = bq->b1 * x - bq->a1 * y + bq->z2;
  bq->z2  = bq->b2 * x - bq->a2 * y;
  return y;
}

#endif
