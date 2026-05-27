#ifndef ACDSP_UTIL_H
#define ACDSP_UTIL_H

#include <math.h>
#include <stddef.h>

#define ACDSP_PI 3.14159265358979323846f

static inline float acdsp_db2lin(float db) { return powf(10.0f, db * 0.05f); }
static inline float acdsp_lin2db(float lin) {
  return 20.0f * log10f(lin < 1e-10f ? 1e-10f : lin);
}
static inline float acdsp_clamp(float x, float lo, float hi) {
  return x < lo ? lo : (x > hi ? hi : x);
}

void  acdsp_set_error(const char *fmt, ...);
char *acdsp_strtrim(char *s);

#endif
