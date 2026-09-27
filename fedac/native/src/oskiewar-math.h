#ifndef AC_OSKIEWAR_MATH_H
#define AC_OSKIEWAR_MATH_H
#include <math.h>
// Exact operation order from xbox/live/oskiewar.js. Compile without FMA
// contraction: these results are part of the paired simulation protocol.
#if defined(__GNUC__) && !defined(__clang__)
#pragma GCC push_options
#pragma GCC optimize ("fp-contract=off")
#endif
#pragma STDC FP_CONTRACT OFF
static double ow_sin(double x) {
  x = fmod(x, 6.283185307179586);
  if (x > 3.141592653589793) x -= 6.283185307179586;
  if (x < -3.141592653589793) x += 6.283185307179586;
  if (x > 1.5707963267948966) x = 3.141592653589793 - x;
  if (x < -1.5707963267948966) x = -3.141592653589793 - x;
  const double square = x * x;
  return x * (1 + square * (-.16666666666666666 + square *
    (.008333333333333333 + square * (-.0001984126984126984 + square *
    (.0000027557319223985893 + square * (-.00000002505210838544172 + square *
    (.00000000016059043836821615 + square * (-.0000000000007647163731819816 + square *
    (.0000000000000028114572543455206 + square * (-.00000000000000000822063524662433 +
    square * .000000000000000000019572941063391263))))))))));
}
static double ow_atan(double x) {
  const double sign = x < 0 ? -1 : 1;
  x = fabs(x);
  const int inverse = x > 1;
  if (inverse) x = 1 / x;
  x = x / (1 + sqrt(1 + x * x));
  const double square = x * x;
  double term = x, sum = x;
  for (int n = 1; n <= 20; n++) { term *= -square; sum += term / (2 * n + 1); }
  const double angle = 2 * sum;
  return sign * (inverse ? 1.5707963267948966 - angle : angle);
}
static double ow_atan2(double y, double x) {
  if (x > 0) return ow_atan(y / x);
  if (x < 0) return ow_atan(y / x) + (y < 0 ? -1 : 1) * 3.141592653589793;
  return y > 0 ? 1.5707963267948966 : y < 0 ? -1.5707963267948966 : 0;
}
static double ow_exp(double x) {
  if (x > 709.782712893384) return INFINITY;
  if (x < -745.1332191019411) return 0;
  if (isnan(x)) return x;
  double quotient = x / .6931471805599453;
  double exponent = floor(quotient);
  if (quotient - exponent >= .5) exponent++;
  const double remainder = x - exponent * .6931471805599453;
  double sum = 1, term = 1;
  for (int n = 1; n <= 16; n++) { term *= remainder / n; sum += term; }
  return sum * ldexp(1, (int)exponent);
}
static double ow_hypot(const double *values, int count) {
  double scale = 0;
  for (int i = 0; i < count; i++) {
    double v = fabs(values[i]);
    if (isnan(v)) return v;
    if (v > scale) scale = v;
  }
  if (!scale || !isfinite(scale)) return scale;
  double sum = 0;
  for (int i = 0; i < count; i++) { double ratio = values[i] / scale; sum += ratio * ratio; }
  return sqrt(sum) * scale;
}
#if defined(__GNUC__) && !defined(__clang__)
#pragma GCC pop_options
#endif
#endif
