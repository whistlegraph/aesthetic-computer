#pragma once
#include <cmath>
#include <cstdint>
#include <vector>
namespace ac::xbox {
// Periodic filtered noise: no seam, no allocations while the wheels roll.
inline std::vector<int16_t> synthesize_skate_roll(unsigned rate) {
  const unsigned count = rate * 2;
  std::vector<double> mix(count, 0.0);
  uint32_t seed = 0x51a7e;
  constexpr double tau = 6.2831853071795864769;
  for (unsigned band = 0; band < 96; ++band) {
    seed = seed * 1664525u + 1013904223u;
    const double phase = tau * (seed / 4294967296.0);
    const double hz = 45.0 + band * 19.5;
    const double level = 1.0 / (1.0 + hz / 520.0);
    for (unsigned i = 0; i < count; ++i) {
      const double t = static_cast<double>(i) / rate;
      mix[i] += std::sin(tau * hz * t + phase) * level *
        (.82 + .12 * std::sin(tau * 23.0 * t) + .06 * std::sin(tau * 37.5 * t));
    }
  }
  double peak = 1;
  for (const double v : mix) if (std::abs(v) > peak) peak = std::abs(v);
  std::vector<int16_t> pcm(count);
  for (unsigned i = 0; i < count; ++i) pcm[i] = static_cast<int16_t>(mix[i] / peak * 24000);
  return pcm;
}
}
