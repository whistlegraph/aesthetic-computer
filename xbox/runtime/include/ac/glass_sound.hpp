#pragma once
#include <algorithm>
#include <cmath>
#include <cstdint>
#include <vector>

namespace ac::xbox {

// Bake once per sample rate: a fracture transient, then irregular shard
// impacts with inharmonic resonances. No recordings and no per-hit allocation.
inline std::vector<int16_t> synthesize_glass(uint32_t rate, bool shard = false) {
  if (rate < 8000 || rate > 192000) return {};
  constexpr double tau = 6.2831853071795864769;
  const double duration = shard ? .16 : .78;
  std::vector<double> mixed(static_cast<std::size_t>(rate * duration), 0);
  uint32_t seed = shard ? 0x51a4d123u : 0x61a55b34u;
  const auto random = [&seed]() {
    seed ^= seed << 13; seed ^= seed >> 17; seed ^= seed << 5;
    return static_cast<double>(seed) / UINT32_MAX;
  };
  const auto burst = [&](double at, double length, double gain) {
    const auto start = static_cast<std::size_t>(at * rate);
    double low = 0;
    const double alpha = 1 - std::exp(-tau * 1800 / rate);
    for (std::size_t i = 0; i < rate * length && start + i < mixed.size(); ++i) {
      const double t = static_cast<double>(i) / rate;
      const double white = random() * 2 - 1;
      low += alpha * (white - low);
      const double envelope = (std::min)(1.0, t / .0004) * std::exp(-t * 7 / length);
      mixed[start + i] += (white - low) * gain * envelope;
    }
  };
  const double ratios[] = {1, 1.467, 2.545, 3.504};
  const double levels[] = {1, .55, .32, .18};
  const auto ring = [&](double at, double root, double length, double gain) {
    const auto start = static_cast<std::size_t>(at * rate);
    for (int mode = 0; mode < 4; ++mode) {
      const double frequency = root * ratios[mode];
      if (frequency > rate * .44) continue;
      for (std::size_t i = 0; i < rate * length && start + i < mixed.size(); ++i) {
        const double t = static_cast<double>(i) / rate;
        const double envelope = (std::min)(1.0, t / .0007) *
          std::exp(-t * (5 + mode * 2) / length);
        mixed[start + i] += std::sin(tau * frequency * t) * gain * levels[mode] * envelope;
      }
    }
  };
  burst(0, shard ? .012 : .055, shard ? .35 : 1.1);
  ring(0, shard ? 3100 : 1380, shard ? .14 : .28, shard ? .32 : .28);
  if (!shard) for (int strike = 0; strike < 14; ++strike) {
    const double at = .018 + strike * strike * .0028 + random() * .012;
    const double gain = .19 * (1 - strike / 18.0);
    burst(at, .009 + random() * .017, gain * 2);
    ring(at, 1600 + random() * 3600, .07 + random() * .17, gain);
  }
  std::vector<int16_t> samples(mixed.size());
  for (std::size_t i = 0; i < mixed.size(); ++i) {
    // Leave headroom for several panes and taper the end without a click.
    const double tail = (std::min)(1.0, (mixed.size() - 1 - i) / (rate * .008));
    samples[i] = static_cast<int16_t>(std::tanh(mixed[i] * .8) * tail * 25000);
  }
  return samples;
}

} // namespace ac::xbox
