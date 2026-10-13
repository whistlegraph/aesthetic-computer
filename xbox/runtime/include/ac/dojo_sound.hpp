#pragma once
#include <algorithm>
#include <cmath>
#include <cstdint>
#include <cstring>
#include <vector>
#include "../../../../pop/nullabye/c/ac_hrtf.h"

namespace ac {
// Generated dry Foley stays mono; the listener's placement is applied at play.
inline std::vector<double> dojo_pcm(const std::vector<uint8_t>& wav) {
  if (wav.size() < 44 || std::memcmp(wav.data(), "RIFF", 4) ||
      std::memcmp(wav.data() + 8, "WAVE", 4)) return {};
  const auto u16 = [&](size_t p) { return unsigned(wav[p]) | unsigned(wav[p+1]) << 8; };
  const auto u32 = [&](size_t p) { return uint32_t(u16(p)) | uint32_t(u16(p+2)) << 16; };
  bool format = false;
  for (size_t p = 12; p + 8 <= wav.size();) {
    const auto size = u32(p + 4);
    if (size > wav.size() - p - 8) return {};
    if (!std::memcmp(wav.data() + p, "fmt ", 4) && size >= 16)
      format = u16(p+8) == 1 && u16(p+10) == 1 && u32(p+12) == 48000 && u16(p+22) == 16;
    if (!std::memcmp(wav.data() + p, "data", 4) && format && size % 2 == 0) {
      if (size > 48000 * 2) return {};
      std::vector<double> result(size / 2);
      for (size_t i = 0; i < result.size(); i++) result[i] = int16_t(u16(p+8+i*2)) / 32768.0;
      return result;
    }
    p += 8 + size + (size & 1);
  }
  return {};
}

inline std::vector<int16_t> dojo_spatial(const std::vector<double>& mono,
    double sampleRate, double pan, double gain, double playbackRate = 1) {
  ACHrtf ear{};
  const double angle = (std::max)(-1.0, (std::min)(1.0, pan)) * 1.15;
  // Pitch scaling happens in XAudio; pre-compensate the ear delays so the
  // listener's head does not double in size during half-speed playback.
  const double acousticRate = sampleRate * playbackRate;
  const size_t tail = size_t(acousticRate * .025) + AC_HRTF_RING;
  std::vector<int16_t> stereo((mono.size() + tail) * 2);
  const size_t reflectionL = size_t(acousticRate * .011), reflectionR = size_t(acousticRate * .017);
  for (size_t i = 0; i < stereo.size() / 2; i++) {
    float l, r;
    ac_hrtf_process_at_rate(&ear, float(i < mono.size() ? mono[i] * gain : 0),
      angle, 0, 1, acousticRate, &l, &r);
    // Two quiet early returns give the box a small room, without a long wash.
    if (i >= reflectionL && i - reflectionL < mono.size()) l += float(mono[i-reflectionL] * gain * .07);
    if (i >= reflectionR && i - reflectionR < mono.size()) r += float(mono[i-reflectionR] * gain * .05);
    stereo[i*2] = int16_t((std::max)(-1.f, (std::min)(1.f, l)) * 32767);
    stereo[i*2+1] = int16_t((std::max)(-1.f, (std::min)(1.f, r)) * 32767);
  }
  return stereo;
}
}
