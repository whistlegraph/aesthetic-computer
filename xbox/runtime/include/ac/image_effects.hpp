#pragma once

#include <algorithm>
#include <cstdint>
#include <vector>

namespace ac::xbox {

// In-place separable box blur for the host's packed ARGB/BGRA8 CPU frame.
// Runtime is O(width * height), independent of the bounded radius.
inline void BoxBlurBgra(std::vector<std::uint32_t>& frame, unsigned width,
                        unsigned height, unsigned radius,
                        std::vector<std::uint32_t>& scratch) {
  if (radius == 0 || frame.empty() || width == 0 || height == 0 ||
      frame.size() != static_cast<std::size_t>(width) * height) return;
  radius = (std::min)(16u, radius);
  const int frameWidth = static_cast<int>(width);
  const int frameHeight = static_cast<int>(height);
  const int diameter = static_cast<int>(radius) * 2 + 1;
  scratch.resize(frame.size());
  const auto channel = [](std::uint32_t pixel, unsigned shift) {
    return (pixel >> shift) & 255u;
  };
  const auto packAverage = [diameter](unsigned a, unsigned r, unsigned g, unsigned b) {
    return ((a / diameter) << 24) | ((r / diameter) << 16) |
      ((g / diameter) << 8) | (b / diameter);
  };

  for (int y = 0; y < frameHeight; ++y) {
    unsigned a = 0, r = 0, g = 0, b = 0;
    const auto sample = [&frame, y, frameWidth](int x) {
      x = (std::max)(0, (std::min)(frameWidth - 1, x));
      return frame[static_cast<std::size_t>(y) * frameWidth + x];
    };
    for (int offset = -static_cast<int>(radius); offset <= static_cast<int>(radius); ++offset) {
      const auto pixel = sample(offset);
      a += channel(pixel, 24); r += channel(pixel, 16);
      g += channel(pixel, 8); b += channel(pixel, 0);
    }
    for (int x = 0; x < frameWidth; ++x) {
      scratch[static_cast<std::size_t>(y) * frameWidth + x] = packAverage(a, r, g, b);
      const auto remove = sample(x - static_cast<int>(radius));
      const auto add = sample(x + static_cast<int>(radius) + 1);
      a = a + channel(add, 24) - channel(remove, 24);
      r = r + channel(add, 16) - channel(remove, 16);
      g = g + channel(add, 8) - channel(remove, 8);
      b = b + channel(add, 0) - channel(remove, 0);
    }
  }

  for (int x = 0; x < frameWidth; ++x) {
    unsigned a = 0, r = 0, g = 0, b = 0;
    const auto sample = [&scratch, x, frameHeight, frameWidth](int y) {
      y = (std::max)(0, (std::min)(frameHeight - 1, y));
      return scratch[static_cast<std::size_t>(y) * frameWidth + x];
    };
    for (int offset = -static_cast<int>(radius); offset <= static_cast<int>(radius); ++offset) {
      const auto pixel = sample(offset);
      a += channel(pixel, 24); r += channel(pixel, 16);
      g += channel(pixel, 8); b += channel(pixel, 0);
    }
    for (int y = 0; y < frameHeight; ++y) {
      frame[static_cast<std::size_t>(y) * frameWidth + x] = packAverage(a, r, g, b);
      const auto remove = sample(y - static_cast<int>(radius));
      const auto add = sample(y + static_cast<int>(radius) + 1);
      a = a + channel(add, 24) - channel(remove, 24);
      r = r + channel(add, 16) - channel(remove, 16);
      g = g + channel(add, 8) - channel(remove, 8);
      b = b + channel(add, 0) - channel(remove, 0);
    }
  }
}

}  // namespace ac::xbox
