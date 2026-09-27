#pragma once
#include <algorithm>
#include <cmath>
#include <cstdint>
#include <vector>

namespace ac::xbox {
// Four alpha-bearing raster stamps: rubber tread, scuff, chipped wood, blood.
// Generated once at startup and retained as a GPU texture, not geometry.
inline std::vector<uint8_t> make_decal_atlas() {
  constexpr unsigned side = 256, cell = 128;
  std::vector<uint8_t> pixels(side * side * 4);
  const auto noise = [](unsigned x, unsigned y, unsigned salt) {
    uint32_t n = x * 374761393u + y * 668265263u + salt * 2246822519u;
    n = (n ^ (n >> 13)) * 1274126177u;
    return static_cast<double>((n ^ (n >> 16)) & 65535u) / 65535.0;
  };
  const auto clamp = [](double n) { return (std::max)(0.0, (std::min)(1.0, n)); };
  for (unsigned y = 0; y < side; ++y) for (unsigned x = 0; x < side; ++x) {
    const unsigned kind = (y / cell) * 2 + x / cell;
    const unsigned sx = x % cell, sy = y % cell;
    const double u = (sx + .5) / cell * 2 - 1, v = (sy + .5) / cell * 2 - 1;
    const double grain = noise(sx, sy, kind + 1);
    const double coarse = noise(sx / 9, sy / 7, kind + 17);
    const double border = clamp((1 - std::abs(u)) * 12) * clamp((1 - std::abs(v)) * 12);
    double alpha = 0, red = 38, green = 31, blue = 35;
    if (kind == 0) {
      const double stripe = .3 + .7 * noise(0, sy, 32);
      const double tread = std::sin((u + v * .16) * 66) > .5 ? .38 : 1;
      alpha = clamp((.85 - std::abs(v)) * 6) * stripe * tread * (.3 + grain * .7) * .72;
    } else if (kind == 1) {
      const double rim = clamp((.84 + coarse * .15 - std::hypot(u, v)) * 7);
      const double scratch = noise(sx / 17, sy, 43) > .55 ? 1 : .16;
      alpha = rim * scratch * (.2 + grain * .8) * .6;
      red = 66; green = 47; blue = 39;
    } else if (kind == 2) {
      const double edge = std::abs(v + std::sin(u * 17) * .09);
      alpha = clamp((.36 - edge - coarse * .14) * 12) * clamp((.88 - std::abs(u)) * 8) * grain * .8;
      red = 222; green = 183; blue = 130;
    } else {
      const double radius = .66 + coarse * .15 + std::sin(std::atan2(v, u) * 9) * .1;
      alpha = clamp((radius - std::hypot(u, v)) * 18) * (.62 + grain * .38) * .85;
      red = 111; green = 9; blue = 20;
    }
    const auto index = (y * side + x) * 4;
    pixels[index] = static_cast<uint8_t>(red + grain * 15);
    pixels[index + 1] = static_cast<uint8_t>(green + grain * 12);
    pixels[index + 2] = static_cast<uint8_t>(blue + grain * 10);
    pixels[index + 3] = sx < 2 || sy < 2 || sx >= cell - 2 || sy >= cell - 2
      ? 0 : static_cast<uint8_t>(clamp(alpha * border) * 255);
  }
  return pixels;
}
} // namespace ac::xbox
