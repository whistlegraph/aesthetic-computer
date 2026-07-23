#include "ac/image_effects.hpp"

#include <cassert>
#include <cstdint>
#include <vector>

using ac::xbox::BoxBlurBgra;

int main() {
  std::vector<std::uint32_t> scratch;
  std::vector<std::uint32_t> horizontal{0xff000000u, 0xffffffffu, 0xff000000u};
  BoxBlurBgra(horizontal, 3, 1, 1, scratch);
  assert((horizontal == std::vector<std::uint32_t>{
    0xff555555u, 0xff555555u, 0xff555555u}));

  std::vector<std::uint32_t> vertical{0xffff0000u, 0xff00ff00u, 0xff0000ffu};
  BoxBlurBgra(vertical, 1, 3, 1, scratch);
  assert(vertical[1] == 0xff555555u);

  const auto unchanged = vertical;
  BoxBlurBgra(vertical, 1, 3, 0, scratch);
  assert(vertical == unchanged);
}
