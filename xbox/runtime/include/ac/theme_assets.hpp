#pragma once
#include <array>
namespace ac::xbox {
struct ThemeAssetSpec { unsigned width, height, master_width, master_height; };
inline constexpr std::array<ThemeAssetSpec, 5> theme_assets{{
  {1024, 576, 1672, 941}, {1024, 512, 1774, 887},
  {1024, 512, 1774, 887}, {1024, 1024, 1254, 1254}, {1024, 576, 1672, 940}
}};
}
