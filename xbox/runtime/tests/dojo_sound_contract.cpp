#include "ac/dojo_sound.hpp"
#include <cassert>
#include <fstream>
#include <iostream>
#include <iterator>

int main(int argc, char** argv) {
  assert(argc == 2);
  std::ifstream stream(argv[1], std::ios::binary);
  const std::vector<uint8_t> wav{std::istreambuf_iterator<char>(stream), {}};
  const auto pcm = ac::dojo_pcm(wav);
  assert(pcm.size() > 1000 && pcm.size() <= 48000);
  double power = 0;
  for (auto v : pcm) { assert(std::isfinite(v) && std::abs(v) <= 1); power += v*v; }
  assert(power > 1);
  assert(ac::dojo_pcm({}).empty());
  auto broken = wav; broken.resize(24);
  assert(ac::dojo_pcm(broken).empty());
  broken = wav; broken[0] = 0;
  assert(ac::dojo_pcm(broken).empty());

  // One impulse exposes direct-ear timing and far-ear shadow without masking.
  std::vector<double> impulse(2048); impulse[0] = 1;
  for (double rate : {.5, 1.0, 2.0}) {
    const auto left = ac::dojo_spatial(impulse, 48000, -.8, .5, rate);
    const auto right = ac::dojo_spatial(impulse, 48000, .8, .5, rate);
    assert(left.size() == right.size() && left.size() > impulse.size()*2);
    assert(left[0] != 0 && left[1] == 0 && right[0] == 0 && right[1] != 0);
    double energyL = 0, energyR = 0;
    for (size_t i = 0; i < 128; i++) {
      assert(left[2*i] == right[2*i+1] && left[2*i+1] == right[2*i]);
      energyL += double(left[2*i]) * left[2*i];
      energyR += double(left[2*i+1]) * left[2*i+1];
    }
    assert(energyL > energyR);
  }
  // Existing 48 kHz users keep the same public function and exact output.
  ACHrtf old{}, explicitRate{};
  for (int i = 0; i < 1024; i++) {
    float l1, r1, l2, r2;
    ac_hrtf_process(&old, float(std::sin(i*.1)), .8, .2, 1, &l1, &r1);
    ac_hrtf_process_at_rate(&explicitRate, float(std::sin(i*.1)), .8, .2, 1, 48000, &l2, &r2);
    assert(l1 == l2 && r1 == r2);
  }
  std::cout << "dojo PCM, binaural timing, head shadow, rate and compatibility passed\n";
}
