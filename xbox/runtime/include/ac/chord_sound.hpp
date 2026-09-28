#pragma once
#include <array>
#include <cmath>
#include <cstdint>
#include <vector>
namespace ac {
// Am9, Fmaj7, Cmaj9, G6: close upper voices over a moving bass.
inline std::vector<int16_t> synthesize_park_chord(int sample_rate, int chord) {
  if (sample_rate <= 0 || chord < 0 || chord > 3) return {};
  constexpr int notes[4][5]={{45,60,64,67,71},{41,60,64,69,72},{48,59,62,64,67},{43,59,62,64,69}};
  const int frames=static_cast<int>(sample_rate*2.2);
  std::array<double,5> steps{};
  for(int n=0;n<5;n++)steps[n]=6.283185307179586*440*std::pow(2.0,(notes[chord][n]-69)/12.0)/sample_rate;
  std::vector<int16_t> samples(frames);
  for(int i=0;i<frames;i++) {
    const double t=static_cast<double>(i)/sample_rate;
    const double attack=std::fmin(1.0,t/.16),release=std::fmin(1.0,(2.2-t)/.75);
    double value=0;for(int n=0;n<5;n++)value+=std::sin(steps[n]*i)*(n==0?.8:1.0);
    samples[i]=static_cast<int16_t>(value*.11*attack*release*32767);
  }
  return samples;
}
}
