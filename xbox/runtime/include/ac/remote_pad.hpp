#pragma once
#include <cmath>
#include <sstream>
#include <string>
#include <cstdint>

namespace ac {
struct RemotePadPacket {
  std::uint64_t sequence = 0;
  float lx=0, ly=0, rx=0, ry=0, lt=0, rt=0;
  unsigned buttons=0;
};
inline bool parse_remote_pad(const std::string& text, const std::string& secret,
    std::uint64_t previous, RemotePadPacket& result) {
  if (text.size()>256 || secret.size()!=64) return false;
  std::istringstream in(text);
  std::string magic, token, extra;
  RemotePadPacket p;
  if (!(in>>magic>>token>>p.sequence>>p.lx>>p.ly>>p.rx>>p.ry>>p.lt>>p.rt>>p.buttons)
      || (in>>extra) || magic!="ACR1" || token!=secret || p.sequence<=previous
      || p.buttons>16383) return false;
  for (float v : {p.lx,p.ly,p.rx,p.ry}) if (!std::isfinite(v)||std::abs(v)>1) return false;
  for (float v : {p.lt,p.rt}) if (!std::isfinite(v)||v<0||v>1) return false;
  result=p; return true;
}
}
