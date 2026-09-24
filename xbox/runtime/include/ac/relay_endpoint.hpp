#pragma once
#include <string_view>
namespace ac::xbox {
// Development-only LAN route. No credentials, DNS, query, fragment, external
// address or path variation can escape this narrow native transport override.
inline bool valid_lan_relay(std::string_view url) {
  constexpr std::string_view prefix="ws://", suffix="/oskiewar-live";
  if (url.size()>200 || url.substr(0,prefix.size())!=prefix ||
      url.size()<prefix.size()+suffix.size() ||
      url.substr(url.size()-suffix.size())!=suffix) return false;
  auto authority=url.substr(prefix.size(),url.size()-prefix.size()-suffix.size());
  const auto colon=authority.find(':');
  if(colon==std::string_view::npos || authority.find(':',colon+1)!=std::string_view::npos) return false;
  auto host=authority.substr(0,colon), port=authority.substr(colon+1);
  if(port.empty() || port.size()>5 || port.front()=='0') return false;
  unsigned number=0;
  for(char c:port){if(c<'0'||c>'9')return false;number=number*10+static_cast<unsigned>(c-'0');}
  if(number<1||number>65535)return false;
  unsigned octets[4]{};
  for(int i=0;i<4;++i){
    const auto dot=host.find('.');
    if((i<3 && dot==std::string_view::npos)||(i==3 && dot!=std::string_view::npos))return false;
    const auto part=host.substr(0,dot);
    if(part.empty()||part.size()>3||(part.size()>1&&part.front()=='0'))return false;
    for(char c:part){if(c<'0'||c>'9')return false;octets[i]=octets[i]*10+static_cast<unsigned>(c-'0');}
    if(octets[i]>255)return false;
    if(i<3)host.remove_prefix(dot+1);
  }
  return octets[0]==10 || (octets[0]==172&&octets[1]>=16&&octets[1]<=31) ||
    (octets[0]==192&&octets[1]==168);
}
}
