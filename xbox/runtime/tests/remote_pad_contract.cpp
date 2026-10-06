#include <ac/remote_pad.hpp>
#include <cassert>
int main(){
 const std::string token(64,'a'),prefix="ACR1 "+token+" ";ac::RemotePadPacket p;
 assert(ac::parse_remote_pad(prefix+"1 0 1 -1 0 .5 1 1024",token,0,p));
 assert(p.ly==1&&p.rx==-1&&p.buttons==1024);
 assert(!ac::parse_remote_pad(prefix+"1 0 1 -1 0 .5 1 0",token,1,p));
 for(const auto& bad:{"2 nan 0 0 0 0 0 0","2 2 0 0 0 0 0 0","2 0 0 0 0 -1 0 0","2 0 0 0 0 0 0 16384","2 0 0 0 0 0 0 0 extra"})assert(!ac::parse_remote_pad(prefix+bad,token,0,p));
 assert(!ac::parse_remote_pad(prefix+"2 0 0 0 0 0 0 0",std::string(64,'b'),0,p));
}
