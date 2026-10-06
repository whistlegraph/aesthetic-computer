#pragma once
#include "../runtime/include/ac/remote_pad.hpp"
#include <fstream>
#include <ctime>

namespace ac::xbox {
// Developer-console input, paired by an authenticated Device Portal file write.
// Every packet is a full snapshot; a lost sender releases all keys after 250 ms.
class RemotePad {
  struct State {
    std::mutex mutex;
    std::string token;
    std::uint64_t sequence=0, received=0;
    long long expires=0;
    RemotePadPacket packet;
    std::string status;
    bool sawPacket=false;
  };
  std::shared_ptr<State> state=std::make_shared<State>();
  Windows::Networking::Sockets::DatagramSocket^ socket=nullptr;
  unsigned long long nextRead=0;
 public:
  RemotePad() {
#if AC_DEV_LIVE_PIECE
    using namespace Windows::Networking::Sockets;
    socket=ref new DatagramSocket();
    std::weak_ptr<State> weak=state;
    socket->MessageReceived += ref new Windows::Foundation::TypedEventHandler<DatagramSocket^, DatagramSocketMessageReceivedEventArgs^>(
      [weak](DatagramSocket^ sender, DatagramSocketMessageReceivedEventArgs^ args) {
        try {
          auto reader=args->GetDataReader();
          const auto n=reader->UnconsumedBufferLength;
          if(n<1||n>256)return;
          auto bytes=ref new Platform::Array<unsigned char>(n);reader->ReadBytes(bytes);
          std::string text(reinterpret_cast<const char*>(bytes->Data),n);
          auto s=weak.lock();if(!s)return;
          RemotePadPacket p;
          {
            std::lock_guard<std::mutex> lock(s->mutex);
            if(!s->sawPacket){s->sawPacket=true;s->status+=" datagram=received";}
            if(std::time(nullptr)>=s->expires||!parse_remote_pad(text,s->token,s->sequence,p))return;
            if(!s->received)s->status+=" input=accepted";
            s->packet=p;s->sequence=p.sequence;s->received=GetTickCount64();
          }
          auto address=args->RemoteAddress;auto port=args->RemotePort;
          concurrency::create_task(sender->GetOutputStreamAsync(address,port)).then([p](concurrency::task<Windows::Storage::Streams::IOutputStream^> result){
            try {
              auto writer=ref new Windows::Storage::Streams::DataWriter(result.get());
              auto ack=L"ACR1 "+std::to_wstring(p.sequence)+L"\n";
              writer->WriteString(ref new Platform::String(ack.c_str()));
              concurrency::create_task(writer->StoreAsync()).then([writer](concurrency::task<unsigned> sent){try{sent.get();}catch(Platform::Exception^){} });
            }catch(Platform::Exception^){}
          });
        }catch(Platform::Exception^){}
      });
    concurrency::create_task(socket->BindServiceNameAsync("51339")).then([weak](concurrency::task<void> task){
      auto s=weak.lock();if(!s)return;
      try{task.get();std::lock_guard<std::mutex> lock(s->mutex);s->status+=" listening=51339";}
      catch(Platform::Exception^ error){std::lock_guard<std::mutex> lock(s->mutex);s->status+=" bindError="+std::to_string(error->HResult);}
    });
#endif
  }
  ~RemotePad(){if(socket){delete socket;socket=nullptr;}}
  std::string takeStatus(){
    std::lock_guard<std::mutex> lock(state->mutex);
    std::string result;result.swap(state->status);return result;
  }
  bool read(PadState& pad){
#if AC_DEV_LIVE_PIECE
    const auto now=GetTickCount64();
    if(now>=nextRead){
      nextRead=now+1000;
      const auto path=std::wstring(Windows::Storage::ApplicationData::Current->LocalFolder->Path->Data())+L"\\ac-remote.txt";
      std::ifstream file(path);std::string key;long long expires=0;
      file>>key>>expires;
      std::lock_guard<std::mutex> lock(state->mutex);
      if(key!=state->token){
        state->token=key;state->sequence=0;state->received=0;state->sawPacket=false;
        state->status+=" paired="+std::to_string(key.size()==64&&expires>std::time(nullptr));
      }
      state->expires=expires;
    }
    std::lock_guard<std::mutex> lock(state->mutex);
    if(!state->received||GetTickCount64()-state->received>250||std::time(nullptr)>=state->expires)return false;
    const auto& p=state->packet;
    pad={};pad.connected=true;pad.left_x=p.lx;pad.left_y=p.ly;
    pad.right_x=p.rx;pad.right_y=p.ry;pad.left_trigger=p.lt;pad.right_trigger=p.rt;
    const char* names[]={"A","B","X","Y","ArrowUp","ArrowDown","ArrowLeft","ArrowRight","LeftShoulder","RightShoulder","Menu","View","LeftStick","RightStick"};
    for(unsigned i=0;i<14;i++)if(p.buttons&(1u<<i))pad.down.insert(names[i]);
    return true;
#else
    return false;
#endif
  }
};
}
