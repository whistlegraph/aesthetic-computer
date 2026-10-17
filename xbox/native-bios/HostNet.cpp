#include "pch.h"
#include "HostNet.hpp"
#include <deque>
#include <map>
#include <mutex>

using namespace Platform;
using namespace Windows::Foundation;
using namespace Windows::Networking;
using namespace Windows::Networking::Sockets;
using namespace Windows::Storage::Streams;
using namespace concurrency;

namespace ac::xbox {

namespace {

std::wstring Wide(std::string_view value) {
  if (value.empty()) return {};
  const int size = MultiByteToWideChar(CP_UTF8, 0, value.data(),
    static_cast<int>(value.size()), nullptr, 0);
  std::wstring result(static_cast<std::size_t>(size), L'\0');
  MultiByteToWideChar(CP_UTF8, 0, value.data(), static_cast<int>(value.size()),
    result.data(), size);
  return result;
}

std::string Narrow(String^ value) {
  if (!value || value->Length() == 0) return {};
  const int size = WideCharToMultiByte(CP_UTF8, 0, value->Data(), value->Length(),
    nullptr, 0, nullptr, nullptr);
  std::string result(static_cast<std::size_t>(size), '\0');
  WideCharToMultiByte(CP_UTF8, 0, value->Data(), value->Length(), result.data(),
    size, nullptr, nullptr);
  return result;
}

bool EndsWith(std::string_view value, std::string_view tail) {
  return value.size() >= tail.size() &&
    value.compare(value.size() - tail.size(), tail.size(), tail) == 0;
}

constexpr std::size_t kMaxMessage = 65536, kMaxQueue = 256, kMaxSockets = 8;

}  // namespace

struct HostNet::State {
  struct Socket {
    MessageWebSocket^ socket = nullptr;
    DataWriter^ writer = nullptr;
    std::deque<std::string> outbox;
    bool open = false, writing = false;
  };
  struct Udp {
    DatagramSocket^ socket = nullptr;
    DataWriter^ writer = nullptr;
  };
  std::mutex mutex;
  Logger logger;
  bool stopped = false;
  int next = 1;
  std::map<int, Socket> sockets;
  std::map<int, Udp> udps;
  std::vector<HostNetEvent> events;

  void post(HostNetEvent event) {
    if (events.size() >= kMaxQueue) events.erase(events.begin());
    events.push_back(std::move(event));
  }
  void log(const std::string& line) { if (logger) logger(line); }
};

namespace {

void FlushSocket(const std::shared_ptr<HostNet::State>& state, int id) {
  std::string payload;
  DataWriter^ writer = nullptr;
  {
    std::lock_guard<std::mutex> lock(state->mutex);
    auto found = state->sockets.find(id);
    if (state->stopped || found == state->sockets.end()) return;
    auto& socket = found->second;
    if (!socket.open || socket.writing || socket.outbox.empty() || !socket.writer) return;
    payload = std::move(socket.outbox.front());
    socket.outbox.pop_front();
    socket.writing = true;
    writer = socket.writer;
  }
  std::weak_ptr<HostNet::State> weak = state;
  try {
    writer->WriteString(ref new String(Wide(payload).c_str()));
    create_task(writer->StoreAsync()).then([weak, id](task<unsigned> completed) {
      const auto state = weak.lock();
      if (!state) return;
      bool failed = false;
      try { completed.get(); } catch (Exception^) { failed = true; }
      {
        std::lock_guard<std::mutex> lock(state->mutex);
        auto found = state->sockets.find(id);
        if (found == state->sockets.end()) return;
        found->second.writing = false;
        if (failed) state->post({ "error", id, 0, "send" });
      }
      FlushSocket(state, id);
    });
  } catch (Exception^) {
    std::lock_guard<std::mutex> lock(state->mutex);
    auto found = state->sockets.find(id);
    if (found != state->sockets.end()) found->second.writing = false;
    state->post({ "error", id, 0, "send" });
  }
}

}  // namespace

bool HostNet::allowed_host(std::string_view host) {
  if (host.empty() || host.size() > 120) return false;
  for (const char c : host)
    if (!((c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c == '.' || c == '-'))
      return false;
  return host == "aesthetic.computer" || host == "oskiewar.com" ||
    EndsWith(host, ".aesthetic.computer") || EndsWith(host, ".oskiewar.com");
}

bool HostNet::allowed_url(std::string_view url, bool socket) {
  const std::string_view scheme = socket ? "wss://" : "https://";
  if (url.size() > 2048 || url.substr(0, scheme.size()) != scheme) return false;
  const auto rest = url.substr(scheme.size());
  const auto end = rest.find_first_of("/?:#");
  return allowed_host(rest.substr(0, end == std::string_view::npos ? rest.size() : end));
}

HostNet::HostNet(Logger logger) : state_(std::make_shared<State>()) {
  state_->logger = std::move(logger);
}

HostNet::~HostNet() { shutdown(); }

int HostNet::socket_open(std::string_view url) {
  if (!allowed_url(url, true)) return 0;
  int id = 0;
  MessageWebSocket^ socket = nullptr;
  {
    std::lock_guard<std::mutex> lock(state_->mutex);
    if (state_->stopped || state_->sockets.size() >= kMaxSockets) return 0;
    id = state_->next++;
    socket = ref new MessageWebSocket();
    socket->Control->MessageType = SocketMessageType::Utf8;
    state_->sockets[id].socket = socket;
  }
  std::weak_ptr<State> weak = state_;
  socket->MessageReceived +=
    ref new TypedEventHandler<MessageWebSocket^, MessageWebSocketMessageReceivedEventArgs^>(
      [weak, id](MessageWebSocket^, MessageWebSocketMessageReceivedEventArgs^ args) {
        try {
          auto reader = args->GetDataReader();
          reader->UnicodeEncoding = UnicodeEncoding::Utf8;
          const auto size = reader->UnconsumedBufferLength;
          if (!size || size > kMaxMessage) return;
          auto text = Narrow(reader->ReadString(size));
          const auto state = weak.lock();
          if (!state) return;
          std::lock_guard<std::mutex> lock(state->mutex);
          if (!state->stopped && state->sockets.count(id))
            state->post({ "message", id, 0, std::move(text) });
        } catch (Exception^) {}
      });
  socket->Closed += ref new TypedEventHandler<IWebSocket^, WebSocketClosedEventArgs^>(
    [weak, id](IWebSocket^, WebSocketClosedEventArgs^ args) {
      const auto state = weak.lock();
      if (!state) return;
      std::lock_guard<std::mutex> lock(state->mutex);
      if (!state->sockets.erase(id)) return;
      state->post({ "close", id, args ? args->Code : 1006, Narrow(args ? args->Reason : nullptr) });
    });
  create_task(socket->ConnectAsync(ref new Uri(ref new String(Wide(url).c_str())))).then(
    [weak, id, socket](task<void> connected) {
      const auto state = weak.lock();
      if (!state) return;
      try { connected.get(); }
      catch (Exception^) {
        std::lock_guard<std::mutex> lock(state->mutex);
        if (state->sockets.erase(id)) state->post({ "close", id, 1006, "connect" });
        return;
      }
      {
        std::lock_guard<std::mutex> lock(state->mutex);
        auto found = state->sockets.find(id);
        if (state->stopped || found == state->sockets.end()) return;
        found->second.writer = ref new DataWriter(socket->OutputStream);
        found->second.open = true;
        state->post({ "open", id, 0, "" });
      }
      FlushSocket(state, id);
    });
  return id;
}

void HostNet::socket_send(int id, std::string_view text) {
  if (text.empty() || text.size() > kMaxMessage) return;
  {
    std::lock_guard<std::mutex> lock(state_->mutex);
    auto found = state_->sockets.find(id);
    if (found == state_->sockets.end() || found->second.outbox.size() >= kMaxQueue) return;
    found->second.outbox.emplace_back(text);
  }
  FlushSocket(state_, id);
}

void HostNet::socket_close(int id) {
  MessageWebSocket^ socket = nullptr;
  {
    std::lock_guard<std::mutex> lock(state_->mutex);
    auto found = state_->sockets.find(id);
    if (found == state_->sockets.end()) return;
    socket = found->second.socket;
    state_->sockets.erase(found);
    state_->post({ "close", id, 1000, "" });
  }
  if (socket) try { socket->Close(1000, ref new String(L"closed")); } catch (Exception^) {}
}

void HostNet::fetch(int id, std::string_view url) {
  std::weak_ptr<State> weak = state_;
  if (!allowed_url(url, false)) {
    std::lock_guard<std::mutex> lock(state_->mutex);
    state_->post({ "fetch", id, -1, "blocked" });
    return;
  }
  auto client = ref new Windows::Web::Http::HttpClient();
  auto uri = ref new Uri(ref new String(Wide(url).c_str()));
  create_task(client->GetAsync(uri)).then(
    [weak, id, client](task<Windows::Web::Http::HttpResponseMessage^> responded) {
      Windows::Web::Http::HttpResponseMessage^ response = nullptr;
      try { response = responded.get(); }
      catch (Exception^) {
        if (const auto state = weak.lock()) {
          std::lock_guard<std::mutex> lock(state->mutex);
          state->post({ "fetch", id, -1, "transport" });
        }
        return;
      }
      const int status = static_cast<int>(response->StatusCode);
      create_task(response->Content->ReadAsStringAsync()).then(
        [weak, id, status](task<String^> read) {
          std::string body;
          try { body = Narrow(read.get()); } catch (Exception^) {}
          if (body.size() > kMaxMessage * 8) body.resize(kMaxMessage * 8);
          if (const auto state = weak.lock()) {
            std::lock_guard<std::mutex> lock(state->mutex);
            state->post({ "fetch", id, status, std::move(body) });
          }
        });
    });
}

int HostNet::udp_open(std::string_view host, int port) {
  if (!allowed_host(host) || port <= 0 || port > 65535) return 0;
  int id = 0;
  DatagramSocket^ socket = ref new DatagramSocket();
  {
    std::lock_guard<std::mutex> lock(state_->mutex);
    if (state_->stopped || state_->udps.size() >= kMaxSockets) return 0;
    id = state_->next++;
    state_->udps[id].socket = socket;
  }
  std::weak_ptr<State> weak = state_;
  socket->MessageReceived += ref new TypedEventHandler<DatagramSocket^,
      DatagramSocketMessageReceivedEventArgs^>(
    [weak, id](DatagramSocket^, DatagramSocketMessageReceivedEventArgs^ args) {
      try {
        auto reader = args->GetDataReader();
        reader->UnicodeEncoding = UnicodeEncoding::Utf8;
        const auto size = reader->UnconsumedBufferLength;
        if (!size || size > 4096) return;
        auto text = Narrow(reader->ReadString(size));
        const auto state = weak.lock();
        if (!state) return;
        std::lock_guard<std::mutex> lock(state->mutex);
        if (!state->stopped && state->udps.count(id))
          state->post({ "datagram", id, 0, std::move(text) });
      } catch (Exception^) {}
    });
  auto remote = ref new HostName(ref new String(Wide(host).c_str()));
  auto service = ref new String(std::to_wstring(port).c_str());
  create_task(socket->ConnectAsync(remote, service)).then(
    [weak, id, socket](task<void> connected) {
      const auto state = weak.lock();
      if (!state) return;
      try { connected.get(); }
      catch (Exception^) {
        std::lock_guard<std::mutex> lock(state->mutex);
        if (state->udps.erase(id)) state->post({ "datagram-close", id, 0, "connect" });
        return;
      }
      std::lock_guard<std::mutex> lock(state->mutex);
      auto found = state->udps.find(id);
      if (found != state->udps.end()) found->second.writer = ref new DataWriter(socket->OutputStream);
    });
  return id;
}

void HostNet::udp_send(int id, std::string_view text) {
  if (text.empty() || text.size() > 2048) return;
  DataWriter^ writer = nullptr;
  {
    std::lock_guard<std::mutex> lock(state_->mutex);
    auto found = state_->udps.find(id);
    if (found == state_->udps.end() || !found->second.writer) return;
    writer = found->second.writer;
  }
  // One datagram per store: a lost or late one is the lane's business, so
  // there is no queue to build latency behind it.
  try {
    writer->WriteString(ref new String(Wide(text).c_str()));
    create_task(writer->StoreAsync()).then([](task<unsigned> stored) {
      try { stored.get(); } catch (Exception^) {}
    });
  } catch (Exception^) {}
}

void HostNet::udp_close(int id) {
  DatagramSocket^ socket = nullptr;
  {
    std::lock_guard<std::mutex> lock(state_->mutex);
    auto found = state_->udps.find(id);
    if (found == state_->udps.end()) return;
    socket = found->second.socket;
    state_->udps.erase(found);
  }
  if (socket) try { delete socket; } catch (Exception^) {}
}

std::vector<HostNetEvent> HostNet::poll() {
  std::lock_guard<std::mutex> lock(state_->mutex);
  std::vector<HostNetEvent> events;
  events.swap(state_->events);
  return events;
}

void HostNet::shutdown() {
  std::vector<MessageWebSocket^> sockets;
  {
    std::lock_guard<std::mutex> lock(state_->mutex);
    if (state_->stopped) return;
    state_->stopped = true;
    for (auto& [id, socket] : state_->sockets) if (socket.socket) sockets.push_back(socket.socket);
    state_->sockets.clear();
    state_->udps.clear();
    state_->events.clear();
  }
  for (auto socket : sockets) try { socket->Close(1000, ref new String(L"leaving")); } catch (Exception^) {}
}

}  // namespace ac::xbox
