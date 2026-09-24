#include "pch.h"
#include "OskiewarLivePublisher.hpp"
#include "../runtime/include/ac/relay_endpoint.hpp"
#include <fstream>

using namespace Platform;
using namespace Windows::Foundation;
using namespace Windows::Networking::Sockets;
using namespace Windows::Storage::Streams;
using namespace concurrency;

namespace ac::xbox {
namespace {

std::wstring RelayBase() {
#if AC_DEV_LIVE_PIECE
  try {
    const std::wstring path = std::wstring(Windows::Storage::ApplicationData::Current->LocalFolder->Path->Data()) +
      L"\\oskiewar-relay.txt";
    std::ifstream input(path, std::ios::binary);
    char buffer[202]{};
    if (input.getline(buffer, sizeof(buffer))) {
      std::string value(buffer);
      if (!value.empty() && value.back()=='\r') value.pop_back();
      if (valid_lan_relay(value)) return std::wstring(value.begin(),value.end());
    }
  } catch (...) {}
#endif
  return L"wss://session-server.aesthetic.computer/oskiewar-live";
}

std::wstring Wide(std::string_view value) {
  if (value.empty()) return {};
  const int size = MultiByteToWideChar(CP_UTF8, 0, value.data(),
    static_cast<int>(value.size()), nullptr, 0);
  std::wstring result(static_cast<std::size_t>(size), L'\0');
  MultiByteToWideChar(CP_UTF8, 0, value.data(), static_cast<int>(value.size()),
    result.data(), size);
  return result;
}

bool ValidMatchId(std::string_view value) {
  if (value.substr(0, 3) != "ow-") return false;
  // Legacy three-word IDs remain durable.
  if (value.size() == 23) {
    for (std::size_t index = 3; index < value.size(); ++index) {
      const auto c = value[index];
      if (!((c >= 'a' && c <= 'z') || c == '-')) return false;
    }
    return value[9] == '-' && value[16] == '-';
  }
  // New Meet-like IDs are one speakable stem plus one to three digits.
  const auto name = value.substr(3);
  std::size_t digit = 0;
  while (digit < name.size() && name[digit] >= 'a' && name[digit] <= 'z') ++digit;
  if (digit < 4 || digit > 7 || name.size() - digit < 1 ||
      name.size() - digit > 3) return false;
  for (; digit < name.size(); ++digit)
    if (name[digit] < '0' || name[digit] > '9') return false;
  return true;
}

}  // namespace

struct OskiewarLivePublisher::State {
  std::mutex mutex;
  Logger logger;
  MessageWebSocket^ socket = nullptr;
  DataWriter^ writer = nullptr;
  Windows::Foundation::EventRegistrationToken message_token{};
  Windows::Foundation::EventRegistrationToken closed_token{};
  bool subscribed = false;
  bool connecting = false;
  bool connected = false;
  bool writing = false;
  bool stopped = false;
  std::uint64_t generation = 0;
  std::string match_id;
  std::string pending;
  std::deque<std::string> net_pending;
  std::vector<std::string> net_inbox;
};

namespace {

void Log(const std::shared_ptr<OskiewarLivePublisher::State>& state,
    const std::string& line) {
  if (state->logger) state->logger(line);
}

void Flush(const std::shared_ptr<OskiewarLivePublisher::State>& state) {
  std::string payload;
  DataWriter^ writer = nullptr;
  std::uint64_t generation = 0;
  {
    std::lock_guard<std::mutex> lock(state->mutex);
    if (state->stopped || !state->connected || state->writing ||
        (state->pending.empty() && state->net_pending.empty()) || !state->writer) return;
    if (!state->net_pending.empty()) {
      payload = std::move(state->net_pending.front());
      state->net_pending.pop_front();
    } else {
      payload = std::move(state->pending);
      state->pending.clear();
    }
    state->writing = true;
    writer = state->writer;
    generation = state->generation;
  }
  try {
    writer->WriteString(ref new String(Wide(payload).c_str()));
    std::weak_ptr<OskiewarLivePublisher::State> weak = state;
    create_task(writer->StoreAsync()).then([weak, generation](task<unsigned> completed) {
      const auto state = weak.lock();
      if (!state) return;
      try { completed.get(); }
      catch (Exception^ error) {
        Log(state, "AC_NATIVE_OSKIEWAR_LIVE_SEND_ERROR");
      }
      {
        std::lock_guard<std::mutex> lock(state->mutex);
        if (generation != state->generation) return;
        state->writing = false;
      }
      Flush(state);
    });
  } catch (Exception^) {
    std::lock_guard<std::mutex> lock(state->mutex);
    if (generation == state->generation) state->writing = false;
    Log(state, "AC_NATIVE_OSKIEWAR_LIVE_SEND_ERROR");
  }
}

void Connect(const std::shared_ptr<OskiewarLivePublisher::State>& state) {
  std::string match_id;
  std::uint64_t generation = 0;
  MessageWebSocket^ socket = nullptr;
  {
    std::lock_guard<std::mutex> lock(state->mutex);
    if (state->stopped || state->connecting || state->connected ||
        state->match_id.empty()) return;
    state->connecting = true;
    generation = state->generation;
    match_id = state->match_id;
    socket = ref new MessageWebSocket();
    socket->Control->MessageType = SocketMessageType::Utf8;
    state->socket = socket;
  }
  std::weak_ptr<OskiewarLivePublisher::State> weak = state;
  const auto message_token = socket->MessageReceived +=
    ref new TypedEventHandler<MessageWebSocket^, MessageWebSocketMessageReceivedEventArgs^>(
      [weak, generation](MessageWebSocket^, MessageWebSocketMessageReceivedEventArgs^ args) {
        try {
          auto reader = args->GetDataReader();
          reader->UnicodeEncoding = UnicodeEncoding::Utf8;
          const auto size = reader->UnconsumedBufferLength;
          if (!size || size > 16384) return;
          auto message = Windows::Data::Json::JsonObject::Parse(reader->ReadString(size));
          if (message->GetNamedString("type", "") != "oskiewar:net") return;
          auto content = message->GetNamedObject("content")->Stringify();
          const int count = WideCharToMultiByte(CP_UTF8, 0, content->Data(),
            content->Length(), nullptr, 0, nullptr, nullptr);
          if (count < 2 || count > 7168) return;
          std::string packet(count, '\0');
          WideCharToMultiByte(CP_UTF8, 0, content->Data(), content->Length(),
            packet.data(), count, nullptr, nullptr);
          const auto state = weak.lock();
          if (!state) return;
          std::lock_guard<std::mutex> lock(state->mutex);
          if (state->stopped || generation != state->generation) return;
          if (state->net_inbox.size() >= 64) state->net_inbox.erase(state->net_inbox.begin());
          state->net_inbox.push_back(std::move(packet));
        } catch (Exception^) {}
      });
  const auto closed_token = socket->Closed +=
    ref new TypedEventHandler<IWebSocket^, WebSocketClosedEventArgs^>(
      [weak, generation](IWebSocket^, WebSocketClosedEventArgs^) {
        const auto state = weak.lock();
        if (!state) return;
        std::lock_guard<std::mutex> lock(state->mutex);
        if (generation != state->generation) return;
        state->connected = false;
        state->connecting = false;
        state->writing = false;
        state->writer = nullptr;
        state->socket = nullptr;
        state->subscribed = false;
      });
  {
    std::lock_guard<std::mutex> lock(state->mutex);
    if (generation != state->generation) return;
    state->message_token = message_token;
    state->closed_token = closed_token;
    state->subscribed = true;
  }
  const auto base = RelayBase();
  Log(state, std::string("AC_NATIVE_OSKIEWAR_RELAY route=") +
    (base.substr(0,5)==L"ws://" ? "lan" : "cloud"));
  const auto url = base + L"?match=" +
    Wide(match_id) + L"&role=publisher&surface=xbox";
  create_task(socket->ConnectAsync(ref new Uri(ref new String(url.c_str())))).then(
    [weak, generation, socket](task<void> completed) {
      const auto state = weak.lock();
      if (!state) return;
      try { completed.get(); }
      catch (Exception^) {
        std::lock_guard<std::mutex> lock(state->mutex);
        if (generation == state->generation) {
          state->connecting = false;
          state->connected = false;
          state->socket = nullptr;
        }
        Log(state, "AC_NATIVE_OSKIEWAR_LIVE_CONNECT_ERROR");
        return;
      }
      {
        std::lock_guard<std::mutex> lock(state->mutex);
        if (state->stopped || generation != state->generation) return;
        state->writer = ref new DataWriter(socket->OutputStream);
        state->connecting = false;
        state->connected = true;
      }
      Log(state, "AC_NATIVE_OSKIEWAR_LIVE_CONNECTED match=" + state->match_id);
      Flush(state);
    });
}

}  // namespace

OskiewarLivePublisher::OskiewarLivePublisher(Logger logger)
    : state_(std::make_shared<State>()) {
  state_->logger = std::move(logger);
}

OskiewarLivePublisher::~OskiewarLivePublisher() { shutdown(); }

void OskiewarLivePublisher::publish(std::string_view match_id,
    std::string_view state_json) {
  if (!ValidMatchId(match_id) || state_json.size() < 2 ||
      state_json.size() > 7168 || state_json.front() != '{') return;
  bool should_connect = false;
  MessageWebSocket^ old_socket = nullptr;
  {
    std::lock_guard<std::mutex> lock(state_->mutex);
    if (state_->stopped) return;
    if (state_->match_id != match_id) {
      old_socket = state_->socket;
      ++state_->generation;
      state_->match_id.assign(match_id);
      state_->net_pending.clear();
      state_->net_inbox.clear();
      state_->writer = nullptr;
      state_->socket = nullptr;
      state_->connecting = false;
      state_->connected = false;
      state_->writing = false;
    }
    // Latest-only queue: a slow network never builds latency or memory.
    state_->pending = std::string("{\"type\":\"oskiewar:state\",\"content\":") +
      std::string(state_json) + "}";
    should_connect = !state_->connecting && !state_->connected;
  }
  if (old_socket) try {
    old_socket->Close(1000, ref new String(L"new match"));
  } catch (Exception^) {}
  if (should_connect) Connect(state_);
  else Flush(state_);
}

bool OskiewarLivePublisher::send_net(std::string_view match_id,
    std::string_view packet_json) {
  if (!ValidMatchId(match_id) || packet_json.size() < 2 ||
      packet_json.size() > 7168 || packet_json.front() != '{') return false;
  // Opening is asynchronous. The piece retries its hello until connected.
  bool open = false;
  {
    std::lock_guard<std::mutex> lock(state_->mutex);
    if (state_->stopped) return false;
    open = state_->match_id != match_id;
  }
  if (open) publish(match_id, "{}");
  Connect(state_);
  {
    std::lock_guard<std::mutex> lock(state_->mutex);
    if (!state_->connected || state_->net_pending.size() >= 32) return false;
    state_->net_pending.push_back(std::string("{\"type\":\"oskiewar:net\",\"content\":") +
      std::string(packet_json) + "}");
  }
  Flush(state_);
  return true;
}

std::vector<std::string> OskiewarLivePublisher::poll_net() {
  std::lock_guard<std::mutex> lock(state_->mutex);
  std::vector<std::string> packets;
  packets.swap(state_->net_inbox);
  return packets;
}

void OskiewarLivePublisher::shutdown() {
  MessageWebSocket^ socket = nullptr;
  {
    std::lock_guard<std::mutex> lock(state_->mutex);
    if (state_->stopped) return;
    state_->stopped = true;
    ++state_->generation;
    state_->pending.clear();
    state_->net_pending.clear();
    state_->net_inbox.clear();
    socket = state_->socket;
    state_->socket = nullptr;
    state_->writer = nullptr;
  }
  if (socket) try {
    socket->Close(1000, ref new String(L"leaving"));
  } catch (Exception^) {}
}

}  // namespace ac::xbox
