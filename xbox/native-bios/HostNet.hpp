#pragma once

#include <functional>
#include <memory>
#include <string>
#include <string_view>
#include <vector>

namespace ac::xbox {

// What the web shell gives the game for free and the console does not: a
// WebSocket, a GET fetch and a UDP datagram lane, so the room bridge
// (round-room.mjs) runs here unchanged, the way it does in the Mac app
// (xbox/macos-native/native-shell.js). Every endpoint is held to
// aesthetic.computer, oskiewar.com and their subdomains — a piece deployed
// to the console still gets no door to anywhere else. Results are queued on
// the network threads and drained on the JS thread once a frame.
struct HostNetEvent {
  std::string kind;   // open | message | close | error | fetch | datagram | datagram-close
  int id = 0;
  int status = 0;     // fetch: HTTP status, or -1 for a transport failure
  std::string text;
};

class HostNet final {
 public:
  using Logger = std::function<void(const std::string&)>;
  struct State;

  explicit HostNet(Logger logger = {});
  ~HostNet();
  HostNet(const HostNet&) = delete;
  HostNet& operator=(const HostNet&) = delete;

  int socket_open(std::string_view url);
  void socket_send(int id, std::string_view text);
  void socket_close(int id);
  void fetch(int id, std::string_view url);
  int udp_open(std::string_view host, int port);
  void udp_send(int id, std::string_view text);
  void udp_close(int id);
  std::vector<HostNetEvent> poll();
  void shutdown();

  // The allowlist, for the binding and the publisher's relay choice.
  static bool allowed_host(std::string_view host);
  static bool allowed_url(std::string_view url, bool socket);

 private:
  std::shared_ptr<State> state_;
};

}  // namespace ac::xbox
