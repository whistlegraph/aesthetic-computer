#pragma once

#include <functional>
#include <memory>
#include <string>
#include <string_view>

namespace ac::xbox {

// Fixed-destination, latest-only WebSocket transport for OSKIEWAR live state.
// This deliberately does not expose a general socket to downloaded pieces.
class OskiewarLivePublisher final {
 public:
  using Logger = std::function<void(const std::string&)>;
  struct State;

  explicit OskiewarLivePublisher(Logger logger = {});
  ~OskiewarLivePublisher();
  OskiewarLivePublisher(const OskiewarLivePublisher&) = delete;
  OskiewarLivePublisher& operator=(const OskiewarLivePublisher&) = delete;

  void publish(std::string_view match_id, std::string_view state_json);
  void shutdown();

 private:
  std::shared_ptr<State> state_;
};

}  // namespace ac::xbox
