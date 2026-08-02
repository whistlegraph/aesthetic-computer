#include "ac/ota.hpp"
#include <cassert>
#include <unordered_map>
using namespace ac::xbox::ota;

class FakePlatform final : public Platform {
 public:
  bool signature_ok = true, install_ok = true, confirm_ok = true, rollback_ok = true;
  std::string payload = "paint(){}";
  std::string digest = std::string(64, 'a');
  std::optional<Slot> active = Slot{"a", "v1", "old", 1, true};
  std::optional<Slot> fallback;
  std::string events;
  bool verify_signature(std::string_view, std::string_view, std::string_view) override { return signature_ok; }
  std::optional<std::string> fetch_https(std::string_view, std::size_t max) override {
    if (payload.size() > max) return {}; return payload;
  }
  std::string sha256(std::string_view) override { return digest; }
  std::optional<Slot> active_slot() override { return active; }
  std::optional<Slot> fallback_slot() override { return fallback; }
  bool atomic_install(const Release& r, std::string_view) override {
    if (!install_ok) return false; fallback = active; active = Slot{"b", r.version, r.source_sha256, r.sequence, false}; return true;
  }
  bool confirm_active() override { if (!confirm_ok) return false; active->confirmed = true; return true; }
  bool rollback_to_fallback() override { if (!rollback_ok || !fallback) return false; active = fallback; return true; }
  void telemetry(std::string_view event, std::string_view) override { events += std::string(event) + "\n"; }
};

static Release release(const FakePlatform& p, std::uint64_t sequence = 2) {
  return {1, "stable", "nopaint", "v2", "https://updates.aesthetic.computer/nopaint-v2.js",
    p.digest, p.payload.size(), sequence, 2000, "xbox-2026", "canonical", "signature"};
}

int main() {
  FakePlatform p; Coordinator ota(p, Policy{"stable", 1024, 3});
  assert(ota.offer(release(p), 1000) == Result::staged);
  assert(ota.probation() && p.active->version == "v2" && !p.active->confirmed);
  ota.healthy_frame(); ota.healthy_frame(); assert(!p.active->confirmed);
  ota.healthy_frame(); assert(p.active->confirmed && !ota.probation());

  auto v3 = release(p, 3); v3.version = "v3";
  assert(ota.offer(v3, 1000) == Result::staged);
  assert(ota.runtime_failure("paint") == Result::rolled_back);
  assert(p.active->version == "v2");

  auto replay = release(p, 2); replay.version = "replay";
  assert(ota.offer(replay, 1000) == Result::rejected);
  auto bad = release(p, 4); bad.version = "bad"; p.signature_ok = false;
  assert(ota.offer(bad, 1000) == Result::rejected);
  p.signature_ok = true; bad.source_url = "http://unsafe";
  assert(ota.offer(bad, 1000) == Result::rejected);
  bad.source_url = "https://safe"; bad.expires_unix_ms = 999;
  assert(ota.offer(bad, 1000) == Result::rejected);
}
