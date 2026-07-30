#pragma once

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <string>
#include <string_view>
#include <utility>

namespace ac::xbox::ota {

// Parsed from the signed release manifest by the platform adapter. The exact
// bytes in signed_payload are what the embedded public key must verify.
struct Release {
  std::uint32_t schema = 1;
  std::string channel;
  std::string slug;
  std::string version;
  std::string source_url;
  std::string source_sha256;
  std::uint64_t source_bytes = 0;
  std::uint64_t sequence = 0;
  std::int64_t expires_unix_ms = 0;
  std::string key_id;
  std::string signed_payload;
  std::string signature_base64;
};

struct Slot {
  std::string name;
  std::string version;
  std::string source_sha256;
  std::uint64_t sequence = 0;
  bool confirmed = false;
};

class Platform {
 public:
  virtual ~Platform() = default;
  virtual bool verify_signature(std::string_view key_id,
                                std::string_view payload,
                                std::string_view signature_base64) = 0;
  virtual std::optional<std::string> fetch_https(std::string_view url,
                                                 std::size_t max_bytes) = 0;
  virtual std::string sha256(std::string_view bytes) = 0;
  virtual std::optional<Slot> active_slot() = 0;
  virtual std::optional<Slot> fallback_slot() = 0;
  // Must write and fsync the inactive slot before atomically replacing the
  // active-slot pointer. A failed call must leave the prior pointer intact.
  virtual bool atomic_install(const Release&, std::string_view source) = 0;
  virtual bool confirm_active() = 0;
  virtual bool rollback_to_fallback() = 0;
  virtual void telemetry(std::string_view event, std::string_view detail) = 0;
};

enum class Result { no_update, staged, rejected, rolled_back };

struct Policy {
  std::string channel = "stable";
  std::size_t max_source_bytes = 2 * 1024 * 1024;
  std::uint32_t healthy_frames = 300; // five seconds at 60 fps
};

// Pure orchestration: networking, Ed25519, JSON parsing and durable storage
// stay in the UWP adapter and can be tested independently.
class Coordinator {
 public:
  explicit Coordinator(Platform& platform, Policy policy = {})
      : platform_(platform), policy_(std::move(policy)) {}

  Result offer(const Release& release, std::int64_t now_unix_ms) {
    if (release.schema != 1 || release.channel != policy_.channel ||
        release.slug.empty() || release.version.empty() || release.sequence == 0 ||
        release.source_bytes == 0 || release.source_bytes > policy_.max_source_bytes ||
        release.expires_unix_ms <= now_unix_ms ||
        release.source_url.rfind("https://", 0) != 0 ||
        release.source_sha256.size() != 64 || release.signed_payload.empty() ||
        !platform_.verify_signature(release.key_id, release.signed_payload,
                                    release.signature_base64)) {
      platform_.telemetry("ota.reject", "manifest");
      return Result::rejected;
    }
    const auto active = platform_.active_slot();
    if (active && active->version == release.version) return Result::no_update;
    const auto durable_sequence = active ? active->sequence : 0;
    if (release.sequence <= (std::max)(accepted_sequence_, durable_sequence)) {
      platform_.telemetry("ota.reject", "sequence");
      return Result::rejected;
    }
    auto source = platform_.fetch_https(release.source_url, policy_.max_source_bytes);
    if (!source || source->size() != release.source_bytes ||
        platform_.sha256(*source) != release.source_sha256) {
      platform_.telemetry("ota.reject", "payload");
      return Result::rejected;
    }
    if (!platform_.atomic_install(release, *source)) {
      platform_.telemetry("ota.reject", "install");
      return Result::rejected;
    }
    accepted_sequence_ = release.sequence;
    probation_version_ = release.version;
    healthy_frames_ = 0;
    platform_.telemetry("ota.staged", release.version);
    return Result::staged;
  }

  void healthy_frame() {
    if (!probation_version_) return;
    if (++healthy_frames_ < policy_.healthy_frames) return;
    if (platform_.confirm_active()) {
      platform_.telemetry("ota.confirmed", *probation_version_);
      probation_version_.reset();
    }
  }

  Result runtime_failure(std::string_view reason) {
    if (!probation_version_) return Result::no_update;
    const auto version = *probation_version_;
    probation_version_.reset();
    healthy_frames_ = 0;
    if (platform_.rollback_to_fallback()) {
      platform_.telemetry("ota.rollback", version + ":" + std::string(reason));
      return Result::rolled_back;
    }
    platform_.telemetry("ota.rollback_failed", version);
    return Result::rejected;
  }

  [[nodiscard]] bool probation() const noexcept { return probation_version_.has_value(); }

 private:
  Platform& platform_;
  Policy policy_;
  std::uint64_t accepted_sequence_ = 0;
  std::uint32_t healthy_frames_ = 0;
  std::optional<std::string> probation_version_;
};

} // namespace ac::xbox::ota
