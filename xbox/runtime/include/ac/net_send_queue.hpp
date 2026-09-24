#pragma once

#include <algorithm>
#include <charconv>
#include <cstdint>
#include <deque>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

namespace ac::xbox {

// Only the current, flat input packet is eligible. Unknown fields (including
// hashes and diagnostic extensions), malformed JSON and control packets stay
// opaque FIFO entries. This deliberately is not a general JSON parser.
struct CoalescibleNetInput {
  std::int64_t origin = 0, first = 0;
  std::vector<std::int32_t> masks;
  std::size_t first_begin = 0, first_end = 0, masks_begin = 0, masks_end = 0;
};

inline std::optional<CoalescibleNetInput> coalescible_net_input(std::string_view json) {
  if (json.size() > 7168) return std::nullopt;
  std::size_t at = 0;
  const auto space = [&] { while (at < json.size() &&
    (json[at] == ' ' || json[at] == '\t' || json[at] == '\r' || json[at] == '\n')) ++at; };
  const auto token = [&](char value) {
    space();
    if (at >= json.size() || json[at] != value) return false;
    ++at; return true;
  };
  const auto string = [&]() -> std::optional<std::string_view> {
    if (!token('"')) return std::nullopt;
    const auto start = at;
    while (at < json.size() && json[at] != '"') {
      if (json[at] == '\\' || static_cast<unsigned char>(json[at]) < 32) return std::nullopt;
      ++at;
    }
    if (at == json.size()) return std::nullopt;
    return json.substr(start, at++ - start);
  };
  const auto integer = [&]() -> std::optional<std::int64_t> {
    space(); const auto start = at;
    if (at < json.size() && json[at] == '-') ++at;
    const auto digits = at;
    while (at < json.size() && json[at] >= '0' && json[at] <= '9') ++at;
    if (at == digits || (at - digits > 1 && json[digits] == '0')) return std::nullopt;
    std::int64_t value = 0;
    const auto result = std::from_chars(json.data() + start, json.data() + at, value);
    if (result.ec != std::errc{} || value < -9007199254740991LL || value > 9007199254740991LL)
      return std::nullopt;
    return value;
  };
  CoalescibleNetInput input;
  unsigned seen = 0;
  if (!token('{')) return std::nullopt;
  for (;;) {
    const auto key = string();
    if (!key || !token(':')) return std::nullopt;
    unsigned bit = 0;
    if (*key == "t") bit = 1;
    else if (*key == "o") bit = 2;
    else if (*key == "f") bit = 4;
    else if (*key == "m") bit = 8;
    else if (*key == "a") bit = 16;
    else if (*key == "s") bit = 32;
    else if (*key == "l") bit = 64;
    else if (*key == "w") bit = 128;
    else if (*key == "e") bit = 256;
    else return std::nullopt;
    if (seen & bit) return std::nullopt;
    seen |= bit;
    space(); const auto start = at;
    if (bit == 1) {
      const auto type = string();
      if (!type || *type != "i") return std::nullopt;
    } else if (bit == 8) {
      if (!token('[')) return std::nullopt;
      do {
        const auto value = integer();
        if (!value || *value < INT32_MIN || *value > INT32_MAX || input.masks.size() >= 20)
          return std::nullopt;
        input.masks.push_back(static_cast<std::int32_t>(*value));
      } while (token(','));
      if (!token(']')) return std::nullopt;
      input.masks_begin = start; input.masks_end = at;
    } else {
      const auto value = integer();
      if (!value) return std::nullopt;
      if (bit == 2) input.origin = *value;
      if (bit == 4) {
        input.first = *value;
        input.first_begin = start; input.first_end = at;
      }
    }
    if (token('}')) break;
    if (!token(',')) return std::nullopt;
  }
  space();
  if (at != json.size() || (seen & 15) != 15 || input.first < 0 || input.origin < 0 ||
      input.first > 9007199254740991LL - static_cast<std::int64_t>(input.masks.size()))
    return std::nullopt;
  return input;
}

class NetSendQueue {
 public:
  static constexpr std::size_t capacity = 32;
  static constexpr std::size_t max_history = 20;  // receiver NET_REDUNDANCY * 2

  bool push(std::string json) {
    auto input = coalescible_net_input(json);
    if (input && !pending_.empty() && pending_.back().input) {
      const auto& previous = *pending_.back().input;
      const auto last = input->first + static_cast<std::int64_t>(input->masks.size());
      const auto previous_last = previous.first + static_cast<std::int64_t>(previous.masks.size());
      // Never cross a gap, move backwards, or discard a frame. The new
      // packet replaces only the adjacent tail; controls/hashes are barriers.
      if (input->origin == previous.origin && input->first >= previous.first &&
          input->first <= previous_last && last >= previous_last &&
          last - previous.first <= static_cast<std::int64_t>(max_history)) {
        bool agrees = true;
        for (auto frame = input->first; frame < previous_last; ++frame)
          agrees &= previous.masks[static_cast<std::size_t>(frame - previous.first)] ==
            input->masks[static_cast<std::size_t>(frame - input->first)];
        if (agrees) {
          std::vector<std::int32_t> union_masks = previous.masks;
          union_masks.insert(union_masks.end(), input->masks.begin() +
            static_cast<std::size_t>(previous_last - input->first), input->masks.end());
          std::string masks = "[";
          for (auto mask : union_masks) {
            if (masks.size() > 1) masks += ',';
            masks += std::to_string(mask);
          }
          masks += ']';
          // Preserve every newest metadata field verbatim. Replace values
          // back-to-front so JSON key ordering cannot invalidate offsets.
          const auto first = std::to_string(previous.first);
          auto merged = json;
          if (input->first_begin > input->masks_begin) {
            merged.replace(input->first_begin, input->first_end - input->first_begin, first);
            merged.replace(input->masks_begin, input->masks_end - input->masks_begin, masks);
          } else {
            merged.replace(input->masks_begin, input->masks_end - input->masks_begin, masks);
            merged.replace(input->first_begin, input->first_end - input->first_begin, first);
          }
          if (merged.size() <= 7168) {
            pending_.back() = { std::move(merged), std::nullopt };
            pending_.back().input = coalescible_net_input(pending_.back().json);
            ++coalesced_;
            return true;
          }
        }
      }
    }
    if (pending_.size() >= capacity) return false;
    pending_.push_back({std::move(json), std::move(input)});
    return true;
  }

  std::string pop() {
    auto json = std::move(pending_.front().json);
    pending_.pop_front();
    return json;
  }
  bool empty() const { return pending_.empty(); }
  std::size_t size() const { return pending_.size(); }
  std::uint64_t coalesced() const { return coalesced_; }
  void clear() { pending_.clear(); coalesced_ = 0; }

 private:
  struct Entry { std::string json; std::optional<CoalescibleNetInput> input; };
  std::deque<Entry> pending_;
  std::uint64_t coalesced_ = 0;
};

}  // namespace ac::xbox
