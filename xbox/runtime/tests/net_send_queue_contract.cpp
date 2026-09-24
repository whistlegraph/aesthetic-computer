#include "ac/net_send_queue.hpp"
#include <cassert>
#include <iostream>
#include <map>
#include <set>

using ac::xbox::NetSendQueue;
using ac::xbox::coalescible_net_input;

std::string packet(int first, int count = 10, int origin = 123, int stamp = 1,
    const std::string& extra = "") {
  std::string json = "{\"t\":\"i\",\"o\":" + std::to_string(origin) +
    ",\"f\":" + std::to_string(first) + ",\"m\":[";
  for (int i = 0; i < count; ++i) {
    if (i) json += ',';
    json += std::to_string((first + i) % 7);
  }
  return json + "],\"a\":12,\"s\":18,\"l\":2,\"w\":" + std::to_string(stamp) +
    ",\"e\":99" + extra + "}";
}

void parser_contract() {
  const auto input = coalescible_net_input(packet(10));
  assert(input && input->first == 10 && input->masks.size() == 10);
  for (const auto& value : {
      "{}", "{\"t\":\"hello\"}", "{\"t\":\"i\",\"o\":1,\"f\":0,\"m\":[]}",
      "{\"t\":\"i\",\"o\":1,\"f\":01,\"m\":[1]}",
      "{\"t\":\"i\",\"o\":1,\"f\":0.5,\"m\":[1]}",
      "{\"t\":\"i\",\"o\":1,\"f\":-1,\"m\":[1]}",
      "{\"t\":\"i\",\"o\":1,\"f\":0,\"m\":[2147483648]}",
      "{\"t\":\"i\",\"o\":1,\"f\":0,\"m\":[1],}",
      "{\"t\":\"i\",\"o\":1,\"f\":0,\"m\":[1]}garbage"})
    assert(!coalescible_net_input(value));
  assert(!coalescible_net_input(packet(0, 21)));
  assert(!coalescible_net_input(packet(0, 10, 123, 1, ",\"h\":[0,123]")));
  assert(!coalescible_net_input(packet(0, 10, 123, 1, ",\"trace\":\"debug\"")));
  assert(!coalescible_net_input(packet(0, 10, 123, 1, ",\"f\":2")));
  assert(coalescible_net_input(" { \"m\": [1,-2], \"f\": 0, \"o\": 123, \"t\": \"i\" } "));
  assert(!coalescible_net_input("{\"t\":\"i\",\"o\":1,\"f\":9007199254740991,\"m\":[1]}"));
}

void preservation_contract() {
  NetSendQueue queue;
  assert(queue.push(packet(0)));
  assert(queue.push(packet(2, 10, 123, 222)));
  assert(queue.size() == 1 && queue.coalesced() == 1);
  const auto combined = queue.pop();
  const auto parsed = coalescible_net_input(combined);
  assert(parsed && parsed->first == 0 && parsed->masks.size() == 12);
  for (int i = 0; i < 12; ++i) assert(parsed->masks[i] == i % 7);
  assert(combined.find("\"w\":222") != std::string::npos);
  assert(combined.find("\"a\":12,\"s\":18,\"l\":2") != std::string::npos);
  assert(combined.find("\"e\":99") != std::string::npos);
  // Both JSON key orders are supported without modifying untouched metadata.
  assert(queue.push("{\"t\":\"i\",\"m\":[1,2],\"f\":0,\"o\":123}"));
  assert(queue.push("{\"t\":\"i\",\"m\":[2,3],\"f\":1,\"o\":123}"));
  assert(queue.pop() == "{\"t\":\"i\",\"m\":[1,2,3],\"f\":0,\"o\":123}");
  // A long pause exceeds ten-frame redundancy: the oldest unique frame must
  // survive, even when that means retaining another FIFO entry.
  for (int first = 0; first < 100; ++first) assert(queue.push(packet(first)));
  std::map<int, int> received;
  while (!queue.empty()) {
    const auto item = coalescible_net_input(queue.pop());
    assert(item && item->masks.size() <= 20);
    for (std::size_t i = 0; i < item->masks.size(); ++i)
      received.emplace(static_cast<int>(item->first + i), item->masks[i]);
  }
  assert(received.size() == 109);
  for (int i = 0; i < 109; ++i) assert(received.at(i) == i % 7);
}

void barrier_contract() {
  const auto old = packet(0), next = packet(1);
  for (const auto& barrier : {"{\"t\":\"hello\"}", "{\"t\":\"start\"}",
      "{\"t\":\"bye\"}", "{\"t\":\"desync\"}", "{\"t\":\"stage\"}",
      "{\"t\":\"stageAck\"}", "{\"t\":\"identity\"}", "not json"}) {
    NetSendQueue queue;
    assert(queue.push(old) && queue.push(barrier) && queue.push(next));
    assert(queue.size() == 3);
    assert(queue.pop() == old && queue.pop() == barrier && queue.pop() == next);
  }
  for (const auto& other : {packet(1, 10, 124), packet(11), packet(0, 10, 123, 1, ",\"h\":[0,12]"),
      packet(0, 10, 123, 1, ",\"trace\":\"debug\""),
      std::string("{\"t\":\"i\",\"o\":123,\"f\":1,\"m\":[99,99,99,99,99,99,99,99,99,99]}")}) {
    NetSendQueue queue;
    assert(queue.push(old) && queue.push(other));
    assert(queue.size() == 2 && queue.pop() == old && queue.pop() == other);
  }
  NetSendQueue queue;
  assert(queue.push(next) && queue.push(old)); // backwards frame range
  assert(queue.size() == 2);
  queue.clear();
  for (int i = 0; i < 32; ++i) assert(queue.push("{\"t\":\"hello\"}"));
  assert(!queue.push(next) && queue.size() == 32);
  queue.clear(); assert(queue.empty() && queue.coalesced() == 0);
  auto maximum = next;
  maximum.append(7168 - maximum.size(), ' ');
  assert(queue.push(old) && queue.push(maximum));
  assert(queue.size() == 2 && queue.pop() == old && queue.pop() == maximum);
}

// Deterministic serialization benchmark, not a claim about device FPS. One
// StoreAsync takes 25ms; 60Hz simulation produces rolling ten-frame histories.
struct Metrics { int high_water = 0, rejected = 0, writes = 0; double lag_us = 0; };
Metrics congested_writer(bool coalesce) {
  NetSendQueue compact;
  std::deque<std::string> fifo;
  Metrics result;
  std::string inflight;
  std::set<int> received;
  int next_complete = 0, produced = 0, next_produce = 0;
  const auto size = [&] { return static_cast<int>(coalesce ? compact.size() : fifo.size()); };
  for (int now = 0; now < 40000000; now += 1000) {
    if (now < 30000000 && now >= next_produce) {
      const int frame = produced++;
      const int first = std::max(0, frame - 9);
      auto value = packet(first, frame - first + 1, 123, now);
      bool accepted;
      if (coalesce) accepted = compact.push(value);
      else { accepted = fifo.size() < 32; if (accepted) fifo.push_back(value); }
      if (!accepted) ++result.rejected;
      next_produce += 16667;
      result.high_water = std::max(result.high_water, size());
    }
    if (!inflight.empty() && now >= next_complete) {
      const auto input = coalescible_net_input(inflight);
      assert(input);
      const auto newest = input->first + static_cast<int>(input->masks.size()) - 1;
      result.lag_us += now - newest * 16667;
      ++result.writes;
      for (std::size_t i = 0; i < input->masks.size(); ++i)
        received.insert(static_cast<int>(input->first + i));
      inflight.clear();
    }
    if (inflight.empty() && size()) {
      if (coalesce) inflight = compact.pop();
      else { inflight = std::move(fifo.front()); fifo.pop_front(); }
      next_complete = now + 25000;
    }
  }
  if (coalesce) assert(received.size() == static_cast<std::size_t>(produced));
  result.lag_us /= result.writes;
  return result;
}

int main() {
  parser_contract(); preservation_contract(); barrier_contract();
  const auto fifo = congested_writer(false), compact = congested_writer(true);
  assert(fifo.high_water == 32 && fifo.rejected > 0);
  assert(compact.high_water <= 2 && compact.rejected == 0);
  assert(compact.lag_us < fifo.lag_us / 5);
  std::cout << "25ms serialized writer / 60Hz inputs / 30s simulation\n"
    << "FIFO: queue=" << fifo.high_water << " rejected=" << fifo.rejected
    << " mean input age=" << fifo.lag_us / 1000 << "ms\n"
    << "Merged: queue=" << compact.high_water << " rejected=" << compact.rejected
    << " mean input age=" << compact.lag_us / 1000 << "ms\n";
}
