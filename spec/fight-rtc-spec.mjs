import assert from "node:assert/strict";
import { createRtcPeer } from "../system/public/aesthetic.computer/lib/fight/rtc-core.mjs";

class MockChannel {
  constructor() { this.readyState = "connecting"; this.sent = []; }
  send(value) { this.sent.push(value); }
  close() { this.readyState = "closed"; }
}
class MockPeer {
  static last;
  constructor(config) { this.config = config; this.connectionState = "new"; MockPeer.last = this; }
  createDataChannel(label, options) { this.label = label; this.options = options; return (this.channel = new MockChannel()); }
  async createOffer() { return { type: "offer", sdp: "offer" }; }
  async createAnswer() { return { type: "answer", sdp: "answer" }; }
  async setLocalDescription(value) { this.localDescription = value; }
  async setRemoteDescription(value) { this.remoteDescription = value; }
  async addIceCandidate(value) { (this.candidates ||= []).push(value); }
  close() {}
}

const signals = [];
const packets = [];
const peer = createRtcPeer({ id: "m", initiator: true, RTCPeerConnectionImpl: MockPeer,
  signal: (value) => signals.push(value), packet: (value) => packets.push(value),
  now: () => 100, setIntervalImpl: null });
await new Promise((resolve) => setTimeout(resolve, 0));
assert.deepEqual(MockPeer.last.options, { ordered: false, maxRetransmits: 0 });
assert.equal(signals[0].description.type, "offer");
await peer.acceptSignal({ candidate: { candidate: "early" } });
assert.equal(MockPeer.last.candidates, undefined);
await peer.acceptSignal({ description: { type: "answer", sdp: "answer" } });
assert.deepEqual(MockPeer.last.candidates, [{ candidate: "early" }]);
MockPeer.last.channel.readyState = "open";
MockPeer.last.channel.onopen();
assert.equal(peer.sendPacket({ from: 0, start: 0, inputs: [1] }), true);
const envelope = JSON.parse(MockPeer.last.channel.sent.at(-1));
assert.equal(envelope.kind, "input");
MockPeer.last.channel.onmessage({ data: JSON.stringify({ kind: "input", sequence: 0,
  packet: { from: 1, start: 0, inputs: [2] } }) });
assert.deepEqual(packets[0].inputs, [2]);
MockPeer.last.channel.onmessage({ data: JSON.stringify({ kind: "input", sequence: 0,
  packet: { from: 1, start: 0, inputs: [99] } }) });
assert.equal(packets.length, 1);
peer.close();
console.log("fight rtc spec ok");
