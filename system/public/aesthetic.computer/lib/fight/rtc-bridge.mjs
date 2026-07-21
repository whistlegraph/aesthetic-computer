// Worker-safe side of the BIOS WebRTC bridge.

export function createRtcBridge({ id, initiator, send, iceServers = [], onSignal, onPacket, onState, onHealth, onRoute }) {
  send({ type: "fight:rtc:create", content: { id, initiator, iceServers } });
  return {
    id,
    accept(event) {
      if (event?.id !== id) return false;
      if (event.event === "signal") onSignal?.(event.data);
      else if (event.event === "packet") onPacket?.(event.data);
      else if (event.event === "state") onState?.(event.data);
      else if (event.event === "health") onHealth?.(event.data);
      else if (event.event === "route") onRoute?.(event.data);
      return true;
    },
    signal(value) { send({ type: "fight:rtc:signal", content: { id, signal: value } }); },
    sendPacket(packet) { send({ type: "fight:rtc:send", content: { id, packet } }); },
    stats() { send({ type: "fight:rtc:stats", content: { id } }); },
    close() { send({ type: "fight:rtc:close", content: { id } }); },
  };
}

