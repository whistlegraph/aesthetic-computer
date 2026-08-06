// BIOS/main-window owner for RTCPeerConnection. Pieces can run in a worker, so
// they talk to this manager with fight:rtc:* bridge messages.

import { createRtcPeer } from "./rtc-core.mjs";

const peers = new Map();

export async function handleFightRtcMessage(type, content, send) {
  if (!type?.startsWith("fight:rtc:")) return false;
  const id = content?.id;
  if (!id) return true;
  if (type === "fight:rtc:create") {
    peers.get(id)?.close();
    try {
      const peer = createRtcPeer({
        id,
        initiator: !!content.initiator,
        iceServers: content.iceServers || [],
        signal: (data) => send({ type: "fight:rtc:event", content: { id, event: "signal", data } }),
        packet: (data) => send({ type: "fight:rtc:event", content: { id, event: "packet", data } }),
        event: ({ event, data }) => send({ type: "fight:rtc:event", content: { id, event, data } }),
      });
      peers.set(id, peer);
    } catch (error) {
      send({ type: "fight:rtc:event", content: { id, event: "state", data: { state: "error", detail: error.message } } });
    }
    return true;
  }
  const peer = peers.get(id);
  if (!peer) return true;
  try {
    if (type === "fight:rtc:signal") await peer.acceptSignal(content.signal);
    else if (type === "fight:rtc:send") peer.sendPacket(content.packet);
    else if (type === "fight:rtc:close") { peer.close(); peers.delete(id); }
    else if (type === "fight:rtc:stats") send({ type: "fight:rtc:event", content: { id, event: "health", data: peer.health() } });
  } catch (error) {
    send({ type: "fight:rtc:event", content: {
      id, event: "state", data: { state: "error", detail: error.message },
    } });
  }
  return true;
}
