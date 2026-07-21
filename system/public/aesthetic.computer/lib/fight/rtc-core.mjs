// Main-thread WebRTC peer. The constructor and clocks are injectable so the
// signaling/channel behavior can be tested in Node without browser globals.

function rolling(values, value, max = 120) {
  values.push(value);
  if (values.length > max) values.shift();
}

function average(values) {
  return values.length ? values.reduce((sum, value) => sum + value, 0) / values.length : 0;
}

export function createRtcPeer({
  id,
  initiator = false,
  iceServers = [],
  signal = () => {},
  event = () => {},
  packet = () => {},
  RTCPeerConnectionImpl = globalThis.RTCPeerConnection,
  now = () => performance.now(),
  setIntervalImpl = globalThis.setInterval?.bind(globalThis),
  clearIntervalImpl = globalThis.clearInterval?.bind(globalThis),
} = {}) {
  if (!RTCPeerConnectionImpl) throw new Error("WebRTC is unavailable on this surface");
  const pc = new RTCPeerConnectionImpl({ iceServers });
  let channel = null;
  let sequence = 0;
  let highestReceived = -1;
  let expectedReceived = 0;
  let latePackets = 0;
  const recentSequences = new Set();
  const sequenceOrder = [];
  const pendingCandidates = [];
  let timer = null;
  const rtts = [];
  const arrivals = [];
  const pendingPings = new Map();
  const counters = { sent: 0, received: 0 };

  const emitState = (state, detail) => event({ id, event: "state", data: { state, detail } });

  async function refreshRoute() {
    if (!pc.getStats) return;
    try {
      const report = await pc.getStats();
      let pair = null;
      const records = new Map();
      report.forEach?.((value, key) => records.set(key, value));
      for (const value of records.values()) {
        if (value.type === "candidate-pair" && value.state === "succeeded" &&
            (value.nominated || value.selected)) pair = value;
      }
      const local = pair && records.get(pair.localCandidateId);
      const remote = pair && records.get(pair.remoteCandidateId);
      const relay = local?.candidateType === "relay" || remote?.candidateType === "relay";
      event({ id, event: "route", data: { type: relay ? "relay" : "direct",
        localCandidateType: local?.candidateType || null,
        remoteCandidateType: remote?.candidateType || null,
        currentRttMs: Number.isFinite(pair?.currentRoundTripTime) ? pair.currentRoundTripTime * 1000 : null } });
    } catch { /* route telemetry must never interrupt play */ }
  }

  function health() {
    const rttMs = average(rtts);
    const deviations = rtts.map((value) => Math.abs(value - rttMs));
    return {
      sent: counters.sent,
      received: counters.received,
      latePackets,
      estimatedLost: Math.max(0, expectedReceived - counters.received),
      packetLoss: expectedReceived ? Math.max(0, expectedReceived - counters.received) / expectedReceived : 0,
      rttMs,
      jitterMs: average(deviations),
      samples: rtts.length,
    };
  }

  function sendEnvelope(value) {
    if (channel?.readyState !== "open") return false;
    channel.send(JSON.stringify(value));
    return true;
  }

  function onMessage(message) {
    let value;
    try { value = JSON.parse(typeof message.data === "string" ? message.data : new TextDecoder().decode(message.data)); }
    catch { return; }
    if (value.kind === "ping") {
      sendEnvelope({ kind: "pong", pingId: value.pingId, sentAt: value.sentAt });
      return;
    }
    if (value.kind === "pong") {
      const started = pendingPings.get(value.pingId);
      if (started !== undefined) {
        rolling(rtts, Math.max(0, now() - started));
        pendingPings.delete(value.pingId);
        event({ id, event: "health", data: health() });
      }
      return;
    }
    if (value.kind !== "input" || !value.packet) return;
    const seq = Number(value.sequence);
    if (Number.isInteger(seq)) {
      if (recentSequences.has(seq)) return;
      recentSequences.add(seq);
      sequenceOrder.push(seq);
      if (sequenceOrder.length > 512) recentSequences.delete(sequenceOrder.shift());
      if (seq < highestReceived) latePackets++;
      highestReceived = Math.max(highestReceived, seq);
      expectedReceived = Math.max(expectedReceived, highestReceived + 1);
    }
    counters.received++;
    rolling(arrivals, now());
    packet(value.packet);
    event({ id, event: "health", data: health() });
  }

  function attach(next) {
    channel = next;
    channel.binaryType = "arraybuffer";
    channel.onopen = () => {
      emitState("open");
      refreshRoute();
      if (setIntervalImpl) timer = setIntervalImpl(() => {
        const pingId = `${sequence}:${Math.floor(now())}`;
        pendingPings.set(pingId, now());
        sendEnvelope({ kind: "ping", pingId, sentAt: now() });
        refreshRoute();
      }, 1000);
    };
    channel.onclose = () => emitState("closed");
    channel.onerror = (error) => emitState("error", error?.message || "data channel error");
    channel.onmessage = onMessage;
  }

  pc.onicecandidate = ({ candidate }) => {
    if (candidate) signal({ candidate: candidate.toJSON?.() || candidate });
  };
  pc.onconnectionstatechange = () => emitState(pc.connectionState || "connecting");
  pc.ondatachannel = ({ channel: incoming }) => attach(incoming);

  async function start() {
    if (!initiator) return;
    attach(pc.createDataChannel("menu-fighter-inputs", { ordered: false, maxRetransmits: 0 }));
    await pc.setLocalDescription(await pc.createOffer());
    signal({ description: pc.localDescription });
  }

  async function acceptSignal(value) {
    if (value?.description) {
      await pc.setRemoteDescription(value.description);
      while (pendingCandidates.length) await pc.addIceCandidate(pendingCandidates.shift());
      if (value.description.type === "offer") {
        await pc.setLocalDescription(await pc.createAnswer());
        signal({ description: pc.localDescription });
      }
      return;
    }
    if (value?.candidate) {
      if (pc.remoteDescription) await pc.addIceCandidate(value.candidate);
      else pendingCandidates.push(value.candidate);
    }
  }

  function sendPacket(value) {
    const ok = sendEnvelope({ kind: "input", sequence: sequence++, sentAt: now(), packet: value });
    if (ok) counters.sent++;
    return ok;
  }

  function close() {
    if (timer && clearIntervalImpl) clearIntervalImpl(timer);
    timer = null;
    try { channel?.close(); } catch {}
    try { pc.close(); } catch {}
  }

  start().catch((error) => emitState("error", error.message));
  return { id, pc, acceptSignal, sendPacket, close, health };
}
