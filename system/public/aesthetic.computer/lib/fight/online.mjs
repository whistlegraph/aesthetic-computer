// Binds coordinator state, the worker-safe BIOS RTC bridge, and rollback.

import { createSession } from "./session.mjs";
import { createRtcBridge } from "./rtc-bridge.mjs";

export function createOnlineFight(game, { lobby, server, send, onSfx, onState = () => {} } = {}) {
  let bridge = null;
  let session = null;
  let started = false;
  let rtcOpen = false;
  let lastRoute = { type: "unknown" };
  let health = null;

  function maybePlaying() {
    if (started && rtcOpen && session) {
      lobby.state.status = "playing";
      onState(snapshot());
    }
  }

  function proposal(data) {
    bridge?.close();
    started = false;
    rtcOpen = false;
    health = null;
    lastRoute = { type: "probing" };
    session = createSession(game, {
      player: data.seat,
      seed: data.seed,
      send: (packet) => bridge?.sendPacket(packet),
      onSfx,
    });
    bridge = createRtcBridge({
      id: data.matchId,
      initiator: data.initiator,
      send,
      // Production TURN credentials are intentionally supplied by the server
      // later. STUN-only keeps this slice direct/LAN-capable without secrets.
      iceServers: [{ urls: "stun:stun.l.google.com:19302" }],
      onSignal: (signal) => lobby.signal(server, signal),
      onPacket: (packet) => session?.receive(packet),
      onState: ({ state, detail }) => {
        rtcOpen = state === "open" ? true : state === "closed" || state === "error" ? false : rtcOpen;
        if (state === "error") { lobby.state.status = "error"; lobby.state.error = detail || "peer connection failed"; }
        maybePlaying();
      },
      onHealth: (value) => {
        health = value;
        lobby.state.health = snapshot().health;
        if (value.samples >= 3) lobby.routeReport(server, {
          direct: {
            rttMs: value.rttMs,
            jitterMs: value.jitterMs,
            loss: value.packetLoss,
            samples: value.samples,
            nonce: data.probeNonce,
          },
          relays: {},
        });
        onState(snapshot());
      },
      onRoute: (route) => { lastRoute = route; onState(snapshot()); },
    });
    lobby.acceptMatch(server);
    onState(snapshot());
  }

  function start() { started = true; maybePlaying(); }
  function signal(value) { bridge?.signal(value); }
  function rtcEvent(value) { return bridge?.accept(value) || false; }

  function advance(input) {
    if (lobby.state.status !== "playing" || !session) return "offline";
    return session.advance(input);
  }

  function snapshot() {
    const rollback = session?.stats || { rollbacks: 0, resimFrames: 0, stalls: 0, predicted: 0 };
    return {
      status: lobby.state.status,
      state: session?.state || null,
      route: lobby.state.route || lastRoute,
      health: {
        ...(health || {}),
        rollbacks: rollback.rollbacks,
        rollbackDepth: rollback.rollbacks ? rollback.resimFrames / rollback.rollbacks : 0,
        stalls: rollback.stalls,
        predicted: rollback.predicted,
      },
    };
  }

  function close() {
    bridge?.close();
    bridge = null;
    session = null;
    started = false;
    rtcOpen = false;
    health = null;
    lastRoute = { type: "unknown" };
  }
  return { proposal, start, signal, rtcEvent, advance, snapshot, close, get session() { return session; } };
}
