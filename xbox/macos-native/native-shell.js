// native-shell.js — what the web shell gives the game for free and a bare
// JavaScriptCore context does not: timers, a WebSocket and fetch. The Mac app
// evaluates this before round-room.mjs and the game, and pumps
// __oskiewarRunTimers and the socket/fetch events once a frame, on the same
// thread as every other JS call. With these the app can take the second
// chair of a room the way a browser tab does: RoundRoom, unchanged, over a
// socket the app holds. 26.10.14
(function () {
  const g = globalThis;

  // ── timers ───────────────────────────────────────────────────────────────
  // Run from the frame loop, so a timer fires at the start of the first frame
  // past its time — never sooner than asked, never between two sim steps.
  const timers = new Map();
  let nextTimer = 1;
  g.setTimeout = function (fn, ms = 0, ...args) {
    const id = nextTimer++;
    timers.set(id, { at: Date.now() + Math.max(0, Number(ms) || 0), fn, args, every: 0 });
    return id;
  };
  g.setInterval = function (fn, ms = 0, ...args) {
    const id = nextTimer++;
    const every = Math.max(1, Number(ms) || 0);
    timers.set(id, { at: Date.now() + every, fn, args, every });
    return id;
  };
  g.clearTimeout = g.clearInterval = function (id) { timers.delete(id); };
  g.__oskiewarTimerError = "";
  g.__oskiewarRunTimers = function () {
    const now = Date.now();
    for (const [id, timer] of timers) {
      if (timer.at > now) continue;
      if (timer.every) timer.at = now + timer.every; else timers.delete(id);
      // A throw here would reach the host's exception handler and stop the
      // game for a bridge's retry; keep it, and let the net log read it.
      try { timer.fn(...timer.args); }
      catch (error) { g.__oskiewarTimerError = String(error && error.stack || error); }
    }
  };

  // ── WebSocket ────────────────────────────────────────────────────────────
  // The app opens, sends and closes; it reports back through
  // __oskiewarSocketEvent(id, "open" | "message" | "close" | "error", text).
  const sockets = new Map();
  class NativeWebSocket {
    constructor(url) {
      this.url = String(url);
      this.readyState = 0;
      this.listeners = {};
      this.id = oskiewarSocketOpen(this.url);
      sockets.set(this.id, this);
    }
    addEventListener(type, fn) { (this.listeners[type] ||= []).push(fn); }
    removeEventListener(type, fn) {
      const list = this.listeners[type];
      if (list) this.listeners[type] = list.filter((entry) => entry !== fn);
    }
    dispatch(type, event) { for (const fn of this.listeners[type] || []) fn(event); }
    send(text) {
      if (this.readyState !== 1) throw new Error("socket is not open");
      oskiewarSocketSend(this.id, String(text));
    }
    close(code = 1000, reason = "") {
      if (this.readyState >= 2) return;
      this.readyState = 2;
      oskiewarSocketClose(this.id, Number(code) || 1000, String(reason));
    }
  }
  NativeWebSocket.CONNECTING = 0; NativeWebSocket.OPEN = 1;
  NativeWebSocket.CLOSING = 2; NativeWebSocket.CLOSED = 3;
  g.NativeWebSocket = NativeWebSocket;
  // The datagram lane's UDP sockets (round-room.mjs nativeLaneOpen).
  g.__oskiewarDatagramEvent = function (id, kind, text) {
    const listener = g.__oskiewarDatagramListeners && g.__oskiewarDatagramListeners[id];
    if (kind === "datagram" && listener) {
      try { listener(text); } catch (error) { g.__oskiewarTimerError = String(error); }
    }
  };
  g.__oskiewarSocketEvent = function (id, kind, text) {
    const socket = sockets.get(id);
    if (!socket) return;
    if (kind === "open") {
      if (socket.readyState === 0) { socket.readyState = 1; socket.dispatch("open", {}); }
    } else if (kind === "message") {
      socket.dispatch("message", { data: text });
    } else if (kind === "close") {
      sockets.delete(id);
      if (socket.readyState !== 3) { socket.readyState = 3; socket.dispatch("close", { code: 1006, reason: text || "" }); }
    } else if (kind === "error") {
      socket.dispatch("error", {});
    }
  };

  // ── fetch ────────────────────────────────────────────────────────────────
  // GET only, which is all the room bridge asks for (a stored replay). The
  // app answers through __oskiewarFetchEvent(id, status, body); a negative
  // status is a transport failure with the reason in `body`.
  const fetches = new Map();
  let nextFetch = 1;
  g.fetch = function (url) {
    return new Promise((resolve, reject) => {
      const id = nextFetch++;
      fetches.set(id, { resolve, reject });
      oskiewarFetch(id, String(url));
    });
  };
  g.__oskiewarFetchEvent = function (id, status, body) {
    const pending = fetches.get(id);
    if (!pending) return;
    fetches.delete(id);
    if (status < 0) { pending.reject(new Error(body || "fetch failed")); return; }
    pending.resolve({
      ok: status >= 200 && status < 300, status,
      json: async () => JSON.parse(body), text: async () => body,
    });
  };
})();
