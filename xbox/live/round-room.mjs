// OSKIEWAR round room transport, 26.08.05
// Keeps live and stored-demo delivery outside the shared game/render engine.

const WORD = "(?:[bdfgklmnprstvz][aeiou]){3}";
const ROUND_NAME = new RegExp(`^(?:${WORD}-${WORD}-${WORD}|[a-z]{4,7}[0-9]{1,3})$`);

export function roundNameFromPath(pathname) {
  const name = String(pathname || "").replace(/^\/+|\/+$/g, "").toLowerCase();
  return ROUND_NAME.test(name) ? name : "";
}

export class RoundRoom {
  constructor(name, { WebSocketImpl = globalThis.WebSocket,
    fetchImpl = globalThis.fetch, historyImpl = globalThis.history,
    analytics = () => false,
    sessionOrigin = "wss://session-server.aesthetic.computer",
    replayOrigin = "" } = {}) {
    if (!ROUND_NAME.test(name || "")) throw new Error("Invalid OSKIEWAR round name");
    this.name = name;
    this.WebSocketImpl = WebSocketImpl;
    this.fetchImpl = typeof fetchImpl === "function"
      ? fetchImpl.bind(globalThis) : null;
    this.historyImpl = historyImpl;
    this.analytics = analytics;
    this.sessionOrigin = sessionOrigin;
    this.replayOrigin = replayOrigin;
    this.socket = null;
    this.timer = null;
    this.listener = null;
    this.live = false;
    this.lastState = null;
    this.generation = 0;
    this.stopped = false;
    this.deliverySeen = new Set();
  }

  start(listener) {
    this.listener = listener;
    this.stopped = false;
    this.open();
    this.loadDemo();
    return () => this.stop();
  }

  emit(type, content = {}) {
    this.listener?.({ type, content, roundName: this.name, live: this.live });
  }

  track(action, properties = {}) {
    this.analytics(action, {
      source_system: "browser",
      surface: "web",
      ...properties,
    });
  }

  open() {
    if (this.stopped || !this.WebSocketImpl) return;
    const generation = ++this.generation;
    const id = `ow-${this.name}`;
    const socket = new this.WebSocketImpl(
      `${this.sessionOrigin}/oskiewar-live?match=${encodeURIComponent(id)}&surface=web`);
    this.socket = socket;
    socket.addEventListener?.("open", () => {
      if (generation === this.generation) this.emit("status", { label: "waiting", live: false });
    });
    socket.addEventListener?.("message", (event) => {
      if (generation !== this.generation) return;
      let message;
      try { message = JSON.parse(event.data); } catch { return; }
      if (message.type === "oskiewar:status") {
        this.live = Boolean(message.content?.live);
        this.emit("status", { ...message.content,
          label: this.live ? "live" : "waiting" });
        if (!this.live) this.loadDemo();
      } else if (message.type === "oskiewar:state") {
        if (message.content?.nextRoundId) {
          this.move(message.content.nextRoundId);
          return;
        }
        if (!this.deliverySeen.has("live")) {
          this.deliverySeen.add("live");
          this.track("live_viewed");
        }
        this.lastState = message.content;
        this.emit("state", message.content);
        if (message.content?.phase === "match") this.loadDemo(true);
      } else if (message.type === "oskiewar:error") {
        this.emit("status", { label: message.content?.message || "unavailable", live: false });
      }
    });
    const closed = () => {
      if (generation !== this.generation || this.stopped) return;
      this.socket = null;
      this.live = false;
      this.loadDemo();
      this.schedule(() => this.open(), 1200);
    };
    socket.addEventListener?.("close", closed);
    socket.addEventListener?.("error", () => socket.close?.());
  }

  async loadDemo(force = false) {
    if (this.stopped || !this.fetchImpl || (this.live && !force)) return;
    const generation = this.generation;
    const id = `ow-${this.name}`;
    try {
      const response = await this.fetchImpl(
        `${this.replayOrigin}/api/oskiewar-replays?id=${encodeURIComponent(id)}`,
        { cache: "no-store" });
      if (generation !== this.generation || this.stopped) return;
      if (response.ok) {
        const replay = (await response.json()).replay;
        if (replay) {
          if (!this.deliverySeen.has("replay")) {
            this.deliverySeen.add("replay");
            this.track("replay_viewed");
          }
          this.emit("demo", replay);
        }
        return;
      }
    } catch {}
    if (!this.live || force) this.schedule(() => this.loadDemo(force), 1800);
  }

  schedule(action, delay) {
    if (this.timer || this.stopped) return;
    this.timer = setTimeout(() => {
      this.timer = null;
      action();
    }, delay);
  }

  move(value) {
    const name = String(value || "").replace(/^ow-/, "");
    if (!ROUND_NAME.test(name) || name === this.name) return;
    this.name = name;
    this.track("round_followed");
    this.live = false;
    this.lastState = null;
    this.deliverySeen = new Set();
    clearTimeout(this.timer);
    this.timer = null;
    ++this.generation;
    this.socket?.close?.(1000, "next round");
    this.socket = null;
    this.historyImpl?.replaceState?.(null, "", `/${name}`);
    this.emit("round", { id: `ow-${name}` });
    this.open();
    this.loadDemo();
  }

  stop() {
    this.stopped = true;
    ++this.generation;
    clearTimeout(this.timer);
    this.timer = null;
    this.socket?.close?.(1000, "leaving");
    this.socket = null;
    this.listener = null;
  }
}
