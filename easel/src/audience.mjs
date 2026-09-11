// audience.mjs — who is watching the piece, right now.
//
// A session already knows the address it is publishing to. This is the other
// half of that fact: whether anyone is at it. The session server keeps a set of
// subscribers per code channel — every browser that opened `@handle/slug` joins
// the channel named after that route — so the count of people looking at the
// piece is a question the server can already answer, and `code-channel:info`
// is the question.
//
// Polled rather than pushed, because the server broadcasts nothing when a
// viewer arrives or leaves; the set is only ever read on demand. That shapes
// what can honestly be reported. `here` is the truth at the last poll. `peak`
// is the most ever seen at once. `arrivals` is the sum of the rises, which is a
// floor and not a visit count — someone who opens the piece and closes it
// between two polls is never seen at all, and the interface should not imply
// otherwise.
//
// `here` is null until a reply actually lands. A server that never answers is
// not a piece with no viewers, and showing a confident zero for "nobody has
// deployed the fix yet" would be the interface making something up.
import { EventEmitter } from "node:events";

// The monolith carries every code channel — `disk.mjs` asks for that service by
// name for all pieces, so there is only ever one server to ask.
export const SESSION_SERVER = "wss://session-server.aesthetic.computer";

export const POLL_MS = 4000;
const RECONNECT_MS = 4000;
const MAX_RECONNECT_MS = 60_000;

export class Audience extends EventEmitter {
  constructor({
    url = SESSION_SERVER,
    channel = "",
    poll = POLL_MS,
    // Injected so a test can drive this without a network. Anything with the
    // WebSocket shape will do.
    socket = (address) => new WebSocket(address),
    now = () => Date.now(),
  } = {}) {
    super();
    this.url = url;
    this.channel = String(channel || "");
    this.poll = poll;
    this.open = socket;
    this.now = now;
    this.ws = null;
    this.timer = null;
    this.retry = null;
    this.backoff = RECONNECT_MS;
    this.closed = false;
    // What the interface is allowed to say. `here` stays null until the server
    // has answered at least once for the current channel.
    this.here = null;
    this.peak = 0;
    this.arrivals = 0;
    // Everyone connected to Aesthetic Computer, which the server volunteers in
    // its greeting. Not this piece's audience — a different, larger fact, and
    // labelled as such wherever it is shown.
    this.online = null;
  }

  get watching() {
    return Boolean(this.channel);
  }

  start() {
    if (this.closed || this.ws) return this;
    this.#connect();
    return this;
  }

  // Point at a different channel — the handle resolved, or the piece was
  // renamed. The counts belong to the old channel, so they reset with it.
  watch(channel) {
    const next = String(channel || "");
    if (next === this.channel) return false;
    this.channel = next;
    this.here = null;
    this.peak = 0;
    this.arrivals = 0;
    this.emit("change", this.report());
    if (this.watching) this.#ask();
    return true;
  }

  report() {
    return {
      channel: this.channel,
      here: this.here,
      peak: this.peak,
      arrivals: this.arrivals,
      online: this.online,
    };
  }

  close() {
    this.closed = true;
    clearTimeout(this.timer);
    clearTimeout(this.retry);
    this.timer = null;
    this.retry = null;
    try {
      this.ws?.close();
    } catch {}
    this.ws = null;
  }

  #connect() {
    let ws;
    try {
      ws = this.open(this.url);
    } catch {
      return this.#retryLater();
    }
    this.ws = ws;
    ws.onopen = () => {
      this.backoff = RECONNECT_MS;
      this.#ask();
      // Anything else riding this socket has to re-introduce itself after a
      // reconnect — the server remembers nothing about a connection that died.
      this.emit("open");
    };
    ws.onmessage = (event) => this.#receive(event?.data);
    // A dropped connection is ordinary — a laptop sleeps, a server redeploys —
    // so it reconnects rather than reporting. What it must not do is keep
    // showing the last count as if it were current.
    ws.onclose = () => {
      if (this.ws !== ws) return;
      this.ws = null;
      this.here = null;
      this.emit("change", this.report());
      this.#retryLater();
    };
    // Swallowed on purpose: an unhandled `error` event is fatal, and every
    // failure worth acting on arrives as a close right behind it.
    ws.onerror = () => {};
  }

  #retryLater() {
    if (this.closed || this.retry) return;
    const wait = this.backoff;
    this.backoff = Math.min(MAX_RECONNECT_MS, this.backoff * 2);
    this.retry = setTimeout(() => {
      this.retry = null;
      this.#connect();
    }, wait);
    this.retry.unref?.();
  }

  // This session's one socket to the session server. The viewer count is what
  // opened it, but it is the only connection there is, so other readouts speak
  // through it rather than each paying for their own.
  send(type, content) {
    if (this.ws?.readyState !== 1) return false;
    try {
      this.ws.send(JSON.stringify({ type, content }));
      return true;
    } catch {
      return false;
    }
  }

  #ask() {
    clearTimeout(this.timer);
    this.timer = null;
    if (this.closed || !this.watching) return;
    if (this.ws?.readyState !== 1) return;
    try {
      this.ws.send(
        JSON.stringify({ type: "code-channel:info", content: this.channel }),
      );
    } catch {}
    this.timer = setTimeout(() => this.#ask(), this.poll);
    this.timer.unref?.();
  }

  #receive(data) {
    let message;
    try {
      message = JSON.parse(String(data));
    } catch {
      return;
    }
    if (message?.type === "connected") {
      // The greeting's content is itself a JSON string.
      let greeting = message.content;
      if (typeof greeting === "string") {
        try {
          greeting = JSON.parse(greeting);
        } catch {
          greeting = null;
        }
      }
      const count = Number(greeting?.playerCount);
      if (Number.isFinite(count)) {
        this.online = count;
        this.emit("change", this.report());
      }
      return;
    }
    if (message?.type !== "code-channel:info") {
      // Not ours — hand it to whoever else is listening on this socket.
      this.emit("message", message);
      return;
    }
    const content = message.content;
    // A reply for a channel we have since stopped watching is stale.
    if (content?.channel !== this.channel) return;
    const viewers = Number(content.viewers);
    if (!Number.isFinite(viewers)) return;
    const before = this.here;
    this.here = viewers;
    this.peak = Math.max(this.peak, viewers);
    if (before !== null && viewers > before) this.arrivals += viewers - before;
    if (before !== viewers) this.emit("change", this.report());
  }
}
