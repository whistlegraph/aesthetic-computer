// diagnostics.mjs — what the piece looks like on the far side of the QR code.
//
// The session already knows who is watching (`audience.mjs`). This is the other
// thing it could never see: what those people's browsers are actually showing.
// A piece runs in a worker, so its errors reach no `window`, no `pageerror`,
// and no terminal — an author watching from here sees a black rectangle on a
// phone and has to guess. This turns the guess into two lines of text.
//
// It rides the same socket as the audience count and speaks to the relay in
// `session-server/diagnostics.mjs`, which decides whether this session is
// allowed to hear a given channel. A handled channel (`@handle/slug`) needs the
// session's own access token; an opaque one is authorized by knowing its name.
//
// What comes back is somebody else's browser talking, so it is treated as text
// and nothing else: it is clipped, counted, and never executed or trusted.
import { EventEmitter } from "node:events";

// How many recent lines to hold. The transcript is the place for a long story;
// this is a readout, and an author wants the last thing that went wrong.
export const KEPT = 50;
const MAX_TEXT = 400;

export class Diagnostics extends EventEmitter {
  constructor({ channel = "", token = null, kept = KEPT } = {}) {
    super();
    this.channel = String(channel || "");
    this.token = token;
    this.kept = kept;
    // The last frame report, and why it matters: `blank` is the piece painting
    // one flat colour, which is what a dropped-write bug looks like from here.
    this.frame = null;
    this.logs = [];
    this.listening = false;
    this.refusal = "";
    this.send = null;
  }

  // Given a way to talk to the session server, ask to hear this channel.
  // Called again whenever the socket reconnects or the piece moves.
  async attach(send) {
    this.send = send;
    await this.#listen();
  }

  async watch(channel) {
    const next = String(channel || "");
    if (next === this.channel) return false;
    this.channel = next;
    // The readouts described the piece we were watching, not this one.
    this.frame = null;
    this.logs = [];
    this.listening = false;
    this.refusal = "";
    this.emit("change", this.report());
    await this.#listen();
    return true;
  }

  async #listen() {
    if (!this.send || !this.channel) return;
    let token = null;
    try {
      token = await this.token?.();
    } catch {
      // A session that cannot produce a token simply asks without one, and the
      // relay refuses it in the ordinary way with a reason worth printing.
    }
    this.send("diagnostics:listen", { channel: this.channel, token });
  }

  // One message off the socket. Returns true when it was ours.
  receive(message) {
    if (message?.type === "diagnostics:listening") {
      if (message.content?.channel !== this.channel) return true;
      this.listening = Boolean(message.content?.ok);
      this.refusal = this.listening ? "" : String(message.content?.reason || "refused");
      this.emit("change", this.report());
      return true;
    }
    if (message?.type !== "diagnostics:report") return false;
    const body = message.content;
    if (!body || body.channel !== this.channel) return true;

    if (body.kind === "frame") {
      const before = this.frame;
      this.frame = {
        colors: Number(body.colors) || 0,
        blank: Boolean(body.blank),
        color: Array.isArray(body.color) ? body.color.slice(0, 3) : null,
        width: Number(body.width) || 0,
        height: Number(body.height) || 0,
      };
      if (!before || before.blank !== this.frame.blank || before.colors !== this.frame.colors)
        this.emit("change", this.report());
      return true;
    }

    if (body.kind === "dropped") {
      this.#keep({ level: "notice", text: `…${Number(body.count) || 0} more lines dropped` });
      return true;
    }

    if (body.kind === "log") {
      this.#keep({
        level: String(body.level || "log"),
        text: String(body.text || "").slice(0, MAX_TEXT),
      });
      return true;
    }
    return true;
  }

  #keep(line) {
    if (!line.text) return;
    this.logs.push(line);
    if (this.logs.length > this.kept) this.logs.shift();
    this.emit("log", line);
  }

  // The errors are the part an author wants surfaced without asking.
  get errors() {
    return this.logs.filter((line) => line.level === "error");
  }

  report() {
    return {
      channel: this.channel,
      listening: this.listening,
      refusal: this.refusal,
      frame: this.frame,
      errors: this.errors.length,
    };
  }
}
