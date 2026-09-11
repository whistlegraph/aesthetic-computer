// autopublish.mjs — publish every save under the user's @handle, coalesced.
//
// The live channel and publishing answer two different questions. A push to
// /run reaches whoever scanned the code, right now, and vanishes with the
// session; publishing writes the source into the user's bucket, where the piece
// keeps answering at aesthetic.computer/@handle/slug after the terminal closes.
// Auto-publish makes the second one follow the first, so a piece someone is
// already watching on a phone is also a URL they can send to somebody.
//
// It is off unless asked for. Publishing is outward-facing — the bytes land on
// a public route under the user's own name — so it stays an opt-in for the
// session (`/autopublish on`, `--autopublish`, EASEL_AUTOPUBLISH=1)
// rather than something the interface starts doing on its own.
//
// Saves arrive far faster than a publish should: an agent turn can write a file
// five times in ten seconds, and each publish is three network round trips onto
// a CDN-fronted key that serves the old bytes for a while after an overwrite.
// So this coalesces. A save marks the piece dirty, a publish runs once the
// saves stop (`settle`) and no sooner than `minGap` after the last one, and a
// save arriving mid-publish re-arms rather than queueing a second. What is
// guaranteed is that the last save wins — `flush()` on the way out is what
// makes that true even when the session ends a second after an edit.
import { EventEmitter } from "node:events";

export const SETTLE = 2500;
export const MIN_GAP = 12_000;

export class AutoPublisher extends EventEmitter {
  constructor({
    publish,
    enabled = false,
    settle = SETTLE,
    minGap = MIN_GAP,
    now = () => Date.now(),
  } = {}) {
    super();
    this.publish = publish;
    this.enabled = Boolean(enabled);
    this.settle = settle;
    this.minGap = minGap;
    this.now = now;
    this.timer = null;
    this.current = null;
    // The source waiting to go out, and the source that last went out. Equal
    // means there is nothing to publish: a save that only rewrote the file with
    // the same bytes (or our own read-back) is not a new version.
    this.queued = null;
    this.published = null;
    this.publishedAt = 0;
  }

  get pending() {
    return this.queued !== null;
  }

  get running() {
    return this.current !== null;
  }

  // Turn it on or off mid-session. Turning it off cancels what was armed but
  // leaves a publish already in flight alone — it is already someone else's
  // bytes on the wire.
  set(enabled) {
    this.enabled = Boolean(enabled);
    if (!this.enabled) {
      this.#disarm();
      this.queued = null;
    }
    return this.enabled;
  }

  // A save happened. Returns true if this changed anything.
  note(source) {
    if (!this.enabled) return false;
    const text = String(source ?? "");
    if (!text.trim() || text === this.published) return false;
    this.queued = text;
    this.#arm();
    return true;
  }

  // Publish the queued save now, ignoring both timers, and resolve with the
  // result. Used on the way out, so quitting right after an edit still lands
  // that edit. Resolves null when there was nothing to do.
  async flush() {
    this.#disarm();
    if (this.current) await this.current.catch(() => {});
    if (!this.pending) return null;
    try {
      return await this.#run();
    } catch {
      return null;
    }
  }

  cancel() {
    this.#disarm();
    this.queued = null;
  }

  #disarm() {
    clearTimeout(this.timer);
    this.timer = null;
  }

  #arm() {
    if (this.timer || this.current || !this.pending) return;
    const since = this.publishedAt > 0 ? this.now() - this.publishedAt : Infinity;
    const delay = Math.max(this.settle, this.minGap - since);
    this.timer = setTimeout(() => {
      this.timer = null;
      this.#run().catch(() => {});
    }, delay);
    this.timer.unref?.();
    this.emit("armed", delay);
  }

  async #run() {
    const source = this.queued;
    if (source === null || this.current) return null;
    this.queued = null;
    this.emit("start");
    this.current = (async () => {
      try {
        const result = await this.publish();
        // Record the bytes that were sent, not what the file says afterwards:
        // a save landing during the upload must still count as unpublished.
        this.published = source;
        this.publishedAt = this.now();
        this.emit("published", result);
        return result;
      } catch (error) {
        // A failure leaves `published` alone, so the next save retries. It does
        // not retry on its own — a broken token or a rejected slug would spin
        // forever, and the transcript already said so once.
        this.publishedAt = this.now();
        this.emit("failed", error);
        throw error;
      } finally {
        this.current = null;
      }
    })();
    const settled = this.current;
    try {
      return await settled;
    } finally {
      this.#arm(); // A save that arrived mid-publish goes out next.
    }
  }
}
