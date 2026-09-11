// diagnostics.mjs — relays a piece's own health back to whoever is authoring it.
//
// A browser running a piece reports what it sees (see
// `system/public/aesthetic.computer/lib/diagnostics.mjs`). This decides who is
// allowed to hear it, which is the whole substance of the module: a report is
// the inside of somebody's browser, and it must reach the author of the piece
// and nobody else — not other viewers of the same piece, and not whoever
// happens to know the channel's name.
//
// Two kinds of channel, two rules, matching the ownership rule `/run` already
// enforces for pushing source:
//
//   `handle/slug` is public — it is in the URL — so knowing it proves nothing.
//   A listener must present an access token that resolves to that handle.
//
//   An opaque channel has no handle in it and is unguessable by construction,
//   so possession of the name is the capability, exactly as it is for pushing.
//
// Reports are never echoed to reporters. A viewer cannot learn anything about
// another viewer through this.

const USERINFO = "https://aesthetic.us.auth0.com/userinfo";

// A listener that never hears anything still costs a socket; a report with
// nobody listening costs nothing and is dropped at the door.
export class DiagnosticsRelay {
  constructor({
    fetch: fetcher = globalThis.fetch,
    userinfo = USERINFO,
    // Where to resolve a token's `sub` into a handle. The session server talks
    // to the site for this the same way the chat manager does.
    site = "https://aesthetic.computer",
    log = () => {},
  } = {}) {
    this.fetch = fetcher;
    this.userinfo = userinfo;
    this.site = site;
    this.log = log;
    // channel -> Set of listening sockets.
    this.rooms = new Map();
    // socket -> Set of channels, so a disconnect cleans up without a sweep.
    this.listeners = new Map();
    // Resolved handles, since a session asks repeatedly with the same token.
    this.handles = new Map();
  }

  // Does this channel name carry a handle? `jeffrey/balozo` does; an opaque
  // session channel does not.
  static owner(channel) {
    const name = String(channel || "");
    const cut = name.indexOf("/");
    if (cut <= 0) return "";
    return name.slice(0, cut);
  }

  async #handleFor(token) {
    if (!token) return "";
    if (this.handles.has(token)) return this.handles.get(token);
    let handle = "";
    try {
      const who = await this.fetch(this.userinfo, {
        headers: { Authorization: `Bearer ${token}` },
      });
      if (who.status === 200) {
        const { sub } = await who.json();
        if (sub) {
          const found = await this.fetch(
            `${this.site}/handle?for=${encodeURIComponent(sub)}`,
          );
          if (found.status === 200) handle = (await found.json())?.handle || "";
        }
      }
    } catch (error) {
      this.log("🩺 Diagnostics authorization failed:", error?.message || error);
    }
    this.handles.set(token, handle);
    return handle;
  }

  // May this socket listen to this channel? Resolves to a reason string when
  // not, so the caller can say so rather than failing silently.
  async permitted(channel, token) {
    const name = String(channel || "");
    if (!name) return "no channel named";
    const owner = DiagnosticsRelay.owner(name);
    if (!owner) return ""; // Opaque: the name is the capability.
    const handle = await this.#handleFor(token);
    if (!handle) return "a handled channel needs a signed-in listener";
    if (handle !== owner) return `@${handle} does not own ${name}`;
    return "";
  }

  async listen(ws, channel, token) {
    const refusal = await this.permitted(channel, token);
    if (refusal) return refusal;
    const name = String(channel);
    if (!this.rooms.has(name)) this.rooms.set(name, new Set());
    this.rooms.get(name).add(ws);
    if (!this.listeners.has(ws)) this.listeners.set(ws, new Set());
    this.listeners.get(ws).add(name);
    this.log(`🩺 Listening to ${name} (${this.rooms.get(name).size})`);
    return "";
  }

  // Hand one report to the channel's listeners. The reporter is excluded even
  // if it happens to be listening, so a report can never loop.
  report(from, channel, body, pack) {
    const room = this.rooms.get(String(channel || ""));
    if (!room || room.size === 0) return 0;
    let sent = 0;
    const message = pack("diagnostics:report", body, "diagnostics");
    for (const ws of room) {
      if (ws === from) continue;
      if (ws.readyState !== 1) continue;
      try {
        ws.send(message);
        sent += 1;
      } catch {}
    }
    return sent;
  }

  forget(ws) {
    const rooms = this.listeners.get(ws);
    if (!rooms) return;
    for (const name of rooms) {
      const room = this.rooms.get(name);
      room?.delete(ws);
      if (room && room.size === 0) this.rooms.delete(name);
    }
    this.listeners.delete(ws);
  }

  get watching() {
    return this.rooms.size;
  }
}
