import { readMask } from "./images.mjs";
export class Companion {
  constructor(token, onchange) {
    if (!/^[A-Za-z0-9_-]{43}$/.test(token))
      throw Error("Invalid desktop pairing.");
    this.token = token;
    this.onchange = onchange;
    this.base = "http://127.0.0.1:8767";
    this.cache = new Map();
    this.state = null;
    this.polling = false;
    this.sending = false;
  }
  async request(path, body) {
    const response = await fetch(this.base + path, {
      method: body ? "POST" : "GET",
      headers: {
        "X-NoPaint-Pairing": this.token,
        ...(body ? { "Content-Type": "application/json" } : {}),
      },
      ...(body ? { body: JSON.stringify(body) } : {}),
      signal: AbortSignal.timeout(15000),
    });
    const value = await response.json();
    if (!response.ok) throw Error(value.error || "Desktop unavailable.");
    return value;
  }
  async image(path) {
    if (!path) return null;
    if (this.cache.has(path)) return this.cache.get(path);
    if (!/^\/image\/[a-zA-Z0-9_-]+\.png$/.test(path))
      throw Error("Invalid desktop image.");
    const response = await fetch(this.base + path, {
      headers: { "X-NoPaint-Pairing": this.token },
      signal: AbortSignal.timeout(15000),
    });
    if (!response.ok) throw Error("Desktop image unavailable.");
    const url = await new Promise((resolve) => {
      const reader = new FileReader();
      reader.onload = () => resolve(reader.result);
      response.blob().then((blob) => reader.readAsDataURL(blob));
    });
    this.cache.set(path, url);
    while (this.cache.size > 24)
      this.cache.delete(this.cache.keys().next().value);
    return url;
  }
  async receive(state) {
    if (this.state && state.revision < this.state.revision) return;
    const [accepted, before, preview] = await Promise.all([
      this.image(state.accepted),
      this.image(state.before),
      this.image(state.trace?.latest?.url),
    ]);
    this.state = state;
    Object.assign(this, {
      accepted,
      before,
      preview,
      mask: readMask(state.mask),
      error: null,
    });
    this.onchange();
  }
  async poll() {
    if (this.polling || this.sending) return;
    this.polling = true;
    try {
      await this.receive(await this.request("/api/state"));
    } catch (error) {
      this.error = error.message;
      this.onchange();
    } finally {
      this.polling = false;
    }
  }
  async act(action, extra = {}) {
    if (this.sending) return;
    this.sending = true;
    try {
      await this.receive(
        await this.request("/api/action", {
          action,
          revision: this.state.revision,
          generation: this.state.generation,
          quote: this.state.quote?.id,
          ...extra,
        }),
      );
    } finally {
      this.sending = false;
      this.onchange();
    }
  }
}
