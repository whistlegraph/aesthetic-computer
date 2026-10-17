import test from "node:test";
import assert from "node:assert/strict";

test("OS appearance, page scrolling, and failed downloads remain usable", async () => {
  const previous = { fetch: globalThis.fetch, WebSocket: globalThis.WebSocket };
  const buttons = [];
  const builds = Array.from({ length: 8 }, (_, i) => ({ name: `build-${i}`, git_hash: "abcdef0",
    commit_msg: "A readable release message", build_ts: "2026-10-15", size: 1048576 }));
  globalThis.fetch = async (url, options) => {
    if (String(url).endsWith("/os-releases")) return { json: async () => ({ releases: builds }) };
    if (String(url).endsWith("/native-build")) return { json: async () => ({}) };
    if (options?.method === "HEAD") return { ok: false };
    throw new Error("Download unavailable");
  };
  globalThis.WebSocket = class { close() {} };
  class TextButton {
    constructor(txt, pos) {
      this.txt = txt; this.pos = pos; this.height = 20; buttons.push(this);
      this.btn = { act: (event, actions) => { if (event.target === this.txt) actions.push?.(); } };
    }
    get width() { return this.txt.length * 6 + 8; }
    replaceLabel(txt) { this.txt = txt; }
    reposition(pos, txt) { if (txt) this.txt = txt; this.pos = pos; }
    paint() {}
  }
  const piece = await import(`../system/public/aesthetic.computer/disks/os.mjs?test=${Date.now()}`);
  const texts = [], backgrounds = [];
  const api = { dark: true, screen: { width: 195, height: 300 }, needsPaint() {},
    store: { retrieve: async () => undefined, persist() {} },
    ui: { TextButton }, wipe: (...color) => backgrounds.push(color),
    ink() { return api; }, box() { return this; }, line() {},
    write(text, pos) { texts.push({ text, pos }); return this; },
    darkMode(dark) { api.dark = dark; },
  };
  const paint = () => { texts.length = 0; piece.paint(api); };
  const event = (type, extra = {}) => piece.act({ ...api, event: { is: name => name === type, ...extra } });
  const flush = () => new Promise(resolve => setImmediate(resolve));
  try {
    await piece.boot(api);
    await flush();
    paint();
    assert.deepEqual(backgrounds.at(-1), [10, 12, 18]);
    event("lift", { target: "light" });
    assert.equal(api.dark, false, "theme control invokes the shared appearance API");
    assert.equal(api.store["os-appearance"], "light");
    event("light-mode");
    paint();
    assert.deepEqual(backgrounds.at(-1), [248, 248, 244]);
    api.dark = true; // Startup media-query detection must not replace the explicit selection.
    paint();
    assert.deepEqual(backgrounds.at(-1), [248, 248, 244]);
    const headerY = () => texts.find(t => t.text === "build-0").pos.y;
    const before = headerY();
    event("scroll", { y: 120 });
    paint();
    assert.ok(headerY() < before, "collapsed page scrolls on a small screen");
    event("scroll", { y: -10000 });
    paint();
    assert.equal(headerY(), before, "scroll clamps at top");
    event("touch");
    event("lift", { target: "download .img" });
    await flush();
    paint();
    assert.ok(texts.some(t => t.text.includes("Download unavailable")), "failure stays visible after downloading stops");
    assert.ok(buttons.some(b => b.txt === "download .img"));
  } finally {
    piece.leave();
    Object.assign(globalThis, previous);
  }
});
