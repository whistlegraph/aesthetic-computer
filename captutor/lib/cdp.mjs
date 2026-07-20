// cdp — the smallest Chrome DevTools Protocol client that can act like a person.
//
// Why not Playwright: we are filming the REAL browser window with `reel`
// (SCStream → hardware h264), not Playwright's internal recorder. We only need
// CDP to move and click, and a direct socket keeps the browser a normal,
// user-profile Chrome — signed in, with the session cookies the fuser app needs.
// See vault/fuser/skills/drive-ui.md for launching that profiled CDP Chrome.
//
// Every click is a TRUSTED event (Input.dispatchMouseEvent), because React and
// the fuser canvas both ignore synthetic ones. The visible pointer, though, is
// ours (see cursor.mjs) — CDP moves no real cursor, and a tutorial where things
// click themselves with no pointer in sight reads as a bug.

// (WebSocket is a Node >= 22 global — no import, no `ws` dependency.)

const HOST = process.env.CDP_HOST || "127.0.0.1";
const PORT = process.env.CDP_PORT || "9222";

export async function attach(urlMatch) {
  const list = await (await fetch(`http://${HOST}:${PORT}/json`)).json();
  const pages = list.filter((t) => t.type === "page");
  const target = urlMatch
    ? pages.find((t) => (t.url || "").includes(urlMatch))
    : pages[0];
  if (!target) {
    throw new Error(
      `no CDP page${urlMatch ? ` matching "${urlMatch}"` : ""}. open pages:\n` +
      pages.map((p) => `  ${p.url}`).join("\n"));
  }
  return new Session(target.webSocketDebuggerUrl);
}

class Session {
  constructor(wsUrl) {
    this.wsUrl = wsUrl;
    this.id = 0;
    this.pending = new Map();
    this.ready = new Promise((res, rej) => {
      // node:ws is not built in; use the global WebSocket (node >= 22).
      this.ws = new globalThis.WebSocket(wsUrl);
      this.ws.addEventListener("open", () => res());
      this.ws.addEventListener("error", (e) => rej(e));
      this.ws.addEventListener("message", (ev) => {
        const msg = JSON.parse(ev.data);
        const p = this.pending.get(msg.id);
        if (!p) return;
        this.pending.delete(msg.id);
        msg.error ? p.rej(new Error(JSON.stringify(msg.error))) : p.res(msg.result);
      });
    });
  }

  async send(method, params = {}) {
    await this.ready;
    const id = ++this.id;
    return new Promise((res, rej) => {
      this.pending.set(id, { res, rej });
      this.ws.send(JSON.stringify({ id, method, params }));
    });
  }

  /// Evaluate ONE expression. Wrap statements in an IIFE — top-level `const`
  /// returns undefined and silently swallows what you meant to return.
  async eval(expression) {
    const r = await this.send("Runtime.evaluate", {
      expression,
      awaitPromise: true,
      returnByValue: true,
    });
    if (r.exceptionDetails) {
      throw new Error(`eval threw: ${r.exceptionDetails.exception?.description || ""}`);
    }
    return r.result?.value;
  }

  async nav(url) {
    await this.send("Page.enable");
    await this.send("Page.navigate", { url });
    await this.waitFor("document.readyState === 'complete'");
  }

  /// Poll an expression until truthy. Every wait in a screenplay should be a
  /// real condition, never a sleep — a sleep that is too short produces a
  /// tutorial that films a spinner, and one that is too long films dead air.
  async waitFor(expression, { timeoutMs = 20000, everyMs = 100 } = {}) {
    const deadline = Date.now() + timeoutMs;
    while (Date.now() < deadline) {
      try {
        if (await this.eval(`!!(${expression})`)) return true;
      } catch {}
      await new Promise((r) => setTimeout(r, everyMs));
    }
    throw new Error(`waitFor timed out: ${expression}`);
  }

  /// Centre of an element, in viewport CSS pixels — what both our drawn cursor
  /// and the trusted click need.
  ///
  /// Accepts a CSS selector, or `text=Add a Node` to find a control by its
  /// visible label. Real UIs label things for people, not for us: fuser's node
  /// picker is just a <button> reading "Add a Node ⇧A", with no test id and no
  /// stable class. Matching the words on screen is both the only handle we have
  /// and the one that reads correctly in a screenplay.
  async center(selector, { waitMs = 10000 } = {}) {
    // `js=<expr>` — the escape hatch for controls a real UI gives you no handle
    // on. fuser's focus composer submits with an icon button that has no
    // aria-label, no test id, and no stable class; the only honest way to name
    // it is "the last button in the composer". Prefer text= or CSS when you can;
    // reach for this when the DOM leaves you nothing else.
    const expr = selector.startsWith("js=")
      ? `(() => {
          const el = (${selector.slice(3)});
          if (!el) return null;
          el.scrollIntoView({ block: "center", inline: "center", behavior: "instant" });
          const r = el.getBoundingClientRect();
          return { x: r.left + r.width / 2, y: r.top + r.height / 2 };
        })()`
      : selector.startsWith("text=")
      ? `(() => {
          const want = ${JSON.stringify(selector.slice(5))}.toLowerCase();
          const el = [...document.querySelectorAll('button,[role=button],a,[role=option],[role=menuitem]')]
            .find((b) => (b.innerText || '').trim().toLowerCase().startsWith(want));
          if (!el) return null;
          el.scrollIntoView({ block: "center", inline: "center", behavior: "instant" });
          const r = el.getBoundingClientRect();
          return { x: r.left + r.width / 2, y: r.top + r.height / 2 };
        })()`
      : `(() => {
          const el = document.querySelector(${JSON.stringify(selector)});
          if (!el) return null;
          el.scrollIntoView({ block: "center", inline: "center", behavior: "instant" });
          const r = el.getBoundingClientRect();
          return { x: r.left + r.width / 2, y: r.top + r.height / 2 };
        })()`;
    // Wait for it. `click('Add a Node')` should mean "when it's there, click it"
    // — a UI that is still mounting is the normal case right after a navigation,
    // not an error. Without this, a screenplay is a race it sometimes loses.
    const deadline = Date.now() + waitMs;
    for (;;) {
      const box = await this.eval(expr);
      if (box) return box;
      if (Date.now() > deadline) throw new Error(`no element: ${selector}`);
      await new Promise((r) => setTimeout(r, 120));
    }
  }

  async mouse(type, x, y, button = "left") {
    await this.send("Input.dispatchMouseEvent", {
      type, x, y, button,
      buttons: type === "mousePressed" ? 1 : 0,
      clickCount: type === "mousePressed" || type === "mouseReleased" ? 1 : 0,
    });
  }

  async type(text) {
    for (const ch of text) {
      await this.send("Input.dispatchKeyEvent", { type: "char", text: ch });
      await new Promise((r) => setTimeout(r, 28));  // human-ish cadence, on camera
    }
  }

  /// modifiers is CDP's bitmask: Alt 1, Ctrl 2, Meta/⌘ 4, Shift 8.
  async key(key, code, keyCode, modifiers = 0) {
    const base = { key, code, windowsVirtualKeyCode: keyCode, modifiers };
    await this.send("Input.dispatchKeyEvent", { type: "keyDown", ...base });
    await this.send("Input.dispatchKeyEvent", { type: "keyUp", ...base });
  }

  close() { try { this.ws.close(); } catch {} }
}
