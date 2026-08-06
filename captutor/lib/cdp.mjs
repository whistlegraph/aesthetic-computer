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

import { basename } from "node:path";

const HOST = process.env.CDP_HOST || "127.0.0.1";
const PORT = process.env.CDP_PORT || "9222";
const COMMAND_TIMEOUT_MS = Number(process.env.CAPTUTOR_CDP_COMMAND_TIMEOUT_MS || 15_000);
const HEALTH_TIMEOUT_MS = Number(process.env.CAPTUTOR_CDP_HEALTH_TIMEOUT_MS || 4_000);
const CRASH_EVENTS = new Set(["Inspector.targetCrashed", "Target.targetCrashed"]);

export class BrowserCrashError extends Error {
  constructor(message, details = {}) {
    super(message);
    this.name = "BrowserCrashError";
    this.code = "BROWSER_RENDERER_CRASH";
    this.details = details;
  }
}

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
  const session = new Session(target.webSocketDebuggerUrl);
  try {
    await session.monitorCrashes();
    return session;
  } catch (error) {
    await session.close();
    throw error;
  }
}

// Short-lived inspection should always use this wrapper. A bare `attach()` in
// a one-off node script leaves the WebSocket holding the process open, which
// turns a two-second preflight into a tool timeout. Long-running screenplays
// still own their Session directly and close it in Captutor's render teardown.
export async function withSession(urlMatch, action) {
  const session = await attach(urlMatch);
  try { return await action(session); }
  finally { await session.close(); }
}

export class Session {
  constructor(wsUrl) {
    this.wsUrl = wsUrl;
    this.id = 0;
    this.pending = new Map();
    this.crashEvent = null;
    this.closing = false;
    const entrypoint = process.argv[1] ? basename(process.argv[1]) : "";
    // Claude frequently explores with `node -e` or a throwaway *probe*. Those
    // scripts used to print their answer and then live forever because nobody
    // closed CDP. Give only those explicitly ephemeral entrypoints a brief idle
    // reaper; production captutor.mjs sessions remain fully caller-owned.
    this.ephemeralIdleMs = (
      process.env.CAPTUTOR_CDP_EPHEMERAL === "1"
      || !entrypoint
      || /(?:^|[-_.])probe/i.test(entrypoint)
    ) ? 5_000 : 0;
    this.idleTimer = null;
    this.ready = new Promise((res, rej) => {
      // node:ws is not built in; use the global WebSocket (node >= 22).
      this.ws = new globalThis.WebSocket(wsUrl);
      this.ws.addEventListener("open", () => res());
      this.ws.addEventListener("error", (e) => rej(e));
      this.ws.addEventListener("close", () => {
        if (!this.closing) this.markCrashed("WebSocket.closed", {});
        else this.rejectPending(new Error("CDP session closed"));
      });
      this.ws.addEventListener("message", (ev) => {
        const msg = JSON.parse(ev.data);
        if (CRASH_EVENTS.has(msg.method)) {
          this.markCrashed(msg.method, msg.params || {});
          return;
        }
        const p = this.pending.get(msg.id);
        if (!p) return;
        this.pending.delete(msg.id);
        clearTimeout(p.timer);
        if (msg.error) {
          const error = new Error(JSON.stringify(msg.error));
          if (/target.*crash|renderer.*crash/i.test(msg.error.message || "")) {
            this.markCrashed("CDP.error", { method:p.method, error:msg.error });
            p.rej(this.crashError());
          } else {
            p.rej(error);
          }
        } else {
          p.res(msg.result);
        }
        this.armIdleClose();
      });
    });
  }

  rejectPending(error) {
    for (const { rej: reject, timer } of this.pending.values()) {
      clearTimeout(timer);
      reject(error);
    }
    this.pending.clear();
  }

  markCrashed(method, params) {
    if (!this.crashEvent) {
      this.crashEvent = { method, params, at:new Date().toISOString() };
    }
    this.rejectPending(this.crashError());
  }

  crashError(context = "") {
    const where = context ? ` during ${context}` : "";
    const signal = this.crashEvent?.method || "unresponsive target";
    return new BrowserCrashError(
      `browser renderer unavailable${where} (${signal})`,
      { context, signal, event:this.crashEvent },
    );
  }

  armIdleClose() {
    clearTimeout(this.idleTimer);
    if (!this.ephemeralIdleMs || this.pending.size) return;
    this.idleTimer = setTimeout(() => { void this.close(); }, this.ephemeralIdleMs);
  }

  async send(method, params = {}, { timeoutMs = COMMAND_TIMEOUT_MS } = {}) {
    await this.ready;
    if (this.crashEvent) throw this.crashError(method);
    clearTimeout(this.idleTimer);
    const id = ++this.id;
    return new Promise((res, rej) => {
      const timer = timeoutMs > 0 ? setTimeout(() => {
        this.pending.delete(id);
        rej(new Error(`CDP ${method} timed out after ${timeoutMs}ms`));
        this.armIdleClose();
      }, timeoutMs) : null;
      this.pending.set(id, { res, rej, timer, method });
      this.ws.send(JSON.stringify({ id, method, params }));
    });
  }

  // Inspector.targetCrashed is the authoritative live signal. The bounded
  // Runtime heartbeat covers two less-obvious cases: attaching after the crash
  // already happened, and Chrome keeping a dead target in /json with its old
  // title and URL (which is exactly how an "Aw, Snap!" page fooled Captutor).
  async monitorCrashes() {
    try {
      await this.send("Inspector.enable", {}, { timeoutMs:HEALTH_TIMEOUT_MS });
      await this.assertHealthy("attach");
    } catch (error) {
      if (error instanceof BrowserCrashError) throw error;
      this.markCrashed("CDP.unresponsive", {
        phase:"attach", message:error.message,
      });
      throw this.crashError("attach");
    }
  }

  async assertHealthy(context = "browser", { timeoutMs = HEALTH_TIMEOUT_MS } = {}) {
    if (this.crashEvent) throw this.crashError(context);
    try {
      const result = await this.send("Runtime.evaluate", {
        expression:"({ readyState:document.readyState, href:location.href })",
        returnByValue:true,
      }, { timeoutMs });
      if (!result?.result || result.exceptionDetails) {
        throw new Error("health expression did not return a page state");
      }
      return result.result.value;
    } catch (error) {
      if (error instanceof BrowserCrashError) throw error;
      this.markCrashed("CDP.unresponsive", { context, message:error.message });
      throw this.crashError(context);
    }
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

  /// Live bounding box for presentation-aware pointer choreography. Unlike a
  /// remembered click point, this lets the native cursor acknowledge the whole
  /// control it just chose (for example by orbiting a node-result card).
  async bounds(selector, { waitMs = 10000 } = {}) {
    const elementExpression = selector.startsWith("js=")
      ? `(${selector.slice(3)})`
      : selector.startsWith("text=")
      ? `([...document.querySelectorAll('button,[role=button],a,[role=option],[role=menuitem]')]
          .find((element) => ((element.innerText || '').trim().toLowerCase())
            .startsWith(${JSON.stringify(selector.slice(5).toLowerCase())})))`
      : `document.querySelector(${JSON.stringify(selector)})`;
    const expression = `(() => {
      const element=${elementExpression};
      if (!element) return null;
      const rect=element.getBoundingClientRect();
      return { x:rect.left, y:rect.top, width:rect.width, height:rect.height,
        cx:rect.left + rect.width / 2, cy:rect.top + rect.height / 2 };
    })()`;
    const deadline = Date.now() + waitMs;
    for (;;) {
      const rect = await this.eval(expression);
      if (rect?.width > 0 && rect?.height > 0) return rect;
      if (Date.now() > deadline) throw new Error(`no element bounds: ${selector}`);
      await new Promise((resolve) => setTimeout(resolve, 120));
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

  // A private, pixels-and-DOM "frame" of the browser target. It never raises
  // a native window, draws OCR boxes, moves the pointer, or mutates the page.
  // Pathfinding agents get the controls and React Flow topology they actually
  // need in one bounded call instead of repeatedly guessing selectors.
  async frame() {
    return this.eval(`(() => {
      const clean = (value, limit = 140) => String(value || '')
        .replace(/\\s+/g, ' ').trim().slice(0, limit);
      const visible = (element) => {
        const style = getComputedStyle(element);
        const rect = element.getBoundingClientRect();
        return style.display !== 'none' && style.visibility !== 'hidden' &&
          Number(style.opacity || 1) > 0 && rect.width > 0 && rect.height > 0 &&
          rect.bottom >= 0 && rect.right >= 0 && rect.top <= innerHeight && rect.left <= innerWidth;
      };
      const box = (element) => {
        const rect = element.getBoundingClientRect();
        return {
          x: Math.round(rect.left), y: Math.round(rect.top),
          width: Math.round(rect.width), height: Math.round(rect.height),
          cx: Math.round(rect.left + rect.width / 2),
          cy: Math.round(rect.top + rect.height / 2),
        };
      };
      const locator = (element) => {
        if (element.id) return '#' + CSS.escape(element.id);
        const testid = element.getAttribute('data-testid');
        if (testid) return '[data-testid=' + JSON.stringify(testid) + ']';
        const aria = element.getAttribute('aria-label');
        if (aria) return '[aria-label=' + JSON.stringify(aria) + ']';
        const role = element.getAttribute('role');
        const label = clean(element.innerText || element.textContent, 80);
        if (label && ['BUTTON', 'A'].includes(element.tagName)) return 'text=' + label;
        return role ? '[role=' + JSON.stringify(role) + ']' : element.tagName.toLowerCase();
      };
      const describe = (element) => ({
        tag: element.tagName.toLowerCase(),
        role: element.getAttribute('role') || '',
        text: clean(element.innerText || element.textContent),
        ariaLabel: clean(element.getAttribute('aria-label')),
        placeholder: clean(element.getAttribute('placeholder')),
        testId: element.getAttribute('data-testid') || '',
        disabled: Boolean(element.disabled || element.getAttribute('aria-disabled') === 'true'),
        locator: locator(element),
        rect: box(element),
      });
      const controls = [...document.querySelectorAll(
        'button,a,input,textarea,select,[role=button],[role=option],[role=menuitem],[role=dialog]'
      )].filter(visible).slice(0, 180).map(describe);
      const nodes = [...document.querySelectorAll('.react-flow__node')]
        .map((node) => {
          const nodeBox = box(node);
          // Fuser's editable node title exposes its real name through the
          // textbox aria-label. Prefer that semantic hook over the first leaf:
          // collaborator/avatar leaves such as "I" can otherwise masquerade
          // as the title and poison off-screen/layout telemetry.
          const accessibleTitle = [...node.querySelectorAll('[role="textbox"][aria-label]')]
            .find((element) => clean(element.getAttribute('aria-label'), 80).length > 1);
          const title = accessibleTitle || [...node.querySelectorAll('*')].find((element) =>
            element.children.length === 0 && clean(element.textContent, 80).length > 1);
          const titleBox = title ? box(title) : null;
          const titleText = accessibleTitle
            ? clean(accessibleTitle.getAttribute('aria-label'), 80)
            : (title ? clean(title.textContent, 80) : '');
          return {
          id: node.getAttribute('data-id') || node.id || '',
          type: [...node.classList].find((name) => name.startsWith('react-flow__node-'))
            ?.replace('react-flow__node-', '') || '',
          text: clean(node.innerText || node.textContent, 220),
          rect: nodeBox,
          visible: visible(node),
          title: titleText,
          titleRect: titleBox,
          titleOnscreen: Boolean(titleBox && titleBox.cx >= 0 && titleBox.cy >= 0 &&
            titleBox.cx <= innerWidth && titleBox.cy <= innerHeight),
          handles: [...node.querySelectorAll('.react-flow__handle')].filter(visible).map((handle) => ({
            id: handle.getAttribute('data-handleid') || handle.getAttribute('data-nodeid') || '',
            kind: handle.classList.contains('source') ? 'source'
              : handle.classList.contains('target') ? 'target' : '',
            position: ['left','right','top','bottom'].find((side) => handle.classList.contains(side)) || '',
            ariaLabel: clean(handle.getAttribute('aria-label')),
            rect: box(handle),
          })),
        }; });
      const focus = document.activeElement && document.activeElement !== document.body
        ? describe(document.activeElement) : null;
      return {
        schema: 'captutor-cdp-frame/v1',
        capturedAt: new Date().toISOString(),
        url: location.href,
        title: document.title,
        readyState: document.readyState,
        viewport: { width: innerWidth, height: innerHeight, dpr: devicePixelRatio },
        focus,
        controls,
        graph: {
          present: Boolean(document.querySelector('.react-flow')),
          nodeCount: nodes.length,
          edgeCount: document.querySelectorAll('.react-flow__edge').length,
          viewportTransform:getComputedStyle(
            document.querySelector('.react-flow__viewport') || document.documentElement,
          ).transform,
          zoomLabel:[...document.querySelectorAll('button')]
            .map((button) => (button.innerText || '').trim())
            .find((text) => /^\\d+\\s*%$/.test(text)) || '',
          nodes,
        },
      };
    })()`);
  }

  // DOM-backed half of Frame's wanderer/wiggler. Pixel diffs reveal that a
  // surface changed; this records why it is actionable: the hit-test stack,
  // computed cursor, role/label, and React Flow ownership at every no-click
  // probe. The caller can persist the returned atlas with a failure receipt.
  async explore(points, { settleMs = 100 } = {}) {
    const probes = [];
    for (const raw of points) {
      const x = Math.round(Number(raw.x));
      const y = Math.round(Number(raw.y));
      if (!Number.isFinite(x) || !Number.isFinite(y)) continue;
      await this.send("Input.dispatchMouseEvent", {
        type:"mouseMoved", x, y, button:"none", buttons:0,
      });
      await new Promise((resolve) => setTimeout(resolve, settleMs));
      const evidence = await this.eval(`(() => {
        const x=${x}, y=${y};
        const clean=(value)=>String(value || '').replace(/\\s+/g, ' ').trim().slice(0, 100);
        return {
          x, y,
          stack:document.elementsFromPoint(x, y).slice(0, 8).map((element) => {
            const style=getComputedStyle(element);
            const node=element.closest('.react-flow__node');
            return {
              tag:element.tagName.toLowerCase(),
              role:element.getAttribute('role') || '',
              text:clean(element.innerText || element.textContent),
              ariaLabel:clean(element.getAttribute('aria-label')),
              cursor:style.cursor,
              pointerEvents:style.pointerEvents,
              nodeId:node?.getAttribute('data-id') || '',
              nodeType:node ? [...node.classList].find((name) =>
                name.startsWith('react-flow__node-'))?.replace('react-flow__node-', '') || '' : '',
            };
          }),
        };
      })()`);
      probes.push(evidence);
    }
    return { schema:"captutor-dom-hover-atlas/v1", probes };
  }

  // Page.captureScreenshot reads the compositor invisibly. Unlike fleet Frame,
  // it cannot create an on-screen overlay that leaks into a subsequent take.
  async screenshot({ format = "png", quality } = {}) {
    await this.send("Page.enable");
    const params = { format, fromSurface: true, captureBeyondViewport: false };
    if (format === "jpeg" && quality != null) params.quality = quality;
    const { data } = await this.send("Page.captureScreenshot", params);
    return Buffer.from(data, "base64");
  }

  async close({ timeoutMs = 1000 } = {}) {
    clearTimeout(this.idleTimer);
    this.closing = true;
    try { await this.ready; } catch { return; }
    if (!this.ws || this.ws.readyState >= 2) return;
    const closed = new Promise((resolve) => this.ws.addEventListener("close", resolve, { once: true }));
    try { this.ws.close(); } catch { return; }
    await Promise.race([
      closed,
      new Promise((resolve) => setTimeout(resolve, timeoutMs)),
    ]);
  }
}
