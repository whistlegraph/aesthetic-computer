// p5-worker.mjs
// Option B from reports/p5js-integration-exploration.md.
//
// Runs real p5.js inside the AC piece worker. The sketch draws into an
// OffscreenCanvas; each AC paint() we copy its pixel buffer into
// screen.pixels so AC's compositor handles recording/preview/share like any
// other piece. Requires DOM stubs because p5 was written for a browser
// document; the stubs are minimal — just enough to load p5.min.js and run
// 2D rendering. p5's createDiv/createSlider/etc. (the p5.dom side) are
// intentionally not supported yet (will throw clear errors).

let p5LoadPromise = null;
let stubsInstalled = false;

const P5_URL = "/aesthetic.computer/dep/p5/p5.min.js";

// Wrap OffscreenCanvas with the bits HTMLCanvasElement provides that p5
// touches: style/id/className containers, getBoundingClientRect,
// addEventListener no-ops, etc.
class CanvasShim {
  constructor(w = 100, h = 100) {
    this._oc = new OffscreenCanvas(w, h);
    // Lock the 2d context to a CPU-friendly backing store so per-frame
    // getImageData() reads don't trigger expensive GPU→CPU readback. Once
    // a context exists with these attributes, p5's later getContext('2d')
    // call returns the same context (per Canvas spec).
    this._ctx2d = this._oc.getContext("2d", { willReadFrequently: true, alpha: true });
    this.style = new Proxy({}, { get: () => "", set: () => true });
    this.dataset = {};
    this.attributes = [];
    this.id = "";
    this.className = "";
    this.classList = {
      add() {}, remove() {}, toggle() {},
      contains() { return false; },
      replace() {}, item() { return null; },
      length: 0,
    };
    this.tagName = "CANVAS";
    this.nodeName = "CANVAS";
    this.nodeType = 1;
    this.children = [];
    this.childNodes = [];
    this.innerHTML = "";
    this.textContent = "";
    // parentNode lazy-stubbed by document.body if appended; default to a
    // benign placeholder so p5 cleanup paths don't deref null.
    this.parentNode = null;
    this.parentElement = null;
    this.ownerDocument = null;
  }
  get width() { return this._oc.width; }
  set width(v) { this._oc.width = v; }
  get height() { return this._oc.height; }
  set height(v) { this._oc.height = v; }
  getContext(type, opts) {
    // Return the pre-created willReadFrequently 2d context when asked.
    if (type === "2d" && this._ctx2d) return this._ctx2d;
    return this._oc.getContext(type, opts);
  }
  getBoundingClientRect() {
    return {
      x: 0, y: 0, left: 0, top: 0,
      right: this._oc.width, bottom: this._oc.height,
      width: this._oc.width, height: this._oc.height,
    };
  }
  toDataURL() { return ""; }
  async toBlob(cb) { cb && cb(await this._oc.convertToBlob()); }
  addEventListener() {}
  removeEventListener() {}
  setAttribute() {}
  getAttribute() { return null; }
  removeAttribute() {}
  appendChild(c) { return c; }
  removeChild(c) { return c; }
  insertBefore(c) { return c; }
  cloneNode() { return new CanvasShim(this._oc.width, this._oc.height); }
  contains() { return false; }
  focus() {}
  blur() {}
  click() {}
  matches() { return false; }
  closest() { return null; }
  // Convenience for AC: get the underlying OffscreenCanvas
  get offscreen() { return this._oc; }
}

function makeElementStub(tag) {
  const t = (tag || "div").toLowerCase();
  return {
    tagName: t.toUpperCase(),
    nodeName: t.toUpperCase(),
    style: new Proxy({}, { get: () => "", set: () => true }),
    children: [],
    childNodes: [],
    classList: {
      add() {}, remove() {}, toggle() {},
      contains() { return false; },
      replace() {},
    },
    dataset: {},
    attributes: [],
    parentNode: null,
    parentElement: null,
    ownerDocument: null,
    innerHTML: "",
    textContent: "",
    setAttribute() {},
    getAttribute() { return null; },
    removeAttribute() {},
    hasAttribute() { return false; },
    appendChild(c) { return c; },
    removeChild(c) { return c; },
    insertBefore(c) { return c; },
    replaceChild(c) { return c; },
    cloneNode() { return makeElementStub(t); },
    addEventListener() {},
    removeEventListener() {},
    dispatchEvent() { return true; },
    getBoundingClientRect() {
      return { x: 0, y: 0, left: 0, top: 0, right: 0, bottom: 0, width: 0, height: 0 };
    },
    contains() { return false; },
    focus() {}, blur() {}, click() {},
    matches() { return false; },
    closest() { return null; },
    querySelector() { return null; },
    querySelectorAll() { return []; },
  };
}

function installDomStubs() {
  if (stubsInstalled) return;
  stubsInstalled = true;

  // Make `window` an alias of `self` so p5's `typeof window` branches pick us up.
  if (typeof self.window === "undefined") self.window = self;

  // Viewport dimensions — will be updated per-piece via setViewport()
  self.window.innerWidth = 256;
  self.window.innerHeight = 256;
  self.window.devicePixelRatio = 1;
  self.window.scrollX = 0;
  self.window.scrollY = 0;
  self.window.pageXOffset = 0;
  self.window.pageYOffset = 0;
  if (!self.window.screen) self.window.screen = { width: 256, height: 256, availWidth: 256, availHeight: 256 };
  if (!self.window.navigator) {
    self.window.navigator = self.navigator || {
      userAgent: "aesthetic-computer",
      platform: "AC",
      language: "en-US",
      languages: ["en-US"],
      onLine: true,
    };
  }
  if (!self.window.location) {
    self.window.location = self.location || { href: "https://aesthetic.computer/", hostname: "aesthetic.computer", protocol: "https:" };
  }

  // requestAnimationFrame is on workers in modern browsers; fall back if missing
  if (typeof self.requestAnimationFrame !== "function") {
    self.requestAnimationFrame = (cb) => setTimeout(() => cb(performance.now()), 16);
    self.cancelAnimationFrame = (id) => clearTimeout(id);
  }

  // Synthetic document
  const fakeBody = makeElementStub("body");
  const fakeHead = makeElementStub("head");
  const fakeHtml = makeElementStub("html");
  // Track canvases created during a piece boot so we can locate p5's canvas
  // even if p5's internals stash it somewhere we don't expect.
  self.__acP5CreatedCanvases = [];
  self.document = {
    readyState: "complete",
    visibilityState: "visible",
    hidden: false,
    title: "aesthetic.computer",
    body: fakeBody,
    head: fakeHead,
    documentElement: fakeHtml,
    location: self.window.location,
    cookie: "",
    createElement(tag) {
      if (typeof tag === "string" && tag.toLowerCase() === "canvas") {
        const c = new CanvasShim();
        self.__acP5CreatedCanvases.push(c);
        return c;
      }
      return makeElementStub(tag);
    },
    createElementNS(_ns, tag) { return this.createElement(tag); },
    createTextNode(s) {
      return { nodeValue: String(s), textContent: String(s), nodeType: 3 };
    },
    getElementById() { return null; },
    getElementsByTagName(tag) {
      const t = String(tag || "").toLowerCase();
      if (t === "head") return [fakeHead];
      if (t === "body" || t === "html") return [fakeBody];
      // p5.createCanvas looks for <main> to host the canvas; if we return an
      // empty list it creates one then re-queries (which would also be empty
      // with a naive stub). Return a usable container for any tag so the
      // canvas just ends up "in body" semantically.
      return [fakeBody];
    },
    getElementsByClassName() { return [fakeBody]; },
    querySelector() { return fakeBody; },
    querySelectorAll() { return [fakeBody]; },
    addEventListener() {}, removeEventListener() {}, dispatchEvent() { return true; },
    hasFocus() { return true; },
    exitFullscreen() { return Promise.resolve(); },
    contains() { return false; },
  };

  // Class stubs — p5 sometimes does instanceof / typeof checks
  self.HTMLCanvasElement = CanvasShim;
  if (typeof self.HTMLElement === "undefined") self.HTMLElement = function HTMLElement() {};
  if (typeof self.Node === "undefined") self.Node = function Node() {};
  if (typeof self.Element === "undefined") self.Element = function Element() {};
  if (typeof self.Image === "undefined") {
    self.Image = class { constructor() { this.src = ""; this.crossOrigin = ""; this.complete = false; } addEventListener() {} removeEventListener() {} };
  }
  if (typeof self.Audio === "undefined") {
    self.Audio = class { constructor() {} play() { return Promise.resolve(); } pause() {} addEventListener() {} };
  }

  // p5 occasionally touches getComputedStyle
  if (typeof self.getComputedStyle === "undefined") {
    self.getComputedStyle = () => new Proxy({}, { get: () => "" });
  }
}

async function ensureP5Loaded() {
  if (p5LoadPromise) return p5LoadPromise;
  p5LoadPromise = (async () => {
    installDomStubs();
    const res = await fetch(P5_URL, { cache: "force-cache" });
    if (!res.ok) throw new Error(`failed to fetch p5: ${res.status}`);
    const code = await res.text();
    // Eval at global scope so p5 attaches to self.p5
    (0, eval)(code);
    if (typeof self.p5 !== "function") {
      throw new Error("p5 did not attach to global after eval");
    }
  })();
  return p5LoadPromise;
}

function setViewport(w, h) {
  if (typeof self.window === "undefined") return;
  self.window.innerWidth = w;
  self.window.innerHeight = h;
  if (self.window.screen) {
    self.window.screen.width = w;
    self.window.screen.height = h;
    self.window.screen.availWidth = w;
    self.window.screen.availHeight = h;
  }
}

// Clean up globals so a previously-loaded sketch can't leak into the next.
const SKETCH_GLOBALS = [
  "setup", "draw", "preload", "mousePressed", "mouseReleased", "mouseClicked",
  "mouseMoved", "mouseDragged", "mouseWheel", "doubleClicked",
  "keyPressed", "keyReleased", "keyTyped",
  "touchStarted", "touchMoved", "touchEnded",
  "windowResized", "deviceMoved", "deviceTurned", "deviceShaken",
];

function clearSketchGlobals() {
  for (const k of SKETCH_GLOBALS) {
    try { delete self[k]; } catch {}
  }
}

export async function makeP5WorkerModule({ slug, source }) {
  await ensureP5Loaded();

  let p5Instance = null;
  let canvasShim = null;
  let mouseDown = false;
  let bootError = null;
  let lastWidth = 0;
  let lastHeight = 0;

  // 🎬 rAF interception: p5 schedules its own draw loop via
  // requestAnimationFrame inside the worker. If we let it run free, p5 draws
  // on the browser's rAF cadence while AC blits on its own paint cadence —
  // two clocks → choppy / stalled frames. Capture p5's rAF callbacks and
  // drain them from AC's paint(), so p5.draw runs exactly once per AC frame.
  const originalRAF = self.requestAnimationFrame?.bind(self);
  const originalCAF = self.cancelAnimationFrame?.bind(self);
  let rafQueue = [];
  let rafIdCounter = 1;
  const queuedRAF = (cb) => {
    const id = rafIdCounter++;
    rafQueue.push({ id, cb });
    return id;
  };
  const queuedCAF = (id) => {
    rafQueue = rafQueue.filter((e) => e.id !== id);
  };
  const installRAFCapture = () => {
    self.requestAnimationFrame = queuedRAF;
    self.cancelAnimationFrame = queuedCAF;
  };
  const restoreRAF = () => {
    if (originalRAF) self.requestAnimationFrame = originalRAF;
    if (originalCAF) self.cancelAnimationFrame = originalCAF;
    rafQueue = [];
  };
  const drainRAF = () => {
    if (rafQueue.length === 0) return;
    const batch = rafQueue;
    rafQueue = [];
    const t = performance.now();
    for (const { cb } of batch) {
      try { cb(t); } catch (err) { console.warn("[p5-worker] rAF cb:", err); }
    }
  };

  // Cached context — avoids per-frame getContext() overhead.
  let cachedCtx = null;
  const blitPixelsToScreen = (screen) => {
    if (!canvasShim) return;
    const oc = canvasShim.offscreen;
    if (!cachedCtx) cachedCtx = canvasShim.getContext("2d");
    if (oc.width !== screen.width || oc.height !== screen.height) {
      // Sketch canvas size differs from AC screen — blit the overlap region.
      const w = Math.min(oc.width, screen.width);
      const h = Math.min(oc.height, screen.height);
      const img = cachedCtx.getImageData(0, 0, w, h);
      const dst = screen.pixels;
      const stride = screen.width * 4;
      const srcStride = w * 4;
      for (let row = 0; row < h; row++) {
        dst.set(img.data.subarray(row * srcStride, (row + 1) * srcStride), row * stride);
      }
    } else {
      const img = cachedCtx.getImageData(0, 0, oc.width, oc.height);
      screen.pixels.set(img.data);
    }
  };

  return {
    boot: async ({ screen }) => {
      try {
        clearSketchGlobals();
        if (self.__acP5CreatedCanvases) self.__acP5CreatedCanvases.length = 0;
        setViewport(screen.width, screen.height);
        lastWidth = screen.width;
        lastHeight = screen.height;
        // Capture p5's rAF before constructing — _start uses setTimeout but
        // its first _draw schedules via rAF.
        installRAFCapture();

        // Eval the sketch into the worker global — defines window.setup, etc.
        try {
          (0, eval)(source);
        } catch (err) {
          bootError = `sketch eval: ${err && err.message || err}`;
          console.error("[p5-worker]", bootError, err);
          return;
        }

        // Construct p5 in global mode. It will call window.setup() shortly.
        // Note: p5 schedules setup via setTimeout — we don't await here.
        try {
          p5Instance = new self.p5(undefined, undefined);
        } catch (err) {
          bootError = `new p5(): ${err && err.message || err}`;
          console.error("[p5-worker]", bootError, err);
          return;
        }

        // Find the canvas p5 created. p5 stores it on _renderer.canvas / .elt,
        // or we fall back to whatever document.createElement('canvas')
        // produced during boot.
        const tryCanvas = () => {
          const candidates = [
            p5Instance?._renderer?.canvas,
            p5Instance?.canvas,
            p5Instance?._renderer?.elt,
            ...(self.__acP5CreatedCanvases || []),
          ];
          for (const c of candidates) {
            if (c instanceof CanvasShim) { canvasShim = c; return true; }
            if (c && c.offscreen instanceof OffscreenCanvas) { canvasShim = c; return true; }
          }
          return false;
        };

        // p5 schedules setup via setTimeout(0); poll until the canvas appears.
        const start = performance.now();
        while (!tryCanvas() && performance.now() - start < 500) {
          await new Promise((r) => setTimeout(r, 10));
        }
        if (!canvasShim) {
          bootError = "could not locate sketch canvas after 500ms — did setup() call createCanvas()?";
          console.warn("[p5-worker]", bootError);
        }
      } catch (err) {
        bootError = String(err);
        console.error("[p5-worker] boot crash:", err);
      }
    },

    paint: ({ screen, ink, wipe }) => {
      if (bootError) {
        wipe(20, 0, 0);
        ink(255, 120, 120).write(`p5 boot error:`, { x: 8, y: 12 });
        ink(255, 200, 200).write(bootError.slice(0, 200), { x: 8, y: 28 });
        return false;
      }
      // Drive p5's draw loop from AC's paint: drain any rAF callbacks p5
      // queued (its _draw → user draw() → schedule next rAF), then blit.
      // Exactly one p5 frame per AC frame — no clock drift.
      drainRAF();
      blitPixelsToScreen(screen);
      return false;
    },

    sim: ({ screen }) => {
      // Reframe support: if AC screen resizes, update p5 viewport and ask the
      // sketch to resize via its windowResized() handler.
      if (screen.width !== lastWidth || screen.height !== lastHeight) {
        lastWidth = screen.width;
        lastHeight = screen.height;
        setViewport(screen.width, screen.height);
        if (typeof self.windowResized === "function") {
          try { self.windowResized(); } catch (err) { console.warn("[p5-worker] windowResized:", err); }
        }
      }
    },

    act: ({ event: e }) => {
      if (!p5Instance) return;
      const updateMouse = (x, y) => {
        self.window.pmouseX = self.window.mouseX ?? x;
        self.window.pmouseY = self.window.mouseY ?? y;
        self.window.mouseX = x;
        self.window.mouseY = y;
        if (p5Instance) {
          p5Instance.pmouseX = p5Instance.mouseX;
          p5Instance.pmouseY = p5Instance.mouseY;
          p5Instance.mouseX = x;
          p5Instance.mouseY = y;
          p5Instance.winMouseX = x;
          p5Instance.winMouseY = y;
        }
      };

      if (e.is("move") || e.is("draw")) {
        updateMouse(e.x, e.y);
        const fn = mouseDown ? self.mouseDragged : self.mouseMoved;
        if (typeof fn === "function") { try { fn(); } catch (err) { console.warn(err); } }
      } else if (e.is("touch")) {
        mouseDown = true;
        self.window.mouseIsPressed = true;
        if (p5Instance) p5Instance.mouseIsPressed = true;
        updateMouse(e.x, e.y);
        if (typeof self.mousePressed === "function") { try { self.mousePressed(); } catch (err) { console.warn(err); } }
      } else if (e.is("lift")) {
        mouseDown = false;
        self.window.mouseIsPressed = false;
        if (p5Instance) p5Instance.mouseIsPressed = false;
        if (typeof self.mouseReleased === "function") { try { self.mouseReleased(); } catch (err) { console.warn(err); } }
        if (typeof self.mouseClicked === "function") { try { self.mouseClicked(); } catch (err) { console.warn(err); } }
      } else if (e.is("keyboard:down")) {
        self.window.key = e.key || "";
        self.window.keyCode = (e.key || "").toUpperCase().charCodeAt(0) || 0;
        self.window.keyIsPressed = true;
        if (typeof self.keyPressed === "function") { try { self.keyPressed(); } catch (err) { console.warn(err); } }
      } else if (e.is("keyboard:up")) {
        self.window.keyIsPressed = false;
        if (typeof self.keyReleased === "function") { try { self.keyReleased(); } catch (err) { console.warn(err); } }
      }
    },

    leave: () => {
      try { p5Instance?.remove?.(); } catch {}
      p5Instance = null;
      canvasShim = null;
      cachedCtx = null;
      restoreRAF();
      clearSketchGlobals();
    },
  };
}
