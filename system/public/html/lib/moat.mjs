// moat.mjs — shared base for the AC "HTML moat sites".
// These are fast, dependency-free HTML front-doors that wrap the AC runtime:
//   • laklok.com/html — the Laer Klokken chat client
//   • prompt.ac       — the HTML prompt shell (AC runtime in an iframe)
// Both load instantly in plain DOM and talk to AC over its existing wire
// protocol, instead of waiting on the full runtime boot. This module holds the
// pieces they have in common: in-message token parsing, handle coloring,
// relative time, and the iframe bridge that drives a hosted AC runtime.

// ── Token parsing ──────────────────────────────────────────────────────────
// Mirrors lib/chat-highlighting.mjs: @handles, urls, 'prompts', #paintings,
// $kidlisp, *clock. Order matters — longest / most-specific alternative first.
const TOKEN =
  /(@[a-z0-9][a-z0-9._]*|https?:\/\/[^\s]+|'[^']+'|#[a-z0-9]{3,}|\$[a-z0-9]+|\*[a-z0-9]+)/gi;

const KIND = {
  "@": "handle",
  h: "url",
  "'": "prompt",
  "#": "painting",
  $: "kidlisp",
  "*": "clock",
};

// Split `text` into typed segments. Plain runs come back as { type: "text" }.
export function parseTokens(text = "") {
  const out = [];
  let last = 0,
    m;
  TOKEN.lastIndex = 0;
  while ((m = TOKEN.exec(text))) {
    if (m.index > last) out.push({ type: "text", value: text.slice(last, m.index) });
    const tok = m[0];
    const key = tok[0] === "h" || /^https?:/i.test(tok) ? "h" : tok[0];
    out.push({ type: KIND[key] || "text", value: tok });
    last = m.index + tok.length;
  }
  if (last < text.length) out.push({ type: "text", value: text.slice(last) });
  return out;
}

// Render `text` into a DocumentFragment with colored, optionally-clickable
// token spans (classes: t-handle / t-url / t-prompt / t-painting / t-kidlisp /
// t-clock — each moat site styles these). `open` defaults to opening the AC
// page for handles + kidlisp; pass your own to intercept.
export function renderBody(text, open = defaultOpen) {
  const frag = document.createDocumentFragment();
  for (const seg of parseTokens(text)) {
    if (seg.type === "text") {
      frag.append(document.createTextNode(seg.value));
      continue;
    }
    let node;
    if (seg.type === "url") {
      node = document.createElement("a");
      node.className = "t-url";
      node.href = seg.value;
      node.target = "_blank";
      node.rel = "noopener nofollow";
      node.textContent = seg.value;
    } else {
      node = document.createElement("span");
      node.className = "t-" + seg.type;
      node.textContent = seg.value;
      if (seg.type === "handle") {
        node.style.color = handleColor(seg.value.slice(1).toLowerCase());
        node.title = "open " + seg.value;
        node.style.cursor = "pointer";
        node.onclick = () => open(seg.value);
      } else if (seg.type === "kidlisp") {
        node.style.cursor = "pointer";
        node.title = "kidlisp " + seg.value;
        node.onclick = () => open(seg.value);
      }
    }
    frag.append(node);
  }
  return frag;
}

function defaultOpen(slug) {
  window.open("https://aesthetic.computer/" + slug.replace(/^[@$#*']+/, (p) => p), "_blank");
}

// ── Handle coloring ────────────────────────────────────────────────────────
// Deterministic hue per handle — a stable social-graph color cue without a
// round-trip. (Real per-character /api/handle-colors is a later upgrade.)
export function handleColor(name) {
  let h = 0;
  for (let i = 0; i < name.length; i++) h = (h * 31 + name.charCodeAt(i)) >>> 0;
  return `hsl(${h % 360} 75% 72%)`;
}

// ── Relative time ──────────────────────────────────────────────────────────
export function timeAgo(when) {
  const t = new Date(when).getTime();
  const s = Math.max(0, (Date.now() - t) / 1000);
  if (s < 45) return "now";
  if (s < 3600) return Math.round(s / 60) + "m";
  if (s < 86400) return Math.round(s / 3600) + "h";
  return Math.round(s / 86400) + "d";
}

// ── Tiny DOM helper ────────────────────────────────────────────────────────
export function el(tag, props = {}, ...kids) {
  const node = Object.assign(document.createElement(tag), props);
  for (const k of kids) node.append(k);
  return node;
}

// ── ACFrame — the iframe bridge ────────────────────────────────────────────
// Wraps a hosted AC runtime (<iframe src="aesthetic.computer/...?nogap&nolabel">)
// and drives it from a parent HTML shell:
//   frame.load("clock")      → in-place piece swap (no reload) via `load-piece`
//   frame.onPiece(cb)        → fires when the runtime reports a new slug
//   frame.onReady(cb)        → fires once the runtime is booted + interactive
// In-place swap depends on boot.mjs's `load-piece` parent handler; if the
// runtime is older, call frame.load(slug, { reload: true }) to fall back to a
// full iframe.src navigation.
export class ACFrame {
  constructor(iframe, { origin = "https://aesthetic.computer" } = {}) {
    this.iframe = iframe;
    this.origin = origin;
    this.piece = null;
    this.ready = false;
    this._onPiece = [];
    this._onReady = [];
    window.addEventListener("message", (e) => {
      if (e.source !== iframe.contentWindow) return;
      const d = e.data || {};
      if (d.type === "url:updated" && typeof d.slug === "string") {
        this.piece = d.slug;
        this._onPiece.forEach((cb) => cb(d.slug));
      } else if (d.type === "ready" || d.type === "kidlisp-ready") {
        if (!this.ready) {
          this.ready = true;
          this._onReady.forEach((cb) => cb());
        }
      }
    });
  }
  // First boot — set the iframe src with chrome suppressed.
  boot(slug = "prompt", params = "nogap=true&nolabel=true") {
    this.iframe.src = `${this.origin}/${slug}?${params}`;
  }
  // Navigate. Fast path posts `load-piece`; reload path swaps the src.
  load(slug, { reload = false } = {}) {
    const clean = String(slug || "").trim();
    if (!clean) return;
    if (reload || !this.ready) {
      this.boot(clean);
    } else {
      this.iframe.contentWindow.postMessage({ type: "load-piece", slug: clean }, "*");
    }
  }
  onPiece(cb) { this._onPiece.push(cb); return this; }
  onReady(cb) { this._onReady.push(cb); return this; }
}

// ── Piece-name parsing (shell command bar) ─────────────────────────────────
// Light front-end of AC's parse(): pulls the leading piece slug for display /
// validation. The runtime still does the authoritative parse on load.
export function pieceName(input = "") {
  const s = input.trim();
  if (!s) return "prompt";
  return s.split(/\s+/)[0];
}
