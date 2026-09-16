// The headless half of a phone session: everything that is not a renderer.
//
// `app.mjs` draws the browser UI and `apple/aesel` draws a SwiftUI one. Both
// want the same things to happen — mount a piece, run a turn through the shared
// bridge, publish what the model writes, report progress — so that lives here
// and the hosts only render.
//
// This is the same division Oskiewar makes. Its engine emits a triangle stream
// and canvas, D3D and Metal each draw it; `MetalSceneView.swift` says so in its
// header. Aesel's equivalent of that stream already existed before anyone
// needed a second renderer: `AcServer` emits six notification kinds, and the
// desktop TUI is just one thing that reads them. A SwiftUI view is another.
//
// Nothing here touches the DOM, so it runs the same in a visible page and in
// the off-screen WKWebView the iOS app keeps for exactly this purpose.

import { AcServer, DEFAULT_AC_MODEL } from "/easel/src/ac-server.mjs";
import { publishPiece } from "/easel/src/publish.mjs";
import * as vfs from "/easel/phone/shim/fs.mjs";

export const SITE = "https://aesthetic.computer";
export const AUTH_DOMAIN = "hi.aesthetic.computer";

// Fetched rather than bundled, at the paths the bridge's own `bundledContext()`
// builds, so editing a guide reaches the phone on reload.
const GUIDES = [
  "/easel/context/pieces.md",
  "/easel/context/screen.md",
  "/easel/context/hand.md",
  "/easel/context/kidlisp.md",
];

const STARTER = `// A new piece.
export function paint({ wipe }) {
  wipe("purple");
}
`;

// A slug a person can say out loud.
export function freshSlug() {
  const vowels = "aeiou";
  const consonants = "bcdfghjklmnprstvwz";
  let out = "";
  for (let i = 0; i < 3; i += 1) {
    out += consonants[Math.floor(Math.random() * consonants.length)];
    out += vowels[Math.floor(Math.random() * vowels.length)];
  }
  return out;
}

// `publish.mjs` sets a `User-Agent` header, which is right for Node and wrong
// in a webview: it is a forbidden header name, so a browser is supposed to drop
// it. Chrome does, silently — the browser client publishes fine. WebKit leaks
// it into the CORS preflight instead, and `/presigned-upload-url` answers
// `Access-Control-Allow-Headers: Content-Type, Authorization, X-Requested-With`
// with no `user-agent`, so the preflight is refused and the whole publish comes
// back as the uninformative "Load failed".
//
// Stripping it here rather than in `publish.mjs` keeps the shared file correct
// for the desktop, which genuinely wants to identify itself.
const FORBIDDEN = ["user-agent"];

function browserFetch(input, init = {}) {
  if (!init.headers) return fetch(input, init);
  const headers = {};
  for (const [name, value] of Object.entries(init.headers)) {
    if (FORBIDDEN.includes(name.toLowerCase())) continue;
    headers[name] = value;
  }
  return fetch(input, { ...init, headers });
}

// Storage is injected because the two hosts keep a session in different places
// — localStorage in a browser, the app's Documents container on iOS — and that
// is the only thing about persistence they disagree on.
const memoryStore = () => {
  const map = new Map();
  return {
    get: (key) => map.get(key) ?? null,
    set: (key, value) => void map.set(key, value),
  };
};

export function createSession({ storage = memoryStore(), emit = () => {} } = {}) {
  const state = {
    token: "",
    handle: "",
    slug: "",
    file: "",
    server: null,
    busy: false,
    publishing: null,
    dirty: false,
    published: false,
  };

  const read = () => {
    try {
      return JSON.parse(storage.get("session") || "{}");
    } catch {
      return {};
    }
  };

  const write = (patch) => {
    try {
      storage.set("session", JSON.stringify({ ...read(), ...patch }));
    } catch {
      // A full or unavailable store should cost persistence, not the session.
    }
  };

  // Every notification the hosts render flows through here, so a host never
  // has to know whether something came from the bridge or from this file.
  const say = (type, payload = {}) => emit({ type, ...payload });

  function route() {
    return state.handle ? `@${state.handle}/${state.slug}` : state.slug;
  }

  function pieceUrl() {
    if (!state.handle) return "";
    // Cache-busted: the URL is stable and the bytes behind it are not.
    return `${SITE}/@${state.handle}/${state.slug}?nolabel=true&nogap=true#${Date.now()}`;
  }

  function mountPiece(slug, source) {
    state.slug = slug;
    state.file = `/piece/${slug}.mjs`;
    state.server = null; // a new piece is a new conversation
    vfs.mount(state.file, source);
    write({ slug, source });
    say("piece", { route: route(), slug, source });
  }

  function onWritten(path, source) {
    if (path !== state.file) return;
    write({ slug: state.slug, source });
    state.dirty = true;
    say("source", { source });
    void publish();
  }

  async function publish() {
    if (state.publishing) return state.publishing;
    if (!state.handle) {
      say("note", { text: "Not published — this account has no @handle yet." });
      return;
    }
    state.dirty = false;
    let lastStep = "starting";
    state.publishing = (async () => {
      try {
        say("status", { text: "publishing", kind: "working" });
        await publishPiece({
          file: state.file,
          slug: state.slug,
          cwd: "/piece",
          site: SITE,
          // publish.mjs wants an AcSession and only ever reads these three.
          session: { handle: state.handle, signedIn: true, token: async () => state.token },
          fetch: browserFetch,
          // Named steps, because "Publish failed: Load failed" does not say
          // whether the grant, the upload or the verify was the thing that
          // could not load, and those fail for different reasons.
          onStep: (step) => {
            lastStep = step;
            say("status", { text: step, kind: "working" });
          },
        });
        state.published = true;
        write({ published: true });
        say("status", { text: "live", kind: "live" });
        say("preview", { url: pieceUrl() });
      } catch (error) {
        say("bad", { text: `Publish failed at "${lastStep}": ${error.message}` });
        say("status", { text: "not published", kind: "failed" });
      } finally {
        state.publishing = null;
        if (state.dirty) void publish(); // a write arrived mid-upload
      }
    })();
    return state.publishing;
  }

  function buildServer() {
    const server = new AcServer({
      cwd: "/piece",
      model: DEFAULT_AC_MODEL,
      piece: { file: state.file },
      // The bridge awaits `token()` per turn so a desktop session can refresh
      // a stale one mid-conversation; the phone has nothing to refresh yet.
      token: async () => state.token,
      site: SITE,
      // Stored and called as `this.fetch(…)`. Node tolerates that; a browser
      // throws "Illegal invocation" unless window.fetch is bound to window.
      fetch: globalThis.fetch.bind(globalThis),
    });
    server.on("notification", ({ method, params }) => say("bridge", { method, params }));
    return server;
  }

  async function ask(text) {
    if (!text.trim() || state.busy) return;
    say("you", { text });
    state.busy = true;
    say("busy", { busy: true });
    try {
      if (!state.server) state.server = buildServer();
      await state.server.startTurn(text);
    } catch (error) {
      say("bad", { text: error.message });
      say("status", { text: "failed", kind: "failed" });
    } finally {
      state.busy = false;
      say("busy", { busy: false });
    }
  }

  function stop() {
    state.server?.interrupt?.();
    state.server?.controller?.abort?.();
  }

  // Two calls, because a token is worth nothing here without the @handle that
  // says where a piece goes.
  async function resolveHandle(token) {
    const who = await fetch(`https://${AUTH_DOMAIN}/userinfo`, {
      headers: { Authorization: `Bearer ${token}` },
    });
    if (!who.ok) throw new Error(`Sign-in check failed (HTTP ${who.status}).`);
    const user = await who.json();
    const lookup = await fetch(`${SITE}/handle?for=${encodeURIComponent(user.sub)}`, {
      headers: { Accept: "application/json" },
    });
    const data = await lookup.json().catch(() => ({}));
    return String(data?.handle || "").replace(/^@/, "");
  }

  async function adoptToken(token) {
    const handle = await resolveHandle(token);
    state.token = token;
    state.handle = handle;
    write({ token, handle });
    say("signedIn", { handle });
    return handle;
  }

  // Resolves to true when a stored session was still good.
  async function restore() {
    const saved = read();
    if (!saved.token) return false;
    try {
      state.token = saved.token;
      state.handle = await resolveHandle(saved.token);
      write({ handle: state.handle });
      say("signedIn", { handle: state.handle });
      return true;
    } catch {
      return false;
    }
  }

  async function open() {
    const saved = read();
    mountPiece(saved.slug || freshSlug(), saved.source || STARTER);
    state.published = Boolean(saved.published && saved.slug === state.slug);
    if (state.handle && state.published) say("preview", { url: pieceUrl() });
    say("status", { text: "ready", kind: "idle" });
  }

  async function begin() {
    vfs.setWriteHandler(onWritten);
    const missing = await vfs.preload(GUIDES);
    if (missing.length) say("note", { text: `Guides missing: ${missing.join(", ")}` });
    say("ready", { signedIn: Boolean(state.token) });
  }

  return {
    state,
    begin,
    restore,
    adoptToken,
    open,
    ask,
    stop,
    publish,
    newPiece: () => mountPiece(freshSlug(), STARTER),
    route,
    pieceUrl,
  };
}
