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
import { fetchHandleColors, handleCharacterColors } from "/easel/src/handle-colors.mjs";
import { publishPiece } from "/easel/src/publish.mjs";
import * as vfs from "/easel/phone/shim/fs.mjs";
import { createCredits } from "./credits.mjs";

export const SITE = "https://aesthetic.computer";
export const AUTH_DOMAIN = "hi.aesthetic.computer";
export const MODEL_CHOICES = []; // Braincell routing is managed by AC.

// Fetched rather than bundled, at the paths the bridge's own `bundledContext()`
// builds, so editing a guide reaches the phone on reload.
const GUIDES = [
  "/easel/context/pieces.md",
  "/easel/context/screen.md",
  "/easel/context/hand.md",
  "/easel/context/kidlisp.md",
  "/easel/context/api.json",
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
    id: "",
    medium: "piece",
    transcript: [],
    engine: null,
    title: "",
    owner: "",
    model: DEFAULT_AC_MODEL,
  };
  const credits = createCredits({ token: () => state.token, emit, site: SITE });

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

  // Credentials stay in the separate account record, never in a thread.
  const readThreads = () => {
    try {
      const value = JSON.parse(storage.get("threads") || "{}");
      return value.schema === 1 && Array.isArray(value.items) ? value.items : [];
    } catch { return []; }
  };
  let saveTimer;
  const say = (type, payload = {}) => {
    const event = { type, ...payload };
    if (state.id && ["you", "note", "bad", "bridge"].includes(type)) {
      const last = state.transcript.at(-1);
      if (type === "bridge" && payload.method === "item/agentMessage/delta" && last?.method === payload.method) {
        last.params.delta += payload.params?.delta || "";
      } else {
        state.transcript.push(JSON.parse(JSON.stringify(event)));
      }
      clearTimeout(saveTimer);
      saveTimer = setTimeout(saveCurrent, 250);
    }
    emit(event);
  };

  function history() {
    return readThreads().sort((a, b) => b.savedAt.localeCompare(a.savedAt)).map(item => ({
      id: item.id, title: item.title || item.slug, medium: item.medium,
      route: item.handle ? `@${item.handle}/${item.slug}` : item.slug,
      updatedAt: item.savedAt,
    }));
  }

  function saveCurrent() {
    clearTimeout(saveTimer);
    if (!state.id || !state.file) return;
    const engine = state.server ? {
      threadId: state.server.threadId || "", messages: state.server.messages || [], turns: state.server.turns || 0, model: state.model,
    } : state.engine;
    const item = {
      id: state.id, title: state.title || state.slug, medium: state.medium, model: state.model,
      savedAt: new Date().toISOString(), handle: state.owner || state.handle, slug: state.slug,
      source: vfs.readFileSync(state.file), published: state.published,
      events: state.transcript, engine,
    };
    const items = readThreads().filter(entry => entry.id !== state.id);
    items.push(item);
    try {
      const encoded = JSON.stringify({schema: 1, items});
      if (encoded.length > 32 * 1024 * 1024) throw new Error("Saved threads exceed 32 MB.");
      storage.set("threads", encoded);
      write({ threadID: state.id });
      emit({ type: "history", items: history() });
    } catch (error) {
      emit({type: "bad", text: `Could not save this thread: ${error.message}`});
    }
  }

  async function settleCurrent() {
    stop();
    const deadline = Date.now() + 15000;
    while (state.busy || state.publishing) {
      if (Date.now() > deadline) throw new Error("Wait for the current turn or upload to finish before switching threads.");
      await new Promise(resolve => setTimeout(resolve, 50));
    }
    saveCurrent();
  }

  function loadThread(item) {
    state.id = item.id;
    state.owner = item.handle || state.handle;
    state.medium = item.medium || "piece";
    state.title = item.title || item.slug;
    state.transcript = Array.isArray(item.events) ? item.events : [];
    state.engine = item.engine || null;
    state.model = DEFAULT_AC_MODEL;
    mountPiece(item.slug, item.source || STARTER);
    state.published = Boolean(item.published && (!item.handle || item.handle === state.handle));
    write({ threadID: state.id, published: state.published });
    emit({type: "thread", id: state.id, medium: state.medium, events: state.transcript});
    say("model", {requested: state.model, choices: MODEL_CHOICES});
    if (state.published && state.handle) say("preview", {url: pieceUrl()});
    else if (item.source && item.source !== STARTER) say("source", {source: item.source});
    say("status", {text: state.token ? "ready" : "signed out", kind: "idle"});
  }

  async function resumeSession(id) {
    let item = readThreads().find(entry => entry.id === id);
    if (!item) throw new Error("That saved thread could not be found.");
    if (item.medium !== "piece") throw new Error("This medium is not supported on iPhone yet.");
    await settleCurrent();
    item = readThreads().find(entry => entry.id === id);
    loadThread(item);
    emit({type: "history", items: history()});
  }

  async function newSession(medium = "piece") {
    if (medium !== "piece") throw new Error("This medium is not supported on iPhone yet.");
    await settleCurrent();
    const slug = freshSlug();
    loadThread({id: `${Date.now()}-${slug}`, slug, medium, source: STARTER, events: [], published: false});
    saveCurrent();
  }

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
    state.published = false;
    write({ slug, source, published: false });
    say("piece", { route: route(), slug, source });
  }

  function onWritten(path, source) {
    if (path !== state.file) return;
    write({ slug: state.slug, source });
    saveCurrent();
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
          // Phone threads preserve drafts, but have no desktop revision ledger.
          version: null,
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
        state.owner = state.handle;
        write({ published: true });
        saveCurrent();
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
      model: state.model,
      piece: { file: state.file },
      // The bridge awaits `token()` per turn so a desktop session can refresh
      // a stale one mid-conversation; the phone has nothing to refresh yet.
      token: async () => state.token,
      site: SITE,
      // Stored and called as `this.fetch(…)`. Node tolerates that; a browser
      // throws "Illegal invocation" unless window.fetch is bound to window.
      fetch: globalThis.fetch.bind(globalThis),
    });
    if (state.engine) {
      server.threadId = state.engine.threadId || "";
      server.messages = JSON.parse(JSON.stringify(state.engine.messages || []));
      server.turns = state.engine.turns || 0;
    }
    server.on("notification", ({ method, params }) => {
      if (method === "model/reported") say("model", {requested: params.requested, reported: params.reported});
      say("bridge", { method, params });
    });
    return server;
  }

  function setModel(input) {
    if (state.busy || state.publishing) throw new Error("Wait for this turn and upload to finish before changing models.");
    if (input !== DEFAULT_AC_MODEL) throw new Error("Braincell models are managed automatically.");
    state.model = DEFAULT_AC_MODEL;
    if (state.server) state.server.model = state.model;
    if (state.engine) state.engine.model = state.model;
    say("model", {requested: state.model, choices: MODEL_CHOICES});
    saveCurrent();
    return state.model;
  }

  async function ask(text) {
    const command = text.trim().match(/^\/model(?:\s+(.+))?$/i);
    if (command) {
      if (command[1]) setModel(command[1]);
      else say("note", {text: "Braincell models are managed automatically."});
      return;
    }
    if (!text.trim() || state.busy) return;
    if (!state.token) { say("bad", { text: "Sign in to AC to make a piece." }); return; }
    if (!state.title || state.title === state.slug) state.title = text.trim().slice(0, 120);
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
      saveCurrent();
      void credits.refresh();
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

  async function loadHandleColors(handle) {
    let colors = handleCharacterColors(`@${handle}`);
    try { colors = await fetchHandleColors(`@${handle}`); } catch { }
    if (state.handle !== handle) return;
    say("handleColors", {handle, colors: colors.map(rgb => "#" + rgb.map(n => n.toString(16).padStart(2, "0")).join(""))});
  }

  async function adoptToken(token) {
    const handle = await resolveHandle(token);
    state.token = token;
    state.handle = handle;
    write({ token, handle });
    say("signedIn", { handle });
    void loadHandleColors(handle);
    void credits.refresh();
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
      void loadHandleColors(state.handle);
      void credits.refresh();
      return true;
    } catch {
      state.token = "";
      state.handle = "";
      write({ token: "", handle: "" });
      credits.clear();
      return false;
    }
  }

  function signOut() {
    saveCurrent();
    stop();
    state.token = "";
    state.handle = "";
    state.server = null;
    write({ token: "", handle: "" });
    say("signedOut");
    credits.clear();
  }

  async function open() {
    const saved = read();
    const prior = readThreads().find(item => item.id === saved.threadID);
    if (prior) {
      loadThread(prior);
    } else {
      const slug = saved.slug || freshSlug();
      loadThread({id: `${Date.now()}-${slug}`, slug, source: saved.source || STARTER,
        published: Boolean(saved.published), handle: saved.handle || state.handle,
        medium: "piece", events: []});
      saveCurrent();
    }
    emit({type: "history", items: history()});
  }

  async function begin() {
    vfs.setWriteHandler(onWritten);
    const missing = await vfs.preload(GUIDES);
    if (missing.length) say("note", { text: `Guides missing: ${missing.join(", ")}` });
    say("model", { requested: state.model, choices: MODEL_CHOICES });
    say("ready", { signedIn: Boolean(state.token) });
  }

  return {
    state,
    begin,
    restore,
    signOut,
    adoptToken,
    open,
    ask,
    stop,
    publish,
    newPiece: newSession,
    newSession,
    resumeSession,
    saveCurrent,
    setModel,
    history,
    refreshCredits: credits.refresh,
    route,
    pieceUrl,
  };
}
