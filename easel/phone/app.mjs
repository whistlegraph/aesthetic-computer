// Aesel, the hosted half, with a touch interface.
//
// This file is the phone's *shell*: sign-in, one piece, a transcript, a live
// preview. The thinking is not here — `AcServer` is imported straight out of
// `easel/src` and runs unmodified, so the agent loop, the guides, the tool
// contract and the twelve-round bound are literally the desktop's. That is the
// whole architectural bet: a phone client that reimplements the loop would
// drift from it inside a month.
//
// What is genuinely different on a phone, and therefore lives here:
//
//   * Sign-in. The desktop runs a loopback server on :44233 and shells out to a
//     browser. A phone cannot, so the token arrives another way — from the dev
//     endpoint while iterating, and from ASWebAuthenticationSession once the
//     Swift shell wraps this page.
//   * Storage. There is no ~/.local/share and no home directory. The piece
//     lives in localStorage here and in the app's Documents container later;
//     `mountPiece` is the only thing that has to change.
//   * Publishing on every write. On desktop a file watcher notices the save and
//     auto-publish takes it from there. Nothing is watching a Map, so the write
//     handler publishes directly — which is also what makes the preview live.

import { AcServer, AC_MODELS, DEFAULT_AC_MODEL } from "/easel/src/ac-server.mjs";
import { Energy, energyReport, formatJoules } from "/easel/src/energy.mjs";
import { publishPiece } from "/easel/src/publish.mjs";
import * as vfs from "/easel/phone/shim/fs.mjs";

const SITE = "https://aesthetic.computer";
const AUTH_DOMAIN = "hi.aesthetic.computer";
const STORE = "aesel.phone.v1";

// The four guides, at the paths `bundledContext()` inside the bridge will ask
// for. They are fetched rather than bundled so a guide edit reaches the phone
// on reload, the same way it reaches the desktop on restart.
const GUIDES = [
  "/easel/context/pieces.md",
  "/easel/context/screen.md",
  "/easel/context/hand.md",
  "/easel/context/kidlisp.md",
];

const el = (id) => document.getElementById(id);
const ui = {
  app: el("app"),
  gate: el("gate"),
  gateBody: el("gate-body"),
  gateNote: el("gate-note"),
  route: el("route"),
  status: el("status"),
  watts: el("watts"),
  stage: el("stage"),
  preview: el("preview"),
  log: el("log"),
  composer: el("composer"),
  input: el("input"),
  send: el("send"),
  stop: el("stop"),
};

const state = {
  token: "",
  handle: "",
  slug: "",
  file: "",
  server: null,
  busy: false,
  streaming: null, // the <li> currently receiving agentMessage deltas
  publishing: null, // in-flight publish, so rapid writes coalesce
  dirty: false,
};

// ─── persistence ────────────────────────────────────────────────────────────
// One JSON blob. Small enough that a read-modify-write is cheaper than keys,
// and it keeps "what a session is" in one visible shape.

function load() {
  try {
    return JSON.parse(localStorage.getItem(STORE) || "{}");
  } catch {
    return {};
  }
}

function save(patch) {
  try {
    localStorage.setItem(STORE, JSON.stringify({ ...load(), ...patch }));
  } catch {
    // Private browsing, or a full quota. The session still works for as long as
    // the tab lives; losing it is better than refusing to start.
  }
}

// ─── transcript ─────────────────────────────────────────────────────────────

function line(kind, tag, text) {
  const li = document.createElement("li");
  li.className = kind;
  if (tag) {
    const span = document.createElement("span");
    span.className = "tag";
    span.textContent = tag;
    li.append(span, " ");
  }
  li.append(document.createTextNode(text));
  ui.log.append(li);
  scrollDown();
  return li;
}

// Only follow the tail when the reader is already at it — otherwise scrolling
// back to re-read something gets yanked away by the next delta.
function scrollDown() {
  const atBottom = ui.log.scrollHeight - ui.log.scrollTop - ui.log.clientHeight < 80;
  if (atBottom) ui.log.scrollTop = ui.log.scrollHeight;
}

// What the session has spent in electricity, as far as the reported token
// counts can say.
const energy = new Energy();

function setStatus(text, stateName = "idle") {
  ui.status.textContent = text;
  ui.status.dataset.state = stateName;
}

// ─── the piece ──────────────────────────────────────────────────────────────

// A slug a person can say out loud. The desktop names threads the same way.
function freshSlug() {
  const vowels = "aeiou";
  const consonants = "bcdfghjklmnprstvwz";
  let out = "";
  for (let i = 0; i < 3; i += 1) {
    out += consonants[Math.floor(Math.random() * consonants.length)];
    out += vowels[Math.floor(Math.random() * vowels.length)];
  }
  return out;
}

const STARTER = `// A new piece.
export function paint({ wipe }) {
  wipe("purple");
}
`;

function mountPiece(slug, source) {
  state.slug = slug;
  state.file = `/piece/${slug}.mjs`;
  vfs.mount(state.file, source);
  ui.route.textContent = state.handle ? `@${state.handle}/${slug}` : slug;
  save({ slug, source });
}

// Every `write_piece` lands here. Persist, then publish — publishing is what
// makes the preview true, so it is not a background nicety.
function onPieceWritten(path, source) {
  if (path !== state.file) return;
  save({ slug: state.slug, source });
  state.dirty = true;
  void publishNow();
}

async function publishNow() {
  if (state.publishing) return state.publishing; // coalesce; the last write wins
  if (!state.handle) {
    line("note", "", "Not published — this account has no @handle yet.");
    return;
  }
  state.dirty = false;
  state.publishing = (async () => {
    try {
      setStatus("publishing", "working");
      await publishPiece({
        file: state.file,
        slug: state.slug,
        cwd: "/piece",
        site: SITE,
        // publish.mjs wants an AcSession; it only ever reads `.handle`,
        // `.signedIn` and awaits `.token()`.
        session: {
          handle: state.handle,
          signedIn: true,
          token: async () => state.token,
        },
      });
      save({ published: true });
      showPreview();
      setStatus("live", "live");
    } catch (error) {
      line("bad", "!", `Publish failed: ${error.message}`);
      setStatus("not published", "failed");
    } finally {
      state.publishing = null;
      // A write that arrived mid-publish still needs its own upload.
      if (state.dirty) void publishNow();
    }
  })();
  return state.publishing;
}

// Cache-busted because the piece URL is stable and the bytes behind it are not.
function showPreview() {
  if (!state.handle) return;
  const url = `${SITE}/@${state.handle}/${state.slug}?nolabel=true&nogap=true#${Date.now()}`;
  ui.preview.src = url;
  ui.stage.classList.add("has-piece");
}

// ─── the bridge ─────────────────────────────────────────────────────────────

function buildServer() {
  const server = new AcServer({
    cwd: "/piece",
    model: DEFAULT_AC_MODEL,
    piece: { file: state.file },
    // The bridge awaits `token()` per turn rather than holding a string, so a
    // desktop session can refresh a stale one mid-conversation. The phone has
    // nothing to refresh yet, so this just answers.
    token: async () => state.token,
    site: SITE,
    // `ac-server` stores this and calls it as `this.fetch(…)`. Node tolerates
    // that; a browser throws "Illegal invocation" because window.fetch must be
    // called on window. Binding here is cheaper than a method on the bridge.
    fetch: globalThis.fetch.bind(globalThis),
  });
  server.on("notification", handle);
  return server;
}

// The bridge's whole output surface. Six methods, documented by the desktop
// TUI's use of them; anything unrecognised is ignored rather than rendered, so
// a new notification type cannot garble the transcript.
function handle({ method, params }) {
  if (method === "turn/started") {
    state.streaming = null;
    setStatus("thinking", "working");
    return;
  }

  // The bridge reports its token counts per round; energy.mjs turns them into
  // watt-hours. It is an estimate, which is why the chip wears a tilde and the
  // basis is one tap away.
  if (method === "turn/usage") {
    energy.add(params?.model || DEFAULT_AC_MODEL, params?.usage);
    ui.watts.hidden = !energy.counted;
    ui.watts.textContent = `~${formatJoules(energy.joules)}`;
    return;
  }

  if (method === "turn/progress") {
    const phase = params?.phase;
    if (phase) setStatus(phase, "working");
    return;
  }

  if (method === "item/agentMessage/delta") {
    if (!state.streaming) state.streaming = line("ac", "AC", "");
    state.streaming.append(document.createTextNode(params.delta));
    scrollDown();
    return;
  }

  if (method === "item/completed") {
    const item = params?.item;
    if (item?.type === "agentMessage") {
      // The delta stream already rendered it; this is the same text arriving
      // whole. Only draw it if nothing streamed.
      if (!state.streaming) line("ac", "AC", item.text || "");
      state.streaming = null;
    } else if (item?.type === "fileChange") {
      const status = String(item.status || "");
      const bad = status.startsWith("failed");
      line(bad ? "bad" : "edit", bad ? "!" : "EDIT", status || "written");
    }
    return;
  }

  if (method === "turn/completed") {
    const turn = params?.turn;
    state.streaming = null;
    if (turn?.error?.message) {
      line("bad", "!", turn.error.message);
      setStatus("failed", "failed");
    } else if (turn?.status === "interrupted") {
      line("note", "", "Stopped.");
      setStatus("stopped");
    } else {
      setStatus(state.publishing ? "publishing" : "ready");
    }
    setBusy(false);
  }
}

function setBusy(busy) {
  state.busy = busy;
  ui.send.hidden = busy;
  ui.stop.hidden = !busy;
  ui.input.disabled = false; // let them type the next thing while it works
}

async function ask(text) {
  if (!text.trim() || state.busy) return;
  line("you", "YOU", text);
  setBusy(true);
  try {
    if (!state.server) state.server = buildServer();
    await state.server.startTurn(text);
  } catch (error) {
    line("bad", "!", error.message);
    setStatus("failed", "failed");
    setBusy(false);
  }
}

// ─── sign-in ────────────────────────────────────────────────────────────────
//
// Two ways in, both temporary. `/dev-token` is the laptop handing over its own
// ~/.ac-token so a phone on the LAN can be tested before any Apple plumbing
// exists; the paste field is the fallback when the page is served from
// somewhere else. Neither survives into the shipped app — the Swift shell will
// run ASWebAuthenticationSession against the same Auth0 client and call
// `window.aeselAdoptToken`, which is why that global exists.

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
  save({ token, handle });
  return handle;
}

globalThis.aeselAdoptToken = async (token) => {
  await adoptToken(token);
  await start();
};

function gateNote(text, bad = false) {
  ui.gateNote.textContent = text;
  ui.gateNote.classList.toggle("bad", bad);
}

function offerPaste() {
  ui.gateBody.innerHTML = "";
  const input = document.createElement("input");
  input.type = "password";
  input.placeholder = "Paste an access token";
  input.autocapitalize = "off";
  input.autocomplete = "off";
  const button = document.createElement("button");
  button.textContent = "Sign in";
  const note = document.createElement("p");
  note.id = "gate-note";
  ui.gateBody.append(input, button, note);
  ui.gateNote = note;
  gateNote("From ~/.ac-token on a machine you have run ac-login on.");

  button.onclick = async () => {
    const token = input.value.trim();
    if (!token) return;
    button.disabled = true;
    gateNote("Checking…");
    try {
      await adoptToken(token);
      await start();
    } catch (error) {
      button.disabled = false;
      gateNote(error.message, true);
    }
  };
}

// ─── boot ───────────────────────────────────────────────────────────────────

async function start() {
  ui.gate.hidden = true;
  ui.app.hidden = false;

  const saved = load();
  mountPiece(saved.slug || freshSlug(), saved.source || STARTER);

  if (!state.handle) {
    line("note", "", "Signed in, but this account has no @handle yet. Claim one at aesthetic.computer/handle to publish.");
  } else if (saved.published && saved.slug === state.slug) {
    // Only a piece that actually reached the bucket last time. Pointing the
    // frame at an unpublished slug just renders AC's 404 into the stage.
    showPreview();
  }
  setStatus("ready");
  ui.input.focus({ preventScroll: true });
}

async function boot() {
  vfs.setWriteHandler(onPieceWritten);

  const missing = await vfs.preload(GUIDES);
  if (missing.length) {
    console.warn("[aesel] guides missing:", missing);
  }

  const saved = load();
  if (saved.token) {
    state.token = saved.token;
    state.handle = saved.handle || "";
    // Re-resolve rather than trust the cache: a handle can be claimed between
    // sessions, and a token can have expired.
    try {
      state.handle = await resolveHandle(saved.token);
      save({ handle: state.handle });
      return start();
    } catch {
      gateNote("That session expired.");
    }
  }

  try {
    const response = await fetch("/dev-token");
    if (response.ok) {
      const { access_token } = await response.json();
      await adoptToken(access_token);
      return start();
    }
  } catch {
    // No dev endpoint: this page is not being served by serve.mjs --token.
  }

  offerPaste();
}

// ─── wiring ─────────────────────────────────────────────────────────────────

ui.composer.addEventListener("submit", (event) => {
  event.preventDefault();
  const text = ui.input.value;
  ui.input.value = "";
  ui.input.style.height = "auto";
  void ask(text);
});

// Enter sends, Shift+Enter breaks the line — but only where there is a real
// keyboard. On a touch screen Enter has to insert a newline or a two-line
// prompt becomes impossible to type.
ui.input.addEventListener("keydown", (event) => {
  const touch = matchMedia("(hover: none)").matches;
  if (event.key === "Enter" && !event.shiftKey && !touch) {
    event.preventDefault();
    ui.composer.requestSubmit();
  }
});

ui.input.addEventListener("input", () => {
  ui.input.style.height = "auto";
  ui.input.style.height = `${ui.input.scrollHeight}px`;
});

ui.stop.addEventListener("click", () => {
  state.server?.interrupt?.();
  state.server?.controller?.abort?.();
});

ui.route.parentElement.addEventListener("click", () => {
  if (state.handle) window.open(`${SITE}/@${state.handle}/${state.slug}`, "_blank");
});

// The chip is a number; the tap is the working behind it — including the same
// conversation priced across every model, which is the comparison the estimate
// can actually defend.
ui.watts.addEventListener("click", () => {
  for (const text of energyReport(energy, DEFAULT_AC_MODEL)) {
    if (text) line("note", "", text);
  }
});

void boot();
