// cancelok, 26.07.14
// A machine makes instruments. You decide which ones live. There is no button
// for it — you decide by staying.
//
// The whole screen is the pad, full-bleed and playable. Swipe up from the bottom
// edge for the next one; swipe down from the top edge to go back. That's the
// entire interface, and the verdict is a side effect of using it: linger and play
// and it's an `ok`, flick past it in two seconds and it's a `cancel`. The two
// signals never went away — you just cast them with your attention instead of
// your thumb, and attention is the thing that was actually scarce.
//
// Descended from nopaint: "Press Paint if you like what you see or No if you
// don't." Make it first, judge it after. Except now judging isn't a press either.
//
// The candidate is HOSTED, not linked: we import a pad's module and drive its
// lifecycle ourselves. lib/pads.mjs is a session singleton whose config each pad
// re-asserts in its own boot, so hosting one is just calling it. Only the thin
// edge bands are ours; every other pixel belongs to the instrument.

const EDGE = 40; // the swipe bands, top and bottom — a home-indicator's worth
const SWIPE = 30; // travel before a drag becomes a page turn
const LINGER = 6000; // stay this long and you meant it
const NAME_MS = 2600; // the name fades — it's a label, not furniture

const OK = [90, 255, 150];
const NO = [255, 70, 80];

const ORIGIN =
  typeof location !== "undefined" && location.origin && location.origin !== "null"
    ? location.origin
    : "https://aesthetic.computer";
const LOCAL = /localhost|127\.0\.0\.1/.test(ORIGIN);
const API = LOCAL ? "http://localhost:8901/verdict" : `${ORIGIN}/api/cancelok`;

// One id for the whole sitting, so a session reads as a session: what you were
// shown, in what order, how long you stayed, what you did with your hands.
const SESSION = `${Date.now().toString(36)}-${Math.floor(Math.random() * 1e6).toString(36)}`;

let state = "loading"; // loading | judging | error
let error = "";

let deck = [];
let at = 0;
let host = null;
let hostFailed = false;

let judge = null; // your handle, or null for anon
let shownAt = 0; // when this pad came up
let taps = 0; // times you played it
let seen = {}; // code → how many times you've come back to it
let sent = 0; // verdicts the server took
let lost = 0; // verdicts it didn't — shown, because silence is how I lost yours

// Real fps, on a real device, measured while a real person watches. The headless
// gate can only certify that a pad runs on a Mac in a datacenter; THIS is whether
// it ran for you. A pad that stutters on the machine it's judged on has already
// been cancelled by the hardware.
let frames = 0;
let fpsAt = 0;
let fps = 0;
let fpsMin = 999;

let drag = null; // { from, y0, moved } — a gesture that began in an edge band
let flash = 0; // the last verdict, briefly, as a colored edge
let flashOk = false;

const now = () => Date.now();
const current = () => deck[at];

async function load(api) {
  const asked = (api.params?.[0] || "").replace(/^\^/, "").toLowerCase();
  const res = await fetch("/aesthetic.computer/bags.json");
  if (!res.ok) throw new Error("bags.json HTTP " + res.status);
  const bag = (await res.json()).bags?.[asked || "pads"];
  if (!bag) throw new Error("no bag named ^" + asked);
  deck = (bag.items || [])
    .filter((it) => it.type === "piece")
    .map((it) => ({ code: it.code, name: it.name || it.code }));
  if (!deck.length) throw new Error("that bag holds no pieces");
  deck.reverse(); // newest first — the fresh ones are the point
}

// Send it. ALWAYS a plain fetch, with the token attached only if one shows up in
// time. `net.userRequest` looked right and was a trap: it awaits authorize(),
// which never settles when you're logged out — so the request hung forever, the
// catch never ran, and a whole session of verdicts vanished without a sound.
// Never again: a failure to send is counted and painted on screen.
async function send(api, row) {
  const headers = { "content-type": "application/json" };
  try {
    const token = await Promise.race([
      api.authorize?.() ?? Promise.resolve(null),
      new Promise((r) => setTimeout(() => r(null), 1200)), // logged out = silence
    ]);
    if (token) headers.Authorization = `Bearer ${token}`;
  } catch {
    // no token. you still count, you're just anonymous.
  }
  try {
    const res = await fetch(API, { method: "POST", headers, body: JSON.stringify(row) });
    if (!res.ok) throw new Error("HTTP " + res.status);
    sent += 1;
  } catch {
    lost += 1;
  }
}

// You're leaving a pad. Whatever you did while you were here IS the verdict —
// nobody asked you a question, so nobody had to be honest.
function leaving(api) {
  const c = current();
  if (!c || !shownAt) return;
  const dwellMs = now() - shownAt;
  const kept = taps > 0 || dwellMs >= LINGER;
  flash = 1;
  flashOk = kept;
  send(api, {
    session: SESSION,
    code: c.code,
    verdict: kept ? "ok" : "cancel",
    dwellMs,
    taps,
    fps: Math.round(fps),
    fpsMin: fpsMin === 999 ? 0 : Math.round(fpsMin),
    revisit: seen[c.code] || 0,
    failed: hostFailed,
    w: api.screen.width,
    h: api.screen.height,
  });
}

async function mount(api) {
  host = null;
  hostFailed = false;
  taps = 0;
  shownAt = now();
  frames = 0;
  fpsAt = 0;
  fps = 0;
  fpsMin = 999;
  const { code } = current();
  seen[code] = (seen[code] || 0) + 1;
  try {
    // Origin-qualified: a disk runs as a blob module, so it has no base URL to
    // resolve `./x.mjs` against, and updateCode only rewrites static imports.
    const mod = await import(`${ORIGIN}/aesthetic.computer/disks/${code}.mjs`);
    mod.boot?.(api);
    host = mod;
  } catch (e) {
    hostFailed = true;
    error = String(e?.message || e);
  }
}

// Turn the page. Going BACK is not a neutral act — you returned to something, and
// coming back is the strongest keep signal in the whole system.
function turn(api, dir) {
  leaving(api);
  at = (at + dir + deck.length) % deck.length;
  mount(api);
}

function boot(api) {
  api.wipe(6, 6, 10);
  api.hud?.label?.("");
  judge = api.handle?.() || null;
  load(api)
    .then(() => {
      state = "judging";
      return mount(api);
    })
    .catch((e) => {
      state = "error";
      error = String(e?.message || e);
    });
}

function sim(api) {
  if (state === "judging" && host && !hostFailed) {
    try {
      host.sim?.(api);
    } catch (e) {
      hostFailed = true;
      error = String(e?.message || e);
    }
  }
  if (flash > 0) flash -= 0.04;
}

function paint(api) {
  const { wipe, ink, write, box, screen } = api;
  const w = screen.width;
  const h = screen.height;

  if (state === "loading") {
    wipe(6, 6, 10);
    ink(150).write("dealing…", { center: "xy" });
    return;
  }
  if (state === "error") {
    wipe(6, 6, 10);
    ink(...NO).write(error, { center: "x", y: h / 2 - 4 });
    return;
  }

  // Frame cadence — the honest one, on the machine a person is actually holding.
  const t = performance?.now?.() ?? now();
  if (fpsAt) {
    const dt = t - fpsAt;
    if (dt > 0 && dt < 1000) {
      const inst = 1000 / dt;
      fps = fps ? fps + (inst - fps) * 0.1 : inst;
      if (++frames > 30 && fps < fpsMin) fpsMin = fps; // ignore the boot stumble
    }
  }
  fpsAt = t;

  // The pad, full-bleed. It gets every pixel — nothing of ours sits on top of it
  // except a name that fades and a number nobody has to read.
  if (host && !hostFailed) {
    try {
      host.paint?.(api);
    } catch (e) {
      hostFailed = true;
      error = String(e?.message || e);
    }
  }
  if (hostFailed || !host) {
    wipe(18, 6, 10);
    ink(...NO).write("didn't run", { center: "x", y: h / 2 - 10 });
    ink(110).write(String(error).slice(0, 44), { center: "x", y: h / 2 + 2 });
  }

  chrome(api, w, h);
}

// Everything of ours. It should feel like almost nothing.
function chrome({ ink, write, box, screen }, w, h) {
  const c = current();
  const age = now() - shownAt;

  // The name — the one thing worth saying out loud, and only for a moment. A
  // label that never leaves stops being read.
  if (age < NAME_MS) {
    const a = Math.min(1, Math.max(0, (NAME_MS - age) / 700));
    const label = c.name;
    ink(0, 0, 0, 150 * a).write(label, { x: 7, y: 8 });
    ink(240, 240, 255, 255 * a).write(label, { x: 6, y: 7 });
    const n = `${at + 1}/${deck.length}`;
    ink(120, 120, 140, 200 * a).write(n, { x: w - 6 - n.length * 6, y: 7 });
  }

  // The last verdict, as a breath of color at the edge you swiped from. You
  // never pressed anything, so this is the only acknowledgement there is.
  if (flash > 0) {
    const [r, g, b] = flashOk ? OK : NO;
    ink(r, g, b, 120 * flash).box(0, h - 3, w, 3);
  }

  // The swipe hint, only while you're touching an edge — furniture the rest of
  // the time would just be in the pad's way.
  if (drag) {
    const up = drag.from === "bottom";
    ink(255, 255, 255, 60).box(0, up ? h - 2 : 0, w, 2);
  }

  // Verdicts the server refused. This is here because the first version of this
  // piece lost an entire session in total silence, and I'm not doing that again.
  if (lost > 0) {
    const m = `${lost} unsent`;
    ink(0, 0, 0, 160).write(m, { x: w - 5 - m.length * 6, y: h - 11 });
    ink(...NO).write(m, { x: w - 6 - m.length * 6, y: h - 12 });
  }
}

function act(api) {
  const { event: e } = api;
  if (state !== "judging") return;

  const h = api.screen.height;

  // A gesture that starts in an edge band is ours; everything else is the pad's.
  // The bands are thin on purpose — the instrument keeps its whole face.
  if (e.is("touch")) {
    if (e.y >= h - EDGE) drag = { from: "bottom", y0: e.y };
    else if (e.y <= EDGE) drag = { from: "top", y0: e.y };
    else drag = null;
  }

  if (drag && e.is("lift")) {
    const moved = drag.y0 - e.y; // positive = swiped up
    const d = drag.from;
    drag = null;
    if (d === "bottom" && moved > SWIPE) return turn(api, +1); // up → next
    if (d === "top" && -moved > SWIPE) return turn(api, -1); // down → back
    return; // a tap on the edge is not a verdict. Say nothing.
  }

  // Keys, for judging a long deck without leaving home.
  if (e.is("keyboard:down:arrowdown") || e.is("keyboard:down:space"))
    return turn(api, +1);
  if (e.is("keyboard:down:arrowup")) return turn(api, -1);

  // Everything else belongs to the instrument. Play it — that's the whole vote.
  if (!drag && host && !hostFailed) {
    if (e.is("touch")) taps += 1;
    try {
      host.act?.(api);
    } catch (err) {
      hostFailed = true;
      error = String(err?.message || err);
    }
  }
}

// The last pad you were on still deserves its verdict — leaving the piece is
// leaving it.
function leave(api) {
  if (state === "judging") leaving(api);
}

export { boot, sim, paint, act, leave };
