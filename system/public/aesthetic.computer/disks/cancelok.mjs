// cancelok, 26.07.14
// The judging surface. A candidate runs for real — full-bleed, playable, not a
// screenshot — and you answer it with one of two buttons: `not` or `ok`. That's
// the whole instrument. Everything downstream (what gets kept, what the next
// generation is told to make) is built out of those two signals plus what your
// hands did while you watched.
//
// The name is the two buttons, the way nopaint's was: "Press Paint if you like
// what you see or No if you don't." Same gesture, one medium over — the judgment
// happens AFTER the thing exists, and it's one bit wide.
//
// A candidate is HOSTED, not linked. We import the pad module and drive its
// lifecycle ourselves (boot → sim/paint/act), then draw the verdict bar on top.
// That works because lib/pads.mjs is a session singleton whose config is
// re-asserted by each pad's own boot — so hosting a pad is just calling it.
// The upshot: you can still PLAY the pad with the same taps you'd use anywhere
// else, and the verdict bar is the only screen real estate we take from it.

const BAR_H = 46; // the verdict bar — the only pixels the pad doesn't get
const CANCEL = [255, 70, 80];
const OK = [90, 255, 150];

// The disk worker's real origin (port included — dev lives on one).
const ORIGIN =
  typeof location !== "undefined" && location.origin && location.origin !== "null"
    ? location.origin
    : "https://aesthetic.computer";

// Where verdicts go and where the queue comes from. On a laptop that's the
// loop's own sink (a file); in production it's the api, and the verdict is
// signed so it carries your handle.
const LOCAL = /localhost|127\.0\.0\.1/.test(ORIGIN);
const SINK = LOCAL ? "http://localhost:8901" : `${ORIGIN}/api/cancelok`;

let state = "loading"; // loading | judging | done | error
let error = "";

let deck = []; // [{ code, name }] — the candidates, in order
let at = 0; // which one we're on
let host = null; // the live candidate module
let hostFailed = false; // it threw on load or on frame — a machine-gate fail

let verdicts = []; // [{ code, verdict, dwellMs, taps, failed, at }]
let shownAt = 0; // when this candidate came up (for dwell)
let taps = 0; // how many times you played it — the usage signal
let judge = null; // the handle whose taste this is (null = anonymous)
let replaying = false; // you've judged them all — this is a second pass
let armed = null; // "cancel" | "ok" — a press waiting on its lift
let fromBar = false; // did this gesture start in the bar? (route the whole drag)

const now = () => Date.now();
const current = () => deck[at];

// Deal a deck. The loop's queue comes first — those are the candidates nobody
// has seen yet, and they're the whole reason to be here. A bag is the fallback,
// so `cancelok` still works with no loop running (and `cancelok:^pads` judges
// the 47 that already shipped).
async function load(api) {
  const asked = (api.params?.[0] || "").replace(/^\^/, "").toLowerCase();

  if (!asked && LOCAL) {
    const q = await fetch(`${SINK}/queue`)
      .then((r) => (r.ok ? r.json() : null))
      .catch(() => null); // no sink running is a normal state, not an error
    const items = (typeof q === "string" ? JSON.parse(q) : q)?.items || [];
    if (items.length) {
      deck = items.map((i) => ({ code: i.code, name: i.trait || i.name || i.code }));
      return;
    }
  }

  const bagName = asked || "pads";
  const res = await fetch("/aesthetic.computer/bags.json");
  if (!res.ok) throw new Error("bags.json HTTP " + res.status);
  const bag = (await res.json()).bags?.[bagName];
  if (!bag) throw new Error("no bag named ^" + bagName);
  deck = (bag.items || [])
    .filter((it) => it.type === "piece")
    .map((it) => ({ code: it.code, name: it.name || it.code }));
  if (!deck.length) throw new Error("^" + bagName + " holds no pieces");

  // Don't ask a question you already have the answer to. The deck is what YOU
  // haven't judged — newest first, since the freshly-made ones are the point.
  // Once you've judged everything, we deal the whole thing again rather than
  // leaving you with an empty screen; a second pass is allowed to change its mind.
  // Signed, because "what have I judged" is a question only you can ask about
  // yourself. userRequest throws when logged out, and a logged-out visitor has
  // judged nothing — so the throw IS the answer.
  if (!LOCAL) {
    const mine = await api.net
      ?.userRequest?.("GET", "/api/cancelok/mine")
      .catch(() => null);
    const seen = new Set(mine?.judged || []);
    if (seen.size) {
      const fresh = deck.filter((d) => !seen.has(d.code));
      if (fresh.length) deck = fresh;
      else replaying = true; // judged them all — deal again, you may differ.
    }
  }
  deck.reverse(); // the newest pads sit at the end of the bag — show them first.
}

// Bring a candidate up. A candidate that won't even load is not a crash — it's
// a verdict the machine already made for you, so we show it as failed and let
// you confirm with the same tap.
async function mount(api) {
  host = null;
  hostFailed = false;
  taps = 0;
  shownAt = now();
  const { code } = current();
  try {
    // A fully-qualified URL, because AC runs a disk as a BLOB module: it has no
    // base to resolve `./x.mjs` or even `/x.mjs` against, and updateCode only
    // rewrites static imports. Fetched as a real URL, the pad's own
    // `../lib/pads.mjs` resolves on its own.
    const mod = await import(`${ORIGIN}/aesthetic.computer/disks/${code}.mjs`);
    mod.boot?.(api);
    host = mod;
  } catch (e) {
    hostFailed = true;
    error = String(e?.message || e);
  }
}

function record(api, verdict) {
  const c = current();
  const v = {
    code: c.code,
    verdict,
    dwellMs: now() - shownAt,
    taps,
    failed: hostFailed,
    at: now(),
  };
  verdicts.push(v);

  // Fire and forget, always. A verdict is worth more than its confirmation, and
  // nothing about judging should ever wait on a socket — the next pad is already
  // playing.
  if (LOCAL) {
    fetch(`${SINK}/verdict`, {
      method: "POST",
      headers: { "content-type": "application/json" },
      body: JSON.stringify(v),
    }).catch(() => {});
  } else {
    // Signed, so the verdict has a name on it. A logged-out visitor still counts
    // — userRequest throws when there's no token, and an anonymous tap is real
    // usage, so we fall back rather than drop it.
    api.net
      ?.userRequest?.("POST", "/api/cancelok", v)
      .catch(() =>
        fetch("/api/cancelok", {
          method: "POST",
          headers: { "content-type": "application/json" },
          body: JSON.stringify(v),
        }).catch(() => {}),
      );
  }
}

function advance(api, verdict) {
  record(api, verdict);
  at += 1;
  if (at >= deck.length) {
    state = "done";
    save(api);
    return;
  }
  mount(api);
}

function save({ store }) {
  store["cancelok:verdicts"] = verdicts;
  store.persist?.("cancelok:verdicts", "local:db");
}

function boot(api) {
  api.wipe(6, 6, 10);
  api.hud?.label?.("cancelok");
  // Your handle goes on every verdict you cast, so it belongs on screen. An
  // unhandled visitor still judges — their taps are real — they just do it
  // anonymously, and they should know that before they start.
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
      hostFailed = true; // it ran, then died. still a verdict.
      error = String(e?.message || e);
    }
  }
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
    ink(...CANCEL).write(error, { center: "x", y: h / 2 - 4 });
    return;
  }

  if (state === "done") return summary(api);

  // The candidate paints itself, full-bleed. If it can't, we say so plainly —
  // a broken candidate still deserves its three seconds and its verdict.
  if (host && !hostFailed) {
    try {
      host.paint?.(api);
    } catch (e) {
      hostFailed = true;
      error = String(e?.message || e);
    }
  }
  if (hostFailed || !host) {
    wipe(20, 6, 10);
    ink(...CANCEL).write("didn't run", { center: "x", y: h / 2 - 10 });
    ink(120).write(error.slice(0, 48), { center: "x", y: h / 2 + 2 });
  }

  bar(api, w, h);
}

// The verdict bar: two halves, the whole width. Cancel is left because that's
// where the thumb rests and the reflex is to keep going, not to keep the thing.
function bar({ ink, write, box }, w, h) {
  const y = h - BAR_H;
  const half = w / 2;
  const c = current();

  ink(0, 0, 0, 170).box(0, y, w, BAR_H);
  ink(40, 40, 50).box(0, y, w, 1);
  ink(40, 40, 50).box(half, y, 1, BAR_H);

  const lit = (side) => (armed === side ? 255 : 90);
  ink(...CANCEL, lit("cancel")).box(0, y + 1, half, BAR_H - 1);
  ink(...OK, lit("ok")).box(half + 1, y + 1, half - 1, BAR_H - 1);

  // Each word centered in its own half. The halves are equal even though the
  // words aren't — the two answers carry the same weight.
  const mid = y + BAR_H / 2 - 4;
  const mark = (word, x) =>
    ink(armed === word ? 0 : 255).write(word, { x: x - word.length * 3, y: mid });
  mark("cancel", half / 2);
  mark("ok", half + half / 2);

  // Progress + name, top-left, shadowed so it survives whatever the pad paints.
  const tag = `${at + 1}/${deck.length}  ${c.name}`;
  ink(0, 0, 0, 180).write(tag, { x: 7, y: 7 });
  ink(230).write(tag, { x: 6, y: 6 });

  // Whose taste this is. Anonymous is a real answer, so say it rather than
  // leaving the corner empty and the question open.
  const who = judge || "anon";
  ink(0, 0, 0, 180).write(who, { x: 7, y: y - 11 });
  ink(judge ? [120, 220, 255] : [110, 110, 120]).write(who, { x: 6, y: y - 12 });

  // The usage signal, shown back to you — taps are the thing you did without
  // being asked, and they're worth as much as the verdict.
  if (taps > 0) {
    const t = `${taps} ▸`;
    ink(0, 0, 0, 180).write(t, { x: w - 6 - t.length * 6 + 1, y: 7 });
    ink(255, 220, 120).write(t, { x: w - 6 - t.length * 6, y: 6 });
  }
}

function summary({ wipe, ink, write, screen }) {
  wipe(6, 6, 10);
  const kept = verdicts.filter((v) => v.verdict === "ok");
  const w = screen.width;

  ink(230).write(`${kept.length} ok · ${verdicts.length - kept.length} not`, {
    x: 8,
    y: 10,
  });
  ink(90).write("saved — tap to judge again", { x: 8, y: 24 });

  let y = 46;
  for (const v of verdicts) {
    if (y > screen.height - 12) break;
    const on = v.verdict === "ok";
    ink(...(on ? OK : [90, 70, 80])).write(on ? "ok" : "cancel", { x: 8, y });
    ink(on ? 230 : 110).write(v.code, { x: 62, y });
    const meta = `${Math.round(v.dwellMs / 100) / 10}s${v.taps ? " · " + v.taps + "▸" : ""}`;
    ink(70).write(meta, { x: w - 8 - meta.length * 6, y });
    y += 12;
  }
}

function act(api) {
  const { event: e } = api;
  if (state === "error") return;

  if (state === "done") {
    if (e.is("lift")) {
      at = 0;
      verdicts = [];
      state = "judging";
      mount(api);
    }
    return;
  }
  if (state !== "judging") return;

  const h = api.screen.height;
  const w = api.screen.width;
  const inBar = (y) => y >= h - BAR_H;

  // Route by where the gesture STARTED, so a drag out of the bar doesn't play
  // the pad and a drag out of the pad doesn't cast a verdict.
  if (e.is("touch")) {
    fromBar = inBar(e.y);
    if (fromBar) armed = e.x < w / 2 ? "cancel" : "ok";
  }

  if (e.is("lift") && fromBar) {
    const still = armed && inBar(e.y) && (e.x < w / 2) === (armed === "cancel");
    const verdict = armed;
    armed = null;
    fromBar = false;
    if (still) return advance(api, verdict);
    return;
  }

  // Keys — the same two signals, for judging a long deck without leaving home.
  if (e.is("keyboard:down:arrowleft") || e.is("keyboard:down:escape"))
    return advance(api, "cancel");
  if (e.is("keyboard:down:arrowright") || e.is("keyboard:down:enter"))
    return advance(api, "ok");

  // Everything else is the candidate's — it's an instrument, so play it.
  if (!fromBar && host && !hostFailed) {
    if (e.is("touch")) taps += 1;
    try {
      host.act?.(api);
    } catch (err) {
      hostFailed = true;
      error = String(err?.message || err);
    }
  }
}

export { boot, sim, paint, act };
