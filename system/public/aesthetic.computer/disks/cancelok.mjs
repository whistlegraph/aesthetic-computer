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

// The slide. Two pads can never run at once — lib/pads.mjs is a session
// singleton, so booting the next one kills the last one dead. So we FREEZE the
// outgoing pad: copy its final frame into a buffer and slide the corpse out while
// the live one comes in underneath it. The card that leaves is a photograph; the
// card that arrives is playing.
let mounting = false; // the next pad is in flight — not broken, just not here yet
let stage = null; // the live pad's own surface

// THE PEEK. You should see the next pad while you drag, not a black hole — but two
// pads can't run at once (lib/pads.mjs is a session singleton: booting B kills A).
// So we boot the next pad ONCE, steal a single frame, and boot the current one
// back. The card under your finger is a photograph of a real instrument, taken a
// moment ago. Not live — but live is a lie a singleton can't tell.
let peek = null; // one frame of the pad below
let peekOf = null; // which code that frame belongs to
let peeking = false;
const mods = new Map(); // imported modules, kept — a page turn shouldn't re-fetch
let lisp = null; // a $code candidate's source, straight from the database
let out = null; // the frozen frame of the pad you just left
let outName = ""; // its name travels with it — the label belongs to the card
let anim = 0; // 0 = settled, →1 = mid-slide
let pending = 0; // a turn waiting on its pad to arrive: +1 next, -1 back
let dir = 1; // +1 = the next pad rises, -1 = the last one falls back in
let held = 0; // how far your finger has dragged the pad, right now

const EASE = (t) => 1 - Math.pow(1 - t, 3); // out-cubic: fast away, soft landing
const SLIDE = 0.075; // ~4 frames of ramp; a page turn should feel thrown, not eased

const now = () => Date.now();
const current = () => deck[at];

async function load(api) {
  const asked = (api.params?.[0] || "").replace(/^\^/, "").toLowerCase();
  const res = await fetch("/aesthetic.computer/bags.json");
  if (!res.ok) throw new Error("bags.json HTTP " + res.status);
  const bag = (await res.json()).bags?.[asked || "pads"];
  if (!bag) throw new Error("no bag named ^" + asked);
  // A candidate is either a compiled piece (a .mjs module we import) or a KidLisp
  // $code (source we fetch from the database and hand to the evaluator). The
  // second kind never touched the repo and was never deployed — which is the whole
  // point, and the reason the machine is allowed to publish those on its own.
  deck = (bag.items || [])
    .filter((it) => it.type === "piece" || it.type === "kidlisp")
    .map((it) => ({
      code: it.code,
      name: it.name || it.code,
      lisp: it.type === "kidlisp",
    }));
  if (!deck.length) throw new Error("that bag holds nothing to judge");
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
  mounting = true; // the next pad is still coming over the wire
  taps = 0;
  shownAt = now();
  frames = 0;
  fpsAt = 0;
  fps = 0;
  fpsMin = 999;
  const c = current();
  const { code } = c;
  seen[code] = (seen[code] || 0) + 1;
  lisp = null;
  try {
    if (c.lisp) {
      // A KidLisp pad lives in the database, not the repo. Nothing was committed
      // and nothing was deployed to put it here.
      const res = await fetch(`/api/store-kidlisp?code=${encodeURIComponent(code)}`);
      if (!res.ok) throw new Error("no $" + code);
      const data = await res.json();
      lisp = data.source || data.code || data.kidlisp;
      if (!lisp) throw new Error("$" + code + " has no source");
      host = { lisp: true };
    } else {
      // Origin-qualified: a disk runs as a blob module, so it has no base URL to
      // resolve `./x.mjs` against, and updateCode only rewrites static imports.
      // Cached, because the peek already fetched it — a page turn should not go
      // back to the network for something it just looked at.
      let mod = mods.get(code);
      if (!mod) {
        mod = await import(`${ORIGIN}/aesthetic.computer/disks/${code}.mjs`);
        mods.set(code, mod);
      }
      if (stage) api.page(stage); // it may draw at boot — into ITS surface, not ours
      mod.boot?.({ ...api, screen: stage || api.screen });
      api.page(api.screen);
      host = mod;
    }
  } catch (e) {
    hostFailed = true;
    error = String(e?.message || e);
  } finally {
    mounting = false;
  }
}

// Steal one frame of the pad below, so the gap you drag open has something in it.
// This is the only honest way to preview a pad without a second engine: boot it,
// paint once, snapshot, and put the current pad back exactly as it was. It costs
// one extra boot, and it happens once, while you're already looking at something
// else.
async function takePeek(api) {
  if (peeking || !host || host.lisp || hostFailed) return;
  const next = deck[(at + 1) % deck.length];
  if (!next || next.code === peekOf || next.lisp) return;
  peeking = true;
  try {
    let mod = mods.get(next.code);
    if (!mod) {
      mod = await import(`${ORIGIN}/aesthetic.computer/disks/${next.code}.mjs`);
      mods.set(next.code, mod);
    }
    const w = api.screen.width;
    const h = api.screen.height;
    const shot = api.painting(w, h, (p) => p.wipe(0, 0, 0));

    api.page(shot);
    mod.boot?.({ ...api, screen: shot }); // this CLOBBERS the engine's config…
    mod.sim?.({ ...api, screen: shot });
    mod.paint?.({ ...api, screen: shot });
    api.page(api.screen);

    peek = shot;
    peekOf = next.code;

    // …so put the current pad back. Its boot re-asserts its own config, which is
    // the only reason any of this is survivable.
    api.page(stage);
    host.boot?.({ ...api, screen: stage });
    api.page(api.screen);
  } catch {
    peek = null; // a pad that won't preview is not an emergency.
    peekOf = null;
  } finally {
    peeking = false;
  }
}

// Turn the page. Going BACK is not a neutral act — you returned to something, and
// coming back is the strongest keep signal in the whole system.
function turn(api, d) {
  if (anim > 0 || pending) return; // one page at a time
  leaving(api);

  // Freeze it. This copy is the only thing that survives the next boot, so it has
  // to happen before mount() — a pad, once replaced, cannot be asked to draw
  // itself again.
  if (stage) {
    out = api.painting(stage.width, stage.height, () => {});
    out.pixels.set(stage.pixels);
    outName = current().name;
  }

  // DON'T slide yet. mount() is async — the next pad is still coming over the
  // wire, and sliding now would raise an empty buffer into view. That's the black
  // flash. We hold the frozen frame instead, and the slide begins the moment the
  // new pad has actually painted something. A page turn should reveal a pad, not
  // a hole where one is about to be.
  pending = d;
  held = 0;
  peek = null; // the pad below is about to become the pad you're on
  peekOf = null;
  at = (at + d + deck.length) % deck.length;
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
  if (state === "judging" && host && !hostFailed && !host.lisp) {
    try {
      host.sim?.(api);
    } catch (e) {
      hostFailed = true;
      error = String(e?.message || e);
    }
  }
  if (flash > 0) flash -= 0.04;
  if (anim > 0) {
    anim += SLIDE;
    if (anim >= 1) {
      anim = 0;
      out = null; // let the photograph go
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

  // The live pad paints into its OWN surface — not the screen. That's what makes
  // the slide possible at all, and it's also what keeps per-pixel pads honest:
  // they write straight into `screen.pixels`, so the surface we hand them has to
  // BE their screen, not a copy of ours with a different shape.
  if (!stage || stage.width !== w || stage.height !== h)
    stage = api.painting(w, h, (p) => p.wipe(0, 0, 0));

  let painted = false;
  if (host && !hostFailed) {
    try {
      api.page(stage);
      // Two kinds of candidate, one surface. A module gets its lifecycle driven;
      // a $code gets handed to the evaluator. Neither knows the other exists.
      if (host.lisp) api.kidlisp(0, 0, w, h, lisp);
      else host.paint?.({ ...api, screen: stage });
      painted = true;
    } catch (e) {
      hostFailed = true;
      error = String(e?.message || e);
    } finally {
      api.page(screen);
    }
  }

  // The turn has been waiting for this: the new pad drew a frame. NOW slide. If it
  // failed to load, slide anyway — a broken pad still deserves its three seconds
  // and its verdict, and freezing forever would be worse than showing the wound.
  if (pending && (painted || hostFailed)) {
    dir = pending;
    anim = 0.0001;
    pending = 0;
  }

  // Still waiting? Hold the frozen frame. This is the whole cure for the black
  // flash: you keep looking at the pad you had until there's a pad to replace it.
  if (pending && out) {
    wipe(0, 0, 0);
    api.paste(out, 0, 0);
    label(api, outName, w, 0, 1);
    return;
  }

  wipe(0, 0, 0);

  // "didn't run" means it FAILED — not that it hasn't arrived yet. A pad takes a
  // moment to come over the wire, and during a slide that gap is exactly where the
  // eye is looking. Claiming a crash for 100ms on every single page turn is a lie
  // told at the worst possible moment.
  if (hostFailed) {
    wipe(18, 6, 10);
    ink(...NO).write("didn't run", { center: "x", y: h / 2 - 10 });
    ink(110).write(String(error).slice(0, 44), { center: "x", y: h / 2 + 2 });
    chrome(api, w, h, 0);
    return;
  }

  // Where the two cards sit. Mid-slide the outgoing one is a photograph and the
  // incoming one is playing; the rest of the time `held` is just your finger.
  let liveY = held;
  if (anim > 0) {
    const e = EASE(Math.min(1, anim));
    liveY = dir > 0 ? h * (1 - e) : -h * (1 - e); // next rises / last falls in
    const outY = dir > 0 ? -h * e : h * e;
    if (out) {
      api.paste(out, 0, Math.round(outY));
      label(api, outName, w, Math.round(outY), 1); // the name leaves with its card
    }
  }

  // The card under your finger. Only while dragging — during the slide the real
  // pad is already live, and a photograph would only be a worse version of it.
  if (!anim && held < 0 && peek) {
    api.paste(peek, 0, Math.round(h + held));
    label(api, deck[(at + 1) % deck.length].name, w, Math.round(h + held), 0.7);
  }

  api.paste(stage, 0, Math.round(liveY));
  chrome(api, w, h, Math.round(liveY));

  // Take the next pad's picture while you're busy with this one. Never mid-gesture
  // and never mid-slide — the boot it costs would show.
  if (!anim && !pending && !held && !mounting) takePeek(api);
}

// The name rides WITH its card. It isn't chrome bolted to the corner of the
// screen — it belongs to the pad, so it slides out when the pad does and arrives
// with the next one. Anything else and the label would sit still while the thing
// it names flies away.
function label({ ink }, text, w, y, a) {
  if (a <= 0 || !text) return;
  ink(0, 0, 0, 150 * a).write(text, { x: 7, y: y + 8 });
  ink(240, 240, 255, 255 * a).write(text, { x: 6, y: y + 7 });
}

// Everything of ours. It should feel like almost nothing.
function chrome(api, w, h, y = 0) {
  const { ink, write, box } = api;
  const c = current();
  const age = now() - shownAt;

  // The name — the one thing worth saying out loud, and only for a moment. A
  // label that never leaves stops being read. It stays lit while the card is
  // still moving, because a name you can't finish reading isn't a name.
  const fade = Math.min(1, Math.max(0, (NAME_MS - age) / 700));
  const a = anim > 0 ? 1 : fade;
  label(api, c.name, w, y, a);
  if (a > 0) {
    const n = `${at + 1}/${deck.length}`;
    ink(120, 120, 140, 200 * a).write(n, { x: w - 6 - n.length * 6, y: y + 7 });
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
    if (anim > 0) return; // mid-slide, the deck isn't listening
    if (e.y >= h - EDGE) drag = { from: "bottom", y0: e.y };
    else if (e.y <= EDGE) drag = { from: "top", y0: e.y };
    else drag = null;
  }

  // The card follows your finger. Rubber-banded, so pulling the wrong way barely
  // moves — the deck should feel like it wants to go one direction.
  if (drag && e.is("draw")) {
    const moved = drag.y0 - e.y; // + = pulled up, − = pulled down
    const right = drag.from === "bottom" ? moved > 0 : moved < 0;
    held = right ? -moved : -moved * 0.2; // wrong way barely budges
    return;
  }

  if (drag && e.is("lift")) {
    const moved = drag.y0 - e.y; // positive = swiped up
    const d = drag.from;
    drag = null;
    held = 0; // whichever way it goes, it stops following the finger now
    if (d === "bottom" && moved > SWIPE) return turn(api, +1); // up → next
    if (d === "top" && -moved > SWIPE) return turn(api, -1); // down → back
    return; // not far enough. It snaps back, and nothing is recorded.
  }

  // Keys, for judging a long deck without leaving home. Down goes forward, the
  // way a page does.
  if (e.is("keyboard:down:arrowdown") || e.is("keyboard:down:space"))
    return turn(api, +1);
  if (e.is("keyboard:down:arrowup")) return turn(api, -1);

  // Everything else belongs to the instrument. Play it — that's the whole vote.
  if (!drag && host && !hostFailed) {
    if (e.is("touch")) taps += 1; // a tap on a $code still counts — the evaluator sees the pen itself
    try {
      if (!host.lisp) host.act?.(api);
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
