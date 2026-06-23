// Profile, 2026.03.26.00.00.00
// @handle profile — activity + portfolio hybrid.

const FETCHING = "Fetching";
const REFRESH_MS = 30000;
const RECONNECT_MS = 3000;
const MOOD_LIMIT = 40;
const CHAT_LIMIT = 120;
const THUMB_MAX = 7; // hero + up to 6 in the filmstrip
const ACTIVITY_MAX = 30;
const SMALL_FONT = "MatrixChunky8";
const EDGE_MARGIN = 6; // bottom-edge breathing room for the Paintings button

const { max, min, floor, round } = Math;

// Activity hues (kept for the ribbon chips) — these are AC's own type colors.
const TYPE_COLOR = {
  painting: [100, 200, 255],
  kidlisp: [120, 255, 120],
  mood: [180, 120, 255],
  clock: [255, 200, 80],
  chat: [200, 200, 200],
};

let debug;
let visiting;
let profile;
let noprofile = FETCHING;
let noprofileAction;
let noprofileBtn;
let paintingsBtn;
let ellipsisTicker;

let loading = false;
let refreshing = false;
let dataError = null;

let scorecard = makeEmptyScorecard();
let presence = makeOfflinePresence();

// Painting thumbnails.
let thumbs = []; // [{ slug, code, img, when, btn, route }]
let thumbsLoading = false;
let getApi = null; // stored from boot for async image loading
let hudApi = null; // stored from boot so we can recolor the handle label

// Handle identity palette (@sat's "handle as a designed object").
let handleColors = null; // [{ r, g, b }] per character (incl. @), or null
let handleLabel = null; // colorized handle string for the HUD label
let dom = { r: 150, g: 162, b: 210 }; // dominant handle hue
let bgTint = [12, 13, 22]; // ambient background, faintly the handle's color
let colorsLoaded = false;

// Tap regions, rebuilt each frame.
let hitRegions = []; // [{ box, route }]

// Responsive geometry, recomputed when the frame size changes.
let layout = null;
let lastW = 0;
let lastH = 0;

let statusSocket = null;
let reconnectTimer = null;
let refreshTimer = null;
let disposed = false;

// Activity button regions (rebuilt each frame).
let activityBtns = [];

// Scroll state for activity feed.
let scrollOffset = 0;
let maxScroll = 0;

function makeEmptyScorecard() {
  return {
    counts: {
      paintings: 0,
      pieces: 0,
      kidlisp: 0,
      clocks: 0,
      tapes: 0,
      moods: 0,
      chats: 0,
    },
    recentMedia: {
      paintings: [],
      kidlisp: [],
      clocks: [],
      moods: [],
      chats: [],
    },
    activity: [],
    updatedAt: null,
  };
}

function makeOfflinePresence() {
  return {
    online: false,
    currentPiece: null,
    worldPiece: null,
    showing: null,
    connections: 0,
    ping: null,
    lastSeenAt: null,
  };
}

function meta({ piece }) {
  return {
    title: `${piece} - aesthetic.computer`,
    desc: `Profile for ${piece}.`,
  };
}

async function boot({
  params,
  user,
  gizmo,
  handle,
  hud,
  net,
  get,
  debug: d,
}) {
  disposed = false;
  debug = d;
  getApi = get;
  hudApi = hud;

  const hand = normalizeHandle(handle());
  visiting = normalizeHandle(params[0] || hand);

  ellipsisTicker = new gizmo.EllipsisTicker();

  resetUiState();
  clearTimersAndSocket();
  scorecard = makeEmptyScorecard();
  presence = makeOfflinePresence();
  profile = null;
  dataError = null;
  thumbs = [];
  thumbsLoading = false;
  scrollOffset = 0;
  layout = null;
  lastW = 0;
  lastH = 0;
  handleColors = null;
  handleLabel = null;
  colorsLoaded = false;

  if (visiting) {
    // Seed the palette from the deterministic hash so the very first frame
    // already wears a color; the stored per-character colors stream in after.
    computeHandlePalette();
    hud.label(handleLabel || visiting);
    net.rewrite(visiting);
    noprofile = FETCHING;
    noprofileAction = null;

    loadHandleColors(); // fire and forget — recolors the label when it lands
    refreshProfile(true);
    startRefreshLoop();
    connectProfileStream();
    return;
  }

  if (user) {
    if (user.email_verified) {
      noprofile = "Create handle.";
      noprofileAction = "handle";
    } else {
      noprofile = "Check email to verify.";
      noprofileAction = "email";
    }
  } else {
    noprofile = "Log in or Sign up";
    noprofileAction = "imnew";
  }
}

function paint({ api, wipe, help, ink, screen, ui, pen, paste, text }) {
  // Ambient background — faintly the handle's color instead of flat grey.
  if (!pen?.drawing) wipe(bgTint[0], bgTint[1], bgTint[2]);

  if (!visiting) {
    paintNoProfileState({ api, help, ink, screen, ui });
    return;
  }

  // Loading state.
  if (!profile && noprofile === FETCHING) {
    ink(dom.r, dom.g, dom.b).write(
      `${FETCHING}${ellipsisTicker.text(help.repeat)}`,
      { center: "xy" },
    );
    return;
  }

  // Error / no-profile state.
  if (!profile && noprofile && noprofile !== FETCHING) {
    ink(220).write(noprofile, { center: "xy" });
    return;
  }

  // Recompute geometry only when the frame size changes (notepat pattern).
  if (!layout || screen.width !== lastW || screen.height !== lastH) {
    layout = computeLayout(screen);
    lastW = screen.width;
    lastH = screen.height;
  }

  hitRegions = [];

  const L = layout;

  // Header band: mood + presence (handle lives in the colored HUD label).
  paintHeader({ ink, text, x: L.header.x, y: L.header.y, w: L.header.w });

  // Hero painting — the largest, most-recent work.
  paintHero({ ink, paste, text, region: L.hero });

  // Secondary surfaces.
  if (L.filmstrip) paintFilmstrip({ ink, paste, region: L.filmstrip });
  if (L.grid) paintGrid({ ink, paste, region: L.grid });
  if (L.ribbon) paintRibbon({ ink, text, region: L.ribbon });

  // Bottom bar — sit the button a margin up from the edge (AC convention),
  // not flush to the bottom.
  paintingsBtn ||= new ui.TextButton("Paintings", {
    x: L.M,
    bottom: EDGE_MARGIN,
    screen,
  });
  paintingsBtn.paint(api);

  if (refreshing) {
    ink(dom.r, dom.g, dom.b, 180).write(
      `sync${ellipsisTicker.text(help.repeat)}`,
      { x: screen.width - 40, y: screen.height - EDGE_MARGIN - 8 },
      undefined,
      36,
      false,
      SMALL_FONT,
    );
  }
}

function act({ event: e, jump, store, user }) {
  paintingsBtn?.act(e, () => {
    if (visiting) jump(`paintings~${visiting}`);
  });

  if (e.is("keyboard:down:g") && visiting) jump(`paintings~${visiting}`);
  if (e.is("keyboard:down:r")) refreshProfile(true);

  // Tap regions (hero, filmstrip, grid, ribbon chips).
  if (e.is("lift")) {
    const px = e.x ?? e.pen?.x;
    const py = e.y ?? e.pen?.y;
    for (const r of hitRegions) {
      const b = r.box;
      if (
        r.route &&
        px >= b.x &&
        px <= b.x + b.w &&
        py >= b.y &&
        py <= b.y + b.h
      ) {
        jump(r.route);
        break;
      }
    }
  }

  // No-profile button.
  noprofileBtn?.act(e, {
    push: () => {
      let slug;
      if (noprofileAction === "handle") {
        slug = "prompt~handle";
      } else if (noprofileAction === "email") {
        slug = "prompt~email~" + user?.email;
      } else if (noprofileAction === "imnew") {
        slug = "prompt";
        store["prompt:splash"] = true;
      }
      if (slug) jump(slug);
    },
  });

  if (e.is("reframed")) {
    noprofileBtn = null;
    paintingsBtn = null;
    layout = null; // force recompute on next paint
    hitRegions = [];
  }
}

function sim() {
  ellipsisTicker?.sim();
}

function leave() {
  disposed = true;
  clearTimersAndSocket();
}

// --- Responsive layout ---
//
// Instead of a single portrait/landscape fork with fixed offsets, we build one
// geometry object keyed to the frame and step through size tiers (notepat
// pattern). The hero is always present and scales; secondary surfaces drop
// gracefully as space tightens, so nothing overlaps.
function computeLayout(screen) {
  const M = 6;
  const HUD_H = 20;
  // Reserve the button (~19px tall) + its bottom margin, plus a little gap so
  // content/ribbon never crowd it.
  const BTN_H = 19 + EDGE_MARGIN + 6;
  const top = HUD_H + 2;
  const bottom = screen.height - BTN_H - 2;
  const W = screen.width;
  const headerH = 22; // mood + presence

  // Note: screen.* is AC's logical resolution (~half the CSS viewport), so all
  // thresholds below are in those logical pixels — a phone is ~211 wide here.

  // Wide frames: hero on the left, a recent-works grid on the right.
  const wide = W >= 420 && W > screen.height * 1.1;

  if (wide) {
    const gap = 10;
    const leftW = floor(W * 0.58) - M;
    const heroY = top + headerH;
    const rightX = M + leftW + gap;
    const rightW = W - rightX - M;
    return {
      mode: "wide",
      M,
      HUD_H,
      BTN_H,
      header: { x: M, y: top, w: leftW },
      hero: { x: M, y: heroY, w: leftW, h: bottom - heroY },
      grid: { x: rightX, y: top, w: rightW, h: bottom - top },
    };
  }

  // Single column — "just pictures": one big hero with the catalog ribbon
  // pinned above the bottom bar. The hero is capped to a near-square so a
  // wide painting doesn't leave tall empty bands; any space that frees up on a
  // tall phone becomes a short filmstrip of recent works. (The full gallery
  // grid lives in wide mode and behind the Paintings button.)
  const colW = W - M * 2;
  const heroY = top + headerH;
  const ribbonH = 11;
  const ribbonY = bottom - ribbonH;
  const avail = ribbonY - 4 - heroY; // vertical room for hero (+ optional film)

  // Show a filmstrip only when the hero would still be at least ~square after
  // reserving it; otherwise the hero fills the whole column.
  const filmH = clamp(floor(colW * 0.18), 38, 64);
  const gap = 6;
  const showFilm = colW >= 150 && avail - filmH - gap >= colW * 0.85;
  const heroH = showFilm ? avail - filmH - gap : avail;

  const layout = {
    mode: "column",
    M,
    HUD_H,
    BTN_H,
    header: { x: M, y: top, w: colW },
    hero: { x: M, y: heroY, w: colW, h: heroH },
    ribbon: { x: M, y: ribbonY, w: colW, h: ribbonH },
  };
  if (showFilm) {
    layout.filmstrip = { x: M, y: heroY + heroH + gap, w: colW, h: filmH };
  }
  return layout;
}

// --- Sections ---

function paintHeader({ ink, text, x, y, w }) {
  // Mood (default face, near-white) — clamped to a single line so it never
  // wraps down into the hero. ~6px per glyph in the default face.
  const mood = profile?.mood || latestMood();
  if (mood) {
    const maxChars = max(8, floor(w / 6));
    ink(232).write(truncate(compact(mood), maxChars), { x, y }, undefined, w, false);
  } else {
    ink(110).write("no mood yet", { x, y }, undefined, w, false);
  }

  // Presence line in the small font (dot + status + piece + ping/last-seen).
  const py = y + 12;
  const on = presence.online;
  ink(on ? 90 : 90, on ? 230 : 96, on ? 110 : 110).box(x, py + 1, 4, 4);

  const parts = [on ? "online" : "offline"];
  if (on && presence.currentPiece) parts.push(formatPieceLabel(presence.currentPiece));
  if (on && presence.ping) parts.push(`${presence.ping}ms`);
  if (!on && presence.lastSeenAt) parts.push(`seen ${formatTimeAgo(presence.lastSeenAt)}`);

  ink(on ? 200 : 130).write(
    parts.join(" · "),
    { x: x + 7, y: py },
    undefined,
    w - 7,
    false,
    SMALL_FONT,
  );
}

function paintHero({ ink, paste, text, region }) {
  const { x, y, w, h } = region;
  if (h <= 0) return;
  const hero = thumbs[0];
  const route = hero?.slug ? `painting~${visiting}/${hero.slug}` : null;

  // Empty / still-loading: a framed placeholder filling the box.
  if (!hero || !hero.img || !paste) {
    ink(bgTint[0] + 7, bgTint[1] + 7, bgTint[2] + 9).box(x, y, w, h);
    ink(dom.r, dom.g, dom.b, 120).box(x, y, w, h, "outline");
    const msg = !hero
      ? thumbsLoading || loading || refreshing
        ? "loading painting…"
        : "no paintings yet"
      : "…";
    ink(140).write(msg, { x: x + 5, y: y + 5 }, undefined, w - 6, false, SMALL_FONT);
    if (route) hitRegions.push({ box: { x, y, w, h }, route });
    return;
  }

  // Fit the painting to the box and frame it *tightly* so wide/tall art floats
  // on the ambient ground instead of leaving empty bands inside a big frame.
  const scale = min(w / hero.img.width, h / hero.img.height);
  const iw = max(1, round(hero.img.width * scale));
  const ih = max(1, round(hero.img.height * scale));
  const ix = x + floor((w - iw) / 2);
  const iy = y + floor((h - ih) / 2);

  ink(bgTint[0] + 6, bgTint[1] + 6, bgTint[2] + 8).box(ix - 1, iy - 1, iw + 2, ih + 2);
  paste(hero.img, ix, iy, { scale });
  ink(dom.r, dom.g, dom.b, 175).box(ix - 1, iy - 1, iw + 2, ih + 2, "outline");

  // Share code, hugging the painting's bottom-right (@sat: short-codes as IDs).
  const code = hero.code ? `#${hero.code}` : shortSlug(hero.slug);
  const cw = measure(text, code) + 6;
  ink(0, 0, 0, 150).box(ix + iw - cw - 1, iy + ih - 10, cw, 9);
  ink(TYPE_COLOR.painting).write(
    code,
    { x: ix + iw - cw + 1, y: iy + ih - 9 },
    undefined,
    cw,
    false,
    SMALL_FONT,
  );

  if (route) hitRegions.push({ box: { x: ix, y: iy, w: iw, h: ih }, route });
}

function paintFilmstrip({ ink, paste, region }) {
  const { x, y, w, h } = region;
  const rest = thumbs.slice(1);
  if (rest.length === 0) return;

  const gap = 4;
  const tileW = h; // square tiles, height-driven
  const count = min(rest.length, max(1, floor((w + gap) / (tileW + gap))));

  for (let i = 0; i < count; i++) {
    const t = rest[i];
    const tx = x + i * (tileW + gap);
    ink(bgTint[0] + 10, bgTint[1] + 10, bgTint[2] + 12).box(tx, y, tileW, h);
    if (t.img && paste) {
      const scale = min(tileW / t.img.width, h / t.img.height);
      const ix = tx + floor((tileW - t.img.width * scale) / 2);
      const iy = y + floor((h - t.img.height * scale) / 2);
      paste(t.img, ix, iy, { scale });
    }
    ink(dom.r, dom.g, dom.b, 90).box(tx, y, tileW, h, "outline");
    t.route = t.slug ? `painting~${visiting}/${t.slug}` : null;
    if (t.route) hitRegions.push({ box: { x: tx, y, w: tileW, h }, route: t.route });
  }
}

// Wide-frame recent-works grid (right column).
function paintGrid({ ink, paste, region }) {
  const { x, y, w, h } = region;
  const rest = thumbs.slice(1);
  if (rest.length === 0) {
    ink(110).write("no other paintings", { x, y }, undefined, w, false, SMALL_FONT);
    paintRibbon({ ink, region: { x, y: y + h - 11, w, h: 11 } });
    return;
  }

  const gap = 5;
  const cols = w >= 320 ? 3 : 2;
  const tileW = floor((w - gap * (cols - 1)) / cols);
  const tileH = floor(tileW * 0.75);
  const ribbonH = 12;
  const gridBottom = y + h - ribbonH - 4;

  let i = 0;
  for (let row = 0; ; row++) {
    const ty = y + row * (tileH + gap);
    if (ty + tileH > gridBottom) break;
    for (let col = 0; col < cols; col++) {
      const t = rest[i];
      if (!t) break;
      const tx = x + col * (tileW + gap);
      ink(bgTint[0] + 10, bgTint[1] + 10, bgTint[2] + 12).box(tx, ty, tileW, tileH);
      if (t.img && paste) {
        const scale = min(tileW / t.img.width, tileH / t.img.height);
        const ix = tx + floor((tileW - t.img.width * scale) / 2);
        const iy = ty + floor((tileH - t.img.height * scale) / 2);
        paste(t.img, ix, iy, { scale });
      }
      ink(dom.r, dom.g, dom.b, 90).box(tx, ty, tileW, tileH, "outline");
      t.route = t.slug ? `painting~${visiting}/${t.slug}` : null;
      if (t.route) hitRegions.push({ box: { x: tx, y: ty, w: tileW, h: tileH }, route: t.route });
      i++;
    }
    if (!rest[i]) break;
  }

  // Catalog ribbon along the bottom of the column.
  paintRibbon({ ink, region: { x, y: y + h - ribbonH, w, h: ribbonH } });
}

// Compact catalog ribbon (@sat's composite catalog, distilled to counts) —
// replaces the 30-row activity ledger. Small font, type-colored, tappable.
function paintRibbon({ ink, text, region }) {
  const { x, y, w } = region;
  const c = scorecard.counts;
  const chips = [
    { n: c.paintings, label: "paintings", color: TYPE_COLOR.painting, route: `paintings~${visiting}` },
    { n: c.pieces, label: "pieces", color: TYPE_COLOR.chat, route: null },
    { n: c.kidlisp, label: "kidlisp", color: TYPE_COLOR.kidlisp, route: null },
    { n: c.moods, label: "moods", color: TYPE_COLOR.mood, route: null },
    { n: c.clocks, label: "clocks", color: TYPE_COLOR.clock, route: null },
  ].filter((chip) => chip.n > 0);

  if (chips.length === 0) return;

  let cx = x;
  const sep = "  ";
  for (let i = 0; i < chips.length; i++) {
    const chip = chips[i];
    const label = `${chip.n} ${chip.label}`;
    const cw = measure(text, label);
    if (cx + cw > x + w) break; // never overflow the ribbon
    ink(chip.color).write(label, { x: cx, y }, undefined, cw + 2, false, SMALL_FONT);
    if (chip.route) hitRegions.push({ box: { x: cx, y: y - 1, w: cw, h: 10 }, route: chip.route });
    cx += cw;
    if (i < chips.length - 1) {
      const sw = measure(text, sep);
      if (cx + sw > x + w) break;
      ink(90).write(sep, { x: cx, y }, undefined, sw + 2, false, SMALL_FONT);
      cx += sw;
    }
  }
}

function paintNoProfileState({ api, help, ink, screen, ui }) {
  const retrieving = noprofile === FETCHING;
  const label = noprofile || "No profile.";

  if (!noprofileAction) {
    const t = retrieving
      ? `${label}${ellipsisTicker.text(help.repeat)}`
      : label;
    ink(retrieving ? 150 : 220).write(t, { center: "xy" });
    return;
  }

  noprofileBtn ||= new ui.TextButton(label, { center: "xy", screen });
  noprofileBtn.paint(api);
}

// --- Data loading ---

async function refreshProfile(force = false) {
  if (!visiting || disposed) return;
  if (loading || refreshing) return;

  if (force) refreshing = true;
  else loading = true;

  dataError = null;

  try {
    await loadIdentity();
    if (!profile) return;
    await loadScorecard();
    loadThumbnails(); // fire and forget
  } catch (err) {
    dataError = "Could not load activity.";
    if (debug) console.warn("Profile refresh failed:", err);
  } finally {
    loading = false;
    refreshing = false;
  }
}

async function loadIdentity() {
  if (!visiting || disposed) return;

  try {
    const response = await fetch(`/api/profile/${visiting}`, {
      headers: { Accept: "application/json" },
    });

    if (!response.ok) {
      profile = null;
      noprofile = `No profile found for: ${visiting}`;
      return;
    }

    const data = await response.json();
    profile = {
      handle: visiting,
      sub: data?.sub || null,
      mood: data?.mood?.mood || null,
      moodWhen: data?.mood?.when || null,
    };
    noprofile = null;
  } catch (error) {
    profile = null;
    noprofile = "Error retrieving profile.";
    if (debug) console.warn("Profile lookup failed:", error);
  }
}

async function loadScorecard() {
  const handle = visiting;
  const bare = bareHandle(visiting);

  const [
    paintingRes,
    pieceRes,
    tapeRes,
    kidlispRes,
    clockRes,
    moodRes,
    chatSystemRes,
    chatClockRes,
  ] = await Promise.all([
    fetchJson(
      `/media-collection?for=${encodeURIComponent(`${handle}/painting`)}`,
    ),
    fetchJson(
      `/media-collection?for=${encodeURIComponent(`${handle}/piece`)}`,
    ),
    fetchJson(
      `/media-collection?for=${encodeURIComponent(`${handle}/tape`)}`,
    ),
    fetchJson(
      `/api/store-kidlisp?recent=true&limit=30&handle=${encodeURIComponent(handle)}`,
    ),
    fetchJson(`/api/store-clock?recent=true&limit=120`),
    fetchJson(`/api/mood/all?for=${encodeURIComponent(handle)}`),
    fetchJson(`/api/chat-messages?instance=system&limit=${CHAT_LIMIT}`),
    fetchJson(`/api/chat-messages?instance=clock&limit=${CHAT_LIMIT}`),
  ]);

  const paintingFiles = Array.isArray(paintingRes?.files)
    ? paintingRes.files
    : [];
  const pieceFiles = Array.isArray(pieceRes?.files) ? pieceRes.files : [];
  const tapeFiles = Array.isArray(tapeRes?.files) ? tapeRes.files : [];
  const kidlispRecent = Array.isArray(kidlispRes?.recent)
    ? kidlispRes.recent
    : [];
  const allClocks = Array.isArray(clockRes?.recent) ? clockRes.recent : [];
  const clocksForHandle = allClocks.filter((item) =>
    sameHandle(item?.handle, handle),
  );
  const moodsRaw = Array.isArray(moodRes?.moods) ? moodRes.moods : [];
  const recentMoods = moodsRaw
    .filter((item) => sameHandle(item?.handle, handle))
    .slice(0, MOOD_LIMIT)
    .map((item) => ({
      mood: item?.mood || "",
      when: toTimestamp(item?.when),
    }));
  if (recentMoods.length === 0 && profile?.mood) {
    recentMoods.push({
      mood: profile.mood,
      when: toTimestamp(profile.moodWhen) || Date.now(),
    });
  }
  const systemMessages = Array.isArray(chatSystemRes?.messages)
    ? chatSystemRes.messages
    : [];
  const clockMessages = Array.isArray(chatClockRes?.messages)
    ? chatClockRes.messages
    : [];
  const recentChats = [...systemMessages, ...clockMessages]
    .filter((item) => sameHandle(item?.from, handle))
    .map((item) => ({
      from: item?.from || null,
      text: item?.text || "",
      when: toTimestamp(item?.when),
      hearts: item?.hearts || 0,
    }))
    .sort((a, b) => (b.when || 0) - (a.when || 0))
    .slice(0, CHAT_LIMIT);

  const recentPaintings = paintingFiles
    .map((url) => parsePaintingUrl(url))
    .filter(Boolean)
    .sort((a, b) => {
      if (a.when && b.when) return b.when - a.when;
      if (a.when) return -1;
      if (b.when) return 1;
      return 0;
    })
    .slice(0, 8);

  await Promise.all(
    recentPaintings.slice(0, 6).map(async (painting) => {
      painting.code = await fetchPaintingCode(painting.slug, bare);
    }),
  );

  const recentKidlisp = kidlispRecent.slice(0, 8).map((item) => ({
    code: item.code,
    when: toTimestamp(item.when),
    hits: item.hits || 0,
  }));

  const recentClocks = clocksForHandle.slice(0, 8).map((item) => ({
    code: item.code,
    when: toTimestamp(item.when),
    hits: item.hits || 0,
  }));

  // Build unified activity list.
  const activity = [];

  recentMoods.slice(0, 12).forEach((item) => {
    if (!item?.mood) return;
    activity.push({
      type: "mood",
      when: item.when,
      label: truncate(compact(item.mood), 50),
    });
  });

  recentPaintings.forEach((item) => {
    const label = item.code ? `#${item.code}` : shortSlug(item.slug);
    activity.push({
      type: "painting",
      when: item.when,
      label,
      route: `painting~${visiting}/${item.slug}`,
    });
  });

  recentKidlisp.forEach((item) => {
    activity.push({
      type: "kidlisp",
      when: item.when,
      label: `$${item.code}`,
      route: `$${item.code}`,
    });
  });

  recentClocks.forEach((item) => {
    activity.push({
      type: "clock",
      when: item.when,
      label: `*${item.code}`,
      route: `*${item.code}`,
    });
  });

  recentChats.slice(0, 18).forEach((item) => {
    const text = compact(item.text);
    if (!text) return;
    activity.push({
      type: "chat",
      when: item.when,
      label: truncate(text, 46),
    });
  });

  activity.sort((a, b) => {
    const aWhen = a.when || 0;
    const bWhen = b.when || 0;
    return bWhen - aWhen;
  });

  scorecard = {
    counts: {
      paintings: paintingFiles.length,
      pieces: pieceFiles.length,
      kidlisp: kidlispRecent.length,
      clocks: clocksForHandle.length,
      tapes: tapeFiles.length,
      moods: recentMoods.length,
      chats: recentChats.length,
    },
    recentMedia: {
      paintings: recentPaintings,
      kidlisp: recentKidlisp,
      clocks: recentClocks,
      moods: recentMoods.slice(0, 10),
      chats: recentChats.slice(0, 10),
    },
    activity: activity.slice(0, ACTIVITY_MAX),
    updatedAt: Date.now(),
  };
}

async function loadThumbnails() {
  if (!getApi || thumbsLoading || disposed) return;

  const paintings = scorecard.recentMedia.paintings;
  if (paintings.length === 0) return;

  thumbsLoading = true;

  const toLoad = paintings.slice(0, THUMB_MAX);

  // Preserve already-loaded thumbs that match.
  const existing = new Map(thumbs.map((t) => [t.slug, t]));

  const next = [];
  for (const p of toLoad) {
    if (existing.has(p.slug) && existing.get(p.slug).img) {
      next.push(existing.get(p.slug));
    } else {
      next.push({
        slug: p.slug,
        code: p.code,
        img: null,
        when: p.when,
        btn: null,
        route: null,
      });
    }
  }
  thumbs = next;

  // Load missing images.
  await Promise.all(
    thumbs.map(async (t) => {
      if (t.img || disposed) return;
      try {
        const got = await getApi.painting(t.slug).by(bareHandle(visiting));
        if (!disposed) t.img = got?.img || null;
      } catch (err) {
        if (debug) console.warn("Thumb load failed:", t.slug, err);
      }
    }),
  );

  thumbsLoading = false;
}

// --- WebSocket profile stream ---

function startRefreshLoop() {
  clearInterval(refreshTimer);
  refreshTimer = setInterval(() => {
    if (!disposed) refreshProfile(true);
  }, REFRESH_MS);
}

function connectProfileStream() {
  if (!visiting || disposed) return;
  closeStatusSocket();

  const url = getProfileStreamUrl();
  if (!url) return;

  try {
    statusSocket = new WebSocket(url);
  } catch (err) {
    if (debug) console.warn("Could not open profile stream:", err);
    scheduleReconnect();
    return;
  }

  statusSocket.addEventListener("message", (event) => {
    if (disposed) return;

    try {
      const msg = JSON.parse(event.data);
      if (msg?.type === "profile:snapshot" || msg?.type === "presence:update") {
        applyPresenceSnapshot(msg?.data?.presence);
        return;
      }
      if (msg?.type === "activity:append") {
        appendActivity(msg?.data?.event);
        return;
      }
      if (msg?.type === "counts:update") {
        mergeCounts(msg?.data?.counts);
        return;
      }
      if (msg?.type === "counts:delta") {
        applyCountDelta(msg?.data?.delta);
        return;
      }
      if (msg?.type === "status") {
        const clients = Array.isArray(msg?.data?.clients)
          ? msg.data.clients
          : [];
        applyPresence(clients);
      }
    } catch (err) {
      if (debug) console.warn("Profile stream parse failure:", err);
    }
  });

  statusSocket.addEventListener("close", () => {
    statusSocket = null;
    if (!disposed) scheduleReconnect();
  });

  statusSocket.addEventListener("error", () => {
    try {
      statusSocket?.close();
    } catch (_) {
      // Ignore close errors.
    }
  });
}

function applyPresence(clients) {
  const matched = clients.find((client) =>
    sameHandle(client?.handle, visiting),
  );

  if (!matched) {
    if (presence.online) presence.lastSeenAt = Date.now();
    presence.online = false;
    presence.currentPiece = null;
    presence.worldPiece = null;
    presence.showing = null;
    presence.connections = 0;
    presence.ping = null;
    return;
  }

  const world = matched?.websocket?.worlds?.[0] || null;

  presence.online = true;
  presence.currentPiece = matched.location || null;
  presence.worldPiece = world?.piece || null;
  presence.showing = formatShowing(world?.showing);
  presence.connections = matched?.connectionCount?.total || 1;
  presence.ping = matched?.websocket?.ping || null;
  presence.lastSeenAt = Date.now();
}

function applyPresenceSnapshot(snapshot) {
  if (!snapshot || typeof snapshot !== "object") return;

  const isOnline = !!snapshot.online;
  if (!isOnline && presence.online) {
    presence.lastSeenAt = Date.now();
  }

  presence.online = isOnline;
  presence.currentPiece = snapshot.currentPiece || null;
  presence.worldPiece = snapshot.worldPiece || null;
  presence.showing = formatShowing(snapshot.showing);

  if (snapshot.connections && typeof snapshot.connections === "object") {
    presence.connections =
      snapshot.connections.total ||
      snapshot.connections.websocket ||
      snapshot.connections.udp ||
      0;
  } else {
    presence.connections = snapshot.connections || 0;
  }

  presence.ping = snapshot.pingMs || snapshot.ping || null;
  const fallbackLastSeen = isOnline ? Date.now() : presence.lastSeenAt || null;
  presence.lastSeenAt = snapshot.lastSeenAt || fallbackLastSeen;
}

function appendActivity(event) {
  if (!event || typeof event !== "object") return;
  const label = compact(event.label || event.text || "");
  if (!label) return;

  const when = toTimestamp(event.when) || Date.now();
  scorecard.activity = [
    { type: event.type || "event", when, label },
    ...scorecard.activity,
  ]
    .sort((a, b) => (b.when || 0) - (a.when || 0))
    .slice(0, ACTIVITY_MAX);
}

function mergeCounts(nextCounts) {
  if (!nextCounts || typeof nextCounts !== "object") return;
  scorecard.counts = { ...scorecard.counts, ...nextCounts };
}

function applyCountDelta(delta) {
  if (!delta || typeof delta !== "object") return;

  const next = { ...scorecard.counts };
  for (const [key, value] of Object.entries(delta)) {
    const amount = Number(value);
    if (!Number.isFinite(amount)) continue;
    const previous = Number(next[key] || 0);
    const updated = previous + amount;
    next[key] = updated < 0 ? 0 : updated;
  }

  scorecard.counts = next;
}

// --- Timers & socket ---

function formatShowing(showing) {
  if (!showing) return null;
  if (typeof showing === "string") return truncate(showing, 18);
  if (showing.slug) return truncate(showing.slug, 18);
  if (showing.code) return `#${showing.code}`;
  if (showing.piece) return truncate(showing.piece, 18);
  if (showing.url)
    return truncate(`${showing.url}`.split("/").pop() || "", 18);
  return null;
}

function getProfileStreamUrl() {
  const handle = normalizeHandle(visiting);
  if (!handle) return null;

  const query = `handle=${encodeURIComponent(handle)}`;
  if (typeof location === "undefined") {
    return `wss://session-server.aesthetic.computer/profile-stream?${query}`;
  }

  const host = location.hostname;
  const isLocal =
    host === "localhost" ||
    host === "127.0.0.1" ||
    host.endsWith(".local") ||
    host.endsWith(".localhost");

  if (isLocal) {
    const protocol = location.protocol === "http:" ? "ws" : "wss";
    return `${protocol}://localhost:8889/profile-stream?${query}`;
  }

  return `wss://session-server.aesthetic.computer/profile-stream?${query}`;
}

function scheduleReconnect() {
  clearTimeout(reconnectTimer);
  reconnectTimer = setTimeout(() => {
    if (!disposed) connectProfileStream();
  }, RECONNECT_MS);
}

function closeStatusSocket() {
  if (!statusSocket) return;
  try {
    statusSocket.close();
  } catch (_) {
    // Ignore close errors.
  }
  statusSocket = null;
}

function clearTimersAndSocket() {
  clearInterval(refreshTimer);
  clearTimeout(reconnectTimer);
  refreshTimer = null;
  reconnectTimer = null;
  closeStatusSocket();
}

function resetUiState() {
  noprofileBtn = null;
  paintingsBtn = null;
  activityBtns = [];
  for (const t of thumbs) t.btn = null;
}

// --- Handle palette ---

async function loadHandleColors() {
  if (!visiting || disposed) return;
  const bare = bareHandle(visiting);
  try {
    const res = await fetch(
      `/api/handle-colors?handle=${encodeURIComponent(bare)}`,
      { headers: { Accept: "application/json" } },
    );
    if (res.ok) {
      const data = await res.json();
      handleColors = Array.isArray(data?.colors) ? data.colors : null;
    }
  } catch (err) {
    if (debug) console.warn("Handle colors fetch failed:", err);
  } finally {
    colorsLoaded = true;
    computeHandlePalette();
    hudApi?.label(handleLabel || normalizeHandle(visiting));
  }
}

// Derive the dominant hue, ambient tint, and colorized HUD label for the
// handle. Uses stored per-character colors when present, else a deterministic
// hash (same scheme as arena.mjs) so every handle still gets a color.
function computeHandlePalette() {
  const stored = Array.isArray(handleColors) && handleColors.length;
  if (stored) {
    let r = 0, g = 0, b = 0, n = 0;
    for (const c of handleColors) {
      if (!c) continue;
      r += c.r; g += c.g; b += c.b; n++;
    }
    if (n) dom = { r: round(r / n), g: round(g / n), b: round(b / n) };
  } else {
    const [r, g, b] = handleColor(visiting);
    dom = { r, g, b };
  }

  // Lift very dark hues so the frame/edge stays visible against the ground.
  const lum = (dom.r + dom.g + dom.b) / 3;
  if (lum < 70) {
    const k = 70 / max(1, lum);
    dom = {
      r: min(255, round(dom.r * k)),
      g: min(255, round(dom.g * k)),
      b: min(255, round(dom.b * k)),
    };
  }

  bgTint = [
    clamp(round(dom.r * 0.1) + 9, 8, 40),
    clamp(round(dom.g * 0.1) + 10, 8, 40),
    clamp(round(dom.b * 0.1) + 16, 10, 46),
  ];

  handleLabel = colorizeHandleStr(visiting, stored ? handleColors : null);
}

// Build a `\r,g,b\char\255,255,255\` string the text renderer colorizes.
function colorizeHandleStr(handle, colors) {
  const h = normalizeHandle(handle);
  if (!h) return null;
  const def = "255,255,255";
  let out = "";
  for (let i = 0; i < h.length; i++) {
    let r = dom.r, g = dom.g, b = dom.b;
    if (colors && colors[i]) ({ r, g, b } = colors[i]);
    out += `\\${r},${g},${b}\\${h[i]}\\${def}\\`;
  }
  return out;
}

function handleHash(h) {
  let x = 0;
  for (let i = 0; i < h.length; i++) x = (x * 31 + h.charCodeAt(i)) | 0;
  return x >>> 0;
}

function hslToRgb(hue, sat, light) {
  const c = sat * (1 - Math.abs(2 * light - 1));
  const xx = c * (1 - Math.abs(((hue / 60) % 2) - 1));
  const m = light - c / 2;
  let r = 0, g = 0, b = 0;
  if (hue < 60) { r = c; g = xx; }
  else if (hue < 120) { r = xx; g = c; }
  else if (hue < 180) { g = c; b = xx; }
  else if (hue < 240) { g = xx; b = c; }
  else if (hue < 300) { r = xx; b = c; }
  else { r = c; b = xx; }
  return [round((r + m) * 255), round((g + m) * 255), round((b + m) * 255)];
}

function handleColor(h) {
  const hue = handleHash(normalizeHandle(h) || "@") % 360;
  return hslToRgb(hue, 0.7, 0.6);
}

function latestMood() {
  const moods = scorecard.recentMedia?.moods;
  return moods && moods.length ? moods[0].mood : null;
}

// Measure a string in the small font; falls back to a char-width estimate.
function measure(text, str) {
  try {
    const tb = text?.box?.(str, { x: 0, y: 0 }, undefined, 1, false, SMALL_FONT);
    const wdt = tb?.box?.width;
    if (Number.isFinite(wdt) && wdt > 0) return round(wdt);
  } catch (_) {
    // fall through to estimate
  }
  return `${str}`.length * 4;
}

function clamp(v, lo, hi) {
  return v < lo ? lo : v > hi ? hi : v;
}

// --- Utilities ---

async function fetchJson(url) {
  try {
    const response = await fetch(url, {
      headers: { Accept: "application/json" },
    });
    if (!response.ok) return null;
    return await response.json();
  } catch (err) {
    if (debug) console.warn("Fetch failed:", url, err);
    return null;
  }
}

function parsePaintingUrl(url) {
  if (!url) return null;
  const clean = `${url}`.split("?")[0];
  const parts = clean.split("/");
  const last = parts.pop();
  if (!last) return null;

  const slug = last.replace(/\.(png|zip)$/i, "");
  if (!slug) return null;

  return {
    slug,
    url,
    when: timestampFromSlug(slug),
    code: null,
  };
}

async function fetchPaintingCode(slug, bare) {
  if (!slug || !bare) return null;

  const params = new URLSearchParams({ slug, handle: bare });
  const data = await fetchJson(`/api/painting-metadata?${params.toString()}`);
  if (!data?.code) return null;
  return `${data.code}`.replace(/^#/, "");
}

function timestampFromSlug(slug) {
  if (!slug) return null;
  const base = `${slug}`.split(":")[0];
  if (!/^\d+$/.test(base)) return null;
  const maybe = Number(base);
  if (!Number.isFinite(maybe)) return null;
  if (maybe < 1000000000) return null;
  return maybe;
}

function normalizeHandle(value) {
  if (!value) return null;
  const text = `${value}`.trim();
  if (!text) return null;
  return text.startsWith("@") ? text : `@${text}`;
}

function bareHandle(value) {
  return normalizeHandle(value)?.slice(1) || null;
}

function sameHandle(a, b) {
  const left = normalizeHandle(a);
  const right = normalizeHandle(b);
  if (!left || !right) return false;
  return left.toLowerCase() === right.toLowerCase();
}

function toTimestamp(value) {
  if (!value) return null;
  const ts = new Date(value).getTime();
  if (!Number.isFinite(ts)) return null;
  return ts;
}

function formatTimeAgo(when) {
  const timestamp = typeof when === "number" ? when : toTimestamp(when);
  if (!timestamp) return "";

  const diff = max(0, Date.now() - timestamp);
  const seconds = floor(diff / 1000);
  if (seconds < 60) return `${seconds}s`;

  const minutes = floor(seconds / 60);
  if (minutes < 60) return `${minutes}m`;

  const hours = floor(minutes / 60);
  if (hours < 24) return `${hours}h`;

  const days = floor(hours / 24);
  return `${days}d`;
}

function formatPieceLabel(slug) {
  if (!slug) return null;
  const normalized = `${slug}`.trim();
  if (!normalized) return null;
  const piece = normalized.split("~")[0];
  return truncate(piece, 20);
}

function shortSlug(slug) {
  if (!slug) return "-";
  const text = `${slug}`;
  if (text.length <= 10) return text;
  return `${text.slice(0, 4)}...${text.slice(-4)}`;
}

function compact(value) {
  return `${value || ""}`.replace(/\s+/g, " ").trim();
}

function truncate(value, mx) {
  const text = `${value || ""}`;
  if (text.length <= mx) return text;
  return `${text.slice(0, mx - 3)}...`;
}

export { boot, paint, act, sim, leave, meta };
