// Profile, 2026.10.30
// @handle pages: a big handle, the numbers, then the media you scroll through.
// MIMEs is the default tab — everything this handle has made that mime can
// comment on. The other tabs are one kind each.

const SMALL = "MatrixChunky8";
const TOP = 22; // under the corner label
const PAD = 6;
const GAP = 1;
const TABS = [
  ["mime", "MIMEs"], ["painting", "paintings"], ["piece", "pieces"],
  ["kidlisp", "kidlisp"], ["tape", "tapes"], ["whistlegraph", "whistlegraphs"],
];
const KIND_COLOR = {
  mime: [235, 235, 225], painting: [100, 200, 255], piece: [255, 160, 200],
  kidlisp: [120, 255, 120], tape: [255, 200, 80], whistlegraph: [200, 160, 255],
};
const ORIGIN = typeof location !== "undefined" && /^https?:$/.test(location.protocol)
  ? location.origin : "https://aesthetic.computer";
const { floor, round, max, min, abs } = Math;

let net, visiting, card, cardError, tab, feeds, images, bg;
let scroll = 0; // <= 0, 0 = top
let drag; // { moved } while a finger is down
let hits = []; // tap targets from the last paint: { x, y, w, h, go }
let contentH = 0;
let noprofileAction, noprofileBtn;

export function meta({ piece }) {
  return { title: `${piece} - aesthetic.computer`, desc: `Profile for ${piece}.` };
}

export function boot({ params, handle, user, net: n, store }) {
  net = n;
  visiting = normalize(params[0] || handle());
  card = cardError = drag = undefined;
  tab = "mime"; feeds = {}; images = {}; hits = []; scroll = 0; bg = [16, 26, 22];
  if (!visiting) {
    noprofileAction = user ? user.email_verified ? "handle" : "email" : "imnew";
    return;
  }
  net.rewrite(visiting);
  loadCard();
  loadFeed(tab);
}

export function paint({ api, wipe, ink, screen, paste, text, pen, ui }) {
  wipe(...bg);
  hits = [];
  if (!visiting) return paintNoProfile({ api, ink, ui });
  const W = screen.width;
  let y = TOP + scroll;

  // Handle, wearing its colors.
  const name = card?.handle || visiting;
  const size = text.width(name) * 2 <= W - PAD * 2 ? 2 : 1;
  const cw = text.width("@") * size;
  [...name].forEach((ch, i) => {
    const c = card?.colors?.[i];
    ink(c ? [c.r, c.g, c.b] : 235).write(ch, { x: PAD + i * cw, y, size });
  });
  y += 10 * size + 3;

  // Mood as the bio line.
  const mood = card?.mood?.text?.replace(/\s+/g, " ").trim();
  if (mood) {
    const bounds = W - PAD * 2;
    const h = text.box(mood, { x: PAD, y }, bounds, 1, true)?.box?.height || 10;
    ink(190).write(mood, { x: PAD, y }, undefined, bounds, true);
    y += min(h, 32) + 4;
  } else if (!card && !cardError) {
    ink(90).write("fetching...", { x: PAD, y });
    y += 12;
  } else if (cardError) {
    ink(255, 120, 120).write(cardError, { x: PAD, y }, undefined, W - PAD * 2, true);
    y += 12;
  }

  // The numbers double as tabs: count on top, kind below, active one underlined.
  const shown = TABS.filter(([k]) => k === "mime" || k === tab || card?.counts?.[k] > 0);
  let x = PAD, rowTop = y, rowH = 0;
  for (const [k, label] of shown) {
    const n = card?.counts?.[k];
    const num = n === undefined ? "-" : compact(n);
    const w = max(text.width(num), text.width(label, SMALL)) + 8;
    if (x + w > W - PAD && x > PAD) { x = PAD; rowTop += 22; }
    const on = k === tab, col = KIND_COLOR[k];
    ink(on ? 255 : 170).write(num, { x: x + 2, y: rowTop });
    ink(on ? col : 120).write(label, { x: x + 2, y: rowTop + 11 }, undefined, undefined, false, SMALL);
    if (on) ink(col).box(x, rowTop + 19, w - 4, 1);
    hits.push({ x, y: rowTop, w, h: 22, go: () => switchTab(k) });
    x += w; rowH = rowTop + 22 - y;
  }
  y += rowH + 2;
  ink(bg[0] + 30, bg[1] + 30, bg[2] + 30).box(0, y, W, 1);
  y += GAP + 1;

  // The grid.
  const feed = feeds[tab];
  const cols = clamp(floor(W / 64), 2, 8);
  const tile = floor((W - (cols - 1) * GAP) / cols);
  const left = floor((W - (cols * tile + (cols - 1) * GAP)) / 2);
  const posts = feed?.posts || [];
  const rows = Math.ceil(posts.length / cols);
  for (let r = 0; r < rows; r++) {
    const ty = y + r * (tile + GAP);
    if (ty + tile < 0 || ty > screen.height) continue;
    for (let c = 0; c < cols; c++) {
      const post = posts[r * cols + c];
      if (!post) break;
      paintTile({ ink, paste, text, pen }, post, left + c * (tile + GAP), ty, tile);
    }
  }
  y += rows * (tile + GAP);

  // Footer line: loading, empty, error, or more to fetch.
  const nearEnd = y - screen.height < tile * 2;
  if (feed?.hasMore && !feed.loading && !feed.error && nearEnd) loadFeed(tab);
  let foot;
  if (feed?.error) foot = [feed.error + " - tap to retry", [255, 120, 120], () => loadFeed(tab)];
  else if (feed?.loading || !feed) foot = ["loading...", 110];
  else if (posts.length === 0) foot = [tab === "mime" ? "nothing on mime yet" : `no ${TABS.find(([k]) => k === tab)[1]} yet`, 110];
  if (foot) {
    ink(foot[1]).write(foot[0], { x: PAD, y: y + 6 }, undefined, W - PAD * 2, true);
    if (foot[2]) hits.push({ x: 0, y: y, w: W, h: 20, go: foot[2] });
    y += 20;
  }
  contentH = y - scroll - TOP + TOP + PAD;
}

function paintTile({ ink, paste, text, pen }, post, x, y, s) {
  const kind = post.media?.kind || "mime";
  const col = KIND_COLOR[kind];
  const img = image(post);
  ink(bg[0] + 12, bg[1] + 12, bg[2] + 14).box(x, y, s, s);
  if (img && img.width) {
    const frame = img.frames ? img.frames[frameAt(img)] : img;
    const scale = min(s / img.width, s / img.height);
    const w = max(1, round(img.width * scale)), h = max(1, round(img.height * scale));
    paste(frame, x + floor((s - w) / 2), y + floor((s - h) / 2), { scale });
  } else {
    // Code and words wear the kind's color.
    ink(col[0], col[1], col[2], img === null ? 40 : 18).box(x, y, s, s);
    ink(col).write(tileLabel(post, kind), { x: x + 3, y: y + 3 }, undefined, s - 5, true, SMALL);
  }
  if (post.replies > 0) {
    const n = compact(post.replies), w = text.width(n, SMALL) + 5;
    ink(0, 0, 0, 170).box(x + s - w - 1, y + s - 10, w, 9);
    ink(255).write(n, { x: x + s - w + 2, y: y + s - 9 }, undefined, undefined, false, SMALL);
  }
  if (pen && pen.x >= x && pen.x < x + s && pen.y >= y && pen.y < y + s) ink(col).box(x, y, s, s, "outline");
  hits.push({ x, y, w: s, h: s, go: () => open(post) });
}

function paintNoProfile({ api, ink, ui }) {
  const label = { handle: "Create a handle", email: "Verify your email", imnew: "Log in or sign up" }[noprofileAction];
  ink(200).write("No profile to show.", { x: PAD, y: TOP });
  noprofileBtn ||= new ui.TextButton(label, { x: PAD, y: TOP + 16 });
  noprofileBtn.paint(api);
}

export function act({ event: e, jump, store, user, screen }) {
  if (!visiting) {
    if (e.is("reframed")) noprofileBtn = undefined;
    noprofileBtn?.act(e, () => {
      if (noprofileAction === "handle") jump("prompt~handle");
      else if (noprofileAction === "email") jump("prompt~email~" + user?.email);
      else { store["prompt:splash"] = true; jump("prompt"); }
    });
    return;
  }
  if (e.is("touch")) drag = { moved: 0 };
  if (e.is("draw") && drag) {
    scroll += e.delta?.y || 0;
    drag.moved += abs(e.delta?.y || 0) + abs(e.delta?.x || 0);
    clampScroll(screen);
  }
  if (e.is("lift")) {
    if (drag && drag.moved < 4) hits.find((h) => e.x >= h.x && e.x < h.x + h.w && e.y >= h.y && e.y < h.y + h.h)?.go(jump);
    drag = undefined;
  }
  if (e.is("scroll")) { scroll -= e.y || 0; clampScroll(screen); }
  if (e.is("keyboard:down:arrowdown")) { scroll -= 24; clampScroll(screen); }
  if (e.is("keyboard:down:arrowup")) { scroll += 24; clampScroll(screen); }
  if (e.is("keyboard:down:pagedown") || e.is("keyboard:down:space")) { scroll -= screen.height - 20; clampScroll(screen); }
  if (e.is("keyboard:down:pageup")) { scroll += screen.height - 20; clampScroll(screen); }
  if (e.is("keyboard:down:r")) { feeds[tab] = undefined; loadCard(); loadFeed(tab); }
  if (e.is("reframed")) clampScroll(screen);
}

export function leave() { net = undefined; }

function clampScroll(screen) {
  const minScroll = min(0, screen.height - contentH);
  if (scroll > 0) scroll = 0;
  if (scroll < minScroll) scroll = minScroll;
}

function switchTab(k) {
  if (k === tab) return;
  tab = k;
  if (!feeds[k]) loadFeed(k);
}

// Where a tap goes. MIMEs open the thread, where the comments are; a kind
// tab opens the thing itself.
function open(post) {
  return (jump) => {
    const m = post.media;
    if (tab === "mime" || !m) return jump("out:https://mime.ac/" + encodeURIComponent(post.code));
    if (m.kind === "painting" && m.code) return jump("painting~" + m.code);
    if (m.kind === "kidlisp" && m.code) return jump("$" + m.code);
    if (m.kind === "tape" && m.code) return jump("!" + m.code);
    if (m.kind === "piece" && m.url) return jump(new URL(m.url).pathname.slice(1));
    if (m.kind === "whistlegraph") return jump("out:" + (m.tokenUrl || m.url));
    jump("out:https://mime.ac/" + encodeURIComponent(post.code));
  };
}

function tileLabel(post, kind) {
  const m = post.media;
  if (kind === "kidlisp") return "$" + (m.code || "");
  if (kind === "tape") return "!" + (m.code || "");
  if (kind === "piece") return m.url ? decodeURIComponent(m.url.split("/").pop()) : m.code || "piece";
  if (kind === "whistlegraph") return m.title || "whistlegraph";
  if (kind === "painting") return "#" + (m.code || "");
  return post.text?.trim() || post.file?.name || post.code;
}

// Lazy, by visibility: a tile asks for its image the first time it paints.
// KidLisp and piece posters are the oven's animated webps; every frame comes
// over so the grid can move. Everything else is one bitmap.
function image(post) {
  const key = post.code;
  if (key in images) return images[key] === "loading" ? undefined : images[key];
  if (Object.keys(images).length > 90) for (const old of Object.keys(images).slice(0, 30)) delete images[old];
  // Paintings go by short code: that route resolves legacy storage folders.
  const m = post.media;
  const url = m?.kind === "painting" && m.code ? `/media/paintings/${m.code}.png`
    : post.file?.poster || (post.file?.type?.startsWith("image/") ? post.file.url : null);
  if (!url || !net) return (images[key] = null);
  images[key] = "loading";
  const href = new URL(url, ORIGIN).href;
  const still = () => net.preload({ path: href, extension: "png" }).then((got) => { images[key] = got?.img || got || null; });
  const animated = ["kidlisp", "piece"].includes(m?.kind) && post.file?.poster
    ? net.preloadAnimatedWebp(href).then((got) => {
      if (!got?.frames?.length) throw new Error("no frames");
      const frames = got.frames.map((f) => ({ width: got.width, height: got.height, pixels: f.pixels }));
      const ends = [];
      let total = 0;
      for (const f of got.frames) ends.push(total += f.duration || 125);
      images[key] = got.frameCount > 1 ? { width: got.width, height: got.height, frames, ends, total } : frames[0];
    }).catch(still)
    : still();
  animated.catch((err) => { images[key] = null; console.warn("profile: image failed", url, err?.message || err); });
  return undefined;
}

function frameAt(anim) {
  const t = Date.now() % anim.total;
  let i = 0;
  while (i < anim.ends.length - 1 && anim.ends[i] <= t) i++;
  return i;
}

async function loadCard() {
  cardError = undefined;
  try {
    card = await json(`/api/profile-card?handle=${encodeURIComponent(visiting)}`);
    const cs = (card.colors || []).filter(Boolean);
    if (cs.length) {
      const avg = ["r", "g", "b"].map((k) => cs.reduce((a, c) => a + c[k], 0) / cs.length);
      bg = avg.map((v) => round(10 + v * 0.1));
    }
  } catch (err) {
    cardError = reason(err);
  }
}

async function loadFeed(k) {
  const feed = (feeds[k] ||= { posts: [], page: 0, hasMore: true });
  if (feed.loading) return;
  feed.loading = true; feed.error = undefined;
  try {
    const got = await json(`/api/profile-card?handle=${encodeURIComponent(visiting)}&kind=${k}&page=${feed.page}`);
    if (feeds[k] !== feed) return;
    feed.posts.push(...got.posts);
    feed.hasMore = got.hasMore;
    feed.page += 1;
  } catch (err) {
    feed.error = reason(err);
  } finally {
    feed.loading = false;
  }
}

async function json(path) {
  const res = await fetch(new URL(path, ORIGIN), { signal: AbortSignal.timeout(25000) });
  if (!res.ok) throw new Error(res.status === 404 ? "no such profile" : "profile unavailable");
  return res.json();
}

function reason(err) {
  return err?.name === "TimeoutError" ? "slow network" : err?.message || "profile unavailable";
}

const normalize = (v) => {
  const h = String(v || "").replace(/^@/, "").toLowerCase();
  return /^[a-z0-9][a-z0-9_.-]{0,63}$/.test(h) ? "@" + h : null;
};
const clamp = (v, lo, hi) => max(lo, min(hi, v));
const compact = (n) => n >= 1e6 ? (n / 1e6).toFixed(1).replace(/\.0$/, "") + "m"
  : n >= 1e4 ? round(n / 1e3) + "k" : n >= 1e3 ? (n / 1e3).toFixed(1).replace(/\.0$/, "") + "k" : String(n);
