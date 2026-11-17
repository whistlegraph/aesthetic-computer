// Juke, 2026.10.19
// A jukebox for every /pop track, and for your own. `pop` is the released
// catalog; `mine` is your Juke cloud (the work in progress `pop/bin/juke-recent.mjs`
// sends up after a /pop commit), newest first, when you are signed in.
// `juke slug` cues a track; `juke mine` opens on your own.
// The iPhone app hands each released song to its native player, so the music
// keeps going on the lock screen; everything else streams here.

import { AestheticIOSApp } from "../lib/platform.mjs";

const CATALOG = "https://pop.aesthetic.computer/releases/catalog.json";
const CLOUD = "/api/juke-cloud";
const STREAM = "juke";
const colors = { ground: [22, 18, 34], panel: [44, 36, 66], text: [240, 232, 255],
  pink: [255, 112, 190], mint: [159, 240, 201], muted: [160, 146, 190], shadow: [70, 30, 80] };
const ROW = 20, GLYPH = 6;
// The list scrolls as chat does: drag, fling, spring back from the edges.
const FRICTION = 0.92, MIN_VELOCITY = 0.5, STIFFNESS = 0.15, DAMPING = 0.7, OVERSCROLL = 60, DRAG = 5;

let lists = { pop: [], mine: [] }, notes = { pop: "loading /pop...", mine: "" };
let tab = "pop", now = null; // now = { tab, i }: the list a track came from keeps playing.
let playing = false, error = "", elapsed = 0, duration = 0, mineAsked = false;
let buttons = {}, frame = 0, controller, handle = "";
let scroll = 0, scrollMax = 0, velocity = 0, flinging = false;
let press = null; // { x, y, dragged, zone: "list" | "rail" }
let listTop = 0, rail = null, scrubTo = null, pressedRow = -1;

const current = () => (now ? lists[now.tab][now.i] : null);
const native = (track) => AestheticIOSApp && track?.slug; // Only releases have a native player.

function boot($) {
  controller?.abort();
  controller = new AbortController();
  buttons = {}; scroll = velocity = frame = 0; flinging = false; error = ""; press = null;
  lists = { pop: [], mine: [] }; notes = { pop: "loading /pop...", mine: "" }; mineAsked = false;
  handle = $.handle?.() || "";
  const wanted = $.params?.[0] || null;
  tab = wanted === "mine" ? "mine" : "pop";
  $.hud?.labelBack();
  const lifetime = controller;
  fetch(CATALOG, { signal: AbortSignal.any([lifetime.signal, AbortSignal.timeout(12000)]) })
    .then(async response => {
      if (!response.ok) throw Error("Catalog unavailable");
      const data = await response.json();
      if (!Array.isArray(data.tracks)) throw Error("Catalog unavailable");
      if (lifetime.signal.aborted) return;
      lists.pop = data.tracks.filter(t => t.slug && t.audio);
      notes.pop = "";
      const i = lists.pop.findIndex(t => t.slug === wanted);
      if (i >= 0) play($, "pop", i);
    })
    .catch(() => { if (!lifetime.signal.aborted) notes.pop = "Couldn't load /pop."; })
    .finally(() => { if (!lifetime.signal.aborted) $.needsPaint(); });
  if (tab === "mine") loadMine($);
}

// Your Juke cloud, newest first. The file name is the title; you are the artist.
function loadMine($) {
  if (mineAsked) return;
  mineAsked = true;
  notes.mine = "loading yours...";
  const lifetime = controller;
  Promise.resolve($.net?.userRequest?.("GET", CLOUD))
    .then(data => {
      if (lifetime.signal.aborted) return;
      if (!Array.isArray(data?.tracks)) { notes.mine = handle ? "Couldn't load yours." : "log in to hear yours"; return; }
      lists.mine = data.tracks.filter(t => t.key).map(t => ({
        title: t.name.replace(/\.[a-z0-9]+$/i, ""), artist: handle || "you", album: "work in progress",
        key: t.key, duration: 0, when: day(t.updatedAt),
      }));
      notes.mine = lists.mine.length ? "" : "nothing in your Juke cloud yet";
    })
    .catch(() => { if (!lifetime.signal.aborted) notes.mine = "log in to hear yours"; })
    .finally(() => { if (!lifetime.signal.aborted) $.needsPaint(); });
}

function day(iso) {
  const d = new Date(iso);
  return Number.isNaN(d.getTime()) ? "" : d.toLocaleDateString("en-US", { month: "short", day: "numeric" }).toLowerCase();
}

function play($, from, i) {
  const track = lists[from][i];
  if (!track) return;
  // A new stream replaces the old one; only a hand-off to the app's player needs a stop.
  if (native(track) && current() && !native(current())) $.send({ type: "stream:stop", content: { id: STREAM } });
  now = { tab: from, i }; elapsed = 0; duration = track.duration || 0; playing = true; error = ""; scrubTo = null;
  if (native(track)) {
    // The native player owns playback in the app: lock screen, AirPlay, background.
    $.send({ type: "ios:send", content: { type: "pop:open", body: track.slug } });
    return;
  }
  if (track.audio) return $.send({ type: "stream:play", content: { id: STREAM, url: track.audio, volume: 1 } });
  // Your own tracks are private: ask for a signed link, then play it if you haven't moved on.
  Promise.resolve($.net?.userRequest?.("POST", CLOUD, { action: "listen", key: track.key }))
    .then(data => {
      if (current() !== track) return;
      if (!data?.url) throw Error("no link");
      $.send({ type: "stream:play", content: { id: STREAM, url: data.url, volume: 1 } });
    })
    .catch(() => { if (current() === track) { playing = false; error = "Couldn't play that one."; $.needsPaint(); } });
}

function toggle($) {
  if (!now) return play($, tab, 0);
  if (native(current())) return play($, now.tab, now.i);
  $.send({ type: playing ? "stream:pause" : "stream:resume", content: { id: STREAM } });
  playing = !playing;
}

function step($, by) {
  const from = now?.tab || tab, n = lists[from].length;
  if (n) play($, from, now ? (now.i + by + n) % n : 0);
}

function seek($, time) {
  if (!now || !duration || native(current())) return;
  elapsed = Math.max(0, Math.min(duration - 0.25, time));
  $.send({ type: "stream:seek", content: { id: STREAM, time: elapsed } });
}

function show($, name) {
  if (tab === name) return;
  tab = name; scroll = velocity = 0; flinging = false;
  if (name === "mine") loadMine($);
}

function clock(seconds) {
  const s = Math.max(0, Math.floor(seconds));
  return `${Math.floor(s / 60)}:${String(s % 60).padStart(2, "0")}`;
}

const fit = (text, chars) => (text.length > chars ? text.slice(0, Math.max(1, chars - 1)) + "~" : text);

function paint($) {
  const { ink, wipe, screen, ui } = $;
  wipe(colors.ground);
  $.hud?.label(); // The jukebox owns the whole screen; no corner label.
  const width = Math.min(260, screen.width - 16), left = Math.floor((screen.width - width) / 2);
  const cx = left + Math.floor(width / 2), chars = Math.max(8, Math.floor(width / GLYPH));
  const track = current(), streamed = track && !native(track);
  const head = 8 + 12 + 14 + (track ? (streamed ? 24 : 14) : 0) + 26 + 22; // title, by, bar, transport, tabs

  // The list goes down first; the header is painted over whatever scrolls under it.
  const rows = lists[tab];
  listTop = head;
  scrollMax = Math.max(0, rows.length * ROW + 8 - (screen.height - listTop));
  if (notes[tab]) ink(colors.muted).write(notes[tab], { x: cx, y: listTop + 6, center: "x" });
  rows.forEach((t, i) => {
    const y = listTop + i * ROW - Math.round(scroll);
    if (y + ROW < listTop || y > screen.height) return;
    const hot = now?.tab === tab && now.i === i;
    if (hot) ink(colors.pink).box(left, y, width, ROW - 2, "fill");
    else if (i === pressedRow) ink(colors.shadow).box(left, y, width, ROW - 2, "fill");
    else ink(colors.panel).box(left, y + ROW - 2, width, 1, "fill");
    const end = t.duration ? clock(t.duration) : t.when || "";
    const room = chars - end.length - 3, title = fit(t.title, room);
    const by = room - title.length - 2 >= 4 ? fit(t.artist || "", room - title.length - 2) : "";
    ink(hot ? colors.ground : colors.text).write(title, { x: left + 4, y: y + 5 });
    if (by) ink(hot ? colors.shadow : colors.muted).write(by, { x: left + 4 + (title.length + 2) * GLYPH, y: y + 5 });
    ink(hot ? colors.ground : colors.muted).write(end, { x: left + width - 4 - end.length * GLYPH, y: y + 5 });
  });
  if (scrollMax > 0) {
    const span = screen.height - listTop - 12, at = Math.max(0, Math.min(1, scroll / scrollMax));
    ink(colors.mint).box(screen.width - 3, listTop + 2 + span * at, 2, 8, "fill");
  }
  ink(colors.ground).box(0, 0, screen.width, listTop, "fill");

  const button = (id, label, x, y, w, h, action, hot = false) => {
    const item = buttons[id] ||= { button: new ui.Button() };
    item.action = action;
    const b = item.button;
    Object.assign(b.box, { x, y, w, h });
    const active = b.down || b.over;
    ink(hot ? colors.pink : active ? colors.shadow : colors.panel).box(b.box, "fill");
    ink(hot ? colors.pink : active ? colors.text : colors.muted).box(b.box, "outline");
    ink(hot ? colors.ground : colors.text).write(label, { x: x + w / 2, y: y + Math.floor((h - 10) / 2), center: "x" });
  };

  // Now playing: title, who made it, a bar you can drag.
  let y = 8;
  const sway = track && playing ? Math.round(Math.sin(frame / 60) * 2) : 0;
  ink(colors.pink).write(track ? fit(track.title, chars) : "juke", { x: cx + sway, y, center: "x" });
  y += 12;
  const by = track ? [track.artist, track.album !== track.title && track.album].filter(Boolean).join(" · ") : "every /pop track, and yours";
  ink(error ? colors.pink : colors.muted).write(fit(error || by, chars), { x: cx, y, center: "x" });
  y += 14;
  rail = null;
  if (streamed) {
    const at = scrubTo ?? elapsed, played = duration ? Math.max(0, Math.min(1, at / duration)) : 0;
    const railX = left + 4, railW = width - 8, knob = railX + Math.round(railW * played);
    rail = { x: railX, y: y - 4, w: railW, h: 14 };
    ink(colors.panel).box(railX, y, railW, 4, "fill");
    ink(colors.mint).box(railX, y, Math.round(railW * played), 4, "fill");
    ink(scrubTo === null ? colors.text : colors.pink).box(knob - 1, y - 3, 3, 10, "fill");
    ink(scrubTo === null ? colors.muted : colors.pink).write(clock(at), { x: railX, y: y + 9 });
    const total = duration ? clock(duration) : "-:--";
    ink(colors.muted).write(total, { x: railX + railW - total.length * GLYPH, y: y + 9 });
    y += 24;
  } else if (track) {
    ink(colors.muted).write("playing in the app's player", { x: cx, y, center: "x" });
    y += 14;
  }
  const third = Math.floor((width - 8) / 3);
  button("prev", "<<", left, y, third, 20, () => step($, -1));
  button("toggle", playing && streamed ? "pause" : "play", left + third + 4, y, third, 20, () => toggle($), !!now);
  button("next", ">>", left + 2 * (third + 4), y, width - 2 * (third + 4), 20, () => step($, 1));
  y += 26;
  const half = Math.floor((width - 4) / 2);
  button("pop", `pop ${lists.pop.length || ""}`.trim(), left, y, half, 16, () => show($, "pop"), tab === "pop");
  button("mine", `${handle || "mine"} ${lists.mine.length || ""}`.trim(), left + half + 4, y, width - half - 4, 16, () => show($, "mine"), tab === "mine");
}

const inside = (e, box) => box && e.x >= box.x && e.x < box.x + box.w && e.y >= box.y && e.y < box.y + box.h;
const rowAt = (e) => Math.floor((e.y - listTop + Math.round(scroll)) / ROW);

function act($) {
  const e = $.event;
  if (e.is("keyboard:down:space")) return toggle($);
  if (e.is("keyboard:down:arrowright")) return step($, 1);
  if (e.is("keyboard:down:arrowleft")) return step($, -1);
  if (e.is("keyboard:down:tab")) return show($, tab === "pop" ? "mine" : "pop");
  if (e.is("keyboard:down:arrowdown")) { velocity = 0; scroll = Math.min(scrollMax, scroll + ROW); }
  if (e.is("keyboard:down:arrowup")) { velocity = 0; scroll = Math.max(0, scroll - ROW); }
  if (e.is("scroll")) { flinging = false; velocity = 0; scroll = Math.max(0, Math.min(scrollMax, scroll - e.y)); }

  const scrubAt = () => duration * Math.max(0, Math.min(1, (e.x - rail.x) / rail.w));
  if (e.is("touch")) {
    flinging = false; velocity = 0;
    const zone = inside(e, rail) && duration ? "rail" : e.y >= listTop ? "list" : null;
    press = zone && { x: e.x, y: e.y, dragged: false, zone };
    if (zone === "rail") scrubTo = scrubAt();
    if (zone === "list") pressedRow = rowAt(e);
  }
  if (e.is("draw") && press?.zone === "rail" && rail) scrubTo = scrubAt();
  if (e.is("draw") && press?.zone === "list") {
    if (!press.dragged && Math.hypot(e.x - press.x, e.y - press.y) > DRAG) { press.dragged = true; pressedRow = -1; }
    if (press.dragged) {
      velocity = velocity * 0.4 - e.delta.y * 0.6; // EMA, so a pause before lifting keeps the fling.
      scroll = Math.max(-OVERSCROLL, Math.min(scrollMax + OVERSCROLL, scroll - e.delta.y));
    }
  }
  if (e.is("lift") && press) {
    if (press.zone === "rail" && scrubTo !== null) { seek($, scrubTo); scrubTo = null; }
    if (press.zone === "list") {
      const i = rowAt(e);
      if (press.dragged) flinging = Math.abs(velocity) > MIN_VELOCITY || scroll < 0 || scroll > scrollMax;
      else if (i >= 0 && i === pressedRow && lists[tab][i]) {
        if (now?.tab === tab && now.i === i) toggle($); else play($, tab, i);
      }
    }
    press = null; pressedRow = -1;
  }
  if (!press) for (const { button, action } of Object.values(buttons))
    button.act(e, { push: () => { action(); $.needsPaint(); } }, $.pens?.());
  $.needsPaint();
}

function sim($) {
  frame++;
  const track = current();
  if (track && !native(track) && playing && frame % 15 === 0) $.send({ type: "stream:time", content: { id: STREAM } });
  if (!flinging) return;
  if (scroll < 0 || scroll > scrollMax) {
    const target = scroll < 0 ? 0 : scrollMax, displacement = scroll - target;
    velocity = (velocity - displacement * STIFFNESS) * DAMPING;
    scroll += velocity;
    const crossed = Math.sign(scroll - target) !== Math.sign(displacement);
    if (crossed || (Math.abs(displacement) < 0.5 && Math.abs(velocity) < MIN_VELOCITY)) { scroll = target; velocity = 0; flinging = false; }
  } else {
    velocity *= FRICTION;
    scroll += velocity;
    if (scroll < 0 || scroll > scrollMax) velocity *= 0.5;
    if (Math.abs(velocity) < MIN_VELOCITY) { velocity = 0; flinging = false; }
  }
  $.needsPaint();
}

function receive({ type, content }, $) {
  if (content?.id !== STREAM) return;
  if (type === "stream:playing") playing = true;
  if (type === "stream:paused" || type === "stream:stopped") playing = false;
  if (type === "stream:error") { playing = false; error = "Couldn't play that one."; }
  if (type === "stream:seeked" && Number.isFinite(content.time)) elapsed = content.time;
  if (type === "stream:time-data") {
    elapsed = content.currentTime || 0;
    if (content.duration) duration = content.duration;
    if (content.ended && playing) step($, 1); // the whole list loops
  }
  $.needsPaint();
}

function leave($) {
  controller?.abort();
  // The web stream ends with the piece; the app's native player plays on.
  $.send({ type: "stream:stop", content: { id: STREAM } });
}

function meta() { return { title: "Juke", desc: "Every /pop track on one jukebox, and your own works in progress." }; }

export { boot, paint, act, sim, receive, leave, meta };
