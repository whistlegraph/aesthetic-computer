// Juke, 2026.10.19
// A jukebox for every /pop track. The iPhone app hands each song to its
// native player, so the music keeps going on the lock screen and in the
// background; the web streams it here.

import { AestheticIOSApp } from "../lib/platform.mjs";

const CATALOG = "https://pop.aesthetic.computer/releases/catalog.json";
const STREAM = "juke";
const colors = { ground: [22, 18, 34], panel: [44, 36, 66], text: [240, 232, 255],
  pink: [255, 112, 190], mint: [159, 240, 201], muted: [160, 146, 190], shadow: [70, 30, 80] };

let tracks = [], index = null, playing = false, loaded = false, error = "";
let elapsed = 0, duration = 0, wanted = null;
let buttons = {}, scroll = 0, scrollMax = 0, frame = 0, controller;

function boot($) {
  controller?.abort();
  controller = new AbortController();
  buttons = {}; scroll = 0; frame = 0; error = ""; loaded = false;
  wanted = $.params?.[0] || null;
  $.hud?.labelBack();
  const lifetime = controller;
  fetch(CATALOG, { signal: AbortSignal.any([lifetime.signal, AbortSignal.timeout(12000)]) })
    .then(async response => {
      if (!response.ok) throw Error("Catalog unavailable");
      const data = await response.json();
      if (!Array.isArray(data.tracks)) throw Error("Catalog unavailable");
      if (lifetime.signal.aborted) return;
      tracks = data.tracks.filter(t => t.slug && t.audio);
      loaded = true;
      const i = tracks.findIndex(t => t.slug === wanted);
      if (i >= 0) play($, i);
    })
    .catch(() => { if (!lifetime.signal.aborted) { loaded = true; error = "Couldn't load /pop."; } })
    .finally(() => { if (!lifetime.signal.aborted) $.needsPaint(); });
}

function play($, i) {
  if (!tracks[i]) return;
  index = i; elapsed = 0; duration = tracks[i].duration || 0; playing = true;
  if (AestheticIOSApp) {
    // The native player owns playback in the app: lock screen, AirPlay, background.
    $.send({ type: "ios:send", content: { type: "pop:open", body: tracks[i].slug } });
    return;
  }
  $.send({ type: "stream:play", content: { id: STREAM, url: tracks[i].audio, volume: 1 } });
}

function toggle($) {
  if (index === null) return play($, 0);
  if (AestheticIOSApp) return play($, index);
  $.send({ type: playing ? "stream:pause" : "stream:resume", content: { id: STREAM } });
  playing = !playing;
}

const step = ($, by) => tracks.length && play($, (index === null ? 0 : index + by + tracks.length) % tracks.length);

function clock(seconds) {
  const s = Math.max(0, Math.floor(seconds));
  return `${Math.floor(s / 60)}:${String(s % 60).padStart(2, "0")}`;
}

function paint($) {
  const { ink, wipe, screen, ui } = $;
  wipe(colors.ground);
  $.hud?.label(); // The jukebox owns the whole screen; no corner label.
  const width = Math.min(240, screen.width - 16), left = Math.floor((screen.width - width) / 2);
  const cx = left + Math.floor(width / 2);
  const button = (id, label, x, y, w, h, action, hot = false) => {
    const item = buttons[id] ||= { button: new ui.Button() };
    item.action = action;
    const b = item.button;
    Object.assign(b.box, { x, y, w, h });
    b.disabled = y + h < 0 || y > screen.height;
    const active = !b.disabled && (b.down || b.over);
    ink(hot ? colors.pink : active ? colors.shadow : colors.panel).box(b.box, "fill");
    ink(hot ? colors.pink : active ? colors.text : colors.muted).box(b.box, "outline");
    ink(hot ? colors.ground : colors.text).write(label, { x: x + w / 2, y: y + Math.floor((h - 10) / 2), center: "x" });
  };
  const chars = Math.max(8, Math.floor((width - 8) / 6));
  const now = tracks[index];

  // Now playing sits at the top; the list scrolls beneath it.
  let y = 8;
  const sway = Math.round(Math.sin(frame / 60) * 2);
  ink(colors.pink).write(now ? now.title.slice(0, chars) : "juke", { x: cx + sway, y, center: "x" });
  y += 12;
  ink(colors.muted).write(now ? `${now.artist}${now.album ? " · " + now.album : ""}`.slice(0, chars) : "every /pop track",
    { x: cx, y, center: "x" });
  y += 14;
  if (now && !AestheticIOSApp) {
    const barW = width - 8, played = duration ? Math.min(1, elapsed / duration) : 0;
    ink(colors.panel).box(left + 4, y, barW, 4, "fill");
    ink(colors.mint).box(left + 4, y, Math.floor(barW * played), 4, "fill");
    ink(colors.muted).write(`${clock(elapsed)} / ${clock(duration)}`, { x: cx, y: y + 7, center: "x" });
    y += 20;
  } else if (now) {
    ink(colors.muted).write("playing in the app's player", { x: cx, y, center: "x" });
    y += 14;
  }
  const third = Math.floor((width - 8) / 3);
  button("prev", "<<", left, y, third, 20, () => step($, -1));
  button("toggle", index === null ? "play" : playing && !AestheticIOSApp ? "pause" : "play", left + third + 4, y, third, 20, () => toggle($), index !== null);
  button("next", ">>", left + 2 * (third + 4), y, width - 2 * (third + 4), 20, () => step($, 1));
  y += 28;

  const rowH = 16, top = y;
  scrollMax = Math.max(0, top + tracks.length * rowH + 8 - screen.height);
  scroll = Math.max(0, Math.min(scrollMax, scroll));
  if (!loaded) ink(colors.muted).write("loading /pop...", { x: cx, y: top, center: "x" });
  if (error) ink(colors.pink).write(error, { x: cx, y: top, center: "x" });
  tracks.forEach((t, i) => {
    const ry = top + i * rowH - scroll;
    if (ry + rowH < top || ry > screen.height) { if (buttons["t" + i]) buttons["t" + i].button.disabled = true; return; }
    const label = `${i === index ? "> " : ""}${t.title}`.slice(0, chars - 5) + (t.duration ? ` ${clock(t.duration)}` : "");
    button("t" + i, label, left, ry, width, rowH - 2, () => play($, i), i === index);
  });
  if (scrollMax) ink(colors.mint).box(screen.width - 3, top + (screen.height - top - 10) * scroll / scrollMax, 1, 8, "fill");
}

function act($) {
  const e = $.event;
  if (e.is("keyboard:down:space")) return toggle($);
  if (e.is("keyboard:down:arrowright")) return step($, 1);
  if (e.is("keyboard:down:arrowleft")) return step($, -1);
  if (e.is("scroll")) scroll = Math.max(0, Math.min(scrollMax, scroll - e.y));
  if (e.is("draw:1") && !Object.values(buttons).some(item => item.button.down))
    scroll = Math.max(0, Math.min(scrollMax, scroll - e.delta.y));
  for (const { button, action } of Object.values(buttons))
    button.act(e, { push: () => { action(); $.needsPaint(); } }, $.pens?.());
  $.needsPaint();
}

function sim($) {
  frame++;
  if (!AestheticIOSApp && playing && frame % 15 === 0) $.send({ type: "stream:time", content: { id: STREAM } });
}

function receive({ type, content }, $) {
  if (content?.id !== STREAM) return;
  if (type === "stream:playing") playing = true;
  if (type === "stream:paused" || type === "stream:stopped") playing = false;
  if (type === "stream:error") { playing = false; error = "Couldn't play that one."; }
  if (type === "stream:time-data") {
    elapsed = content.currentTime || 0;
    if (content.duration) duration = content.duration;
    if (content.ended && playing) step($, 1); // the whole catalog loops
  }
  $.needsPaint();
}

function leave($) {
  controller?.abort();
  // The web stream ends with the piece; the app's native player plays on.
  if (!AestheticIOSApp) $.send({ type: "stream:stop", content: { id: STREAM } });
}

function meta() { return { title: "Juke", desc: "Every /pop track on one jukebox." }; }

export { boot, paint, act, sim, receive, leave, meta };
