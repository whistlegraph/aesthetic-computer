// menuband, 2026.9.28
// Your Menu Band takes, backed up from the Mac app to your handle.
// Tap a take to play its mix; tap `save` to download it. Takes are private,
// so `menuband @handle` only opens your own. Admin-only (@jeffrey) for now.

const FONT = "MatrixChunky8";
const ROW = 20; // Two text lines plus a gap.
const TOP = 22; // Clear of the corner label.
const BOTTOM = 12;

let takes = [];
let status = "loading"; // loading | ready | signed-out | private | error
let message = "";
let scroll = 0;
let playing = null; // { code, sfx }
let pendingCode = null; // A mix that is still loading.
let saveBoxes = {}; // code → { x, y, w, h, url } registered with bios last paint.

async function boot({ params, user, handle, authorize, hud }) {
  hud.label("menuband");
  const asked = params[0]?.startsWith("@") ? params[0].toLowerCase() : null;
  const mine = handle()?.toLowerCase();
  if (!user) return void (status = "signed-out");
  if (asked && asked !== mine) {
    status = "private";
    message = `${asked}'s takes are private`;
    return;
  }
  try {
    const token = await authorize();
    const res = await fetch("/api/menuband-takes?limit=200", {
      headers: { Authorization: `Bearer ${token}` },
    });
    const data = await res.json();
    if (res.status === 403) {
      status = "private";
      message = data.error;
      return;
    }
    if (!res.ok) throw new Error(data.error || `HTTP ${res.status}`);
    takes = data.takes || [];
    status = "ready";
  } catch (err) {
    status = "error";
    message = err.message;
  }
}

function when(iso) {
  const d = new Date(iso);
  const month = d.toLocaleString("en-US", { month: "short" });
  const time = d.toLocaleTimeString("en-US", { hour: "numeric", minute: "2-digit" });
  return `${month} ${d.getDate()} ${time}`;
}

const seconds = (s) => `${Math.floor(s / 60)}:${String(Math.round(s % 60)).padStart(2, "0")}`;
const rowY = (i) => TOP + i * ROW - scroll;
const listHeight = (screen) => screen.height - TOP - BOTTOM;

function paint({ wipe, ink, screen, text, mask, unmask, send }) {
  const { width: w, height: h } = screen;
  wipe(14, 12, 22);

  const center = (msg, color) =>
    ink(...color).write(msg, { center: "xy", x: w / 2, y: h / 2 }, false, w - 16, false, FONT);
  if (status !== "ready" || takes.length === 0) {
    syncSaveBoxes(send, {});
    if (status === "loading") center("loading takes...", [150, 150, 180]);
    else if (status === "signed-out") center("log in to see your takes - type login", [220, 200, 150]);
    else if (status === "private") center(message, [220, 200, 150]);
    else if (status === "error") center(`error: ${message.slice(0, 60)}`, [255, 110, 110]);
    else center("no takes yet - record one in Menu Band", [150, 150, 180]);
    return;
  }

  const boxes = {};
  mask({ x: 0, y: TOP, width: w, height: listHeight(screen) });
  takes.forEach((take, i) => {
    const y = rowY(i);
    if (y + ROW < TOP || y > h - BOTTOM) return;
    const live = playing?.code === take.code || pendingCode === take.code;
    if (live) ink(40, 36, 70).box(0, y, w, ROW - 2);
    ink(...(live ? [255, 210, 120] : [230, 220, 255])).write(
      `${live ? "■" : "▶"} ${when(take.recordedAt)}`, { x: 6, y: y + 1 }, false, undefined, false, FONT);
    const detail = [seconds(take.duration || 0), take.machine, take.bpm && `${Math.round(take.bpm)}bpm`]
      .filter(Boolean).join(" · ");
    ink(120, 120, 150).write(detail, { x: 6, y: y + 10 }, false, undefined, false, FONT);

    const file = take.files?.["mix.mp3"] || take.files?.["mix.wav"];
    if (file?.download) {
      const label = "save";
      const bw = text.width(label, FONT) + 8;
      const box = { x: w - bw - 6, y: y + 3, w: bw, h: 12 };
      ink(60, 90, 70).box(box.x, box.y, box.w, box.h);
      ink(170, 240, 190).write(label, { x: box.x + 4, y: box.y + 2 }, false, undefined, false, FONT);
      // Only fully visible buttons can take a tap.
      if (box.y >= TOP && box.y + box.h <= h - BOTTOM) boxes[take.code] = { ...box, url: file.download };
    }
  });
  unmask();

  const count = `${takes.length} take${takes.length === 1 ? "" : "s"}`;
  ink(90, 90, 120).write(count, { x: w - text.width(count, FONT) - 6, y: h - BOTTOM + 2 }, false, undefined, false, FONT);
  syncSaveBoxes(send, boxes);
}

// `save` opens its presigned URL from bios' native pointerup, which keeps
// the user gesture iOS needs for a download (see ableton.mjs).
function syncSaveBoxes(send, boxes) {
  for (const [code, box] of Object.entries(boxes)) {
    const was = saveBoxes[code];
    if (was && was.x === box.x && was.y === box.y && was.w === box.w && was.url === box.url) continue;
    const { url, ...rect } = box;
    send({ type: "button:hitbox:add", content: { label: `menuband-${code}`, box: rect, url, action: "open-url" } });
  }
  for (const code of Object.keys(saveBoxes))
    if (!boxes[code]) send({ type: "button:hitbox:remove", content: `menuband-${code}` });
  saveBoxes = boxes;
}

function stop() {
  playing?.sfx?.kill?.(0.05);
  playing = null;
  pendingCode = null;
}

async function play(take, { net, sound }) {
  const file = take.files?.["mix.mp3"] || take.files?.["mix.wav"];
  if (!file) return;
  const extension = take.files["mix.mp3"] ? "mp3" : "wav";
  const code = take.code;
  pendingCode = code;
  try {
    // Presigned URLs carry dots in their query, so name the type outright.
    const sample = await net.preload({ path: file.url, extension });
    if (pendingCode !== code) return; // Another tap got there first.
    pendingCode = null;
    playing = {
      code,
      sfx: sound.play(sample, undefined, { kill: () => { if (playing?.code === code) playing = null; } }),
    };
  } catch (err) {
    if (pendingCode === code) pendingCode = null;
    console.warn("menuband: couldn't play", code, err);
  }
}

function act({ event: e, screen, net, sound, jump }) {
  if (status !== "ready") {
    if (status === "signed-out" && e.is("keyboard:down:enter")) jump("login");
    return;
  }
  const most = Math.max(0, takes.length * ROW - listHeight(screen));
  if (e.is("draw")) scroll = Math.min(most, Math.max(0, scroll - e.delta.y));
  if (e.is("scroll")) scroll = Math.min(most, Math.max(0, scroll - e.y)); // As chat.mjs.
  if (e.is("touch") && e.y >= TOP && e.y < screen.height - BOTTOM) {
    if (Object.values(saveBoxes).some((b) => e.x >= b.x && e.x < b.x + b.w && e.y >= b.y && e.y < b.y + b.h))
      return; // bios already opened the download.
    const take = takes[Math.floor((e.y - TOP + scroll) / ROW)];
    if (!take) return;
    const again = playing?.code === take.code || pendingCode === take.code;
    stop();
    if (!again) play(take, { net, sound });
  }
  if (e.is("keyboard:down:escape")) jump("prompt");
}

function leave({ send }) {
  stop();
  syncSaveBoxes(send, {});
}

function meta() {
  return { title: "Menu Band", desc: "Your Menu Band takes, backed up to your handle." };
}

export { boot, paint, act, leave, meta };
