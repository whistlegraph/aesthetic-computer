// Sack, 2026.11.18
// A sack is a shareable directory a handle owns. Drag files in: they upload
// under your handle, never expire, and anyone with the link can download them
// one by one or as one zip.
//
//   sack        make a sack, see your sacks and how much space they use
//   sack~code   open a sack (and add to it, if it is yours)
//
// Everything you see is drawn here. Files are megabytes to gigabytes and must
// go from the browser straight to storage, so `../sack/ui.mjs` runs on the
// main thread with no look of its own: it picks files, uploads them in parts
// and plays audio, and signals its state back. The one DOM element is an
// invisible file picker laid exactly over the drop zone drawn below, because
// a file dialog only opens from a real tap.

const SMALL = "MatrixChunky8";
const TOP = 22; // under the corner label
const PAD = 6;
const COLUMN = 360; // widest the list gets on a big screen
const ROW = 24;
const ZONE_H = 42;
const BG = [24, 18, 12];
const ORANGE = [255, 170, 90], PALE = [255, 217, 168], DIM = [168, 148, 124], RULE = [58, 44, 28], RED = [255, 138, 122];
const { floor, round, max, min, abs } = Math;

let state; // the last "sack:state" from ui.mjs
let code, scroll = 0, drag, hits = [], contentH = 0;
let armed; // the id of a remove/delete awaiting its second tap
let copied = 0; // frames left to say "copied"
let zoneSent;
let signal; // piece → ui.mjs; the runtime hands it to sim, not paint

export function meta({ params }) {
  return {
    title: params[0] ? `sack ${params[0]} - aesthetic.computer` : "sack - aesthetic.computer",
    desc: "A shareable directory of files that do not expire.",
  };
}

export function boot({ params, dom: { html } }) {
  code = /^[a-z0-9]{1,12}$/i.test(params[0] || "") ? params[0] : "";
  state = undefined; scroll = 0; hits = []; drag = armed = zoneSent = signal = undefined; copied = 0;
  html`
    <div id="sack" data-code="${code}"></div>
    <script src="/aesthetic.computer/sack/ui.mjs"></script>
  `;
}

export function paint({ wipe, ink, screen, text, pen, hud, net }) {
  wipe(...BG);
  hits = [];
  const W = screen.width;
  const colW = min(COLUMN, W - PAD * 2), X = floor((W - colW) / 2);
  let y = TOP + scroll;
  let zone = null;
  const small = (str, x, yy, color = DIM) => ink(color).write(str, { x, y: yy }, undefined, undefined, false, SMALL);
  const hit = (x, yy, w, h, go) => hits.push({ x, y: yy, w, h, go });
  const over = (x, yy, w, h) => pen && pen.x >= x && pen.x < x + w && pen.y >= yy && pen.y < yy + h;
  const bar = (yy, part, color = ORANGE) => {
    ink(RULE).box(X, yy, colW, 2);
    if (part > 0) ink(color).box(X, yy, max(1, round(colW * min(1, part))), 2);
  };
  const button = (label, x, yy, go, color = ORANGE) => {
    const w = text.width(label) + 8;
    ink(over(x, yy, w, 14) ? PALE : color).box(x, yy, w, 14);
    ink(BG).write(label, { x: x + 4, y: yy + 2 });
    hit(x, yy, w, 14, go);
    return w + 4;
  };
  // A small word you tap; the second tap on a dangerous one does it.
  const word = (id, label, x, yy, go, danger) => {
    const sure = danger && armed === id, str = sure ? "sure?" : label;
    const w = text.width(str, SMALL) + 4;
    small(str, x, yy, sure || (danger && over(x - 2, yy - 2, w, 12)) ? RED : over(x - 2, yy - 2, w, 12) ? PALE : DIM);
    hit(x - 2, yy - 3, w + 2, 13, (...tools) => { if (danger && armed !== id) { armed = id; return; } armed = undefined; go(...tools); });
    return w + 6;
  };
  const drawZone = (title) => {
    const on = state?.over;
    ink(...ORANGE, on ? 40 : 10).box(X, y, colW, ZONE_H);
    const edge = on ? ORANGE : DIM;
    for (let x = X; x < X + colW; x += 6) { ink(edge).box(x, y, 3, 1); ink(edge).box(x, y + ZONE_H - 1, 3, 1); }
    for (let yy = y; yy < y + ZONE_H; yy += 6) { ink(edge).box(X, yy, 1, 3); ink(edge).box(X + colW - 1, yy, 1, 3); }
    ink(ORANGE).write(title, { x: X + floor((colW - text.width(title)) / 2), y: y + 10 });
    const sub = "drag files here or tap";
    small(sub, X + floor((colW - text.width(sub, SMALL)) / 2), y + 25);
    zone = { x: X, y, w: colW, h: ZONE_H };
    y += ZONE_H + 8;
  };

  const sack = state?.sack, mine = state?.mine;
  // Once a drop has made a sack, this page is that sack.
  if (state?.code && state.code !== code) {
    code = state.code;
    hud.label("sack " + code, "white");
    net.rewrite("sack~" + code);
  }

  if (!state) {
    small("opening...", X, y); y += 14;
  } else if (code && state.missing) {
    ink(ORANGE).write("no such sack", { x: X, y }); y += 14;
    small("its owner may have removed it", X, y); y += 14;
  } else if (code && sack) {
    // Name, as large as fits.
    const size = text.width(sack.name) * 2 <= colW ? 2 : 1;
    if (size === 2) { ink(ORANGE).write(sack.name, { x: X, y, size }); y += 22; }
    else {
      const h = text.box(sack.name, { x: X, y }, colW, 1, true)?.box?.height || 10;
      ink(ORANGE).write(sack.name, { x: X, y }, undefined, colW, true);
      y += h + 3;
    }
    let x = X;
    if (sack.handle) {
      small(sack.handle, x, y, over(x, y - 2, text.width(sack.handle, SMALL), 11) ? PALE : ORANGE);
      const w = text.width(sack.handle, SMALL);
      hit(x, y - 3, w, 13, (jump) => jump(sack.handle));
      x += w + 4;
    }
    small(`${sack.files.length} file${sack.files.length === 1 ? "" : "s"}  ${bytes(sack.bytes)}`, x, y);
    y += 13;
    x = X;
    if (sack.files.length) x += button("zip", x, y, () => signal({ type: "sack:get", content: { url: sack.zip } }));
    x += button(copied > 0 ? "copied" : "copy link", x, y, () => { signal({ type: "sack:copy", content: {} }); copied = 90; });
    if (copied > 0) copied--;
    y += 20;

    for (const file of sack.files) {
      if (y + ROW > 0 && y < screen.height) {
        const playing = state.playing === file.id;
        const name = fit(text, file.name, colW);
        ink(over(X, y, colW, 11) ? 255 : PALE).write(name, { x: X, y });
        hit(X, y - 1, colW, 12, () => signal({ type: "sack:get", content: { url: file.download } }));
        let wx = X;
        small(bytes(file.bytes), wx, y + 12); wx += text.width(bytes(file.bytes), SMALL) + 8;
        if (file.inline && /^audio\//.test(file.type))
          wx += word("p" + file.id, playing ? "stop" : "play", wx, y + 12, () => signal({ type: "sack:play", content: { id: file.id } }));
        else if (file.inline)
          wx += word("v" + file.id, "view", wx, y + 12, (jump) => jump("out:" + new URL(file.inline, state.origin).href));
        if (state.yours)
          word("r" + file.id, "remove", wx, y + 12, () => signal({ type: "sack:remove", content: { id: file.id } }), true);
        if (playing) ink(ORANGE).box(X, y + 21, colW, 1);
        else ink(RULE).box(X, y + 21, colW, 1);
      }
      y += ROW;
    }
    y += 6;
    if (state.yours) drawZone("add to this sack");
  } else if (code && !sack) {
    if (!state.uploads.length) { small("opening...", X, y); y += 14; }
  } else if (!code) {
    small("a shareable directory of files", X, y); y += 10;
    small("they do not expire", X, y); y += 16;
    if (state.loaded && !mine) {
      button("log in to keep a sack", X, y, (jump, store) => { store["prompt:splash"] = true; jump("prompt"); });
      y += 22;
    } else if (mine) drawZone("make a sack");
  }

  // Uploads in flight.
  for (const up of state?.uploads || []) {
    const part = up.bytes ? up.sent / up.bytes : 0;
    ink(PALE).write(fit(text, up.name, colW), { x: X, y });
    const says = { waiting: "waiting", sending: `${floor(part * 100)}% of ${bytes(up.bytes)}`, saving: "saving", done: "kept",
      failed: "failed: " + (up.error || "") }[up.status];
    small(fit(text, says, colW, SMALL), X, y + 12, up.status === "failed" ? RED : DIM);
    bar(y + 21, up.status === "failed" ? 0 : part);
    y += ROW + 2;
  }
  if (state?.note) {
    const h = text.box(state.note, { x: X, y }, colW, 1, true)?.box?.height || 10;
    ink(PALE).write(state.note, { x: X, y }, undefined, colW, true);
    y += h + 6;
  }

  // Your space, and your other sacks.
  if (mine) {
    y += 6;
    small(`${mine.handle} uses ${bytes(mine.used)} of ${bytes(mine.limit)}`, X, y); y += 11;
    bar(y, mine.used / mine.limit); y += 8;
    if (!code || state.yours) for (const each of mine.sacks) {
      if (!each.files || each.code === code) continue;
      if (y + ROW > 0 && y < screen.height) {
        ink(over(X, y, colW, ROW - 2) ? 255 : PALE).write(fit(text, each.name, colW), { x: X, y });
        small(`${each.files} file${each.files === 1 ? "" : "s"}  ${bytes(each.bytes)}`, X, y + 12);
        ink(RULE).box(X, y + 21, colW, 1);
        hit(X, y - 1, colW, ROW - 1, (jump) => jump("sack~" + each.code));
      }
      y += ROW;
    }
    if (code && state.yours) {
      y += 8;
      word("sack", "delete this sack", X, y, () => signal({ type: "sack:delete", content: {} }), true);
      y += 14;
    }
  }
  contentH = y - scroll + PAD;

  // Keep the invisible picker on the zone (or off the page when there is none).
  const visible = zone && zone.y + zone.h > TOP && zone.y < screen.height ? zone : null;
  const sending = JSON.stringify([visible, W, state?.seq === undefined]);
  if (state && signal && sending !== zoneSent) {
    zoneSent = sending;
    signal({ type: "sack:zone", content: { zone: visible, width: W } });
  }
}

export function sim({ signal: send }) { signal = send; }

export function act({ event: e, jump, store, screen }) {
  if (e.is("signal")) {
    for (const s of e.signal) {
      if (s?.type !== "sack:state") continue;
      if (!state) zoneSent = undefined; // ui.mjs has only now started listening
      state = s;
      if (s.gone) jump("sack");
    }
    return;
  }
  if (e.is("touch")) drag = { moved: 0 };
  if (e.is("draw") && drag) {
    scroll += e.delta?.y || 0;
    drag.moved += abs(e.delta?.y || 0) + abs(e.delta?.x || 0);
    clampScroll(screen);
  }
  if (e.is("lift")) {
    if (drag && drag.moved < 4) {
      const h = hits.find((h) => e.x >= h.x && e.x < h.x + h.w && e.y >= h.y && e.y < h.y + h.h);
      if (h) h.go(jump, store); else armed = undefined;
    }
    drag = undefined;
  }
  if (e.is("scroll")) { scroll -= e.y || 0; clampScroll(screen); }
  if (e.is("keyboard:down:arrowdown")) { scroll -= 24; clampScroll(screen); }
  if (e.is("keyboard:down:arrowup")) { scroll += 24; clampScroll(screen); }
  if (e.is("reframed")) clampScroll(screen);
}

function clampScroll(screen) {
  scroll = min(0, max(scroll, min(0, screen.height - contentH)));
}

// Shorten to a width, keeping the end: that is where a file's extension is.
function fit(text, str, w, font) {
  if (text.width(str, font) <= w) return str;
  const tail = str.slice(-7);
  let head = str.slice(0, -7);
  while (head.length > 1 && text.width(head + ".." + tail, font) > w) head = head.slice(0, -1);
  return head + ".." + tail;
}

// 1.9 GB, 124 MB, 12 KB: three figures at most.
function bytes(n) {
  const units = ["B", "KB", "MB", "GB", "TB"];
  let u = 0;
  while (n >= 1000 && u < units.length - 1) { n /= 1000; u++; }
  return (n >= 100 || u === 0 ? round(n) : n.toFixed(1).replace(/\.0$/, "")) + " " + units[u];
}
