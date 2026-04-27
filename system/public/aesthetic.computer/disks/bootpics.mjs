// bootpics, 2026.04.27
// Browse the webcam snapshots ac-native captured at boot + shutdown.
//
// On AC Native OS the bootpic capture runs once at boot and once at
// shutdown (see fedac/native/src/ac-native.c — bootpic_capture_to).
// PGM (Portable Gray Map) files land in /mnt/bootpics/ named
// "<prefix>-<unix_ts>.pgm" where prefix is "boot" or "off".
//
// This piece lists them, decodes the header, and stamps a downsampled
// thumbnail of each via the plot() primitive. Tap a thumbnail to view
// it full-size.

const PICS_DIR = "/mnt/bootpics";
const THUMB_W = 64;
const THUMB_H = 48;
const GUTTER = 4;

let pics = [];
let loadError = null;
let selected = null; // index into pics when a pic is enlarged
let needsLayout = true;
let cols = 1;
let scrollOffset = 0;

// PGM decoder. Format:
//   "P5\n<w> <h>\n255\n<w*h bytes>"
// Tolerates "P5 " / extra whitespace / comment lines starting with '#'.
// Returns { w, h, pixels: Uint8Array } or null on parse failure.
function decodePgm(buffer) {
  if (!buffer || buffer.byteLength < 8) return null;
  const bytes = new Uint8Array(buffer);
  if (bytes[0] !== 0x50 || bytes[1] !== 0x35) return null; // 'P5'
  let i = 2;
  // Skip a single whitespace after the magic.
  while (i < bytes.length && (bytes[i] === 0x0a || bytes[i] === 0x20 || bytes[i] === 0x09)) i++;

  function readToken() {
    // Skip whitespace + comments.
    while (i < bytes.length) {
      const b = bytes[i];
      if (b === 0x23) { // '#' — eat to end of line
        while (i < bytes.length && bytes[i] !== 0x0a) i++;
        continue;
      }
      if (b === 0x20 || b === 0x09 || b === 0x0a || b === 0x0d) { i++; continue; }
      break;
    }
    const start = i;
    while (i < bytes.length) {
      const b = bytes[i];
      if (b === 0x20 || b === 0x09 || b === 0x0a || b === 0x0d) break;
      i++;
    }
    if (i === start) return null;
    let s = "";
    for (let j = start; j < i; j++) s += String.fromCharCode(bytes[j]);
    return s;
  }

  const wTok = readToken();
  const hTok = readToken();
  const maxTok = readToken();
  if (!wTok || !hTok || !maxTok) return null;
  const w = parseInt(wTok, 10);
  const h = parseInt(hTok, 10);
  const maxv = parseInt(maxTok, 10);
  if (!w || !h || maxv !== 255) return null;
  // One whitespace byte after maxval.
  if (i < bytes.length && (bytes[i] === 0x0a || bytes[i] === 0x20 || bytes[i] === 0x09 || bytes[i] === 0x0d)) i++;
  const expected = w * h;
  if (bytes.length - i < expected) return null;
  const pixels = bytes.subarray(i, i + expected);
  return { w, h, pixels };
}

// Sub-sample pixels to a target thumbnail size. Nearest-neighbour —
// we're stamping single pixels via plot() so quality isn't a goal,
// just legibility.
function thumbnail(pgm, tw, th) {
  const out = new Uint8Array(tw * th);
  for (let ty = 0; ty < th; ty++) {
    const sy = Math.floor((ty * pgm.h) / th);
    for (let tx = 0; tx < tw; tx++) {
      const sx = Math.floor((tx * pgm.w) / tw);
      out[ty * tw + tx] = pgm.pixels[sy * pgm.w + sx];
    }
  }
  return { w: tw, h: th, pixels: out };
}

function parseTimestamp(name) {
  // "boot-1714253849.pgm" / "off-1714256000.pgm"
  const m = /^(boot|off)-(\d+)\.pgm$/.exec(name);
  if (!m) return null;
  return { kind: m[1], ts: parseInt(m[2], 10) };
}

function formatTime(ts) {
  if (!ts) return "";
  const d = new Date(ts * 1000);
  const mo = String(d.getMonth() + 1).padStart(2, "0");
  const da = String(d.getDate()).padStart(2, "0");
  const hh = String(d.getHours()).padStart(2, "0");
  const mm = String(d.getMinutes()).padStart(2, "0");
  return `${mo}/${da} ${hh}:${mm}`;
}

function loadPics({ system }) {
  pics = [];
  loadError = null;
  if (!system?.listDir || !system?.readFileBytes) {
    loadError = "this piece needs ac-native (system.listDir + readFileBytes)";
    return;
  }
  const entries = system.listDir(PICS_DIR);
  if (!entries) {
    loadError = `${PICS_DIR} not found — bootpics will appear after the next boot`;
    return;
  }
  // Newest first.
  const items = entries
    .filter((e) => !e.isDir && e.name.endsWith(".pgm"))
    .map((e) => ({ name: e.name, size: e.size, meta: parseTimestamp(e.name) }))
    .filter((e) => e.meta)
    .sort((a, b) => b.meta.ts - a.meta.ts);

  for (const it of items) {
    const buf = system.readFileBytes(`${PICS_DIR}/${it.name}`);
    if (!buf) continue;
    const pgm = decodePgm(buf);
    if (!pgm) continue;
    const thumb = thumbnail(pgm, THUMB_W, THUMB_H);
    pics.push({
      name: it.name,
      kind: it.meta.kind,
      ts: it.meta.ts,
      pgm,
      thumb,
    });
  }
}

function boot({ system }) {
  loadPics({ system });
}

function paint({ wipe, ink, write, screen, plot, line, box }) {
  wipe(0, 0, 0);

  if (loadError) {
    ink(255, 200, 80).write(loadError, { x: 8, y: 12 });
    return;
  }
  if (pics.length === 0) {
    ink(180, 180, 180).write("no bootpics yet", { x: 8, y: 12 });
    ink(120, 120, 120).write(
      "AC Native OS captures one on boot",
      { x: 8, y: 24 },
    );
    ink(120, 120, 120).write(
      "and one on shutdown if a webcam exists",
      { x: 8, y: 32 },
    );
    return;
  }

  // Enlarged single-pic view.
  if (selected != null && pics[selected]) {
    const p = pics[selected];
    const fit = Math.min(
      Math.floor(screen.width / p.pgm.w),
      Math.floor((screen.height - 24) / p.pgm.h),
    );
    const scale = Math.max(1, fit);
    const dw = p.pgm.w * scale;
    const dh = p.pgm.h * scale;
    const ox = Math.floor((screen.width - dw) / 2);
    const oy = Math.floor((screen.height - dh) / 2);
    for (let y = 0; y < p.pgm.h; y++) {
      for (let x = 0; x < p.pgm.w; x++) {
        const v = p.pgm.pixels[y * p.pgm.w + x];
        ink(v, v, v);
        if (scale === 1) {
          plot(ox + x, oy + y);
        } else {
          box(ox + x * scale, oy + y * scale, scale, scale);
        }
      }
    }
    const label = `${p.kind} · ${formatTime(p.ts)}`;
    const labelColor = p.kind === "boot" ? [120, 220, 255] : [255, 160, 120];
    ink(...labelColor).write(label, { x: 6, y: screen.height - 12 });
    ink(140, 140, 140).write("tap to close", {
      x: screen.width - 80,
      y: screen.height - 12,
    });
    return;
  }

  // Grid view.
  if (needsLayout) {
    cols = Math.max(1, Math.floor((screen.width - GUTTER) / (THUMB_W + GUTTER)));
    needsLayout = false;
  }
  const rowH = THUMB_H + 16; // thumbnail + caption
  ink(180, 220, 255).write(
    `${pics.length} bootpic${pics.length === 1 ? "" : "s"}`,
    { x: 6, y: 4 },
  );

  let idx = 0;
  for (let row = 0; idx < pics.length; row++) {
    for (let col = 0; col < cols && idx < pics.length; col++, idx++) {
      const p = pics[idx];
      const x = GUTTER + col * (THUMB_W + GUTTER);
      const y = 16 + row * rowH - scrollOffset;
      if (y + rowH < 0 || y > screen.height) continue;

      // Thumbnail
      for (let ty = 0; ty < p.thumb.h; ty++) {
        for (let tx = 0; tx < p.thumb.w; tx++) {
          const v = p.thumb.pixels[ty * p.thumb.w + tx];
          ink(v, v, v).plot(x + tx, y + ty);
        }
      }
      // Frame
      ink(p.kind === "boot" ? [60, 110, 140] : [140, 80, 60]).box(
        x - 1,
        y - 1,
        THUMB_W + 2,
        THUMB_H + 2,
        "outline",
      );
      // Caption
      ink(p.kind === "boot" ? [120, 220, 255] : [255, 160, 120]).write(
        `${p.kind}`,
        { x, y: y + THUMB_H + 2 },
      );
      ink(150, 150, 150).write(formatTime(p.ts), {
        x,
        y: y + THUMB_H + 10,
      });
    }
  }
}

function act({ event: e, screen }) {
  if (e.is("touch") || (e.is("keyboard:down") && !e.repeat)) {
    if (selected != null) {
      // Any tap or keypress closes the enlarged view.
      selected = null;
      return;
    }
  }

  // Tap-to-open in grid view.
  if (e.is("lift") && selected == null) {
    if (typeof e.x !== "number" || typeof e.y !== "number") return;
    const rowH = THUMB_H + 16;
    const baseY = 16 - scrollOffset;
    const row = Math.floor((e.y - baseY) / rowH);
    if (row < 0) return;
    const col = Math.floor((e.x - GUTTER) / (THUMB_W + GUTTER));
    if (col < 0 || col >= cols) return;
    // Verify within the actual thumb rect (not the gap).
    const ix = GUTTER + col * (THUMB_W + GUTTER);
    const iy = baseY + row * rowH;
    if (e.x >= ix && e.x < ix + THUMB_W && e.y >= iy && e.y < iy + THUMB_H) {
      const idx = row * cols + col;
      if (idx < pics.length) selected = idx;
    }
  }

  // Scroll wheel / arrow-down.
  if (e.is("keyboard:down:arrowdown")) scrollOffset = Math.min(scrollOffset + 32, 4096);
  if (e.is("keyboard:down:arrowup")) scrollOffset = Math.max(scrollOffset - 32, 0);
  if (e.is("scroll")) {
    if (typeof e.delta?.y === "number") scrollOffset = Math.max(0, scrollOffset + e.delta.y);
  }
}

function leave() {
  pics = [];
}

export { boot, paint, act, leave };
