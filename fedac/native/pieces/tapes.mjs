// tapes.mjs — browse + play MP4 tapes recorded on this device.
//
// Tapes are recorded by pressing PrintScreen anywhere in notepat (or
// any piece that doesn't eat the key). The PrtSc handler in
// fedac/native/src/ac-native.c drives the existing libavcodec
// recorder (recorder.c) to produce an MP4 at
// /mnt/tapes/YYYY.MM.DD.HH.MM.SS.mmm.mp4, then fires an async curl
// upload to the cloud via /api/presigned-upload-url/mp4/<slug>/user
// + /api/track-tape (ext=mp4), which extends the existing
// MongoDB `tapes` collection with a kind="mp4" + source="ac-native"
// record. Web AC tapes (ZIP) and ac-native tapes (MP4) coexist in
// the same collection — the extension + kind field disambiguates.
//
// This piece is the local browser for them. It lists /mnt/tapes/
// with timestamp, duration (from filesystem), and a simple
// keyboard-driven UI to play, delete, or jump out. Cloud browsing
// (handle-scoped listing via the tapes collection API) is a Phase 3
// addition — for now, anything that was uploaded is still viewable
// here locally since the MP4 lives on disk until you delete it.

let tapes = [];          // [{ name, path, size, modified }]
let selection = 0;       // index in tapes
let message = "";
let messageFrame = 0;
let frame = 0;

function fmtBytes(n) {
  if (!Number.isFinite(n)) return "?";
  if (n < 1024) return `${n}B`;
  if (n < 1024 * 1024) return `${(n / 1024).toFixed(1)}K`;
  return `${(n / 1024 / 1024).toFixed(1)}M`;
}

function msg(t) { message = t; messageFrame = frame; }

function scan(system) {
  tapes = [];
  const listing = system?.listDir?.("/mnt/tapes");
  if (listing) {
    for (const f of listing) {
      if (f.name.endsWith(".mp4") && !f.name.startsWith(".")) {
        tapes.push({
          name: f.name,
          path: "/mnt/tapes/" + f.name,
          size: f.size || 0,
        });
      }
    }
    // Newest first (filenames are timestamp-prefixed so lexicographic works)
    tapes.sort((a, b) => b.name.localeCompare(a.name));
  }
  if (selection >= tapes.length) selection = Math.max(0, tapes.length - 1);
  msg(tapes.length > 0 ? `${tapes.length} tapes` : "no tapes — press PrtSc in notepat");
}

function boot({ system, sound }) {
  scan(system);
  sound?.speak?.(tapes.length > 0 ? `${tapes.length} tapes` : "no tapes yet");
}

function act({ event: e, system, sound }) {
  if (e.is("keyboard:down")) {
    const key = e.key?.toLowerCase();
    if (!key) return;
    if (key === "arrowup" || key === "k") {
      if (tapes.length > 0) {
        selection = (selection - 1 + tapes.length) % tapes.length;
        msg(tapes[selection].name);
      }
      return;
    }
    if (key === "arrowdown" || key === "j") {
      if (tapes.length > 0) {
        selection = (selection + 1) % tapes.length;
        msg(tapes[selection].name);
      }
      return;
    }
    if (key === "r") {
      scan(system);
      return;
    }
    if (key === "enter" || key === " " || key === "space") {
      // Play via deck 0 — same mechanism the dj piece uses.
      if (tapes.length > 0) {
        const t = tapes[selection];
        const ok = sound?.deck?.load?.(0, t.path);
        if (ok) {
          sound.deck.play(0);
          msg("playing " + t.name);
          sound?.speak?.("playing tape");
        } else {
          msg("failed to play " + t.name);
        }
      }
      return;
    }
    if (key === "delete" || key === "backspace") {
      if (tapes.length > 0) {
        const t = tapes[selection];
        if (system?.deleteFile?.(t.path)) {
          msg("deleted " + t.name);
          scan(system);
        } else {
          msg("couldn't delete");
        }
      }
      return;
    }
    if (key === "escape") {
      system?.jump?.("prompt");
      return;
    }
  }
}

function paint({ wipe, ink, box, line, write, screen }) {
  const w = screen.width;
  const h = screen.height;
  wipe(16, 14, 20);

  // Title
  ink(200, 220, 255);
  write("tapes", { x: 8, y: 8, size: 2, font: "matrix" });
  ink(120, 140, 170);
  write("/mnt/tapes  —  ↑↓ nav  enter play  r rescan  del delete  esc prompt",
        { x: 80, y: 14, size: 1, font: "font_1" });

  // Tape list
  const listY = 40;
  const rowH = 14;
  const visibleRows = Math.max(1, Math.floor((h - listY - 20) / rowH));
  const scrollStart = Math.max(0, selection - Math.floor(visibleRows / 2));
  const scrollEnd = Math.min(tapes.length, scrollStart + visibleRows);

  if (tapes.length === 0) {
    ink(180, 180, 200);
    write("No tapes recorded yet.", { x: 20, y: listY + 8, size: 1, font: "font_1" });
    ink(120, 140, 170);
    write("Press PrintScreen in notepat to record a tape.",
          { x: 20, y: listY + 24, size: 1, font: "font_1" });
  } else {
    for (let i = scrollStart; i < scrollEnd; i++) {
      const t = tapes[i];
      const y = listY + (i - scrollStart) * rowH;
      if (i === selection) {
        ink(40, 60, 100, 200);
        box(4, y - 2, w - 8, rowH, true);
        ink(255, 255, 255);
      } else {
        ink(200, 220, 240);
      }
      write(t.name, { x: 10, y: y + 2, size: 1, font: "font_1" });
      ink(140, 160, 190);
      const sizeText = fmtBytes(t.size);
      const sizeX = w - sizeText.length * 6 - 10;
      write(sizeText, { x: sizeX, y: y + 2, size: 1, font: "font_1" });
    }
  }

  // Message line
  if (message && frame - messageFrame < 240) {
    const age = frame - messageFrame;
    const a = Math.max(80, 255 - Math.floor(age * 0.6));
    ink(255, 220, 140, a);
    write(message, { x: 8, y: h - 14, size: 1, font: "font_1" });
  }
}

function sim() {
  frame++;
}

export { boot, act, paint, sim };
