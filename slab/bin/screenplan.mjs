#!/usr/bin/env node
// screenplan.mjs — rectangle-packing window placement for remote test rigs.
//
// The slab AXTiler idea, generalized over ssh: ask a machine for every
// visible window's frame (System Events; needs Accessibility), add fixed
// no-go zones (menu bar, desktop badge corner, dock strip), then first-fit
// scan free space for each requested rect. Prints JSON placements; callers
// (puppet windows, Stickies moves) apply them.
//
//   screenplan.mjs <ssh-host> --want 1200x1000,1200x1000 [--badge TL|TR]
//                  [--ignore "Google Chrome"] [--margin 16] [--grid 24]
//
// `--ignore` drops an app's windows from the occupied set (e.g. when the
// caller is about to move/replace its own Chrome windows). Stickies are
// always considered occupied unless ignored. PUBLIC repo: no machine names.

import { execFileSync } from "node:child_process";

const args = process.argv.slice(2);
const host = args[0];
const flag = name => {
  const i = args.indexOf(`--${name}`);
  return i >= 0 ? args[i + 1] : undefined;
};
if (!host || !flag("want")) {
  console.error("usage: screenplan.mjs <ssh-host> --want WxH[,WxH...] [--badge TL|TR] [--ignore App] [--margin 16] [--grid 24]");
  process.exit(1);
}
const want = flag("want").split(",").map(s => s.split("x").map(Number));
const badge = flag("badge") ?? "TL";
const ignore = (flag("ignore") ?? "").split(",").filter(Boolean);
const MARGIN = Number(flag("margin") ?? 16);
const GRID = Number(flag("grid") ?? 24);

// ── occupied rects from the machine ────────────────────────────────────
const SCRIPT = `
set out to ""
tell application "Finder" to set sb to bounds of window of desktop
set out to out & "SCREEN|" & (item 3 of sb) & "|" & (item 4 of sb) & linefeed
tell application "System Events"
  repeat with proc in (every process whose visible is true and background only is false)
    set pname to name of proc
    try
      repeat with w in (every window of proc)
        try
          set p to position of w
          set s to size of w
          set out to out & pname & "|" & (item 1 of p) & "|" & (item 2 of p) & "|" & (item 1 of s) & "|" & (item 2 of s) & linefeed
        end try
      end repeat
    end try
  end repeat
end tell
return out`;

const raw = execFileSync("ssh", ["-o", "ConnectTimeout=10", host, "osascript", "-"], {
  encoding: "utf8",
  timeout: 30000,
  input: SCRIPT,
});

let scrW = 1440, scrH = 900;
const occupied = [];
for (const line of raw.split("\n")) {
  const parts = line.trim().split("|");
  if (parts[0] === "SCREEN") {
    scrW = Number(parts[1]);
    scrH = Number(parts[2]);
  } else if (parts.length === 5) {
    const [app, x, y, w, h] = [parts[0], ...parts.slice(1).map(Number)];
    if (ignore.includes(app)) continue;
    if (w < 40 || h < 40) continue; // ignore slivers
    occupied.push({ app, x, y, w, h });
  }
}

// fixed no-go zones: menu bar, badge corner, dock strip
occupied.push({ app: "_menubar", x: 0, y: 0, w: scrW, h: 28 });
occupied.push(
  badge === "TR"
    ? { app: "_badge", x: scrW - 300, y: 0, w: 300, h: 220 }
    : { app: "_badge", x: 0, y: 0, w: 300, h: 220 },
);
occupied.push({ app: "_dock", x: 0, y: scrH - 70, w: scrW, h: 70 });

const pad = r => ({ x: r.x - MARGIN, y: r.y - MARGIN, w: r.w + 2 * MARGIN, h: r.h + 2 * MARGIN });
const blocked = occupied.map(pad);
const hits = (x, y, w, h, rects) =>
  rects.some(r => x < r.x + r.w && x + w > r.x && y < r.y + r.h && y + h > r.y);

// ── first-fit raster scan, top-left bias ────────────────────────────────
const placed = [];
for (const [w, h] of want) {
  let spot = null;
  for (let y = 28; y <= scrH - h && !spot; y += GRID) {
    for (let x = 0; x <= scrW - w && !spot; x += GRID) {
      if (!hits(x, y, w, h, blocked) && !hits(x, y, w, h, placed.map(pad))) spot = { x, y };
    }
  }
  // fall back to shrinking 10% steps if nothing fits
  let sw = w, sh = h;
  while (!spot && sw > w * 0.5) {
    sw = Math.round(sw * 0.9);
    sh = Math.round(sh * 0.9);
    for (let y = 28; y <= scrH - sh && !spot; y += GRID) {
      for (let x = 0; x <= scrW - sw && !spot; x += GRID) {
        if (!hits(x, y, sw, sh, blocked) && !hits(x, y, sw, sh, placed.map(pad))) spot = { x, y };
      }
    }
  }
  placed.push(spot ? { x: spot.x, y: spot.y, w: sw, h: sh } : null);
}

console.log(
  JSON.stringify(
    { screen: { w: scrW, h: scrH }, occupied: occupied.filter(o => !o.app.startsWith("_")).map(o => `${o.app} ${o.x},${o.y} ${o.w}x${o.h}`), placements: placed },
    null,
    2,
  ),
);
