#!/usr/bin/env node
// app-store-real.mjs — Mac App Store screenshots from the REAL Menu Band UI.
//
// Drives the app's own headless render modes — `--render-popover`,
// `--render-about`, `--render-jam`, `--render-menubar` (PopoverCapture/
// AboutCapture/JamCapture/MenubarCapture.swift snapshot the actual AppKit view
// trees via cacheDisplay) — then plainly places those true-UI PNGs on a SOLID
// purple desktop with a minimal menu bar (Apple logo in the corner + the real
// piano status item + clock). No marketing typography.
//
// Four shots: menu bar (no popover) / menu bar + popover / About / Jam.
// Output: ~/Desktop/MenuBand-AppStore-Real/*.png (review) AND
//         fastlane/screenshots/en-US/*.png (so `fastlane mac shots` uploads).

import { execFileSync } from "node:child_process";
import { mkdirSync, readFileSync, writeFileSync, existsSync, rmSync, readdirSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { homedir } from "node:os";

const __dirname = dirname(fileURLToPath(import.meta.url));
const ROOT = resolve(__dirname, "..");
const BIN = resolve(ROOT, ".build/debug/MenuBand");
const DESK = resolve(homedir(), "Desktop/MenuBand-AppStore-Real");
const RAW = resolve(DESK, "raw");
const FL = resolve(ROOT, "fastlane/screenshots/en-US");
const CHROME = "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome";
// Lower resolution (1440x900 — a valid Mac App Store size) so the UI fills more
// of the frame and reads larger than it did at 2880x1800.
const W = 1440, H = 900;
const PURPLE = "#d4f1e4";        // solid desktop (light mint)
const BAR_H = 44;
const S = 1.6;                   // surface scale (popover ~730px tall, prominent)

mkdirSync(RAW, { recursive: true });
mkdirSync(FL, { recursive: true });
if (!existsSync(BIN)) { console.error(`build first: (cd ${ROOT} && swift build)`); process.exit(1); }

// ── 1. render the real UI surfaces ──────────────────────────────────────────
const render = (args, out) => {
  execFileSync(BIN, [...args, "--out", resolve(RAW, out), "--scale", "3"], { stdio: "ignore" });
  return out;
};
console.log("rendering real UI surfaces…");
render(["--render-menubar", "--light"], "menubar.png");   // keys at rest, light theme
render(["--render-popover", "--program", "0"], "popover.png");
render(["--render-about"], "about.png");
render(["--render-jam"], "jam.png");

// The ♪ status glyph at the strip's right end renders in the system accent
// (green). For a clean light-mode bar, recolor it to INK. That right slice
// holds ONLY the note (the piano keys end well to its left), so colorizing all
// its non-transparent pixels is safe. (Strip is rendered at --scale 3 = 1074px
// wide; the ♪ lives in the rightmost ~90px.)
{
  const strip = resolve(RAW, "menubar.png");
  execFileSync("magick", [strip, "(", "-clone", "0",
    "-crop", "96x66+978+0", "+repage",
    "-channel", "RGB", "-fill", "#16161a", "-colorize", "100", "+channel", ")",
    "-geometry", "+978+0", "-composite", strip], { stdio: "ignore" });
}

const uri = (f) => `data:image/png;base64,${readFileSync(resolve(RAW, f)).toString("base64")}`;
// Real macOS menu-bar glyphs cropped from the live bar (committed under
// bin/menubar-assets) — black variants for the LIGHT bar that pairs with the
// light piano keys.
const ASSETS = resolve(__dirname, "menubar-assets");
const asset = (f) => `data:image/png;base64,${readFileSync(resolve(ASSETS, f)).toString("base64")}`;
// native point heights of each surface (from the Swift renders), for shared scale
const NATIVE_H = { popover: 456, about: 487, jam: 360 };

// ── 2. the four shots: plainly place the real screens on a purple desktop ────
// `center` floats the window centered below the bar; `drop` anchors it under
// the top-right status item like the live popover.
const SHOTS = [
  { file: "01-menu-band",           screen: "popover", place: "drop" },
  { file: "02-about",               screen: "about",   place: "center", chrome: true },
  { file: "03-looking-for-players", screen: "jam",     place: "center", chrome: true },
];

const FILL = "#ffffff";          // solid light-theme backing behind the panels
const INK = "#16161a";           // light-mode menu-bar ink (glyphs + clock)
const screenCSS = (s) => {
  if (!s.screen) return "";
  const h = Math.round(NATIVE_H[s.screen] * S);
  const img = uri(`${s.screen}.png`);
  // `drop`: sit the popover directly under the menu-bar piano strip (its center
  // lands ~407px from the right edge given the bar layout below). `center`:
  // float the window centered in the desktop below the bar.
  const pos = s.place === "drop"
    ? `top:${BAR_H - 4}px; right:407px;`
    : `top:${BAR_H + Math.round((H - BAR_H - h) / 2)}px; left:50%; transform:translateX(-50%);`;
  // The captured panels are content-views only (no window frame), so windows
  // get macOS traffic lights drawn top-left here — the close (✕) affordance the
  // capture can't include. The popover is a popover and correctly has none.
  // Panels have translucent (vibrancy) backgrounds — fill solid white so the
  // wallpaper doesn't bleed through; border-radius clips.
  const lights = s.chrome
    ? `<div class="lights"><i style="background:#ff5f57"></i><i style="background:#febc2e"></i><i style="background:#28c840"></i></div>`
    : "";
  return `<div class="win" style="position:absolute;${pos}height:${h}px;
    border-radius:15px; background:${FILL};
    box-shadow:0 30px 60px -18px rgba(30,60,45,.4), 0 0 0 1px rgba(0,0,0,.05);">
    <img style="display:block;height:${h}px;width:auto;border-radius:15px;" src="${img}">${lights}
  </div>`;
};

// Clean light-mode (black) macOS system glyphs, drawn (the live crops caught a
// low-battery red state + clipped the wifi). The Apple logo stays the REAL one
// (apple-dark.png — cropped + inverted), since its silhouette is iconic.
const SYS_ICONS = `
  <svg class="ic" viewBox="0 0 24 18" fill="${INK}"><path d="M12 16.2l3.4-4a5 5 0 0 0-6.8 0l3.4 4z"/><path d="M12 7.6c2.9 0 5.6 1.1 7.6 3l2-2.3A15 15 0 0 0 12 4.2 15 15 0 0 0 2.4 8.3l2 2.3A11.2 11.2 0 0 1 12 7.6z"/></svg>
  <svg class="ic" viewBox="0 0 40 18" fill="none"><rect x="1" y="3.5" width="32" height="11" rx="3" stroke="${INK}" stroke-width="1.6" opacity=".85"/><rect x="3" y="5.5" width="28" height="7" rx="1.5" fill="${INK}"/><rect x="34.5" y="6.5" width="2.6" height="5" rx="1.3" fill="${INK}" opacity=".85"/></svg>
  <svg class="ic" viewBox="0 0 26 18" fill="none"><rect x="2" y="3" width="22" height="5.4" rx="2.7" stroke="${INK}" stroke-width="1.6"/><rect x="2" y="9.6" width="22" height="5.4" rx="2.7" stroke="${INK}" stroke-width="1.6"/><circle cx="18" cy="5.7" r="1.7" fill="${INK}"/><circle cx="8" cy="12.3" r="1.7" fill="${INK}"/></svg>`;

const html = (s) => `<!doctype html><meta charset="utf8"><style>
  *{margin:0;padding:0;box-sizing:border-box}html,body{width:${W}px;height:${H}px;overflow:hidden}
  body{font-family:-apple-system,"SF Pro Display",sans-serif;background:${PURPLE};position:relative}
  /* Transparent light-mode menu bar — content sits on the mint wallpaper. */
  .menubar{position:absolute;top:0;left:0;width:100%;height:${BAR_H}px;display:flex;align-items:center;
    justify-content:space-between;padding:0 22px;background:transparent}
  .apple{height:21px;width:auto;display:block}
  .right{display:flex;align-items:center;gap:22px}
  .right .strip{height:30px;width:auto;display:block}
  .sys{display:flex;align-items:center;gap:17px}
  .sys .ic{height:18px;width:auto;display:block}
  .clock{color:${INK};font-size:18px;font-weight:500;white-space:nowrap}
  .win{overflow:hidden}
  .lights{position:absolute;top:16px;left:18px;display:flex;gap:12px}
  .lights i{display:block;width:20px;height:20px;border-radius:50%;
    box-shadow:inset 0 0 0 .5px rgba(0,0,0,.14)}
</style>
<div class="menubar">
  <img class="apple" src="${asset('apple-dark.png')}">
  <div class="right">
    <img class="strip" src="${uri('menubar.png')}">
    <span class="sys">${SYS_ICONS}</span>
    <span class="clock">Sat Jun 20&nbsp;&nbsp;9:41&#8202;AM</span>
  </div>
</div>
${screenCSS(s)}`;

console.log("compositing 2880x1800 canvases (solid purple)…");
for (const s of SHOTS) {
  const htmlPath = resolve(RAW, `.${s.file}.html`);
  writeFileSync(htmlPath, html(s));
  const deskOut = resolve(DESK, `${s.file}.png`);
  execFileSync(CHROME, ["--headless", "--disable-gpu", "--hide-scrollbars",
    "--force-device-scale-factor=1", `--window-size=${W},${H}`,
    `--screenshot=${deskOut}`, `file://${htmlPath}`], { stdio: "ignore" });
  const flOut = resolve(FL, `${s.file}-${W}x${H}.png`);
  execFileSync("magick", [deskOut, "-background", PURPLE, "-alpha", "remove", "-alpha", "off",
    "-resize", `${W}x${H}!`, flOut], { stdio: "ignore" });
  console.log(`  ${s.file}.png`);
}

// keep only the current real set in fastlane
for (const f of readdirSync(FL)) {
  if (!SHOTS.some((s) => f.startsWith(s.file))) { rmSync(resolve(FL, f)); console.log(`  removed stale ${f}`); }
}
console.log(`\n✓ review on Desktop: ${DESK}`);
console.log(`✓ staged for upload: ${FL}`);
