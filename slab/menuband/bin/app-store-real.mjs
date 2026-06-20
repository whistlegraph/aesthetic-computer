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
const W = 2880, H = 1800;
const PURPLE = "#6d24c4";        // solid desktop
const BAR_H = 76;
const S = 56 / 22;               // shared screen scale: menubar status item 22pt -> 56px

mkdirSync(RAW, { recursive: true });
mkdirSync(FL, { recursive: true });
if (!existsSync(BIN)) { console.error(`build first: (cd ${ROOT} && swift build)`); process.exit(1); }

// ── 1. render the real UI surfaces ──────────────────────────────────────────
const render = (args, out) => {
  execFileSync(BIN, [...args, "--out", resolve(RAW, out), "--scale", "3"], { stdio: "ignore" });
  return out;
};
console.log("rendering real UI surfaces…");
render(["--render-menubar", "--notes", "60,64,67"], "menubar.png");
render(["--render-popover", "--program", "0"], "popover.png");
render(["--render-about"], "about.png");
render(["--render-jam"], "jam.png");

const uri = (f) => `data:image/png;base64,${readFileSync(resolve(RAW, f)).toString("base64")}`;
// native point heights of each surface (from the Swift renders), for shared scale
const NATIVE_H = { popover: 456, about: 487, jam: 360 };

// ── 2. the four shots: plainly place the real screens on a purple desktop ────
// `center` floats the window centered below the bar; `drop` anchors it under
// the top-right status item like the live popover.
const SHOTS = [
  { file: "01-menu-bar",            screen: null },
  { file: "02-menu-bar-popover",    screen: "popover", place: "drop" },
  { file: "03-about",               screen: "about",   place: "center" },
  { file: "04-looking-for-players", screen: "jam",     place: "center" },
];

const FILL = "#f3f3f5";          // solid light-theme backing behind the panels
const screenCSS = (s) => {
  if (!s.screen) return "";
  const h = Math.round(NATIVE_H[s.screen] * S);
  const img = uri(`${s.screen}.png`);
  const pos = s.place === "drop"
    ? `top:${BAR_H - 8}px; right:320px;`
    : `top:${BAR_H + Math.round((H - BAR_H - h) / 2)}px; left:50%; transform:translateX(-50%);`;
  // The captured panels have translucent (vibrancy) backgrounds. Fill them with
  // a solid light-theme color so the purple desktop doesn't bleed through —
  // the img composites over this element background; border-radius clips both.
  return `<img class="screen" style="position:absolute;${pos}height:${h}px;width:auto;
    background:${FILL}; border-radius:24px;
    box-shadow:0 60px 120px -28px rgba(20,0,50,.6), 0 0 0 1px rgba(0,0,0,.05);" src="${img}">`;
};

const html = (s) => `<!doctype html><meta charset="utf8"><style>
  *{margin:0;padding:0;box-sizing:border-box}html,body{width:${W}px;height:${H}px;overflow:hidden}
  body{font-family:-apple-system,"SF Pro Display",sans-serif;background:${PURPLE};position:relative}
  .menubar{position:absolute;top:0;left:0;width:100%;height:${BAR_H}px;display:flex;align-items:center;
    justify-content:space-between;padding:0 40px;background:rgba(20,6,44,.34);backdrop-filter:blur(30px);
    border-bottom:1px solid rgba(255,255,255,.10)}
  .apple{color:#fff;font-size:40px;line-height:1}
  .right{display:flex;align-items:center;gap:34px}
  .right img{height:56px;width:auto;display:block}
  .clock{color:#fff;font-size:31px;font-weight:500}
</style>
<div class="menubar">
  <span class="apple"></span>
  <div class="right"><img src="${uri('menubar.png')}"><span class="clock">9:41</span></div>
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
  const flOut = resolve(FL, `${s.file}-2880x1800.png`);
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
