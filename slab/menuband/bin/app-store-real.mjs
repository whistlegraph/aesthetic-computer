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
const GRAY = "#a8a8a8";          // solid neutral-gray desktop (slightly darker)
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
render(["--render-popover", "--lang", "en", "--program", "0"], "popover.png");
render(["--render-about", "--lang", "en"], "about.png");
render(["--render-jam", "--lang", "en"], "jam.png");
render(["--render-keymap", "--lang", "en", "--program", "0"], "keymap.png");  // fullscreen expanded view

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
// Render scale the Swift --render-* modes use (px = points × this). Read each
// surface's TRUE pixel size straight from its PNG header so the composite never
// drifts when the real UI changes shape — a stale hardcoded height + aspect was
// mis-placing the popover (its arrow drifted off the menu-bar icon).
const RENDER_SCALE = 3;
const pngDims = (name) => {
  const b = readFileSync(resolve(RAW, `${name}.png`));
  return { w: b.readUInt32BE(16), h: b.readUInt32BE(20) };  // PNG IHDR width/height
};

// ── 2. the shots: plainly place the real screens on a neutral-gray desktop ───
// `center` floats the window centered below the bar; `drop` anchors it under
// the top-right status item like the live popover; `layered` cascades all
// three surfaces (About + Looking-for-Players to the left, popover dropping
// from the ♪ glyph) into one composed overview scene.
// Single App Store shot: the three-panel aggregate overview (popover dropping
// from the ♩M note, About + Looking-for-Players cascaded). The per-surface
// renders below still run because the overview composites them; the cleanup
// pass at the end deletes any other PNGs left in the upload dir.
const SHOTS = [
  { file: "00-overview", layered: true },
  { file: "01-keymap", screen: "keymap", place: "fill" },  // fullscreen expanded view
];

const FILL = "#ffffff";          // solid light-theme backing behind the panels
const INK = "#16161a";           // light-mode menu-bar ink (glyphs + clock)
const STRIP_CENTER = 1082;       // px from left — center of the ♩M note glyph (the real status item the popover drops from). Saturated piano keys end ≈x1089, Control Center starts ≈x1122; tuned visually so the callout tip lands on the note glyph's center.

// A single composited window. `screen` names the rendered surface PNG;
// `left`/`top`/`scale` place + size it (scale defaults to the global S);
// `chrome` draws macOS traffic lights; `z` is the stacking order; `arrow`
// draws the upward NSPopover callout (only the popover gets one). Returns
// the absolute-positioned <div> markup; reused by both `screenCSS` (one
// surface per canvas) and `overviewCSS` (three cascaded on one canvas).
const windowEl = ({ screen, left, top, scale = S, chrome = false, z = 1, arrow = false }) => {
  const dims = pngDims(screen);
  const h = Math.round(dims.h / RENDER_SCALE * scale);   // displayed height (points × scale)
  const w = Math.round(h * dims.w / dims.h);             // true width from the real aspect
  const img = uri(`${screen}.png`);
  // NSPopover callout arrow — a triangle pointing UP at the status item, tip
  // at the menu-bar bottom, base meeting the popover top, centered on the strip.
  const callout = arrow
    ? `<div style="position:absolute;z-index:${z};left:${STRIP_CENTER - 13}px;top:${BAR_H}px;width:0;height:0;
        border-left:13px solid transparent;border-right:13px solid transparent;
        border-bottom:12px solid ${FILL};
        filter:drop-shadow(0 -1px 1.5px rgba(0,0,0,.16));"></div>`
    : "";
  // The captured panels are content-views only (no window frame), so windows
  // get macOS traffic lights drawn top-left here — the close (✕) affordance the
  // capture can't include. The popover is a popover and correctly has none.
  // Panels have translucent (vibrancy) backgrounds — fill solid white so the
  // wallpaper doesn't bleed through; border-radius clips.
  const lights = chrome
    ? `<div class="lights"><i style="background:#ff5f57"></i><i style="background:#febc2e"></i><i style="background:#28c840"></i></div>`
    : "";
  return `${callout}<div class="win" style="position:absolute;left:${left}px;top:${top}px;
    width:${w}px;height:${h}px;z-index:${z};
    border-radius:15px; background:${FILL};
    box-shadow:0 30px 60px -18px rgba(0,0,0,.4), 0 0 0 1px rgba(0,0,0,.05);">
    <img style="display:block;height:${h}px;width:${w}px;border-radius:15px;" src="${img}">${lights}
  </div>`;
};

const screenCSS = (s) => {
  if (s.layered) return overviewCSS();
  if (!s.screen) return "";
  const dims = pngDims(s.screen);
  const h = Math.round(dims.h / RENDER_SCALE * S);   // displayed height (points × S)
  const w = Math.round(h * dims.w / dims.h);         // true width from real aspect
  // `drop`: sit the popover directly under the menu-bar piano strip so its
  // callout arrow lands on the ♪ status item. `center`: float the window
  // centered in the desktop below the bar.
  if (s.place === "drop") {
    return windowEl({ screen: s.screen, left: Math.round(STRIP_CENTER - w / 2),
                      top: BAR_H + 12, chrome: s.chrome, arrow: true });
  }
  // `fill`: scale the surface to fill most of the desktop below the bar —
  // used for the fullscreen expanded (keymap) view, which covers the screen
  // in the live app. No traffic lights (it's a borderless overlay).
  if (s.place === "fill") {
    const dims2 = pngDims(s.screen);
    const fh = Math.round((H - BAR_H) * 0.94);
    const fscale = (fh / (dims2.h / RENDER_SCALE));
    const fw = Math.round(fh * dims2.w / dims2.h);
    return windowEl({ screen: s.screen, scale: fscale,
                      left: Math.round((W - fw) / 2),
                      top: BAR_H + Math.round((H - BAR_H - fh) / 2),
                      chrome: false });
  }
  return windowEl({ screen: s.screen,
                    left: Math.round((W - w) / 2),
                    top: BAR_H + Math.round((H - BAR_H - h) / 2),
                    chrome: s.chrome });
};

// Layered overview: three real surfaces cascaded on one canvas. The popover
// drops from the ♪ glyph at the right (foreground); the About window sits to
// its left; the Looking-For-Players window tucks behind the About's left edge,
// slightly overlapping, so the scene reads as windows stacked on the desktop.
// A smaller scale than the single-surface shots so all three fit the 1440×900
// frame with margins, and the taller popover stays inside the bottom edge.
const overviewCSS = () => {
  const OS = 1.3;                               // overview surface scale (fits 3 evenly)
  const sized = (n) => {
    const d = pngDims(n), h = Math.round(d.h / RENDER_SCALE * OS);
    return { h, w: Math.round(h * d.w / d.h) };
  };
  const pop = sized("popover"), about = sized("about"), jam = sized("jam");

  // The popover DROPS from the ♪ status item like the real NSPopover — its
  // callout arrow lands on the note glyph, top tucked right under the bar.
  // About + Looking-For-Players cascade in the desktop space to its left,
  // vertically centered, with the three gaps (left margin, inter-window
  // gutter, gutter to the popover) equal so the rhythm reads as even.
  const popLeft = Math.round(STRIP_CENTER - pop.w / 2);
  const gap = Math.round((popLeft - (jam.w + about.w)) / 3);
  const jamLeft = gap;
  const aboutLeft = jamLeft + jam.w + gap;
  const cy = (BAR_H + H) / 2;
  const topFor = (hh) => Math.round(cy - hh / 2);

  return [
    windowEl({ screen: "jam",     left: jamLeft,   top: topFor(jam.h),   scale: OS, chrome: true, z: 1 }),
    windowEl({ screen: "about",   left: aboutLeft, top: topFor(about.h), scale: OS, chrome: true, z: 2 }),
    windowEl({ screen: "popover", left: popLeft,   top: BAR_H + 12,      scale: OS, chrome: false, z: 3, arrow: true }),
  ].join("\n");
};

// Real macOS menu-bar glyphs — actual SF Symbols (Control Center, Wi-Fi,
// battery) rendered by the OS at their natural aspect ratios via
// bin/render-sf.swift, tinted to the light-bar ink. No hand-drawn SVG, no
// stretching. The Apple logo is the SF "apple.logo" symbol for the same reason.
const SYS_ICONS = `
  <img class="ic" src="${asset('sf-control.png')}">
  <img class="ic" src="${asset('sf-wifi.png')}">
  <img class="ic" src="${asset('sf-battery.png')}">`;

const html = (s) => `<!doctype html><meta charset="utf8"><style>
  *{margin:0;padding:0;box-sizing:border-box}html,body{width:${W}px;height:${H}px;overflow:hidden}
  body{font-family:-apple-system,"SF Pro Display",sans-serif;background:${GRAY};position:relative}
  /* Transparent light-mode menu bar — content sits on the gray wallpaper. */
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
  <img class="apple" src="${asset('sf-apple.png')}">
  <div class="right">
    <img class="strip" src="${uri('menubar.png')}">
    <span class="sys">${SYS_ICONS}</span>
    <span class="clock">Sat Jun 20&nbsp;&nbsp;9:41&#8202;AM</span>
  </div>
</div>
${screenCSS(s)}`;

console.log("compositing canvases (solid neutral gray)…");
for (const s of SHOTS) {
  const htmlPath = resolve(RAW, `.${s.file}.html`);
  writeFileSync(htmlPath, html(s));
  const deskOut = resolve(DESK, `${s.file}.png`);
  execFileSync(CHROME, ["--headless", "--disable-gpu", "--hide-scrollbars",
    "--force-device-scale-factor=1", `--window-size=${W},${H}`,
    `--screenshot=${deskOut}`, `file://${htmlPath}`], { stdio: "ignore" });
  const flOut = resolve(FL, `${s.file}-${W}x${H}.png`);
  execFileSync("magick", [deskOut, "-background", GRAY, "-alpha", "remove", "-alpha", "off",
    "-resize", `${W}x${H}!`, flOut], { stdio: "ignore" });
  console.log(`  ${s.file}.png`);
}

// keep only the current real set in fastlane
for (const f of readdirSync(FL)) {
  if (!SHOTS.some((s) => f.startsWith(s.file))) { rmSync(resolve(FL, f)); console.log(`  removed stale ${f}`); }
}
console.log(`\n✓ review on Desktop: ${DESK}`);
console.log(`✓ staged for upload: ${FL}`);
