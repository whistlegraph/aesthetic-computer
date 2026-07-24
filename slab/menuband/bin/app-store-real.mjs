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
// Three shots: a tightly framed real popover, a reel-inspired composition
// that keeps the real expanded keymap as its dominant surface, and a focused
// MIDI portrait built around the real popover's MIDI OUT control.
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
const REEL_BG = resolve(ROOT, "screenshots/reel-background.png");
const PALS_SVG = resolve(ROOT, "../../system/public/purple-pals.svg");
const CHROME = "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome";
// Lower resolution (1440x900 — a valid Mac App Store size) so the UI fills more
// of the frame and reads larger than it did at 2880x1800.
const W = 1440, H = 900;
const GRAY = "#a8a8a8";          // solid neutral-gray desktop (slightly darker)
const BAR_H = 44;
const S = 1.6;                   // surface scale (popover ~730px tall, prominent)

mkdirSync(RAW, { recursive: true });
mkdirSync(FL, { recursive: true });

// Always capture the sandboxed product. A direct-build binary contains host
// integrations (notably Spotify) that do not ship in the Mac App Store and
// therefore must never appear in its screenshots.
console.log("building sandboxed App Store capture binary…");
execFileSync("swift", ["build", "-Xswiftc", "-DMAC_APP_STORE"], {
  cwd: ROOT,
  stdio: "inherit",
});
if (!existsSync(BIN)) { console.error(`missing capture binary: ${BIN}`); process.exit(1); }

// ── 1. render the real UI surfaces ──────────────────────────────────────────
const render = (args, out) => {
  execFileSync(BIN, [...args, "--out", resolve(RAW, out), "--scale", "3"], { stdio: "ignore" });
  return out;
};
console.log("rendering real UI surfaces…");
render(["--render-menubar", "--light"], "menubar.png");   // keys at rest, light theme
render(["--render-popover", "--lang", "en", "--program", "0"], "popover.png");
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
const fileUri = (f, mime = "image/png") => `data:${mime};base64,${readFileSync(f).toString("base64")}`;
if (!existsSync(REEL_BG)) { console.error(`missing reel backdrop: ${REEL_BG}`); process.exit(1); }
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

// ── 2. two focused App Store portraits ──────────────────────────────────────
const SHOTS = [
  { file: "00-keyboard-menu", kind: "literal" },
  { file: "01-reel", kind: "reel" },
  { file: "02-midi", kind: "midi" },
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
const windowEl = ({ screen, left, top, scale = S, chrome = false, z = 1,
                    arrow = false, anchorX = STRIP_CENTER, arrowTop = BAR_H }) => {
  const dims = pngDims(screen);
  const h = Math.round(dims.h / RENDER_SCALE * scale);   // displayed height (points × scale)
  const w = Math.round(h * dims.w / dims.h);             // true width from the real aspect
  const img = uri(`${screen}.png`);
  // NSPopover callout arrow — a triangle pointing UP at the status item, tip
  // at the menu-bar bottom, base meeting the popover top, centered on the strip.
  const callout = arrow
    ? `<div style="position:absolute;z-index:${z};left:${anchorX - 13}px;top:${arrowTop}px;width:0;height:0;
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
  if (s.kind === "literal") {
    const d = pngDims("popover");
    const scale = 1.85;
    const h = Math.round(d.h / RENDER_SCALE * scale);
    const w = Math.round(h * d.w / d.h);
    const anchorX = Math.round(W / 2);
    return windowEl({ screen: "popover", scale,
      left: Math.round(anchorX - w / 2), top: 90,
      arrow: true, anchorX, arrowTop: 78 });
  }
  if (s.kind === "reel") {
    const d = pngDims("keymap");
    const h = 742;
    const scale = h / (d.h / RENDER_SCALE);
    const w = Math.round(h * d.w / d.h);
    return windowEl({ screen: "keymap", scale,
      left: Math.round((W - w) / 2), top: 104, z: 3 });
  }
  if (s.kind === "midi") {
    const d = pngDims("popover");
    const scale = 1.62;
    const h = Math.round(d.h / RENDER_SCALE * scale);
    const w = Math.round(h * d.w / d.h);
    return windowEl({ screen: "popover", scale,
      left: 845, top: Math.round((H - h) / 2), z: 3 });
  }
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

const literalStrip = () => {
  const d = pngDims("menubar");
  const height = 42;
  const width = Math.round(height * d.w / d.h);
  // The note glyph occupies the strip's far-right slice; position that slice
  // over the popover callout while keeping the complete keyboard visible.
  const noteOffset = Math.round(width * 0.953);
  return `<img class="literal-strip" style="left:${Math.round(W / 2 - noteOffset)}px;width:${width}px;height:${height}px" src="${uri('menubar.png')}">`;
};

const scene = (s) => {
  if (s.kind === "literal") return `${literalStrip()}${screenCSS(s)}`;
  if (s.kind === "midi") return `<div class="midi-copy">
      <div class="midi-kicker">VIRTUAL MIDI OUT</div>
      <h1>Your menu bar<br>plays the studio.</h1>
      <p>Send every note to your DAW, instrument plugin, or hardware setup.</p>
    </div>
    <img class="midi-strip" src="${uri('menubar.png')}">
    ${screenCSS(s)}`;
  return `<div class="edge-title left">menuband</div>
    <div class="edge-title right">menuband</div>
    <img class="pals pals-left" src="${fileUri(PALS_SVG, 'image/svg+xml')}">
    <img class="pals pals-right" src="${fileUri(PALS_SVG, 'image/svg+xml')}">
    <img class="reel-strip" src="${uri('menubar.png')}">
    ${screenCSS(s)}`;
};

const html = (s) => `<!doctype html><meta charset="utf8"><style>
  *{margin:0;padding:0;box-sizing:border-box}html,body{width:${W}px;height:${H}px;overflow:hidden}
  body{font-family:-apple-system,"SF Pro Display",sans-serif;position:relative;
    background:${s.kind === 'literal' ? '#d4d1d8' : s.kind === 'midi' ? 'linear-gradient(135deg,#1a1138 0%,#4f2f91 52%,#ef6ea8 100%)' : `url(${fileUri(REEL_BG)}) center/cover no-repeat`}}
  .win{overflow:hidden}
  .lights{position:absolute;top:16px;left:18px;display:flex;gap:12px}
  .lights i{display:block;width:20px;height:20px;border-radius:50%;
    box-shadow:inset 0 0 0 .5px rgba(0,0,0,.14)}
  .literal-strip{position:absolute;top:22px;display:block;object-fit:contain;z-index:5;
    filter:drop-shadow(0 8px 12px rgba(0,0,0,.16))}
  .reel-strip{position:absolute;left:50%;top:24px;transform:translateX(-50%);
    height:38px;width:auto;z-index:5;filter:drop-shadow(0 8px 14px rgba(0,0,0,.35))}
  .edge-title{position:absolute;z-index:2;top:50%;color:#8fb4ff;font:500 38px/1 "SF Mono",monospace;
    letter-spacing:5px;text-shadow:0 2px 0 #24183f,0 0 16px rgba(83,127,255,.5)}
  .edge-title.left{left:31px;transform:translateY(-50%) rotate(-90deg)}
  .edge-title.right{right:31px;transform:translateY(-50%) rotate(90deg);color:#ffab45}
  .pals{position:absolute;z-index:2;width:116px;height:116px;opacity:.72;
    filter:drop-shadow(3px 4px 0 rgba(24,13,49,.5))}
  .pals-left{left:38px;bottom:72px;transform:rotate(90deg)}
  .pals-right{right:38px;top:86px;transform:rotate(-90deg)}
  .midi-copy{position:absolute;left:92px;top:220px;width:660px;color:white;z-index:4}
  .midi-kicker{margin-bottom:22px;font:700 22px/1 "SF Mono",monospace;letter-spacing:4px;color:#8de7f0}
  .midi-copy h1{font-size:72px;line-height:.98;letter-spacing:-3.5px;text-shadow:0 6px 24px rgba(19,7,45,.3)}
  .midi-copy p{margin-top:30px;width:580px;font-size:27px;line-height:1.25;color:rgba(255,255,255,.86)}
  .midi-strip{position:absolute;left:890px;top:27px;height:40px;width:auto;z-index:5;
    filter:drop-shadow(0 8px 14px rgba(0,0,0,.28))}
</style>
${scene(s)}`;

console.log("compositing focused App Store portraits…");
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
