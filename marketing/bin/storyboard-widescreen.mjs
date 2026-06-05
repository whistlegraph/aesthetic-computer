#!/usr/bin/env node
// storyboard-widescreen.mjs — GENERIC single-page "broadside" storyboard PDF for
// a narrated application video: per-segment illustration + spoken narration +
// on-screen text + illustration prompt + timing/beat/marker, all driven by the
// campaign config. Output to the Desktop (or cfg.storyboard.out), opened in Preview.
//
// Usage: node marketing/bin/storyboard-widescreen.mjs <campaign-dir> [--open]
import { readFileSync, existsSync } from "node:fs";
import { execSync } from "node:child_process";
import { dirname, resolve } from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";
import puppeteer from "puppeteer";
import sharp from "sharp";

const REPO = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
const CHROME = "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome";

const argv = process.argv.slice(2);
const campArg = argv.find((a) => !a.startsWith("--"));
if (!campArg) { console.error("usage: storyboard-widescreen.mjs <campaign-dir> [--open]"); process.exit(2); }
const campDir = resolve(process.cwd(), campArg);
const { default: cfg } = await import(pathToFileURL(`${campDir}/campaign.mjs`).href);
const sb = cfg.storyboard || {};
const OUT = sb.out || `${process.env.HOME}/Desktop/${cfg.name}-storyboard.pdf`;

const segs = JSON.parse(readFileSync(`${REPO}/recap/out/segments.json`, "utf8"));
const words = JSON.parse(readFileSync(`${REPO}/recap/out/words.json`, "utf8"));
const recapDur = Math.max(...segs.map((s) => s.endSec));
const TITLES = { ...(cfg.titles || {}), "10_end": "—" };

const FIX = (cfg.textFixes || []).map(([p, r, f]) => [new RegExp(p, f || ""), r]);
const fixWords = (s) => FIX.reduce((a, [re, r]) => a.replace(re, r), s);
const esc = (s) => s.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;");
const mmss = (t) => `${Math.floor(t / 60)}:${String(Math.floor(t % 60)).padStart(2, "0")}`;

const spokenFor = (s) => fixWords(words
  .filter((w) => (w.fromMs / 1000) >= s.startSec - 0.05 && (w.fromMs / 1000) < s.endSec - 0.05)
  .map((w) => w.text).join(" ").replace(/\s+([,.;:!?])/g, "$1").replace(/^["“]/, "")).trim();

async function dataUri(p) {
  const buf = await sharp(p).resize({ width: 1400, withoutEnlargement: true }).jpeg({ quality: 82 }).toBuffer();
  return `data:image/jpeg;base64,${buf.toString("base64")}`;
}

const rows = await Promise.all(segs.map(async (s, i) => {
  const dir = `${campDir}/${s.name}`;
  let img = `${dir}/gens/native.png`; if (!existsSync(img)) img = `${dir}/gens/pencil.png`;
  const hasImg = existsSync(img);
  const src = img.includes("native") ? "native" : (img.includes("pencil") ? "pencil" : "—");
  let prompt = `${dir}/cover-prompt-native.txt`; if (!existsSync(prompt)) prompt = `${dir}/cover-prompt.txt`;
  const promptText = existsSync(prompt) ? readFileSync(prompt, "utf8").trim() : "(no prompt on file)";
  const dur = s.endSec - s.startSec;
  const spoken = spokenFor(s) || (TITLES[s.name] ? "" : "(silent tail · wordmark hold)");
  const wc = spoken && !spoken.startsWith("(") ? spoken.split(/\s+/).length : 0;
  return {
    n: i + 1, name: s.name, title: TITLES[s.name] || s.name, start: s.startSec, end: s.endSec, dur, wc,
    wpm: wc && dur ? Math.round(wc / dur * 60) : 0,
    marker: (sb.markers || {})[s.name] || "", beat: (sb.beats || {})[s.name] || "",
    spoken, onscreen: (sb.onscreen || {})[s.name] || "", img: hasImg ? await dataUri(img) : null, src, promptText,
  };
}));

const PAGE_W = 1180;
const html = `<!doctype html><html><head><meta charset="utf-8"><style>
  * { box-sizing: border-box; } html, body { margin: 0; padding: 0; }
  body { font-family: -apple-system, "Helvetica Neue", Arial, sans-serif; color: #14201a; width: ${PAGE_W}px; background: #fff; }
  .wrap { padding: 40px 48px 56px; }
  .cover { background: #06120c; color: #e8ffe8; padding: 40px 44px; border-radius: 8px; margin-bottom: 12px; }
  .cover .kicker { color: #3dff88; letter-spacing: 2px; text-transform: uppercase; font-size: 13px; }
  .cover h1 { font-size: 50px; margin: 8px 0 4px; line-height: 1.0; color: #fcf7c5; }
  .cover h2 { font-size: 20px; font-weight: 500; color: #ffbf3d; margin: 0 0 22px; }
  .cover .spine { font-size: 14.5px; line-height: 1.55; max-width: 760px; color: #cfe; }
  .cover .meta { margin-top: 20px; font-size: 12px; color: #7fae98; line-height: 1.7; }
  .seg { border-top: 1.5px solid #06120c; padding-top: 10px; margin-top: 12px; }
  .seg h3 { font-size: 17px; margin: 0 0 2px; } .seg h3 .num { color: #1c9b5e; font-weight: 800; }
  .seg .time { color: #6b8; font-size: 11px; font-weight: 600; }
  .beat { font-size: 12px; color: #2c4a3c; line-height: 1.35; margin: 0 0 7px; }
  .metaline { font-size: 10px; color: #8a98; margin-bottom: 6px; } .metaline b { color: #1c9b5e; }
  .grid { display: grid; grid-template-columns: 360px 1fr; gap: 20px; align-items: start; }
  .shot img { width: 100%; border: 1px solid #cdd; border-radius: 3px; display: block; }
  .panel > div { margin-bottom: 7px; }
  .lbl { font-size: 9px; letter-spacing: 1.2px; text-transform: uppercase; color: #1c9b5e; font-weight: 700; margin-bottom: 2px; }
  .spoken { font-size: 13.5px; line-height: 1.4; } .onscreen { font-size: 11.5px; color: #2a6; font-style: italic; }
  .prompt { font-size: 9.5px; line-height: 1.36; color: #56615b; white-space: pre-wrap; columns: 2; column-gap: 16px;
    background: #f4f7f4; border-left: 3px solid #3dff88; padding: 7px 9px; border-radius: 0 3px 3px 0; }
</style></head><body><div class="wrap">
  <div class="cover">
    <div class="kicker">${esc(sb.coverKicker || "storyboard")}</div>
    <h1>${esc(sb.coverTitle || cfg.name)}</h1>
    <h2>${esc(sb.coverSub || "")}</h2>
    <div class="spine">${esc(sb.spine || "")}</div>
    <div class="meta">Widescreen 1920×1080 · ${mmss(recapDur)} · ${segs.length} segments<br>
      Audience: ${esc(cfg.audience || cfg.name)} · karaoke word-train captions<br>
      Pipeline: recap/audience/${esc(cfg.audience || cfg.name)}.mjs → marketing/bin/compose-widescreen.mjs ${esc(campArg)}</div>
  </div>
  ${rows.map((r) => `
  <div class="seg">
    <h3><span class="num">${String(r.n).padStart(2, "0")}</span> &nbsp;${esc(r.title)} <span class="time">· ${mmss(r.start)}–${mmss(r.end)} (${r.dur.toFixed(1)}s) · ${esc(r.name)}</span></h3>
    <div class="beat">${esc(r.beat)}</div>
    <div class="metaline">marker cut: <b>“${esc(r.marker)}”</b>${r.wc ? ` &nbsp;·&nbsp; ${r.wc} words · ${r.wpm} wpm` : ""} &nbsp;·&nbsp; illustration: <b>${r.src}</b>.png</div>
    <div class="grid">
      <div class="shot">${r.img ? `<img src="${r.img}">` : `<div>(no illustration)</div>`}</div>
      <div class="panel">
        <div><div class="lbl">Spoken (narration)</div><div class="spoken">${esc(r.spoken)}</div></div>
        <div><div class="lbl">On-screen text</div><div class="onscreen">${esc(r.onscreen)}</div></div>
        <div><div class="lbl">Illustration prompt</div><div class="prompt">${esc(r.promptText)}</div></div>
      </div>
    </div>
  </div>`).join("")}
</div></body></html>`;

const browser = await puppeteer.launch({ headless: "new", executablePath: CHROME, args: ["--no-sandbox", "--allow-file-access-from-files"] });
try {
  const page = await browser.newPage();
  await page.setContent(html, { waitUntil: "networkidle0" });
  const heightPx = await page.evaluate(() => Math.ceil(document.documentElement.scrollHeight));
  await page.pdf({ path: OUT, width: `${PAGE_W}px`, height: `${heightPx}px`, printBackground: true, pageRanges: "1", margin: { top: 0, right: 0, bottom: 0, left: 0 } });
  console.log(`✓ broadside PDF (${PAGE_W}×${heightPx}px) → ${OUT}`);
} finally { await browser.close(); }
if (argv.includes("--open")) spawnSyncOpen(OUT);
function spawnSyncOpen(p) { execSync(`open -a Preview "${p}"`); }
