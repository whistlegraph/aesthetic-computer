#!/usr/bin/env node
// build-storyboard.mjs — render board.json as a single visual storyboard
// image (HTML → full-page PNG via chrome-shot). Shots are shown in story
// order (sorted by t0), each panel with its real thumbnail, timing, lane,
// status, and VO line.

import { readFileSync, writeFileSync, existsSync } from "node:fs";
import { resolve, dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { execFileSync } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../../..");
const board = JSON.parse(readFileSync(join(HERE, "board.json"), "utf8"));

// thumbnail per shot id (a clip frame, a source still, or a stand-in image)
const THUMB = {
  "1-emblem": "seedance-1-emblem-frame.png",
  "2-morph": "../figures/cover.png",
  "3-notepat": null,
  "4-moat": null,
  "5-anywhere": "seedance-5-boot-frame.png",
  "6-platform": "../../lacma-2026/figures/card-berz.png",
  "7-ask": "../figures/cover.png",
};
const LANE = { GEN: ["✦", "#7a55b4"], CAP: ["▣", "#2f8f7a"], CARD: ["▤", "#b44887"], VO: ["♪", "#777"] };
const fileURL = (p) => "file://" + resolve(HERE, p);
const mmss = (s) => `${Math.floor(s / 60)}:${String(Math.floor(s % 60)).padStart(2, "0")}`;

const shots = [...board.shots].sort((a, b) => a.t0 - b.t0);

const panels = shots.map((s, i) => {
  const thumbRel = THUMB[s.id];
  const thumbAbs = thumbRel ? resolve(HERE, thumbRel) : null;
  const hasImg = thumbAbs && existsSync(thumbAbs);
  const [glyph, col] = LANE[s.lane] || ["•", "#777"];
  const done = (s.status || "pending") === "done";
  const img = hasImg
    ? `<img src="${fileURL(thumbRel)}" />`
    : `<div class="ph"><span style="color:${col}">${glyph}</span><em>${s.lane}</em><small>to render</small></div>`;
  return `
  <div class="panel">
    <div class="frame">${img}
      <div class="badge" style="background:${col}">${glyph} ${s.lane}</div>
      <div class="num">${i + 1}</div>
    </div>
    <div class="meta">
      <span class="time">${mmss(s.t0)}–${mmss(s.t1)}</span>
      <span class="dot ${done ? "done" : "todo"}">${done ? "✓ shot" : "○ to make"}</span>
    </div>
    <div class="vo">“${(s.vo || "").replace(/</g, "&lt;")}”</div>
    <div class="note">${(s.prompt || "").replace(/</g, "&lt;")}</div>
  </div>`;
}).join("");

const html = `<!doctype html><meta charset="utf8"><style>
  :root { color-scheme: light; }
  * { box-sizing: border-box; }
  body { margin:0; background:#f4efe6; font-family:-apple-system,Helvetica,Arial,sans-serif; color:#2c2733; padding:34px 30px 40px; }
  h1 { font-size:26px; margin:0 0 2px; }
  .sub { color:#8a8290; font-size:13px; margin-bottom:22px; }
  .grid { display:grid; grid-template-columns:1fr 1fr; gap:20px; }
  .panel { background:#fff; border-radius:12px; padding:12px; box-shadow:0 1px 4px rgba(0,0,0,.08); }
  .frame { position:relative; aspect-ratio:16/9; border-radius:8px; overflow:hidden; background:#15131a; }
  .frame img { width:100%; height:100%; object-fit:cover; display:block; }
  .ph { width:100%; height:100%; display:flex; flex-direction:column; align-items:center; justify-content:center; gap:4px; background:#efeae1; }
  .ph span { font-size:34px; line-height:1; }
  .ph em { font-style:normal; font-weight:700; font-size:13px; color:#6b6577; letter-spacing:.08em; }
  .ph small { color:#a59fb0; font-size:11px; }
  .badge { position:absolute; left:8px; bottom:8px; color:#fff; font-size:11px; font-weight:700; padding:3px 8px; border-radius:20px; letter-spacing:.04em; }
  .num { position:absolute; top:8px; left:8px; width:26px; height:26px; border-radius:50%; background:rgba(0,0,0,.55); color:#fff; font-weight:700; font-size:13px; display:flex; align-items:center; justify-content:center; }
  .meta { display:flex; align-items:center; gap:10px; margin:10px 2px 6px; }
  .time { font-variant-numeric:tabular-nums; font-weight:700; font-size:13px; color:#4a4458; }
  .dot { font-size:11px; font-weight:600; }
  .dot.done { color:#2e9b56; } .dot.todo { color:#b59a3a; }
  .vo { font-size:14px; line-height:1.35; color:#1f1b27; margin:0 2px 6px; }
  .note { font-size:11px; line-height:1.3; color:#9b94a6; margin:0 2px; }
</style>
<h1>${board.title}</h1>
<div class="sub">${shots.length} shots · ${mmss(shots.at(-1)?.t1 ?? 0)} · jeffrey-pvc VO + notepat bed · /pop chrome look · ✦ Seedance ▣ capture ▤ card</div>
<div class="grid">${panels}</div>`;

const htmlPath = join(HERE, "storyboard.html");
const pngPath = join(HERE, "storyboard.png");
writeFileSync(htmlPath, html);
console.log("→ wrote storyboard.html");
execFileSync("node", [
  join(REPO, "toolchain/macos/chrome-shot.mjs"),
  "file://" + htmlPath, pngPath, "--size", "1300x2320", "--full-page", "--budget", "9000", "--wait", "20000",
], { stdio: "inherit" });
console.log("✓ storyboard.png");
