#!/usr/bin/env node
// Build a /papers-style visual receipt from an accepted MP4 + Captutor trace.

import { execFileSync, spawn, spawnSync } from "node:child_process";
import {
  existsSync, mkdirSync, mkdtempSync, readFileSync, rmSync, writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { basename, dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { pathToFileURL } from "node:url";
import { setTimeout as delay } from "node:timers/promises";

const HERE = dirname(fileURLToPath(import.meta.url));
const args = process.argv.slice(2);
const arg = (name) => {
  const index = args.indexOf(name);
  return index < 0 ? null : args[index + 1];
};
const video = resolve(arg("--video") || "");
const storyboardPath = resolve(arg("--storyboard") || "");
const out = resolve(arg("--out") || video.replace(/\.mp4$/i, ".storyboard-receipt.pdf"));
if (!existsSync(video) || !existsSync(storyboardPath)) {
  console.error("usage: storyboard-receipt --video take.mp4 --storyboard take.storyboard.json [--out receipt.pdf]");
  process.exit(2);
}

const story = JSON.parse(readFileSync(storyboardPath, "utf8"));
const receiptEnglish = story.receiptEnglish || null;
const nonEnglish = !/^en(?:-|$)/i.test(story.locale || "en");
if (nonEnglish && (!receiptEnglish?.title || !Array.isArray(receiptEnglish?.beats))) {
  throw new Error(
    `non-English storyboard ${storyboardPath} must include receiptEnglish title, subtitle, and beats`,
  );
}
const work = mkdtempSync(join(tmpdir(), "captutor-receipt-"));
const frames = join(work, "frames");
mkdirSync(frames);

function run(file, argv, options = {}) {
  return execFileSync(file, argv, { encoding:"utf8", ...options });
}

function escapeTex(value) {
  return String(value ?? "")
    .replaceAll("\\", "\\textbackslash{}")
    .replaceAll("&", "\\&").replaceAll("%", "\\%")
    .replaceAll("$", "\\$").replaceAll("#", "\\#")
    .replaceAll("_", "\\_").replaceAll("{", "\\{")
    .replaceAll("}", "\\}").replaceAll("~", "\\textasciitilde{}")
    .replaceAll("^", "\\textasciicircum{}")
    .replaceAll("→", "\\ensuremath{\\rightarrow}");
}

function escapeHtml(value) {
  return String(value ?? "")
    .replaceAll("&", "&amp;")
    .replaceAll("<", "&lt;")
    .replaceAll(">", "&gt;")
    .replaceAll('"', "&quot;")
    .replaceAll("'", "&#39;");
}

function imageData(path) {
  return `data:image/jpeg;base64,${readFileSync(path).toString("base64")}`;
}

function commandWorks(file) {
  const result = spawnSync(file, ["--version"], { encoding:"utf8" });
  return !result.error && result.status === 0;
}

function findChrome() {
  const candidates = [
    process.env.CAPTUTOR_CHROME,
    "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
    "/Applications/Chromium.app/Contents/MacOS/Chromium",
    "google-chrome",
    "chromium",
    "chromium-browser",
  ].filter(Boolean);
  return candidates.find((candidate) =>
    (candidate.startsWith("/") ? existsSync(candidate) : commandWorks(candidate))) || null;
}

const usesDevanagari = /^hi(?:-|$)/i.test(story.locale || "");
const storyTex = (value) => usesDevanagari
  ? `{\\storyfont ${escapeTex(value)}}`
  : escapeTex(value);
const englishTitle = receiptEnglish?.title || story.title;
const englishSubtitle = receiptEnglish?.subtitle || story.subtitle;
const englishBeat = (beat) => receiptEnglish?.beats?.[beat.index] || null;

const probe = JSON.parse(run("ffprobe", [
  "-v", "error", "-show_entries",
  "format=duration,size:stream=codec_name,codec_type,width,height,r_frame_rate,sample_rate,channels",
  "-of", "json", video,
]));
let loudness = null;
const loudnessProbe = spawnSync("ffmpeg", [
  "-hide_banner", "-nostats", "-i", video,
  "-filter_complex", "ebur128=framelog=verbose", "-f", "null", "-",
], { encoding:"utf8" });
const loudnessMatches = String(loudnessProbe.stderr || "")
  .match(/I:\s*(-?\d+(?:\.\d+)?) LUFS/g);
if (loudnessMatches?.length) {
  loudness = Number(loudnessMatches.at(-1).match(/-?\d+(?:\.\d+)?/)[0]);
}

const duration = Number(probe.format.duration);
const checks = new Map(story.events.filter((event) => event.kind === "check")
  .map((event) => [event.name, event]));
const acceptance = story.acceptance || {};
const defaultGeometry = story.format === "vertical" || story.format === "reel"
  ? [1440, 2560] : [2560, 1440];
const expected = acceptance.resolution || (story.acceptance
  ? defaultGeometry : [story.media?.width, story.media?.height]);
const videoStream = probe.streams.find((stream) => stream.codec_type === "video");
const audioStream = probe.streams.find((stream) => stream.codec_type === "audio");
const signboards = story.events.filter((event) => event.kind === "signboard");
const opening = signboards.find((event) => event.role === "opening");
const ending = signboards.find((event) => event.role === "closing");
const evidenceSummary = (event) => {
  if (!event) return "missing trace event";
  const evidence = event.evidence;
  if (typeof evidence === "string") return evidence;
  if (evidence == null) return `observed at ${event.atSec.toFixed(2)}s`;
  if (evidence.note) return evidence.note;
  const compact = Object.entries(evidence)
    .filter(([key]) => key !== "pass")
    .map(([key, value]) => `${key}=${Array.isArray(value) ? value.join("×") : value}`)
    .join("; ") || "trace check passed";
  return compact.length > 92 ? compact.slice(0, 89) + "…" : compact;
};
const requiredChecks = acceptance.requiredChecks || [];
const loudnessRange = acceptance.loudnessLufs || null;
const validations = [
  ["English receipt copy", !nonEnglish || Boolean(receiptEnglish?.title &&
    receiptEnglish?.subtitle && receiptEnglish?.beats?.length === story.beats.length),
  nonEnglish
    ? `${receiptEnglish?.beats?.length || 0}/${story.beats.length} beats carry English QA copy`
    : "source and receipt language are English"],
  ["Delivery geometry", videoStream?.width === expected[0] && videoStream?.height === expected[1],
    `${videoStream?.width}×${videoStream?.height}; expected ${expected.join("×")}`],
  ...(acceptance.minimumDurationSec ? [[
    "Minimum length", duration >= acceptance.minimumDurationSec,
    `${duration.toFixed(2)} s; minimum ${acceptance.minimumDurationSec} s`,
  ]] : []),
  ...(acceptance.maximumDurationSec ? [[
    "Maximum length", duration <= acceptance.maximumDurationSec,
    `${duration.toFixed(2)} s; maximum ${acceptance.maximumDurationSec} s`,
  ]] : []),
  ...(acceptance.requireOpeningCard ? [[
    "Opening title card", Boolean(opening?.result?.filmed),
    opening ? `${opening.result?.durationMs || opening.durationSec * 1000} ms; filmed=${Boolean(opening.result?.filmed)}` : "missing opening card",
  ]] : []),
  ...(acceptance.requireEndingCard ? [[
    "Ending card", Boolean(ending?.result?.filmed),
    ending ? `${ending.result?.durationMs || ending.durationSec * 1000} ms; filmed=${Boolean(ending.result?.filmed)}` : "missing ending card",
  ]] : []),
  ...(acceptance.requireBrandChrome ? [[
    "Client brand chrome", Boolean(story.brandChrome?.id),
    story.brandChrome?.id ? `theme ${story.brandChrome.id}; responsive inward side lockups` : "theme missing",
  ]] : []),
  ...requiredChecks.map((name) => {
    const event = checks.get(name);
    return [name.replaceAll("_", " "), Boolean(event) && event.evidence?.pass !== false,
      evidenceSummary(event)];
  }),
  ["Audio delivery", audioStream?.sample_rate === "48000" &&
    (!loudnessRange || loudness == null || (loudness >= loudnessRange[0] && loudness <= loudnessRange[1])),
    `AAC ${audioStream?.sample_rate || "?"} Hz${loudness == null ? "" : `; ${loudness.toFixed(1)} LUFS`}`],
];
const accepted = validations.every(([, pass]) => pass);

function visualEventForBeat(beat, nextOffset) {
  const within = story.events.filter((event) =>
    event.atSec >= beat.offsetSec && event.atSec < nextOffset);
  return within.findLast((event) => ["spotlight", "outline"].includes(event.kind))
    || within.find((event) => event.kind === "drag")
    || within.find((event) => event.kind === "click")
    || within.findLast((event) => event.kind === "check");
}

function extract(name, atSec) {
  const path = join(frames, `${name}.jpg`);
  const time = Math.max(0, Math.min(duration - 0.08, atSec));
  run("ffmpeg", ["-hide_banner", "-loglevel", "error", "-ss", time.toFixed(3),
    "-i", video, "-frames:v", "1", "-q:v", "2", path]);
  return path;
}

const storyboardCards = signboards.map((event, index) => ({
  ...event,
  frame:extract(`card-${index + 1}`, event.atSec - event.durationSec / 2),
}));
const beatFrames = story.beats.map((beat, index) => {
  const next = story.beats[index + 1]?.offsetSec ?? duration;
  const event = visualEventForBeat(beat, next);
  let at = beat.offsetSec + Math.min(1.25, beat.durationSec * 0.36);
  if (event && ["spotlight", "outline"].includes(event.kind)) at = event.atSec + 1.05;
  else if (event?.kind === "drag") at = event.atSec + 0.15;
  else if (event?.kind === "click") at = Math.max(beat.offsetSec + 0.2, event.atSec - 0.12);
  return { ...beat, at, event, frame:extract(`beat-${beat.index + 1}`, at) };
});

const acFonts = resolve(HERE, "../../system/public/type/webfonts");
const statusRow = ([name, pass, evidence]) => `
${pass ? "\\pass" : "\\fail"} & \\textbf{${escapeTex(name)}} & ${escapeTex(evidence)} \\\\`;
const statusRows = validations.slice(0, 10).map(statusRow).join("\n");
const continuedStatusRows = validations.slice(10).map(statusRow).join("\n");
const beatPages = beatFrames.map((beat) => {
  const english = englishBeat(beat);
  const eventText = beat.event
    ? `${beat.event.kind} at ${beat.event.atSec.toFixed(2)}s`
    : "timed narration frame";
  return `
\\clearpage
\\section*{Beat ${beat.index + 1} \\hfill \\textcolor{acgray}{${beat.at.toFixed(2)}s}}
\\begin{center}
\\includegraphics[width=\\textwidth]{\\detokenize{${beat.frame}}}
\\end{center}
\\renewcommand{\\arraystretch}{1.35}
{\\fontsize{11.5pt}{15pt}\\selectfont
\\begin{tabularx}{\\textwidth}{@{}>{\\bfseries\\color{acgray}}p{1.08in}X@{}}
${nonEnglish ? "English script" : "Script"} & {\\fontsize{15pt}{19pt}\\selectfont ${escapeTex(english?.narration || beat.narration)}} \\\\
${nonEnglish ? `Caption (${escapeTex(story.locale)}) & ${storyTex(beat.narration)} \\\\\n` : ""}Logic & ${escapeTex(english?.logic || beat.logic || "—")} \\\\
Cursor & ${escapeTex(english?.cursorIntent || beat.cursorIntent || "—")} \\\\
Trace & ${escapeTex(eventText)} \\\\
\\end{tabularx}
}
`;
}).join("\n");
const cards = storyboardCards.map((card, index) => `
\\begin{minipage}[t]{0.48\\textwidth}
\\textbf{${escapeTex(card.role || card.card?.phase || `card ${index + 1}`)}}\\par\\smallskip
\\includegraphics[width=\\linewidth]{\\detokenize{${card.frame}}}\\par
\\scriptsize ${escapeTex((card.role === "opening"
    ? receiptEnglish?.openingCard?.title
    : receiptEnglish?.closingCard?.title) || card.card?.title || "Concept card")} · ${(card.atSec - card.durationSec).toFixed(2)}–${card.atSec.toFixed(2)}s
${nonEnglish ? `\\par\\scriptsize\\color{acgray} Filmed caption: ${storyTex(card.card?.title || "Concept card")}` : ""}
\\end{minipage}`).join("\\hfill\n");

const tex = `
\\documentclass[10pt,letterpaper]{article}
\\usepackage[top=0.62in,bottom=0.62in,left=0.68in,right=0.68in]{geometry}
\\usepackage{fontspec}
\\usepackage{xeCJK}
\\setmainfont{Avenir Next}
\\setsansfont{Avenir Next}
\\setCJKmainfont{Hiragino Sans GB}
\\newfontfamily\\storyfont{Kohinoor Devanagari}[Script=Devanagari]
\\newfontfamily\\acbold{ywft-processing-bold}[Path=${acFonts}/,Extension=.ttf]
\\newfontfamily\\aclight{ywft-processing-light}[Path=${acFonts}/,Extension=.ttf]
\\usepackage{xcolor,graphicx,tabularx,array,booktabs,fancyhdr,hyperref,pifont,titlesec}
\\definecolor{acpink}{RGB}{180,72,135}
\\definecolor{acpurple}{RGB}{120,80,180}
\\definecolor{acdark}{RGB}{40,36,48}
\\definecolor{acgray}{RGB}{110,106,116}
\\definecolor{acgreen}{RGB}{31,139,85}
\\definecolor{acred}{RGB}{192,55,65}
\\newcommand{\\pass}{\\textcolor{acgreen}{\\ding{52}}}
\\newcommand{\\fail}{\\textcolor{acred}{\\ding{56}}}
\\titleformat{\\section}{\\acbold\\Large\\color{acdark}}{}{0pt}{}
\\pagestyle{fancy}\\fancyhf{}\\renewcommand{\\headrulewidth}{0pt}
\\fancyhead[L]{\\scriptsize\\color{acpink} CAPTUTOR / STORYBOARD RECEIPT}
\\fancyhead[R]{\\scriptsize\\color{acgray} ${escapeTex(story.locale)} · ${escapeTex(story.format)}}
\\fancyfoot[C]{\\scriptsize\\thepage}
\\hypersetup{colorlinks=true,linkcolor=acpurple,urlcolor=acpurple,pdftitle={${escapeTex(story.title)} — storyboard receipt}}
\\setlength{\\parindent}{0pt}\\setlength{\\parskip}{0.35em}
\\begin{document}
\\begin{center}
{\\aclight\\fontsize{13pt}{15pt}\\selectfont\\color{acpink} STORYBOARD RECEIPT}\\par\\vspace{0.2em}
{\\acbold\\fontsize{27pt}{31pt}\\selectfont\\color{acdark} ${escapeTex(englishTitle)}}\\par
{\\large\\color{acgray} ${escapeTex(englishSubtitle)}}\\par
${nonEnglish ? `{\\small\\color{acgray} Filmed language: ${storyTex(story.title)} — ${storyTex(story.subtitle)}}\\par` : ""}\\vspace{0.5em}
{\\small ${escapeTex(story.locale)} · ${escapeTex(story.format)} · ${escapeTex(basename(video))}}\\par
\\vspace{0.45em}\\rule{\\textwidth}{1.1pt}
\\end{center}

\\begin{center}
{\\acbold\\fontsize{18pt}{20pt}\\selectfont\\color{${accepted ? "acgreen" : "acred"}} ${accepted ? "ACCEPTED" : "REVIEW REQUIRED"}}
\\end{center}

\\begin{tabularx}{\\textwidth}{@{}p{0.28in}p{1.78in}X@{}}
\\toprule & Check & Evidence \\\\ \\midrule
${statusRows}
\\bottomrule
\\end{tabularx}

${continuedStatusRows ? `
\\clearpage
\\section*{Validation evidence continued}
\\begin{tabularx}{\\textwidth}{@{}p{0.28in}p{1.78in}X@{}}
\\toprule & Check & Evidence \\\\ \\midrule
${continuedStatusRows}
\\bottomrule
\\end{tabularx}
` : ""}

\\section*{Programmed cards}
${cards || "No signboard events recorded."}

\\section*{Media receipt}
\\begin{tabularx}{\\textwidth}{@{}>{\\bfseries}p{1.4in}X>{\\bfseries}p{1.2in}X@{}}
Resolution & ${videoStream?.width}×${videoStream?.height} & Duration & ${duration.toFixed(2)} s \\\\
Video & ${escapeTex(videoStream?.codec_name)} ${escapeTex(videoStream?.r_frame_rate)} & Audio & ${escapeTex(audioStream?.codec_name)} ${escapeTex(audioStream?.sample_rate)} Hz \\\\
Credits & ${story.credits ? `${story.credits.before} → ${story.credits.after} (${story.credits.spent} spent)` : "not metered"} & Beats & ${story.beats.length} \\\\
\\end{tabularx}
${beatPages}
\\clearpage
\\section*{Trace inventory}
\\small This receipt was generated from \\texttt{captutor-storyboard/v1}. It records ${story.events.length} timed events and ${checks.size} business-logic checks. Frame evidence is extracted from the accepted MP4 at trace-derived times; it is not reconstructed from DOM screenshots.\\par
\\vfill
{\\color{acgray}\\scriptsize Generated ${escapeTex(story.createdAt)} · Aesthetic Computer / Captutor · SHA is carried by the paired outbox manifest.}
\\end{document}
`;

const texPath = join(work, "receipt.tex");
writeFileSync(texPath, tex);
mkdirSync(dirname(out), { recursive:true });
const built = join(work, "receipt.pdf");
const requestedEngine = process.env.CAPTUTOR_RECEIPT_ENGINE || "auto";
const hasXelatex = commandWorks("xelatex");
let engine = requestedEngine === "auto" ? (hasXelatex ? "xelatex" : "chrome") : requestedEngine;

async function buildWithChrome() {
  const chrome = findChrome();
  if (!chrome) {
    throw new Error("receipt needs xelatex or Google Chrome/Chromium; neither is available");
  }
  const validationRows = validations.map(([name, pass, evidence]) => `
    <tr><td class="mark ${pass ? "pass" : "fail"}">${pass ? "✓" : "×"}</td>
      <th>${escapeHtml(name)}</th><td>${escapeHtml(evidence)}</td></tr>`).join("");
  const cardBlocks = storyboardCards.map((card, index) => `
    <figure><img src="${imageData(card.frame)}" alt="${escapeHtml(card.role || `card ${index + 1}`)}">
      <figcaption>${escapeHtml((card.role === "opening"
        ? receiptEnglish?.openingCard?.title
        : receiptEnglish?.closingCard?.title) || card.card?.title || "Concept card")}</figcaption></figure>`).join("");
  const beatBlocks = beatFrames.map((beat) => {
    const english = englishBeat(beat);
    const logic = english?.logic || beat.logic || null;
    const cursor = english?.cursorIntent || beat.cursorIntent || null;
    const eventText = beat.event
      ? `${beat.event.kind} at ${beat.event.atSec.toFixed(2)}s`
      : "timed narration frame";
    return `<section class="page beat">
      <header><h1>Beat ${beat.index + 1}</h1><time>${beat.at.toFixed(2)}s</time></header>
      <img class="evidence" src="${imageData(beat.frame)}" alt="Frame evidence for beat ${beat.index + 1}">
      <dl>
        <dt class="script-label">${nonEnglish ? "English script" : "Script"}</dt><dd class="script">${escapeHtml(english?.narration || beat.narration)}</dd>
        ${nonEnglish ? `<dt>Caption (${escapeHtml(story.locale)})</dt><dd>${escapeHtml(beat.narration)}</dd>` : ""}
        ${logic ? `<dt>Logic</dt><dd>${escapeHtml(logic)}</dd>` : ""}
        ${cursor ? `<dt>Cursor</dt><dd>${escapeHtml(cursor)}</dd>` : ""}
        <dt>Trace</dt><dd>${escapeHtml(eventText)}</dd>
      </dl>
    </section>`;
  }).join("");
  const html = `<!doctype html><html lang="en"><head><meta charset="utf-8">
    <title>${escapeHtml(englishTitle)} — storyboard receipt</title>
    <style>
      @page { size: letter; margin: 0.46in 0.52in; }
      * { box-sizing: border-box; }
      body { margin: 0; color: #282430; font: 12px/1.35 -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif; }
      .page { break-after: page; min-height: 9.8in; }
      .page:last-child { break-after: auto; }
      h1, h2, p { margin: 0; }
      .cover > header { text-align: center; border-bottom: 2px solid #b44887; padding: 0.2in 0 0.18in; }
      .cover h1 { font-size: 28px; line-height: 1.05; }
      .subtitle { color: #6e6a74; font-size: 16px; margin-top: 6px; }
      .meta { color: #6e6a74; margin-top: 8px; }
      .verdict { color: ${accepted ? "#1f8b55" : "#c03741"}; font-size: 22px; font-weight: 800; text-align: center; margin: 18px 0 12px; }
      table { border-collapse: collapse; width: 100%; }
      th, td { border-bottom: 1px solid #ddd9e1; padding: 5px 7px; text-align: left; vertical-align: top; }
      th { width: 27%; }
      .mark { width: 24px; font-size: 16px; font-weight: 800; }
      .pass { color: #1f8b55; } .fail { color: #c03741; }
      h2 { font-size: 16px; margin: 16px 0 7px; }
      .cards { display: flex; gap: 12px; }
      figure { flex: 1; margin: 0; }
      figure img { display: block; width: 100%; max-height: 2.05in; object-fit: contain; border: 1px solid #ddd9e1; }
      figcaption { color: #6e6a74; margin-top: 4px; }
      .media { display: grid; grid-template-columns: max-content 1fr max-content 1fr; gap: 4px 10px; }
      .media b { color: #6e6a74; }
      .beat header { display: flex; align-items: baseline; justify-content: space-between; margin-bottom: 14px; }
      .beat h1 { font-size: 30px; line-height: 1; }
      .beat time { color: #6e6a74; font-size: 16px; font-weight: 650; }
      .evidence { display: block; width: 100%; height: auto; aspect-ratio: 16 / 9; object-fit: cover; }
      dl { display: grid; grid-template-columns: 0.95in 1fr; gap: 9px 12px; margin: 18px 0 0; }
      dt { color: #6e6a74; font-size: 13px; font-weight: 750; letter-spacing: 0.01em; }
      dd { margin: 0; font-size: 14px; line-height: 1.42; }
      .script-label { color: #b44887; font-size: 14px; }
      .script { font-size: 18px; line-height: 1.38; font-weight: 620; padding-bottom: 5px; }
    </style></head><body>
    <section class="page cover"><header><h1>${escapeHtml(englishTitle)}</h1>
      <p class="subtitle">${escapeHtml(englishSubtitle)}</p>
      <p class="meta">${escapeHtml(story.locale)} · ${escapeHtml(story.format)} · ${escapeHtml(basename(video))}</p></header>
      <p class="verdict">${accepted ? "ACCEPTED" : "REVIEW REQUIRED"}</p>
      <table><tbody>${validationRows}</tbody></table>
      <h2>Programmed cards</h2><div class="cards">${cardBlocks || "No signboard events recorded."}</div>
      <h2>Media receipt</h2><div class="media">
        <b>Resolution</b><span>${videoStream?.width}×${videoStream?.height}</span>
        <b>Duration</b><span>${duration.toFixed(2)} s</span>
        <b>Video</b><span>${escapeHtml(videoStream?.codec_name)} ${escapeHtml(videoStream?.r_frame_rate)}</span>
        <b>Audio</b><span>${escapeHtml(audioStream?.codec_name)} ${escapeHtml(audioStream?.sample_rate)} Hz</span>
        <b>Beats</b><span>${story.beats.length}</span><b>Trace events</b><span>${story.events.length}</span>
      </div></section>${beatBlocks}</body></html>`;
  const htmlPath = join(work, "receipt.html");
  writeFileSync(htmlPath, html);
  const child = spawn(chrome, [
    "--headless=new", "--disable-gpu", "--disable-dev-shm-usage",
    `--user-data-dir=${join(work, "chrome-profile")}`,
    "--no-pdf-header-footer", `--print-to-pdf=${built}`,
    pathToFileURL(htmlPath).href,
  ], { stdio:["ignore", "ignore", "pipe"] });
  let stderr = "";
  let exited = false;
  child.stderr.on("data", (chunk) => {
    stderr = (stderr + chunk).slice(-8_000);
  });
  child.once("exit", () => { exited = true; });

  // Chrome on macOS can finish --print-to-pdf yet retain an idle browser
  // process. Treat a stable, valid PDF as completion and reap only the
  // isolated process that owns this receipt's temporary profile.
  const deadline = Date.now() + 30_000;
  let lastSize = -1;
  let stableReads = 0;
  while (Date.now() < deadline) {
    if (existsSync(built)) {
      const bytes = readFileSync(built);
      if (bytes.length > 1_000 && bytes.subarray(0, 4).toString() === "%PDF") {
        stableReads = bytes.length === lastSize ? stableReads + 1 : 0;
        lastSize = bytes.length;
        if (stableReads >= 2) break;
      }
    }
    if (exited && !existsSync(built)) break;
    await delay(120);
  }
  const valid = existsSync(built) && readFileSync(built).length > 1_000 &&
    readFileSync(built).subarray(0, 4).toString() === "%PDF";
  if (!exited) {
    child.kill("SIGTERM");
    for (let wait = 0; wait < 10 && !exited; wait += 1) await delay(100);
    if (!exited) child.kill("SIGKILL");
  }
  if (!valid) {
    throw new Error(`Chrome receipt build failed: ${stderr || "no valid PDF was written"}`);
  }
}

try {
  if (engine === "xelatex") {
    if (!hasXelatex) throw new Error("CAPTUTOR_RECEIPT_ENGINE=xelatex but xelatex is unavailable");
    for (let pass = 0; pass < 2; pass += 1) {
      run("xelatex", ["-interaction=nonstopmode", "-halt-on-error", "-output-directory", work, texPath]);
    }
  } else if (engine === "chrome") {
    await buildWithChrome();
  } else {
    throw new Error(`unknown receipt engine: ${engine}`);
  }
  writeFileSync(out, readFileSync(built));
  console.log(JSON.stringify({
    out, accepted, engine,
    validations:validations.map(([name, pass]) => ({ name, pass })),
  }, null, 2));
} finally {
  rmSync(work, { recursive:true, force:true });
}
