#!/usr/bin/env node
// Build a /papers-style visual receipt from an accepted MP4 + Captutor trace.

import { execFileSync, spawnSync } from "node:child_process";
import {
  existsSync, mkdirSync, mkdtempSync, readFileSync, rmSync, writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { basename, dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

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
    "Introduction length", duration >= acceptance.minimumDurationSec,
    `${duration.toFixed(2)} s; minimum ${acceptance.minimumDurationSec} s`,
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
\\fbox{\\includegraphics[width=0.96\\textwidth,height=0.53\\textheight,keepaspectratio]{\\detokenize{${beat.frame}}}}
\\end{center}
\\begin{tabularx}{\\textwidth}{@{}>{\\bfseries}p{0.95in}X@{}}
English & ${escapeTex(english?.narration || beat.narration)} \\\\
${nonEnglish ? `Caption (${escapeTex(story.locale)}) & ${storyTex(beat.narration)} \\\\\n+` : ""}Logic & ${escapeTex(english?.logic || beat.logic || "—")} \\\\
Cursor & ${escapeTex(english?.cursorIntent || beat.cursorIntent || "—")} \\\\
Trace & ${escapeTex(eventText)} \\\\
\\end{tabularx}
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
for (let pass = 0; pass < 2; pass += 1) {
  run("xelatex", ["-interaction=nonstopmode", "-halt-on-error", "-output-directory", work, texPath]);
}
const built = join(work, "receipt.pdf");
writeFileSync(out, readFileSync(built));
console.log(JSON.stringify({ out, accepted, validations:validations.map(([name, pass]) => ({ name, pass })) }, null, 2));
rmSync(work, { recursive:true, force:true });
