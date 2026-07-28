#!/usr/bin/env node
import { mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const REEL = resolve(HERE, "..");
const REV = resolve(HERE, "revision-03");
const OUT = resolve(REV, "scores-for-social-software-r03-receipt.html");
const spec = JSON.parse(readFileSync(resolve(REEL, "narrator-spec.json"), "utf8"));
const timeline = JSON.parse(readFileSync(resolve(REEL, "out/narration-timeline.json"), "utf8"));
const timingById = new Map(timeline.lines.map((line) => [line.id, line]));

const REVIEW_VIDEO = "https://drive.google.com/file/d/17jMTkVX7OCekLxKq8X1-4iRLrWzMA0E5/view";
const REVIEW_DOC = "https://docs.google.com/document/d/13YiibAH8Jfsyq-qZ3ugHpxF6XulM-hY8GDo_f854Hd0/edit";
const ARTICLE = "https://sosoft.arts.ucla.edu/keymaps-as-social-software/";
const FINAL_SOURCE = "https://docs.google.com/document/d/1hNzUm3SmsEBRtM3zWhcQqsYvsoRf4ZioFIQMFndlwXY/edit";

const esc = (value) => value
  .replaceAll("&", "&amp;")
  .replaceAll("<", "&lt;")
  .replaceAll(">", "&gt;")
  .replaceAll('"', "&quot;");
const stamp = (seconds) => {
  const rounded = Math.round(seconds);
  return `${String(Math.floor(rounded / 60)).padStart(2, "0")}:${String(rounded % 60).padStart(2, "0")}`;
};

const scenes = spec.lines.map((line) => {
  const timing = timingById.get(line.id);
  const image = `frames/${line.id.toLowerCase()}.jpg`;
  return `<section class="scene">
  <h2>${line.id} · ${stamp(timing.startSec)}–${stamp(timing.endSec)}<br>${esc(line.title)}</h2>
  <img src="${image}" alt="Video frame for ${esc(line.title)}">
  <p><strong>Narration.</strong> ${esc(line.text)}</p>
  <p class="comment">Comment with ${line.id} to request a script, timing, image, or crop change.</p>
</section>`;
}).join("\n");

const html = `<!doctype html>
<html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">
<title>Scores for Social Software — revision 03 video receipt</title>
<style>
@page{size:Letter;margin:1in}*{box-sizing:border-box}body{max-width:7in;margin:0 auto;padding:1in;font:11pt/1.15 Arial,sans-serif;color:#000}h1{font-size:24pt;margin:0 0 10pt}h2{font-size:14pt;line-height:1.2;margin:14pt 0 8pt}h3{font-size:16pt;margin:18pt 0 8pt}p{margin:0 0 8pt}.meta,.comment{color:#5a5a5a}.links a{display:block;margin:0 0 8pt;color:#1155cc}.scene{break-before:page;page-break-before:always}.scene img{display:block;width:2.55in;height:auto;margin:0 auto 12pt}.comment{font-style:italic}@media(max-width:700px){body{padding:24px}.scene{break-before:auto}.scene img{max-width:55vw}}
</style></head><body>
<h1>Scores for Social Software</h1>
<p><strong>Revision 03 video receipt + timecoded screenplay</strong></p>
<p class="meta">Review cut · 02:55 · 1080 × 1920 vertical · prepared 27 July 2026</p>
<h3>Review links</h3>
<p class="links"><a href="${REVIEW_VIDEO}">Watch the revision 03 review video</a><a href="${REVIEW_DOC}">Comment on the Google Doc screenplay</a><a href="${FINAL_SOURCE}">Open the final article source + Casey edit log</a><a href="${ARTICLE}">Open the published article: The Keymap Is the Score</a></p>
<h3>How to review</h3>
<p>Comment on the words or image you want changed and include the stable scene ID. Script, timing, image choice, and crop notes are all welcome.</p>
<p>The current voice is a temporary guide. This screenplay is the review surface before Jeffrey records the final narration in Narrator Wizard.</p>
<h3>What changed in revision 03</h3>
<p><strong>Casey’s copy feedback.</strong> Corrected the Æther description, replaced the limiting “same question” conclusion, and incorporated the accepted Google Doc copy edits into the published article and screenplay.</p>
<p><strong>Event documentation.</strong> The closing now moves through Casey’s June 13 Fuser photographs: the room, presentation, cohort, audience, and floor performance.</p>
${scenes}
</body></html>`;

mkdirSync(REV, { recursive: true });
writeFileSync(OUT, html);
console.log(OUT);
