#!/usr/bin/env node
import { mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";

const ROOT = dirname(fileURLToPath(import.meta.url));
const REV = resolve(ROOT, "revision-02");
const QA = resolve(ROOT, "../qa");
const OUT = resolve(REV, "scores-for-social-software-r02-receipt.html");
const REVIEW_VIDEO = "https://drive.google.com/file/d/17jMTkVX7OCekLxKq8X1-4iRLrWzMA0E5/view";
const ARTICLE = "https://sosoft.arts.ucla.edu/keymaps-as-social-software/";
const FINAL_SOURCE = "https://docs.google.com/document/d/1hNzUm3SmsEBRtM3zWhcQqsYvsoRf4ZioFIQMFndlwXY/edit";

const beats = [
  ["SSF-00", "Introduction", "00:00–00:18", "SSF-00-introduction.jpg", "This blue folder just arrived from Social Software at UCLA. Inside is Scores for Social Software, a spring 2026 edition of sixty-four. The copy in my hands is number fifty-one, assembled after ten weeks of making, testing, and performing together."],
  ["SSF-01", "Jeffrey Alan Scudder — Notepat", "00:18–00:31", "SSF-01-notepat.jpg", "My contribution is Notepat. A folded white user manual opens into a pointed shape, combining an illustrated player, a QR code, instructions, and circular keyboard diagrams."],
  ["SSF-02", "Æther Cavendish — Vigil Score", "00:31–00:38", "SSF-02-vigil-score.jpg", "Æther Cavendish’s Vigil Score arrives as a matte-black folded packet, closed with a small circular silver seal."],
  ["SSF-03", "Chelly Jin — Software as a Choreography", "00:38–00:48", "SSF-03-software-as-choreography.jpg", "Chelly Jin’s Software as a Choreography presents a grid of silhouetted hands and arms, pairing each gesture with marks for timing and movement."],
  ["SSF-04", "Jordan Silver — Sonic Architecture", "00:48–01:01", "SSF-04-sonic-architecture.jpg", "Jordan Silver’s Sonic Architecture is a single sheet of printed columns, large numbers, and typewritten commands, anchored by a looping spiral diagram for inhabiting and measuring space through sound."],
  ["SSF-05", "Em Lugo — Cues for Losing Direction", "01:01–01:11", "SSF-05-cues-for-losing-direction.jpg", "Em Lugo’s Cues for Losing Direction fits on a small black card, its title set in pale condensed type like a portable instruction carried in the hand."],
  ["SSF-06", "Darlyn Phan — Line Piece 1", "01:11–01:22", "SSF-06-line-piece-1.jpg", "Darlyn Phan’s Line Piece 1 begins on translucent white paper. Its faint rainbow cast and minimal title let the page behave like a line or veil."],
  ["SSF-07", "Thomas Noya — Biophonía", "01:22–01:34", "SSF-07-biophonia.jpg", "Thomas Noya’s Biophonía is a branching field of blue, yellow, black, and beige organic forms. In the video, these cell-like clusters drift and reorganize."],
  ["SSF-08", "Banyi Huang — A Cosmographic Score…", "01:34–01:44", "SSF-08-cosmographic-score.jpg", "Banyi Huang’s A Cosmographic Score for Folding Back into the Kernel centers a luminous circle and a small diagram of connected nodes inside a lavender field."],
  ["SSF-09", "Alexander Espinosa — Music for World Computers", "01:44–01:57", "SSF-09-music-for-world-computers.jpg", "Alexander Espinosa’s Music for World Computers is a white typographic score: awake, expand, decrease, oxygen, forest, junk, cinnamon, hammer."],
  ["SSF-10", "Mavyn Vu — The Radio Is an Altar: Portal", "01:57–02:07", "SSF-10-radio-altar-portal.jpg", "Mavyn Vu’s The Radio Is an Altar: Portal combines translucent blue and white score cards, a target-like radio image, small figures, and instructions arranged around their edges."],
  ["SSF-11", "Closing", "02:07–02:21", "SSF-11-closing.jpg", "Casey Reas facilitated the cycle, with Lauren Lee McCarthy and the Social Software community. Together, the contributions open many paths through a question: if software organizes behavior, what else can we ask it to organize?"],
];

function esc(value) {
  return value.replaceAll("&", "&amp;").replaceAll("<", "&lt;").replaceAll(">", "&gt;");
}

const scenes = beats.map(([id, title, time, image, narration]) => `
  <section class="scene">
    <h2>${id} · ${esc(title)} <span>${time}</span></h2>
    <table role="presentation"><tr>
      <td class="still"><img src="${pathToFileURL(resolve(QA, image)).href}" alt="${esc(title)} video frame"></td>
      <td class="script"><h3>Narration</h3><p>${esc(narration)}</p><h3>Requested changes</h3><p class="request">Add a comment here and name <strong>${id}</strong>.</p></td>
    </tr></table>
  </section>`).join("\n");

const html = `<!doctype html>
<html><head><meta charset="utf-8"><title>Scores for Social Software — revision 02 video receipt</title>
<style>
  @page { size: Letter; margin: 1in; }
  body { font-family: Arial, sans-serif; font-size: 11pt; line-height: 1.35; color: #000; }
  h1 { font-size: 24pt; line-height: 1.1; margin: 0 0 8pt; font-weight: 700; }
  h2 { font-size: 14pt; margin: 20pt 0 8pt; page-break-after: avoid; border-bottom: 1px solid #bbb; padding-bottom: 4pt; }
  h2 span { float: right; font-size: 10pt; font-weight: 400; color: #555; }
  h3 { font-size: 11pt; margin: 0 0 5pt; }
  p { margin: 0 0 9pt; }
  a { color: #1155cc; text-decoration: underline; }
  .meta { color: #555; margin-bottom: 16pt; }
  .lead { font-size: 12pt; }
  .links { border-left: 4px solid #f6cd3f; padding-left: 10pt; margin: 14pt 0 20pt; }
  ul { margin-top: 5pt; }
  .scene { page-break-inside: avoid; }
  table { width: 100%; border-collapse: collapse; }
  td { vertical-align: top; }
  .still { width: 2.35in; padding-right: 18pt; }
  .still img { width: 2.15in; height: auto; }
  .script { width: 4in; }
  .request { min-height: 44pt; background: #fff8d8; border: 1px solid #ead88a; padding: 7pt; color: #555; }
  .footer-note { margin-top: 22pt; border-top: 1px solid #bbb; padding-top: 10pt; }
</style></head><body>
<h1>Scores for Social Software</h1>
<p class="lead"><strong>Revision 02 video receipt + timecoded screenplay</strong></p>
<p class="meta">Review cut · 02:21 · 1080 × 1920 vertical · prepared 27 July 2026</p>
<div class="links"><p><strong><a href="${REVIEW_VIDEO}">Watch the revision 02 review video</a></strong></p><p><a href="${FINAL_SOURCE}">Open the final article source + Casey edit log</a></p><p><a href="${ARTICLE}">Open the published article: The Keymap Is the Score</a></p></div>
<h2>How to review</h2>
<p>Comment on the words or image you want changed and include the stable scene ID—for example, <strong>SSF-04</strong>. Scene IDs persist even when later edits move the timestamps.</p>
<h2>What changed since revision 01</h2>
<ul><li><strong>SSF-02:</strong> corrected Æther Cavendish’s Vigil Score to the matte-black folded packet visible in the film.</li><li><strong>SSF-04:</strong> replaced the provisional Jordan Silver description with the observed single-sheet layout, printed columns, large numbers, typewritten commands, and spiral diagram.</li><li><strong>SSF-11:</strong> replaced “every contribution asks the same question” with a close that preserves the distinct entry offered by every contribution.</li><li>Re-timed all scenes and rebuilt the lower information panel with exact captions, current artist/work labels, and a chapter timeline.</li></ul>
<h2>Timecoded screenplay</h2>
${scenes}
<p class="footer-note"><strong>Revision chain:</strong> comment → accepted note → revised scene → new receipt. Accepted comments are copied to the repository’s <em>accepted-feedback.md</em> ledger under the same scene ID before revision 03 begins.</p>
</body></html>`;

mkdirSync(REV, { recursive: true });
writeFileSync(OUT, html);
console.log(OUT);
