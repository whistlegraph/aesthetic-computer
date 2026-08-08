// Build `oskiewar pads.adg`, the Ableton Drum Rack that receives the game.
//
// The pad layout is not written down here — it comes from `oskiewar-midi.mjs`,
// which derives it from OSKIEWAR_SIGNAL_EVENTS. So the rack and the bridge
// cannot drift: add a gameSignal event, re-run this, and the new pad lands on
// the note the bridge already sends.
//
// An .adg is gzipped XML. Rather than author that schema by hand, this clones a
// branch out of an existing rack and rewrites its label and note — so it needs
// a rack to start from, and can use its own last output as that template.
//
//   node make-oskiewar-pads.mjs <template.adg> [out.adg]
//
// Existing pads keep their sounds only if their note is unchanged; the labels
// and notes are rewritten from the bridge every time.

import { readFileSync, writeFileSync } from "node:fs";
import { gzipSync, gunzipSync } from "node:zlib";
import createOskiewarMidi, { themePadName } from "./oskiewar-midi.mjs";

const [, , templatePath, outPath = "oskiewar pads.adg"] = process.argv;
if (!templatePath) {
  console.error("usage: node make-oskiewar-pads.mjs <template.adg> [out.adg]");
  process.exit(1);
}

const BRANCH = /<DrumBranchPreset\b/g;
const xml = gunzipSync(readFileSync(templatePath)).toString("utf8");

// Slice the document into head, the branches, and the tail after them.
const starts = [...xml.matchAll(BRANCH)].map((m) => m.index);
if (!starts.length) throw new Error("template has no drum branches");
const endOfBranches = xml.indexOf("</BranchPresets>");
if (endOfBranches < 0) throw new Error("template has no </BranchPresets>");
const head = xml.slice(0, starts[0]);
const tail = xml.slice(endOfBranches);
const template = xml.slice(starts[0], starts[1] ?? endOfBranches);

// Whatever the template branch happens to be called is what gets rewritten.
const label = /<Name Value="([^"]*)"/.exec(template)?.[1];
const note = /<ReceivingNote Value="(\d+)"/.exec(template)?.[1];
if (!label || !note) throw new Error("template branch has no name or note");

// Replace only the first match — the second Annotation belongs to the mixer.
function replaceFirst(text, pattern, replacement) {
  const found = pattern.exec(text);
  if (!found) throw new Error(`template is missing ${pattern}`);
  return text.slice(0, found.index) + replacement +
    text.slice(found.index + found[0].length);
}

function branchFor({ pad, note: target, kind }, index) {
  let out = template
    .replace(/^<DrumBranchPreset Id="\d+">/, `<DrumBranchPreset Id="${index}">`)
    .split(`Value="${label}"`).join(`Value="${pad}"`)
    .split(`<ReceivingNote Value="${note}" />`).join(`<ReceivingNote Value="${target}" />`)
    .split(`<SendingNote Value="${note}" />`).join(`<SendingNote Value="${target}" />`);
  out = replaceFirst(out, /<Annotation Value="[^"]*"/,
    `<Annotation Value="${kind === "theme"
      ? `held while the ${pad.split(" ")[0]} section lasts — MIDI note ${target}`
      : `/oskiewar/${pad} — MIDI note ${target}`}"`);
  if (!out.includes(`<ReceivingNote Value="${target}" />`)) {
    throw new Error(`could not set note ${target} for ${pad}`);
  }
  return out;
}

const pads = createOskiewarMidi().chart();
const body = pads.map(branchFor).join("");

const rackName = "oskiewar pads";
const signals = pads.filter((p) => p.kind === "signal");
const themes = pads.filter((p) => p.kind === "theme");
const annotation = `${signals.length} signal pads on notes ` +
  `${signals[0].note}-${signals.at(-1).note}, ${themes.length} held theme pads on ` +
  `${themes[0].note}-${themes.at(-1).note}. velocity is intensity. ` +
  `drop a sample or instrument on each labeled pad.`;

const rackLabel = /<UserName Value="([^"]*)"/.exec(head)?.[1];
let outHead = replaceFirst(head, /<UserName Value="[^"]*"/,
  `<UserName Value="${rackName}"`);
outHead = replaceFirst(outHead, /<Annotation Value="[^"]*"/,
  `<Annotation Value="${annotation}"`);

const built = outHead + body + tail;
writeFileSync(outPath, gzipSync(Buffer.from(built, "utf8"), { level: 9 }));
console.log(`${templatePath} (${starts.length} pads, rack "${rackLabel}")`);
console.log(`-> ${outPath}: ${pads.length} pads ` +
  `(${signals.length} signals ${signals[0].note}-${signals.at(-1).note}, ` +
  `${themes.length} themes ${themes[0].note}-${themes.at(-1).note})`);
for (const t of themes) console.log(`   ${t.note}  ${themePadName(t.event)}`);
