#!/usr/bin/env node
// als-template.mjs — build a stripped-down Ableton Live template .als from the blank Live 12 set.
//
//   node als-template.mjs                              # 1 audio track + Main, no MIDI, no returns
//   node als-template.mjs --audio 1 --midi 0 --returns 0 --out AC-Blank.als
//   node als-template.mjs --xml-out AC-Blank.xml       # also emit the decompressed XML to inspect
//
// WHY THIS EXISTS
// Live 12's stock "New Live Set" gives you 2 MIDI + 2 audio + 2 return tracks. @jeffrey wants a
// truly blank default — one input channel + the Main out, nothing else — as the fleet's default
// Set. Live has no CLI for this and its UI can't be driven headlessly (locked/among other work),
// so we shape the .als directly. The repo already reads .als (reference/ableton/*, bios.mjs,
// wipppps.mjs) but had no writer; this is the minimal one.
//
// HOW
// An .als is gzipped UTF-8 XML rooted at <Ableton><LiveSet>. reference/ableton/live-12-blank.xml
// is that XML, already decompressed — a stock blank set. We remove whole <MidiTrack>/<AudioTrack>/
// <ReturnTrack> elements down to the requested counts, then fix the one dependent invariant: every
// track carries one <TrackSendHolder> per return track, so surviving tracks' <Sends> are truncated
// to match the new return count (emptied when 0 returns). Everything else is self-contained —
// TracksListWrapper/SendsListWrapper are empty LomId="0" stubs, and the only stray Id="12/13/14"
// hits are device-macro locals, not track references — so no other patching is needed.
//
// Verify by opening the result in Live (it is the real test for hand-built XML); the companion
// fleet installer does that before committing it as the default.
import { readFile, writeFile } from "node:fs/promises";
import { gzipSync } from "node:zlib";
import { fileURLToPath } from "node:url";
import { dirname, join } from "node:path";

const HERE = dirname(fileURLToPath(import.meta.url));
const BLANK = join(HERE, "live-12-blank.xml");

// Extract the full text of the element that opens at `openIdx` (index of its "<Tag").
// Tracks and Sends never nest inside an element of their own name, so first matching close wins.
function elementSpan(xml, tag, openIdx) {
  const close = `</${tag}>`;
  const end = xml.indexOf(close, openIdx);
  if (end === -1) throw new Error(`no ${close} after index ${openIdx}`);
  return { start: openIdx, end: end + close.length };
}

// All top-level occurrences of `<Tag ` (open tag with attributes) within [from, to).
function findOpens(xml, tag, from, to) {
  const opens = [];
  const needle = `<${tag} `;
  let i = xml.indexOf(needle, from);
  while (i !== -1 && i < to) {
    opens.push(i);
    // skip past this element so we don't match a same-named element nested inside it
    const { end } = elementSpan(xml, tag, i);
    i = xml.indexOf(needle, end);
  }
  return opens;
}

// Truncate a single track's <Sends> to `returns` holders, renumbered 0..returns-1.
function fixSends(trackXml, returns) {
  const open = trackXml.indexOf("<Sends>");
  if (open === -1) return trackXml; // e.g. the PreHear/Main have none
  const { end } = elementSpan(trackXml, "Sends", open);
  const holders = findOpens(trackXml, "TrackSendHolder", open, end);
  const indent = "\t\t\t\t\t\t"; // Sends sits six tabs deep in the stock file

  if (returns === 0) {
    return trackXml.slice(0, open) + "<Sends />" + trackXml.slice(end);
  }
  const kept = holders.slice(0, returns).map((h, n) => {
    const span = elementSpan(trackXml, "TrackSendHolder", h);
    // renumber Id="n" on the holder's own open tag only
    const text = trackXml.slice(span.start, span.end);
    return text.replace(/^<TrackSendHolder Id="\d+"/, `<TrackSendHolder Id="${n}"`);
  });
  const rebuilt =
    "<Sends>\n" +
    kept.map((k) => `${indent}\t${k}`).join("\n") +
    `\n${indent}</Sends>`;
  return trackXml.slice(0, open) + rebuilt + trackXml.slice(end);
}

function buildTemplate(xml, { audio, midi, returns }) {
  const tOpen = xml.indexOf("<Tracks>");
  const tClose = xml.indexOf("</Tracks>", tOpen);
  const head = xml.slice(0, tOpen + "<Tracks>".length);
  const tail = xml.slice(tClose);
  const body = xml.slice(tOpen + "<Tracks>".length, tClose);

  // Collect every track element in document order with its type.
  const wanted = { MidiTrack: midi, AudioTrack: audio, ReturnTrack: returns };
  const spans = [];
  for (const tag of ["MidiTrack", "AudioTrack", "ReturnTrack", "GroupTrack"]) {
    for (const rel of findOpens(body, tag, 0, body.length)) {
      spans.push({ tag, ...elementSpan(body, tag, rel) });
    }
  }
  spans.sort((a, b) => a.start - b.start);

  const seen = { MidiTrack: 0, AudioTrack: 0, ReturnTrack: 0, GroupTrack: 0 };
  const kept = [];
  for (const s of spans) {
    const quota = wanted[s.tag] ?? 0;
    if (seen[s.tag] < quota) {
      let text = body.slice(s.start, s.end);
      text = fixSends(text, returns); // surviving track's send count must match returns
      kept.push(text);
    }
    seen[s.tag] += 1;
  }

  const rebuiltBody = "\n\t\t\t" + kept.join("\n\t\t\t") + "\n\t\t";
  return head + rebuiltBody + tail;
}

const argv = process.argv.slice(2);
const opt = (n, d) => (argv.includes(n) ? argv[argv.indexOf(n) + 1] : d);
const audio = Number(opt("--audio", "1"));
const midi = Number(opt("--midi", "0"));
const returns = Number(opt("--returns", "0"));
const out = opt("--out", join(HERE, "AC-Blank.als"));
const xmlOut = opt("--xml-out", null);

const blank = await readFile(BLANK, "utf8");
const xml = buildTemplate(blank, { audio, midi, returns });

const counts = (s) => ["MidiTrack", "AudioTrack", "ReturnTrack"]
  .map((t) => `${t.replace("Track", "")}=${(s.match(new RegExp(`<${t} `, "g")) || []).length}`)
  .join(" ");
console.log(`tracks after strip: ${counts(xml)}`);

if (xmlOut) {
  await writeFile(xmlOut, xml);
  console.log(`wrote XML  ${xmlOut}`);
}
// mtime 0 keeps the gzip byte-identical across runs (deterministic, easy to diff/verify).
await writeFile(out, gzipSync(Buffer.from(xml, "utf8"), { mtime: 0 }));
console.log(`wrote .als ${out}`);
