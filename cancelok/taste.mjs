// taste.mjs — read the verdicts, write the pheromone.
//
// This is the only part of cancelok that holds an opinion, and it doesn't hold
// one of its own: it just reports back what @jeffrey's thumbs already said. The
// ant colony's pheromones.log carries "did the tests pass." This carries "did a
// person want it," which is the one thing no script can measure.
//
// The output is a block of prose that gets pasted into the generator's prompt.
// Prose, not JSON, because the reader is a language model and the thing we want
// it to do — notice a pattern in what got kept — is a reading task.

import { readFileSync, writeFileSync, existsSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
export const REPO = resolve(HERE, "..");
export const VERDICTS = resolve(HERE, "verdicts.jsonl");
export const QUEUE = resolve(HERE, "queue.json");

export const API = process.env.CANCELOK_API || "https://aesthetic.computer/api/cancelok";
export const KIN = resolve(HERE, "lineage.json");

// Who begat whom. Without this a bred pad is indistinguishable from a fresh one
// after the fact, and the only interesting question about an evolving system —
// is the line actually IMPROVING, or just drifting — becomes unanswerable.
export function lineage() {
  if (!existsSync(KIN)) return {};
  try {
    return JSON.parse(readFileSync(KIN, "utf8"));
  } catch {
    return {};
  }
}

export function remember(code, kin) {
  const all = lineage();
  all[code] = kin;
  writeFileSync(KIN, JSON.stringify(all, null, 2) + "\n");
}

// How many generations back does this pad go? A line that keeps getting kept is
// the whole point; a line that dies at depth 1 every time means the mutations are
// destroying whatever earned the parent its keep.
export function depth(code, kin = lineage()) {
  let d = 0;
  let at = code;
  while (kin[at]?.parent && d < 50) {
    at = kin[at].parent;
    d += 1;
  }
  return d;
}

// The crowd's thumbs, from the live database. This is the whole reason the game
// is on the internet instead of on a laptop: the generator should be learning
// from everyone who judged, not just from whoever happened to be sitting here.
//
// The API hands back tallies (per pad: ok, cancel, taps, judges) rather than raw
// rows, which is exactly the shape taste wants — one person's ok is an opinion,
// forty is a fact.
export async function crowd() {
  try {
    const res = await fetch(API);
    if (!res.ok) return [];
    return (await res.json()).pads || [];
  } catch {
    return []; // offline is a normal state for a cron on a laptop.
  }
}

// One verdict per line, appended by sink.mjs. A line that won't parse is a line
// we drop — a corrupt tail shouldn't cost us the whole history.
export function verdicts() {
  if (!existsSync(VERDICTS)) return [];
  return readFileSync(VERDICTS, "utf8")
    .split("\n")
    .filter(Boolean)
    .map((l) => {
      try {
        return JSON.parse(l);
      } catch {
        return null;
      }
    })
    .filter(Boolean);
}

// The LAST verdict for a pad is the one that counts — you're allowed to change
// your mind, and re-judging a pad should overwrite, not double-count.
export function latest(all = verdicts()) {
  const by = new Map();
  for (const v of all) by.set(v.code, v);
  return [...by.values()];
}

// A pad that got an ok AND got played is worth more than one that got a shrug
// and an ok. Taps are the signal nobody asked you for.
const enthusiasm = (v) => (v.verdict === "ok" ? 1 : 0) + Math.min(2, v.taps / 3);

const describe = (v, notes) => {
  const trait = notes?.[v.code] || "";
  const played = v.taps ? `, played ${v.taps}×` : ", never touched";
  const dwell = `${Math.round(v.dwellMs / 100) / 10}s`;
  return `- ${v.code}${trait ? ` (${trait})` : ""} — ${dwell}${played}`;
};

// The honest warning, said every single time. A loop that only ever hears "more
// like the kept ones" collapses onto one mode — and the collapse is invisible
// from inside, because every generation still scores well against the last one.
// This costs nothing to keep saying, so we keep saying it.
const DONT_COLLAPSE = [
  "DON'T COLLAPSE: the point is not to converge on the favorite pad. Rhyme with",
  "what got kept, then go somewhere nobody has been. A generation that only",
  "interpolates between past keeps is a dead loop.",
].join("\n");

// A pad the room agreed on. A lone ok is an opinion; a pad that several people
// kept and nobody cancelled is the closest thing to a fact this system produces.
const consensus = (p) => p.ok - p.cancel;

// What the crowd said, as prose. Ratios, not raw counts — "4 of 5 kept it" is a
// sentence a model can reason about; "ok: 4" is a number it will pattern-match.
function crowdBlock(pads) {
  const seen = pads.filter((p) => p.ok + p.cancel > 0);
  if (!seen.length) return null;

  const loved = seen.filter((p) => consensus(p) > 0).sort((a, b) => consensus(b) - consensus(a));
  const hated = seen.filter((p) => consensus(p) < 0).sort((a, b) => consensus(a) - consensus(b));
  const played = [...seen].sort((a, b) => b.taps - a.taps).filter((p) => p.taps > 0);

  const line = (p) =>
    `- ${p.code} — ${p.ok} ok / ${p.cancel} cancel` +
    `${p.judges ? ` (${p.judges} judge${p.judges > 1 ? "s" : ""})` : ""}` +
    `${p.taps ? `, played ${p.taps}×` : ", never touched"}`;

  const out = [`THE ROOM HAS SPOKEN — ${seen.length} pads judged by real visitors:`, ""];
  if (loved.length) {
    out.push("KEPT by the room — make more things that rhyme with these:");
    out.push(...loved.slice(0, 10).map(line), "");
  }
  if (hated.length) {
    out.push("CANCELLED by the room — do not make these again:");
    out.push(...hated.slice(0, 10).map(line), "");
  }
  if (played.length) {
    out.push(
      `MOST PLAYED: ${played.slice(0, 5).map((p) => `${p.code} (${p.taps}×)`).join(", ")}.`,
      "People reached out and touched these without being asked to. Whatever",
      "they do with the tap, do that.",
      "",
    );
  }
  return out.join("\n");
}

// The block that goes into the generator's prompt. If there's no history yet we
// say so plainly rather than inventing a preference — a cold start is a real
// state, and a model told "here is what he likes" when nothing is known will
// hallucinate a taste and then chase it.
export async function pheromone(notes = {}) {
  const pads = await crowd();
  const room = crowdBlock(pads);
  if (room) return room + "\n" + DONT_COLLAPSE;

  const all = latest();
  if (!all.length) {
    return [
      "TASTE SO FAR: nothing judged yet. This is the cold start.",
      "Make something you'd defend. Don't hedge toward the middle — an",
      "unjudged loop has no median to regress to, and this is the only",
      "generation that gets to be reckless.",
    ].join("\n");
  }

  const kept = all.filter((v) => v.verdict === "ok").sort((a, b) => enthusiasm(b) - enthusiasm(a));
  const cut = all.filter((v) => v.verdict === "cancel");
  const failed = all.filter((v) => v.failed);
  const loved = kept.filter((v) => v.taps >= 3);
  const shrugged = kept.filter((v) => !v.taps);

  const out = [`TASTE SO FAR: ${kept.length} ok, ${cut.length} cancelled, of ${all.length} judged.`, ""];

  if (kept.length) {
    out.push("KEPT — make more things that rhyme with these:");
    out.push(...kept.slice(0, 12).map((v) => describe(v, notes)), "");
  }
  if (loved.length) {
    out.push(
      `PLAYED HARDEST (${loved.map((v) => v.code).join(", ")}) — he didn't just`,
      "approve these, he reached out and touched them. Whatever they're doing",
      "with the tap, do that.",
      "",
    );
  }
  if (cut.length) {
    out.push("CANCELLED — do not make these again:");
    out.push(...cut.slice(0, 12).map((v) => describe(v, notes)), "");
  }
  if (shrugged.length) {
    out.push(
      `APPROVED BUT NEVER TOUCHED (${shrugged.map((v) => v.code).join(", ")}) —`,
      "these passed, but they didn't invite a hand. Being inoffensive is not",
      "the goal. Aim above this.",
      "",
    );
  }
  if (failed.length) {
    out.push(
      `${failed.length} candidate(s) never even ran and were cancelled on sight.`,
      "The engine contract is not optional — read it again.",
      "",
    );
  }

  out.push(DONT_COLLAPSE);
  return out.join("\n");
}

if (import.meta.url === `file://${process.argv[1]}`) console.log(await pheromone());
