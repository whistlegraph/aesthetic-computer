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

import { readFileSync, existsSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
export const REPO = resolve(HERE, "..");
export const VERDICTS = resolve(HERE, "verdicts.jsonl");
export const QUEUE = resolve(HERE, "queue.json");

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

// The block that goes into the generator's prompt. If there's no history yet we
// say so plainly rather than inventing a preference — a cold start is a real
// state, and a model told "here is what he likes" when nothing is known will
// hallucinate a taste and then chase it.
export function pheromone(notes = {}) {
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

  // The honest warning. A loop that only ever hears "more like the kept ones"
  // collapses onto one mode; saying so in the prompt is the cheapest defense
  // we have, and it costs nothing to keep saying it.
  out.push(
    "DON'T COLLAPSE: the point is not to converge on his favorite pad. Rhyme",
    "with what he kept, then go somewhere he hasn't been. A generation that",
    "only interpolates between past keeps is a dead loop.",
  );

  return out.join("\n");
}

if (import.meta.url === `file://${process.argv[1]}`) console.log(pheromone());
