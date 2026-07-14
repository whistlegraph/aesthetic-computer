// evolve.mjs — breed a new pad from one you kept.
//
// generate.mjs rerolls from the contract every time: each pad is a fresh idea and
// the loop never gets anywhere. Evolution is the other move — take something that
// already earned attention and change ONE thing about it, so the good part is
// carried and the new part is tested.
//
// A pad's genes are written in the way we already describe them: every one is a
// VISUAL GENERATOR × a SYNTH VOICE (reaction-diffusion × bell, L-system fern ×
// flute). That's the crossover point, and it means a mutation can be named out
// loud instead of being a temperature knob:
//
//   voice  — keep the eye, replace the ear
//   eye    — keep the ear, replace the eye
//   cross  — the winner's eye, another winner's ear
//   push   — find what the TAP does and make the pad be about that
//   invert — the same idea, run backwards
//
// The parent's entire source goes in the prompt. Not a summary of it — a summary
// is where the good part gets lost.
//
//   node cancelok/evolve.mjs           # breed one → disks/<name>.mjs
//   node cancelok/evolve.mjs --dry     # print the prompt, write nothing

import { execFileSync } from "node:child_process";
import { readFileSync, writeFileSync, existsSync, readdirSync } from "node:fs";
import { resolve } from "node:path";
import { REPO, crowd, pheromone, lineage, remember } from "./taste.mjs";

const DISKS = resolve(REPO, "system/public/aesthetic.computer/disks");
const ENGINE = resolve(REPO, "system/public/aesthetic.computer/lib/pads.mjs");
const dry = process.argv.includes("--dry");

// Fitness. Being kept is the floor; what we're really ranking on is whether you
// COULDN'T LEAVE IT ALONE. Taps and revisits are worth more than an ok, because
// an ok can be politeness and a revisit can't.
const fitness = (p) =>
  (p.ok - p.cancel) * 2 +
  Math.min(10, p.taps) +
  (p.revisits || 0) * 3 +
  Math.min(4, (p.dwellMs || 0) / 8000);

const MUTATIONS = [
  {
    key: "voice",
    ask: `KEEP THE EYE, REPLACE THE EAR. The visual generator stays — same geometry,
same allegory, same paint. Give it a completely different synth voice, and rewrite
the score to suit that voice. The pad should look like its parent and sound like a
stranger. Make the visuals answer to the NEW sound (the visual must still BE the
score — if the eye no longer tracks the ear, you've failed).`,
  },
  {
    key: "eye",
    ask: `KEEP THE EAR, REPLACE THE EYE. The score, the voice, the rhythm all stay —
someone should be able to hum along to both. Throw the visual generator away and
build a new one that is a DIFFERENT allegory for the same music. Same song, new
instrument to look at.`,
  },
  {
    key: "push",
    ask: `FIND WHAT THE TAP DOES, AND MAKE THE PAD BE ABOUT THAT. The parent earned
its keep because someone reached out and touched it. Whatever onTap does, that is
the most interesting thing in the file — so promote it. Make the tap gesture the
CENTER of the piece instead of a garnish: richer, more expressive under a drag,
with more of the visual and the voice responding to where the finger is.`,
  },
  {
    key: "invert",
    ask: `THE SAME IDEA, RUN BACKWARDS. Take the parent's core allegory and invert
it — if it grows, make it decay; if it accumulates, make it erode; if it's a rising
arpeggio, make it a falling one; if it's dense, make it sparse and let silence do
the work. Not a different idea: the SAME idea, seen from the other side.`,
  },
];

const CROSS = {
  key: "cross",
  ask: (a, b) => `CROSSBREED. Take the VISUAL GENERATOR from ${a} and the SYNTH VOICE
from ${b}. Not a collage of both files — a single coherent pad that inherits its eye
from the first parent and its ear from the second, and makes them belong together.
The visual must BE the score for the NEW voice.`,
};

const source = (code) => readFileSync(resolve(DISKS, `${code}.mjs`), "utf8");
const contract = readFileSync(ENGINE, "utf8").split("let cfg =")[0];
const taken = () =>
  readdirSync(DISKS).filter((f) => f.endsWith(".mjs")).map((f) => f.slice(0, -4));

const pads = (await crowd()).filter((p) => p.ok > 0 && fitness(p) > 0);
if (!pads.length) {
  console.error("❌ nothing has been kept yet — there's nothing to breed from.");
  console.error("   run cancelok/generate.mjs until something earns an ok.");
  process.exit(2);
}
pads.sort((a, b) => fitness(b) - fitness(a));

// Breed from the top few, not the top one. The best pad is a local maximum and
// the loop's job is to leave it.
const elite = pads.slice(0, Math.min(4, pads.length));
const mum = elite[Math.floor(Math.random() * elite.length)];
const canCross = elite.length > 1;

let mutation;
if (canCross && Math.random() < 0.25) {
  const others = elite.filter((p) => p.code !== mum.code);
  const dad = others[Math.floor(Math.random() * others.length)];
  mutation = { key: "cross", ask: CROSS.ask(mum.code, dad.code), dad: dad.code };
} else {
  mutation = MUTATIONS[Math.floor(Math.random() * MUTATIONS.length)];
}

const parentSrc = source(mum.code);
const dadSrc = mutation.dad ? source(mutation.dad) : null;

const why =
  `${mum.code} earned this: ${mum.ok} ok / ${mum.cancel} cancel, ` +
  `played ${mum.taps}×, ${mum.revisits ? `came back to ${mum.revisits}×, ` : ""}` +
  `held on screen ${Math.round((mum.dwellMs || 0) / 1000)}s on average.`;

const prompt = `You are EVOLVING an existing pad for Aesthetic Computer.

A pad is a self-running audiovisual instrument piece. The whole screen is one
button: tap or XY-drag to play. Every pad is a marriage of a VISUAL GENERATOR and
a SYNTH VOICE, and the visual should BE the score — you should be able to SEE the
sound you're hearing.

Here is the engine contract every pad must obey:

--- lib/pads.mjs (contract) ---
${contract}
--- end contract ---

THE PARENT — this pad was kept by real people. ${why}

--- disks/${mum.code}.mjs ---
${parentSrc}
--- end parent ---
${
  dadSrc
    ? `\nTHE SECOND PARENT — its voice is what you're borrowing.\n\n--- disks/${mutation.dad}.mjs ---\n${dadSrc}\n--- end second parent ---\n`
    : ""
}
${await pheromone()}

YOUR MUTATION — ${mutation.key.toUpperCase()}:

${mutation.ask}

This is a CHILD, not a cousin. The parent earned its keep, so inherit the part
that earned it and change only what the mutation asks you to change. A rewrite
that shares nothing with its parent is not evolution, it's just another reroll —
and we already have a tool for that.

Hard requirements:
- A single .mjs file, a thin wrapper: import from "../lib/pads.mjs", define CONFIG
  with bpm/steps/hooks, call initPad(CONFIG) inside boot (NOT at import — the
  engine is a session singleton), export { boot, sim, paint, act }.
- READ ctx.quality in onPaint and scale your cost by it. It must hold 60fps or the
  gate kills it before anyone sees it, and a killed child is a wasted generation.
- Pick a name that is an invented syllable-word in the house style. NOT taken:
  ${taken().join(" ")}
- Comment like the parent does: say WHY, name the allegory, no banner walls.

Output EXACTLY this and nothing else:

NAME: <the-name>
TRAIT: <visual generator> x <synth voice>
KIN: ${mum.code}${mutation.dad ? ` + ${mutation.dad}` : ""} (${mutation.key})
\`\`\`javascript
<the complete file>
\`\`\``;

if (dry) {
  console.log(prompt);
  process.exit(0);
}

console.log(`🧬 breeding ${mum.code} — mutation: ${mutation.key}${mutation.dad ? ` × ${mutation.dad}` : ""}`);
// A language model, not an agent: `claude --print` can WRITE FILES, and a
// generator that writes its own pad collides with itself on the name check.
const raw = execFileSync("claude", ["--print", "--model", "sonnet", "--tools", ""], {
  input: prompt,
  encoding: "utf8",
  maxBuffer: 1024 * 1024 * 8,
  cwd: REPO,
});

const name = /NAME:\s*([a-z][a-z0-9-]*)/i.exec(raw)?.[1]?.toLowerCase();
// AC's font is ASCII — a stray × lands on screen as a `?`.
const trait = (/TRAIT:\s*(.+)/.exec(raw)?.[1]?.trim() || "")
  .replace(/[×✕]/g, "x")
  .replace(/[^\x20-\x7e]/g, "");
const code = /```(?:javascript|js)?\n([\s\S]*?)```/.exec(raw)?.[1];

if (!name || !code) {
  console.error("❌ couldn't parse a pad out of that.\n" + raw.slice(0, 400));
  process.exit(1);
}
if (existsSync(resolve(DISKS, `${name}.mjs`))) {
  console.error(`❌ ${name} already exists — burned a generation on a collision.`);
  process.exit(1);
}
if (!/initPad\(/.test(code) || !/export\s*\{[^}]*boot/.test(code)) {
  console.error(`❌ ${name} doesn't honor the contract.`);
  process.exit(1);
}

// A child that is its parent is not a child. This is the failure mode of breeding
// and it's a quiet one — a near-copy passes the gate, earns the same ok, and
// breeds again, and the line converges on one pad wearing different names. So:
// count how much of the parent survived verbatim, and refuse a clone.
const meat = (s) =>
  s
    .split("\n")
    .map((l) => l.trim())
    .filter((l) => l.length > 12 && !l.startsWith("//"));
const kid = new Set(meat(code));
const kept = meat(parentSrc).filter((l) => kid.has(l)).length;
const echo = kept / Math.max(1, meat(parentSrc).length);
if (echo > 0.85) {
  console.error(
    `❌ ${name} is ${Math.round(echo * 100)}% ${mum.code} — that's a copy, not a child.`,
  );
  process.exit(1);
}
console.log(`   inherited ${Math.round(echo * 100)}% of ${mum.code} verbatim`);

writeFileSync(resolve(DISKS, `${name}.mjs`), code);
remember(name, {
  parent: mum.code,
  second: mutation.dad || null,
  mutation: mutation.key,
  trait,
  born: new Date().toISOString(),
});
console.log(`🐣 ${name} — ${trait}  (${mum.code}${mutation.dad ? ` + ${mutation.dad}` : ""}, ${mutation.key})`);
console.log(JSON.stringify({ code: name, name, trait, parent: mum.code, mutation: mutation.key }));
