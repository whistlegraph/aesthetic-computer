// generate.mjs — author one new pad.
//
// The prompt is assembled, not written: the engine's own header IS the contract
// (lib/pads.mjs says exactly what a wrapper must do), one real pad is included
// as a worked example, and the taste block says what got kept and what got cut.
// So the thing that teaches the model to write a pad is the same thing that
// teaches a person to write one. There is no second, drifting copy of the spec.
//
//   node cancelok/generate.mjs           # one candidate → disks/<name>.mjs
//   node cancelok/generate.mjs --dry     # print the prompt, write nothing

import { execFileSync } from "node:child_process";
import { readFileSync, writeFileSync, existsSync, readdirSync } from "node:fs";
import { resolve } from "node:path";
import { REPO, pheromone } from "./taste.mjs";

const DISKS = resolve(REPO, "system/public/aesthetic.computer/disks");
const ENGINE = resolve(REPO, "system/public/aesthetic.computer/lib/pads.mjs");
const EXAMPLE = resolve(DISKS, "wispo.mjs");
const dry = process.argv.includes("--dry");

// Pad names are invented syllable-words (wispo, zorb, plimo, glibo, torva…).
// The model picks one; we only insist it isn't already taken.
const taken = (name) => existsSync(resolve(DISKS, `${name}.mjs`));

// The engine header is the contract — everything above `let cfg` is the spec.
const contract = readFileSync(ENGINE, "utf8").split("let cfg =")[0];

const prompt = `You are writing ONE new "pad" for Aesthetic Computer.

A pad is a self-running audiovisual instrument piece. The whole screen is one
button: tap or XY-drag to play it. Left alone it still runs itself. Every pad is
a marriage of a VISUAL GENERATOR and a SYNTH VOICE, and the visual should BE the
score — you should be able to see the sound you're hearing.

Every pad is a thin wrapper over the shared engine. Here is the engine's own
header, which is the complete contract. Obey it exactly:

--- lib/pads.mjs (contract) ---
${contract}
--- end contract ---

Here is one real, working pad as a worked example of the form:

--- disks/wispo.mjs ---
${readFileSync(EXAMPLE, "utf8")}
--- end example ---

${await pheromone()}

WRITE ONE NEW PAD.

Hard requirements:
- A single .mjs file, a thin wrapper: import from "../lib/pads.mjs", define a
  CONFIG with bpm/steps/hooks, call initPad(CONFIG) inside boot (NOT at import —
  the engine is a session singleton), and export { boot, sim, paint, act }.
- READ ctx.quality in onPaint and scale your cost by it (stride per-pixel loops,
  scale particle counts). It must hold ~60fps. A pad that can't will be killed by
  the gate before anyone sees it, and that is a wasted generation.
- Pick a name that is an invented syllable-word in the house style (wispo, zorb,
  plimo, glibo, vunn, quilo, nibbo, torva, flim, brindo, cobo, loxa, gulmo).
  These names are ALREADY TAKEN — pick something that is not on this list:
  ${readdirSync(DISKS).filter((f) => f.endsWith(".mjs")).map((f) => f.slice(0, -4)).join(" ")}
- Comment like the example: say WHY, name the allegory, no banner walls.

Output format — EXACTLY this, nothing else, no commentary before or after:

NAME: <the-name>
TRAIT: <visual generator> × <synth voice>
\`\`\`javascript
<the complete file>
\`\`\``;

if (dry) {
  console.log(prompt);
  process.exit(0);
}

console.log("🧠 generating…");
// `--tools ""` because `claude --print` can WRITE FILES, and a generator that
// writes its own pad will then collide with itself when we check the name. We
// want a language model here, not an agent: it returns text, WE decide what
// lands on disk. (This cost a whole generation to learn.)
const raw = execFileSync("claude", ["--print", "--model", "sonnet", "--tools", ""], {
  input: prompt,
  encoding: "utf8",
  maxBuffer: 1024 * 1024 * 8,
  cwd: REPO,
});

const name = /NAME:\s*([a-z][a-z0-9-]*)/i.exec(raw)?.[1]?.toLowerCase();
// AC's font is ASCII — a stray × lands on screen as a `?`. The trait is shown
// in the judging bar, so it has to be typeable in the same alphabet as a piece.
const trait = (/TRAIT:\s*(.+)/.exec(raw)?.[1]?.trim() || "")
  .replace(/[×✕]/g, "x")
  .replace(/[^\x20-\x7e]/g, "");
const code = /```(?:javascript|js)?\n([\s\S]*?)```/.exec(raw)?.[1];

if (!name || !code) {
  console.error("❌ couldn't parse a pad out of that. Raw head:\n" + raw.slice(0, 400));
  process.exit(1);
}
if (taken(name)) {
  console.error(`❌ ${name} already exists — burned a generation on a collision.`);
  process.exit(1);
}
if (!/initPad\(/.test(code) || !/export\s*\{[^}]*boot/.test(code)) {
  console.error(`❌ ${name} doesn't honor the contract (no initPad, or no boot export).`);
  process.exit(1);
}

writeFileSync(resolve(DISKS, `${name}.mjs`), code);
console.log(`✍️  ${name} — ${trait}`);
console.log(JSON.stringify({ code: name, name, trait }));
