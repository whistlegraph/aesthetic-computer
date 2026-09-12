#!/usr/bin/env node
// sync-context — copy the Aesthetic Computer authoring guides into easel/context/.
//
// Easel tells the model how to write an AC piece by naming the repo's guides and
// asking it to read them. That works inside the monorepo and nowhere else: the
// lookup is existsSync against the working directory, so an installed Easel
// opened on someone's Desktop passes along no AC knowledge at all. It becomes a
// general-purpose editor that happens to publish to a URL.
//
// So the guides travel with the tool. They are 24 KB of markdown against 224 KB
// of source — a tenth of what is already being shipped — and they are the entire
// reason to install this rather than use the vendor CLI directly.
//
// Copies, not symlinks: a symlink does not survive an npm tarball or a curl
// install. Copies drift, so `npm test` fails when they do, and this script is
// how you fix it.
//
//   node bin/sync-context.mjs          # write the bundle
//   node bin/sync-context.mjs --check  # exit 1 if it is stale
import { readFileSync, writeFileSync, mkdirSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const EASEL = join(HERE, "..");
const REPO = join(EASEL, "..");
const OUT = join(EASEL, "context");

// What a model needs to write a piece, and nothing else. Deliberately not the
// whole repository: this is the knowledge that is about Aesthetic Computer
// rather than about this checkout.
export const BUNDLE = [
  ["SCREEN.md", "screen.md", "how a piece draws on the AC canvas"],
  ["HAND.md", "hand.md", "how the code reads"],
  ["system/public/aesthetic.computer/disks/CLAUDE.md", "pieces.md", "the piece authoring guide"],
  ["kidlisp/README.md", "kidlisp.md", "the KidLisp language"],
];

const header = (from, subject) =>
  `<!-- ${subject}\n     Bundled with Easel from ${from} in the Aesthetic Computer repository.\n     Do not edit here — edit the source and run \`npm run context\`. -->\n\n`;

export function build() {
  mkdirSync(OUT, { recursive: true });
  return BUNDLE.map(([from, to, subject]) => {
    const body = header(from, subject) + readFileSync(join(REPO, from), "utf8");
    return { path: join(OUT, to), body, from, to, subject };
  });
}

const check = process.argv.includes("--check");
let stale = 0;
for (const file of build()) {
  let current = "";
  try { current = readFileSync(file.path, "utf8"); } catch {}
  if (current === file.body) continue;
  stale += 1;
  if (check) {
    console.error(`stale: context/${file.to} no longer matches ${file.from}`);
  } else {
    writeFileSync(file.path, file.body);
    console.log(`wrote context/${file.to}  (${(file.body.length / 1024).toFixed(1)} KB)`);
  }
}

if (check && stale) {
  console.error(`\n${stale} file(s) stale — run \`npm run context\`.`);
  process.exit(1);
}
if (check) console.log("context bundle is current.");
else if (!stale) console.log("context bundle already current.");
