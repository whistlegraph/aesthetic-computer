#!/usr/bin/env node
// pull-strings — snapshot fuser's translations so captutor can speak their words.
//
//   node bin/pull-strings.mjs            # every language fuser ships
//   node bin/pull-strings.mjs es fr      # just these
//
// fuser's locales are TypeScript modules (packages/core/src/i18n/locales/<lng>/),
// not JSON, so they cannot simply be read — we evaluate them with tsx inside the
// fuser repo and dump the resolved object.
//
// Re-run this whenever Hirad lands new translations. If a key our screenplays use
// disappears, `translator()` throws by name rather than quietly narrating English
// over a Spanish UI.

import { execFileSync } from "node:child_process";
import { writeFileSync, mkdirSync } from "node:fs";
import { join } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = fileURLToPath(new URL(".", import.meta.url));
const FUSER = process.env.FUSER_REPO || `${process.env.HOME}/Developer/fuser`;
const OUT = join(HERE, "..", "assets");

const ALL = ["en", "es", "fr", "hi", "pt-BR", "zh-CN"];
const want = process.argv.slice(2).length ? process.argv.slice(2) : ALL;

mkdirSync(OUT, { recursive: true });

for (const lng of want) {
  const json = execFileSync("npx", ["tsx", "-e", `
import { translations } from './packages/core/src/i18n/locales/${lng}/index.ts';
process.stdout.write(JSON.stringify(translations));
`], { cwd: FUSER, encoding: "utf8", maxBuffer: 32 * 1024 * 1024 });

  const path = join(OUT, `strings.${lng}.json`);
  writeFileSync(path, json);
  const n = (json.match(/"/g) || []).length;
  console.log(`→ strings.${lng}.json  ${(json.length / 1024).toFixed(0)} KB  (~${Math.round(n / 4)} strings)`);
}
