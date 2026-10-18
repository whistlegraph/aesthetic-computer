#!/usr/bin/env node
// sync-harness — compile aesel/harness/*.md into src/harness-text.mjs.
//
// The words aesel says to a model live in harness/ as markdown, one component
// per file: the system prompt, the guardrails, the tool guidance. That is the
// surface a person — or a harness optimiser — edits. Each `## key` section is
// one paragraph the code places by name; `{{name}}` marks a value the session
// fills in.
//
// They are compiled rather than read at run time because the same text reaches
// the browser clients, the native app and the Bun bytecode bundle, none of which
// can open a file beside the source. The compiled module drifts from the
// markdown, so `npm test` fails when it does, and this script is how you fix it.
//
//   node bin/sync-harness.mjs          # write src/harness-text.mjs
//   node bin/sync-harness.mjs --check  # exit 1 if it is stale
import { readFileSync, readdirSync, writeFileSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const aesel = join(dirname(fileURLToPath(import.meta.url)), "..");
const SOURCE = join(aesel, "harness");
const OUT = join(aesel, "src/harness-text.mjs");

// Text before the first `## key` heading is for the human reading the file.
export function parse(markdown, file) {
  const sections = {};
  let key = null;
  for (const line of markdown.split("\n")) {
    const heading = /^## ([a-z0-9-]+)\s*$/.exec(line);
    if (heading) {
      key = heading[1];
      if (key in sections) throw Error(`${file}: section ${key} appears twice`);
      sections[key] = [];
    } else if (line.startsWith("## ")) {
      throw Error(`${file}: section names are lowercase-kebab, got "${line}"`);
    } else if (key) sections[key].push(line);
  }
  return Object.fromEntries(
    Object.entries(sections).map(([name, lines]) => {
      const text = lines.join("\n").trim();
      if (!text) throw Error(`${file}: section ${name} is empty`);
      return [name, text];
    }),
  );
}

export function build() {
  const components = {};
  const owner = {};
  for (const file of readdirSync(SOURCE).filter((name) => name.endsWith(".md")).sort()) {
    const sections = parse(readFileSync(join(SOURCE, file), "utf8"), file);
    for (const key of Object.keys(sections)) {
      if (owner[key]) throw Error(`section ${key} is in both ${owner[key]} and ${file}`);
      owner[key] = file;
    }
    components[file.replace(/\.md$/, "")] = sections;
  }
  return `// Compiled from aesel/harness/*.md by bin/sync-harness.mjs. Do not edit here —
// edit the markdown and run \`npm run harness\`.

export const HARNESS = ${JSON.stringify(components, null, 1)};

const TEXT = Object.assign({}, ...Object.values(HARNESS));

// One section by name, with its {{placeholders}} filled. A missing section or
// an unfilled placeholder throws: a prompt with a hole in it is worse than none.
export function harnessText(key, values = {}) {
  if (!(key in TEXT)) throw Error(\`No harness section named \${key}\`);
  return TEXT[key].replace(/\\{\\{(\\w+)\\}\\}/g, (_, name) => {
    if (!(name in values)) throw Error(\`Harness section \${key} needs {{\${name}}}\`);
    return String(values[name]);
  });
}
`;
}

if (process.argv[1] && resolve(process.argv[1]) === fileURLToPath(import.meta.url)) {
  const check = process.argv.includes("--check");
  const body = build();
  let current = "";
  try { current = readFileSync(OUT, "utf8"); } catch {}
  if (current === body) console.log("harness text is current.");
  else if (check) {
    console.error("stale: src/harness-text.mjs no longer matches harness/ — run `npm run harness`.");
    process.exit(1);
  } else {
    writeFileSync(OUT, body);
    console.log(`wrote src/harness-text.mjs  (${(body.length / 1024).toFixed(1)} KB)`);
  }
}
