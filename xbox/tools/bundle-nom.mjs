#!/usr/bin/env node

import { readFile, writeFile } from "node:fs/promises";
import { fileURLToPath } from "node:url";

const root = new URL("../../", import.meta.url);
const sources = [
  "system/public/aesthetic.computer/lib/percussion.mjs",
  "system/public/aesthetic.computer/lib/synth.mjs",
  "system/public/aesthetic.computer/lib/nom-score.mjs",
  "system/public/aesthetic.computer/lib/nom.mjs",
];

function flattenModule(source, path) {
  let flat = source
    .replace(/^import[^\n]*\n?/gm, "")
    .replace(/^export\s+default\s+[^;]+;\s*$/gm, "")
    .replace(/^export\s*\{[^}]*\};\s*$/gm, "")
    .replace(/^export\s+(?=(?:const|let|class|function)\b)/gm, "");
  if (path.endsWith("/nom.mjs")) {
    for (const name of ["boot", "sim", "paint", "act", "leave"])
      flat = flat.replace(new RegExp(`^function ${name}\\(`, "m"), `function nom${name[0].toUpperCase()}${name.slice(1)}(`);
  }
  return flat.trim();
}

export async function buildNomBundle() {
  const modules = [];
  for (const path of sources) {
    const source = await readFile(new URL(path, root), "utf8");
    modules.push(`// source: ${path}\n${flattenModule(source, path)}`);
  }
  const adapter = await readFile(new URL("xbox/live/nom-host-adapter.js", root), "utf8");
  return `// Dannom for the oskiewar-generation native Xbox host. GENERATED.\n` +
    `// Rebuild: node xbox/tools/bundle-nom.mjs\n(function (xboxNativeSynth) {\n${modules.join("\n\n")}\n\n${adapter}\n})(synth);\n`;
}

if (process.argv[1] === fileURLToPath(import.meta.url)) {
  const output = new URL("xbox/live/dannom.js", root);
  await writeFile(output, await buildNomBundle());
  console.log(fileURLToPath(output));
}
