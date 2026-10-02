#!/usr/bin/env node
// ow.mjs — pack, unpack and check .ow packages (xbox/OW-FORMAT.md).
//
//   node xbox/tools/ow.mjs check <file.ow>                  read, validate the level, compile every object
//   node xbox/tools/ow.mjs pack [--level map.json|level.ow] [--object name=file.lisp …] > out.ow
//   node xbox/tools/ow.mjs unpack <file.ow> <dir>           level.json (arena) or level.ow, and one .lisp an object
//   node xbox/tools/ow.mjs show <file.ow>                    what's inside, as JSON
import { readFileSync, writeFileSync, mkdirSync } from "node:fs";
import { join, basename } from "node:path";
import { read, compile } from "../live/object-lisp.mjs";
import { validateMap } from "../live/oskiewar-map.mjs";
import { readOw, writeOw, levelToMap, levelFromMap, islandParams, writeLevel } from "../live/ow.mjs";

const [command, ...args] = process.argv.slice(2);
const text = (file) => readFileSync(file, "utf8");

function check(file) {
  const ow = readOw(text(file), { read });
  const report = { file, level: null, objects: {} };
  if (ow.level) {
    report.level = { name: ow.level.name, kind: ow.level.kind, title: ow.level.title };
    if (ow.level.kind === "arena") report.level.map = validateMap(levelToMap(ow.level)).name;
    if (ow.level.kind === "island") report.level.island = islandParams(ow.level);
  }
  for (const [name, source] of Object.entries(ow.objects)) {
    const compiled = compile(source, name);
    report.objects[name] = { lines: source.split("\n").length - 1, sketches: compiled.sketches?.length ?? 0 };
  }
  return report;
}

if (command === "check" || command === "show") {
  for (const file of args) console.log(JSON.stringify(check(file), null, 2));
} else if (command === "pack") {
  let level = null;
  const objects = {};
  for (let i = 0; i < args.length; i++) {
    if (args[i] === "--level") {
      const file = args[++i];
      level = file.endsWith(".json") ? levelFromMap(validateMap(JSON.parse(text(file))))
        : readOw(text(file), { read }).level;
    } else if (args[i] === "--object") {
      const spec = args[++i], eq = spec.indexOf("=");
      const name = eq < 0 ? basename(spec).replace(/\.lisp$/, "").replace(/-flat$/, "") : spec.slice(0, eq);
      objects[name] = text(eq < 0 ? spec : spec.slice(eq + 1));
    } else throw new Error(`pack: unknown argument ${args[i]}`);
  }
  process.stdout.write(writeOw({ level, objects }));
} else if (command === "unpack") {
  const [file, dir] = args;
  const ow = readOw(text(file), { read });
  mkdirSync(dir, { recursive: true });
  if (ow.level) {
    if (ow.level.kind === "arena") writeFileSync(join(dir, "level.json"), JSON.stringify(levelToMap(ow.level), null, 2) + "\n");
    else writeFileSync(join(dir, "level.ow"), `;; ow 1\n;; level ${ow.level.name}\n` + writeLevel(ow.level));
  }
  for (const [name, source] of Object.entries(ow.objects)) writeFileSync(join(dir, `${name}.lisp`), source);
  console.log(JSON.stringify({ level: ow.level?.name ?? null, objects: Object.keys(ow.objects) }));
} else {
  console.error("usage: ow.mjs check|show <file.ow> | pack [--level f] [--object n=f …] | unpack <file.ow> <dir>");
  process.exit(1);
}
