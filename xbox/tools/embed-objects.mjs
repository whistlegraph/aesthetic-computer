#!/usr/bin/env node
// embed-objects.mjs — carry the object compiler (xbox/live/object-lisp.mjs)
// and the objects the game draws (xbox/live/objects/*.lisp) into oskiewar.js.
//
// The game is one global script every platform loads as-is, so it can't
// import the compiler the object lab uses. This writes it into a sealed block
// between markers, in its own scope, with each object's source as a string,
// and exposes one global, `gameObjects`: each object compiled — and so baked
// — once, when the script loads. The lab and the game run the same compiler
// on the same source.
//
//   node xbox/tools/embed-objects.mjs          rewrite the block
//   node xbox/tools/embed-objects.mjs --check  exit 1 if the block is stale

import { readFileSync, writeFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

const live = new URL("../live/", import.meta.url);
const read = (name) => readFileSync(new URL(name, live), "utf8");
export const gamePath = fileURLToPath(new URL("oskiewar.js", live));
export const start = "// <objects>";
export const end = "// </objects>";

// What the game draws, by the name it asks for: the source file each is
// compiled from. The monowheel is drawn flat, and figures are, behind a flag
// (OBJECT-DIALECT.md).
export const objects = { monowheel: "monowheel-flat", figure: "figure-flat" };

// A module's source with its `export` keywords taken off, and the names it
// exported.
function unexport(source) {
  const names = [];
  const body = source.replace(/^export (async function\*?|function\*?|const|let|class) (\w+)/gm, (_, kind, name) => {
    names.push(name);
    return `${kind} ${name}`;
  });
  return { body, names };
}

export function generate() {
  const lisp = unexport(read("object-lisp.mjs"));
  if (/^import /m.test(lisp.body)) throw new Error("object-lisp.mjs: the game can't carry imports");
  const indent = (text) => text.trimEnd().split("\n").map((line) => (line ? "    " + line : line)).join("\n");
  const sources = Object.entries(objects).map(([name, file]) =>
    `    ${name}: ${JSON.stringify(read(`objects/${file}.lisp`))},`);
  return [
    start,
    "// Generated from xbox/live/object-lisp.mjs and xbox/live/objects/*.lisp by",
    "// xbox/tools/embed-objects.mjs. Don't edit here: edit those, try them in",
    "// xbox/live/object-lab.html, then rerun the tool.",
    "const gameObjects = (() => {",
    "  const objectLisp = (() => {",
    indent(lisp.body),
    `    return { ${lisp.names.join(", ")} };`,
    "  })();",
    "  const sources = {",
    ...sources,
    "  };",
    "  const compiled = {};",
    "  for (const name in sources) compiled[name] = objectLisp.compile(sources[name], name);",
    "  return { ...compiled, light: objectLisp.objectLight, drawFigureShapes: objectLisp.drawFigureShapes };",
    "})();",
    end,
  ].join("\n");
}

// Where the block goes the first time: just before the monowheel it draws.
const anchor = "function drawMonowheel(";

export function embedded(source = readFileSync(gamePath, "utf8")) {
  const a = source.indexOf(start), b = source.indexOf(end);
  return a < 0 || b < 0 ? null : source.slice(a, b + end.length);
}

function write() {
  const source = readFileSync(gamePath, "utf8"), block = generate();
  const current = embedded(source);
  let next;
  if (current) next = source.replace(current, () => block);
  else {
    const at = source.indexOf(anchor);
    if (at < 0) throw new Error("oskiewar.js: can't find where the objects go");
    next = source.slice(0, at) + block + "\n" + source.slice(at);
  }
  if (next !== source) writeFileSync(gamePath, next);
  return next !== source;
}

if (process.argv[1] && fileURLToPath(import.meta.url) === process.argv[1]) {
  if (process.argv.includes("--check")) {
    const fresh = embedded() === generate();
    console.log(fresh ? "objects in oskiewar.js are current" : "objects in oskiewar.js are stale: run node xbox/tools/embed-objects.mjs");
    process.exit(fresh ? 0 : 1);
  }
  console.log(write() ? "embedded the objects into oskiewar.js" : "objects already current");
}
