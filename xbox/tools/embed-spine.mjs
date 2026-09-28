#!/usr/bin/env node
// embed-spine.mjs — carry the spine body (xbox/live/spine.mjs + actions.mjs)
// into oskiewar.js.
//
// The game is one global script every platform loads as-is (JavaScriptCore
// on the Mac, QuickJS on the Xbox, the browser), so it can't import the
// modules the Spine Lab uses. This writes them into a sealed block between
// markers instead — each module in its own scope, so none of their names can
// collide with the game's — exposing one global, `spineBody`. The lab and the
// game then run the same code.
//
//   node xbox/tools/embed-spine.mjs          rewrite the block
//   node xbox/tools/embed-spine.mjs --check  exit 1 if the block is stale

import { readFileSync, writeFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

const live = new URL("../live/", import.meta.url);
const read = (name) => readFileSync(new URL(name, live), "utf8");
export const gamePath = fileURLToPath(new URL("oskiewar.js", live));
export const start = "// <spine-body>";
export const end = "// </spine-body>";

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
  const spine = unexport(read("spine.mjs"));
  const actionsSource = read("actions.mjs");
  const importLine = actionsSource.match(/^import \{([^}]+)\} from "\.\/spine\.mjs";\n/m);
  if (!importLine) throw new Error("actions.mjs: expected one import from ./spine.mjs");
  const actions = unexport(actionsSource.replace(importLine[0], ""));
  const indent = (text) => text.trimEnd().split("\n").map((line) => (line ? "    " + line : line)).join("\n");
  return [
    start,
    "// Generated from xbox/live/spine.mjs + actions.mjs by xbox/tools/embed-spine.mjs.",
    "// Don't edit here: edit those, try them in xbox/live/spine-lab.html, then rerun the tool.",
    "const spineBody = (() => {",
    "  const spine = (() => {",
    indent(spine.body),
    `    return { ${spine.names.join(", ")} };`,
    "  })();",
    "  const actions = (() => {",
    `    const {${importLine[1]}} = spine;`,
    indent(actions.body),
    `    return { ${actions.names.join(", ")} };`,
    "  })();",
    "  return { ...spine, ...actions };",
    "})();",
    end,
  ].join("\n");
}

// Where the block goes the first time: just before the rig it replaces.
const anchor = "function runnerWorldGeometry(player, t) {";

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
    if (at < 0) throw new Error("oskiewar.js: can't find where the spine body goes");
    next = source.slice(0, at) + block + "\n\n" + source.slice(at);
  }
  if (next !== source) writeFileSync(gamePath, next);
  return next !== source;
}

if (process.argv[1] && fileURLToPath(import.meta.url) === process.argv[1]) {
  if (process.argv.includes("--check")) {
    const fresh = embedded() === generate();
    console.log(fresh ? "spine body in oskiewar.js is current" : "spine body in oskiewar.js is stale: run node xbox/tools/embed-spine.mjs");
    process.exit(fresh ? 0 : 1);
  }
  console.log(write() ? "embedded the spine body into oskiewar.js" : "spine body already current");
}
