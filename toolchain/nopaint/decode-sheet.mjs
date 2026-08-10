// Print one No Paint event sheet with its expressions resolved.
//
//   node toolchain/nopaint/decode-sheet.mjs Caterpillar
//
// Event parameters are [type, [expressionNumber, ...nodes]]; this walks the
// sheet and prints the compiled source behind each expressionNumber so a
// recovered constant can be read rather than guessed. See expressions.mjs.

import { readFile } from "node:fs/promises";
import { expressionTable } from "./expressions.mjs";

const project = JSON.parse(await readFile(
  new URL("../../system/public/nopaint.art/data.json", import.meta.url), "utf8")).project;
const table = expressionTable();

const name = process.argv[2];
const sheet = project[6].find((entry) => entry[0] === name);
if (!sheet) throw new Error(`no sheet ${name}; have ${project[6].map((s) => s[0]).join(", ")}`);

const variables = new Map();
function collectVariables(node) {
  if (!Array.isArray(node)) return;
  if (node[0] === 1 && typeof node[1] === "string") variables.set(node[6], node[1]);
  else node.forEach(collectVariables);
}
collectVariables(sheet[1]);

const expression = (value) => {
  if (!Array.isArray(value)) return JSON.stringify(value);
  const [number, ...nodes] = value;
  const body = table[number] ?? "?";
  const named = nodes.map((node) => Array.isArray(node) && node[0] === 3
    ? variables.get(node[1]) || `var#${node[1]}`
    : JSON.stringify(node)).join(", ");
  return named ? `${body}   ← ${named}` : body;
};

// [type, id, behavior, sid, flags, …, params]; params sit at index 6 for
// actions and index 9 for conditions. Type 2 is an audio file reference —
// [name, isLooping] — not an expression, so print it as written.
const parameters = (list) => (list || [])
  .map(([type, value]) => `      p${type}: ${type === 2
    ? `audio ${JSON.stringify(value?.[0])}${value?.[1] ? " (looping)" : ""}`
    : expression(value)}`).join("\n");

function walk(node, depth) {
  if (!Array.isArray(node)) return;
  const pad = "  ".repeat(depth);
  if (node[0] === 1 && typeof node[1] === "string") {
    console.log(`${pad}var ${node[1]} = ${JSON.stringify(node[3])}`);
    return;
  }
  if (node[0] === 2 && typeof node[1] === "string") {
    console.log(`${pad}// ${node[1].replace(/\s+/g, " ").slice(0, 120)}`);
    return;
  }
  // An event block is [0, …, conditions, actions, children]; a function block
  // is [4, [name, …], …] with the same tail. Naming the functions matters —
  // a brush's real work usually lives in them, and the sheet only calls them.
  if ((node[0] === 0 || node[0] === 4) && Array.isArray(node[6]) && Array.isArray(node[7])) {
    console.log(`${pad}${node[0] === 4 ? `function ${node[1][0]}` : "event"}`);
    for (const condition of node[6]) {
      console.log(`${pad}  when object=${condition[0]} ace=${condition[1]}${condition[2] ? ` (${condition[2]})` : ""}`);
      const printed = parameters(condition[9]);
      if (printed) console.log(printed);
    }
    for (const action of node[7]) {
      console.log(`${pad}  do object=${typeof action[1] === "string" ? action[1] : `${action[0]}/${action[1]}`}${action[2] ? ` (${action[2]})` : ""}`);
      const printed = parameters(action[6]);
      if (printed) console.log(printed);
    }
    walk(node[8], depth + 1);
    return;
  }
  for (const child of node) walk(child, depth + 1);
}

console.log(`# ${sheet[0]}`);
walk(sheet[1], 0);
