// Index nopaint.art's compiled C3 expression table.
//
//   node toolchain/nopaint/expressions.mjs 434 5 215
//   node toolchain/nopaint/expressions.mjs --grep "trotting"
//
// Every numeric constant in the surviving No Paint export lives here: an event
// parameter is [type, [expressionNumber, ...nodes]], and C3_ExpressionFuncs
// [expressionNumber] is the compiled function that produces the value. Reading
// this table is how a recovered brush constant stops being a guess.

import { readFile } from "node:fs/promises";

const runtime = new URL("../../system/public/nopaint.art/scripts/c3runtime.js", import.meta.url);
const source = await readFile(runtime, "utf8");

const start = source.indexOf("C3_ExpressionFuncs=[");
if (start < 0) throw new Error("no C3_ExpressionFuncs in c3runtime.js");

// Split the array literal on its top-level commas, tracking nesting and the
// string/template quoting that would otherwise hide a bracket.
export function expressionTable() {
  const entries = [];
  let depth = 0;
  let quote = null;
  let from = start + "C3_ExpressionFuncs=[".length;
  for (let i = from; i < source.length; i += 1) {
    const character = source[i];
    if (quote) {
      if (character === "\\") i += 1;
      else if (character === quote) quote = null;
      continue;
    }
    if (character === '"' || character === "'" || character === "`") { quote = character; continue; }
    if ("([{".includes(character)) { depth += 1; continue; }
    if (")}".includes(character)) { depth -= 1; continue; }
    if (character === "]") {
      if (depth === 0) { entries.push(source.slice(from, i)); break; }
      depth -= 1;
      continue;
    }
    if (character === "," && depth === 0) {
      entries.push(source.slice(from, i));
      from = i + 1;
    }
  }
  return entries.map((entry) => entry.replace(/\s+/g, " ").trim());
}

const table = expressionTable();
const args = process.argv.slice(2);

if (args[0] === "--grep") {
  const pattern = new RegExp(args[1], "i");
  table.forEach((entry, index) => {
    if (pattern.test(entry)) console.log(`${index}\t${entry}`);
  });
} else if (args.length) {
  for (const index of args) console.log(`${index}\t${table[Number(index)]}`);
} else {
  console.log(`${table.length} expressions`);
}
