#!/usr/bin/env node
// printer-cli — `ac printer`: turn a thing you made into a program that makes more.
//
//   ac printer <dir> --from <file> ["what you asked for"…] [--out <mime>] [--in text,image]
//   ac printer <dir> --out <mime> ["what it is for"…]
import "./env.mjs";
import path from "node:path";
import process from "node:process";
import { parseArgs } from "node:util";
import { printer } from "./printer.mjs";
import { OUTPUTS } from "./harness-factory.mjs";

const out = (text = "") => process.stdout.write(`${text}\n`);
const usage = 'usage: ac printer <dir> --from <file> ["what you asked for"…] [--out <mime>] [--in text,image,file]';

try {
  const { values, positionals } = parseArgs({ args: process.argv.slice(2), allowPositionals: true, options: {
    from: { type: "string" }, out: { type: "string" }, in: { type: "string" }, about: { type: "string" }, help: { type: "boolean", short: "h" } } });
  if (values.help || !positionals.length) { out(usage); process.exit(values.help ? 0 : 1); }
  const [dir, ...asks] = positionals;
  const made = printer(dir, { source: values.from, asks, output: values.out, description: values.about,
    inputs: values.in?.split(",").map((s) => s.trim()).filter(Boolean) });
  const shown = (file) => path.relative(process.cwd(), file) || file;
  const { harness } = made;
  out(`${OUTPUTS[harness.output].glyph} ${harness.output}  ${harness.name}  ← ${harness.inputs.join(" + ")}`);
  out(`an Aesel printer: ${shown(harness.dir)}/`);
  out(`  edit ${shown(path.join(harness.dir, "prompt.md"))}, then:`);
  out(`  ${shown(made.launcher)} ${harness.inputs.map((i) => (i === "text" ? '"…"' : `--${i} <path>`)).join(" ")}`);
} catch (error) {
  process.stderr.write(`aesthetic printer: ${error?.message || error}\n`);
  process.exit(1);
}
