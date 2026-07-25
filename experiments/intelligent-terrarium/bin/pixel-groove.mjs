#!/usr/bin/env node
import { readFile, writeFile } from "node:fs/promises";
import { resolve } from "node:path";
import { canonical } from "../src/canonical.mjs";
import { evaluateRasterProgram, NoveltyArchive } from "../src/sort-soup.mjs";
import { grooveHex, inspectPixelGroove, printPixelGroove, zeroPixelGroove } from "../src/pixel-groove.mjs";

function option(name, fallback = null) {
  const index = process.argv.indexOf(name);
  return index < 0 ? fallback : process.argv[index + 1];
}

function positional() {
  const values = [];
  for (let index = 3; index < process.argv.length; index += 1) {
    if (process.argv[index].startsWith("--")) { index += 1; continue; }
    values.push(process.argv[index]);
  }
  return values;
}

async function writeRecord(prefix, groove, field = null) {
  const target = resolve(prefix);
  const record = inspectPixelGroove(groove);
  await writeFile(`${target}.pgr`, groove, { mode: 0o600 });
  await writeFile(`${target}.ppm`, printPixelGroove(groove, { field }), { mode: 0o600 });
  await writeFile(`${target}.json`, `${canonical(record)}\n`, { mode: 0o600 });
  return { record: `${target}.pgr`, print: `${target}.ppm`, map: `${target}.json` };
}

const command = process.argv[2];
if (command === "start") {
  const [source] = positional();
  if (!source) throw new Error("usage: pixel-groove start '(raster ...)' [--output record]");
  const candidate = evaluateRasterProgram(source, {
    origin: option("--origin", "groove-press"), parent: option("--parent", null),
    generation: Number(option("--generation", "0")), profile: option("--profile", "standard"),
  });
  const groove = Buffer.from(candidate.sample.groove, "hex");
  console.log(JSON.stringify({ id: candidate.id, ...await writeRecord(option("--output", candidate.id), groove, Buffer.from(candidate.sample.rgb, "hex")) }, null, 2));
} else if (command === "zero") {
  const id = option("--id", "blank");
  const groove = zeroPixelGroove({ id, parent: option("--parent", ""), generation: Number(option("--generation", "0")),
    profile: option("--profile", "standard") });
  console.log(JSON.stringify({ id, ...await writeRecord(option("--output", id), groove) }, null, 2));
} else if (command === "print" || command === "inspect") {
  const [input] = positional();
  if (!input) throw new Error(`usage: pixel-groove ${command} record.pgr [--output record.ppm]`);
  const groove = new Uint8Array(await readFile(resolve(input)));
  const record = inspectPixelGroove(groove);
  if (command === "print") await writeFile(resolve(option("--output", `${input}.ppm`)), printPixelGroove(groove), { mode: 0o600 });
  console.log(JSON.stringify(record, null, 2));
} else if (command === "upconvert") {
  const [input] = positional();
  const output = option("--output");
  if (!input || !output) throw new Error("usage: pixel-groove upconvert archive.json --output archive-v1.json");
  const archive = NoveltyArchive.fromJSON(JSON.parse(await readFile(resolve(input), "utf8")));
  const converted = archive.toJSON();
  await writeFile(resolve(output), `${canonical(converted)}\n`, { mode: 0o600 });
  const records = converted.recent.filter((candidate) => candidate.domain === "raster" && candidate.grooveVersion === 1).length;
  console.log(JSON.stringify({ input: resolve(input), output: resolve(output), records, schema: converted.schema }, null, 2));
} else {
  console.error("usage: pixel-groove start|zero|print|inspect|upconvert ...");
  process.exitCode = 2;
}
