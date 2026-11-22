// printer — turn the thing you just made into a program that makes more of it.
//
// A printer is a harness: a folder with harness.json,
// prompt.md and a launcher named after it. The prompt is drafted from what was
// asked for and the thing that worked, kept beside it as the seed. The launcher
// runs on libaesel, so a printer is a standalone command with Aesel as its library.
//
//   poster/
//     harness.json   inputs in, one MIME type out
//     prompt.md      what it knows (the part to edit)
//     seed.svg       the one that worked
//     poster         ./poster "rooftop karaoke, friday"
import { chmodSync, copyFileSync, existsSync, mkdirSync, readFileSync, readdirSync, statSync, writeFileSync } from "node:fs";
import { homedir } from "node:os";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { INPUTS, OUTPUTS, outputType, readHarness, validateManifest } from "./harness-factory.mjs";

/** The Aesel this copy of the library belongs to: where a printer looks first. */
// Found by walking up, because the terminal interface runs as one bundled file
// whose own address is not this file's.
export const AESEL_HOME = (() => {
  let dir = path.dirname(fileURLToPath(import.meta.url));
  for (let i = 0; i < 4; i++, dir = path.dirname(dir)) if (existsSync(path.join(dir, "src", "libaesel.mjs"))) return dir;
  return "";
})();
const MAX_SEED = 32 * 1024;

// Every printer has a code: a short sayable name (zip, gat, lule) to call it by,
// in a room or from the Aesel thread that made it. They are made the way slab's
// prox names are (consonant, vowel, consonant…), and a machine that runs slab
// never gets a code one of its prox already has. Slab does not have to be there.
const CONSONANTS = "bdfgklmnprstvz", VOWELS = "aeiou";
const REGISTRY = path.join(homedir(), ".config", "aesel", "printers.json");
const readJson = (file) => { try { return JSON.parse(readFileSync(file, "utf8")); } catch { return null; } };

/** Codes already spoken for on this machine: slab's prox and Aesel's printers. */
export function takenCodes({ home = homedir(), registry = REGISTRY } = {}) {
  const ledger = path.join(home, ".config", "slab", "ledger");
  const files = [path.join(ledger, "local.json")];
  try { for (const peer of readdirSync(path.join(ledger, "peers"))) if (peer.endsWith(".json")) files.push(path.join(ledger, "peers", peer)); } catch { /* no slab here */ }
  const names = files.flatMap((file) => (readJson(file)?.entries || []).map((entry) => String(entry.name || "").toLowerCase()));
  return new Set([...names, ...Object.keys(readJson(registry) || {})].filter(Boolean));
}

/** A fresh code: three letters, longer only when the short ones run out. */
export function printerCode(taken = takenCodes(), random = Math.random) {
  for (let length = 3; ; length++) for (let tries = 0; tries < 60; tries++) {
    const code = Array.from({ length }, (_, i) => { const set = i % 2 ? VOWELS : CONSONANTS; return set[Math.floor(random() * set.length)]; }).join("");
    if (!taken.has(code)) return code;
  }
}

/** The printers this machine knows, by code: { name, dir }. */
export const printers = ({ registry = REGISTRY } = {}) => readJson(registry) || {};

/** Give the printer in `root` its code (kept in harness.json) and list it. */
export function codeFor(root, { registry = REGISTRY } = {}) {
  const file = path.join(root, "harness.json");
  const manifest = readJson(file);
  if (!manifest) return "";
  const listed = !process.env.NODE_TEST_CONTEXT;
  if (!manifest.printer?.code) {
    manifest.printer = { of: "aesel", ...manifest.printer, code: printerCode(listed ? takenCodes({ registry }) : new Set()) };
    writeFileSync(file, JSON.stringify(manifest, null, 2) + "\n");
  }
  if (listed) {
    const known = printers({ registry });
    known[manifest.printer.code] = { name: manifest.name, dir: root };
    mkdirSync(path.dirname(registry), { recursive: true });
    writeFileSync(registry, JSON.stringify(known, null, 2) + "\n");
  }
  return manifest.printer.code;
}
const EXTENSIONS = { js: "text/javascript", mjs: "text/javascript", lisp: "text/x-lisp", svg: "image/svg+xml", html: "text/html", htm: "text/html",
  md: "text/markdown", txt: "text/plain", json: "application/json", csv: "text/csv", png: "image/png", wav: "audio/wav" };

/** The MIME goal a seed file implies, or undefined when its extension says nothing. */
export const printerOutput = (file) => EXTENSIONS[path.extname(String(file || "")).slice(1).toLowerCase()];

const slug = (text) => String(text || "").toLowerCase().replace(/[^a-z0-9-]+/g, "-").replace(/^-+|-+$/g, "").slice(0, 40);
const fenceOf = (mime) => OUTPUTS[mime].lang.replace(/^\(\?:([a-z0-9]+).*$/, "$1");

function prompt({ name, output, inputs, asks, seedName, seedText }) {
  const goal = OUTPUTS[output];
  return [
    `# ${name}`,
    "",
    `You make one ${goal.kind} at a time, in the manner of the one below. It was made in Aesel, and its maker liked it enough to keep the recipe.`,
    "",
    "## What the maker asked for",
    "",
    ...(asks.length ? asks.map((ask) => `- ${ask.replace(/\s+/g, " ").trim()}`) : ["- (nothing was recorded; say here what this is for)"]),
    "",
    ...(seedText ? [
      "## The one that worked",
      "",
      `This is ${seedName}. Keep what makes it recognisable: its structure, its palette, its pacing. Change the subject to whatever comes in.`,
      "",
      "```" + fenceOf(output),
      seedText.replace(/\s+$/, ""),
      "```",
      "",
    ] : seedName ? [`The one that worked is kept beside this file as ${seedName}. Describe here what makes it good, since it cannot be read from the file.`, ""] : []),
    "## What comes in",
    "",
    ...inputs.map((input) => `- ${input}: ${INPUTS[input].about}`),
    "",
    "## Your taste",
    "",
    "Edit this part. Say what a good one has, and what you never want to see.",
    "",
  ].join("\n");
}

const launcher = (name, description, born) => `#!/usr/bin/env node
// ${name} — an Aesel printer.${description ? ` ${description}` : ""}
//
//   ./${name} "words"        make one
//   ./${name} --dry "words"  show what would be sent, spend nothing
//   ./${name} --serve        switch it on: it appears in the Print dialog
//   ./${name} --help
//
// This folder is the whole program: harness.json says what goes in and what
// comes out, prompt.md is what it knows. It runs on libaesel.
import { execFileSync } from "node:child_process";
import { existsSync, realpathSync } from "node:fs";
import path from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";

const here = path.dirname(fileURLToPath(import.meta.url));
const born = ${JSON.stringify(born)};
const installed = () => ["a", "aesel", "ac"].map((command) => {
  try { return path.dirname(path.dirname(realpathSync(execFileSync("which", [command], { encoding: "utf8", stdio: ["ignore", "pipe", "ignore"] }).trim()))); } catch { return ""; }
});
const lib = [process.env.AESEL_HOME, born, ...installed()].filter(Boolean)
  .map((home) => path.join(home, "src", "libaesel.mjs")).find((file) => existsSync(file));
if (!lib) {
  console.error("${name} needs Aesel: curl -fsSL https://prompt.ac/aesel.sh | sh");
  process.exit(1);
}
const { main } = await import(pathToFileURL(lib).href);
try { await main(here, process.argv.slice(2)); } catch (error) { console.error(\`${name}: \${error?.message || error}\`); process.exit(1); }
`;

/**
 * Make a printer in `dir`. `source` is the file that worked (optional), `asks` the
 * prompts that led to it. Returns the harness, its launcher and its seed.
 */
export function printer(dir, { source = "", asks = [], output, inputs, description = "", cwd = process.cwd(), born = AESEL_HOME } = {}) {
  const seedFile = source ? path.resolve(cwd, source) : "";
  const automatic = dir == null;
  const name = slug(automatic ? path.parse(seedFile).name : path.basename(path.resolve(cwd, dir))) || "printer";
  let root = path.resolve(cwd, automatic ? name : dir);
  if (!automatic && existsSync(path.join(root, "harness.json"))) throw new Error(`${dir} already has a harness.json`);
  if (seedFile && !existsSync(seedFile)) throw new Error(`no such file: ${source}`);
  const mime = output ? outputType(output) : printerOutput(seedFile);
  if (!mime) throw new Error(seedFile ? `${path.basename(seedFile)} does not say what kind of thing this is — add --out <mime>` : "name what it makes: --from <file> or --out <mime>");
  const manifest = validateManifest({ name, inputs: inputs?.length ? inputs : ["text"], output: mime, description });
  const said = asks.map((ask) => String(ask || "").trim()).filter(Boolean).slice(-12);
  const seedName = seedFile ? `seed.${OUTPUTS[mime].ext}` : "";
  const readable = seedFile && OUTPUTS[mime].as === "text" && statSync(seedFile).size <= MAX_SEED;

  if (automatic) {
    // Reserve a fresh folder for each click, even across concurrent sessions.
    for (let next = 2; ; next++) {
      try { mkdirSync(root); break; }
      catch (error) {
        if (error.code !== "EEXIST") throw error;
        root = path.join(cwd, `${name}-${next}`);
      }
    }
  } else mkdirSync(root, { recursive: true });
  writeFileSync(path.join(root, "harness.json"), JSON.stringify({
    name, ...(manifest.description ? { description: manifest.description } : {}), inputs: manifest.inputs, output: manifest.output,
    printer: { of: "aesel", ...(seedName ? { seed: seedName } : {}) },
  }, null, 2) + "\n");
  if (seedFile) copyFileSync(seedFile, path.join(root, seedName));
  writeFileSync(path.join(root, "prompt.md"), prompt({ name, output: mime, inputs: manifest.inputs, asks: said, seedName,
    seedText: readable ? readFileSync(seedFile, "utf8") : "" }));
  const run = path.join(root, name);
  writeFileSync(run, launcher(name, manifest.description, born));
  chmodSync(run, 0o755);
  // Double-clicked, or opened by the Printer button: the printer's own window.
  const on = path.join(root, "switch-on.command");
  writeFileSync(on, `#!/bin/sh\ncd "$(dirname "$0")" && exec ./${name} --serve\n`);
  chmodSync(on, 0o755);
  const code = codeFor(root);
  return { on, code, harness: readHarness(root), launcher: run, seed: seedName ? path.join(root, seedName) : "" };
}
