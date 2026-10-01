#!/usr/bin/env node
// Generate llms.txt for LLM readability of aesthetic.computer.
// Uses llms.txt for a short map and llms-full.txt for the generated catalog.
//
// Each .mjs piece may export a `meta()` returning any of:
//   { title, desc, controls, keys, params, example }
// All fields are optional; `controls`/`keys`/`params`/`example` are surfaced
// to LLMs so an agent can learn how to drive the piece *before* loading it.
//
// Usage: node utilities/generate-llms-txt.mjs

import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);
const REPO_ROOT = path.resolve(__dirname, "..");
const DISKS_DIR = path.join(
  REPO_ROOT,
  "system",
  "public",
  "aesthetic.computer",
  "disks",
);
const OUTPUT_FILE = path.join(REPO_ROOT, "system", "public", "llms.txt");

const META_FIELDS = ["title", "desc", "controls", "keys", "params", "example"];
const ADVERTISED_FIELDS = ["controls", "keys", "params", "example"];

// Pull the body of `function meta(...)` (handles a few common shapes:
// `function meta()`, `async function meta()`, `const meta = () => { ... }`,
// `const meta = function() { ... }`). Returns the inner brace contents or null.
function extractMetaBody(source) {
  const patterns = [
    /\b(?:async\s+)?function\s+meta\s*\([^)]*\)\s*\{/,
    /\bconst\s+meta\s*=\s*(?:async\s*)?(?:function\s*\([^)]*\)|\([^)]*\)\s*=>)\s*\{/,
  ];
  for (const re of patterns) {
    const match = re.exec(source);
    if (!match) continue;
    const start = match.index + match[0].length;
    let depth = 1;
    for (let i = start; i < source.length; i++) {
      const ch = source[i];
      if (ch === "{") depth++;
      else if (ch === "}") {
        depth--;
        if (depth === 0) return source.slice(start, i);
      } else if (ch === "/" && source[i + 1] === "/") {
        const nl = source.indexOf("\n", i);
        i = nl === -1 ? source.length : nl;
      } else if (ch === "/" && source[i + 1] === "*") {
        const end = source.indexOf("*/", i + 2);
        i = end === -1 ? source.length : end + 1;
      } else if (ch === '"' || ch === "'" || ch === "`") {
        i = skipString(source, i, ch);
      }
    }
  }
  return null;
}

function skipString(source, i, quote) {
  for (let j = i + 1; j < source.length; j++) {
    const ch = source[j];
    if (ch === "\\") {
      j++;
      continue;
    }
    if (ch === quote) return j;
    if (quote === "`" && ch === "$" && source[j + 1] === "{") {
      let depth = 1;
      j += 2;
      while (j < source.length && depth > 0) {
        if (source[j] === "{") depth++;
        else if (source[j] === "}") depth--;
        j++;
      }
      j--;
    }
  }
  return source.length;
}

// Parse a single string-literal value following `field:` in a meta() body.
// Supports "..." 'quoted' and `template` strings (no interpolation expanded;
// template literals with ${...} are skipped to avoid emitting raw expressions).
function readMetaField(body, field) {
  const re = new RegExp(`\\b${field}\\s*:\\s*(["'\`])`);
  const m = re.exec(body);
  if (!m) return null;
  const quote = m[1];
  const start = m.index + m[0].length;
  let out = "";
  for (let i = start; i < body.length; i++) {
    const ch = body[i];
    if (ch === "\\") {
      const next = body[i + 1];
      if (next === "n") out += "\n";
      else if (next === "t") out += "\t";
      else if (next !== undefined) out += next;
      i++;
      continue;
    }
    if (ch === quote) return out.trim();
    if (quote === "`" && ch === "$" && body[i + 1] === "{") return null;
    out += ch;
  }
  return null;
}

function extractMeta(source) {
  const body = extractMetaBody(source);
  if (!body) return null;
  const meta = {};
  for (const field of META_FIELDS) {
    const value = readMetaField(body, field);
    if (value) meta[field] = value;
  }
  return Object.keys(meta).length > 0 ? meta : null;
}

function firstCommentLine(source, marker) {
  const lines = source.split("\n", 8);
  for (const line of lines) {
    const trimmed = line.trim();
    if (trimmed.startsWith(marker)) {
      return trimmed.slice(marker.length).trim();
    }
  }
  return "";
}

function describePiece(filePath, ext) {
  const source = fs.readFileSync(filePath, "utf8");
  if (ext === ".mjs") {
    const meta = extractMeta(source);
    const comment = firstCommentLine(source, "//");
    // Prefer a meta.desc when it parsed cleanly; otherwise the human-written
    // header comment is usually richer than a bare meta.title.
    const summary = (meta && meta.desc) || comment || (meta && meta.title) || "";
    return { summary, meta };
  }
  return { summary: firstCommentLine(source, ";"), meta: null };
}

function pieceLine(piece, ext, summary) {
  const url = `https://aesthetic.computer/${piece}`;
  const src = `https://github.com/whistlegraph/aesthetic-computer/blob/main/system/public/aesthetic.computer/disks/${piece}${ext}`;
  const tail = summary ? `: ${summary}` : "";
  return `- [${piece}](${url}) ([source](${src}))${tail}`;
}

function metaDetails(meta) {
  if (!meta) return "";
  const lines = [];
  for (const field of ADVERTISED_FIELDS) {
    if (meta[field]) lines.push(`  - ${field}: ${meta[field]}`);
  }
  return lines.length > 0 ? "\n" + lines.join("\n") : "";
}

const HEADER = `# Aesthetic Computer

> An open creative computing platform for making art, games, and tools in the browser using JavaScript and KidLisp.

Aesthetic Computer is a browser-based creative coding environment. Users write programs called "pieces" in JavaScript (.mjs) or KidLisp (.lisp) that run in a custom runtime with a pixel-art canvas, audio engine, and networking layer.

The main interface is a command prompt. Type a piece name and press Enter to load it. Pieces can also be accessed directly via URL: https://aesthetic.computer/{piece-name}

User-created pieces are stored under handles: https://aesthetic.computer/@{handle}/{piece-name}

KidLisp pieces use short codes: https://aesthetic.computer/\${code}

## Core Features

- [Prompt](https://aesthetic.computer/prompt): Command-line interface for navigating pieces, with LLM fallback for natural language
- [Painting](https://aesthetic.computer/painting): Digital painting viewer and tool
- [List](https://aesthetic.computer/list): Browsable directory of all available pieces and commands
- [About](https://aesthetic.computer/about): Interactive guide explaining the platform

## Creative Tools

- [Line](https://aesthetic.computer/line): Simple line drawing brush
- [Wand](https://aesthetic.computer/wand): Geometric visualization with VR support
- [Camera](https://aesthetic.computer/camera): Photo capture tool with filters
- [Whistlegraph](https://aesthetic.computer/whistlegraph): 2D recording tool for whistlegraphs

## KidLisp

- [KidLisp Reference](https://kidlisp.com/learn): Programming language reference and examples
- [KidLisp Default](https://aesthetic.computer/kidlisp): Default KidLisp piece with checkerboard pattern

## Source Code

- [Repository](https://tangled.org/aesthetic.computer/core): Canonical source repository. GitHub links below are the mirror.
- [Pieces (disks/)](https://github.com/whistlegraph/aesthetic-computer/tree/main/system/public/aesthetic.computer/disks): All piece source files (.mjs and .lisp)
- [Runtime Library (lib/)](https://github.com/whistlegraph/aesthetic-computer/tree/main/system/public/aesthetic.computer/lib): Core runtime modules (parse, graph, num, geo, text, pen, disk, help)
- [Boot Loader (boot.mjs)](https://github.com/whistlegraph/aesthetic-computer/blob/main/system/public/aesthetic.computer/boot.mjs): Application entry point
- [BIOS (bios.mjs)](https://github.com/whistlegraph/aesthetic-computer/blob/main/system/public/aesthetic.computer/bios.mjs): Hardware abstraction layer
- [Backend handlers](https://github.com/whistlegraph/aesthetic-computer/tree/main/system/netlify/functions): Server-side handlers hosted by Lith; the netlify directory name is historical.

## Machine access

- [Public HTTP API](https://aesthetic.computer/api?format=json): Supported endpoints, schemas, authentication and limits as JSON.
- [Piece runtime API](https://aesthetic.computer/docs.json): JavaScript lifecycle, drawing, audio, input and published piece metadata.
- [Remote MCP](https://aesthetic.computer/mcp): Existing JSON-RPC endpoint with validation, documentation resources and publishing tools. Publishing requires user intent; follow the API's authentication requirements.
- [Full piece catalog](https://aesthetic.computer/llms-full.txt): Generated names, source links and advertised controls.
- [Public network directory](https://aesthetic.computer/network.json): Product-specific machine entry points, including KidLisp, Aesel, papers, music releases and linked data.

Read only the resources needed for the task. Public discovery does not grant
account access or a license to reuse artworks. Operational MCP tools, private
notebooks, account telemetry and authenticated pages are not public resources.

## How to Interact

To use Aesthetic Computer via a browser:
1. Navigate to https://aesthetic.computer (loads the prompt piece by default)
2. Type a piece name (e.g. "painting", "line", "wand") and press Enter
3. Or visit https://aesthetic.computer/{piece-name} directly
4. User pieces: https://aesthetic.computer/@{handle}/{piece}
5. KidLisp codes: https://aesthetic.computer/\${nanoid-code}

Pieces may advertise their input model via a \`meta()\` export. Where present,
indented fields below a piece (\`controls\`, \`keys\`, \`params\`, \`example\`)
describe how to drive it without loading the runtime first. \`controls\`
documents pointer/touch behavior (drag, tap, etc.); \`keys\` lists keyboard
bindings; \`params\` describes URL/colon parameters; \`example\` is a ready-to-run
prompt invocation.
`;

function main() {
  console.log("Generating llms.txt...");

  // ASCII sort to match the previous shell-glob ordering ($ before digits,
  // digits before _, _ before letters).
  const entries = fs
    .readdirSync(DISKS_DIR)
    .filter((f) => f.endsWith(".mjs") || f.endsWith(".lisp"))
    .sort();

  const mjs = entries.filter((f) => f.endsWith(".mjs"));
  const lisp = entries.filter((f) => f.endsWith(".lisp"));

  let out = HEADER + "\n## Available Pieces\n\n";

  let metaCount = 0;
  for (const ext of [".mjs", ".lisp"]) {
    const list = ext === ".mjs" ? mjs : lisp;
    for (const file of list) {
      const piece = path.basename(file, ext);
      const filePath = path.join(DISKS_DIR, file);
      const { summary, meta } = describePiece(filePath, ext);
      out += pieceLine(piece, ext, summary) + metaDetails(meta) + "\n";
      if (meta && ADVERTISED_FIELDS.some((f) => meta[f])) metaCount++;
    }
  }

  const totals = `${entries.length} pieces (${mjs.length} JavaScript, ${lisp.length} KidLisp)`;
  out += `\n## Optional\n\n`;
  out += `- [Public API Documentation](https://aesthetic.computer/api-docs): Supported HTTP integrations.\n\n`;
  out += `_Generated by utilities/generate-llms-txt.mjs — ${totals}; ${metaCount} with advertised controls._\n`;

  const outputs = [[OUTPUT_FILE, HEADER.trimEnd() + "\n"], [OUTPUT_FILE.replace("llms.txt", "llms-full.txt"), out]];
  for (const [file, content] of outputs) {
    if (process.argv.includes("--check")) {
      if (!fs.existsSync(file) || fs.readFileSync(file, "utf8") !== content) {
        console.error(`Out of date: ${file}`);
        process.exitCode = 1;
      }
    } else fs.writeFileSync(file, content);
  }
  console.log(`Generated short map and full piece catalog${process.argv.includes("--check") ? " checked" : " written"}.`);
  console.log(`   ${totals}, ${metaCount} pieces advertise controls/keys/params/example.`);
}

main();
