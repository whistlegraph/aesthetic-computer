#!/usr/bin/env node
// tools.mjs — the native tools Aesel hands the engine, as an MCP server on stdio.
//
// Read the transcripts of the first ten Aesel sessions and they open the same
// way: the model reads the guides, then spends six to twelve shell calls —
// `grep -n "function circle(" graph.mjs`, `sed -n 6590,6650p disk.mjs`,
// `grep -rn "synth({" disks/*.mjs | head` — rebuilding a picture of the API
// that the previous session had already built and thrown away. A minute or two
// per session before the first edit, on a surface that does not change.
//
// So the picture is built once (`bin/build-api-map.mjs` → `context/api.json`)
// and served here, alongside the two things a large piece needs that `sed -n`
// gives badly: an outline of its symbols, and one symbol's source by name.
// Four tools, all read-only, all answered from local files:
//
//   ac_api       what does `circle` / `sound.synth` / `ui.Button` take?
//   ac_examples  show me pieces that call it
//   ac_outline   what is in notepat.mjs, and where?
//   ac_symbol    give me `setupButtons` from notepat.mjs
//
// This is an MCP server without a dependency: the protocol is JSON-RPC over
// newline-delimited stdio, and a server that only lists and calls tools needs
// four methods. Claude Code is pointed at it with `--mcp-config`, which is the
// one hole `--strict-mcp-config` leaves open on purpose.
//
//   node src/tools.mjs --cwd /path/to/workspace
import { readFileSync, readdirSync, existsSync, statSync } from "node:fs";
import { dirname, join, resolve, relative, isAbsolute } from "node:path";
import { fileURLToPath } from "node:url";
import { createInterface } from "node:readline";

const HERE = dirname(fileURLToPath(import.meta.url));
const AESEL = join(HERE, "..");

export const SERVER_NAME = "ac";
export const TOOL_PREFIX = `mcp__${SERVER_NAME}__`;

// Where the pieces are: the repo's disks folder when the workspace is the
// Aesthetic Computer repository, the workspace itself anywhere else.
export function disksDir(cwd) {
  const inRepo = join(cwd, "system", "public", "aesthetic.computer", "disks");
  return existsSync(inRepo) ? inRepo : cwd;
}

export function loadMap() {
  try {
    return JSON.parse(readFileSync(join(AESEL, "context", "api.json"), "utf8"));
  } catch {
    return { entries: [] };
  }
}

// ---------------------------------------------------------------- ac_api ----

function scoreEntry(entry, terms) {
  const path = entry.path.toLowerCase();
  const name = entry.name.toLowerCase();
  const hay = `${path} ${entry.signature} ${entry.doc}`.toLowerCase();
  let score = 0;
  for (const term of terms) {
    if (name === term || path === term) score += 100;
    else if (name.startsWith(term) || path.endsWith(`.${term}`)) score += 40;
    else if (path.includes(term)) score += 20;
    else if (hay.includes(term)) score += 5;
  }
  return score;
}

export function apiLookup(map, query, { limit = 6 } = {}) {
  const terms = String(query || "")
    .toLowerCase()
    .split(/[^a-z0-9_.$]+/)
    .filter(Boolean);
  if (!terms.length) {
    return map.entries.map((entry) => `${entry.path} — ${entry.signature}`).join("\n");
  }
  const ranked = map.entries
    .map((entry) => [scoreEntry(entry, terms), entry])
    .filter(([score]) => score > 0)
    .sort((a, b) => b[0] - a[0])
    .slice(0, limit)
    .map(([, entry]) => entry);
  if (!ranked.length) return `Nothing in the API map matches "${query}". Call ac_api with no query for the full list.`;
  return ranked.map(describe).join("\n\n");
}

function describe(entry) {
  const lines = [`${entry.path}`, `  ${entry.signature}`];
  if (entry.doc) lines.push(`  ${entry.doc}`);
  if (entry.source) lines.push(`  source: ${entry.source}`);
  for (const example of entry.examples || []) lines.push(`  e.g. ${example}`);
  return lines.join("\n");
}

// ----------------------------------------------------------- ac_examples ----

export function examples(cwd, symbol, { limit = 12 } = {}) {
  const dir = disksDir(cwd);
  const leaf = String(symbol || "").trim();
  if (!leaf) return "Name a symbol, e.g. synth or ui.Button.";
  const escaped = leaf.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
  const needle = new RegExp(leaf.includes(".") ? `\\b${escaped}\\b` : `(?<![\\w$])${escaped}\\b`);
  const files = readdirSync(dir)
    .filter((f) => /\.(mjs|lisp)$/.test(f))
    .sort();
  const found = [];
  for (const file of files) {
    let text;
    try {
      text = readFileSync(join(dir, file), "utf8");
    } catch {
      continue;
    }
    const lines = text.split("\n");
    let perFile = 0;
    for (let i = 0; i < lines.length && found.length < limit && perFile < 3; i++) {
      if (!needle.test(lines[i])) continue;
      if (/^\s*\/\//.test(lines[i])) continue;
      found.push(`${file}:${i + 1}  ${lines[i].trim().slice(0, 160)}`);
      perFile++;
    }
    if (found.length >= limit) break;
  }
  if (!found.length) return `No piece in ${relative(cwd, dir) || "."} calls ${leaf}.`;
  return found.join("\n");
}

// ------------------------------------------------- ac_outline / ac_symbol ----

// Resolve a piece name or path to a file, never outside the workspace.
export function resolvePiece(cwd, file) {
  const raw = String(file || "").trim();
  if (!raw) throw new Error("name a file, e.g. notepat.mjs");
  const candidates = [];
  if (isAbsolute(raw)) candidates.push(raw);
  else {
    candidates.push(resolve(cwd, raw));
    const dir = disksDir(cwd);
    candidates.push(resolve(dir, raw));
    if (!/\.\w+$/.test(raw)) {
      candidates.push(resolve(dir, `${raw}.mjs`), resolve(dir, `${raw}.lisp`));
    }
  }
  for (const path of candidates) {
    const inside = !relative(cwd, path).startsWith("..");
    if (inside && existsSync(path) && statSync(path).isFile()) return path;
  }
  throw new Error(`no such piece: ${raw}`);
}

const SYMBOL_LINE = [
  // export function paint({ ... }) {
  [/^(?:export\s+)?(?:async\s+)?function\s*\*?\s*([\w$]+)\s*\(/, "function"],
  // const foo = (a, b) => {   /  const foo = function
  [/^(?:export\s+)?(?:const|let|var)\s+([\w$]+)\s*=\s*(?:async\s*)?(?:\([^)]*\)|[\w$]+)\s*=>/, "function"],
  [/^(?:export\s+)?(?:const|let|var)\s+([\w$]+)\s*=\s*(?:async\s+)?function\b/, "function"],
  [/^(?:export\s+)?class\s+([\w$]+)/, "class"],
  // top-level data: const buttons = {  /  let x = 0
  [/^(?:export\s+)?(?:const|let|var)\s+([\w$]+)\s*=/, "value"],
  [/^export\s*\{([^}]*)\}/, "exports"],
  [/^import\b.*from\s+["']([^"']+)["']/, "import"],
];

// The top-level shape of a JavaScript piece: every symbol declared at column
// zero, with the line where it starts and where the next one begins. Column
// zero is the whole heuristic — pieces are written flat, one function after
// another, and nesting inside a symbol is exactly what the outline is meant to
// skip over.
export function outline(source) {
  const lines = source.split("\n");
  const items = [];
  for (let i = 0; i < lines.length; i++) {
    const text = lines[i];
    if (!text || /^\s/.test(text)) continue;
    for (const [pattern, kind] of SYMBOL_LINE) {
      const match = text.match(pattern);
      if (!match) continue;
      items.push({ name: match[1].trim(), kind, line: i + 1 });
      break;
    }
  }
  for (let i = 0; i < items.length; i++) {
    const next = items[i + 1];
    let end = next ? next.line - 1 : lines.length;
    // Trim trailing blank lines and comments off the span so a symbol's source
    // ends where its brace does, not where the next one's header comment begins.
    while (end > items[i].line && /^\s*(\/\/.*)?$/.test(lines[end - 1])) end--;
    items[i].end = end;
  }
  return { lines: lines.length, items };
}

export function outlineText(cwd, file) {
  const path = resolvePiece(cwd, file);
  const source = readFileSync(path, "utf8");
  if (path.endsWith(".lisp")) {
    const heads = source
      .split("\n")
      .map((text, i) => [text, i + 1])
      .filter(([text]) => /^\(/.test(text))
      .map(([text, line]) => `${String(line).padStart(5)}  ${text.slice(0, 80)}`);
    return [`${relative(cwd, path)} — ${source.split("\n").length} lines, ${heads.length} top-level forms`, ...heads].join("\n");
  }
  const { lines, items } = outline(source);
  const rows = items
    .filter((item) => item.kind !== "import")
    .map((item) => `${String(item.line).padStart(5)}-${String(item.end).padEnd(5)} ${item.kind.padEnd(8)} ${item.name}`);
  const imports = items.filter((item) => item.kind === "import").map((item) => item.name);
  return [
    `${relative(cwd, path)} — ${lines} lines, ${rows.length} top-level symbols${imports.length ? `, imports: ${imports.join(", ")}` : ""}`,
    "lines       kind     name",
    ...rows,
  ].join("\n");
}

export function symbolText(cwd, file, name, { maxLines = 220 } = {}) {
  const path = resolvePiece(cwd, file);
  const source = readFileSync(path, "utf8");
  const wanted = String(name || "").trim();
  const { items } = outline(source);
  const item = items.find((entry) => entry.name === wanted) || items.find((entry) => entry.name.startsWith(wanted));
  if (!item) {
    const near = items.filter((entry) => entry.name.toLowerCase().includes(wanted.toLowerCase())).map((entry) => entry.name);
    return `No top-level symbol "${wanted}" in ${relative(cwd, path)}.${near.length ? ` Close: ${near.join(", ")}.` : " Call ac_outline to see what is there."}`;
  }
  const lines = source.split("\n");
  const span = lines.slice(item.line - 1, item.end);
  const clipped = span.length > maxLines;
  const shown = clipped ? span.slice(0, maxLines) : span;
  const numbered = shown.map((text, i) => `${String(item.line + i).padStart(5)}  ${text}`);
  const head = `${relative(cwd, path)}:${item.line}-${item.end}  ${item.kind} ${item.name}`;
  const tail = clipped ? `… ${span.length - maxLines} more lines; read ${relative(cwd, path)} from line ${item.line + maxLines} for the rest.` : "";
  return [head, ...numbered, tail].filter(Boolean).join("\n");
}

// ------------------------------------------------------------- the server ----

export const TOOLS = [
  {
    name: "ac_api",
    description:
      "Look up the Aesthetic Computer piece API: what a drawing primitive, sound call, ui class or event takes, with its runtime signature and real call sites from existing pieces. Use this before grepping graph.mjs or disk.mjs. No query lists every name.",
    inputSchema: {
      type: "object",
      properties: {
        query: { type: "string", description: "A name or words: circle, synth, button, text, multitouch." },
      },
    },
  },
  {
    name: "ac_examples",
    description:
      "Lines from existing pieces that call a symbol (e.g. synth, pline, ui.Button, hud.label), as file:line. Use instead of grep -rn over disks/.",
    inputSchema: {
      type: "object",
      properties: {
        symbol: { type: "string", description: "The function or dotted name to find call sites for." },
        limit: { type: "integer", description: "Max lines (default 12)." },
      },
      required: ["symbol"],
    },
  },
  {
    name: "ac_outline",
    description:
      "The top-level symbols of a piece with their line spans — functions, classes, values, exports. Use before reading a large piece so you can fetch one symbol with ac_symbol instead of paging through it.",
    inputSchema: {
      type: "object",
      properties: {
        file: { type: "string", description: "A piece name (notepat), file (notepat.mjs) or path." },
      },
      required: ["file"],
    },
  },
  {
    name: "ac_symbol",
    description: "The full source of one top-level symbol from a piece, numbered by line. Pairs with ac_outline.",
    inputSchema: {
      type: "object",
      properties: {
        file: { type: "string", description: "A piece name, file or path." },
        name: { type: "string", description: "The symbol to fetch, as ac_outline listed it." },
      },
      required: ["file", "name"],
    },
  },
];

export function callTool(name, args, { cwd, map }) {
  switch (name) {
    case "ac_api":
      return apiLookup(map, args?.query);
    case "ac_examples":
      return examples(cwd, args?.symbol, { limit: Number(args?.limit) || 12 });
    case "ac_outline":
      return outlineText(cwd, args?.file);
    case "ac_symbol":
      return symbolText(cwd, args?.file, args?.name);
    default:
      throw new Error(`unknown tool: ${name}`);
  }
}

// One JSON-RPC message in, at most one out. Notifications get nothing back.
export function handle(message, context) {
  const { id, method, params } = message;
  const reply = (result) => (id === undefined ? null : { jsonrpc: "2.0", id, result });
  const fail = (code, text) => (id === undefined ? null : { jsonrpc: "2.0", id, error: { code, message: text } });
  switch (method) {
    case "initialize":
      return reply({
        protocolVersion: params?.protocolVersion || "2025-06-18",
        capabilities: { tools: {} },
        serverInfo: { name: `easel-${SERVER_NAME}`, version: "1" },
      });
    case "notifications/initialized":
    case "notifications/cancelled":
      return null;
    case "ping":
      return reply({});
    case "tools/list":
      return reply({ tools: TOOLS });
    case "tools/call": {
      try {
        const text = callTool(params?.name, params?.arguments || {}, context);
        return reply({ content: [{ type: "text", text }] });
      } catch (error) {
        return reply({ content: [{ type: "text", text: String(error?.message || error) }], isError: true });
      }
    }
    default:
      return fail(-32601, `method not found: ${method}`);
  }
}

export function serve({ cwd = process.cwd(), input = process.stdin, output = process.stdout } = {}) {
  const context = { cwd: resolve(cwd), map: loadMap() };
  const lines = createInterface({ input, crlfDelay: Infinity });
  lines.on("line", (line) => {
    if (!line.trim()) return;
    let message;
    try {
      message = JSON.parse(line);
    } catch {
      output.write(`${JSON.stringify({ jsonrpc: "2.0", id: null, error: { code: -32700, message: "parse error" } })}\n`);
      return;
    }
    const response = handle(message, context);
    if (response) output.write(`${JSON.stringify(response)}\n`);
  });
  return lines;
}

// The MCP configuration the Claude bridge passes with --mcp-config: this file,
// run by the same node that is running Aesel, pointed at the workspace.
export function mcpConfig(cwd) {
  return {
    mcpServers: {
      [SERVER_NAME]: {
        command: process.execPath,
        args: [fileURLToPath(import.meta.url), "--cwd", cwd],
      },
    },
  };
}

if (process.argv[1] === fileURLToPath(import.meta.url)) {
  const at = process.argv.indexOf("--cwd");
  serve({ cwd: at >= 0 ? process.argv[at + 1] : process.cwd() });
}
