import test from "node:test";
import assert from "node:assert/strict";
import { spawn } from "node:child_process";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { mkdtempSync, writeFileSync, rmSync, mkdirSync } from "node:fs";
import { tmpdir } from "node:os";
import { apiLookup, loadMap, outline, symbolText, outlineText, examples, handle, mcpConfig, TOOLS } from "../src/tools.mjs";

const here = path.dirname(fileURLToPath(import.meta.url));
const repo = path.resolve(here, "..", "..");
const server = path.join(here, "..", "src", "tools.mjs");

const PIECE = `// smiley, 2026
import { thing } from "./lib/thing.mjs";

const RADIUS = 20;

// Draw the face.
function paint({ wipe, ink, circle, screen }) {
  wipe("blue");
  ink("yellow").circle(screen.width / 2, screen.height / 2, RADIUS, true);
}

function act({ event: e }) {
  if (e.is("touch")) grow();
}

const grow = () => {
  // nothing yet
};

export { paint, act };
`;

function workspace() {
  const root = mkdtempSync(path.join(tmpdir(), "easel-tools-"));
  mkdirSync(path.join(root, "disks"));
  writeFileSync(path.join(root, "smiley.mjs"), PIECE);
  writeFileSync(path.join(root, "other.mjs"), `function paint({ circle }) { circle(1, 2, 3); }\nexport { paint };\n`);
  return { root, cleanup: () => rmSync(root, { recursive: true, force: true }) };
}

test("the API map is built and answers the questions sessions actually asked", () => {
  const map = loadMap();
  assert.ok(map.entries.length > 60, `map has ${map.entries.length} entries`);
  const circle = apiLookup(map, "circle");
  assert.match(circle, /^circle\n\s+circle\(x0, y0, radius, filled/m);
  assert.match(circle, /e\.g\. disks\//);
  const synth = apiLookup(map, "synth");
  assert.match(synth, /sound\.synth\n\s+sound\.synth\(\{ tone = 440/);
  const button = apiLookup(map, "button multitouch");
  assert.match(button, /ui\.Button/);
  assert.match(apiLookup(map, "zzzznotathing"), /Nothing in the API map/);
  // No query lists everything, one per line.
  const index = apiLookup(map, "").split("\n");
  assert.ok(index.length >= map.entries.length);
  assert.equal(new Set(index.map(line => line.split(" — ")[0])).size, index.length);
  assert.ok(index.some(line => line.startsWith("typeface — ")));
});

test("outline reads a flat piece as symbols with spans", () => {
  const { items, lines } = outline(PIECE);
  assert.equal(lines, PIECE.split("\n").length);
  const names = items.map((item) => `${item.kind}:${item.name}`);
  assert.deepEqual(names, [
    "import:./lib/thing.mjs",
    "value:RADIUS",
    "function:paint",
    "function:act",
    "function:grow",
    "exports:paint, act",
  ]);
  const paint = items.find((item) => item.name === "paint");
  assert.equal(paint.line, 7);
  // The span ends at paint's closing brace, not at act's opening line.
  assert.equal(paint.end, 10);
});

test("symbol and outline resolve a piece by bare name inside the workspace", () => {
  const { root, cleanup } = workspace();
  try {
    const text = symbolText(root, "smiley", "paint");
    assert.match(text, /^smiley\.mjs:7-10 {2}function paint/);
    assert.match(text, /circle\(screen\.width/);
    assert.match(symbolText(root, "smiley.mjs", "nope"), /No top-level symbol "nope"/);
    assert.match(outlineText(root, "smiley"), /5 top-level symbols/);
    assert.throws(() => outlineText(root, "../../etc/passwd"), /no such piece/);
    const hits = examples(root, "circle");
    assert.match(hits, /smiley\.mjs:9/);
    assert.match(hits, /other\.mjs:1/);
  } finally {
    cleanup();
  }
});

test("outline of a real large piece is a page, not a file", () => {
  const text = outlineText(repo, "notepat");
  const rows = text.split("\n");
  assert.match(rows[0], /notepat\.mjs — \d+ lines, \d+ top-level symbols/);
  assert.ok(rows.length > 20 && rows.length < 400, `${rows.length} rows`);
});

test("the JSON-RPC surface: initialize, list, call, unknown", () => {
  const context = { cwd: repo, map: loadMap() };
  const init = handle({ jsonrpc: "2.0", id: 1, method: "initialize", params: { protocolVersion: "2025-06-18" } }, context);
  assert.equal(init.result.protocolVersion, "2025-06-18");
  assert.deepEqual(init.result.capabilities, { tools: {} });
  assert.equal(handle({ jsonrpc: "2.0", method: "notifications/initialized" }, context), null);
  const list = handle({ jsonrpc: "2.0", id: 2, method: "tools/list" }, context);
  assert.deepEqual(list.result.tools.map((tool) => tool.name), ["ac_preview", "ac_frame", "ac_api", "ac_examples", "ac_outline", "ac_symbol"]);
  assert.equal(list.result.tools, TOOLS);
  const call = handle({ jsonrpc: "2.0", id: 3, method: "tools/call", params: { name: "ac_api", arguments: { query: "wipe" } } }, context);
  assert.match(call.result.content[0].text, /^wipe\n/);
  const bad = handle({ jsonrpc: "2.0", id: 4, method: "tools/call", params: { name: "ac_symbol", arguments: { file: "missing", name: "x" } } }, context);
  assert.equal(bad.result.isError, true);
  const unknown = handle({ jsonrpc: "2.0", id: 5, method: "resources/list" }, context);
  assert.equal(unknown.error.code, -32601);
});

test("the server runs on stdio and the config points the CLI at it", async () => {
  const config = mcpConfig(repo);
  assert.equal(config.mcpServers.ac.command, process.execPath);
  assert.deepEqual(config.mcpServers.ac.args.slice(1), ["--cwd", repo]);
  const child = spawn(config.mcpServers.ac.command, config.mcpServers.ac.args, { stdio: ["pipe", "pipe", "inherit"] });
  const out = [];
  child.stdout.on("data", (chunk) => out.push(chunk));
  child.stdin.write(`${JSON.stringify({ jsonrpc: "2.0", id: 1, method: "initialize", params: {} })}\n`);
  child.stdin.write(`${JSON.stringify({ jsonrpc: "2.0", id: 2, method: "tools/call", params: { name: "ac_outline", arguments: { file: "notepat" } } })}\n`);
  child.stdin.end();
  await new Promise((done) => child.on("close", done));
  const replies = Buffer.concat(out).toString().trim().split("\n").map((line) => JSON.parse(line));
  assert.equal(replies.length, 2);
  assert.equal(replies[0].result.serverInfo.name, "easel-ac");
  assert.match(replies[1].result.content[0].text, /notepat\.mjs — \d+ lines/);
});
