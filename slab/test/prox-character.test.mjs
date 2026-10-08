import test from "node:test";
import assert from "node:assert/strict";
import { readFile, mkdir, mkdtemp, writeFile, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

// Exercise the production handler with a ledger fixture and no session/MCP
// daemon. The renderer is independently checked with the Swift export probe.
const source = await readFile(new URL("../bin/prox-mcp.mjs", import.meta.url), "utf8");
const handler = source.slice(source.indexOf("async function toolCharacter("), source.indexOf("async function toolPoke("));
const appearance = { schemaVersion: 1, id: "fixture", name: "miva", seed: "000000000000007b",
  bornAt: 1800000000000, activeSeconds: 0, stage: "egg", traits: [] };
const row = { host: "fixture", name: "miva", subject: "private subject", cwd: "/private/path",
  memoir: "private prose", creature: appearance };
function tool(rows, renderer = async () => false) {
  return new Function("resolve", "allRocks", "join", "mkdir", "writeFile", "renderRockBundle",
    `${handler}\nreturn toolCharacter;`)(rocks => rocks, async () => rows, join, mkdir, writeFile, renderer);
}

test("character reads expose only the portable appearance", async () => {
  const result = await tool([row])({ handle: "fixture:miva" });
  assert.deepEqual(JSON.parse(result[0].text), appearance);
  assert.doesNotMatch(result[0].text, /private|subject|memoir|cwd/);
});

test("character reads refuse ambiguity and older ledgers without a creature", async () => {
  await assert.rejects(tool([])({ handle: "missing" }), /found 0/);
  await assert.rejects(tool([row, row])({ handle: "miva" }), /found 2/);
  await assert.rejects(tool([{ ...row, creature: undefined }])({ handle: "miva" }), /no saved creature/);
});

test("appearance export preserves JSON even when images are unavailable and refuses overwrite", async t => {
  const destination = await mkdtemp(join(tmpdir(), "prox-character-"));
  t.after(() => rm(destination, { recursive: true, force: true }));
  let rendered;
  const exportCharacter = tool([row], async (record, path) => { rendered = { record, path }; return false; });
  const result = await exportCharacter({ handle: "fixture:miva", destination });
  assert.match(result[0].text, /image renderer unavailable/);
  assert.equal(rendered.record.creature, appearance);
  assert.deepEqual(JSON.parse(await readFile(join(rendered.path, "character.json"), "utf8")), appearance);
  await assert.rejects(exportCharacter({ handle: "fixture:miva", destination }), /EEXIST/);
});
