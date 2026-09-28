import assert from "node:assert/strict";
import { mkdtempSync, readFileSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import test from "node:test";
import { CLAUDE_FALLBACK, cachedCatalog, catalogFile, claudeCredential, claudeModels, family, loadCatalog, newestModel, preferNewer } from "../src/model-catalog.mjs";
import { pickerModels } from "../src/provider-picker.mjs";

const answer = (data, ok = true, status = 200) => async () => ({ ok, status, json: async () => ({ data }) });
const listed = [
  { id: "claude-opus-5-5", display_name: "Claude Opus 5.5", created_at: "2026-09-21T00:00:00Z" },
  { id: "claude-sonnet-5", display_name: "Claude Sonnet 5", created_at: "2026-06-29T00:00:00Z" },
  { id: "not-a-claude", display_name: "Other", created_at: "2026-01-01T00:00:00Z" },
];

test("the credential is resolved the way the SDK resolves it, and the keychain is the last resort", async () => {
  assert.deepEqual(await claudeCredential({ env: { ANTHROPIC_API_KEY: "k" }, keychain: async () => "t" }), { "x-api-key": "k" });
  assert.deepEqual(await claudeCredential({ env: { ANTHROPIC_AUTH_TOKEN: "a" }, keychain: async () => "t" }), { authorization: "Bearer a" });
  assert.deepEqual(await claudeCredential({ env: {}, keychain: async () => "t" }), { authorization: "Bearer t", "anthropic-beta": "oauth-2025-04-20" });
  assert.equal(await claudeCredential({ env: {}, keychain: async () => { throw new Error("no keychain"); } }), null);
});

test("the Models API answer is filtered to Claude models and ordered newest first", async () => {
  const models = await claudeModels({ env: { ANTHROPIC_API_KEY: "k" }, fetch: answer(listed) });
  assert.deepEqual(models.map((m) => m.id), ["claude-opus-5-5", "claude-sonnet-5"]);
  assert.equal(models[0].displayName, "Claude Opus 5.5");
  await assert.rejects(claudeModels({ env: {}, keychain: async () => "", fetch: answer(listed) }), /no Anthropic credential/);
  await assert.rejects(claudeModels({ env: { ANTHROPIC_API_KEY: "k" }, fetch: answer([], false, 401) }), /401/);
});

test("the catalog is cached on disk, served fresh from the cache, and falls back when the ask fails", async (context) => {
  const root = mkdtempSync(join(tmpdir(), "catalog-"));
  context.after(() => rmSync(root, { recursive: true, force: true }));
  let asks = 0;
  const loaders = { claude: async () => { asks += 1; return [{ id: "claude-opus-5-5", displayName: "Claude Opus 5.5" }]; } };
  let clock = 1000;
  const now = () => clock;
  const first = await loadCatalog("claude", { root, loaders, now });
  assert.equal(first[0].id, "claude-opus-5-5");
  assert.equal(asks, 1);
  assert.ok(readFileSync(catalogFile("claude", root), "utf8").includes("claude-opus-5-5"), "written to disk");
  await loadCatalog("claude", { root, loaders, now });
  assert.equal(asks, 1, "a fresh cache is not asked again");
  clock += 7 * 60 * 60 * 1000;
  await loadCatalog("claude", { root, loaders, now });
  assert.equal(asks, 2, "a stale cache is asked again");
  const broken = { claude: async () => { throw new Error("offline"); } };
  const stale = await loadCatalog("claude", { root, loaders: broken, now: () => clock + 8 * 60 * 60 * 1000 });
  assert.equal(stale[0].id, "claude-opus-5-5", "the stale cache answers when the ask fails");
  writeFileSync(catalogFile("claude", root), "garbage");
  assert.deepEqual(await loadCatalog("claude", { root, loaders: broken, now }), CLAUDE_FALLBACK, "and the table answers when there is no cache");
  assert.deepEqual(await loadCatalog("ac", { root, loaders, now }), [], "a provider with no loader has no catalog");
});

test("the picker shows the live Claude list with versions, and an alias selects its newest family member", () => {
  const catalog = [
    { id: "claude-opus-5-5", displayName: "Claude Opus 5.5" },
    { id: "claude-opus-5", displayName: "Claude Opus 5" },
    { id: "claude-sonnet-5", displayName: "Claude Sonnet 5" },
  ];
  const rows = pickerModels({ backend: "claude", model: "claude-sonnet-5", catalog });
  assert.deepEqual(rows.map((r) => r.label), ["Claude Opus 5.5", "Claude Opus 5", "Claude Sonnet 5"]);
  assert.equal(rows[0].detail, "claude-opus-5-5", "the id shows beside the name");
  const aliased = pickerModels({ backend: "claude", model: "opus", catalog });
  assert.equal(aliased[0].id, "opus", "the alias in use stays selectable as itself");
  assert.equal(aliased[0].detail, "opus → claude-opus-5-5");
  assert.equal(pickerModels({ backend: "claude", model: "fable", catalog: [] }).length, 4, "no catalog: the families");
});

test("newer is preferred: a session opens on the newest model, and a remembered family moves up", async (context) => {
  const list = [
    { id: "claude-haiku-6", displayName: "Claude Haiku 6" },
    { id: "claude-opus-5-5", displayName: "Claude Opus 5.5" },
    { id: "claude-fable-5-1", displayName: "Claude Fable 5.1" },
    { id: "claude-opus-5", displayName: "Claude Opus 5" },
    { id: "claude-sonnet-5", displayName: "Claude Sonnet 5" },
  ];
  assert.equal(newestModel(list), "claude-opus-5-5", "the newest that is not the small tier");
  assert.equal(newestModel([{ id: "claude-haiku-6" }]), "claude-haiku-6", "unless it is all there is");
  assert.equal(family("claude-opus-5-5"), "opus");
  assert.equal(family("gpt-6-astra"), "");
  assert.equal(preferNewer("claude-opus-5", list), "claude-opus-5-5", "opus moves up to the newest opus");
  assert.equal(preferNewer("claude-sonnet-5", list), "claude-sonnet-5", "sonnet stays where it is newest");
  assert.equal(preferNewer("opus", list), "opus", "an alias the CLI resolves is left to the CLI");
  assert.equal(preferNewer("gpt-6-astra", list), "gpt-6-astra", "another provider's id is untouched");
  const root = mkdtempSync(join(tmpdir(), "catalog-"));
  context.after(() => rmSync(root, { recursive: true, force: true }));
  assert.deepEqual(cachedCatalog("claude", root), CLAUDE_FALLBACK, "no cache yet: the table");
  await loadCatalog("claude", { root, loaders: { claude: async () => list }, now: () => 5 });
  assert.equal(cachedCatalog("claude", root)[0].id, "claude-haiku-6", "the cache, read at once");
});
