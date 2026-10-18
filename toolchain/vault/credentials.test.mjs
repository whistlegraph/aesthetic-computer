import test from "node:test";
import assert from "node:assert/strict";
import { mkdtempSync, mkdirSync, writeFileSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join } from "node:path";
import { resolveCredentials, credentialStatus, vaultRoot, vaultPath, vaultStatus, redactCredentials } from "./credentials.mjs";
import { loadRegistry } from "../fleet/registry.mjs";

function fixture(t) {
  const root = mkdtempSync(join(tmpdir(), "ac-vault-test-"));
  t.after(() => rmSync(root, { recursive: true, force: true }));
  const repo = join(root, "core");
  mkdirSync(repo);
  return { root, repo, options: { env: {}, repo, home: join(root, "home") },
    write(file, value) { const path = join(root, file); mkdirSync(dirname(path), { recursive: true }); writeFileSync(path, value); } };
}

test("nested and sibling vaults resolve independently of cwd; feed credentials are scoped", t => {
  const f = fixture(t);
  f.write("core/aesthetic-computer-vault/feed/.env", 'export CLOUDFLARE_EMAIL="owner@example.test"\nCLOUDFLARE_API_KEY=cf-test-secret # comment\nUNRELATED_SECRET=private\n');
  const found = resolveCredentials("cloudflare", f.options);
  assert.equal(found.state, "ready");
  assert.equal(found.source, "vault:feed/.env");
  assert.equal(found.values.CLOUDFLARE_API_KEY, "cf-test-secret");
  assert.equal(found.values.UNRELATED_SECRET, undefined);
  assert.doesNotMatch(JSON.stringify(found), /cf-test-secret|owner@example|private/);
  rmSync(join(f.repo, "aesthetic-computer-vault"), { recursive: true });
  f.write("aesthetic-computer-vault/.env", "PORKBUN_API_KEY=pb-test\nPORKBUN_SECRET_API_KEY=pb-secret\n");
  assert.equal(resolveCredentials("porkbun", f.options).state, "ready");
});

test("environment credentials win; partial overrides never mix accounts with a file", t => {
  const f = fixture(t);
  f.write("core/aesthetic-computer-vault/feed/.env", "CLOUDFLARE_EMAIL=file@example.test\nCLOUDFLARE_API_KEY=file-secret\n");
  const env = { CLOUDFLARE_API_TOKEN: "scoped-token" };
  assert.equal(resolveCredentials("cloudflare", { ...f.options, env }).auth, "api-token");
  assert.equal(resolveCredentials("cloudflare", { ...f.options, env: { CLOUDFLARE_EMAIL: "other@example.test" } }).state, "incomplete");
});

test("credential pairs are not assembled across files; rotation is picked up on the next call", t => {
  const f = fixture(t);
  f.write("core/aesthetic-computer-vault/cloudflare.env", "CLOUDFLARE_EMAIL=one@example.test\n");
  f.write("core/aesthetic-computer-vault/feed/.env", "CLOUDFLARE_API_KEY=another-secret\n");
  assert.equal(resolveCredentials("cloudflare", f.options).state, "incomplete");
  f.write("core/aesthetic-computer-vault/cloudflare.env", "CLOUDFLARE_API_TOKEN=first-token\n");
  assert.equal(resolveCredentials("cloudflare", f.options).values.CLOUDFLARE_API_TOKEN, "first-token");
  f.write("core/aesthetic-computer-vault/cloudflare.env", "CLOUDFLARE_API_TOKEN=rotated-token\n");
  assert.equal(resolveCredentials("cloudflare", f.options).values.CLOUDFLARE_API_TOKEN, "rotated-token");
});

test("explicit vault location is authoritative and missing/locked are distinct", t => {
  const f = fixture(t);
  f.write("core/aesthetic-computer-vault/feed/.env", "CLOUDFLARE_API_TOKEN=fallback\n");
  const options = { ...f.options, env: { AC_VAULT: join(f.root, "separate") } };
  assert.equal(vaultRoot(options), options.env.AC_VAULT);
  assert.equal(credentialStatus("cloudflare", options).state, "missing");
  f.write("separate/cloudflare.env.gpg", "not plaintext");
  assert.equal(credentialStatus("cloudflare", options).state, "locked");
});

test("lookup never scans client folders or arbitrary service names", t => {
  const f = fixture(t);
  f.write("core/aesthetic-computer-vault/fuser/.env", "CLOUDFLARE_API_TOKEN=client-secret\n");
  assert.equal(resolveCredentials("cloudflare", f.options).state, "missing");
  assert.throws(() => resolveCredentials("../../fuser", f.options), /Unknown/);
  assert.throws(() => resolveCredentials("constructor", f.options), /Unknown/);
});

test("redacts raw, URL-encoded, and JSON-escaped credentials", () => {
  const secret = 'test/secret"value';
  const text = [secret, encodeURIComponent(secret), JSON.stringify(secret)].join(" ");
  const safe = redactCredentials(text, { secret });
  assert.equal(safe, '[redacted] [redacted] "[redacted]"');
});

test("social account lookup is isolated, quoted, and shares the availability contract", t => {
  const f = fixture(t);
  f.write("core/aesthetic-computer-vault/oskiewar/instagram.env", 'OSKIEWAR_IG_USER_ID="123"\nOSKIEWAR_IG_TOKEN="ig-private"\nOSKIEWAR_SPACES_KEY=space-private\nUNRELATED=unrelated-private\n');
  f.write("core/aesthetic-computer-vault/promptdotac/x.env", ["API_KEY", "API_SECRET", "ACCESS_TOKEN", "ACCESS_SECRET"].map(key => `PROMPTDOTAC_X_${key}=x-private-${key}`).join("\n"));
  const ig = resolveCredentials("instagram:oskiewar", f.options);
  assert.equal(ig.values.OSKIEWAR_IG_TOKEN, "ig-private");
  assert.equal(ig.values.OSKIEWAR_SPACES_KEY, "space-private");
  assert.equal(ig.values.UNRELATED, undefined);
  assert.equal(credentialStatus("instagram:whistlegraph", f.options).state, "missing");
  assert.equal(credentialStatus("x:promptdotac", f.options).state, "ready");
  assert.equal(credentialStatus("x:whistlegraph", f.options).state, "missing");
  assert.doesNotMatch(JSON.stringify(vaultStatus(f.options)), /ig-private|space-private|x-private|unrelated-private/);
});

test("explicit service file overrides and AC_VAULT do not silently borrow another key", t => {
  const f = fixture(t);
  f.write("home/.config/ac-web-search/exa.env", "EXA_API_KEY=local-exa");
  f.write("core/aesthetic-computer-vault/mcp/exa.env", "EXA_API_KEY=vault-exa");
  assert.equal(resolveCredentials("exa", f.options).values.EXA_API_KEY, "local-exa");
  assert.equal(resolveCredentials("exa", { ...f.options, env: { AC_VAULT: join(f.repo, "aesthetic-computer-vault") } }).values.EXA_API_KEY, "vault-exa");
  assert.equal(resolveCredentials("exa", { ...f.options, env: { AC_WEB_SEARCH_ENV: join(f.root, "missing") } }).state, "missing");
  f.write("frame.env", "OPENAI_API_KEY=frame-only");
  assert.equal(resolveCredentials("openai", { ...f.options, file: join(f.root, "frame.env") }).values.OPENAI_API_KEY, "frame-only");
});

test("Slack session credentials require the cookie and export only registered settings", t => {
  const f = fixture(t);
  f.write("core/aesthetic-computer-vault/mcp/slack.env", "SLACK_MCP_XOXC_TOKEN=session-secret\nSLACK_MCP_ADD_MESSAGE_TOOL=false\nUNRELATED=private\n");
  assert.equal(credentialStatus("slack", f.options).state, "incomplete");
  f.write("core/aesthetic-computer-vault/mcp/slack.env", "SLACK_MCP_XOXC_TOKEN=session-secret\nSLACK_MCP_XOXD_TOKEN=cookie-secret\nSLACK_MCP_ADD_MESSAGE_TOOL=false\nUNRELATED=private\n");
  const resolved = resolveCredentials("slack", f.options);
  assert.equal(resolved.auth, "session-token");
  assert.equal(resolved.values.SLACK_MCP_ADD_MESSAGE_TOOL, "false");
  assert.equal(resolved.values.UNRELATED, undefined);
  assert.doesNotMatch(JSON.stringify(resolved), /session-secret|cookie-secret/);
});

test("Apple aliases work as complete pairs without combining environments and vault accounts", t => {
  const f = fixture(t);
  f.write("core/aesthetic-computer-vault/apple/app-specific-password.env", "APPLE_ID=apple@example.test\nAPP_SPECIFIC_PASSWORD=app-private\nAPPLE_TEAM_ID=team\n");
  assert.equal(resolveCredentials("apple-notary", f.options).values.APP_SPECIFIC_PASSWORD, "app-private");
  assert.equal(resolveCredentials("apple-notary", { ...f.options, env: { APPLE_ID: "other@example.test" } }).state, "incomplete");
  const env = { APPLE_ID: "env@example.test", APPLE_APP_PASSWORD: "fake" };
  assert.equal(resolveCredentials("apple-notary", { ...f.options, env }).values.APPLE_APP_PASSWORD, "fake");
});

test("Fleet and file-backed consumers honor the same root and authoritative overrides", t => {
  const f = fixture(t);
  f.write("aesthetic-computer-vault/machines.json", JSON.stringify({ machines: { fixture: { designation: "test" } } }));
  assert.equal(loadRegistry(f.options).machines.fixture.designation, "test");
  assert.equal(vaultPath("home/.ssh/id_rsa", f.options), join(f.root, "aesthetic-computer-vault/home/.ssh/id_rsa"));
  assert.throws(() => vaultPath("../escape", f.options), /within the vault/);
  assert.throws(() => loadRegistry({ ...f.options, env: { FLEET_MACHINES: join(f.root, "missing") } }), /No machine registry/);
});

test("Bluesky preserves the local devault copy without mixing it with vault credentials", t => {
  const f = fixture(t);
  f.write("core/at/.env", "BSKY_IDENTIFIER=local.example\nBSKY_APP_PASSWORD=local-private\nPDS_ADMIN_PASSWORD=unrelated-private\n");
  f.write("core/aesthetic-computer-vault/at/.env", "BSKY_IDENTIFIER=vault.example\nBSKY_APP_PASSWORD=vault-private\n");
  const local = resolveCredentials("bluesky", f.options);
  assert.equal(local.values.BSKY_APP_PASSWORD, "local-private");
  assert.equal(local.values.PDS_ADMIN_PASSWORD, undefined);
  const overridden = resolveCredentials("bluesky", { ...f.options, env: { AC_VAULT: join(f.repo, "aesthetic-computer-vault") } });
  assert.equal(overridden.values.BSKY_APP_PASSWORD, "vault-private");
  assert.equal(resolveCredentials("bluesky", { ...f.options, env: { BSKY_IDENTIFIER: "partial.example" } }).state, "incomplete");
});
