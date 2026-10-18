import test from "node:test";
import assert from "node:assert/strict";
import { mkdtempSync, mkdirSync, writeFileSync, rmSync, existsSync } from "node:fs";
import { execFileSync } from "node:child_process";
import { tmpdir } from "node:os";
import { dirname, join, resolve } from "node:path";

const repo = resolve(import.meta.dirname, "../..");
function fixture(t) {
  const root = mkdtempSync(join(tmpdir(), "ac-mcp-vault-"));
  t.after(() => rmSync(root, { recursive: true, force: true }));
  const env = { ...process.env, AC_VAULT: root };
  for (const key of Object.keys(env)) if (/^(OPENAI_|FAL_|EXA_|SLACK_MCP_|OSKIEWAR_IG_|WHISTLEGRAPH_IG_|AESTHETIC_IG_|MENUBAND_IG_|PROMPTDOTAC_X_|WHISTLEGRAPH_X_)/.test(key)) delete env[key];
  return { root, env, write(file, text) { const path = join(root, file); mkdirSync(dirname(path), { recursive: true }); writeFileSync(path, text); } };
}
function rpc(script, name, env) {
  const input = JSON.stringify({ jsonrpc: "2.0", id: 1, method: "tools/call", params: { name, arguments: {} } }) + "\n";
  const raw = execFileSync(process.execPath, [join(repo, script)], { input, env, cwd: tmpdir(), encoding: "utf8", timeout: 15000, stdio: ["pipe", "pipe", "pipe"] });
  assert.doesNotMatch(raw, /fixture-private/);
  const result = JSON.parse(raw.trim());
  assert.equal(result.result.isError, undefined, result.result.content?.[0]?.text);
  return result.result.content.map(part => part.text).join("\n");
}

test("Instagram and X MCP availability matches their shared scoped credentials outside the checkout cwd", t => {
  const f = fixture(t);
  f.write("oskiewar/instagram.env", "OSKIEWAR_IG_USER_ID=123\nOSKIEWAR_IG_TOKEN=fixture-private-instagram\n");
  f.write("promptdotac/x.env", ["API_KEY", "API_SECRET", "ACCESS_TOKEN", "ACCESS_SECRET"].map(key => `PROMPTDOTAC_X_${key}=fixture-private-${key}`).join("\n"));
  const instagram = JSON.parse(rpc("slab/bin/instagram-mcp.mjs", "instagram_accounts", f.env));
  assert.equal(instagram.find(row => row.account === "@oskiewar").ready, true);
  assert.equal(instagram.find(row => row.account === "@whistlegraph").ready, false);
  const x = JSON.parse(rpc("slab/bin/x-mcp.mjs", "x_accounts", f.env));
  assert.equal(x.find(row => row.account === "@promptdotac").ready, true);
  assert.equal(x.find(row => row.account === "@whistlegraph").ready, false);
});

test("Illy and Exa use the selected vault without a provider request", t => {
  const f = fixture(t);
  f.write(".devcontainer/envs/devcontainer.env", "OPENAI_API_KEY=fixture-private-openai\nFAL_KEY=fixture-private-fal\n");
  f.write("mcp/exa.env", "EXA_API_KEY=fixture-private-exa\n");
  const illy = JSON.parse(rpc("plugins/illy/scripts/illy-mcp.mjs", "illy_backends", f.env));
  assert.equal(illy.openai.available, true);
  assert.equal(illy.fal.available, true);
  assert.equal(JSON.parse(rpc("toolchain/mcp/ac-web-search/server.mjs", "ac_web_search_status", f.env)).authentication, "api-key");
});

test("Slack launcher never evaluates dotenv as shell or exports unrelated vault values", t => {
  const f = fixture(t);
  const marker = join(f.root, "shell-executed");
  const literal = `$(touch ${marker})`;
  f.write("mcp/slack.env", `SLACK_MCP_XOXC_TOKEN='${literal}'\nSLACK_MCP_XOXD_TOKEN=fixture-private-cookie\nSLACK_MCP_ADD_MESSAGE_TOOL=false\nUNRELATED=fixture-private-unrelated\n`);
  f.write("check.mjs", `import assert from 'node:assert/strict';
assert.equal(process.env.SLACK_MCP_XOXC_TOKEN, ${JSON.stringify(literal)});
assert.equal(process.env.SLACK_MCP_XOXD_TOKEN, 'fixture-private-cookie');
assert.equal(process.env.SLACK_MCP_ADD_MESSAGE_TOOL, 'false');
assert.equal(process.env.UNRELATED, undefined);
console.log('scoped launch passed');`);
  const output = execFileSync(process.execPath, [join(repo, "toolchain/vault/launch.mjs"), "slack", process.execPath, join(f.root, "check.mjs")], {
    env: f.env, encoding: "utf8", timeout: 10000, stdio: ["ignore", "pipe", "pipe"] });
  assert.equal(output.trim(), "scoped launch passed");
  assert.equal(existsSync(marker), false);
});
