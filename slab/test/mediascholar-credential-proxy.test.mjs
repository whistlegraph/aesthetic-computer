import assert from "node:assert/strict";
import { createServer } from "node:http";
import { mkdtemp, readFile, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import test from "node:test";

import { createCredentialProxy, proxyConfig } from "../bin/mediascholar-credential-proxy.mjs";

const listen = (server) => new Promise((resolve, reject) => {
  server.once("error", reject);
  server.listen(0, "127.0.0.1", () => resolve(server.address().port));
});
const close = (server) => new Promise((resolve, reject) => server.close((error) => error ? reject(error) : resolve()));

test("credential proxy substitutes secrets and exposes no prompt or credential data", async (t) => {
  const root = await mkdtemp(join(tmpdir(), "mediascholar-proxy-"));
  t.after(() => rm(root, { recursive: true, force: true }));
  let observed = null;
  const upstream = createServer(async (req, res) => {
    const chunks = [];
    for await (const chunk of req) chunks.push(chunk);
    observed = {
      authorization: req.headers.authorization,
      apiKey: req.headers["x-api-key"],
      body: Buffer.concat(chunks).toString("utf8"),
      path: req.url,
    };
    res.writeHead(200, { "content-type": "application/json" });
    res.end(JSON.stringify({ ok: true }));
  });
  const upstreamPort = await listen(upstream);
  t.after(() => close(upstream));

  const config = proxyConfig({
    MEDIASCHOLAR_PROXY_STATE: join(root, "usage.json"),
    MEDIASCHOLAR_OPENAI_UPSTREAM: `http://127.0.0.1:${upstreamPort}`,
    MEDIASCHOLAR_ANTHROPIC_UPSTREAM: `http://127.0.0.1:${upstreamPort}`,
    MEDIASCHOLAR_PROXY_MAX_DAILY_REQUESTS: "2",
  });
  const proxy = createCredentialProxy(config, {
    claude: { kind: "oauth", value: "sk-ant-oat01-real-test-credential" },
    openai: { kind: "api-key", value: "sk-real-test-credential" },
  });
  const proxyPort = await listen(proxy);
  t.after(() => close(proxy));

  const health = await fetch(`http://127.0.0.1:${proxyPort}/health`).then((response) => response.text());
  assert.doesNotMatch(health, /real-test-credential/);
  assert.match(health, /"openai":true/);

  const response = await fetch(`http://127.0.0.1:${proxyPort}/openai/v1/responses`, {
    method: "POST",
    headers: { authorization: "Bearer sk-dummy", "content-type": "application/json" },
    body: JSON.stringify({ input: "private prompt body" }),
  });
  assert.equal(response.status, 200);
  assert.equal(observed.authorization, "Bearer sk-real-test-credential");
  assert.equal(observed.path, "/v1/responses");
  assert.match(observed.body, /private prompt body/);
  const usage = JSON.parse(await readFile(config.statePath, "utf8"));
  assert.equal(usage.requests, 1);

  const claudeResponse = await fetch(`http://127.0.0.1:${proxyPort}/anthropic/v1/messages`, {
    method: "POST",
    headers: { "x-api-key": "sk-ant-dummy", "content-type": "application/json" },
    body: JSON.stringify({ messages: [{ role: "user", content: "fixture" }] }),
  });
  assert.equal(claudeResponse.status, 200);
  assert.equal(observed.authorization, "Bearer sk-ant-oat01-real-test-credential");
  assert.equal(observed.apiKey, undefined);
  assert.equal(observed.path, "/v1/messages");

  const forbidden = await fetch(`http://127.0.0.1:${proxyPort}/openai/v1/files`, { method: "POST" });
  assert.equal(forbidden.status, 404);
});
