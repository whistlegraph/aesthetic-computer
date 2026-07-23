import assert from "node:assert/strict";
import { mkdtemp, readFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join, resolve } from "node:path";
import test from "node:test";
import { createACIdentityVerifier, IdentityError } from "../src/identity.mjs";
import { createTerrariumServer } from "../src/server.mjs";

const jsonResponse = (value, status = 200) => new Response(JSON.stringify(value), {
  status,
  headers: { "content-type": "application/json" },
});

test("AC verifier resolves only the server-side handle from Auth0 identity", async () => {
  const calls = [];
  const verifier = createACIdentityVerifier({
    fetchImpl: async (url, options = {}) => {
      calls.push({ url: String(url), authorization: options.headers?.Authorization || null });
      if (String(url).includes("/userinfo")) {
        return jsonResponse({ sub: "auth0|alex private", email: "private@example.test", name: "Private Name" });
      }
      return jsonResponse({ handle: "Alex" });
    },
  });
  assert.equal(await verifier.verifyAuthorization("Bearer real-ac-token"), "@alex");
  assert.deepEqual(calls, [
    { url: "https://aesthetic.us.auth0.com/userinfo", authorization: "Bearer real-ac-token" },
    { url: "https://aesthetic.computer/handle/auth0%7Calex%20private", authorization: null },
  ]);
});

test("AC verifier rejects missing, malformed, expired, and upstream-rejected tokens", async () => {
  let fetches = 0;
  const verifier = createACIdentityVerifier({
    now: () => 2_000_000,
    fetchImpl: async () => {
      fetches += 1;
      return jsonResponse({}, 401);
    },
  });
  await assert.rejects(verifier.verifyAuthorization(), (error) => error instanceof IdentityError && error.code === "missing_token");
  await assert.rejects(verifier.verifyAuthorization("Token nope"), (error) => error.code === "invalid_token");
  const expired = [
    Buffer.from("{}").toString("base64url"),
    Buffer.from(JSON.stringify({ exp: 1 })).toString("base64url"),
    "signature",
  ].join(".");
  await assert.rejects(verifier.verifyAuthorization(`Bearer ${expired}`), (error) => error.code === "expired_token");
  await assert.rejects(verifier.verifyAuthorization("Bearer upstream-expired"), (error) => error.code === "invalid_or_expired_token");
  assert.equal(fetches, 1);
});

test("verified identity authors prods and spoofed identity fields never persist", async () => {
  const root = await mkdtemp(join(tmpdir(), "terrarium-ac-identity-"));
  const secret = "auth0-secret-token-never-persist";
  const privateSub = "auth0|private-sub-never-persist";
  const privateEmail = "private-email@example.test";
  const audit = [];
  const identityVerifier = createACIdentityVerifier({
    fetchImpl: async (url, options = {}) => {
      if (String(url).includes("/userinfo")) {
        if (options.headers?.Authorization !== `Bearer ${secret}`) return jsonResponse({}, 401);
        return jsonResponse({ sub: privateSub, email: privateEmail });
      }
      return jsonResponse({ handle: "alex" });
    },
  });
  const app = await createTerrariumServer({ root, identityVerifier, tickMs: 0, audit: (entry) => audit.push(entry) });
  const base = `http://${app.address.address}:${app.address.port}`;
  assert.equal((await fetch(`${base}/api/state`)).status, 401);
  assert.equal((await fetch(`${base}/api/state`, { headers: { Authorization: "Bearer rejected" } })).status, 401);

  const preflight = await fetch(`${base}/api/prod`, {
    method: "OPTIONS",
    headers: { Origin: "http://localhost:8888" },
  });
  assert.equal(preflight.status, 204);
  assert.equal(preflight.headers.get("access-control-allow-origin"), "http://localhost:8888");
  assert.equal((await fetch(`${base}/api/state`, { headers: { Origin: "https://hostile.example" } })).status, 403);

  const response = await fetch(`${base}/api/prod`, {
    method: "POST",
    headers: { Authorization: `Bearer ${secret}`, "Content-Type": "application/json" },
    body: JSON.stringify({
      handle: "@mallory",
      sub: "spoofed-private-sub",
      email: "spoofed@example.test",
      token: "payload-token-never-persist",
      target: "voice",
      modality: "text",
      stimulus: "hello mediorgan",
    }),
  });
  assert.equal(response.status, 202);
  await app.stop();

  const journal = await readFile(app.repository.segmentPath, "utf8");
  assert.match(journal, /"handle":"@alex"/);
  for (const forbidden of [secret, privateSub, privateEmail, "@mallory", "spoofed-private-sub", "spoofed@example.test", "payload-token-never-persist"]) {
    assert.doesNotMatch(journal, new RegExp(forbidden.replace(/[.*+?^${}()|[\]\\]/g, "\\$&")));
    assert.doesNotMatch(JSON.stringify(audit), new RegExp(forbidden.replace(/[.*+?^${}()|[\]\\]/g, "\\$&")));
  }
  assert.ok(audit.some(({ outcome }) => outcome === "accepted"));
  assert.ok(audit.some(({ outcome }) => outcome === "rejected"));
});

test("verified prods obey a deterministic per-handle rate bound", async () => {
  const root = await mkdtemp(join(tmpdir(), "terrarium-ac-rate-"));
  const identityVerifier = { verifyAuthorization: async () => "@alex" };
  const app = await createTerrariumServer({
    root,
    identityVerifier,
    maxProdsPerSecond: 2,
    now: () => 1_000_000,
    tickMs: 0,
  });
  const url = `http://${app.address.address}:${app.address.port}/api/prod`;
  const request = () => fetch(url, {
    method: "POST",
    headers: { Authorization: "Bearer test", "Content-Type": "application/json" },
    body: JSON.stringify({ target: "sensory", modality: "gesture", stimulus: "tap" }),
  });
  assert.equal((await request()).status, 202);
  assert.equal((await request()).status, 202);
  assert.equal((await request()).status, 429);
  await app.stop();
  assert.equal(app.repository.records.filter(({ kind }) => kind === "organ-prod").length, 2);
});

test("terrarium-dev is a hidden authorize-only loopback client seam", async () => {
  const source = await readFile(resolve("../../system/public/aesthetic.computer/disks/terrarium-dev.mjs"), "utf8");
  const serverSource = await readFile(resolve("src/server.mjs"), "utf8");
  assert.match(source, /await authorize\(\)/);
  assert.match(source, /Authorization: `Bearer \$\{token\}`/);
  assert.match(source, /http:\/\/127\.0\.0\.1/);
  assert.doesNotMatch(source, /function meta\s*\(/);
  assert.doesNotMatch(source, /JSON\.stringify\(\{[^}]*handle/s);
  assert.match(serverSource, /TERRARIUM_AC_AUTH === "1"/);
  assert.match(serverSource, /createACIdentityVerifier\(\)/);
});
