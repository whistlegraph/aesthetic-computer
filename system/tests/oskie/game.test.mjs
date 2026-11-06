import test from "node:test";
import assert from "node:assert/strict";
import { createHash } from "node:crypto";
import { memoryDb } from "./memory-db.mjs";
import {
  starterGame,
  validateGame,
  MAX_BYTES,
} from "../../public/oskie/format.mjs";
import { createGameStore } from "../../backend/oskie-store.mjs";
import {
  resolveGameAccount,
  OSKIE_RESOURCE,
} from "../../backend/oskie-account.mjs";
import { createOAuth } from "../../backend/ac-oauth.mjs";
import {
  createConnector,
  TOOLS,
} from "../../netlify/functions/ac-connector.mjs";
import { createGameAPI } from "../../netlify/functions/oskie.mjs";
const alice = { sub: "auth0|alice", handle: "@alice" },
  bob = { sub: "auth0|bob", handle: "@bob" };
const rejected = (promise, status) => assert.rejects(promise, { status });
const previewToken = (p) =>
  new URLSearchParams(new URL(p.preview).hash.slice(1)).get("preview");

test("format rejects traversal, prototype paths, oversized data, malformed files and incompatible runtimes", () => {
  for (const path of [
    "../secret.js",
    "/game.js",
    "x/../../game.js",
    "__proto__",
    "x/constructor",
  ]) {
    const b = starterGame();
    b.files[path] = { type: "text/plain", text: "x" };
    assert.throws(() => validateGame(b), /invalid path/);
  }
  const large = starterGame();
  large.files["big.txt"] = { type: "text/plain", text: "x".repeat(MAX_BYTES) };
  assert.throws(() => validateGame(large), /2 MB/);
  for (const change of [
    (b) => (b.manifest.mode = "overlay"),
    (b) => (b.manifest.runtime = "unknown"),
    (b) => (b.manifest.entry = "missing.mjs"),
    (b) => (b.files["bad.bin"] = { type: "image/png", base64: "%%%" }),
  ]) {
    const b = starterGame();
    change(b);
    assert.throws(() => validateGame(b));
  }
  assert.equal(validateGame(starterGame()).manifest.runtime, "ac-game-1");
});

test("owner isolation, concurrent edits, public revision pinning and restoration", async () => {
  const store = createGameStore(memoryDb()),
    p = await store.create(alice),
    token = previewToken(p);
  assert.deepEqual(await store.list(bob), []);
  for (const action of [
    () => store.read(bob, p.id),
    () => store.update(bob, p.id, starterGame(), p.revision),
    () => store.open(bob, p.id),
    () => store.publish(bob, p.id, p.revision),
    () => store.history(bob, p.id),
    () => store.restore(bob, p.id, p.revision, p.revision),
  ])
    await rejected(action(), 404);
  await rejected(store.readPublic(p.id), 404);
  const read = await store.read(alice, p.id);
  assert.equal(read.bundle.manifest.author, "@alice");
  await store.publish(alice, p.id, p.revision);
  const b = starterGame("next-game", "Changed");
  b.manifest.author = "@forged";
  const changes = await Promise.allSettled([
    store.update(alice, p.id, b, p.revision),
    store.update(alice, p.id, b, p.revision),
  ]);
  assert.equal(changes.filter((x) => x.status === "fulfilled").length, 1);
  assert.equal(changes.find((x) => x.status === "rejected").reason.status, 409);
  const draft = await store.readPreview(p.id, token);
  assert.equal(draft.bundle.manifest.author, "@alice");
  assert.equal(draft.bundle.manifest.title, "Changed");
  assert.equal(
    (await store.readPublic(p.id)).bundle.manifest.title,
    "Catch the square",
  );
  await rejected(store.publish(alice, p.id, p.revision), 409);
  const restored = await store.restore(alice, p.id, p.revision, draft.revision);
  assert.notEqual(restored.revision, p.revision);
  assert.equal((await store.history(alice, p.id)).length, 3);
});

test("preview grants expire, cannot cross games and accept feedback only for the current revision", async () => {
  let clock = new Date("2026-01-01");
  const store = createGameStore(memoryDb(), { now: () => clock });
  const p = await store.create(alice),
    q = await store.create(alice),
    token = previewToken(p);
  await rejected(store.readPreview(q.id, token), 401);
  await rejected(
    store.report(p.id, token, { revision: "stale", status: "running" }),
    409,
  );
  await store.report(p.id, token, {
    revision: p.revision,
    status: "error",
    error: "a".repeat(3000),
  });
  const feedback = await store.feedback(alice, p.id);
  assert.equal(feedback.untrusted, true);
  assert.equal(feedback.error.length, 2000);
  await store.update(alice, p.id, starterGame(), p.revision);
  assert.equal((await store.feedback(alice, p.id)).status, "waiting");
  clock = new Date(+clock + 25 * 60 * 60 * 1000);
  await rejected(store.readPreview(p.id, token), 401);
});

test("AC OAuth PKCE binds the connector resource and resolves the current handle", async () => {
  const oauth = createOAuth({ db: memoryDb() }),
    client = await oauth.register({
      redirect_uris: ["https://claude.ai/api/mcp/auth_callback"],
    });
  const verifier = "a".repeat(64),
    challenge = createHash("sha256").update(verifier).digest("base64url");
  const query = {
    client_id: client.client_id,
    response_type: "code",
    code_challenge: challenge,
    code_challenge_method: "S256",
    resource: OSKIE_RESOURCE,
  };
  const checked = await oauth.checkRequest(query);
  assert.equal(checked.product.name, "Aesthetic.Computer");
  const url = new URL(await oauth.approve(query, alice));
  const token = await oauth.token({
    grant_type: "authorization_code",
    code: url.searchParams.get("code"),
    client_id: client.client_id,
    code_verifier: verifier,
    resource: OSKIE_RESOURCE,
  });
  const deps = {
    oauth,
    locked: async () => false,
    identity: async (sub) => ({ sub, handle: "renamed" }),
  };
  assert.deepEqual(
    await resolveGameAccount(
      { authorization: "Bearer " + token.access_token },
      deps,
    ),
    { sub: alice.sub, handle: "@renamed" },
  );
  await rejected(
    resolveGameAccount(
      { authorization: "Bearer " + token.access_token },
      { ...deps, locked: async () => true },
    ),
    401,
  );
  await rejected(
    resolveGameAccount(
      { authorization: "Bearer act_wrong" },
      {
        ...deps,
        oauth: {
          resolve: async () => ({
            ...alice,
            resource: "https://notepaint.art/mcp",
          }),
        },
      },
    ),
    401,
  );
  await rejected(
    resolveGameAccount(
      {},
      {
        ...deps,
        authorize: async () => ({ sub: alice.sub, email_verified: false }),
      },
    ),
    403,
  );
});

test("MCP discovery, errors, full tool lifecycle and public API access", async () => {
  const store = createGameStore(memoryDb());
  const handler = createConnector({
    account: async (headers) => {
      if (headers.authorization !== "Bearer test")
        throw Object.assign(Error("Sign in"), { status: 401 });
      return alice;
    },
    store: async () => store,
  });
  const call = async (method, params = {}, id = 1) =>
    handler({
      path: "/mcp/ac",
      httpMethod: "POST",
      headers: { authorization: "Bearer test" },
      body: JSON.stringify({ jsonrpc: "2.0", id, method, params }),
    });
  const tool = async (name, args = {}) => {
    const r = JSON.parse(
      (await call("tools/call", { name, arguments: args })).body,
    ).result;
    assert.equal(r.isError, undefined, JSON.stringify(r));
    return JSON.parse(r.content[0].text);
  };
  const unauth = await handler({ httpMethod: "POST", headers: {} });
  assert.equal(unauth.statusCode, 401);
  assert.match(
    unauth.headers["WWW-Authenticate"],
    /oauth-protected-resource\/mcp\/ac/,
  );
  const badOrigin = await handler({
    httpMethod: "POST",
    headers: { origin: "https://evil.example" },
  });
  assert.equal(badOrigin.statusCode, 403);
  assert.equal(
    JSON.parse(
      (await call("initialize", { protocolVersion: "2025-06-18" })).body,
    ).result.serverInfo.name,
    "Aesthetic.Computer",
  );
  assert.equal(
    JSON.parse((await call("tools/list")).body).result.tools.length,
    TOOLS.length,
  );
  assert.ok(TOOLS.every((t) => t.title && t.annotations));
  assert.equal(
    (
      await handler({
        httpMethod: "POST",
        headers: { authorization: "Bearer test" },
        body: JSON.stringify({
          jsonrpc: "2.0",
          method: "notifications/initialized",
        }),
      })
    ).statusCode,
    202,
  );
  assert.equal(
    JSON.parse(
      (await call("tools/call", { name: "read_game", arguments: { id: 7 } }))
        .body,
    ).error.code,
    -32602,
  );
  assert.equal((await tool("whoami")).handle, "@alice");
  const p = await tool("create_game");
  assert.equal((await tool("list_games")).length, 1);
  const g = await tool("read_game", { id: p.id });
  const u = await tool("update_game", {
    id: p.id,
    bundle: g.bundle,
    expected_revision: g.revision,
  });
  assert.equal(
    (await tool("preview_feedback", { id: p.id })).status,
    "waiting",
  );
  assert.equal((await tool("game_history", { id: p.id })).length, 2);
  const restored = await tool("restore_game", {
    id: p.id,
    revision: p.revision,
    expected_revision: u.revision,
  });
  const preview = await tool("open_preview", { id: p.id });
  assert.ok(preview.preview);
  const api = createGameAPI({ store: async () => store });
  const event = { path: "/api/oskie/" + p.id, httpMethod: "GET", headers: {} };
  assert.equal((await api(event)).statusCode, 404);
  assert.equal(
    (
      await api({
        ...event,
        headers: { authorization: "Preview " + previewToken(preview) },
      })
    ).statusCode,
    200,
  );
  await tool("publish_game", {
    id: p.id,
    expected_revision: restored.revision,
  });
  const download = await api({ ...event, path: event.path + "/download" });
  assert.equal(download.statusCode, 200);
  assert.match(download.headers["Content-Disposition"], /\.oskie/);
  assert.equal(JSON.parse(download.body).manifest.author, "@alice");
  await tool("unpublish_game", { id: p.id });
  assert.equal((await api(event)).statusCode, 404);
  await tool("delete_game", { id: p.id, expected_revision: restored.revision });
  assert.deepEqual(await tool("list_games"), []);
  await rejected(store.readPreview(p.id, previewToken(preview)), 401);
});

// Consent must use the browser account, never a token granted to a product.
test("product tokens cannot approve another connector", async () => {
  const { verifiedUser } = await import("../../netlify/functions/ac-oauth.mjs");
  await assert.rejects(verifiedUser({authorization:"Bearer act_existing_product"}), {status:401});
  await assert.rejects(verifiedUser({authorization:"bearer act_existing_product"}), {status:401});
});
