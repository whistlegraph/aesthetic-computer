import test from "node:test";
import assert from "node:assert/strict";
import { createHash } from "node:crypto";
import {
  createNoPaintSignIn,
  CALLBACK,
  CLIENT_ID,
} from "../backend/nopaint-signin.mjs";

function fixture() {
  let clock = Date.now();
  const links = new Map(),
    calls = [];
  const pair = {
    _id: "ABC234",
    kind: "browser",
    createdAt: new Date(clock),
    claimed: false,
  };
  const user = {
    sub: "email|fixture",
    email: "painter@example.test",
    email_verified: true,
  };
  const handler = createNoPaintSignIn({
    now: () => clock,
    pairs: {
      findOne: async (filter) =>
        filter._id === pair._id && filter.kind === pair.kind
          ? { ...pair }
          : null,
    },
    links: {
      insertOne: async (row) => links.set(row._id, row),
      findOneAndDelete: async (filter) => {
        const row = links.get(filter._id);
        links.delete(filter._id);
        return row;
      },
    },
    authorize: async ({ authorization }) =>
      authorization === "Bearer fixture-token" ? user : null,
    fetch: async (url, options) => {
      calls.push({ url, body: JSON.parse(options.body) });
      return Response.json({ access_token: "fixture-token" });
    },
    pairHandler: async (event) => {
      calls.push(event);
      if (event.httpMethod === "GET")
        return {
          statusCode: 403,
          body: JSON.stringify({ message: "Invalid polling credential" }),
        };
      if (JSON.parse(event.body).action === "claim") pair.claimed = true;
      return {
        statusCode: 200,
        body: JSON.stringify({ code: pair._id, kind: "browser" }),
      };
    },
  });
  const post = (body, authorization) =>
    handler({
      httpMethod: "POST",
      headers: { authorization },
      body: JSON.stringify(body),
    });
  const callback = (state, code = "auth-code") =>
    handler({ httpMethod: "GET", queryStringParameters: { state, code } });
  return {
    pair,
    user,
    links,
    calls,
    post,
    callback,
    advance: (ms) => {
      clock += ms;
    },
  };
}

test("email link binds PKCE, requested email and browser pairing; callback is single-use", async () => {
  const f = fixture();
  const response = await f.post({
    action: "email",
    code: "ABC234",
    email: "painter@example.test",
  });
  assert.equal(response.statusCode, 200);
  const { client_id, authParams } = JSON.parse(response.body);
  assert.equal(client_id, CLIENT_ID);
  assert.equal(authParams.redirect_uri, CALLBACK);
  assert.equal(authParams.response_type, "code");
  const stored = [...f.links.values()][0];
  assert.equal(
    authParams.code_challenge,
    createHash("sha256").update(stored.verifier).digest("base64url"),
  );
  assert.ok(!response.body.includes(stored.verifier));
  assert.ok(!JSON.stringify(stored).includes("painter@example.test"));
  assert.equal(f.calls.length, 0, "API does not send email itself");
  const done = await f.callback(authParams.state);
  assert.equal(done.statusCode, 200);
  assert.equal(f.pair.claimed, true);
  assert.ok(!done.body.includes("fixture-token"));
  assert.equal(f.calls[0].body.code_verifier, stored.verifier);
  assert.equal(f.calls[0].body.redirect_uri, CALLBACK);
  assert.equal((await f.callback(authParams.state)).statusCode, 400);
  assert.equal(f.calls.length, 2, "one exchange and one claim only");
});

test("wrong email, unverified identity and invalid state cannot claim the browser", async () => {
  for (const changed of [
    { email: "someone-else@example.test" },
    { email_verified: false },
  ]) {
    const f = fixture();
    const response = await f.post({
      action: "email",
      code: "ABC234",
      email: "painter@example.test",
    });
    Object.assign(f.user, changed);
    assert.equal(
      (await f.callback(JSON.parse(response.body).authParams.state)).statusCode,
      400,
    );
    assert.equal(f.pair.claimed, false);
  }
  const f = fixture();
  assert.equal((await f.callback("bogus")).statusCode, 400);
  assert.equal(f.calls.length, 0);
});

test("expired, delivered and native pairings are refused before authentication", async () => {
  for (const change of [
    { kind: "native" },
    { deliveredAt: new Date() },
    { createdAt: new Date(0) },
  ]) {
    const f = fixture();
    Object.assign(f.pair, change);
    assert.equal(
      (
        await f.post(
          { action: "claim", code: "ABC234" },
          "Bearer fixture-token",
        )
      ).statusCode,
      410,
    );
    assert.equal(f.calls.length, 0);
  }
  const f = fixture();
  const response = await f.post({
    action: "email",
    code: "ABC234",
    email: "painter@example.test",
  });
  f.advance(600_001);
  assert.equal(
    (await f.callback(JSON.parse(response.body).authParams.state)).statusCode,
    400,
  );
  assert.equal(f.calls.length, 0);
});

test("public QR code cannot collect a session and paired sessions never enter cache", async () => {
  const f = fixture();
  assert.equal(
    (await f.post({ action: "poll", code: "ABC234" })).statusCode,
    403,
  );
  const response = await f.post({
    action: "poll",
    code: "ABC234",
    pollSecret: "a".repeat(43),
  });
  assert.equal(response.statusCode, 403);
  assert.equal(f.calls[0].queryStringParameters.secret, "a".repeat(43));
  assert.equal(response.headers["Cache-Control"], "private, no-store");
  assert.equal(
    (await f.post({ action: "claim", code: "ABC234" }, "Bearer wrong"))
      .statusCode,
    401,
  );
});

test("email attempts are bounded and cannot choose a callback or native kind", async () => {
  const f = fixture();
  const body = {
    action: "email",
    code: "ABC234",
    email: "painter@example.test",
    redirect_uri: "https://evil.example",
  };
  const first = await f.post(body);
  assert.equal(JSON.parse(first.body).authParams.redirect_uri, CALLBACK);
  assert.equal((await f.post(body)).statusCode, 200);
  assert.equal((await f.post(body)).statusCode, 429);
  await f.post({ action: "create", kind: "native" });
  assert.equal(JSON.parse(f.calls.at(-1).body).kind, "browser");
});
