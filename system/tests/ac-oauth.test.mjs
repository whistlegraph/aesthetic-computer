import test from "node:test";
import assert from "node:assert/strict";
import { createHash } from "node:crypto";
import { createOAuth, allowedRedirect, authorizationServerMetadata, ISSUER } from "../backend/ac-oauth.mjs";
import { resolveNoPaintAccount } from "../backend/nopaint-account.mjs";

// Just enough of a Mongo collection for the turnstile.
function fakeDb() {
  const collections = new Map();
  const matches = (doc, query) => Object.entries(query).every(([key, value]) => doc[key] === value);
  return {
    collection(name) {
      if (!collections.has(name)) collections.set(name, []);
      const docs = collections.get(name);
      return {
        insertOne: async (doc) => { docs.push(structuredClone(doc)); },
        findOne: async (query) => structuredClone(docs.find((doc) => matches(doc, query)) ?? null),
        findOneAndDelete: async (query) => {
          const at = docs.findIndex((doc) => matches(doc, query));
          return at < 0 ? null : docs.splice(at, 1)[0];
        },
        deleteMany: async (query) => {
          for (let at = docs.length - 1; at >= 0; at--) if (matches(docs[at], query)) docs.splice(at, 1);
        },
        createIndex: async () => {},
      };
    },
  };
}

const verifier = "a".repeat(64);
const challenge = createHash("sha256").update(verifier).digest("base64url");
const painter = { sub: "auth0|painter", handle: "@painter" };

async function linked(clock = { now: 1_000_000 }) {
  const oauth = createOAuth({ db: fakeDb(), now: () => clock.now });
  const client = await oauth.register({ client_name: "Claude", redirect_uris: ["https://claude.ai/api/mcp/auth_callback"] });
  const query = { client_id: client.client_id, redirect_uri: "https://claude.ai/api/mcp/auth_callback",
    response_type: "code", code_challenge: challenge, code_challenge_method: "S256",
    resource: "https://notepaint.art/mcp", state: "xyz" };
  const redirect = new URL(await oauth.approve(query, painter));
  return { oauth, client, query, redirect, code: redirect.searchParams.get("code"), clock };
}

test("metadata names aesthetic.computer as issuer with PKCE S256 only", () => {
  const meta = authorizationServerMetadata();
  assert.equal(meta.issuer, ISSUER);
  assert.deepEqual(meta.code_challenge_methods_supported, ["S256"]);
  assert.equal(meta.registration_endpoint, "https://aesthetic.computer/oauth/register");
});

test("registration accepts https and localhost redirects only", async () => {
  assert.ok(allowedRedirect("https://chatgpt.com/connector_platform_oauth_redirect"));
  assert.ok(allowedRedirect("http://localhost:33418/callback"));
  assert.ok(!allowedRedirect("http://evil.example/callback"));
  assert.ok(!allowedRedirect("javascript:alert(1)"));
  assert.ok(!allowedRedirect("https://claude.ai/cb#frag"));
  const oauth = createOAuth({ db: fakeDb() });
  await assert.rejects(oauth.register({ redirect_uris: ["http://evil.example/cb"] }), { oauth: "invalid_redirect_uri" });
  const client = await oauth.register({ redirect_uris: ["https://claude.ai/api/mcp/auth_callback"] });
  assert.match(client.client_id, /^acc_/);
  assert.equal(client.token_endpoint_auth_method, "none");
});

test("authorize rejects unknown clients, unregistered redirects and missing PKCE", async () => {
  const { oauth, query } = await linked();
  await assert.rejects(oauth.checkRequest({ ...query, client_id: "acc_nope" }), { oauth: "invalid_client" });
  await assert.rejects(oauth.checkRequest({ ...query, redirect_uri: "https://evil.example/cb" }), { oauth: "invalid_request" });
  const noPkce = await oauth.checkRequest({ ...query, code_challenge: undefined }).catch((error) => error);
  assert.equal(noPkce.oauth, "invalid_request");
  assert.match(noPkce.redirect, /^https:\/\/claude\.ai\/api\/mcp\/auth_callback\?error=invalid_request/);
  const badTarget = await oauth.checkRequest({ ...query, resource: "https://elsewhere.example/mcp" }).catch((error) => error);
  assert.equal(badTarget.oauth, "invalid_target");
});

test("a code trades once for tokens that resolve to the painter and the product", async () => {
  const { oauth, client, redirect, code } = await linked();
  assert.equal(redirect.searchParams.get("state"), "xyz");
  assert.equal(redirect.searchParams.get("iss"), ISSUER);
  const form = { grant_type: "authorization_code", code, code_verifier: verifier, client_id: client.client_id,
    redirect_uri: "https://claude.ai/api/mcp/auth_callback" };
  const tokens = await oauth.token(form);
  assert.match(tokens.access_token, /^act_/);
  assert.equal(tokens.token_type, "Bearer");
  assert.deepEqual(await oauth.resolve(tokens.access_token),
    { sub: painter.sub, handle: painter.handle, resource: "https://notepaint.art/mcp", client_id: client.client_id });
  await assert.rejects(oauth.token(form), { oauth: "invalid_grant" }); // single use
  assert.equal(await oauth.resolve(tokens.refresh_token), null); // a refresh token is not an access token
});

test("a code needs the right verifier and client, and expires", async () => {
  let { oauth, client, code } = await linked();
  await assert.rejects(oauth.token({ grant_type: "authorization_code", code, code_verifier: "b".repeat(64), client_id: client.client_id }),
    { oauth: "invalid_grant" });
  ({ oauth, client, code } = await linked());
  await assert.rejects(oauth.token({ grant_type: "authorization_code", code, code_verifier: verifier, client_id: "acc_other" }),
    { oauth: "invalid_grant" });
  const clock = { now: 0 };
  ({ oauth, client, code } = await linked(clock));
  clock.now += 11 * 60 * 1000;
  await assert.rejects(oauth.token({ grant_type: "authorization_code", code, code_verifier: verifier, client_id: client.client_id }),
    { oauth: "invalid_grant" });
});

test("refresh rotates: the old refresh and access tokens stop working", async () => {
  const { oauth, client, code, clock } = await linked();
  const first = await oauth.token({ grant_type: "authorization_code", code, code_verifier: verifier, client_id: client.client_id });
  const second = await oauth.token({ grant_type: "refresh_token", refresh_token: first.refresh_token, client_id: client.client_id });
  assert.notEqual(second.access_token, first.access_token);
  assert.equal(await oauth.resolve(first.access_token), null);
  assert.ok(await oauth.resolve(second.access_token));
  await assert.rejects(oauth.token({ grant_type: "refresh_token", refresh_token: first.refresh_token, client_id: client.client_id }),
    { oauth: "invalid_grant" });
  clock.now += 61 * 60 * 1000;
  assert.equal(await oauth.resolve(second.access_token), null); // access tokens last an hour
});

test("deny sends access_denied back to the registered redirect", async () => {
  const { oauth, query } = await linked();
  const url = new URL(await oauth.deny(query));
  assert.equal(url.origin + url.pathname, "https://claude.ai/api/mcp/auth_callback");
  assert.equal(url.searchParams.get("error"), "access_denied");
  assert.equal(url.searchParams.get("state"), "xyz");
});

test("Note Paint accepts a linked token for its own resource only, with the live handle", async () => {
  const identity = async (sub) => ({ sub, handle: "painter" });
  const authorize = async () => { throw Error("Auth0 must not be asked about act_ tokens"); };
  const connector = { resolve: async (bearer) => bearer === "act_good"
    ? { sub: painter.sub, handle: painter.handle, resource: "https://notepaint.art/mcp" }
    : bearer === "act_elsewhere" ? { sub: painter.sub, resource: "https://other.example/mcp" } : null };
  const user = await resolveNoPaintAccount({ authorization: "Bearer act_good" }, { authorize, identity, connector });
  assert.equal(user.sub, painter.sub);
  assert.equal(user.handle, "@painter");
  for (const bearer of ["act_elsewhere", "act_expired"])
    await assert.rejects(resolveNoPaintAccount({ authorization: `Bearer ${bearer}` }, { authorize, identity, connector }), { status: 401 });
});
