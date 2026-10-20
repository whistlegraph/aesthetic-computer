// AC OAuth, 26.10.19
// One turnstile for the Aesthetic network's connectors. Claude, ChatGPT and
// Muse speak MCP authorization: they discover an authorization server from a
// product's /.well-known/oauth-protected-resource, register themselves, send
// the painter to /oauth/authorize, and trade a PKCE code for tokens.
//
// aesthetic.computer is that authorization server. Its link page signs the
// person in with the same AC sign-in every AC site uses (the aesthetic Auth0
// tenant, unchanged), and then AC issues its own tokens. Each token names the
// one product (resource) it was linked for, so a Note Paint link can paint and
// read the Braincell wallet and nothing else. sotce-net keeps its own tenant.
//
// Stored: clients, single-use codes and token hashes (never the tokens), each
// with an expiresAt that a TTL index sweeps.

import { createHash, randomBytes } from "node:crypto";

export const ISSUER = "https://aesthetic.computer";
export const TOKEN_PREFIX = "act_";

// The products a connector can be linked to.
export const RESOURCES = {
  "https://notepaint.art/mcp": { name: "Note Paint", home: "https://notepaint.art" },
};

const CODE_MS = 10 * 60 * 1000;
const ACCESS_MS = 60 * 60 * 1000;
const REFRESH_MS = 90 * 24 * 60 * 60 * 1000;

const fail = (status, error, description) =>
  Object.assign(Error(description), { status, oauth: error });
const hash = (value) => createHash("sha256").update(value).digest("hex");
const base64url = (buffer) => buffer.toString("base64url");

export function authorizationServerMetadata() {
  return {
    issuer: ISSUER,
    authorization_endpoint: `${ISSUER}/oauth/authorize`,
    token_endpoint: `${ISSUER}/oauth/token`,
    registration_endpoint: `${ISSUER}/oauth/register`,
    response_types_supported: ["code"],
    grant_types_supported: ["authorization_code", "refresh_token"],
    code_challenge_methods_supported: ["S256"],
    token_endpoint_auth_methods_supported: ["none"],
    scopes_supported: ["openid", "profile", "email", "offline_access"],
    service_documentation: "https://aesthetic.computer",
  };
}

// https anywhere, or plain http back to this machine (desktop and CLI clients).
export function allowedRedirect(uri) {
  if (typeof uri !== "string" || uri.length > 2000) return false;
  let url;
  try { url = new URL(uri); } catch { return false; }
  if (url.hash || url.username || url.password) return false;
  if (url.protocol === "https:") return true;
  return url.protocol === "http:" && ["localhost", "127.0.0.1", "[::1]"].includes(url.hostname);
}

export async function ensureOAuthIndexes(db) {
  for (const name of ["oauth-clients", "oauth-codes", "oauth-tokens"])
    await db.collection(name).createIndex({ expiresAt: 1 }, { expireAfterSeconds: 0, name: "expires" });
  await db.collection("oauth-tokens").createIndex({ family: 1 }, { name: "family" });
}

export function createOAuth({ db, now = Date.now, random = (bytes) => base64url(randomBytes(bytes)) }) {
  const clients = db.collection("oauth-clients");
  const codes = db.collection("oauth-codes");
  const tokens = db.collection("oauth-tokens");

  // RFC 7591 dynamic registration. Every client is public: PKCE, not a secret,
  // is what binds a code to the client that asked for it.
  async function register(body) {
    if (!body || typeof body !== "object" || Array.isArray(body))
      throw fail(400, "invalid_client_metadata", "Expected client metadata");
    const uris = body.redirect_uris;
    if (!Array.isArray(uris) || !uris.length || uris.length > 10 || !uris.every(allowedRedirect))
      throw fail(400, "invalid_redirect_uri", "redirect_uris must be https, or http to localhost");
    const name = typeof body.client_name === "string"
      ? body.client_name.replace(/\s+/g, " ").trim().slice(0, 100) : "";
    const client = {
      _id: `acc_${random(18)}`,
      name: name || new URL(uris[0]).hostname,
      redirect_uris: uris,
      created: new Date(now()),
    };
    await clients.insertOne(client);
    return {
      client_id: client._id,
      client_id_issued_at: Math.floor(now() / 1000),
      client_name: client.name,
      redirect_uris: client.redirect_uris,
      grant_types: ["authorization_code", "refresh_token"],
      response_types: ["code"],
      token_endpoint_auth_method: "none",
    };
  }

  // The authorization request, validated the same way when the link page is
  // served and again when the person approves it: the page is never trusted.
  async function checkRequest(query = {}) {
    const client = typeof query.client_id === "string" && await clients.findOne({ _id: query.client_id });
    if (!client) throw fail(400, "invalid_client", "Unknown client. Reconnect from the app.");
    const redirect = query.redirect_uri || (client.redirect_uris.length === 1 ? client.redirect_uris[0] : null);
    if (!client.redirect_uris.includes(redirect))
      throw fail(400, "invalid_request", "That return address isn't registered for this client.");
    // Past this point errors go back to the client, which can show them.
    const back = (error, description) =>
      Object.assign(fail(400, error, description), { redirect: errorRedirect(redirect, query.state, error, description) });
    if (query.response_type !== "code") throw back("unsupported_response_type", "Only code is supported");
    if (typeof query.code_challenge !== "string" || !/^[A-Za-z0-9_-]{43,128}$/.test(query.code_challenge) ||
        query.code_challenge_method !== "S256")
      throw back("invalid_request", "PKCE with S256 is required");
    // One product today, so a client that leaves `resource` out means it.
    const resource = query.resource || Object.keys(RESOURCES)[0];
    if (!RESOURCES[resource]) throw back("invalid_target", "Unknown resource");
    const scope = typeof query.scope === "string" ? query.scope.slice(0, 200) : "";
    const state = typeof query.state === "string" ? query.state.slice(0, 1000) : undefined;
    return {
      client: { id: client._id, name: client.name, redirect },
      product: RESOURCES[resource],
      request: { client_id: client._id, redirect_uri: redirect, code_challenge: query.code_challenge,
        code_challenge_method: "S256", resource, scope, ...(state !== undefined ? { state } : {}) },
    };
  }

  function errorRedirect(redirect, state, error, description) {
    const url = new URL(redirect);
    url.searchParams.set("error", error);
    if (description) url.searchParams.set("error_description", description);
    if (state !== undefined) url.searchParams.set("state", state);
    url.searchParams.set("iss", ISSUER);
    return url.toString();
  }

  // The person said yes as `user` ({ sub, handle }, already verified).
  async function approve(query, user) {
    const { request } = await checkRequest(query);
    const code = random(32);
    await codes.insertOne({
      _id: hash(code), client_id: request.client_id, redirect_uri: request.redirect_uri,
      challenge: request.code_challenge, resource: request.resource, scope: request.scope,
      sub: user.sub, handle: user.handle, expiresAt: new Date(now() + CODE_MS),
    });
    const url = new URL(request.redirect_uri);
    url.searchParams.set("code", code);
    if (request.state !== undefined) url.searchParams.set("state", request.state);
    url.searchParams.set("iss", ISSUER);
    return url.toString();
  }

  async function deny(query) {
    const { request } = await checkRequest(query);
    return errorRedirect(request.redirect_uri, request.state, "access_denied", "The person declined");
  }

  async function issue(grant, family = random(12)) {
    const access = `${TOKEN_PREFIX}${random(32)}`, refresh = `${TOKEN_PREFIX}r_${random(32)}`;
    const shared = { client_id: grant.client_id, sub: grant.sub, handle: grant.handle,
      resource: grant.resource, scope: grant.scope, family };
    await tokens.insertOne({ _id: hash(access), kind: "access", ...shared, expiresAt: new Date(now() + ACCESS_MS) });
    await tokens.insertOne({ _id: hash(refresh), kind: "refresh", ...shared, expiresAt: new Date(now() + REFRESH_MS) });
    return { access_token: access, token_type: "Bearer", expires_in: ACCESS_MS / 1000,
      refresh_token: refresh, scope: grant.scope || undefined, resource: grant.resource };
  }

  async function token(form = {}) {
    const clientId = form.client_id;
    if (form.grant_type === "authorization_code") {
      if (typeof form.code !== "string" || typeof form.code_verifier !== "string")
        throw fail(400, "invalid_request", "code and code_verifier are required");
      // Deleting on read makes the code single use, even under a race.
      const found = await codes.findOneAndDelete({ _id: hash(form.code) });
      const grant = found?.value !== undefined && found?.ok !== undefined ? found.value : found;
      if (!grant || grant.expiresAt <= new Date(now()))
        throw fail(400, "invalid_grant", "The code is expired or already used");
      if (grant.client_id !== clientId || (form.redirect_uri && form.redirect_uri !== grant.redirect_uri))
        throw fail(400, "invalid_grant", "The code was issued to a different client");
      if (base64url(createHash("sha256").update(form.code_verifier).digest()) !== grant.challenge)
        throw fail(400, "invalid_grant", "PKCE verification failed");
      if (form.resource && form.resource !== grant.resource)
        throw fail(400, "invalid_target", "The code was issued for a different resource");
      return issue(grant);
    }
    if (form.grant_type === "refresh_token") {
      if (typeof form.refresh_token !== "string") throw fail(400, "invalid_request", "refresh_token is required");
      const found = await tokens.findOneAndDelete({ _id: hash(form.refresh_token), kind: "refresh" });
      const grant = found?.value !== undefined && found?.ok !== undefined ? found.value : found;
      if (!grant || grant.expiresAt <= new Date(now()) || grant.client_id !== clientId)
        throw fail(400, "invalid_grant", "The refresh token is expired or already used");
      // Rotation: the old access token goes with the refresh token it came from.
      await tokens.deleteMany({ family: grant.family, kind: "access" });
      return issue(grant, grant.family);
    }
    throw fail(400, "unsupported_grant_type", "Use authorization_code or refresh_token");
  }

  // { sub, handle, resource, client_id } for a live access token, else null.
  async function resolve(bearer) {
    if (typeof bearer !== "string" || !bearer.startsWith(TOKEN_PREFIX) || bearer.length > 200) return null;
    const found = await tokens.findOne({ _id: hash(bearer), kind: "access" });
    if (!found || found.expiresAt <= new Date(now())) return null;
    return { sub: found.sub, handle: found.handle, resource: found.resource, client_id: found.client_id };
  }

  return { register, checkRequest, approve, deny, token, resolve };
}

// The shared, lazily connected instance lith's functions use.
let shared;
export async function acOAuth() {
  if (!shared) shared = (async () => {
    const { connect } = await import("./database.mjs");
    const { db } = await connect();
    await ensureOAuthIndexes(db);
    return createOAuth({ db });
  })().catch((error) => { shared = null; throw error; });
  return shared;
}
