import { createHash, randomBytes } from "node:crypto";

export const CLIENT_ID = "LVdZaMbyXctkGfZDnpzDATB5nR0ZhmMt";
export const CALLBACK = "https://aesthetic.computer/api/nopaint-signin";
const digest = (value) => createHash("sha256").update(value).digest("hex");
const secret = () => randomBytes(32).toString("base64url");
const fail = (status, message) => Object.assign(Error(message), { status });
const headers = {
  "Content-Type": "application/json",
  "Cache-Control": "private, no-store",
  "Referrer-Policy": "no-referrer",
  "X-Content-Type-Options": "nosniff",
};
const reply = (statusCode, value) => ({
  statusCode,
  headers,
  body: JSON.stringify(value),
});
const page = (ok) => ({
  statusCode: ok ? 200 : 400,
  headers: {
    ...headers,
    "Content-Type": "text/html; charset=utf-8",
    "Content-Security-Policy":
      "default-src 'none'; style-src 'unsafe-inline'; frame-ancestors 'none'; base-uri 'none'",
  },
  body: `<!doctype html><html lang="en"><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>No Paint sign-in</title><style>body{font:20px system-ui;background:#eee9df;color:#202019;max-width:28em;padding:32px;margin:10vh auto}a{color:inherit}</style><h1>${ok ? "Signed in" : "This link could not sign you in"}</h1><p>${ok ? "Return to No Paint on the screen where you started." : "Return to No Paint and request a new sign-in link."}</p><a href="https://nopaint.art">No Paint</a></html>`,
});

// Reuse AC's browser pairing: only the initiating browser holds the polling
// credential, and a session is delivered once. Native pairings are excluded.
export function createNoPaintSignIn({
  pairs,
  links,
  pairHandler,
  authorize,
  fetch = globalThis.fetch,
  now = Date.now,
}) {
  const limits = new Map();
  function limit(key, maximum) {
    const at = now();
    for (const [id, value] of limits) if (value.until <= at) limits.delete(id);
    const value = limits.get(key) || { count: 0, until: at + 60_000 };
    if (value.count >= maximum || limits.size >= 2000)
      throw fail(429, "Please wait a minute before trying again.");
    value.count++;
    limits.set(key, value);
  }
  async function pair(code, { claimed = false } = {}) {
    if (!/^[A-Z0-9]{6}$/.test(code || ""))
      throw fail(400, "Invalid sign-in request.");
    const row = await pairs.findOne({ _id: code, kind: "browser" });
    if (
      !row ||
      +new Date(row.createdAt) + 600_000 <= now() ||
      row.deliveredAt ||
      (!claimed && row.claimed)
    )
      throw fail(410, "This sign-in expired. Open sign-in again.");
    return row;
  }
  async function claim(code, token) {
    await pair(code);
    const user = await authorize({ authorization: `Bearer ${token}` });
    if (!user?.sub || user.email_verified !== true)
      throw fail(401, "Sign in with your verified AC email.");
    return pairHandler({
      httpMethod: "POST",
      headers: { authorization: `Bearer ${token}` },
      body: JSON.stringify({ action: "claim", code }),
    });
  }
  return async (event) => {
    const query = event.queryStringParameters || {};
    if (event.httpMethod === "GET" && query.state) {
      try {
        if (
          !/^[A-Za-z0-9_-]{43}$/.test(query.state) ||
          typeof query.code !== "string" ||
          query.code.length > 2048
        )
          return page(false);
        // Atomic consumption prevents callback replay across processes.
        const link = await links.findOneAndDelete({ _id: digest(query.state) });
        if (!link || +new Date(link.expiresAt) <= now()) return page(false);
        await pair(link.code);
        const response = await fetch(
          "https://hi.aesthetic.computer/oauth/token",
          {
            method: "POST",
            redirect: "error",
            signal: AbortSignal.timeout(15000),
            headers: { "Content-Type": "application/json" },
            body: JSON.stringify({
              grant_type: "authorization_code",
              client_id: CLIENT_ID,
              code: query.code,
              code_verifier: link.verifier,
              redirect_uri: CALLBACK,
            }),
          },
        );
        if (!response.ok) return page(false);
        const tokens = await response.json();
        if (typeof tokens.access_token !== "string") return page(false);
        const user = await authorize({
          authorization: `Bearer ${tokens.access_token}`,
        });
        if (
          !user?.sub ||
          !user.email ||
          user.email_verified !== true ||
          digest(user.email.toLowerCase()) !== link.emailHash
        )
          return page(false);
        const result = await claim(link.code, tokens.access_token);
        return page(result.statusCode === 200);
      } catch {
        return page(false);
      }
    }
    if (event.httpMethod !== "POST") return reply(405, { error: "POST only" });
    try {
      if (typeof event.body !== "string" || event.body.length > 4096)
        throw fail(400, "Invalid sign-in request.");
      let body;
      try {
        body = JSON.parse(event.body);
      } catch {
        throw fail(400, "Invalid sign-in request.");
      }
      if (!body || typeof body !== "object" || Array.isArray(body))
        throw fail(400, "Invalid sign-in request.");
      const ip =
        event.headers?.["x-real-ip"] ||
        event.headers?.["x-forwarded-for"] ||
        "unknown";
      if (body.action === "create") {
        limit(`create:${digest(ip)}`, 10);
        const result = await pairHandler({
          httpMethod: "POST",
          headers: {},
          body: JSON.stringify({ action: "create", kind: "browser" }),
        });
        return { ...result, headers: { ...result.headers, ...headers } };
      }
      await pair(body.code, { claimed: body.action === "poll" });
      if (body.action === "poll") {
        if (
          typeof body.pollSecret !== "string" ||
          !/^[A-Za-z0-9_-]{43}$/.test(body.pollSecret)
        )
          throw fail(403, "Invalid sign-in credential.");
        const result = await pairHandler({
          httpMethod: "GET",
          headers: {},
          queryStringParameters: { code: body.code, secret: body.pollSecret },
        });
        return { ...result, headers: { ...result.headers, ...headers } };
      }
      if (body.action === "claim") {
        const token = (event.headers?.authorization || "").replace(
          /^Bearer /,
          "",
        );
        if (!token) throw fail(401, "Sign in with AC first.");
        const result = await claim(body.code, token);
        return { ...result, headers: { ...result.headers, ...headers } };
      }
      if (body.action !== "email") throw fail(400, "Unknown sign-in action.");
      if (
        typeof body.email !== "string" ||
        body.email.length > 254 ||
        !/^[^\s@]+@[^\s@]+\.[^\s@]+$/.test(body.email)
      )
        throw fail(400, "Enter your email address.");
      limit(`email:${digest(ip)}`, 5);
      limit(`pair:${body.code}`, 2);
      const state = secret(),
        verifier = secret();
      await links.insertOne({
        _id: digest(state),
        code: body.code,
        verifier,
        emailHash: digest(body.email.toLowerCase()),
        expiresAt: new Date(now() + 600_000),
      });
      // The browser sends the email request directly to Auth0, preserving its
      // per-client abuse protection. This endpoint is not a mail relay.
      return reply(200, {
        client_id: CLIENT_ID,
        authParams: {
          response_type: "code",
          response_mode: "query",
          scope: "openid profile email",
          redirect_uri: CALLBACK,
          state,
          code_challenge_method: "S256",
          code_challenge: createHash("sha256")
            .update(verifier)
            .digest("base64url"),
        },
      });
    } catch (error) {
      return reply(error.status || 503, {
        error: error.status
          ? error.message
          : "Sign-in is temporarily unavailable.",
      });
    }
  };
}
