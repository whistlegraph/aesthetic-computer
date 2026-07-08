// nela-signin, 26.07.07
// Signal OAuth for nela.aesthetic.computer — the donate page greets members
// by their Signal username. The identity provider is NCC's Signal-authed
// jump box (https://jump.nelacomputer.club, an OpenID Connect issuer run by
// max). The page can't hold the client secret, so it hands its authorization
// code here and we finish the dance:
//
//   POST /api/nela-signin   body: { code }
//        → { username, uid, sub }
//
// Flow: exchange the code at /oauth/token (confidential client — id + secret
// live in the lith env, vault lith/.env), then read the claims from
// /oauth/userinfo. No scopes; tokens carry sub, phone_number,
// preferred_username, and uid. We return only what the greeting needs —
// phone_number stays server-side.
//
// The redirect_uri is pinned to the registered value (exact-match rule), so
// sign-in only completes against the production page.

import { respond } from "../../backend/http.mjs";

const ISSUER = "https://jump.nelacomputer.club";
const REDIRECT_URI =
  process.env.NELA_OAUTH_REDIRECT_URI || "https://nela.aesthetic.computer";

export async function handler(event) {
  if (event.httpMethod === "OPTIONS") return respond(204, null);
  if (event.httpMethod !== "POST")
    return respond(405, { message: "Method Not Allowed." });

  const clientId = process.env.NELA_OAUTH_CLIENT_ID;
  const clientSecret = process.env.NELA_OAUTH_CLIENT_SECRET;
  if (!clientId || !clientSecret) {
    console.error("🚪 nela-signin: NELA_OAUTH_* missing from the environment");
    return respond(500, { message: "Sign-in is not configured." });
  }

  let code;
  try {
    code = JSON.parse(event.body || "{}").code;
  } catch {
    return respond(400, { message: "Bad JSON." });
  }
  if (!code || typeof code !== "string")
    return respond(400, { message: "Missing code." });

  // Code → token.
  const tokenRes = await fetch(`${ISSUER}/oauth/token`, {
    method: "POST",
    headers: { "Content-Type": "application/x-www-form-urlencoded" },
    body: new URLSearchParams({
      grant_type: "authorization_code",
      code,
      redirect_uri: REDIRECT_URI,
      client_id: clientId,
      client_secret: clientSecret,
    }),
  });
  if (!tokenRes.ok) {
    const detail = await tokenRes.text().catch(() => "");
    console.error("🚪 nela-signin: token exchange failed", tokenRes.status, detail);
    return respond(401, { message: "Sign-in failed." });
  }
  const { access_token } = await tokenRes.json();

  // Token → claims.
  const infoRes = await fetch(`${ISSUER}/oauth/userinfo`, {
    headers: { Authorization: `Bearer ${access_token}` },
  });
  if (!infoRes.ok) {
    console.error("🚪 nela-signin: userinfo failed", infoRes.status);
    return respond(502, { message: "Could not read profile." });
  }
  const claims = await infoRes.json();
  console.log("🚪 nela-signin:", claims.preferred_username, claims.sub);

  return respond(200, {
    username: claims.preferred_username,
    uid: claims.uid,
    sub: claims.sub,
  });
}
