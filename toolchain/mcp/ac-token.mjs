// ac-token.mjs — the shared AC session, for any MCP that acts as @jeffrey.
//
// `ac-login` mints ~/.ac-token; DateWizard watches it; cal-mcp and chat-mcp read
// it. The access token is an Auth0 one, so it expires — we refresh it through the
// refresh_token grant against the custom domain and rewrite the file in place.
//
// Cloudflare fronts the write paths and 403s (code 1010) a bare user agent, so
// anything reaching for the API sends a browser-shaped UA.
import { readFile, writeFile } from "node:fs/promises";
import { join } from "node:path";
import { homedir } from "node:os";

const TOKEN_FILE = join(homedir(), ".ac-token");
const AUTH0_DOMAIN = "hi.aesthetic.computer";
const AUTH0_CLIENT_ID = "LVdZaMbyXctkGfZDnpzDATB5nR0ZhmMt";
const USERINFO = "https://aesthetic.us.auth0.com/userinfo";

export const UA =
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 " +
  "(KHTML, like Gecko) Chrome/125.0 Safari/537.36 ac-mcp";

// Read ~/.ac-token, refreshing the access token when it has expired.
export async function loadTokens() {
  let tokens;
  try {
    tokens = JSON.parse(await readFile(TOKEN_FILE, "utf8"));
  } catch {
    throw new Error(
      "not signed in — no ~/.ac-token. Run `ac-login` (the shared AC session).",
    );
  }
  // Refresh a minute early so a call mid-flight never races the expiry.
  const stale = tokens.expires_at && Date.now() > tokens.expires_at - 60_000;
  if (stale) {
    if (!tokens.refresh_token) {
      throw new Error("AC token expired and no refresh_token — run `ac-login`.");
    }
    const res = await fetch(`https://${AUTH0_DOMAIN}/oauth/token`, {
      method: "POST",
      headers: { "content-type": "application/json" },
      body: JSON.stringify({
        grant_type: "refresh_token",
        client_id: AUTH0_CLIENT_ID,
        refresh_token: tokens.refresh_token,
      }),
    });
    if (!res.ok) {
      throw new Error(
        `token refresh failed (HTTP ${res.status}) — run \`ac-login\` to re-auth.`,
      );
    }
    const next = await res.json();
    tokens.access_token = next.access_token;
    if (next.refresh_token) tokens.refresh_token = next.refresh_token; // rotation
    if (next.id_token) tokens.id_token = next.id_token;
    tokens.expires_at = Date.now() + (next.expires_in || 3600) * 1000;
    await writeFile(TOKEN_FILE, JSON.stringify(tokens, null, 2)).catch(() => {});
  }
  return tokens;
}

// Who the token says we are. The chat server authorizes a message by sending the
// same token to /userinfo and checking the `sub` we claim matches — so we ask
// first rather than guess, and a mismatch fails loudly here instead of silently
// there.
export async function whoami() {
  const { access_token } = await loadTokens();
  const res = await fetch(USERINFO, {
    headers: { Authorization: `Bearer ${access_token}`, "User-Agent": UA },
    signal: AbortSignal.timeout(15_000),
  });
  if (!res.ok) {
    throw new Error(
      `/userinfo → HTTP ${res.status} (token rejected — run \`ac-login\`)`,
    );
  }
  const user = await res.json();
  if (!user.sub) throw new Error("/userinfo returned no sub");
  return { sub: user.sub, email: user.email, access_token };
}
