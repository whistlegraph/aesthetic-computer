// AC OAuth endpoints, 26.10.19 — see backend/ac-oauth.mjs for the turnstile.
//
//   GET  /.well-known/oauth-authorization-server   metadata (RFC 8414)
//   POST /oauth/register                           dynamic registration (RFC 7591)
//   GET  /oauth/authorize                          the link page
//   POST /oauth/link                               the link page's whoami / approve / deny
//   POST /oauth/token                              code and refresh grants

const AUTH0_CLIENT_ID = "LVdZaMbyXctkGfZDnpzDATB5nR0ZhmMt"; // the aesthetic SPA

const cors = {
  "Access-Control-Allow-Origin": "*",
  "Access-Control-Allow-Methods": "GET, POST, OPTIONS",
  "Access-Control-Allow-Headers": "Authorization, Content-Type",
};
const json = (statusCode, value, extra = {}) => ({
  statusCode,
  headers: { "Content-Type": "application/json", "Cache-Control": "no-store", ...cors, ...extra },
  body: JSON.stringify(value),
});
const oauthError = (error) =>
  json(error.status || 500, error.oauth
    ? { error: error.oauth, error_description: error.message }
    : { error: "server_error", error_description: "Linking is unavailable right now" });

function readBody(event) {
  const body = event.body || "";
  if (body.length > 20000) return null;
  const type = (event.headers?.["content-type"] || "").toLowerCase();
  if (type.includes("application/x-www-form-urlencoded"))
    return Object.fromEntries(new URLSearchParams(body));
  try { return body ? JSON.parse(body) : {}; } catch { return null; }
}

// A public client may still send HTTP Basic with an empty or ignored secret.
function basicClientId(headers = {}) {
  const value = headers.authorization || "";
  if (!value.startsWith("Basic ")) return undefined;
  try { return decodeURIComponent(Buffer.from(value.slice(6), "base64").toString().split(":")[0]); }
  catch { return undefined; }
}

async function verifiedUser(headers) {
  const [{ authorize }, { fighterIdentity }, { resolveNoPaintAccount }] = await Promise.all([
    import("../../backend/authorization.mjs"),
    import("../../backend/oskiewar-identity.mjs"),
    import("../../backend/nopaint-account.mjs"),
  ]);
  return resolveNoPaintAccount(headers, { authorize, identity: fighterIdentity });
}

const escape = (value) => String(value).replace(/[&<>"']/g, (c) =>
  ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;", "'": "&#39;" })[c]);

function page(title, body, status = 200) {
  return {
    statusCode: status,
    headers: { "Content-Type": "text/html; charset=utf-8", "Cache-Control": "no-store",
      "X-Frame-Options": "DENY", "Content-Security-Policy": "frame-ancestors 'none'" },
    body: `<!doctype html><html lang="en"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width,initial-scale=1"><title>${escape(title)}</title>
<style>
:root{color-scheme:light dark;--bg:#f4f1ea;--fg:#1d1b18;--dim:#6d675d;--line:#d6d0c4;--accent:#6b3fd1}
@media (prefers-color-scheme:dark){:root{--bg:#141311;--fg:#ece7dc;--dim:#9b9486;--line:#36332e;--accent:#a98bff}}
body{margin:0;background:var(--bg);color:var(--fg);font:16px/1.45 ui-monospace,Menlo,monospace;display:grid;min-height:100vh;place-items:center}
main{width:min(420px,100% - 32px);padding:24px 0}
h1{font-size:20px;margin:0 0 12px}p{margin:0 0 12px}.dim{color:var(--dim);font-size:14px}
input,button{font:inherit;width:100%;box-sizing:border-box;padding:10px 12px;margin:0 0 10px;border-radius:6px;border:1px solid var(--line);background:transparent;color:var(--fg)}
button{cursor:pointer}button.go{background:var(--accent);border-color:var(--accent);color:#fff}
.err{color:#c0392b}[hidden]{display:none}
</style></head><body><main>${body}</main></body></html>`,
  };
}

function linkPage({ client, product, request }) {
  const host = new URL(client.redirect).host;
  return page(`Link ${product.name}`, `
<h1>Link ${escape(client.name)} to your AC account</h1>
<p><b>${escape(client.name)}</b> wants to use <b>${escape(product.name)}</b> as you: paint, and spend your Braincells when it does.</p>
<p class="dim">It will return to ${escape(host)}. It can't reach the rest of your AC account.</p>
<section id="who" hidden><p id="as"></p><button class="go" id="approve">Link</button><button id="other">Use another account</button></section>
<section id="signin" hidden>
  <p class="dim">Sign in with AC</p>
  <input id="email" type="email" autocomplete="email" placeholder="email">
  <button class="go" id="send">Send a code</button>
  <div id="codebox" hidden><input id="code" inputmode="numeric" autocomplete="one-time-code" placeholder="six digit code"><button class="go" id="verify">Sign in</button></div>
</section>
<button id="deny">Cancel</button>
<p id="status" class="dim"></p>
<script type="application/json" id="request">${JSON.stringify(request).replace(/</g, "\\u003c")}</script>
<script type="module">
import { otpSignIn } from "/aesthetic.computer/lib/auth0-otp.mjs";
const $ = (id) => document.getElementById(id);
const request = JSON.parse($("request").textContent);
const otp = otpSignIn({ clientId: "${AUTH0_CLIENT_ID}" });
const say = (text, error) => { $("status").textContent = text || ""; $("status").className = error ? "err" : "dim"; };
let token = null, ignoreSpa = false;
async function spaToken() {
  let cached = false;
  try { cached = Object.keys(localStorage).some((key) => key.startsWith("@@auth0spajs@@")); } catch {}
  if (!cached || ignoreSpa) return null;
  await new Promise((resolve, reject) => document.head.append(Object.assign(document.createElement("script"),
    { src: "/aesthetic.computer/dep/auth0-spa-js.production.js", onload: resolve, onerror: reject })));
  const spa = await window.auth0.createAuth0Client({ domain: "https://hi.aesthetic.computer", clientId: "${AUTH0_CLIENT_ID}",
    cacheLocation: "localstorage", useRefreshTokens: true, authorizationParams: { redirect_uri: location.origin } });
  return (await spa.isAuthenticated()) ? spa.getTokenSilently() : null;
}
async function link(action) {
  const response = await fetch("/oauth/link", { method: "POST", cache: "no-store",
    headers: { "Content-Type": "application/json", ...(token ? { Authorization: "Bearer " + token } : {}) },
    body: JSON.stringify({ action, request }) });
  const result = await response.json();
  if (!response.ok) throw Error(result.error_description || result.error || "Linking failed");
  return result;
}
async function show() {
  say("");
  try { token = (await otp.token()) || (await spaToken().catch(() => null)); } catch { token = null; }
  if (token) {
    try {
      const { handle } = await link("whoami");
      $("as").textContent = "Signed in as " + handle;
      $("who").hidden = false; $("signin").hidden = true; return;
    } catch (error) { say(error.message, true); }
  }
  $("who").hidden = true; $("signin").hidden = false;
}
$("send").onclick = async () => {
  try { await otp.sendCode($("email").value); $("codebox").hidden = false; say("Check your email for a code."); }
  catch (error) { say(error.hint || error.message, true); }
};
$("verify").onclick = async () => {
  try { await otp.verify($("email").value, $("code").value); await show(); }
  catch (error) { say(error.hint || error.message, true); }
};
$("other").onclick = () => { otp.forget(); ignoreSpa = true; token = null; show(); };
$("approve").onclick = async () => {
  try { say("Linking…"); location.assign((await link("approve")).redirect); }
  catch (error) { say(error.message, true); }
};
$("deny").onclick = async () => {
  try { location.assign((await link("deny")).redirect); } catch (error) { say(error.message, true); }
};
show();
</script>`);
}

export async function handler(event) {
  if (event.httpMethod === "OPTIONS") return { statusCode: 204, headers: cors, body: "" };
  const path = event.path || "";
  const { acOAuth, authorizationServerMetadata } = await import("../../backend/ac-oauth.mjs");

  if (path.startsWith("/.well-known/oauth-authorization-server"))
    return json(200, authorizationServerMetadata(), { "Cache-Control": "public, max-age=3600" });

  try {
    const oauth = await acOAuth();

    if (path === "/oauth/authorize" && event.httpMethod === "GET") {
      try {
        return linkPage(await oauth.checkRequest(event.queryStringParameters || {}));
      } catch (error) {
        if (error.redirect) return { statusCode: 302, headers: { Location: error.redirect }, body: "" };
        if (!error.oauth) throw error;
        return page("Can't link", `<h1>Can't link</h1><p>${escape(error.message)}</p>`, 400);
      }
    }

    if (event.httpMethod !== "POST") return json(405, { error: "invalid_request", error_description: "POST only" });
    const body = readBody(event);
    if (!body) return json(400, { error: "invalid_request", error_description: "Unreadable body" });

    if (path === "/oauth/register") return json(201, await oauth.register(body));

    if (path === "/oauth/token")
      return json(200, await oauth.token({ ...body, client_id: body.client_id || basicClientId(event.headers) }));

    if (path === "/oauth/link") {
      if (body.action === "deny") return json(200, { redirect: await oauth.deny(body.request || {}) });
      let user;
      try { user = await verifiedUser(event.headers); }
      catch (error) { return json(error.status || 401, { error: "login_required", error_description: error.message }); }
      if (body.action === "whoami") return json(200, { handle: user.handle });
      if (body.action === "approve")
        return json(200, { redirect: await oauth.approve(body.request || {}, { sub: user.sub, handle: user.handle }) });
      return json(400, { error: "invalid_request", error_description: "Unknown action" });
    }

    return json(404, { error: "invalid_request", error_description: "Not found" });
  } catch (error) {
    if (!error.oauth) console.error("AC OAuth failed:", error);
    return oauthError(error);
  }
}
