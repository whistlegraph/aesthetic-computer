// Auth0 OTP, 26.09.11
// Signing in without leaving the page. Auth0 mails a six digit code, we trade
// the code for tokens, and the visitor never sees a hosted login page.
//
// Why this exists at all: the Universal Login page at hi.aesthetic.computer has
// Auth0's Attack Protection turned on, and its bot-detection provider is
// Cloudflare Turnstile. On some networks and on the Xbox that widget loops
// forever and there is no way past it — the redirect itself is the trap. The
// passwordless OTP grant never touches that page, so a shell that carries its
// own email-and-code form can sign a person in where they already are.
//
// Two calls, both public (verified 26.09.11 against the aesthetic tenant):
//
//   POST /passwordless/start  { client_id, connection, email, send: "code" }
//   POST /oauth/token         { grant_type: …/passwordless/otp, client_id,
//                               realm, username, otp, scope }
//
// Neither needs a client secret — the client is public (an authorization_code
// exchange with no secret is answered `invalid_grant`, not "Client
// authentication is required"), and both endpoints answer cross-origin for any
// host in the client's Allowed Web Origins. So this runs in the browser and
// there is no backend function, no new secret and no extra hop to keep alive.
//
// It lives in lib/ rather than in a shell because the second reader is already
// known: aesthetic.computer wants the same in-page option, and the dance is the
// half that has nothing to do with either surface's furniture. Nothing in here
// touches the DOM.

// Auth0's error vocabulary is wide and mostly not worth repeating to a person.
// These are the distinctions that change what someone should do next; anything
// unrecognised comes back as `unknown` with Auth0's own sentence attached, which
// is more honest than a shrug.
//
// `grant-off`, `no-connection` and `captcha` all mean the same thing to a
// caller: this tenant will not do it in-page right now, offer the redirect.
const messages = {
  "bad-email": "that email address doesn't look right",
  "no-connection": "email codes aren't switched on for this app yet",
  "grant-off": "email codes aren't switched on for this app yet",
  captcha: "sign-in needs the web page for this one",
  "wrong-code": "that code doesn't match",
  expired: "that code expired — ask for a new one",
  "rate-limited": "too many tries, wait a minute",
  offline: "couldn't reach the sign-in service",
  unknown: "sign-in failed",
};

// A caller that sees one of these should stop offering the form and offer the
// redirect instead: the tenant, not the person, is what is wrong.
export const tenantFaults = ["no-connection", "grant-off", "captcha"];

function fault(code, detail) {
  const error = new Error(detail || messages[code] || messages.unknown);
  error.code = code;
  error.hint = messages[code] || messages.unknown;
  error.tenant = tenantFaults.includes(code);
  return error;
}

// Auth0 answers /passwordless/start and /oauth/token with different spellings
// for the same trouble, so both get read through one table.
function readFault(status, body) {
  const name = body?.error || "";
  const detail = body?.error_description || "";
  if (status === 429 || name === "too_many_requests") return fault("rate-limited");
  if (name === "requires_verification") return fault("captcha");
  if (name === "bad.email" || (name === "bad.request" && /email/i.test(detail)))
    return fault("bad-email");
  if (name === "bad.connection") return fault("no-connection");
  if (name === "unauthorized_client") {
    // The same name covers "you never enabled this grant" and "this origin is
    // not allowed", and only the sentence tells them apart.
    if (/grant type/i.test(detail)) return fault("grant-off");
    return fault("grant-off", detail);
  }
  if (name === "invalid_grant") {
    // Auth0 folds a wrong code, an expired code and a spent allowance of
    // attempts into one error name. A person needs a new code for two of the
    // three, so the sentence is what splits them.
    if (/maximum number of attempts|expired/i.test(detail)) return fault("expired");
    return fault("wrong-code");
  }
  return fault("unknown", detail || undefined);
}

// The id_token's payload, unverified. We do not check the signature because the
// token came back on this very request from Auth0 over TLS — there is no third
// party in the middle to have forged it — and every server that later acts on
// the access token re-establishes who it belongs to through /userinfo anyway.
// A client-side signature check would only be theatre.
function claims(idToken) {
  const part = String(idToken || "").split(".")[1];
  if (!part) return {};
  // base64url, and JWTs travel unpadded — atob wants both fixed.
  const plain = part.replace(/-/g, "+").replace(/_/g, "/")
    .padEnd(Math.ceil(part.length / 4) * 4, "=");
  const bytes = Uint8Array.from(atob(plain), (c) => c.charCodeAt(0));
  return JSON.parse(new TextDecoder().decode(bytes));
}

export function otpSignIn({
  domain = "hi.aesthetic.computer",
  clientId,
  // The connection's name in the Auth0 dashboard, and the realm the OTP grant
  // resolves it by. They are the same string for passwordless email.
  connection = "email",
  // No `audience`, deliberately. The access token then carries Auth0's own
  // /userinfo audience, which is exactly the token shape the redirect login
  // already produces — so `authorize()` on the AC backend accepts it with no
  // change. Asking for a custom audience here would quietly break that.
  scope = "openid profile email offline_access",
  store = globalThis.localStorage,
  storeKey = "ac-otp-session",
} = {}) {
  let live = null; // The session in memory; `store` is only the copy that survives a reload.

  async function ask(path, body) {
    let response;
    try {
      response = await fetch(`https://${domain}${path}`, {
        method: "POST",
        headers: { "content-type": "application/json" },
        body: JSON.stringify(body),
      });
    } catch {
      throw fault("offline"); // The one boundary worth guarding: somebody's wifi.
    }
    const text = await response.text();
    let parsed = null;
    try { parsed = JSON.parse(text); } catch {}
    if (!response.ok) throw readFault(response.status, parsed);
    return parsed || {};
  }

  // Tokens in localStorage are readable by any script on this origin, which is
  // the honest cost of the design: oskiewar.com is a different registrable
  // domain from aesthetic.computer, so no cookie can carry this session across
  // — that gap is the whole reason a native sign-in is worth building. The
  // origin runs only our own code and loads no third-party script, and
  // auth0-spa-js already parks its own cache here for the redirect path.
  function keep(session) {
    live = session;
    try { store?.setItem(storeKey, JSON.stringify(session)); } catch {}
    return session;
  }

  function forget() {
    live = null;
    try { store?.removeItem(storeKey); } catch {}
  }

  function held() {
    if (live) return live;
    try {
      const raw = store?.getItem(storeKey);
      if (raw) live = JSON.parse(raw);
    } catch { forget(); }
    // A record written by an older shape of this module, or one that never
    // finished being written, is worse than none: it would be read back and
    // fail identically on every load, and its owner would never be offered the
    // door. Storage is a boundary, so this is a guard worth having.
    if (live && !(live.sub && live.access)) forget();
    return live;
  }

  function fromTokens(body, email) {
    const who = claims(body.id_token);
    return keep({
      sub: who.sub,
      email: who.email || email || "",
      // A refresh token only arrives if the client allows them. If it does not,
      // the session simply ends when the access token does and the person signs
      // in again — nothing here needs to defend against its absence.
      access: body.access_token,
      refresh: body.refresh_token || held()?.refresh || "",
      expires: Date.now() + (Number(body.expires_in) || 3600) * 1000,
    });
  }

  return {
    // What is on hand right now, without asking Auth0 anything. Null when
    // nobody is signed in this way.
    session: () => held(),

    // Step one: mail a code. Returns nothing — the only interesting outcome is
    // the throw.
    async sendCode(email) {
      const address = String(email || "").trim();
      if (!/^[^\s@]+@[^\s@]+\.[^\s@]+$/.test(address)) throw fault("bad-email");
      await ask("/passwordless/start", {
        client_id: clientId, connection, email: address, send: "code",
      });
      return address;
    },

    // Step two: spend the code. The realm has to name the same connection the
    // code was mailed from, or Auth0 looks for the address in the wrong place.
    async verify(email, code) {
      const otp = String(code || "").replace(/\D/g, "");
      const body = await ask("/oauth/token", {
        grant_type: "http://auth0.com/oauth/grant-type/passwordless/otp",
        client_id: clientId,
        realm: connection,
        username: String(email || "").trim(),
        otp,
        scope,
      });
      return fromTokens(body, email);
    },

    // An access token that is good for at least another minute, renewed from the
    // refresh token when it is not. Null means sign in again.
    async token() {
      const session = held();
      if (!session) return null;
      if (session.expires - Date.now() > 60000) return session.access;
      if (!session.refresh) { forget(); return null; }
      try {
        const body = await ask("/oauth/token", {
          grant_type: "refresh_token",
          client_id: clientId,
          refresh_token: session.refresh,
        });
        return fromTokens(body, session.email).access;
      } catch {
        forget(); // A refresh that fails is a session that is over, not an error to raise.
        return null;
      }
    },

    // No /v2/logout round trip: the OTP grant never set a browser cookie on
    // hi.aesthetic.computer, so there is no hosted session to end. Forgetting
    // the tokens is the whole of signing out, and it costs no navigation.
    forget,
  };
}
