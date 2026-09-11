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
// Four steps, no client secret at any of them (each verified 26.09.11 against
// the aesthetic tenant, from an Origin of https://oskiewar.com):
//
//   1. POST /passwordless/start  { client_id, connection, email, send: "code" }
//        → Auth0 mails six digits.
//   2. POST /co/authenticate     { client_id, credential_type: …/passwordless/otp,
//                                  realm, username, otp }
//        → { login_ticket }.  Cross-origin authentication: this is the SPA's
//          door, and the passwordless OTP credential type is accepted here.
//   3. GET /authorize?login_ticket=…&response_mode=web_message&prompt=none
//        in a hidden iframe → the page posts
//          { type: "authorization_response", response: { code, state } }
//        back to this origin.
//   4. POST /oauth/token  { grant_type: "authorization_code", code, code_verifier }
//        → id_token / access_token / refresh_token.
//
// Why not the one-shot /oauth/token passwordless OTP grant, which is shorter:
// it is refused for this client —
//   "Grant type 'http://auth0.com/oauth/grant-type/passwordless/otp' not allowed"
// — and that is on purpose rather than an oversight. Auth0 greys that grant out
// for a Single Page Application; the OTP grant is the Native / Regular Web door.
// A SPA gets there through cross-origin authentication instead, which is what
// auth0.js's `passwordlessLogin` does under its own covers. The authorization
// code grant in step 4 IS enabled for this client, which is why the last leg
// works where the first one could not.
//
// Every call stays on the custom domain. Cross-origin authentication leans on a
// cookie Auth0 sets on the authorization host, and on the canonical
// aesthetic.us.auth0.com that cookie is third-party to everybody. On
// hi.aesthetic.computer it is at least same-site for aesthetic.computer itself,
// which is where this module is headed next. For oskiewar.com — a different
// registrable domain — it is still third-party, so step 3 can fail on a browser
// that blocks those outright. That failure is caught and named rather than hung
// on: `silent-blocked` tells the caller to offer the redirect. (Encouragingly,
// a bogus ticket already gets as far as "Unknown login ticket" from a cookieless
// request, so the ticket lookup itself does not appear to need one.)
//
// It lives in lib/ rather than in a shell because the second reader is already
// known: aesthetic.computer wants the same in-page option, and the dance is the
// half that has nothing to do with either surface's furniture. It needs a
// browser — an iframe and a postMessage — but nothing of any app's furniture.

// Auth0's error vocabulary is wide and mostly not worth repeating to a person.
// These are the distinctions that change what someone should do next; anything
// unrecognised comes back as `unknown` with Auth0's own sentence attached, which
// is more honest than a shrug.
//
// The five tenant faults all mean the same thing to a caller — this tenant will
// not do it in-page right now, offer the redirect — but they are kept apart
// because each one names a different thing an operator would have to go and fix.
const messages = {
  "bad-email": "that email address doesn't look right",
  "no-connection": "email codes aren't switched on for this app yet",
  "grant-off": "email codes aren't switched on for this app yet",
  "origin-blocked": "this site isn't cleared for in-page sign-in yet",
  "silent-blocked": "in-page sign-in couldn't finish here",
  captcha: "sign-in needs the web page for this one",
  "wrong-code": "that code doesn't match",
  expired: "that code expired — ask for a new one",
  "rate-limited": "too many tries, wait a minute",
  offline: "couldn't reach the sign-in service",
  unknown: "sign-in failed",
};

// A caller that sees one of these should stop offering the form and offer the
// redirect instead: the tenant, not the person, is what is wrong.
export const tenantFaults = ["no-connection", "grant-off", "origin-blocked",
  "silent-blocked", "captcha"];

function fault(code, detail) {
  const error = new Error(detail || messages[code] || messages.unknown);
  error.code = code;
  error.hint = messages[code] || messages.unknown;
  error.tenant = tenantFaults.includes(code);
  return error;
}

// Four endpoints, four spellings of the same handful of troubles, so they are
// all read through one table. The sentence matters as much as the name here:
// Auth0 reuses `access_denied` and `invalid_request` for things a person can fix
// and things only an operator can.
function readFault(status, body) {
  const name = body?.error || "";
  const detail = body?.error_description || "";
  if (status === 429 || name === "too_many_requests") return fault("rate-limited");
  if (name === "requires_verification") return fault("captcha");
  if (name === "bad.email" || (name === "bad.request" && /email/i.test(detail)))
    return fault("bad-email");
  // /passwordless/start says bad.connection; /co/authenticate says "Unknown
  // realm <name>." for the very same missing connection.
  if (name === "bad.connection" || /unknown realm/i.test(detail))
    return fault("no-connection");
  if (/origin .* is not allowed/i.test(detail)) return fault("origin-blocked");
  // The one-shot OTP grant is refused for a SPA by design, so this branch should
  // now be unreachable from here. Kept because it costs a line and it is the
  // answer anyone pointing this module at a differently-typed client would get.
  if (name === "unauthorized_client") return fault("grant-off", detail);
  // A ticket that Auth0 does not recognise is a spent or timed-out one, which
  // reads to a person exactly like an expired code: ask for another.
  if (/login ticket/i.test(detail)) return fault("expired");
  // The silent /authorize refusing to proceed without a visible page. Nothing
  // typed in the form can fix it.
  if (name === "login_required" || name === "interaction_required" ||
      name === "consent_required") return fault("silent-blocked");
  if (name === "access_denied") {
    if (/wrong email or verification code/i.test(detail)) return fault("wrong-code");
    return fault("silent-blocked", detail);
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

// PKCE, by hand. auth0-spa-js has all of this, but it also insists on owning the
// whole redirect flow, and the only piece wanted here is a verifier and its
// challenge.
const base64url = (bytes) => btoa(String.fromCharCode(...new Uint8Array(bytes)))
  .replace(/\+/g, "-").replace(/\//g, "_").replace(/=+$/, "");

const entropy = (bytes = 32) =>
  base64url(crypto.getRandomValues(new Uint8Array(bytes)));

async function challenge(verifier) {
  const digest = await crypto.subtle.digest("SHA-256",
    new TextEncoder().encode(verifier));
  return base64url(digest);
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
  // Where the authorization code comes back to. It is never navigated to — the
  // silent leg only needs it to match a registered callback — but Auth0 checks
  // it at both /authorize and the token exchange, so the two must agree.
  redirectUri = globalThis.location?.origin,
  // How long to wait on the hidden iframe. An error page inside it posts
  // nothing at all (a refused /authorize answers 401 with plain HTML), so a
  // clock is the only thing standing between a blocked cookie and a form that
  // spins forever.
  silentTimeout = 20000,
  store = globalThis.localStorage,
  storeKey = "ac-otp-session",
} = {}) {
  let live = null; // The session in memory; `store` is only the copy that survives a reload.

  async function ask(path, body, credentials = "omit") {
    let response;
    try {
      response = await fetch(`https://${domain}${path}`, {
        method: "POST",
        headers: { "content-type": "application/json" },
        // /co/authenticate is the one that answers
        // `access-control-allow-credentials: true` and wants its cookie to ride.
        credentials,
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

  // The silent leg. A login ticket is spent at /authorize inside a hidden
  // iframe, and Auth0 answers by posting the authorization response back to this
  // window rather than navigating anywhere — that is what `response_mode=
  // web_message` means, and it is the only reason this whole flow can happen
  // without the page moving.
  //
  // Verified shape of what comes back (26.09.11, bogus ticket):
  //   { type: "authorization_response",
  //     response: { error: "invalid_request",
  //                 error_description: "Unknown login ticket.", state: "s" } }
  // posted with a targetOrigin of this very origin, and echoing our `state`.
  function silently(ticket, codeChallenge) {
    const state = entropy(16);
    const origin = "https://" + domain;
    const at = new URL(origin + "/authorize");
    for (const [key, value] of Object.entries({
      client_id: clientId,
      response_type: "code",
      response_mode: "web_message",
      // Never show a page. If Auth0 decides it needs one, it says so in the
      // response and this fails fast instead of parking a login form in a
      // 1-pixel iframe nobody can see.
      prompt: "none",
      login_ticket: ticket,
      redirect_uri: redirectUri,
      scope,
      state,
      nonce: entropy(16),
      code_challenge: codeChallenge,
      code_challenge_method: "S256",
    })) at.searchParams.set(key, value);

    return new Promise((resolve, reject) => {
      const frame = document.createElement("iframe");
      frame.style.display = "none";
      frame.setAttribute("aria-hidden", "true");
      let clock;

      function done(outcome, value) {
        clearTimeout(clock);
        removeEventListener("message", hear);
        frame.remove();
        outcome(value);
      }

      function hear(event) {
        // Anyone can postMessage at a window, so the sender, the shape and the
        // state all have to match before a word of it is believed.
        if (event.origin !== origin) return;
        if (event.data?.type !== "authorization_response") return;
        const answer = event.data.response || {};
        if (answer.state !== state) return;
        if (answer.code) return done(resolve, answer.code);
        done(reject, readFault(400, answer));
      }

      addEventListener("message", hear);
      clock = setTimeout(() => done(reject, fault("silent-blocked")),
        silentTimeout);
      frame.src = at.toString();
      document.body.appendChild(frame);
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

    // Steps two through four: spend the code. The realm has to name the same
    // connection the code was mailed from, or Auth0 looks for the address in the
    // wrong place.
    async verify(email, code) {
      const otp = String(code || "").replace(/\D/g, "");
      const username = String(email || "").trim();

      // 2 — the code buys a login ticket.
      const { login_ticket: ticket } = await ask("/co/authenticate", {
        client_id: clientId,
        credential_type: "http://auth0.com/oauth/grant-type/passwordless/otp",
        realm: connection,
        username,
        otp,
      }, "include");
      if (!ticket) throw fault("unknown", "no login ticket came back");

      // 3 — the ticket buys an authorization code, silently.
      const verifier = entropy();
      const authCode = await silently(ticket, await challenge(verifier));

      // 4 — the code buys the tokens, with the verifier proving we are the same
      // page that asked for it.
      const body = await ask("/oauth/token", {
        grant_type: "authorization_code",
        client_id: clientId,
        code: authCode,
        code_verifier: verifier,
        redirect_uri: redirectUri,
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
