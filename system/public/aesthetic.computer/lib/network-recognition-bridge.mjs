import { RECOGNITION_ORIGIN, RECOGNITION_PARENTS, RECOGNITION_UUID, RECOGNITION_TIMEOUT, recognitionEnabled } from "./network-recognition-model.mjs";

const AUTH_ORIGIN = "https://hi.aesthetic.computer";
const CLIENT_ID = "LVdZaMbyXctkGfZDnpzDATB5nR0ZhmMt";

// Read only existing credentials. In particular, constructing Auth0Client
// avoids createAuth0Client's automatic session refresh. cache-only never
// opens an auth iframe, refreshes a token, or triggers a login prompt.
export async function cachedAcToken(win, doc) {
  try {
    const session = win.localStorage.getItem("session-aesthetic");
    if (session) {
      const token = JSON.parse(win.atob(decodeURIComponent(session)))?.accessToken;
      if (typeof token === "string" && token.length > 0 && token.length <= 16384) return token;
    }
  } catch { /* inaccessible or malformed storage means no usable session */ }
  if (!win.auth0?.Auth0Client) {
    await new Promise((resolve, reject) => {
      const script = doc.createElement("script");
      script.src = "/aesthetic.computer/dep/auth0-spa-js.production.js";
      script.onload = resolve; script.onerror = reject;
      doc.head.append(script);
    });
  }
  const client = new win.auth0.Auth0Client({
    domain: AUTH_ORIGIN, clientId: CLIENT_ID,
    cacheLocation: "localstorage", useRefreshTokens: true,
    authorizationParams: { redirect_uri: RECOGNITION_ORIGIN },
  });
  return client.getTokenSilently({ cacheMode: "cache-only" });
}

// Bounded OIDC code + PKCE check against an existing SSO cookie. prompt=none
// fails when login/consent/MFA is needed. Keep the exchanged token in memory;
// do not use SDK silent login here, which can alter the app's logout/cache state.
export async function silentAcToken(win, doc, signal) {
  const encode = bytes => win.btoa(String.fromCharCode(...bytes)).replace(/\+/g, "-").replace(/\//g, "_").replace(/=+$/, "");
  const entropy = () => encode(win.crypto.getRandomValues(new Uint8Array(32)));
  const verifier = entropy(), state = entropy();
  const challenge = encode(new Uint8Array(await win.crypto.subtle.digest("SHA-256", new TextEncoder().encode(verifier))));
  if (signal?.aborted || !recognitionEnabled(win)) return;
  return new Promise(resolve => {
    const abort = new AbortController(), frame = doc.createElement("iframe");
    let settled = false, exchanging = false;
    const finish = token => {
      if (settled) return;
      settled = true; abort.abort(); frame.remove(); win.clearTimeout(timeout);
      win.removeEventListener("message", receive); win.removeEventListener("pagehide", cancel);
      signal?.removeEventListener("abort", cancel);
      resolve(token);
    };
    const cancel = () => finish();
    const receive = async event => {
      if (settled || exchanging || event.source !== frame.contentWindow || event.origin !== AUTH_ORIGIN ||
          event.data?.type !== "authorization_response") return;
      const response = event.data.response;
      if (response?.state !== state || response.error || typeof response.code !== "string" || !recognitionEnabled(win)) { finish(); return; }
      exchanging = true;
      try {
        const result = await win.fetch(`${AUTH_ORIGIN}/oauth/token`, {
          method: "POST", credentials: "omit", referrerPolicy: "no-referrer", signal: abort.signal,
          headers: { "Content-Type": "application/json" },
          body: JSON.stringify({ client_id: CLIENT_ID, grant_type: "authorization_code", code: response.code,
            code_verifier: verifier, redirect_uri: RECOGNITION_ORIGIN }),
        });
        const body = result.ok ? await result.json() : null;
        const token = body?.access_token;
        finish(recognitionEnabled(win) && typeof token === "string" && token.length > 0 && token.length <= 16384 ? token : undefined);
      } catch { finish(); }
    };
    const timeout = win.setTimeout(cancel, 4000);
    win.addEventListener("message", receive); win.addEventListener("pagehide", cancel);
    signal?.addEventListener("abort", cancel, { once: true });
    const url = new URL(`${AUTH_ORIGIN}/authorize`);
    url.search = new URLSearchParams({ client_id: CLIENT_ID, redirect_uri: RECOGNITION_ORIGIN,
      response_type: "code", response_mode: "web_message", scope: "openid profile email", prompt: "none",
      state, nonce: entropy(), code_challenge: challenge, code_challenge_method: "S256" }).toString();
    frame.hidden = true; frame.tabIndex = -1; frame.title = "AC sign-in check";
    frame.referrerPolicy = "no-referrer"; frame.src = url.href;
    doc.body.append(frame);
  });
}

export async function existingAcToken(win, doc, signal) {
  if (signal?.aborted || !recognitionEnabled(win)) return;
  try { const token = await cachedAcToken(win, doc); if (token) return token; } catch { /* try the existing SSO session */ }
  if (!signal?.aborted && recognitionEnabled(win)) return silentAcToken(win, doc, signal);
}

export function startRecognitionBridge(win = window, doc = document, getToken = signal => existingAcToken(win, doc, signal)) {
  if (win === win.parent || win.location.origin !== RECOGNITION_ORIGIN || !recognitionEnabled(win)) return null;
  let used = false, stopped = false;
  const abort = new AbortController();
  const stop = () => {
    stopped = true; abort.abort(); win.clearTimeout(timeout);
    win.removeEventListener("message", receive); win.removeEventListener("pagehide", stop);
    win.removeEventListener("storage", storageChanged);
  };
  const storageChanged = event => {
    if (event.key === null || event.key === "session-aesthetic" || event.key === "ac-otp-session" ||
        event.key?.startsWith("@@auth0spajs@@")) stop();
  };
  async function receive(event) {
    if (used || stopped || event.source !== win.parent || !RECOGNITION_PARENTS.includes(event.origin) ||
        event.data?.type !== "ac-recognition:check" || !RECOGNITION_UUID.test(event.data.id || "") ||
        !recognitionEnabled(win)) return;
    used = true;
    const id = event.data.id;
    let token;
    try { token = await getToken(abort.signal); } catch { /* unavailable authentication is unknown */ }
    if (stopped || !recognitionEnabled(win)) { stop(); return; }
    try {
      await win.fetch("/api/network-recognition", {
        method: "POST", credentials: "omit", referrerPolicy: "no-referrer", signal: abort.signal,
        headers: { "Content-Type": "application/json", ...(token ? { Authorization: `Bearer ${token}` } : {}) },
        body: JSON.stringify({ version: 1, id, phase: "checked" }),
      });
    } catch { /* the initial record stays unknown */ }
    if (!stopped) win.parent.postMessage({ type: "ac-recognition:done", id }, event.origin);
    stop();
  }
  const timeout = win.setTimeout(stop, RECOGNITION_TIMEOUT);
  win.addEventListener("message", receive); win.addEventListener("pagehide", stop);
  win.addEventListener("storage", storageChanged);
  return { stop };
}

if (typeof window !== "undefined") startRecognitionBridge();
