// Reuse the existing AC browser session. On aesthetic.computer there is no
// sign-in UI or token persistence here; rax.ac signs in on its own (below).
const CLIENT_ID = 'LVdZaMbyXctkGfZDnpzDATB5nR0ZhmMt';
// Under the prompt's paper the host page is the same origin and already holds
// the session: its token is this world's token, with no second sign-in.
function host() {
  try {
    return window.parent !== window && window.parent.location.origin === location.origin ? window.parent : null;
  } catch {
    return null;
  }
}
const stored = () => {
  try {
    return Object.keys(localStorage).some((k) => k.startsWith(`@@auth0spajs@@::${CLIENT_ID}`));
  } catch {
    return false;
  }
};
// 🪪 A session handed to AC's pages by a host (the native app's shared login, the desktop app, the VS Code extension) is kept by boot.mjs as `session-aesthetic`, on this same origin: base64 JSON of { accessToken, account }. rax opened on its own (a page loaded again at /rax/, with no prompt around it) reads it there, so it comes up as who the prompt was, not as a guest (2026-11-20). An expired one is refused by the server and the visit is a guest's, as before.
const handed = () => {
  try {
    const kept = localStorage.getItem('session-aesthetic');
    return (kept && JSON.parse(atob(decodeURIComponent(kept)))?.accessToken) || null;
  } catch {
    return null;
  }
};
// Known at once, before any network: is someone signed in here? The server
// holds the seat for the handle when this says yes, so a slow sign-in never
// shows as a guest first.
export function accountExpected() {
  const h = host();
  return !!(window.acTOKEN || h?.acTOKEN || h?.acUSER || stored() || handed() || returning());
}
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
// 🏠 rax.ac is rax on a name of its own (2026-11-27). aesthetic.computer's
// session lives in that origin's storage, so here rax keeps its own: the same
// Auth0 app, signed into from this page and kept in rax.ac's storage.
const OWN = location.hostname === 'rax.ac';
let auth0;
async function client() {
  if (!auth0) {
    await import('./auth0.js');
    auth0 = new window.auth0.Auth0Client({
      domain: 'https://hi.aesthetic.computer',
      clientId: CLIENT_ID,
      cacheLocation: 'localstorage',
      useRefreshTokens: true,
      authorizationParams: { redirect_uri: location.origin },
      httpTimeoutInSeconds: 15,
    });
  }
  return auth0;
}
// Back from Auth0 (?code=…&state=…): finish once, then put back the address
// the player left from, without the code. A room other than the front one
// is loaded again (the page chose its room before this finished).
const returning = () => OWN && /[?&]code=/.test(location.search) && /[?&]state=/.test(location.search);
let landed;
const land = () =>
  (landed ||= (async () => {
    if (!returning()) return;
    try {
      const { appState } = await (await client()).handleRedirectCallback();
      const back = appState?.back || '/';
      if (back !== '/') return void location.replace(back);
      history.replaceState(null, '', back);
    } catch {
      history.replaceState(null, '', '/');
    }
  })());
export async function accountToken() {
  await land();
  try {
    if (window.acTOKEN) return window.acTOKEN;
    const h = host();
    if (h) {
      // The host may still be starting its own session: give it a moment
      // (8 s: on a slow link the prompt's own sign-in takes that long)
      // rather than opening a second one beside it.
      for (let i = 0; i < 80 && !h.acTOKEN && !h.auth0Client && stored(); i++) await sleep(100);
      if (h.acTOKEN) return h.acTOKEN;
      if (h.auth0Client) {
        try {
          const token = await h.auth0Client.getTokenSilently();
          if (token) return token;
        } catch {}
      }
    }
    if (window.auth0Client) return await window.auth0Client.getTokenSilently();
    if (location.hostname !== 'aesthetic.computer' && !OWN) return null;
    if (!h && !stored() && handed()) return handed(); // (standing alone, with a host's session and none of its own)
    if (!stored()) return null;
    return await (await client()).getTokenSilently({ timeoutInSeconds: 15 });
  } catch {
    return null;
  }
}
// 🪪 The way to a seat. Under the prompt the host's own sign-up opens; a
// page on its own goes to the prompt, where an account or a handle is made.
export function askAccount(needs = 'account') {
  const h = host();
  if (needs === 'account' && typeof h?.acLOGIN === 'function') return void h.acLOGIN('signup');
  // On rax.ac the sign-in is this page's own, and comes back to the same room.
  if (needs === 'account' && OWN) {
    return void client().then((auth) =>
      auth.loginWithRedirect({
        appState: { back: location.pathname + location.search },
        authorizationParams: { screen_hint: 'signup' },
      }),
    );
  }
  const to = needs === 'account' ? 'https://aesthetic.computer/?signup' : 'https://aesthetic.computer/prompt';
  try {
    (window.top || window).location.href = to;
  } catch {
    location.href = to;
  }
}
