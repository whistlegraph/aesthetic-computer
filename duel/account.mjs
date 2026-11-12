// Reuse the existing AC browser session. No sign-in UI or token persistence here.
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
// Known at once, before any network: is someone signed in here? The server
// holds the seat for the handle when this says yes, so a slow sign-in never
// shows as a guest first.
export function accountExpected() {
  const h = host();
  return !!(window.acTOKEN || h?.acTOKEN || h?.acUSER || stored());
}
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
export async function accountToken() {
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
    if (location.hostname !== 'aesthetic.computer') return null;
    if (!stored()) return null;
    await import('./auth0.js');
    const auth = new window.auth0.Auth0Client({
      domain: 'https://hi.aesthetic.computer',
      clientId: CLIENT_ID,
      cacheLocation: 'localstorage',
      useRefreshTokens: true,
      authorizationParams: { redirect_uri: location.origin },
      httpTimeoutInSeconds: 15,
    });
    return await auth.getTokenSilently({ timeoutInSeconds: 15 });
  } catch {
    return null;
  }
}
