// Reuse the existing AC browser session. No sign-in UI or token persistence here.
export async function accountToken() {
  try {
    if (window.acTOKEN) return window.acTOKEN;
    if (window.auth0Client) return await window.auth0Client.getTokenSilently();
    if (location.hostname !== 'aesthetic.computer') return null;
    const clientId = 'LVdZaMbyXctkGfZDnpzDATB5nR0ZhmMt';
    if (
      !Object.keys(localStorage).some((k) =>
        k.startsWith(`@@auth0spajs@@::${clientId}`),
      )
    )
      return null;
    await import('./auth0.js');
    const auth = new window.auth0.Auth0Client({
      domain: 'https://hi.aesthetic.computer',
      clientId,
      cacheLocation: 'localstorage',
      useRefreshTokens: true,
      authorizationParams: { redirect_uri: location.origin },
      httpTimeoutInSeconds: 5,
    });
    return await auth.getTokenSilently({ timeoutInSeconds: 5 });
  } catch {
    return null;
  }
}
