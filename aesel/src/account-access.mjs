// The same account requirement applies to every Aesel interface and provider.
export async function verifyAccount(token, {fetch = globalThis.fetch, site = 'https://aesthetic.computer', authDomain = 'hi.aesthetic.computer', userAgent} = {}) {
  if (!token) throw Object.assign(new Error('Sign in to Aesthetic Computer to use Aesel.'), {code:'sign-in'});
  const json = async (url, headers, allowMissing = false) => {
    let response;
    try { response = await fetch(url, {headers, signal:AbortSignal.timeout(8000)}); }
    catch (error) { throw offlineError(error); }
    if (allowMissing && response.status === 404) return {handle:""};
    if (!response.ok) throw Object.assign(new Error(`Could not verify your AC account (HTTP ${response.status}). Sign in again or retry.`), {status:response.status, code:'account-unverified'});
    return response.json();
  };
  const user = await json(`https://${authDomain}/userinfo`, {Authorization:`Bearer ${token}`});
  if (typeof user?.sub !== 'string' || !user.sub) throw new Error('AC sign-in did not identify an account. Sign in again.');
  const result = await json(`${site}/handle?for=${encodeURIComponent(user.sub)}`, {Accept:'application/json', ...(userAgent ? {'User-Agent':userAgent} : {})}, true);
  const handle = typeof result?.handle === 'string' ? result.handle.trim().replace(/^@/, '') : '';
  return {sub:user.sub, handle};
}
// A request that never got an answer — no route, DNS, a timeout — as opposed
// to one the server refused. Offline is not signed out.
export function offlineError(error) {
  return Object.assign(new Error(`Can't reach aesthetic.computer — offline (${error?.cause?.code || error?.name || error?.message || 'no answer'}).`), {code:'offline', cause:error});
}
export const isOffline = error => error?.code === 'offline';
export function requireHandle(account) {
  if (!account?.handle) throw Object.assign(new Error('An AC @handle is required to use Aesel. Claim one before continuing.'), {code:'no-handle'});
  return account;
}
