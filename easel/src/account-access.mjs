// The same account requirement applies to every Aesel interface and provider.
export async function verifyAccount(token, {fetch = globalThis.fetch, site = 'https://aesthetic.computer', authDomain = 'hi.aesthetic.computer', userAgent} = {}) {
  if (!token) throw Object.assign(new Error('Sign in to Aesthetic Computer to use Aesel.'), {code:'sign-in'});
  const json = async (url, headers, allowMissing = false) => {
    const response = await fetch(url, {headers, signal:AbortSignal.timeout(8000)});
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
export function requireHandle(account) {
  if (!account?.handle) throw Object.assign(new Error('An AC @handle is required to use Aesel. Claim one before continuing.'), {code:'no-handle'});
  return account;
}
