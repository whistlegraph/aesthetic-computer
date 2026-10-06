// A capability from the authenticated relay, never a saved handle preference.
export function hasPersonalAccess(access, now=Date.now()) {
  return access?.personal===true && Array.isArray(access.providers) &&
    ['claude','codex'].every(provider=>access.providers.includes(provider)) &&
    (access.expiresAt===null || Date.parse(access.expiresAt)>now);
}
export async function fetchPersonalAccess(token, {fetch=globalThis.fetch}={}) {
  if(!token)return null;
  try {
    const response=await fetch('https://help.aesthetic.computer/api/aesel/access',{
      headers:{Authorization:'Bearer '+token},signal:AbortSignal.timeout(8000)});
    if(!response.ok)return null;
    const access=await response.json();return hasPersonalAccess(access)?access:null;
  } catch {return null;}
}
