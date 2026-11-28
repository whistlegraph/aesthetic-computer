// ☠️ The death count. Every time a handle's body dies in rax it is counted in
// AC's database (the site's /api/rax-deaths, collection `rax-deaths`), so the
// number is the handle's own across rooms, hosts and restarts. Only this
// server writes it, signed with the secret it shares with lith
// (RAX_BUILDER_TOKEN); without that secret (a satellite, a test, a laptop)
// nothing is counted and nothing is shown. A guest has no handle, so no count.
const API = 'https://aesthetic.computer/api/rax-deaths';
export function deathLedger({ url = API, token = process.env.RAX_BUILDER_TOKEN, fetch = globalThis.fetch } = {}) {
  const ask = async (init, query = '') => {
    try {
      const r = await fetch(url + query, { ...init, signal: AbortSignal.timeout(6000) });
      if (!r.ok) return null;
      const { deaths } = await r.json();
      return Number.isFinite(deaths) ? deaths : null;
    } catch { return null; }
  };
  return {
    // One more death for this handle; the new total, or null if it was not counted.
    count: (handle, kind = 'other') => (!token || !handle ? Promise.resolve(null) : ask({
      method: 'POST',
      headers: { 'Content-Type': 'application/json', authorization: `Bearer ${token}` },
      body: JSON.stringify({ handle, kind }),
    })),
    read: (handle) => (!handle ? Promise.resolve(null) : ask({}, `?handle=${encodeURIComponent(handle)}`)),
  };
}
