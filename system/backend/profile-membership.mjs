// The auth provider owns registration dates and successful-login totals.
// Keep this optional lookup separate from the fast media card response.
export function createMembershipReader({ request = fetch, env = process.env, now = Date.now } = {}) {
  const base = "https://aesthetic.us.auth0.com", cache = new Map(), pending = new Map();
  let token, tokenUntil = 0, tokenRequest;
  async function json(url, options) {
    const response = await request(url, { ...options, signal: AbortSignal.timeout(5000) });
    if (!response.ok) throw new Error("Membership data unavailable");
    return response.json();
  }
  async function accessToken() {
    if (token && now() < tokenUntil) return token;
    tokenRequest ||= json(base + "/oauth/token", {
      method: "POST", headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ client_id: env.AUTH0_M2M_CLIENT_ID, client_secret: env.AUTH0_M2M_SECRET,
        audience: base + "/api/v2/", grant_type: "client_credentials" }),
    }).then(data => {
      if (!data.access_token) throw new Error("Membership data unavailable");
      token = data.access_token;
      tokenUntil = now() + Math.max(0, (Number(data.expires_in) || 3600) - 60) * 1000;
      return token;
    }).finally(() => { tokenRequest = null; });
    return tokenRequest;
  }
  return async user => {
    const cached = cache.get(user);
    if (cached && now() < cached.until) return cached.value;
    if (pending.has(user)) return pending.get(user);
    if (!env.AUTH0_M2M_CLIENT_ID || !env.AUTH0_M2M_SECRET) return { registeredAt: null, loginCount: null };
    const task = (async () => {
      const value = await json(base + "/api/v2/users/" + encodeURIComponent(user) + "?fields=created_at,logins_count&include_fields=true",
        { headers: { Authorization: "Bearer " + await accessToken() } });
      const date = new Date(value.created_at);
      const result = { registeredAt: Number.isFinite(+date) ? date.toISOString().slice(0, 10) : null,
        loginCount: Number.isSafeInteger(value.logins_count) && value.logins_count >= 0 ? value.logins_count : null };
      cache.delete(user);
      cache.set(user, { value: result, until: now() + 1800000 });
      if (cache.size > 500) cache.delete(cache.keys().next().value);
      return result;
    })();
    pending.set(user, task);
    try { return await task; } finally { pending.delete(user); }
  };
}

export const readMembership = createMembershipReader();
