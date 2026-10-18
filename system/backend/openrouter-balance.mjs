// AC's OpenRouter balance. One account pays every product's inference —
// Aesel, Whistlegraph, No Paint, Choir's curator, Oskiewar's Jev — and
// nothing tops it up from braincell sales: braincells land in
// ac-credit-wallets, and the account is funded on openrouter.ai (by hand, or
// by its auto top-up). Read-only, so a check can say the account is low
// before OpenRouter starts answering 402.

// Below this many dollars the account counts as unfunded. A few long
// Whistlegraph turns or one `ac run` batch can spend a dollar.
export const MIN_BALANCE_USD = 5;

export function createBalance({ key, fetch = globalThis.fetch, now = Date.now, ttl = 60_000 } = {}) {
  let cached = null, until = 0, pending = null;
  return async function balance() {
    if (cached && until > now()) return cached;
    if (pending) return pending;
    pending = (async () => {
      const checked_at = new Date(now()).toISOString();
      if (!key) return { checked_at, unknown: true, reason: "no key" };
      try {
        const response = await fetch("https://openrouter.ai/api/v1/credits", {
          headers: { Authorization: `Bearer ${key}` }, redirect: "error", signal: AbortSignal.timeout(5000),
        });
        if (!response.ok) return { checked_at, unknown: true, reason: `OpenRouter answered ${response.status}` };
        const { data } = await response.json();
        if (!Number.isFinite(data?.total_credits) || !Number.isFinite(data?.total_usage)) return { checked_at, unknown: true, reason: "unreadable balance" };
        return { checked_at, credits_usd: data.total_credits, balance_usd: Math.max(0, data.total_credits - data.total_usage) };
      } catch { return { checked_at, unknown: true, reason: "OpenRouter unreachable" }; }
    })().then((value) => { cached = value; until = now() + ttl; return value; }).finally(() => { pending = null; });
    return pending;
  };
}
