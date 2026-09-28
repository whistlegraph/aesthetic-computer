// Account allowance only. Never persist a balance or let an old account's
// response overwrite the active one after sign-out or an account switch.
export function createCredits({ token, emit, site, fetch: request = (...args) => fetch(...args) }) {
  let generation = 0, controller;
  function clear() {
    generation++;
    controller?.abort();
    emit({ type: "credits", total: null, status: "Sign in to view braincells" });
  }
  async function refresh() {
    const credential = token();
    if (!credential) { clear(); return; }
    const current = ++generation;
    controller?.abort();
    const active = controller = new AbortController();
    const timer = setTimeout(() => active.abort(), 8000);
    try {
      const response = await request(`${site}/api/easel-credits`, {
        headers: { Authorization: `Bearer ${credential}` }, signal: active.signal,
      });
      if (!response.ok) throw new Error("Allowance unavailable");
      const value = await response.json();
      if (![value.remaining, value.purchased].every(n => Number.isFinite(n) && n >= 0)) throw new Error("Invalid allowance");
      if (current !== generation || credential !== token()) return;
      const dollars = value.dollars;
      const validDollars = dollars?.currency === "USD" && [dollars.total, dollars.free, dollars.purchased].every(n => Number.isFinite(n) && n >= 0);
      emit({ type: "credits", total: value.remaining + value.purchased, ...(validDollars ? { dollars } : {}), status: "ready" });
    } catch {
      if (current === generation && credential === token()) {
        emit({ type: "credits", total: null, status: "Braincells unavailable · tap to retry" });
      }
    } finally { clearTimeout(timer); }
  }
  // The desktop's checkout: one Stripe session per request id, reused until it
  // is paid or expires, and credited only once the server has seen the payment.
  let pending = null, opening = false;
  async function call(body) {
    const credential = token();
    if (!credential) throw new Error("Sign in to AC before buying braincells.");
    const response = await request(`${site}/api/easel-checkout`, {
      method: "POST", headers: { "Content-Type": "application/json", Authorization: `Bearer ${credential}` },
      body: JSON.stringify(body),
    });
    const value = await response.json().catch(() => ({}));
    if (!response.ok) throw new Error(value.error || "Checkout unavailable");
    return value;
  }
  async function buy() {
    if (opening) return;
    opening = true;
    try {
      if (pending?.sessionId) {
        const status = await call({ sessionId: pending.sessionId });
        if (status.paid || status.status === "expired") pending = null;
      }
      if (!pending) pending = { requestId: crypto.randomUUID() };
      const checkout = await call({ pack: "luna-1m-v1", requestId: pending.requestId });
      const url = new URL(checkout.url);
      if (url.protocol !== "https:" || !["checkout.stripe.com", "pay.aesthetic.computer"].includes(url.hostname)) throw new Error("Invalid checkout address");
      pending.sessionId = checkout.sessionId;
      emit({ type: "checkout", url: url.href, sessionId: checkout.sessionId });
    } catch (error) {
      emit({ type: "checkout", error: error.message || "Checkout unavailable" });
    } finally { opening = false; }
  }
  async function check() {
    if (!pending?.sessionId) return;
    try {
      const status = await call({ sessionId: pending.sessionId });
      if (status.paid || status.status === "expired") {
        pending = null;
        await refresh();
        emit({ type: "checkout", paid: !!status.paid, done: true });
      }
    } catch {}
  }
  return { refresh, clear: () => { pending = null; clear(); }, buy, check };
}
