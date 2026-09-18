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
      emit({ type: "credits", total: value.remaining + value.purchased, status: "ready" });
    } catch {
      if (current === generation && credential === token()) {
        emit({ type: "credits", total: null, status: "Braincells unavailable · tap to retry" });
      }
    } finally { clearTimeout(timer); }
  }
  return { refresh, clear };
}
