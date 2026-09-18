// Read the signed-in handle's existing daily allowance; never a vendor balance.
export function createHandler({ authorize, getHandleOrEmail, checkBudget, paidBalance = async () => 0, offer = null, creditPack = {amount: 500, credits: 1_000_000} }) {
  const reply = (statusCode, value) => ({ statusCode, headers: {
    "Content-Type": "application/json", "Cache-Control": "private, no-store",
    "Access-Control-Allow-Origin": "*", "Access-Control-Allow-Methods": "GET, OPTIONS",
    "Access-Control-Allow-Headers": "Authorization",
  }, body: JSON.stringify(value) });
  return async event => {
    if (event.httpMethod === "OPTIONS") return reply(204, null);
    if (event.httpMethod !== "GET") return reply(405, { error: "GET only" });
    if (!event.headers?.authorization) return reply(401, { error: "Sign in to view your allowance" });
    try {
      const user = await authorize(event.headers);
      if (!user?.sub) return reply(401, { error: "Invalid sign-in" });
      const handle = await getHandleOrEmail(user.sub);
      if (typeof handle !== "string" || !handle.startsWith("@")) return reply(403, { error: "A handle is required" });
      const budget = await checkBudget(handle.slice(1));
      if (!budget || budget.unknown || ![budget.used, budget.budget, budget.remaining].every(Number.isFinite)) {
        return reply(503, { error: "Allowance unavailable" });
      }
      const purchased = await paidBalance(user.sub);
      if (!Number.isFinite(purchased) || purchased < 0) return reply(503, { error: "Allowance unavailable" });
      const valueUSD = cells => Math.round(cells * creditPack.amount / creditPack.credits * 1e6) / 1e8;
      const dollars = { currency: "USD", free: valueUSD(budget.remaining), purchased: valueUSD(purchased),
        total: valueUSD(budget.remaining + purchased) };
      return reply(200, { dollars, purchased, offer, handle, unit: "weighted_tokens", remaining: budget.remaining,
        used: budget.used, limit: budget.budget, day: budget.day,
        resetsAt: new Date(Date.parse(budget.day + "T00:00:00Z") + 86400000).toISOString() });
    } catch { return reply(503, { error: "Allowance unavailable" }); }
  };
}

export async function handler(event) {
  const [{ authorize, getHandleOrEmail }, { checkBudget }, paid] = await Promise.all([
    import("../../backend/authorization.mjs"), import("../../backend/ai-budget.mjs"),
    import("../../backend/easel-paid-credits.mjs"),
  ]);
  return createHandler({ authorize, getHandleOrEmail, checkBudget, paidBalance:user=>paid.withWallets(w=>paid.balance(user,w)), creditPack:paid.CREDIT_PACK, offer:process.env.AC_CREDITS_CHECKOUT_ENABLED === "true" ? paid.CREDIT_PACK : null })(event);
}
