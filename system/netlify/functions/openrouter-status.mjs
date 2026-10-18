// Is AC's OpenRouter account funded? Anyone may ask and hear yes, no or
// unknown, with the floor; an admin also sees the dollars. `npm run doctor`
// and `ac run` ask before they rely on it. See backend/openrouter-balance.mjs.
import { createBalance, MIN_BALANCE_USD } from "../../backend/openrouter-balance.mjs";

export function createHandler({ balance, authorize, hasAdmin, floor = MIN_BALANCE_USD }) {
  const reply = (statusCode, value) => ({ statusCode, headers: {
    "Content-Type": "application/json", "Cache-Control": "no-store",
    "Access-Control-Allow-Origin": "*", "Access-Control-Allow-Methods": "GET, OPTIONS",
    "Access-Control-Allow-Headers": "Authorization",
  }, body: JSON.stringify(value) });
  return async (event) => {
    if (event.httpMethod === "OPTIONS") return reply(204, null);
    if (event.httpMethod !== "GET") return reply(405, { error: "GET only" });
    const state = await balance();
    const funded = state.unknown ? null : state.balance_usd >= floor;
    let admin = false;
    if (event.headers?.authorization) {
      try { const user = await authorize(event.headers); admin = !!user && await hasAdmin(user, "aesthetic"); } catch {}
    }
    return reply(200, {
      provider: "openrouter", funded, floor_usd: floor, checked_at: state.checked_at,
      ...(funded === false ? { message: "AC's OpenRouter account needs funding; inference will fail with 402 when it runs out." } : {}),
      ...(admin ? (state.unknown ? { reason: state.reason } : { balance_usd: Math.round(state.balance_usd * 100) / 100, credits_usd: state.credits_usd }) : {}),
    });
  };
}

let balance = null;
export async function handler(event) {
  const { authorize, hasAdmin } = await import("../../backend/authorization.mjs");
  balance ??= createBalance({ key: process.env.OPENROUTER_API_KEY });
  const floor = Number(process.env.OPENROUTER_MIN_BALANCE_USD) || MIN_BALANCE_USD;
  return createHandler({ balance, authorize, hasAdmin, floor })(event);
}
