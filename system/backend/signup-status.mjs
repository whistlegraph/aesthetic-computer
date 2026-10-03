import { respond } from "./http.mjs";

// Only the caller's own account. Never select an account by submitted email/sub.
export function createSignupStatusHandler({ authorize, userEmailFromID, handleFor, forgetAuthorizations, resendVerificationEmail }) {
  const rates = new Map();
  return async event => {
    const reply = (status, body) => respond(status, body, { "Cache-Control": "no-store" });
    if (event.httpMethod === "OPTIONS") return reply(204, "");
    if (!["GET", "POST"].includes(event.httpMethod)) return reply(405, { message: "Method not allowed" });
    if (event.httpMethod === "POST") {
      if (event.isBase64Encoded || typeof event.body !== "string" || event.body.length > 128) return reply(400, { message: "Invalid request" });
      try { if (JSON.parse(event.body)?.action !== "resend") return reply(400, { message: "Invalid action" }); }
      catch { return reply(400, { message: "Invalid JSON" }); }
    }
    try {
      const user = await authorize(event.headers || {}, "aesthetic");
      if (!user?.sub) return reply(401, { message: "unauthorized" });
      const now = Date.now();
      for (const [key, rate] of rates) if (rate.until <= now) rates.delete(key);
      if (!rates.has(user.sub) && rates.size >= 10000) return reply(429, { message: "Try again later" });
      const rate = rates.get(user.sub) || { until: now + 60000, count: 0, resent: false };
      rates.set(user.sub, rate);
      if (++rate.count > 12 || (event.httpMethod === "POST" && rate.resent)) return reply(429, { message: "Try again in a minute" });
      // The login token can predate verification; check Auth0's current record.
      const profile = await userEmailFromID(user.sub, "aesthetic");
      if (typeof profile?.email_verified !== "boolean") return reply(503, { message: "Account check unavailable" });
      if (event.httpMethod === "POST") {
        if (profile.email_verified) return reply(400, { message: "Already verified" });
        rate.resent = true;
        await resendVerificationEmail(user.sub);
        return reply(200, { sent: true });
      }
      if (profile.email_verified) forgetAuthorizations(user.sub);
      const handle = await handleFor(user.sub);
      return reply(200, { verified: profile.email_verified, handle: typeof handle === "string" ? handle : null });
    } catch { return reply(503, { message: "Account check unavailable" }); }
  };
}
