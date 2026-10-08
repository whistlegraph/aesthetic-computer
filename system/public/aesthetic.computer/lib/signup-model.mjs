// Signup attempts are anonymous and short-lived. Account totals come from Auth0.
import { visitReferrer } from "./visit-model.mjs";
export const SIGNUP_COLLECTION = "signup-attempts";
export const SIGNUP_RETENTION_DAYS = 35;
export const SIGNUP_TTL_MS = 24 * 60 * 60 * 1000;
export const SIGNUP_STAGES = Object.freeze([
  "started", "auth_returned", "auth_failed", "verification_shown",
  "verification_resent", "verified", "handle_shown", "handle_failed", "completed",
  // In-page email-code door (signup-flow.mjs): handle first, then a mailed code.
  "handle_held", "code_sent", "code_failed", "fallback", "social_started",
]);
export const SIGNUP_SOURCES = Object.freeze(["prompt", "get-handle", "chat", "laer-klokken", "piece"]);
export const SIGNUP_ERRORS = Object.freeze(["network", "auth", "taken", "invalid", "unverified", "code", "other"]);
export const signupID = value => typeof value === "string" && /^[a-f0-9]{8}-[a-f0-9]{4}-4[a-f0-9]{3}-[89ab][a-f0-9]{3}-[a-f0-9]{12}$/i.test(value);

// Return navigation stays on this origin. Never retain prompt text, searches,
// fragments, callback credentials or private-tool routes in browser storage.
export function signupReturnPath(value, origin) {
  if (typeof value !== "string" || value.length > 256) return null;
  try {
    const url = new URL(value, origin);
    if (url.origin !== origin || url.username || url.password) return null;
    const path = url.pathname;
    if (!/^\/(?:@[a-z0-9._-]+\/)?[$a-z0-9][a-z0-9._$-]*\/?$/i.test(path)) return null;
    if (/^\/(?:prompt|hi|imnew|signup|get-handle|login|login-wait|email|mail|auth|callback|admin|desk|aa|wallet|account|device|machines|consent|help)\/?$/i.test(path)) return null;
    return path;
  } catch { return null; }
}

export function signupSource(path) {
  const name = String(path || "/").split(/[/?~:]/).filter(Boolean)[0] || "prompt";
  return SIGNUP_SOURCES.includes(name) ? name : "piece";
}

export function validateSignupEvent(body, origin) {
  if (!["https://aesthetic.computer", "https://www.aesthetic.computer", "https://laklok.com", "https://www.laklok.com"].includes(origin) ||
      !body || body.version !== 1 || !signupID(body.id) ||
      !["signup", "login", "handle"].includes(body.mode) ||
      !SIGNUP_SOURCES.includes(body.source) || !Array.isArray(body.stages) ||
      !body.stages.length || body.stages.length > SIGNUP_STAGES.length ||
      body.stages.some(stage => !SIGNUP_STAGES.includes(stage)) ||
      typeof body.automated !== "boolean" ||
      (body.error !== null && body.error !== undefined && !SIGNUP_ERRORS.includes(body.error))) return null;
  return { id: body.id.toLowerCase(), mode: body.mode, source: body.source,
    stages: [...new Set(body.stages)], automated: body.automated, error: body.error || null,
    property: new URL(origin).hostname.replace(/^www\./, ""),
    referrerHost: typeof body.referrerHost === "string" ? visitReferrer(body.referrerHost) : null };
}

export function signupUpdate(event, now = new Date()) {
  return {
    $setOnInsert: { mode: event.mode, source: event.source, property: event.property, referrerHost: event.referrerHost,
      expiresAt: new Date(+now + SIGNUP_RETENTION_DAYS * 86400000) },
    $min: { startedAt: now },
    $max: { updatedAt: now, ...Object.fromEntries(event.stages.map(stage => [`stages.${stage}`, true])),
      ...(event.error ? { [`errors.${event.error}`]: true } : {}) },
  };
}
