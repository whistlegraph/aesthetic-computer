// Loopboy subscriptions are data. Producers run their own code and publish
// events; binding, delivery and waiting never execute producer-supplied code.
const KEY = /^[a-z0-9][a-z0-9_-]{0,39}$/;

export function loopboyBinding(value = {}) {
  if (!value || typeof value !== "object" || Array.isArray(value)) throw new Error("Loopboy binding must be an object");
  const event = String(value.event || (value.contact ? "imessage" : "")).trim().toLowerCase();
  const source = String(value.source || value.contact || "").trim().toLowerCase();
  if (!KEY.test(event) || !KEY.test(source)) throw new Error("Loopboy event and source must be short lowercase keys (1–40 characters)");
  if (value.contact && (event !== "imessage" || String(value.contact).trim().toLowerCase() !== source)) {
    throw new Error("contact is only an alias for an imessage source");
  }
  const binding = { event, source, routeKey: event === "imessage" ? source : `${event}:${source}`,
    contact: event === "imessage" ? source : "" };
  if (value.description !== undefined) {
    if (typeof value.description !== "string" || value.description.length > 240) throw new Error("Loopboy description must be at most 240 characters");
    binding.description = value.description;
  }
  if (value.config !== undefined) {
    if (!value.config || typeof value.config !== "object" || Array.isArray(value.config) || Buffer.byteLength(JSON.stringify(value.config)) > 4096) {
      throw new Error("Loopboy config must be a JSON object of at most 4096 bytes");
    }
    binding.config = JSON.parse(JSON.stringify(value.config));
  }
  if (value.expiresAt !== undefined && value.expiresAt !== "") {
    if (typeof value.expiresAt !== "string" || !Number.isFinite(Date.parse(value.expiresAt))) throw new Error("Loopboy expiresAt must be an ISO date");
    binding.expiresAt = new Date(value.expiresAt).toISOString();
  }
  return binding;
}

// An explicit OFF/corrupt/expired mode must never fall back to launch settings.
export function activeLoopboyBinding(value, now = Date.now()) {
  try {
    const binding = loopboyBinding(value);
    return binding.expiresAt && Date.parse(binding.expiresAt) <= now ? null : binding;
  } catch { return null; }
}

export function loopboyRouteMatches(route, binding, sessionId, bindingId) {
  if (!route || route.sessionId !== sessionId) return false;
  const actual = activeLoopboyBinding({ ...route, event: route.event || route.channel || "imessage",
    source: route.source || route.contact || binding.source });
  return actual?.routeKey === binding.routeKey && (!bindingId || route.bindingId === bindingId);
}

export const LOOPBOY_BINDING_PROPERTIES = {
  event: { type: "string", description: "Event family, e.g. imessage, fishery, build, deploy. Lowercase key, 1–40 characters." },
  source: { type: "string", description: "Source key within that event family, e.g. studio. For imessage, the configured contact key." },
  description: { type: "string", maxLength: 240, description: "What this source reports." },
  config: { type: "object", description: "Producer-specific JSON metadata, at most 4096 bytes. Stored for inspection; never executed." },
  expiresAt: { type: "string", description: "Optional ISO expiry; delivery and waiting stop when it expires." },
};
