// choir events, 26.10.12
// A live trail of what people do in choir, so a stuck sign-in or a refused
// post shows up while it is happening. Web and app send client events; the
// server adds its own at each door. Kept 30 days.
export const EVENT_COLLECTION = "choir-events";
const NAME = /^[a-z][a-z0-9:_-]{1,40}$/;

export function cleanEvent(raw, server = {}) {
  if (!raw || !NAME.test(raw.name || "")) return null;
  const props = {};
  for (const [k, v] of Object.entries(raw.props || {}).slice(0, 12)) {
    if (!/^[a-zA-Z0-9_]{1,24}$/.test(k)) continue;
    if (typeof v === "string") props[k] = v.slice(0, 160);
    else if (typeof v === "number" || typeof v === "boolean") props[k] = v;
  }
  const session = typeof raw.session === "string" && /^[a-zA-Z0-9-]{8,64}$/.test(raw.session) ? raw.session : null;
  const surface = ["web", "ios", "server"].includes(raw.surface) ? raw.surface : "web";
  return { name: raw.name, props, session, surface, ...server };
}

export async function record(db, event) {
  if (!event) return;
  const c = db.collection(EVENT_COLLECTION);
  record.ready ??= c.createIndex({ at: 1 }, { expireAfterSeconds: 30 * 86400 }).catch(() => (record.ready = null));
  await c.insertOne({ ...event, at: new Date() });
}

export async function recent(db, { since, limit = 200 } = {}) {
  const q = since ? { at: { $gt: new Date(since) } } : {};
  return db.collection(EVENT_COLLECTION).find(q, { projection: { _id: 0 } }).sort({ at: -1 }).limit(Math.min(limit, 500)).toArray();
}
