import { createHash, timingSafeEqual } from "node:crypto";

// Keep operational inventories and request/error logs behind the same admin
// identity check used by AC's other admin APIs. No network call for anonymous
// requests; authorization failures never fall through to the data handler.
export function operationsAuth(load = () => import("../system/backend/authorization.mjs"), serviceToken = process.env.LITH_OPERATIONS_TOKEN) {
  return async (req, res, next) => {
    res.set("Cache-Control", "private, no-store");
    if (!/^Bearer\s+\S+$/i.test(req.headers.authorization || "")) {
      return res.status(401).json({ error: "Unauthorized" });
    }
    // Server-side diagnostics have no interactive login. This credential is
    // accepted only by these read routes, never by product or deploy handlers.
    const token = req.headers.authorization.replace(/^Bearer\s+/i, "");
    const digest = value => createHash("sha256").update(value).digest();
    if (serviceToken && ["GET", "HEAD"].includes(req.method) &&
        timingSafeEqual(digest(token), digest(serviceToken))) return next();
    try {
      const { authorize, hasAdmin } = await load();
      const user = await authorize({ authorization: req.headers.authorization });
      if (!user) return res.status(401).json({ error: "Unauthorized" });
      if (!await hasAdmin(user)) return res.status(403).json({ error: "Admin access required" });
    } catch {
      return res.status(503).json({ error: "Authorization unavailable" });
    }
    return next();
  };
}
