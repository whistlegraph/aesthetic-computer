import { gameStore } from "../../backend/oskie-store.mjs";
const headers = {
  "Content-Type": "application/json",
  "Cache-Control": "no-store",
  "Referrer-Policy": "no-referrer",
  "X-Content-Type-Options": "nosniff",
};
const reply = (statusCode, value, extra = {}) => ({
  statusCode,
  headers: { ...headers, ...extra },
  body: JSON.stringify(value),
});
export function createGameAPI({ store = gameStore } = {}) {
  return async (event) => {
    try {
      const match =
        /\/api\/oskie\/([a-f0-9]{32})(?:\/(download|feedback))?\/?$/.exec(
          event.path || "",
        );
      if (!match) return reply(404, { error: "Not found" });
      const [, id, action] = match,
        token = (event.headers?.authorization || "").replace(/^Preview /, "");
      const s = await store();
      if (event.httpMethod === "POST" && action === "feedback") {
        if ((event.body || "").length > 950000)
          return reply(413, { error: "Report too large" });
        let data;
        try {
          data = JSON.parse(event.body || "");
        } catch {
          return reply(400, { error: "Invalid JSON" });
        }
        return reply(200, await s.report(id, token, data));
      }
      if (event.httpMethod !== "GET")
        return reply(405, { error: "Method not allowed" });
      const game = token
        ? await s.readPreview(id, token)
        : await s.readPublic(id);
      if (action === "feedback") return reply(404, { error: "Not found" });
      if (action === "download")
        return reply(200, game.bundle, {
          "Content-Type": "application/vnd.aesthetic.oskie+json",
          "Content-Disposition": `attachment; filename="${game.bundle.manifest.slug}.oskie"`,
        });
      return reply(200, game);
    } catch (e) {
      return reply(e.status || 503, {
        error: e.status ? e.message : "Game unavailable.",
      });
    }
  };
}
export const handler = createGameAPI();
