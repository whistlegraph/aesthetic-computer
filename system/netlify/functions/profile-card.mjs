import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";
import { normalizeProfileHandle, readProfileCard } from "../../backend/profile-card.mjs";
import { readMembership } from "../../backend/profile-membership.mjs";

export function createHandler(connectDatabase = connect, membershipReader = readMembership) {
  return async (event) => {
    if (event.httpMethod === "OPTIONS") return respond(204, "");
    if (event.httpMethod !== "GET") return respond(405, { error: "Use GET." });
    const query = event.queryStringParameters || {};
    const handle = normalizeProfileHandle(query.handle);
    if (!handle || query.kind || query.page || (query.membership && query.membership !== "1")) return respond(400, { error: "Invalid profile request." });
    let database;
    try {
      database = await connectDatabase();
      const profile = await readProfileCard(database.db, { handle, membership: query.membership === "1", readMembership: membershipReader });
      return profile ? respond(200, profile, { "Cache-Control": "public, max-age=15, must-revalidate" })
        : respond(404, { error: "Profile not found." });
    } catch (error) {
      console.error("Profile card:", error.message);
      return respond(503, { error: "Profile is unavailable. Try again." });
    } finally {
      await database?.disconnect?.();
    }
  };
}

export const handler = createHandler();
