import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";
import { PROFILE_KINDS, normalizeProfileHandle, readProfileCard, readProfilePosts } from "../../backend/profile-card.mjs";
import { readMembership } from "../../backend/profile-membership.mjs";

export function createHandler(connectDatabase = connect, membershipReader = readMembership) {
  return async (event) => {
    if (event.httpMethod === "OPTIONS") return respond(204, "");
    if (event.httpMethod !== "GET") return respond(405, { error: "Use GET." });
    const query = event.queryStringParameters || {};
    const handle = normalizeProfileHandle(query.handle);
    const kind = query.kind;
    const page = kind ? Number(query.page ?? 0) : null;
    const listing = kind !== undefined;
    if (!handle || (query.page !== undefined && !listing) || (query.membership && query.membership !== "1")
      || (listing && (!PROFILE_KINDS.includes(kind) || query.membership || !/^\d{1,3}$/.test(String(query.page ?? 0)) || page > 500))) {
      return respond(400, { error: "Invalid profile request." });
    }
    let database;
    try {
      database = await connectDatabase();
      const result = listing
        ? await readProfilePosts(database.db, { handle, kind, page })
        : await readProfileCard(database.db, { handle, membership: query.membership === "1", readMembership: membershipReader });
      return result ? respond(200, result, { "Cache-Control": "public, max-age=15, must-revalidate" })
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
