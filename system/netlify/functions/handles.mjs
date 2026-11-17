// handles, 2025.10.16
// GET endpoint to list/search handles with autocomplete support

import { respond } from "../../backend/http.mjs";
import { connect } from "../../backend/database.mjs";

export async function handler(event) {
  if (event.httpMethod !== "GET") {
    return respond(405, { error: "Method not allowed" });
  }

  // order=created lists handles oldest first (dool gives the tower's floors
  // out in that order); the default is alphabetical.
  const { search, limit = "100", tenant = "aesthetic", order } = event.queryStringParameters || {};
  const created = order === "created";

  try {
    const database = await connect();
    const collection = database.db.collection("@handles");

    let query = {};

    // Filter by tenant (aesthetic vs sotce)
    if (tenant === "sotce") {
      query._id = { $regex: "^sotce-" };
    } else if (tenant === "aesthetic") {
      query._id = { $not: { $regex: "^sotce-" } };
    }

    // Add search filter if provided
    if (search) {
      query.handle = { $regex: `^${search}`, $options: "i" };
    }

    let handles;
    if (created) {
      // Records from before 2026-10-03 carry no createdAt. An Auth0 database
      // account's id holds the second it was made (the first eight hex digits
      // after "auth0|"), which stands in; accounts with neither follow the
      // dated old ones, by name, ahead of everything stamped since.
      const STAMPED = Date.UTC(2026, 9, 3);
      const born = (doc) => {
        if (doc.createdAt) return new Date(doc.createdAt).getTime();
        const id = /auth0\|([0-9a-f]{8})[0-9a-f]{16}$/.exec(String(doc._id));
        const at = id ? parseInt(id[1], 16) * 1000 : NaN;
        return at > Date.UTC(2015, 0, 1) && at < STAMPED ? at : STAMPED - 1;
      };
      const name = (doc) => (typeof doc.handle === "string" ? doc.handle : doc.handle?.handle || "");
      handles = (await collection.find(query, { projection: { handle: 1, createdAt: 1 } }).toArray())
        .map((doc) => ({ doc, at: born(doc) }))
        .sort((a, b) => a.at - b.at || (name(a.doc) < name(b.doc) ? -1 : 1))
        .slice(0, parseInt(limit))
        .map((x) => x.doc);
    } else {
      handles = await collection
        .find(query, { projection: { handle: 1, _id: 0 } })
        .limit(parseInt(limit))
        .sort({ handle: 1 })
        .toArray();
    }

    await database.disconnect();

    return respond(200, {
      handles: handles
        .map((doc) => {
          const h = doc.handle;
          return typeof h === "string" ? h : (h?.handle || null);
        })
        .filter((h) => h && typeof h === "string"), // Filter out null/undefined/non-strings
      count: handles.length,
      ...(created ? { order: "created" } : {}),
    });
  } catch (error) {
    console.error("Error fetching handles:", error);
    return respond(500, { error: "Failed to fetch handles" });
  }
}
