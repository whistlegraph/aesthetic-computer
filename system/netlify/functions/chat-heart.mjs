// chat-heart, 26.02.25
// POST: Toggle a heart on any content item { for, type }.
// Returns { for, type, hearted, count }.

import { authorize } from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";

async function toggleHeart(db, { user, type, for: forId }) {
  const collection = db.collection("hearts");

  await collection.createIndex(
    { type: 1, for: 1, user: 1 },
    { unique: true, background: true, name: "hearts_type_for_user_unique" },
  );
  await collection.createIndex(
    { type: 1, for: 1 },
    { background: true, name: "hearts_type_for" },
  );

  let hearted;
  try {
    await collection.insertOne({ user, type, for: forId, when: new Date() });
    hearted = true;
  } catch (err) {
    if (err.code === 11000) {
      await collection.deleteOne({ user, type, for: forId });
      hearted = false;
    } else {
      throw err;
    }
  }

  const count = await collection.countDocuments({ type, for: forId });
  return { hearted, count };
}

export async function handler(event) {
  if (event.httpMethod === "OPTIONS") return respond(204, null);
  if (event.httpMethod !== "POST")
    return respond(405, { message: "Method Not Allowed" });

  try {
    const body = JSON.parse(event.body || "{}");
    const { for: forId, type } = body;

    if (!forId || !type)
      return respond(400, { message: "Missing `for` or `type`." });

    const user = await authorize(event.headers);
    if (!user?.sub) return respond(401, { message: "Unauthorized." });

    const database = await connect();
    const result = await toggleHeart(database.db, {
      user: user.sub,
      type,
      for: forId,
    });
    await database.disconnect();

    return respond(200, { for: forId, type, ...result });
  } catch (err) {
    return respond(500, { message: err.message });
  }
}
