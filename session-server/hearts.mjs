// Hearts, 26.02.25
// Toggle and count hearts for any content type.
// Used by chat-manager (session server) and the chat-heart Netlify function.

export async function ensureIndexes(db) {
  const collection = db.collection("hearts");
  try {
    await collection.createIndex(
      { type: 1, for: 1, user: 1 },
      { unique: true, background: true, name: "hearts_type_for_user_unique" },
    );
    await collection.createIndex(
      { type: 1, for: 1 },
      { background: true, name: "hearts_type_for" },
    );
  } catch {
    // Indexes already exist — safe to ignore
  }
}

// Toggle a heart. Returns { hearted: bool, count: number }.
export async function toggleHeart(db, { user, type, for: forId }) {
  const collection = db.collection("hearts");
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

// Bulk-count hearts for a set of ids within a type.
// Returns { [forId]: count } — missing ids have count 0.
export async function countHearts(db, type, forIds) {
  if (!forIds.length) return {};
  const collection = db.collection("hearts");
  const results = await collection
    .aggregate([
      { $match: { type, for: { $in: forIds } } },
      { $group: { _id: "$for", count: { $sum: 1 } } },
    ])
    .toArray();
  return Object.fromEntries(results.map((r) => [r._id, r.count]));
}
