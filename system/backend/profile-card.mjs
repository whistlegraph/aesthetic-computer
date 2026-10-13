import { MEDIA_KINDS, mediaPipeline, publicPosts } from "./mime-media.mjs";

export const normalizeProfileHandle = (value) => {
  const handle = String(value || "").replace(/^@/, "");
  return /^[a-z0-9][a-z0-9_.-]{0,63}$/i.test(handle) ? handle.toLowerCase() : null;
};

// Only public counts and one latest painting. Never load whole collections or
// expose account IDs, private usage events, or unpublished media.
export async function readProfileCard(db, { handle, membership = false, readMembership }) {
  const handles = db.collection("@handles");
  const projection = { projection: { handle: 1, colors: 1 } };
  // Legacy case variants coexist: the canonical exact record wins.
  const record = await handles.findOne({ handle }, projection) || await handles.findOne({
    handle: { $regex: `^${handle.replaceAll(".", "\\.")}$`, $options: "i" },
  }, projection);
  if (!record) return null;
  const user = record._id;
  if (membership) return readMembership(user);
  const moodMatch = { user, deleted: { $ne: true }, private: { $ne: true },
    hidden: { $ne: true }, nuked: { $ne: true }, draft: { $ne: true },
    visibility: { $in: [null, "public"] } };
  const paintingPipeline = mediaPipeline("painting", { user }, false);
  paintingPipeline.splice(1, 0, { $sort: { when: -1, _id: -1 } }, { $limit: 1 });
  const [moods, moodCount, counts, painting] = await Promise.all([
    db.collection("moods").find(moodMatch, { projection: { _id: 0, mood: 1, when: 1 } })
      .sort({ when: -1 }).limit(1).toArray(),
    db.collection("moods").countDocuments(moodMatch),
    Promise.all(Object.entries(MEDIA_KINDS).map(async ([kind, collection]) => [kind,
      await db.collection(collection).countDocuments(mediaPipeline(kind, { user }, false)[0].$match)])),
    db.collection("paintings").aggregate(paintingPipeline).toArray(),
  ]);
  const [latestPainting] = await publicPosts(db, painting);
  return {
    handle: "@" + record.handle.replace(/^@/, ""), colors: record.colors || null,
    mood: moods[0] ? { text: String(moods[0].mood || "").replaceAll("\\n", "\n"), when: moods[0].when } : null,
    counts: { ...Object.fromEntries(counts), mood: moodCount },
    latestPainting: latestPainting || null,
    // Presence is live, but no historical per-account duration is recorded.
    onlineTimeMs: null,
  };
}
