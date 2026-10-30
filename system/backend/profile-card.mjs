import { MEDIA_KINDS, MEDIA_THREADS, mediaPipeline, publicPosts } from "./mime-media.mjs";

export const PROFILE_KINDS = ["mime", ...Object.keys(MEDIA_KINDS)];
export const PROFILE_PAGE_SIZE = 12;

export const normalizeProfileHandle = (value) => {
  const handle = String(value || "").replace(/^@/, "");
  return /^[a-z0-9][a-z0-9_.-]{0,63}$/i.test(handle) ? handle.toLowerCase() : null;
};

async function findHandle(db, handle) {
  const handles = db.collection("@handles");
  const projection = { projection: { handle: 1, colors: 1 } };
  // Legacy case variants coexist: the canonical exact record wins.
  return await handles.findOne({ handle }, projection) || await handles.findOne({
    handle: { $regex: `^${handle.replaceAll(".", "\\.")}$`, $options: "i" },
  }, projection);
}

// Only public counts and one latest painting. Never load whole collections or
// expose account IDs, private usage events, or unpublished media.
export async function readProfileCard(db, { handle, membership = false, readMembership }) {
  const record = await findHandle(db, handle);
  if (!record) return null;
  const user = record._id;
  if (membership) return readMembership(user);
  const moodMatch = { user, deleted: { $ne: true }, private: { $ne: true },
    hidden: { $ne: true }, nuked: { $ne: true }, draft: { $ne: true },
    visibility: { $in: [null, "public"] } };
  const paintingPipeline = mediaPipeline("painting", { user }, false);
  paintingPipeline.splice(1, 0, { $sort: { when: -1, _id: -1 } }, { $limit: 1 });
  const [moods, moodCount, counts, uploads, painting] = await Promise.all([
    db.collection("moods").find(moodMatch, { projection: { _id: 0, mood: 1, when: 1 } })
      .sort({ when: -1 }).limit(1).toArray(),
    db.collection("moods").countDocuments(moodMatch),
    Promise.all(Object.entries(MEDIA_KINDS).map(async ([kind, collection]) => [kind,
      await db.collection(collection).countDocuments(mediaPipeline(kind, { user }, false)[0].$match)])),
    // Direct mime uploads are the only posts not derived from a media record.
    db.collection("mimechan").countDocuments({ user, parent: null }),
    db.collection("paintings").aggregate(paintingPipeline).toArray(),
  ]);
  const [latestPainting] = await publicPosts(db, painting);
  const media = Object.fromEntries(counts);
  return {
    handle: "@" + record.handle.replace(/^@/, ""), colors: record.colors || null,
    mood: moods[0] ? { text: String(moods[0].mood || "").replaceAll("\\n", "\n"), when: moods[0].when } : null,
    counts: { ...media, mood: moodCount, mime: uploads + Object.values(media).reduce((a, b) => a + b, 0) },
    latestPainting: latestPainting || null,
    // Presence is live, but no historical per-account duration is recorded.
    onlineTimeMs: null,
  };
}

// One page of a handle's public posts as mime shows them, newest first.
// `mime` is everything commentable: direct uploads plus every media kind.
export async function readProfilePosts(db, { handle, kind, page }) {
  const record = await findHandle(db, handle);
  if (!record) return null;
  const user = record._id;
  const order = [{ $sort: { when: -1, code: 1 } }, { $skip: page * PROFILE_PAGE_SIZE }, { $limit: PROFILE_PAGE_SIZE + 1 }];
  const docs = kind === "mime"
    ? await db.collection("mimechan").aggregate([
      { $match: { parent: null, user } },
      { $project: { "file.data": 0 } },
      ...Object.entries(MEDIA_KINDS).map(([k, coll]) => ({ $unionWith: { coll, pipeline: mediaPipeline(k, { user }, false) } })),
      ...order,
    ]).toArray()
    : await db.collection(MEDIA_KINDS[kind]).aggregate([...mediaPipeline(kind, { user }, false), ...order]).toArray();
  const hasMore = docs.length > PROFILE_PAGE_SIZE;
  const shown = docs.slice(0, PROFILE_PAGE_SIZE);
  const ids = shown.filter((doc) => doc._media).map((doc) => doc.code);
  const activity = ids.length ? await db.collection(MEDIA_THREADS)
    .find({ _id: { $in: ids } }, { projection: { replies: 1 } }).toArray() : [];
  const replies = new Map(activity.map((thread) => [thread._id, thread.replies]));
  for (const doc of shown) if (doc._media) doc.replies = replies.get(doc.code) || 0;
  return { handle: "@" + record.handle.replace(/^@/, ""), kind, page, hasMore, posts: await publicPosts(db, shown) };
}
