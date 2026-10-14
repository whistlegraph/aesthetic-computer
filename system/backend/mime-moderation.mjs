// MIME moderation, 26.10.13
// What App Store guideline 1.2 asks of a feed of other people's work: a way to
// report a card, a way to hide its author, and a moderator who acts on a
// report within a day (take the card down, keep its author out of the feed).
//
// Pure functions over Mongo collections, tested in mime-moderation.test.mjs.
// The HTTP doors are in netlify/functions/mime.mjs; the feed engine reads
// `removals` when it refreshes its pool.

export const REPORT_COLLECTION = "mime-reports";
export const REMOVED_COLLECTION = "mime-removed";
export const REASONS = ["Harassment or hate", "Sexual or violent", "Spam or a scam", "Something else", "hidden"];

const hex = (n) => [...crypto.getRandomValues(new Uint8Array(n))].map((b) => b.toString(16).padStart(2, "0")).join("");

// A viewer reports a card. Signing in is not required to watch MIME, so a
// reporter is an AC account or an app install. One open report per reporter
// per card; the card is copied in so the moderator sees what was reported
// even after it changes or is gone.
export async function fileReport(reports, { code, reason, reporter, handle, post, now = new Date(), id = () => hex(8) }) {
  if (typeof code !== "string" || !/^[A-Za-z0-9_~.-]{1,96}$/.test(code)) throw Error("Bad card");
  if (!post) throw Error("That card is gone");
  if (typeof reporter !== "string" || !reporter) throw Error("Who is reporting?");
  const why = REASONS.includes(reason) ? reason : String(reason ?? "").trim().slice(0, 200) || "Something else";
  const existing = await reports.findOne({ code, reporter, status: "open" });
  if (existing) return { report: existing, fresh: false };
  const report = {
    _id: id(), at: now, status: "open", code, reason: why, reporter, handle: handle || null,
    post: { name: post.name || null, text: String(post.text || "").slice(0, 2000), when: post.when || null,
      kind: post.media?.kind || null, file: post.file?.url || null, url: post.media?.url || null },
  };
  await reports.insertOne(report);
  return { report, fresh: true };
}

// The moderator's decision. `remove` takes the card out of the feed for
// everyone; `ban` keeps every card by its author out.
export async function resolveReport(reports, removed, { reportID, status, remove = false, ban = false, note, by, now = new Date() }) {
  if (!["open", "actioned", "dismissed"].includes(status)) throw Error("status must be open, actioned or dismissed");
  const report = await reports.findOne({ _id: String(reportID) });
  if (!report) return null;
  const mark = { at: now.toISOString(), report: report._id, by: by || null };
  if (remove) await removed.updateOne({ _id: report.code }, { $set: { kind: "card", ...mark } }, { upsert: true });
  if (ban && report.post?.name) await removed.updateOne({ _id: report.post.name }, { $set: { kind: "author", ...mark } }, { upsert: true });
  const set = { status, resolvedAt: now, resolvedBy: by || null, removed: !!remove, banned: !!ban, ...(note ? { note: String(note).slice(0, 2000) } : {}) };
  await reports.updateOne({ _id: report._id }, { $set: set });
  return { ...report, ...set };
}

// What the feed leaves out: removed card codes and banned @handles.
export async function removals(removed) {
  const rows = await removed.find({}).toArray().catch(() => []);
  return {
    cards: new Set(rows.filter((r) => r.kind === "card").map((r) => r._id)),
    authors: new Set(rows.filter((r) => r.kind === "author").map((r) => r._id)),
  };
}

export const allowed = ({ cards, authors }) => (item) => !cards.has(item.code) && !authors.has(item.author);
