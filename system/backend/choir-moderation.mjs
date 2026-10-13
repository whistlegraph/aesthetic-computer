// choir moderation, 26.10.12
// What App Store guideline 1.2 asks of a room where people post: a filter for
// objectionable words, a way to report a post, a way for the moderator to act
// on a report within a day (take the post down, keep its author out), and a
// record that each member agreed to the rules at choir.ac/terms.
//
// Pure functions over Mongo collections, tested in choir-moderation.test.mjs.
// The HTTP doors are in netlify/functions/choir.mjs.

// Bump when choir.ac/terms changes in substance; the app asks again.
export const TERMS_VERSION = "2026-10-12";
export const REPORT_COLLECTION = "choir-reports";
export const REFUSED = "choir doesn't allow slurs or telling someone to hurt themselves (choir.ac/terms). Try saying it another way.";

// Phrases that tell a person to hurt themselves. The slur list is not kept in
// this public repo: lith reads it from CHOIR_BLOCKED_WORDS (comma-separated,
// vault lith/.env). Swearing is fine in choir; these are not.
const PHRASES = [/\bkill\s*(?:your|ur|yo)\s*self\b/, /\bkys\b/, /\bgo\s+die\b/];

// Lowercase, strip accents, and undo the common letter-for-number swaps.
function plain(text) {
  return String(text ?? "").normalize("NFKD").replace(/[̀-ͯ]/g, "").toLowerCase()
    .replace(/[0134578]/g, (c) => ({ 0: "o", 1: "i", 3: "e", 4: "a", 5: "s", 7: "t", 8: "b" })[c])
    // A symbol stands in for a letter only inside a word, so "wow!" stays "wow".
    .replace(/[@$!|](?=[a-z])/g, (c) => ({ "@": "a", $: "s", "!": "i", "|": "i" })[c]);
}

// A listed word matches on its own (word boundaries), with any letter
// stretched ("heyyy" spellings) and an s/z/es ending. So a listed "cat" stops
// "caaats" but never "catalog" or "scat".
function wordPattern(word) {
  const letters = [...plain(word).replace(/[^a-z]/g, "")];
  if (!letters.length) return null;
  return new RegExp(`\\b${letters.map((c) => `${c}+`).join("")}(?:e?s|z)?\\b`);
}

export function makeScreen(words = []) {
  const patterns = [...PHRASES, ...words.map(wordPattern).filter(Boolean)];
  return (...texts) => {
    for (const t of texts) {
      const p = plain(t);
      if (p && patterns.some((re) => re.test(p))) return false;
    }
    return true;
  };
}

const fromEnv = () => String(process.env.CHOIR_BLOCKED_WORDS || "").split(",").map((w) => w.trim()).filter(Boolean);
let envScreen;
export const screen = (...texts) => (envScreen ??= makeScreen(fromEnv()))(...texts);

// Everything a person can read in a post: its words, voice-note transcripts
// and file names.
export function screenPost(clean, ok = screen) {
  const extra = (clean.attachments || []).flatMap((a) => [a.transcript, a.name]);
  if (!ok(clean.text, ...extra)) throw Error(REFUSED);
  return clean;
}

const hex = (n) => [...crypto.getRandomValues(new Uint8Array(n))].map((b) => b.toString(16).padStart(2, "0")).join("");
const validID = (v) => typeof v === "string" && /^[a-f0-9]{24}$/i.test(v);

// A member reports a post. One open report per reporter per post; the post's
// words are copied in so the moderator sees what was reported even after an
// edit or a delete.
export async function fileReport(reports, posts, { postID, reason, member, handle, now = new Date(), id = () => hex(8) }) {
  if (!validID(postID)) throw Error("Bad post");
  const post = await posts.findOne({ _id: postID });
  if (!post) throw Error("That post is gone");
  if (post.owner === member) throw Error("That's your own post");
  const why = String(reason ?? "").trim().slice(0, 1000);
  const existing = await reports.findOne({ postID, member, status: "open" });
  if (existing) return { report: existing, fresh: false };
  const report = {
    _id: id(), at: now, status: "open", postID, reason: why, member, handle: handle || null,
    post: { handle: post.handle, owner: post.owner, text: post.text || "", when: post.when,
      attachments: (post.attachments || []).map((a) => a.url), replyTo: post.replyTo ? String(post.replyTo) : null },
  };
  await reports.insertOne(report);
  return { report, fresh: true };
}

// The moderator's decision. `remove` takes the post down for everyone;
// `ban` keeps its author from posting again (store.post checks `banned`).
export async function resolveReport(reports, posts, prefs, { reportID, status, remove = false, ban = false, note, by, now = new Date() }) {
  if (!["open", "actioned", "dismissed"].includes(status)) throw Error("status must be open, actioned or dismissed");
  const report = await reports.findOne({ _id: String(reportID) });
  if (!report) return null;
  if (remove) await posts.updateOne({ _id: report.postID }, { $set: { deleted: true, text: "", links: [], removedAt: now.toISOString(), removedBy: "moderator" } });
  if (ban && report.post?.owner) await prefs.updateOne({ _id: report.post.owner }, { $set: { banned: true, bannedAt: now.toISOString() } }, { upsert: true });
  const set = { status, resolvedAt: now, resolvedBy: by || null, removed: !!remove, banned: !!ban, ...(note ? { note: String(note).slice(0, 2000) } : {}) };
  await reports.updateOne({ _id: report._id }, { $set: set });
  return { ...report, ...set };
}

// A member agrees to choir.ac/terms (the app asks once per TERMS_VERSION).
export async function acceptTerms(prefs, member, { version = TERMS_VERSION, now = new Date() } = {}) {
  if (version !== TERMS_VERSION) throw Error("Those aren't the current terms");
  await prefs.updateOne({ _id: member }, { $set: { termsVersion: version, termsAcceptedAt: now.toISOString() } }, { upsert: true });
  return { termsVersion: version, current: true };
}

export async function termsOf(prefs, member) {
  const row = await prefs.findOne({ _id: member });
  return { termsVersion: row?.termsVersion || null, current: row?.termsVersion === TERMS_VERSION, banned: row?.banned === true };
}
