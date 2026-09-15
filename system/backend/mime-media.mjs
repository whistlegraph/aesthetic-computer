// Public AC media are mime's opening posts. Only replies and thread activity
// are stored separately; source records remain authoritative for visibility.
import { ObjectId } from "mongodb";

export const MEDIA_KINDS = {
  painting: "paintings",
  tape: "tapes",
  piece: "pieces",
  kidlisp: "kidlisp",
};
export const MEDIA_THREADS = "mime-media-threads";

const visible = {
  nuked: { $ne: true },
  deleted: { $ne: true },
  private: { $ne: true },
  draft: { $ne: true },
  hidden: { $ne: true },
  visibility: { $in: [null, "public"] },
};
const nonempty = { $type: "string", $ne: "" };
const literal = (value) => ({ $literal: value });
const pieceExtension = { $toLower: { $ifNull: ["$extension", { $ifNull: ["$ext", "mjs"] }] } };

export function mediaPipeline(kind, match = {}, activity = true) {
  if (!Object.hasOwn(MEDIA_KINDS, kind)) throw new Error("Unknown media kind");
  const type = kind === "painting" ? literal("image/png")
    : kind === "kidlisp" ? literal("text/x-lisp")
    : kind === "tape" ? { $cond: [
      { $and: [{ $eq: ["$mp4Status", "complete"] }, { $ne: [{ $ifNull: ["$mp4Url", ""] }, ""] }] },
      "video/mp4", { $cond: [{ $eq: ["$kind", "mp4"] }, "video/mp4", "application/zip"] },
    ] }
    : { $switch: {
      branches: [
        { case: { $in: [pieceExtension, ["lisp", ".lisp"]] }, then: "text/x-lisp" },
        { case: { $in: [pieceExtension, ["lua", ".lua"]] }, then: "text/x-lua" },
      ], default: "text/javascript",
    } };
  const pipeline = [
    { $match: { ...visible, ...(kind === "kidlisp" ? { source: nonempty } : { slug: nonempty }), ...match } },
    { $project: {
      _id: 0,
      code: { $concat: [kind + "_", { $toString: "$_id" }] },
      parent: literal(null), board: type,
      when: { $ifNull: ["$when", { $toDate: "$_id" }] },
      text: literal(""),
      _media: {
        kind: literal(kind), id: { $toString: "$_id" }, code: "$code",
        user: "$user", slug: "$slug", size: "$size",
      },
    } },
  ];
  if (activity) pipeline.push(
    { $lookup: { from: MEDIA_THREADS, localField: "code", foreignField: "_id", as: "_activity" } },
    { $set: {
      replies: { $ifNull: [{ $arrayElemAt: ["$_activity.replies", 0] }, 0] },
      bumped: { $ifNull: [{ $arrayElemAt: ["$_activity.bumped", 0] }, "$when"] },
    } },
    { $unset: "_activity" },
  );
  else pipeline.push({ $set: { replies: literal(0), bumped: "$when" } });
  return pipeline;
}

export function parseMediaThread(code) {
  if (typeof code !== "string") return null;
  const match = /^(painting|tape|piece|kidlisp)_([a-f0-9]{24})$/.exec(code);
  return match ? { kind: match[1], id: new ObjectId(match[2]) } : null;
}

export async function mediaThread(db, kind, match) {
  if (!Object.hasOwn(MEDIA_KINDS, kind)) return null;
  return (await db.collection(MEDIA_KINDS[kind]).aggregate(mediaPipeline(kind, match)).toArray())[0] || null;
}

export async function resolveMedia(db, kind, ref) {
  if (typeof ref !== "string" || !ref) return null;
  // Prefer a public short code. An ObjectId is the fallback for legacy records.
  return await mediaThread(db, kind, { code: ref }) ||
    (/^[a-f0-9]{24}$/.test(ref) ? mediaThread(db, kind, { _id: new ObjectId(ref) }) : null);
}

export async function sourceRecord(db, code) {
  const ref = parseMediaThread(code);
  if (!ref) return null;
  return db.collection(MEDIA_KINDS[ref.kind]).findOne({ ...visible, _id: ref.id });
}

export async function publicPosts(db, docs) {
  const users = [...new Set(docs.map((doc) => doc._media?.user || doc.user).filter(Boolean))];
  const handles = users.length ? await db.collection("@handles")
    .find({ _id: { $in: users } }, { projection: { handle: 1 } }).toArray() : [];
  const byUser = new Map(handles.filter((h) => typeof h.handle === "string")
    .map((h) => [h._id, "@" + h.handle.replace(/^@/, "")]));
  return docs.map((doc) => {
    const media = doc._media;
    const handle = byUser.get(media?.user || doc.user) || null;
    const extension = {
      "image/png": "png", "video/mp4": "mp4", "application/zip": "zip",
      "text/javascript": "mjs", "text/x-lisp": "lisp", "text/x-lua": "lua",
    }[doc.board];
    const url = `/api/mime?file=${encodeURIComponent(doc.code)}`;
    const file = media
      ? { name: `${media.code || media.id}.${extension}`, type: doc.board, size: media.size ?? null, url }
      : doc.file ? { name: doc.file.name, type: doc.file.type, size: doc.file.size, url } : null;
    let original;
    if (media?.code) {
      const code = encodeURIComponent(media.code);
      original = media.kind === "painting" ? `https://aesthetic.computer/#${code}`
        : media.kind === "tape" ? `https://aesthetic.computer/!${code}`
        : media.kind === "kidlisp" ? `https://aesthetic.computer/$${code}`
        : `https://aesthetic.computer/${code}`;
    }
    return {
      code: doc.code, parent: doc.parent, board: doc.board,
      name: media || doc.user ? handle : doc.name, text: doc.text, when: doc.when,
      replies: doc.replies || 0, file,
      ...(media ? { media: {
        kind: media.kind, id: media.id, code: media.code || null,
        url: original || url, author: handle,
      } } : {}),
    };
  });
}

// Resolve storage only from an allowlisted, visible media record. Request
// bodies cannot supply external URLs or choose arbitrary collections/buckets.
export function mediaFile(kind, record) {
  if ((kind === "kidlisp" || kind === "piece") && typeof record.source === "string") {
    return { text: record.source };
  }
  if (kind === "tape" && record.mp4Status === "complete" && record.mp4Url) {
    const url = new URL(record.mp4Url);
    if (url.protocol !== "https:" || url.username || url.password || ![
      "art-aesthetic-computer.sfo3.digitaloceanspaces.com",
      "user-aesthetic-computer.sfo3.digitaloceanspaces.com",
      "at-blobs-aesthetic-computer.sfo3.digitaloceanspaces.com",
      "art.aesthetic.computer", "user.aesthetic.computer", "at-blobs.aesthetic.computer",
    ].includes(url.hostname)) throw new Error("Unsupported media origin");
    return { url: url.href };
  }
  const bucket = record.bucket || (record.user ? "user-aesthetic-computer" : "art-aesthetic-computer");
  if (!["art-aesthetic-computer", "user-aesthetic-computer"].includes(bucket)) {
    throw new Error("Unsupported media bucket");
  }
  const ext = kind === "painting" ? "png" : kind === "tape" ? (record.kind === "mp4" ? "mp4" : "zip")
    : String(record.extension || record.ext || "mjs").replace(/^\./, "");
  const slug = kind === "painting" ? record.slug.split(":")[0] : record.slug;
  const key = record.user && !slug.startsWith(`${record.user}/`) ? `${record.user}/${slug}` : slug;
  if (key.split("/").some((part) => part === ".." || part === ".")) throw new Error("Invalid media path");
  return { url: `https://${bucket}.sfo3.digitaloceanspaces.com/${key.split("/").map(encodeURIComponent).join("/")}.${encodeURIComponent(ext)}` };
}
