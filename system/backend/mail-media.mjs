// Mail references public AC media; files arriving by SMTP stay in the letter.
// Eight MiB of base64 stays below MongoDB's 16 MiB document limit.
export const MAX_MAIL_FILES = 10;
export const MAX_MAIL_FILE_BYTES = 8 * 1024 * 1024;
export const MAX_MAIL_WIRE_BYTES = 12 * 1024 * 1024;
const RASTER = new Set(["image/png", "image/jpeg", "image/gif", "image/webp"]);
const KINDS = { "#": "painting", "!": "tape", "$": "kidlisp" };
const COLLECTIONS = { painting: "paintings", tape: "tapes", kidlisp: "kidlisp" };
const PUBLIC = {
  nuked: { $ne: true }, deleted: { $ne: true }, private: { $ne: true },
  hidden: { $ne: true }, draft: { $ne: true }, visibility: { $in: [null, "public"] },
};

export function mediaCodes(text) {
  const found = new Map();
  // Match bare chat codes and canonical AC URLs, but not URL fragments on
  // unrelated sites, email local-parts, or pieces of longer words.
  const rx = /(?:^|[\s(\[])((?:https:\/\/aesthetic\.computer\/)?([#!$])([a-zA-Z0-9]{3,64}))(?=$|[\s)\].,;:?])/g;
  for (const match of (text || "").matchAll(rx)) {
    const label = match[2] + match[3];
    found.set(label, { kind: KINDS[match[2]], code: match[3], label });
    if (found.size === 10) break;
  }
  return [...found.values()];
}

export async function resolveMailMedia(text, database) {
  const refs = await Promise.all(mediaCodes(text).map(async (ref) => {
    const record = await database.db.collection(COLLECTIONS[ref.kind]).findOne(
      { ...PUBLIC, code: ref.code }, { projection: { _id: 1 } },
    );
    if (!record) return null; // An ordinary hashtag stays ordinary text.
    return {
      ...ref, path: ref.kind === "painting" ? `painting${ref.label}` : ref.kind === "tape" ? `video~${ref.label}` : ref.label,
      url: `https://aesthetic.computer/${ref.label}`,
      ...(ref.kind === "painting" ? { preview: `/media/paintings/${ref.code}.png` } : {}),
    };
  }));
  return refs.filter(Boolean);
}

const escapeHTML = (text) => String(text).replace(/[&<>"']/g, (c) => ({
  "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;", "'": "&#39;",
})[c]);

export function outsideMediaBody(text, media) {
  if (!media.length) return { text };
  return {
    text: `${text}\n\n${media.map((m) => `${m.label}: ${m.url}`).join("\n")}`,
    html: `<div style="white-space:pre-wrap">${escapeHTML(text)}</div>` + media.map((m) =>
      `<p><a href="${escapeHTML(m.url)}">${m.preview
        ? `<img src="https://aesthetic.computer${escapeHTML(m.preview)}" alt="${escapeHTML(m.label)}" style="max-width:100%;height:auto" width="480"><br>`
        : ""}${escapeHTML(m.label)}</a></p>`).join(""),
  };
}

export function incomingAttachments(files = []) {
  let total = 0;
  const tooLarge = () => Object.assign(new Error("At most 10 files and 8 MiB of attachments per letter"), { responseCode: 552 });
  if (files.length > MAX_MAIL_FILES) throw tooLarge();
  return files.map((file, index) => {
    const bytes = file.content;
    total += bytes.length;
    if (total > MAX_MAIL_FILE_BYTES) throw tooLarge();
    const name = String(file.filename || `file-${index + 1}`)
      .split(/[\\/]/).pop().replace(/[\x00-\x1f\x7f\u202a-\u202e\u2066-\u2069]/g, "").slice(0, 120) || `file-${index + 1}`;
    const declared = String(file.contentType || "").toLowerCase();
    const type = /^[a-z0-9!#$&^_.+-]+\/[a-z0-9!#$&^_.+-]+$/.test(declared)
      ? declared : "application/octet-stream";
    return { name, type, size: bytes.length, data: bytes.toString("base64") };
  });
}

export function attachmentList(files = []) {
  return files.map(({ name, type, size }, index) => ({ index, name, type, size, image: RASTER.has(type) }));
}

export async function attachmentThumbnail(file) {
  if (!RASTER.has(file.type)) return null;
  try {
    const sharp = (await import("sharp")).default;
    const data = await sharp(Buffer.from(file.data, "base64"), { limitInputPixels: 20_000_000 })
      .resize(320, 240, { fit: "inside", withoutEnlargement: true }).png().toBuffer();
    return { type: "image/png", data: data.toString("base64") };
  } catch {
    return null; // A bad image is still downloadable; never log its contents.
  }
}
