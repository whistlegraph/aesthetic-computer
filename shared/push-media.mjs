// push-media, 2026.11.18
// The picture a notification carries. A push with an `image` shows a thumbnail
// on the banner and opens to the full image, GIF or clip (the iOS app's
// notification service extension, apple/notify, attaches it; Web Push shows it
// where the browser can). The picture comes from what the message points at,
// and when it points at nothing, from the sender: their newest painting stands
// in as their face, so it changes whenever they paint.

const SITE = "https://aesthetic.computer";
const OVEN = "https://oven.aesthetic.computer";

// A message's own media, or null. Pure: no network, no database.
//   #code          a painting
//   $code          a KidLisp piece, as the oven's still
//   image links    the image itself
//   YouTube links  the video's thumbnail
export function mediaInText(text) {
  const words = String(text || "");
  const link = words.match(/https:\/\/[^\s<>"']+/i)?.[0];
  if (link) {
    const video = link.match(/(?:youtube\.com\/(?:watch\?(?:[^\s#]*&)?v=|shorts\/|embed\/)|youtu\.be\/)([A-Za-z0-9_-]{11})/);
    if (video) return `https://i.ytimg.com/vi/${video[1]}/hqdefault.jpg`;
    if (/\.(png|jpe?g|gif|webp)(\?[^\s]*)?$/i.test(link)) return link;
  }
  // A sigil counts only at the start of a word and needs three characters or
  // more, so "$5" is not a code. "#fff" still reads as one; a code that names
  // nothing just fails to load and the notification goes out plain.
  const painting = words.match(/(?:^|\s)#([A-Za-z0-9]{3,12})(?=$|[\s.,!?])/);
  if (painting) return `${SITE}/media/paintings/${painting[1]}.png`;
  const kidlisp = words.match(/(?:^|\s)\$([A-Za-z0-9]{3,12})(?=$|[\s.,!?])/);
  if (kidlisp) return `${OVEN}/grab/png/512/512/${encodeURIComponent("$" + kidlisp[1])}`;
  return null;
}

// The sender's newest public painting, or null.
export async function faceFor(db, handle) {
  const name = String(handle || "").replace(/^@/, "");
  if (!db || !/^[a-z0-9][a-z0-9_.-]{0,63}$/i.test(name)) return null;
  // Legacy case variants coexist: the exact record wins.
  const handles = db.collection("@handles"), only = { projection: { _id: 1 } };
  const owner = await handles.findOne({ handle: name }, only) || await handles.findOne(
    { handle: { $regex: `^${name.replaceAll(".", "\\.")}$`, $options: "i" } }, only);
  if (!owner) return null;
  const [painting] = await db.collection("paintings")
    .find({
      user: owner._id, code: { $type: "string", $ne: "" },
      status: { $ne: "wip" }, nuked: { $ne: true }, deleted: { $ne: true }, private: { $ne: true },
      draft: { $ne: true }, hidden: { $ne: true }, visibility: { $in: [null, "public"] },
    }, { projection: { code: 1 } })
    .sort({ when: -1, _id: -1 }).limit(1).toArray();
  return painting ? `${SITE}/media/paintings/${painting.code}.png` : null;
}

// Never hold a message back for its picture: a slow lookup sends it plain.
export async function mediaFor(db, handle, text, { wait = 1200 } = {}) {
  const own = mediaInText(text);
  if (own) return own;
  let timer;
  try {
    return await Promise.race([
      faceFor(db, handle),
      new Promise((resolve) => { timer = setTimeout(resolve, wait, null); }),
    ]);
  } catch {
    return null;
  } finally {
    clearTimeout(timer);
  }
}
