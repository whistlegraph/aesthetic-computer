const USER_MEDIA_ORIGIN = "https://user.aesthetic.computer";
const USER_SPACES_ORIGIN =
  "https://user-aesthetic-computer.sfo3.digitaloceanspaces.com";

// Piece sources are rewritten in place on every save — Aesel autopublishes
// each one — so they are read from the storage origin, never the CDN, whose
// hour-long cache would keep serving the version before last. Media (pictures,
// sounds, papers) is written once and stays on the CDN.
export const PIECE_SOURCE_EXTENSIONS = new Set(["mjs", "lisp", "p5", "l5", "lua"]);
export const userMediaOrigin = (extension) =>
  PIECE_SOURCE_EXTENSIONS.has(String(extension || "").toLowerCase()) ? USER_SPACES_ORIGIN : USER_MEDIA_ORIGIN;

export function userMediaTarget({ userId, subPath, extension }) {
  const normalized = decodeURIComponent(subPath).replace(/^\/+/, "");
  const alreadyQualified =
    normalized === userId || normalized.startsWith(`${userId}/`);
  const key = alreadyQualified ? normalized : `${userId}/${normalized}`;
  const origin = userMediaOrigin(extension);
  const encoded = key.split("/").map(encodeURIComponent).join("/");
  return `${origin}/${encoded}`;
}
