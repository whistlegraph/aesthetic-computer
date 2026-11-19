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

// A piece written on an AC OS machine (`platform: "ac-native"` in its `pieces`
// row) is written against the OS's sound and input and does not run in a
// browser. The web is given this notice in its place; `?source` on the media
// URL still returns the piece itself.
export const NATIVE_PLATFORM = "ac-native";
export function nativePiecePath(subPath) {
  const match = /^piece\/([a-zA-Z0-9_-]{1,100})\.mjs$/.exec(String(subPath || ""));
  return match ? { slug: match[1] } : null;
}
export function nativeNoticeModule({ handle, slug }) {
  const name = JSON.stringify(String(slug));
  const author = JSON.stringify(String(handle).startsWith("@") ? String(handle) : `@${handle}`);
  return `// ${slug}, by ${String(handle).replace(/[^@a-zA-Z0-9_.-]/g, "")} — an AC OS piece.
// This notice stands in for it on the web. The piece itself: add ?source to this URL.
const NAME = ${name};
const AUTHOR = ${author};
export const platform = "ac-native";
export function paint({ wipe, ink, write, screen }) {
  wipe(0, 0, 0);
  const y = Math.floor(screen.height / 2) - 26;
  ink(57, 255, 20);
  write(NAME, { center: "x", y });
  ink(255, 255, 255);
  write("by " + AUTHOR, { center: "x", y: y + 14 });
  ink(150, 150, 150);
  write("an AC OS piece", { center: "x", y: y + 34 });
  write("it runs on Aesthetic Computer OS", { center: "x", y: y + 46 });
  return false;
}
export function meta() {
  return { title: NAME + " · " + AUTHOR, desc: "A piece for Aesthetic Computer OS." };
}
`;
}
