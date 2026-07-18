const USER_MEDIA_ORIGIN = "https://user.aesthetic.computer";
const USER_SPACES_ORIGIN =
  "https://user-aesthetic-computer.sfo3.digitaloceanspaces.com";

export function userMediaTarget({ userId, subPath, extension }) {
  const normalized = decodeURIComponent(subPath).replace(/^\/+/, "");
  const alreadyQualified =
    normalized === userId || normalized.startsWith(`${userId}/`);
  const key = alreadyQualified ? normalized : `${userId}/${normalized}`;
  const origin = extension === "mjs" ? USER_SPACES_ORIGIN : USER_MEDIA_ORIGIN;
  const encoded = key.split("/").map(encodeURIComponent).join("/");
  return `${origin}/${encoded}`;
}
