// Legacy records may store a bare timestamp, a folder-relative slug, or the
// complete account key (including chat uploads). Preserve the stored folder.
export function paintingStorageUrl(painting) {
  const slug = String(painting.slug || "").split(":")[0];
  if (!slug || slug.split("/").some((part) => !part || part === "." || part === "..")) {
    throw new Error("Invalid painting slug");
  }
  const user = painting.user;
  const key = user && !slug.startsWith(`${user}/`)
    ? `${user}/${slug.includes("/") ? "" : "painting/"}${slug}` : slug;
  const bucket = user ? "user-aesthetic-computer" : "art-aesthetic-computer";
  return `https://${bucket}.sfo3.digitaloceanspaces.com/${key.split("/").map(encodeURIComponent).join("/")}.png`;
}
