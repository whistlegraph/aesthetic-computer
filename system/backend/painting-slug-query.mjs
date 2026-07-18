export function escapeRegex(value) {
  return `${value}`.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
}

export function userPaintingSlugQuery(slug) {
  const escaped = escapeRegex(slug);
  return {
    $or: [
      { slug },
      { slug: new RegExp(`/${escaped}$`) },
    ],
  };
}
