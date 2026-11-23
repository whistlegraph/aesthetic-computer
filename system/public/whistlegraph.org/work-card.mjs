const cdn = "https://assets.aesthetic.computer/whistlegraph";
const mediaCache = new Map();

async function featuredTake(work) {
  if (work.slug) {
    const folder = cdn + "/" + encodeURIComponent(work.slug) + "/" + encodeURIComponent(work.slug);
    return { media: "video", src: folder + "-web.mp4", thumb: folder + ".webp" };
  }
  const query = new URLSearchParams(work.featuredPost
    ? { action: "post", id: work.featuredPost }
    : { action: "work", code: work.code, limit: 1 });
  const response = await fetch("/api/whistlegraph-query?" + query);
  if (!response.ok) throw new Error("Take unavailable");
  const data = await response.json();
  return work.featuredPost ? data.item : data.posts?.[0];
}

export function workCard(work) {
  const card = document.createElement("article");
  card.className = "work-card";
  const caption = document.createElement("div");
  caption.className = "work-caption";
  const link = document.createElement("a");
  link.href = "/" + work.code;
  link.textContent = work.title;
  const by = document.createElement("small");
  by.textContent = [work.by, work.year].filter(Boolean).join(" · ");
  caption.append(link, by);
  card.append(caption);
  if (!mediaCache.has(work.code)) {
    mediaCache.set(work.code, featuredTake(work).catch(() => { mediaCache.delete(work.code); }));
  }
  mediaCache.get(work.code).then(take => {
    if (!take?.src || !card.isConnected) return;
    const player = document.createElement(take.media === "audio" ? "audio" : "video");
    if (player.tagName === "VIDEO") {
      player.playsInline = true;
      if (take.thumb || work.thumb) player.poster = take.thumb || work.thumb;
    }
    player.preload = "none";
    player.controls = true;
    player.setAttribute("aria-label", "Play " + work.title);
    player.src = take.src;
    card.prepend(player);
  });
  return card;
}
