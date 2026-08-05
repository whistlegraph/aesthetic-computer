const grid = document.querySelector("#track-grid");

function duration(seconds) {
  const whole = Math.round(Number(seconds));
  return `${Math.floor(whole / 60)}:${String(whole % 60).padStart(2, "0")}`;
}

function link(label, href) {
  const anchor = document.createElement("a");
  anchor.textContent = label;
  anchor.href = href;
  return anchor;
}

function trackCard(track) {
  const article = document.createElement("article");
  article.className = "track";

  const image = document.createElement("img");
  image.src = track.cover;
  image.alt = `${track.title} cover art`;
  article.append(image);

  const body = document.createElement("div");
  body.className = "track-body";
  const title = document.createElement("h3");
  title.textContent = track.title;
  const meta = document.createElement("p");
  meta.className = "track-meta";
  meta.textContent = `${duration(track.duration)} · ${track.bpm} BPM · ${track.meter} · ${track.key}`;
  const links = document.createElement("div");
  links.className = "track-links";
  links.append(link("open", track.piece));
  if (track.links.spotify) links.append(link("spotify", track.links.spotify));
  if (track.releaseData) links.append(link("release data", track.releaseData));
  body.append(title, meta, links);
  article.append(body);
  return article;
}

try {
  const response = await fetch("/releases/catalog.json");
  if (!response.ok) throw new Error(`catalog ${response.status}`);
  const catalog = await response.json();
  grid.replaceChildren(...catalog.tracks.map(trackCard));
} catch {
  grid.textContent = "Release catalog unavailable.";
}
