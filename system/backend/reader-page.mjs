// reader-page — what a machine reader is served instead of the runtime.
//
// AI crawlers (Meta's agent, OpenAI's search bot and the rest) ask for a piece
// the way a browser does, but a piece is a canvas: after booting the whole
// runtime there is no text on the page to take away. In three days of November
// 2026 Meta's crawler booted 10,000 times and learned a title and one line.
// So a reader gets a plain page with no scripts: what the piece or painting
// is, who made it, the picture itself, and links to walk on from.
//
// The same facts dress the normal page's title, description and preview image
// (`paintingMeta`), so a person's link preview says the same thing.

import { connect } from "./database.mjs";
import { paintingWipExpired } from "./painting-wips.mjs";

// Self-named AI readers. Search engines (Googlebot, Bingbot) are left on the
// normal page, which already carries the hidden #llm-context article.
const READERS = [
  ["Meta", /meta-externalagent|meta-webindexer|meta-externalfetcher/i],
  ["OpenAI", /GPTBot|OAI-SearchBot|ChatGPT-User/i],
  ["Anthropic", /ClaudeBot|Claude-User|Claude-SearchBot|anthropic-ai/i],
  ["Perplexity", /PerplexityBot|Perplexity-User/i],
  ["Mistral", /MistralAI-User/i],
  ["DuckAssist", /DuckAssistBot/i],
];

// The reader family a request names itself as, or null for everyone else.
export function aiReader(headers = {}) {
  const ua = headers["user-agent"] || headers["User-Agent"] || "";
  if (!ua) return null;
  return READERS.find(([, pattern]) => pattern.test(ua))?.[0] || null;
}

// A painting's short code from a page slug: `painting~ucI` or `painting~#ucI`
// (index.mjs redirects `painting/ucI` there). Timestamps and @handles are not
// codes.
export function paintingCodeFromSlug(slug = "") {
  const match = /^painting~#?([A-Za-z0-9]{3,12})$/.exec(slug);
  return match ? match[1] : null;
}

// A user painting's slug opens with the moment it was made (2023.8.17.18.58…).
function madeOn(slug = "") {
  const match = /^(\d{4})\.(\d{1,2})\.(\d{1,2})\b/.exec(slug);
  if (!match) return null;
  const [, y, m, d] = match.map(Number);
  if (m < 1 || m > 12 || d < 1 || d > 31) return null;
  const month = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"][m - 1];
  return { text: `${d} ${month} ${y}`, iso: `${y}-${String(m).padStart(2, "0")}-${String(d).padStart(2, "0")}` };
}

// Look a painting up by code. Null when there is none, it was removed, or
// the database does not answer — the page then stands on its defaults.
export async function paintingFacts(code) {
  let database;
  try {
    database = await connect();
    const painting = await database.db.collection("paintings").findOne({ code },
      { projection: { _id: 0, slug: 1, code: 1, user: 1, nuked: 1, status: 1, "wip.steps": 1, "wip.expiresAt": 1 } });
    if (!painting || painting.nuked || paintingWipExpired(painting)) return null;
    let handle = null;
    if (painting.user) {
      const owner = await database.db.collection("@handles").findOne({ _id: painting.user }, { projection: { _id: 0, handle: 1 } });
      handle = owner?.handle || null;
    }
    const slug = String(painting.slug || "").split(":")[0]; // drop a recording code
    if (!slug) return null;
    return {
      code: painting.code, handle, made: madeOn(slug),
      // Addressed by code, scaled up: a pixel painting is small, and the
      // by-handle path needs the "@" a slug alone does not carry.
      image: `https://aesthetic.computer/api/pixel/2048:conform/${painting.code}.png`,
      url: `https://aesthetic.computer/painting~${painting.code}`,
    };
  } catch (err) {
    console.warn("📖 painting facts unavailable:", err?.message || err);
    return null;
  } finally {
    try { await database?.disconnect(); } catch (_e) { /* already closed */ }
  }
}

// The facts as page metadata (`meta.title`, `meta.desc`, `meta.image_url`).
export function paintingMeta(facts) {
  if (!facts) return null;
  const by = facts.handle ? ` by @${facts.handle}` : "";
  return {
    title: `Painting #${facts.code}${by}`,
    desc: `A pixel painting${by}${facts.made ? `, made ${facts.made.text}` : ""} on Aesthetic Computer.`,
    image_url: facts.image,
  };
}

const esc = (value) => String(value ?? "").replace(/[&<>"']/g, (c) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;", "'": "&#39;" })[c]);

// The page itself. `piece` is the bare piece name; `painting` is paintingFacts.
export function readerPage({ title, desc, canonical, image, piece, painting = null, handle = null }) {
  const url = painting?.url || canonical;
  const author = painting?.handle || handle;
  const links = [
    author && [`https://aesthetic.computer/@${author}`, `@${author} on Aesthetic Computer`],
    painting && ["https://aesthetic.computer/paintings", "More paintings"],
    ["https://aesthetic.computer/list", "Every piece, by name"],
    ["https://aesthetic.computer/prompt", "The prompt: type a piece name to open it"],
    ["https://aesthetic.computer/llms.txt", "Aesthetic Computer for language models (llms.txt)"],
    ["https://tangled.org/aesthetic.computer/core", "Source code"],
  ].filter(Boolean);
  const ld = {
    "@context": "https://schema.org",
    "@type": painting ? "VisualArtwork" : "WebApplication",
    name: title, description: desc, url,
    ...(image ? { image } : {}),
    ...(painting ? { artform: "Pixel painting", artMedium: "Digital" } : { applicationCategory: "Creative Coding", operatingSystem: "Web Browser" }),
    ...(painting?.made ? { dateCreated: painting.made.iso } : {}),
    ...(author ? { creator: { "@type": "Person", name: `@${author}`, url: `https://aesthetic.computer/@${author}` } } : {}),
    isPartOf: { "@type": "WebSite", name: "Aesthetic Computer", url: "https://aesthetic.computer" },
  };
  return `<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8" />
<meta name="viewport" content="width=device-width, initial-scale=1" />
<title>${esc(title)}</title>
<meta name="description" content="${esc(desc)}" />
<link rel="canonical" href="${esc(url)}" />
<meta property="og:type" content="${painting ? "article" : "website"}" />
<meta property="og:site_name" content="Aesthetic Computer" />
<meta property="og:title" content="${esc(title)}" />
<meta property="og:description" content="${esc(desc)}" />
<meta property="og:url" content="${esc(url)}" />
${image ? `<meta property="og:image" content="${esc(image)}" />\n<meta name="twitter:card" content="summary_large_image" />\n<meta name="twitter:image" content="${esc(image)}" />` : ""}
<script type="application/ld+json">${JSON.stringify(ld).replace(/</g, "\\u003c")}</script>
</head>
<body>
<main>
<h1>${esc(title)}</h1>
<p>${esc(desc)}</p>
${image ? `<p><img src="${esc(image)}" alt="${esc(title)}" /></p>` : ""}
${painting ? `<p>Open this painting to view it, replay how it was drawn, or print it as a sticker: <a href="${esc(url)}">${esc(url)}</a></p>` : `<p>Open and play this piece in a browser: <a href="${esc(url)}">${esc(url)}</a></p>`}
<h2>About Aesthetic Computer</h2>
<p>Aesthetic Computer is a mobile-first runtime and social network for creative computing: paintings, instruments, games and tools called pieces, each at its own address. People open a piece by typing its name at the prompt, and write new ones in JavaScript or KidLisp.</p>
<ul>
${links.map(([href, text]) => `<li><a href="${esc(href)}">${esc(text)}</a></li>`).join("\n")}
</ul>
</main>
</body>
</html>
`;
}
