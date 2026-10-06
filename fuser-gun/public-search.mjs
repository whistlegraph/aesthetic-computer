// Public Instagram discovery through Exa's keyless, rate-limited MCP endpoint.
// Search evidence is not a complete feed or an authenticated Graph response.
const ENDPOINT = "https://mcp.exa.ai/mcp?tools=web_search_advanced_exa";

export async function searchInstagram(handle, limit = 25, query = `"${handle}" Instagram product launch collection`) {
  const response = await fetch(ENDPOINT, {
    method: "POST",
    headers: { "Content-Type": "application/json", Accept: "application/json, text/event-stream" },
    body: JSON.stringify({
      jsonrpc: "2.0", id: 1, method: "tools/call",
      params: { name: "web_search_advanced_exa", arguments: {
        query, includeDomains: ["instagram.com"],
        numResults: limit, textMaxCharacters: 1500, type: "auto",
      } },
    }),
    signal: AbortSignal.timeout(55_000),
  });
  if (!response.ok) throw new Error(`Exa search HTTP ${response.status}; no fixture substituted`);
  const text = await response.text();
  const messages = response.headers.get("content-type")?.includes("text/event-stream")
    ? text.split(/\r?\n\r?\n/).map((event) => event.split(/\r?\n/).filter((line) => line.startsWith("data:")).map((line) => line.slice(5).trimStart()).join("\n")).filter(Boolean).map((data) => JSON.parse(data))
    : [JSON.parse(text)];
  const rpc = messages.find((message) => message.id === 1);
  if (!rpc?.result || rpc.error || rpc.result.isError) throw new Error("Exa search failed; no fixture substituted");
  let data;
  for (const item of rpc.result.content || []) {
    if (item.type !== "text") continue;
    try { const value = JSON.parse(item.text); if (Array.isArray(value.results)) data = value; } catch {}
  }
  if (!data) throw new Error("Exa returned no structured results; no fixture substituted");
  return { provider: "exa", query, retrieved_at: new Date().toISOString(), ...data };
}

function instagramURL(value) {
  try {
    const url = new URL(value);
    if (url.protocol !== "https:" || !["instagram.com", "www.instagram.com"].includes(url.hostname)) return null;
    return url;
  } catch { return null; }
}

function imageURL(value) {
  try {
    const url = new URL(value);
    if (url.protocol !== "https:" || !["cdninstagram.com", "fbcdn.net"].some((host) => url.hostname === host || url.hostname.endsWith(`.${host}`))) return null;
    return url.href;
  } catch { return null; }
}

const identity = (value) => String(value || "").trim().replace(/^@/, "").toLowerCase();

export function normalizeSearchResults(handle, data) {
  if (!Array.isArray(data.results)) throw new Error("Search results must contain a results array");
  const media = [], excluded = [], seen = new Set();
  const profile = { username: handle, name: handle, biography: null, website: null, followers_count: null, media_count: null };
  for (const result of data.results) {
    const url = instagramURL(result.url);
    if (!url) { excluded.push({ url: result.url, reason: "not a public Instagram URL" }); continue; }
    const parts = url.pathname.split("/").filter(Boolean);
    if (parts.length === 1 && identity(parts[0]) === handle) {
      profile.permalink = `https://www.instagram.com/${handle}/`;
      const match = (result.title || "").match(/^(.+?)\s*\(@[^)]+\)/);
      if (match) profile.name = match[1].trim();
      continue;
    }
    const hasOwner = parts.length === 3;
    const [kind, shortcode] = hasOwner ? parts.slice(1) : parts;
    if (![2, 3].includes(parts.length) || !["p", "reel", "tv"].includes(kind) || !/^[A-Za-z0-9_-]+$/.test(shortcode || "")) {
      excluded.push({ url: result.url, reason: "not a post permalink" }); continue;
    }
    const titleOwner = (result.title || "").match(/^(.+?) on Instagram:/i)?.[1] || (result.title || "").match(/^([^|]+)\s*\|/)?.[1];
    const textOwner = (result.text || "").match(/^([^|\n]+)\s*\|/)?.[1];
    // A mention of @handle inside another account's caption is not authorship.
    const evidence = hasOwner
      ? (identity(parts[0]) === handle ? "account in URL" : null)
      : [result.author, titleOwner, textOwner].some((name) => identity(name) === handle) ? "account in indexed author/title" : null;
    if (!evidence) { excluded.push({ url: result.url, reason: "target authorship not established by search evidence" }); continue; }
    if (seen.has(shortcode)) continue;
    seen.add(shortcode);
    if (profile.name === handle && identity(titleOwner) === handle) profile.name = titleOwner.trim();
    const caption = (result.title || "").replace(/^.+? on Instagram:\s*/i, "").replace(/^[^|]+\s*\|\s*/, "").replace(/^["“]|["”]$/g, "").trim();
    media.push({
      id: shortcode, permalink: `https://www.instagram.com/${kind}/${shortcode}/`,
      caption, caption_kind: "indexed title excerpt",
      media_type: kind === "p" ? "UNKNOWN" : "VIDEO",
      media_url: kind === "p" ? imageURL(result.image) : null,
      thumbnail_url: kind !== "p" ? imageURL(result.image) : null,
      like_count: null, comments_count: null, timestamp: null,
      discovery: {
        provider: data.provider || "public-search", query: data.query || null,
        retrieved_at: data.retrieved_at || null, owner_evidence: evidence,
        indexed_url: result.url, indexed_published_date: result.publishedDate || null,
        image_kind: "search thumbnail; inspect crop and source before generation",
      },
    });
  }
  return { profile, media, excluded };
}
