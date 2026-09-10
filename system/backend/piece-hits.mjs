// Piece Hits, 26.09.09
// One place that knows how a hit is counted, so the HTML renderer and the
// /api/piece-hit endpoint cannot drift apart.
//
// Two rules live here:
//   1. A hit is written straight to Mongo. The renderer used to POST to its
//      own public API — out through Cloudflare and back — which cost a second
//      function invocation and up to two seconds on every landing page.
//   2. Only people count. Crawlers, preview fetchers and scripted clients
//      reach the same routes a reader does, and counting them made the
//      `piece-hits` collection describe machines instead of an audience.

// Substrings that appear in the user agent of something that is not a person.
// Deliberately blunt: a false negative is one inflated hit, a false positive
// is a reader who never existed in the record.
const AUTOMATED = [
  "bot", "crawl", "spider", "slurp", "headless", "phantom", "puppeteer",
  "playwright", "curl", "wget", "python", "go-http", "got (", "node-fetch",
  "axios", "okhttp", "java/", "libwww", "httpclient", "scrapy", "facebookexternalhit",
  "embedly", "quora link preview", "outbrain", "pinterest", "slackbot",
  "vkshare", "w3c_validator", "whatsapp", "flipboard", "tumblr", "bitlybot",
  "skypeuripreview", "nuzzel", "discordbot", "google-read-aloud",
  "telegrambot", "applebot", "monitoring", "uptime", "pingdom", "lighthouse",
];

// A request with no user agent at all is not a browser. Real browsers always
// send one; what turns up here is scripts, probes and stale clients.
export function looksAutomated(headers = {}) {
  const ua = (headers["user-agent"] || headers["User-Agent"] || "").toLowerCase();
  if (!ua) return true;
  return AUTOMATED.some((needle) => ua.includes(needle));
}

// The indexes were being (re)declared on every single request. Once per
// process is enough, and a failure here must not stop a hit being counted.
let indexed = null;
function ensureIndexes(db) {
  indexed ??= Promise.all([
    db.collection("piece-hits").createIndex({ piece: 1 }, { unique: true }),
    db.collection("piece-hits").createIndex({ hits: -1 }),
    db.collection("piece-user-hits").createIndex({ piece: 1, user: 1 }, { unique: true }),
    db.collection("piece-user-hits").createIndex({ piece: 1, hits: -1 }),
  ]).catch(() => {});
  return indexed;
}

// Record one hit. `db` is an open handle — the caller owns the connection, so
// this never opens or closes a pool of its own.
export async function recordPieceHit(db, { piece, type, user = null }) {
  if (!piece) return;
  await ensureIndexes(db);

  const now = new Date();
  const today = now.toISOString().split("T")[0];
  const hits = db.collection("piece-hits");
  const userHits = db.collection("piece-user-hits");

  await hits.updateOne(
    { piece },
    {
      $inc: { hits: 1, [`daily.${today}.hits`]: 1 },
      $set: { lastHit: now, type },
      $setOnInsert: { firstHit: now, uniqueUsers: 0 },
    },
    { upsert: true },
  );

  const userKey = user || "anonymous";
  const result = await userHits.updateOne(
    { piece, user: userKey },
    {
      $inc: { hits: 1 },
      $set: { lastHit: now },
      $setOnInsert: { firstHit: now },
    },
    { upsert: true },
  );

  // A first sighting of a signed-in reader is also a new unique.
  if (result.upsertedCount > 0 && userKey !== "anonymous") {
    await hits.updateOne(
      { piece },
      { $inc: { uniqueUsers: 1, [`daily.${today}.unique`]: 1 } },
    );
  }
}
