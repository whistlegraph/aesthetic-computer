// Download, 2026.09.28
// Counts a direct app download, then redirects to the file on the CDN.
// GET /api/download?app=slab&file=Slab-1.1.dmg
// GET /api/download?whoami=1 answers the caller's own hash, so reports can
// leave the fleet's own downloads out.
//
// The DMGs live behind Cloudflare with caching bypassed, so no server of ours
// ever saw a download. This is the one place that does. A row keeps the
// app, version, day, country and a coarse platform. The address is kept only
// as a keyed hash, so repeat downloads from one place can be told apart
// without storing where anyone is.
import { connect } from "../../backend/database.mjs";
import { createHmac } from "node:crypto";
import { respond } from "../../backend/http.mjs";
import { automatedVisit } from "../../public/aesthetic.computer/lib/visit-model.mjs";
import { resolveAppDownload } from "../../backend/app-downloads.mjs";

export const DOWNLOAD_COLLECTION = "downloads";

// Keyed on a secret lith already holds, so the hash can't be reversed by
// hashing every address, and it stays stable across restarts.
const key = createHmac("sha256", "ac-download-v1")
  .update(process.env.MONGODB_CONNECTION_STRING || "").digest();
const addressHash = (event) => {
  const address = event.headers?.["cf-connecting-ip"] ||
    event.headers?.["x-forwarded-for"]?.split(",")[0].trim() || "unknown";
  return createHmac("sha256", key).update(address).digest("hex").slice(0, 16);
};

function platform(agent = "") {
  if (/iPhone|iPad/i.test(agent)) return "ios";
  if (/Macintosh|Mac OS X/i.test(agent)) return "mac";
  if (/Windows/i.test(agent)) return "windows";
  if (/Android/i.test(agent)) return "android";
  if (/Linux/i.test(agent)) return "linux";
  return "other";
}

let indexed;
async function record(row) {
  const { db } = await connect();
  const collection = db.collection(DOWNLOAD_COLLECTION);
  indexed ||= collection.createIndex({ app: 1, at: -1 }).catch((error) => { indexed = null; throw error; });
  await indexed;
  await collection.insertOne(row);
}

export async function handler(event) {
  const headers = { "Cache-Control": "no-store" };
  const query = event.queryStringParameters || {};

  if (query.whoami !== undefined) return respond(200, { hash: addressHash(event) }, headers);

  const download = resolveAppDownload(query.app, query.file);
  if (!download) return respond(404, { error: "Unknown download" }, headers);

  const { location, version } = download;
  if (event.httpMethod === "GET") {
    const agent = event.headers?.["user-agent"] || "";
    const at = new Date();
    const row = {
      app: query.app, version, file: query.file, at, day: at.toISOString().slice(0, 10),
      hash: addressHash(event), country: event.headers?.["cf-ipcountry"] || null,
      platform: platform(agent), automated: automatedVisit({ userAgent: agent }),
      from: typeof query.from === "string" ? query.from.slice(0, 32) : null,
    };
    // The download never waits on the database for more than a moment.
    await Promise.race([
      record(row).catch((error) => console.warn("⬇️ download not recorded:", error.message)),
      new Promise((resolve) => setTimeout(resolve, 1500)),
    ]);
    console.log(`⬇️ download ${row.app} ${row.version} ${row.platform} ${row.country || "?"}`);
  }
  return respond(302, "", { ...headers, Location: location, "Content-Type": "text/plain" });
}
