// ipfs-add.mjs — Admin-only pin to AC's own IPFS node.
//
// POST /api/ipfs-add
//   Body: { name, mimeType, base64 }   a file
//      or { name, json }               a JSON document (TZIP-21 metadata)
//   Auth: AC Bearer token (must be admin)
//   Returns: { cid, uri: "ipfs://<cid>" }
//
// The same path keep-prepare-background takes for Keeps media: add + pin on
// lith's Kubo, then best-effort seed to the oven node and warm the public
// gateways so objkt's indexer finds the CID quickly. Lets off-box jobs (the
// daily token on jasellite) pin without a third-party pinning plan.

import { authorize, hasAdmin } from "../../backend/authorization.mjs";

const IPFS_API = process.env.IPFS_API_URL || "http://localhost:5001";
const IPFS_SEEDER_URL = process.env.IPFS_SEEDER_URL || "http://137.184.237.166:5001";
const PUBLIC_GATEWAYS = ["https://ipfs.io", "https://dweb.link", "https://nftstorage.link"];

const CORS_HEADERS = {
  "Access-Control-Allow-Origin": "*",
  "Access-Control-Allow-Headers": "Content-Type, Authorization",
  "Content-Type": "application/json",
};

function jsonResponse(statusCode, body) {
  return { statusCode, headers: CORS_HEADERS, body: JSON.stringify(body) };
}

export const handler = async (event) => {
  if (event.httpMethod === "OPTIONS") {
    return { statusCode: 200, headers: CORS_HEADERS, body: "" };
  }
  if (event.httpMethod !== "POST") {
    return jsonResponse(405, { error: "Method not allowed" });
  }

  const user = await authorize(event.headers);
  if (!user) return jsonResponse(401, { error: "Unauthorized" });
  if (!(await hasAdmin(user))) return jsonResponse(403, { error: "Admin only" });

  let body;
  try {
    body = JSON.parse(event.body || "{}");
  } catch {
    return jsonResponse(400, { error: "Invalid JSON body" });
  }

  const name = String(body.name || "file").slice(0, 120);
  let content, mimeType;
  if (body.json !== undefined) {
    content = Buffer.from(JSON.stringify(body.json));
    mimeType = "application/json";
  } else if (typeof body.base64 === "string") {
    content = Buffer.from(body.base64, "base64");
    mimeType = String(body.mimeType || "application/octet-stream");
  } else {
    return jsonResponse(400, { error: "Send { name, mimeType, base64 } or { name, json }" });
  }
  if (!content.length) return jsonResponse(400, { error: "Empty content" });

  const form = new FormData();
  form.append("file", new Blob([content], { type: mimeType }), name);
  let cid;
  try {
    const res = await fetch(`${IPFS_API}/api/v0/add?pin=true&cid-version=0`, {
      method: "POST",
      body: form,
      signal: AbortSignal.timeout(120000),
    });
    if (!res.ok) throw new Error(`IPFS add ${res.status}`);
    cid = (await res.json()).Hash;
  } catch (err) {
    return jsonResponse(502, { error: err.message });
  }

  // Best-effort replication; never blocks the response.
  fetch(`${IPFS_SEEDER_URL}/api/v0/pin/add?arg=${cid}`, { method: "POST", signal: AbortSignal.timeout(120000) }).catch(() => {});
  for (const gw of PUBLIC_GATEWAYS) {
    fetch(`${gw}/ipfs/${cid}`, { headers: { Range: "bytes=0-0" }, signal: AbortSignal.timeout(20000) }).catch(() => {});
  }

  console.log(`📌 ipfs-add ${name} (${mimeType}, ${content.length} bytes) → ${cid}`);
  return jsonResponse(200, { cid, uri: `ipfs://${cid}` });
};
