// ipfs-add.mjs — Admin-only pin to AC's own IPFS node.
//
// POST /api/ipfs-add
//   Body: { name, mimeType, base64 }   a file
//      or { name, json }               a JSON document (TZIP-21 metadata)
//      or { files: [{ name, mimeType, base64 }] }  a flat IPFS directory
//   Auth: AC Bearer token (must be admin)
//   Returns: { cid, uri: "ipfs://<cid>" }
//
// The same path keep-prepare-background takes for Keeps media: add + pin on
// lith's Kubo, then best-effort seed to the oven node and warm the public
// gateways so objkt's indexer finds the CID quickly. Lets off-box jobs (the
// daily token on jasellite) pin without a third-party pinning plan.

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

export const createHandler = ({ authorize, hasAdmin, fetch = globalThis.fetch }) => async (event) => {
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

  if (!body || typeof body !== "object") return jsonResponse(400, { error: "Invalid body" });
  const directory = body.files !== undefined;
  if (directory && (!Array.isArray(body.files) || !body.files.length || body.files.length > 32)) {
    return jsonResponse(400, { error: "A directory needs 1–32 files" });
  }
  const form = new FormData();
  const names = new Set();
  let bytes = 0;
  for (const file of directory ? body.files : [body]) {
    if (!file || typeof file !== "object") return jsonResponse(400, { error: "Invalid file" });
    const name = String(file.name || "file").slice(0, 120);
    if (directory && (!/^[A-Za-z0-9][A-Za-z0-9._-]*$/.test(name) || names.has(name))) {
      return jsonResponse(400, { error: "Directory filenames must be unique and have no paths" });
    }
    names.add(name);
    let content, mimeType;
    if (file.json !== undefined) {
      content = Buffer.from(JSON.stringify(file.json));
      mimeType = "application/json";
    } else if (typeof file.base64 === "string") {
      content = Buffer.from(file.base64, "base64");
      mimeType = String(file.mimeType || "application/octet-stream");
    } else {
      return jsonResponse(400, { error: "Send { name, mimeType, base64 }, { name, json }, or { files: [...] }" });
    }
    if (!content.length) return jsonResponse(400, { error: "Empty content" });
    bytes += content.length;
    form.append("file", new Blob([content], { type: mimeType }), name);
  }
  let cid;
  try {
    const res = await fetch(`${IPFS_API}/api/v0/add?pin=true&cid-version=0${directory ? "&wrap-with-directory=true" : ""}`, {
      method: "POST",
      body: form,
      signal: AbortSignal.timeout(120000),
    });
    if (!res.ok) throw new Error(`IPFS add ${res.status}`);
    const entries = (await res.text()).trim().split("\n").map(line => JSON.parse(line));
    cid = (directory ? entries.find(entry => entry.Name === "") : entries[0])?.Hash;
    if (!/^Qm[1-9A-HJ-NP-Za-km-z]{44}$/.test(cid || "")) throw new Error("IPFS did not return the requested file or directory CID");
  } catch (err) {
    return jsonResponse(502, { error: err.message });
  }

  // Best-effort replication; never blocks the response.
  fetch(`${IPFS_SEEDER_URL}/api/v0/pin/add?arg=${cid}`, { method: "POST", signal: AbortSignal.timeout(120000) }).catch(() => {});
  for (const gw of PUBLIC_GATEWAYS) {
    fetch(`${gw}/ipfs/${cid}`, { headers: { Range: "bytes=0-0" }, signal: AbortSignal.timeout(20000) }).catch(() => {});
  }

  console.log(`📌 ipfs-add ${[...names].join(", ")} (${bytes} bytes${directory ? ", directory" : ""}) → ${cid}`);
  return jsonResponse(200, { cid, uri: `ipfs://${cid}` });
};

export const handler = async event => {
  const { authorize, hasAdmin } = await import("../../backend/authorization.mjs");
  return createHandler({ authorize, hasAdmin })(event);
};
