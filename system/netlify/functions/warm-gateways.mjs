// warm-gateways.mjs — Best-effort gateway warming for a set of IPFS CIDs.
//
// POST /api/warm-gateways  { cids: ["Qm...", ...] }
//
// Triggers public IPFS gateways to fetch each CID from the DHT (finding us as
// a provider) and cache it. Once cached there, downstream indexers like objkt
// have multiple places to fetch from, cutting the lag after on-chain metadata
// updates. Fire-and-forget per request — returns 200 immediately.
//
// The server-side upload pipeline already warms gateways at bake time; this
// endpoint exists so the post-tx confirmation UI can re-warm right before the
// on-chain update is visible to indexers, shown as a visible track step.

const PUBLIC_GATEWAYS = [
  "https://ipfs.io",
  "https://gateway.ipfs.io",
  "https://dweb.link",
  "https://nftstorage.link",
];

const CORS_HEADERS = {
  "Access-Control-Allow-Origin": "*",
  "Access-Control-Allow-Headers": "Content-Type, Authorization",
  "Content-Type": "application/json",
};

function isValidCid(v) {
  return typeof v === "string" && /^(Qm[1-9A-HJ-NP-Za-km-z]{44}|ba[a-z0-9]{56,})$/.test(v.trim());
}

export const handler = async (event) => {
  if (event.httpMethod === "OPTIONS") {
    return { statusCode: 200, headers: CORS_HEADERS, body: "" };
  }
  if (event.httpMethod !== "POST") {
    return { statusCode: 405, headers: CORS_HEADERS, body: JSON.stringify({ error: "Method not allowed" }) };
  }

  let body;
  try { body = JSON.parse(event.body || "{}"); }
  catch { return { statusCode: 400, headers: CORS_HEADERS, body: JSON.stringify({ error: "Invalid JSON body" }) }; }

  const cids = Array.isArray(body.cids) ? body.cids.filter(isValidCid) : [];
  if (!cids.length) {
    return { statusCode: 400, headers: CORS_HEADERS, body: JSON.stringify({ error: "cids array required" }) };
  }

  for (const cid of cids) {
    for (const gw of PUBLIC_GATEWAYS) {
      fetch(`${gw}/ipfs/${cid}`, {
        method: "GET",
        headers: { Range: "bytes=0-0" },
        redirect: "follow",
        signal: AbortSignal.timeout(20000),
      }).catch(() => {});
    }
    console.log(`🔥 WARM-GATEWAYS: ${cid.slice(0, 12)}... fanned out to ${PUBLIC_GATEWAYS.length} gateways`);
  }

  return {
    statusCode: 200,
    headers: CORS_HEADERS,
    body: JSON.stringify({
      ok: true,
      cidCount: cids.length,
      gateways: PUBLIC_GATEWAYS.map((u) => u.replace(/^https:\/\//, "")),
    }),
  };
};
