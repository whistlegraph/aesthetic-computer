// Paid machine access to the Whistlegraph index, metered with x402.
//
// The free tier is static and needs no code: llms.txt, index.md, graphs.json,
// and posts.json are all served straight off disk. Gating those would be
// theater — they are already public. What is sold here is the work a crawler
// would otherwise have to do itself, the audit trail that is published nowhere,
// and an actual license.
//
//   GET /api/wg/bulk             the entire normalized dataset in one document
//   GET /api/wg/sources/<code>   every source video behind one work, with its
//                                own view count — the audit trail behind the
//                                aggregate numbers, published nowhere else
//   GET /api/wg/license/<code>   a signed redistribution license receipt plus
//                                resolved full-resolution asset URLs
//
// x402 (https://x402.org) is the HTTP 402 flow: ask for a resource, get back a
// 402 that says exactly what it costs and where to pay, pay, then repeat the
// request with an `X-PAYMENT` header. No account, no API key, no signup — which
// is the point, because the buyer here is usually not a person.
//
// Asking is always free: an unpaid GET returns the terms, never an error page.
//
// Configuration (all required before any paid route will serve):
//   WHISTLEGRAPH_X402_PAY_TO       receiving address
//   WHISTLEGRAPH_X402_ASSET        payment token contract (USDC on the network)
//   WHISTLEGRAPH_X402_NETWORK      default "base"
//   WHISTLEGRAPH_X402_FACILITATOR  default "https://x402.org/facilitator"
//   WHISTLEGRAPH_LICENSE_SECRET    HMAC key for signing license receipts
//
// Unconfigured, every paid route answers 503. It must never fall open and serve
// paid data for free just because an env var is missing.

import { createHmac } from "node:crypto";
import { existsSync, readFileSync } from "node:fs";
import { join } from "node:path";
import { respond } from "../../backend/http.mjs";

const DIR = join(process.cwd(), "public", "whistlegraph.org");
const ASSETS = "https://assets.aesthetic.computer/whistlegraph/index";
const SITE_URL = "https://whistlegraph.org";

const X402_VERSION = 1;
const NETWORK = process.env.WHISTLEGRAPH_X402_NETWORK || "base";
const FACILITATOR = process.env.WHISTLEGRAPH_X402_FACILITATOR || "https://x402.org/facilitator";
const PAY_TO = process.env.WHISTLEGRAPH_X402_PAY_TO || "";
const ASSET = process.env.WHISTLEGRAPH_X402_ASSET || "";
const LICENSE_SECRET = process.env.WHISTLEGRAPH_LICENSE_SECRET || "";

// USDC carries 6 decimals, so prices are quoted here in atomic units. Keep the
// display prices in step with toolchain/whistlegraph/gen-llms.mjs — that file
// advertises the offer, this one enforces it.
const DECIMALS = 6;
const PRICES = {
  bulk: { atomic: "5000000", display: "5.00" },
  sources: { atomic: "100000", display: "0.10" },
  license: { atomic: "1000000", display: "1.00" },
};
const LICENSE_DAYS = 365;
const HEADERS = { "Cache-Control": "no-store" };

let cache = null;
function data() {
  if (cache) return cache;
  const rd = (name) => {
    const path = join(DIR, name);
    if (!existsSync(path)) throw new Error(`missing ${name}`);
    return JSON.parse(readFileSync(path, "utf8"));
  };
  cache = { graphs: rd("graphs.json"), posts: rd("posts.json") };
  return cache;
}

// Renamed works kept their ORIGINAL asset key; `asset` overrides `code`.
const assetKey = (w) => w.asset || w.code;
const scoreURL = (w) => w.thumb || `${ASSETS}/${assetKey(w)}.jpg`;
const videoURL = (w) => `${ASSETS}/${assetKey(w)}.mp4`;

const cleanCode = (value) => String(value || "").trim().toLowerCase().replace(/[^a-z0-9]/g, "").slice(0, 24);

// Resolve a code through the alias table so an old link finds its record.
function findWork(graphs, code) {
  const canonical = graphs.aliases?.[code] || code;
  const pools = [graphs.works, graphs.candidates, graphs.legacy];
  for (const pool of pools) {
    const hit = (pool || []).find((w) => w.code === canonical);
    if (hit) return hit;
  }
  return null;
}

// --- the paid resources ----------------------------------------------------

function bulk() {
  const { graphs, posts } = data();
  const resolve = (w) => ({ ...w, score: scoreURL(w), video: videoURL(w), record: `${SITE_URL}/${w.code}` });
  return {
    generated: graphs.generated,
    counts: {
      works: (graphs.works || []).length,
      candidates: (graphs.candidates || []).length,
      legacy: (graphs.legacy || []).length,
      posts: (posts.posts || []).length,
    },
    works: (graphs.works || []).map(resolve),
    candidates: (graphs.candidates || []).map(resolve),
    legacy: (graphs.legacy || []).map(resolve),
    aliases: graphs.aliases || {},
    posts: posts.posts || [],
    license: "Index data may be redistributed with attribution. The works themselves remain © their listed authors.",
  };
}

function sources(code) {
  const { graphs, posts } = data();
  const work = findWork(graphs, code);
  if (!work) return null;

  // Only explicit `contributes` edges count toward a work's totals; anything
  // else is an appearance, and the whole point of this endpoint is being able
  // to tell those apart.
  const items = (posts.posts || []).filter((p) =>
    (p.relationships || []).some((r) => r.work === work.code && r.role === "contributes"),
  );
  const mentions = (posts.posts || []).filter(
    (p) => (p.works || []).includes(work.code) && !items.includes(p),
  );
  const shape = (p) => ({
    id: p.id,
    url: p.url,
    date: p.date,
    views: p.views ?? null,
    likes: p.likes ?? null,
    comments: p.comments ?? null,
    duration: p.duration ?? null,
    desc: p.desc || "",
    video: p.src || null,
    thumb: p.thumb || null,
  });
  const contributing = items.map(shape).sort((a, b) => (b.views || 0) - (a.views || 0));
  const summed = contributing.reduce((total, p) => total + (p.views || 0), 0);

  return {
    code: work.code,
    requested: code,
    title: work.title,
    by: work.by,
    year: work.year ?? null,
    status: work.status,
    reported: { videos: work.perf ?? null, views: work.views ?? null },
    // The audit: if `summed` and the reported total disagree, the aggregate is
    // carrying posts this list does not. That discrepancy is the product.
    computed: { videos: contributing.length, views: summed },
    reconciles: (work.views ?? null) === summed && (work.perf ?? null) === contributing.length,
    contributing,
    mentions: mentions.map(shape),
  };
}

function license(code, payer) {
  const { graphs } = data();
  const work = findWork(graphs, code);
  if (!work) return null;
  if (!LICENSE_SECRET) throw new Error("license signing key not configured");

  const issued = new Date();
  const expires = new Date(issued.getTime() + LICENSE_DAYS * 86400000);
  const receipt = {
    code: work.code,
    title: work.title,
    author: work.by,
    licensee: payer || "unknown",
    issued: issued.toISOString(),
    expires: expires.toISOString(),
    grant: "Non-exclusive worldwide right to reproduce and redistribute this work's score image and video, with attribution to the named author and a link to its record. Not a grant to train generative models, sublicense, or sell.",
  };
  const signature = createHmac("sha256", LICENSE_SECRET)
    .update(JSON.stringify(receipt))
    .digest("hex");

  return {
    receipt,
    signature,
    verify: `${SITE_URL}/api/wg/verify?code=${work.code}&sig=${signature}`,
    assets: {
      // These URLs are public on the CDN — what is being sold is the license and
      // the signed receipt, not access. Saying otherwise would be a lie the
      // buyer discovers in one request.
      score: scoreURL(work),
      video: videoURL(work),
      record: `${SITE_URL}/${work.code}`,
      note: "These assets are publicly reachable. This receipt licenses their reuse; it does not gate their delivery.",
    },
  };
}

// --- x402 ------------------------------------------------------------------

function requirements(resource, price, description) {
  return {
    scheme: "exact",
    network: NETWORK,
    maxAmountRequired: price.atomic,
    resource,
    description,
    mimeType: "application/json",
    payTo: PAY_TO,
    maxTimeoutSeconds: 120,
    asset: ASSET,
    extra: { name: "USDC", version: "2", decimals: DECIMALS, display: price.display },
  };
}

function paymentRequired(accepts, message) {
  return respond(402, { x402Version: X402_VERSION, accepts: [accepts], error: message }, HEADERS);
}

function decodePayment(header) {
  try {
    return JSON.parse(Buffer.from(String(header), "base64").toString("utf8"));
  } catch {
    return null;
  }
}

async function facilitate(path, body) {
  const res = await fetch(`${FACILITATOR.replace(/\/$/, "")}/${path}`, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify(body),
  });
  if (!res.ok) throw new Error(`facilitator ${path} responded ${res.status}`);
  return res.json();
}

// --- handler ---------------------------------------------------------------

export async function handler(event) {
  if (event.httpMethod === "OPTIONS") return respond(204, "", { ...HEADERS, "Access-Control-Allow-Headers": "Content-Type, X-PAYMENT" });
  if (event.httpMethod !== "GET") return respond(405, { message: "Method Not Allowed." }, HEADERS);

  const params = event.queryStringParameters || {};
  const kind = String(params.resource || "").trim();
  const code = cleanCode(params.code);
  const resource = `${SITE_URL}/api/wg/${kind}${code ? `/${code}` : ""}`;

  let price;
  let description;
  if (kind === "bulk") {
    price = PRICES.bulk;
    description = "The complete Whistlegraph dataset — every work, candidate, legacy code, alias, and post, with all media URLs resolved.";
  } else if (kind === "sources") {
    if (!code) return respond(400, { message: "A work code is required: /api/wg/sources/<code>." }, HEADERS);
    price = PRICES.sources;
    description = `Every source video behind whistlegraph "${code}", each with its own view count and date.`;
  } else if (kind === "license") {
    if (!code) return respond(400, { message: "A work code is required: /api/wg/license/<code>." }, HEADERS);
    price = PRICES.license;
    description = `A signed, verifiable redistribution license for whistlegraph "${code}".`;
  } else {
    return respond(404, {
      message: "Unknown resource.",
      resources: {
        "/api/wg/bulk": `${PRICES.bulk.display} USDC — the whole dataset in one document`,
        "/api/wg/sources/<code>": `${PRICES.sources.display} USDC — the source videos behind one work`,
        "/api/wg/license/<code>": `${PRICES.license.display} USDC — a signed redistribution license`,
      },
      free: {
        [`${SITE_URL}/llms.txt`]: "Where to start.",
        [`${SITE_URL}/index.md`]: "The complete index as Markdown.",
        [`${SITE_URL}/graphs.json`]: "Works, candidates, legacy, aliases.",
        [`${SITE_URL}/posts.json`]: "Every published appearance.",
      },
    }, HEADERS);
  }

  // Fail closed. An unconfigured wallet must never mean free paid data.
  if (!PAY_TO || !ASSET) {
    return respond(503, {
      message: "Paid access is not accepting payment yet.",
      free: `${SITE_URL}/index.md`,
    }, HEADERS);
  }

  const accepts = requirements(resource, price, description);
  const headers = event.headers || {};
  const paymentHeader = headers["x-payment"] || headers["X-PAYMENT"];

  if (!paymentHeader) {
    return paymentRequired(accepts, `Payment required: ${price.display} USDC. Repeat this request with an X-PAYMENT header. See https://x402.org.`);
  }

  const paymentPayload = decodePayment(paymentHeader);
  if (!paymentPayload) return paymentRequired(accepts, "X-PAYMENT was not valid base64-encoded JSON.");

  let payer;
  try {
    const verification = await facilitate("verify", {
      x402Version: X402_VERSION,
      paymentPayload,
      paymentRequirements: accepts,
    });
    const ok = verification?.isValid ?? verification?.valid;
    if (!ok) {
      return paymentRequired(accepts, verification?.invalidReason || "Payment could not be verified.");
    }
    payer = verification?.payer;
  } catch (error) {
    console.error("Whistlegraph x402 verify failed:", error?.message || error);
    return respond(502, { message: "Payment verification is unavailable; nothing was charged." }, HEADERS);
  }

  // Build the payload BEFORE settling, so a bad code can never take money.
  let payload;
  try {
    if (kind === "bulk") payload = bulk();
    else if (kind === "sources") payload = sources(code);
    else payload = license(code, payer);
  } catch (error) {
    console.error("Whistlegraph paid resource failed:", error?.message || error);
    return respond(500, { message: "Resource unavailable; nothing was charged." }, HEADERS);
  }
  if (!payload) {
    return respond(404, { message: `No whistlegraph with code "${code}"; nothing was charged.` }, HEADERS);
  }

  let settlement;
  try {
    settlement = await facilitate("settle", {
      x402Version: X402_VERSION,
      paymentPayload,
      paymentRequirements: accepts,
    });
    if (settlement?.success === false) {
      return paymentRequired(accepts, settlement?.errorReason || "Payment could not be settled.");
    }
  } catch (error) {
    console.error("Whistlegraph x402 settle failed:", error?.message || error);
    return respond(502, { message: "Payment settlement is unavailable; nothing was charged." }, HEADERS);
  }

  const receiptHeader = Buffer.from(JSON.stringify(settlement || {})).toString("base64");
  return respond(200, payload, {
    ...HEADERS,
    "X-PAYMENT-RESPONSE": receiptHeader,
    "Access-Control-Expose-Headers": "X-PAYMENT-RESPONSE",
  });
}
