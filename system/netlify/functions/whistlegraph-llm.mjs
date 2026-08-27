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
//   GET /api/wg/verify           free: checks a license receipt's signature
//
// Verification is stateless and therefore carries its own evidence. The
// signature covers the whole receipt — licensee and issue date included — so a
// code alone can never re-derive it; the receipt travels in the link as a
// base64url token and is checked against the same bytes that were signed.
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
//   WHISTLEGRAPH_X402_FACILITATOR_TOKEN  bearer token, if the facilitator wants one
//   WHISTLEGRAPH_LICENSE_SECRET    HMAC key for signing license receipts
//
// The default facilitator settles TESTNETS ONLY (base-sepolia, solana-devnet,
// and friends) — it does not carry Base mainnet. Pointing mainnet terms at it
// quotes a price nobody can pay: the buyer signs, verify is refused, and the
// request dies at 502 having promised a settlement that was never possible.
// Taking real USDC means an account-holding facilitator (Coinbase CDP, PayAI),
// which is why the facilitator takes a bearer token here. `supported()` checks
// the pairing at request time so the mismatch surfaces as an honest 503 to the
// asker instead of a broken payment to the payer.
//
// Unconfigured, every paid route answers 503. It must never fall open and serve
// paid data for free just because an env var is missing.

import { createHmac, createPrivateKey, randomBytes, sign, timingSafeEqual } from "node:crypto";
import { existsSync, readFileSync } from "node:fs";
import { join } from "node:path";
import { respond } from "../../backend/http.mjs";

const DIR = join(process.cwd(), "public", "whistlegraph.org");
const ASSETS = "https://assets.aesthetic.computer/whistlegraph/index";
const SITE_URL = "https://whistlegraph.org";

const X402_VERSION = 1;
const NETWORK = process.env.WHISTLEGRAPH_X402_NETWORK || "base";
const FACILITATOR = process.env.WHISTLEGRAPH_X402_FACILITATOR || "https://x402.org/facilitator";
const FACILITATOR_TOKEN = process.env.WHISTLEGRAPH_X402_FACILITATOR_TOKEN || "";
const CDP_API_KEY_ID = process.env.CDP_API_KEY_ID || "";
const CDP_API_KEY_SECRET = process.env.CDP_API_KEY_SECRET || "";
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
  // Sign the exact bytes that travel, so verification never has to reproduce
  // this object's key order to get the same digest back.
  const signed = JSON.stringify(receipt);
  const signature = createHmac("sha256", LICENSE_SECRET).update(signed).digest("hex");
  const token = Buffer.from(signed, "utf8").toString("base64url");

  return {
    receipt,
    signature,
    verify: `${SITE_URL}/api/wg/verify?receipt=${token}&sig=${signature}`,
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

// Checking a receipt is free. Charging to confirm a license someone already
// bought would make the signature worth less than the paper it is printed on.
function verifyLicense(token, signature) {
  if (!token || !signature) {
    return {
      valid: false,
      reason: "Pass the whole receipt: /api/wg/verify?receipt=<token>&sig=<signature>, both taken verbatim from the `verify` link in a license.",
    };
  }

  let signed;
  try {
    signed = Buffer.from(String(token), "base64url").toString("utf8");
  } catch {
    return { valid: false, reason: "The receipt token is not valid base64url." };
  }

  const expected = createHmac("sha256", LICENSE_SECRET).update(signed).digest("hex");
  const given = String(signature).toLowerCase();
  const match =
    given.length === expected.length &&
    timingSafeEqual(Buffer.from(expected, "utf8"), Buffer.from(given, "utf8"));
  if (!match) {
    return { valid: false, reason: "Signature does not match this receipt. It was not issued here, or it has been edited since." };
  }

  let receipt;
  try {
    receipt = JSON.parse(signed);
  } catch {
    return { valid: false, reason: "The receipt carries a valid signature but is not readable JSON." };
  }

  // A signature stays good forever; the grant it describes does not. Report
  // both, and never call an expired license valid.
  const expires = Date.parse(receipt?.expires);
  const expired = Number.isFinite(expires) && expires < Date.now();
  return {
    valid: !expired,
    signature: "valid",
    expired,
    receipt,
    record: receipt?.code ? `${SITE_URL}/${receipt.code}` : undefined,
    note: expired
      ? "This receipt was genuinely issued here, but its term has run out."
      : "This receipt was issued here and has not been altered.",
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

const facilitatorURL = (path) => `${FACILITATOR.replace(/\/$/, "")}/${path}`;

const b64url = (input) => Buffer.from(input).toString("base64url");

// CDP will not take a fixed bearer token. Each call carries its own JWT, signed
// with the account's Ed25519 key and bound to the exact method and URI it is
// for, so a token lifted from one request cannot be replayed against another.
// They are minted per call and expire in two minutes; there is nothing to cache.
const CDP_PKCS8_PREFIX = Buffer.from("302e020100300506032b657004220420", "hex");

function cdpKey() {
  // CDP hands back 64 bytes: a 32-byte seed followed by its public half. Node
  // wants PKCS8, so wrap the seed and let it derive the rest.
  const raw = Buffer.from(CDP_API_KEY_SECRET, "base64");
  if (raw.length !== 64 && raw.length !== 32) {
    throw new Error(`CDP private key is ${raw.length} bytes; expected 32 or 64`);
  }
  return createPrivateKey({
    key: Buffer.concat([CDP_PKCS8_PREFIX, raw.subarray(0, 32)]),
    format: "der",
    type: "pkcs8",
  });
}

function cdpToken(method, path) {
  const { host, pathname } = new URL(facilitatorURL(path));
  const now = Math.floor(Date.now() / 1000);
  const header = b64url(
    JSON.stringify({
      alg: "EdDSA",
      typ: "JWT",
      kid: CDP_API_KEY_ID,
      nonce: randomBytes(16).toString("hex"),
    }),
  );
  const payload = b64url(
    JSON.stringify({
      iss: "cdp",
      sub: CDP_API_KEY_ID,
      aud: ["cdp_service"],
      nbf: now,
      exp: now + 120,
      uris: [`${method} ${host}${pathname}`],
    }),
  );
  const signature = sign(null, Buffer.from(`${header}.${payload}`), cdpKey());
  return `${header}.${payload}.${b64url(signature)}`;
}

function facilitatorAuth(method, path) {
  if (CDP_API_KEY_ID && CDP_API_KEY_SECRET) {
    return { Authorization: `Bearer ${cdpToken(method, path)}` };
  }
  // Other facilitators (PayAI and friends) do take a plain token.
  return FACILITATOR_TOKEN ? { Authorization: `Bearer ${FACILITATOR_TOKEN}` } : {};
}

async function facilitate(path, body) {
  const res = await fetch(facilitatorURL(path), {
    method: "POST",
    headers: { "Content-Type": "application/json", ...facilitatorAuth("POST", path) },
    body: JSON.stringify(body),
  });
  if (!res.ok) throw new Error(`facilitator ${path} responded ${res.status}`);
  return res.json();
}

// Networks are named two ways in the wild — "base" and the CAIP-2 "eip155:8453"
// that means the same chain — and a facilitator may advertise either.
const CHAIN_IDS = { base: "eip155:8453", "base-sepolia": "eip155:84532" };

// Ask the facilitator whether it can actually settle what we are about to quote.
// Cached for the life of the process: it is a fact about a deployment pairing,
// not about a request, and a failed lookup must not be cached as a refusal.
let supportedCache;
async function settles(network) {
  if (supportedCache === undefined) {
    const res = await fetch(facilitatorURL("supported"), {
      headers: facilitatorAuth("GET", "supported"),
    });
    if (!res.ok) throw new Error(`facilitator supported responded ${res.status}`);
    const body = await res.json();
    supportedCache = new Set((body?.kinds || []).map((k) => k.network).filter(Boolean));
  }
  return supportedCache.has(network) || supportedCache.has(CHAIN_IDS[network]);
}

// --- handler ---------------------------------------------------------------

export async function handler(event) {
  if (event.httpMethod === "OPTIONS") return respond(204, "", { ...HEADERS, "Access-Control-Allow-Headers": "Content-Type, X-PAYMENT" });
  if (event.httpMethod !== "GET") return respond(405, { message: "Method Not Allowed." }, HEADERS);

  const params = event.queryStringParameters || {};
  const kind = String(params.resource || "").trim();
  const code = cleanCode(params.code);
  const resource = `${SITE_URL}/api/wg/${kind}${code ? `/${code}` : ""}`;

  // Free, and deliberately ahead of every paywalled branch below.
  if (kind === "verify") {
    if (!LICENSE_SECRET) {
      return respond(503, { message: "License verification is not configured." }, HEADERS);
    }
    const checked = verifyLicense(params.receipt, params.sig);
    return respond(checked.valid ? 200 : 400, checked, HEADERS);
  }

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
        [`${SITE_URL}/api/wg/verify`]: "Check a license receipt: ?receipt=<token>&sig=<signature>.",
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

  // Never quote a price on a network this facilitator cannot settle. Only an
  // affirmative "no" refuses: if the lookup itself fails we still quote, since
  // verify and settle remain in the way of any money actually moving.
  try {
    if (!(await settles(NETWORK))) {
      return respond(503, {
        message: `Paid access is configured for "${NETWORK}", which its payment facilitator does not settle. Nothing can be charged until that pairing is fixed.`,
        free: `${SITE_URL}/index.md`,
      }, HEADERS);
    }
  } catch (error) {
    console.error("Whistlegraph x402 supported lookup failed:", error?.message || error);
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
