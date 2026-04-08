// lith — AC monolith server
// Wraps Netlify function handlers in Express routes + serves static files.

// Shim awslambda before anything imports @netlify/functions.
// Netlify's stream() calls awslambda.streamifyResponse() at wrap time,
// which doesn't exist outside AWS Lambda. This shim adapts the 3-arg
// streaming function (event, responseStream, context) back to a normal
// 2-arg handler (event, context) that returns {statusCode, headers, body}.
import { PassThrough } from "stream";
import { Readable } from "stream";

if (typeof globalThis.awslambda === "undefined") {
  globalThis.awslambda = {
    streamifyResponse: (wrappedFn) => {
      // wrappedFn expects (event, responseStream, context).
      // It calls the real handler(event, context) internally, then pipes
      // the body to responseStream via pipeline(). We provide a PassThrough
      // as the responseStream and return it as the response body.
      return async (event, context) => {
        const pt = new PassThrough();

        // Promise that resolves when HttpResponseStream.from() is called
        // inside wrappedFn, giving us the response metadata (statusCode, headers).
        let resolveMetadata;
        const metadataPromise = new Promise((r) => { resolveMetadata = r; });
        pt._resolveMetadata = resolveMetadata;

        // Start the pipeline (don't await — data streams to pt asynchronously)
        wrappedFn(event, pt, context).catch((err) => {
          if (!pt.destroyed) pt.destroy(err);
        });

        const metadata = await metadataPromise;
        const webStream = Readable.toWeb(pt);

        return { ...metadata, body: webStream };
      };
    },
    HttpResponseStream: {
      from: (stream, metadata) => {
        // Signal metadata to the adapter above
        if (stream._resolveMetadata) stream._resolveMetadata(metadata || {});
        return stream;
      },
    },
  };
}

import express from "express";
import { readdirSync, readFileSync, existsSync, mkdirSync, writeFileSync } from "fs";
import { join, dirname } from "path";
import { fileURLToPath } from "url";
import { createServer as createHttpsServer } from "https";
import { createServer as createHttpServer } from "http";
import { resolveFunctionName } from "./route-resolution.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const SYSTEM = join(__dirname, "..", "system");
const PUBLIC = join(SYSTEM, "public");
const FN_DIR = join(SYSTEM, "netlify", "functions");

// Load .env from system/ if present (handles special chars in values)
const envPath = join(SYSTEM, ".env");
if (existsSync(envPath)) {
  for (const line of readFileSync(envPath, "utf-8").split("\n")) {
    if (!line || line.startsWith("#")) continue;
    const idx = line.indexOf("=");
    if (idx === -1) continue;
    const key = line.slice(0, idx).trim();
    const val = line.slice(idx + 1).trim();
    if (key && !process.env[key]) process.env[key] = val;
  }
}

const PORT = process.env.PORT || 8888;
const DEV = process.env.NODE_ENV !== "production";

// Tell functions we're in dev mode (so index.mjs uses cwd instead of /var/task)
if (DEV) {
  process.env.CONTEXT = process.env.CONTEXT || "dev";
  process.env.NETLIFY_DEV = process.env.NETLIFY_DEV || "true";
}

// Set cwd to system/ so relative paths in functions resolve correctly
process.chdir(SYSTEM);

// SSL certs for local dev (same ones Netlify local context uses)
const SSL_CERT = join(__dirname, "..", "ssl-dev", "localhost.pem");
const SSL_KEY = join(__dirname, "..", "ssl-dev", "localhost-key.pem");
const HAS_SSL = existsSync(SSL_CERT) && existsSync(SSL_KEY);

const app = express();
const BOOT_TIME = Date.now();

// --- Response cache for hot GET endpoints ---
const responseCache = new Map(); // key → { body, headers, statusCode, expires }
const CACHE_TTLS = {
  "handle-colors": 60_000,  // 1 min (colors rarely change)
  "version": 30_000,        // 30s (git state)
  "handles": 60_000,        // 1 min
  "mood": 30_000,           // 30s
  "tv": 30_000,             // 30s
  "keeps-config": 300_000,  // 5 min (contract addresses)
  "kidlisp-count": 60_000,  // 1 min
  "playlist": 60_000,       // 1 min
  "clock": 0,               // never cache (it's a clock)
};

// Clean expired entries every 30s
setInterval(() => {
  const now = Date.now();
  for (const [k, v] of responseCache) {
    if (v.expires < now) responseCache.delete(k);
  }
}, 30_000);

// --- Function stats & error log ---
const fnStats = {};    // { fnName: { calls, errors, totalMs, lastCall, lastError } }
const errorLog = [];   // [{ time, fn, status, error, path, method }]
const requestLog = []; // [{ time, fn, ms, status, path, method }]
const MAX_ERROR_LOG = 500;
const MAX_REQUEST_LOG = 1000;

function recordCall(name, ms, status, path, method, error) {
  if (!fnStats[name]) fnStats[name] = { calls: 0, errors: 0, totalMs: 0, lastCall: null, lastError: null };
  const s = fnStats[name];
  s.calls++;
  s.totalMs += ms;
  s.lastCall = new Date().toISOString();

  requestLog.unshift({ time: s.lastCall, fn: name, ms: Math.round(ms), status, path, method });
  if (requestLog.length > MAX_REQUEST_LOG) requestLog.length = MAX_REQUEST_LOG;

  if (error || status >= 500) {
    s.errors++;
    s.lastError = new Date().toISOString();
    errorLog.unshift({ time: s.lastError, fn: name, status, error: error || `HTTP ${status}`, path, method });
    if (errorLog.length > MAX_ERROR_LOG) errorLog.length = MAX_ERROR_LOG;
  }
}

function captureRawBody(req, _res, buf) {
  if (buf?.length) req.rawBody = Buffer.from(buf);
}

// --- Body parsing ---
app.use(express.json({ limit: "50mb", verify: captureRawBody }));
app.use(express.urlencoded({ extended: true, limit: "50mb", verify: captureRawBody }));
app.use(express.raw({ type: "*/*", limit: "50mb", verify: captureRawBody }));

// --- CORS (mirrors Netlify _headers) ---
app.use((req, res, next) => {
  res.set("Access-Control-Allow-Origin", "*");
  res.set(
    "Access-Control-Allow-Headers",
    "Content-Type, Authorization, X-Requested-With",
  );
  res.set("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS");
  if (req.method === "OPTIONS") return res.sendStatus(204);
  next();
});
// --- Host-based rewrites that Netlify previously handled ---
app.use((req, _res, next) => {
  const host = (req.headers.host || "").split(":")[0].toLowerCase();

  // Preserve branded notepat.com URLs while serving the /notepat piece.
  if ((host === "notepat.com" || host === "www.notepat.com") && req.path === "/") {
    req.url = "/notepat" + (req.url === "/" ? "" : req.url.slice(req.path.length));
  }

  next();
});

// --- Load Netlify functions ---
const functions = {};

// Scripts that call process.exit() at import time — not API functions.
const SKIP = new Set(["backfill-painting-codes", "test-tv-hits"]);

for (const file of readdirSync(FN_DIR)) {
  if (!file.endsWith(".mjs") && !file.endsWith(".js")) continue;
  const name = file.replace(/\.(mjs|js)$/, "");
  if (SKIP.has(name)) continue;
  try {
    const mod = await import(join(FN_DIR, file));
    if (mod.handler) {
      // Netlify Functions v1: export { handler }
      functions[name] = mod.handler;
    } else if (mod.default && typeof mod.default === "function") {
      // Netlify Functions v2: export default async (req) => { ... }
      // Wrap v2 handler to match v1 event/context signature
      const v2fn = mod.default;
      functions[name] = async (event, context) => {
        // V2 functions receive a Request-like object; build one from the event
        const url = event.rawUrl || `http://localhost${event.path || "/"}`;
        const req = new Request(url, {
          method: event.httpMethod,
          headers: event.headers,
          body: event.httpMethod !== "GET" && event.httpMethod !== "HEAD" ? event.body : undefined,
        });
        req.query = event.queryStringParameters;
        const resp = await v2fn(req, context);
        // V2 returns a Web Response object
        const body = await resp.text();
        const headers = {};
        resp.headers.forEach((v, k) => { headers[k] = v; });
        return { statusCode: resp.status, headers, body };
      };
    }
  } catch (err) {
    console.warn(`  skip: ${name} (${err.message})`);
  }
}

console.log(`Loaded ${Object.keys(functions).length} functions`);

// --- Netlify event adapter ---
function toEvent(req) {
  // Reconstruct body as string (Netlify handlers expect string or null).
  // Prefer rawBody when available — it preserves the exact bytes the client
  // sent, which is critical for webhook signature verification (Stripe, etc.).
  let body = null;
  if (req.rawBody) {
    body = Buffer.isBuffer(req.rawBody)
      ? req.rawBody.toString("utf-8")
      : String(req.rawBody);
  } else if (req.body) {
    const contentType = (req.headers["content-type"] || "").toLowerCase();
    body =
      typeof req.body === "string"
        ? req.body
        : Buffer.isBuffer(req.body)
          ? req.body.toString("utf-8")
          // Preserve HTML form posts as urlencoded strings so legacy handlers
          // using URLSearchParams(event.body) continue to work after lith.
          : contentType.includes("application/x-www-form-urlencoded")
            ? new URLSearchParams(
                Object.entries(req.body).flatMap(([key, value]) =>
                  Array.isArray(value)
                    ? value.map((item) => [key, item])
                    : [[key, value]],
                ),
              ).toString()
          : JSON.stringify(req.body);
  }

  return {
    httpMethod: req.method,
    headers: req.headers,
    body,
    rawBody: req.rawBody ?? req.body,
    queryStringParameters: req.query || {},
    path: req.path,
    rawUrl: `${req.protocol}://${req.get("host")}${req.originalUrl}`,
    isBase64Encoded: false,
  };
}

// --- Function handler ---
async function handleFunction(req, res) {
  const name = req.params.fn;
  const handler = functions[name];
  if (!handler) {
    recordCall(name || "unknown", 0, 404, req.path, req.method, "Function not found");
    return res.status(404).send("Function not found: " + name);
  }

  // Check response cache (GET only, with matching query string)
  const ttl = CACHE_TTLS[name];
  if (ttl && req.method === "GET") {
    const cacheKey = `${name}:${req.originalUrl}`;
    const cached = responseCache.get(cacheKey);
    if (cached && cached.expires > Date.now()) {
      recordCall(name, 0, cached.statusCode, req.path, req.method, null);
      if (cached.headers) res.set(cached.headers);
      res.set("X-Lith-Cache", "HIT");
      return res.status(cached.statusCode).send(cached.body);
    }
  }

  const t0 = Date.now();
  try {
    const event = toEvent(req);
    const context = { clientContext: {} };
    const result = await handler(event, context);

    const statusCode = result.statusCode || 200;
    const ms = Date.now() - t0;
    recordCall(name, ms, statusCode, req.path, req.method, statusCode >= 500 ? result.body : null);

    if (result.headers) res.set(result.headers);
    if (result.multiValueHeaders) {
      for (const [k, vals] of Object.entries(result.multiValueHeaders)) {
        for (const v of vals) res.append(k, v);
      }
    }

    // Handle ReadableStream bodies (from streaming functions like ask, keep-mint)
    if (result.body && typeof result.body === "object" && typeof result.body.getReader === "function") {
      res.status(statusCode);
      const reader = result.body.getReader();
      const pump = async () => {
        while (true) {
          const { done, value } = await reader.read();
          if (done) { res.end(); return; }
          res.write(value);
        }
      };
      return pump().catch((err) => {
        console.error(`fn/${name} stream error:`, err);
        res.end();
      });
    }

    // Store in cache if cacheable
    if (ttl && req.method === "GET" && statusCode < 400) {
      const cacheKey = `${name}:${req.originalUrl}`;
      responseCache.set(cacheKey, {
        body: result.isBase64Encoded ? Buffer.from(result.body, "base64") : result.body,
        headers: result.headers,
        statusCode,
        expires: Date.now() + ttl,
      });
    }

    if (result.isBase64Encoded) {
      res.status(statusCode).send(Buffer.from(result.body, "base64"));
    } else {
      res.status(statusCode).send(result.body);
    }
  } catch (err) {
    const ms = Date.now() - t0;
    recordCall(name, ms, 500, req.path, req.method, err.message);
    console.error(`fn/${name} error:`, err);
    res.status(500).send("Internal Server Error");
  }
}

// Resolve function name from URL params
function resolveFunction(req) {
  return resolveFunctionName(req.params.fn, req.params.rest, functions);
}

// --- Function handler (updated to use resolveFunction) ---
async function handleFunctionResolved(req, res) {
  req.params.fn = resolveFunction(req);
  return handleFunction(req, res);
}

// --- Deploy webhook (POST /lith/deploy?secret=...) ---
import { execFile } from "child_process";
import { createHmac, timingSafeEqual } from "crypto";
const DEPLOY_SECRET = process.env.DEPLOY_SECRET || "";
const DEPLOY_BRANCHES = (process.env.DEPLOY_BRANCHES || process.env.DEPLOY_BRANCH || "main,master")
  .split(",")
  .map((branch) => branch.trim())
  .filter(Boolean);
const DEFAULT_DEPLOY_BRANCH = DEPLOY_BRANCHES[0] || "main";
let deployInProgress = false;
let queuedDeployBranch = null;

function normalizeDeployBranch(branch) {
  if (typeof branch !== "string") return null;
  const trimmed = branch.trim();
  if (!trimmed) return null;
  if (!/^[A-Za-z0-9._/-]+$/.test(trimmed)) return null;
  return trimmed;
}

function branchFromRef(ref) {
  if (typeof ref !== "string") return null;
  const prefix = "refs/heads/";
  if (!ref.startsWith(prefix)) return null;
  return normalizeDeployBranch(ref.slice(prefix.length));
}

function requestedDeployBranch(req) {
  const fromRef = branchFromRef(req.body?.ref);
  if (fromRef) return fromRef;
  return (
    normalizeDeployBranch(req.query.branch) ||
    normalizeDeployBranch(req.headers["x-deploy-branch"]) ||
    DEFAULT_DEPLOY_BRANCH
  );
}

function verifyDeploy(req) {
  // GitHub HMAC signature (webhook secret)
  const sig = req.headers["x-hub-signature-256"];
  if (sig && DEPLOY_SECRET) {
    const rawBody = Buffer.isBuffer(req.rawBody)
      ? req.rawBody
      : Buffer.from(
          typeof req.body === "string" ? req.body : JSON.stringify(req.body ?? {}),
          "utf8",
        );
    const hmac = createHmac("sha256", DEPLOY_SECRET)
      .update(rawBody)
      .digest("hex");
    const expected = `sha256=${hmac}`;
    if (sig.length === expected.length &&
        timingSafeEqual(Buffer.from(sig), Buffer.from(expected))) {
      return true;
    }
  }
  // Fallback: query param or header (manual triggers)
  const plain = req.query.secret || req.headers["x-deploy-secret"];
  return plain === DEPLOY_SECRET;
}

function runDeploy(branch) {
  deployInProgress = true;
  console.log(`[deploy] starting branch=${branch}`);

  execFile(
    "/opt/ac/lith/webhook.sh",
    {
      timeout: 120000,
      env: { ...process.env, DEPLOY_BRANCH: branch },
    },
    (err, stdout, stderr) => {
      deployInProgress = false;

      if (stdout?.trim()) {
        console.log(`[deploy][${branch}] ${stdout.trim()}`);
      }
      if (stderr?.trim()) {
        console.error(`[deploy][${branch}] ${stderr.trim()}`);
      }
      if (err) {
        console.error(`[deploy] failed for ${branch}:`, err.message);
      }

      if (queuedDeployBranch) {
        const nextBranch = queuedDeployBranch;
        queuedDeployBranch = null;
        setImmediate(() => runDeploy(nextBranch));
      }
    },
  );
}

app.post("/lith/deploy", (req, res) => {
  if (!DEPLOY_SECRET || !verifyDeploy(req)) {
    return res.status(401).send("Unauthorized");
  }

  const githubEvent = req.headers["x-github-event"];
  if (githubEvent === "ping") {
    return res.send("pong");
  }
  if (githubEvent && githubEvent !== "push") {
    return res.send(`Ignored GitHub event: ${githubEvent}`);
  }

  const ref = req.body?.ref;
  const branch = requestedDeployBranch(req);
  if (!DEPLOY_BRANCHES.includes(branch)) {
    const detail = ref || branch;
    return res.send(`Ignored non-deploy branch: ${detail}`);
  }

  if (deployInProgress) {
    queuedDeployBranch = branch;
    return res.status(202).send(`Deploy queued for ${branch}`);
  }

  runDeploy(branch);
  res.status(202).send(`Deploy started for ${branch}`);
});

// --- Routes ---

app.get(["/lith", "/lith/"], (_req, res) => {
  res.redirect(302, "/lith/stats");
});

// --- Lith stats API (consumed by silo dashboard) ---
app.get("/lith/stats", (req, res) => {
  const uptime = Math.floor((Date.now() - BOOT_TIME) / 1000);
  const mem = process.memoryUsage();
  const sorted = Object.entries(fnStats)
    .map(([name, s]) => ({ name, ...s, avgMs: s.calls ? Math.round(s.totalMs / s.calls) : 0 }))
    .sort((a, b) => b.calls - a.calls);

  res.json({
    uptime,
    boot: new Date(BOOT_TIME).toISOString(),
    functionsLoaded: Object.keys(functions).length,
    memory: { rss: Math.round(mem.rss / 1048576), heap: Math.round(mem.heapUsed / 1048576) },
    totals: {
      calls: sorted.reduce((s, f) => s + f.calls, 0),
      errors: sorted.reduce((s, f) => s + f.errors, 0),
    },
    functions: sorted,
  });
});

app.get("/lith/errors", (req, res) => {
  const limit = Math.min(parseInt(req.query.limit) || 100, MAX_ERROR_LOG);
  res.json({ errors: errorLog.slice(0, limit), total: errorLog.length });
});

app.get("/lith/requests", (req, res) => {
  const limit = Math.min(parseInt(req.query.limit) || 100, MAX_REQUEST_LOG);
  const fn = req.query.fn;
  const filtered = fn ? requestLog.filter((r) => r.fn === fn) : requestLog;
  res.json({ requests: filtered.slice(0, limit), total: filtered.length });
});

// --- Caddy access log summary (for silo dashboard) ---
app.get("/lith/traffic", async (req, res) => {
  try {
    const logPath = "/var/log/caddy/access.log";
    const lines = readFileSync(logPath, "utf-8").trim().split("\n").filter(Boolean);
    const recent = lines.slice(-500); // last 500 entries
    const byPath = {}, byHost = {}, byStatus = {};
    let total = 0;

    for (const line of recent) {
      try {
        const d = JSON.parse(line);
        const r = d.request || {};
        const uri = (r.uri || "/").split("?")[0];
        const host = r.host || "unknown";
        const status = String(d.status || 0);
        // Aggregate by first path segment
        const seg = "/" + (uri.split("/")[1] || "");
        byPath[seg] = (byPath[seg] || 0) + 1;
        byHost[host] = (byHost[host] || 0) + 1;
        byStatus[status] = (byStatus[status] || 0) + 1;
        total++;
      } catch {}
    }

    const sortDesc = (obj) => Object.entries(obj).sort((a, b) => b[1] - a[1]);
    res.json({
      total,
      logLines: lines.length,
      byPath: sortDesc(byPath).slice(0, 30),
      byHost: sortDesc(byHost).slice(0, 20),
      byStatus: sortDesc(byStatus),
    });
  } catch (err) {
    res.json({ total: 0, error: err.message });
  }
});

// --- Farcaster Frame endpoint for KidLisp pieces ---
app.get("/frame/:piece", async (req, res) => {
  const piece = req.params.piece.startsWith("$") ? req.params.piece : `$${req.params.piece}`;
  const code = piece.slice(1);
  const base = "https://aesthetic.computer";
  const pieceUrl = `${base}/${piece}`;
  const keepUrl = `https://keep.kidlisp.com/${code}`;

  // Try to get thumbnail from oven cache
  const thumbUrl = `https://oven.aesthetic.computer/grab/webp/600/400/${piece}`;
  // Fallback OG image
  const ogImage = `https://oven.aesthetic.computer/kidlisp-og.png`;

  const frameEmbed = JSON.stringify({
    version: "1",
    imageUrl: thumbUrl,
    button: {
      title: `View ${piece}`,
      action: {
        type: "launch_frame",
        url: pieceUrl,
        name: `KidLisp ${piece}`,
        splashImageUrl: "https://assets.aesthetic.computer/kidlisp-favicon.gif",
        splashBackgroundColor: "#000000",
      },
    },
  });

  res.setHeader("Content-Type", "text/html; charset=utf-8");
  res.send(`<!DOCTYPE html>
<html>
<head>
  <meta charset="utf-8">
  <meta property="og:title" content="${piece} — KidLisp" />
  <meta property="og:description" content="A KidLisp piece on Aesthetic Computer" />
  <meta property="og:image" content="${thumbUrl}" />
  <meta property="og:url" content="${pieceUrl}" />
  <meta property="fc:frame" content='${frameEmbed.replace(/'/g, "&#39;")}' />
  <meta name="fc:frame" content='${frameEmbed.replace(/'/g, "&#39;")}' />
  <title>${piece} — KidLisp</title>
</head>
<body>
  <h1>${piece}</h1>
  <p><a href="${pieceUrl}">View on Aesthetic Computer</a></p>
  <p><a href="${keepUrl}">Keep on KidLisp</a></p>
</body>
</html>`);
});

// --- /api/os-release-upload (ports Netlify edge function os-release-upload.js) ---
app.post("/api/os-release-upload", async (req, res) => {
  const { createHmac } = await import("crypto");

  // Auth: verify AC token
  const authHeader = req.headers["authorization"] || "";
  const token = authHeader.startsWith("Bearer ") ? authHeader.slice(7).trim() : "";
  if (!token) return res.status(401).json({ error: "Missing Authorization: Bearer <ac_token>" });

  let user;
  try {
    const uiRes = await fetch("https://hi.aesthetic.computer/userinfo", {
      headers: { Authorization: `Bearer ${token}` },
    });
    if (!uiRes.ok) throw new Error(`Auth0 ${uiRes.status}`);
    user = await uiRes.json();
  } catch (err) {
    return res.status(401).json({ error: `Auth failed: ${err.message}` });
  }

  const userSub = user.sub || "unknown";
  const userName = user.name || user.nickname || userSub;

  const accessKey = process.env.DO_SPACES_KEY || process.env.ART_KEY;
  const secretKey = process.env.DO_SPACES_SECRET || process.env.ART_SECRET;
  if (!accessKey || !secretKey) return res.status(503).json({ error: "Spaces creds not configured" });

  const bucket = "releases-aesthetic-computer";
  const host = `${bucket}.sfo3.digitaloceanspaces.com`;

  const buildName = req.headers["x-build-name"] || `upload-${Date.now()}`;
  const gitHash = req.headers["x-git-hash"] || "unknown";
  const buildTs = req.headers["x-build-ts"] || new Date().toISOString().slice(0, 16);
  const commitMsg = req.headers["x-commit-msg"] || "";
  const version = `${buildName} ${gitHash}-${buildTs}`;

  function presignUrl(key, contentType, expiresSec = 900) {
    const expires = Math.floor(Date.now() / 1000) + expiresSec;
    const stringToSign = `PUT\n\n${contentType}\n${expires}\nx-amz-acl:public-read\n/${bucket}/${key}`;
    const sig = createHmac("sha1", secretKey).update(stringToSign).digest("base64");
    return `https://${host}/${key}?AWSAccessKeyId=${encodeURIComponent(accessKey)}&Expires=${expires}&Signature=${encodeURIComponent(sig)}&x-amz-acl=public-read`;
  }

  async function s3Put(key, body, contentType) {
    const dateStr = new Date().toUTCString();
    const stringToSign = `PUT\n\n${contentType}\n${dateStr}\nx-amz-acl:public-read\n/${bucket}/${key}`;
    const sig = createHmac("sha1", secretKey).update(stringToSign).digest("base64");
    const putRes = await fetch(`https://${host}/${key}`, {
      method: "PUT",
      headers: { Date: dateStr, "Content-Type": contentType, "x-amz-acl": "public-read", Authorization: `AWS ${accessKey}:${sig}` },
      body: typeof body === "string" ? body : body,
    });
    if (!putRes.ok) {
      const text = await putRes.text();
      throw new Error(`S3 PUT ${key}: ${putRes.status} ${text.slice(0, 200)}`);
    }
  }

  async function loadMachineTokenSecret() {
    try {
      const connStr = process.env.MONGODB_CONNECTION_STRING;
      if (!connStr) return null;
      const { MongoClient } = await import("mongodb");
      const client = new MongoClient(connStr);
      await client.connect();
      const dbName = process.env.MONGODB_NAME || "aesthetic";
      const doc = await client.db(dbName).collection("secrets").findOne({ _id: "machine-token" });
      await client.close();
      return doc?.secret || null;
    } catch (e) {
      console.error("[os-release-upload] Failed to load machine-token secret:", e.message);
      return null;
    }
  }

  async function generateDeviceToken(sub, handle) {
    const secret = await loadMachineTokenSecret();
    if (!secret) return null;
    const payload = { sub, handle, iat: Math.floor(Date.now() / 1000) };
    const payloadB64 = Buffer.from(JSON.stringify(payload)).toString("base64url");
    const sigB64 = createHmac("sha256", secret).update(payloadB64).digest("base64url");
    return `${payloadB64}.${sigB64}`;
  }

  const isFinalize = req.headers["x-finalize"] === "true";

  if (isFinalize) {
    const sha256 = req.headers["x-sha256"] || "unknown";
    const size = parseInt(req.headers["x-size"] || "0", 10);
    try {
      const versionWithSize = `${version}\n${size}`;
      await Promise.all([
        s3Put("os/native-notepat-latest.version", versionWithSize, "text/plain"),
        s3Put("os/native-notepat-latest.sha256", sha256, "text/plain"),
      ]);
      let releases = { releases: [] };
      try {
        const existing = await fetch(`https://${host}/os/releases.json`);
        if (existing.ok) releases = await existing.json();
      } catch { /* first release */ }
      const userHandle = req.headers["x-handle"] || user.nickname || user.name || userName;
      releases.releases = releases.releases || [];
      for (const r of releases.releases) r.deprecated = true;
      releases.releases.unshift({
        version, name: buildName, sha256, size, git_hash: gitHash, build_ts: buildTs,
        commit_msg: commitMsg, user: userSub, handle: userHandle,
        url: `https://${host}/os/native-notepat-latest.vmlinuz`,
        archive_url: `https://${host}/os/builds/${buildName}.vmlinuz`,
      });
      releases.releases = releases.releases.slice(0, 50);
      releases.latest = version;
      releases.latest_name = buildName;
      const deviceToken = await generateDeviceToken(userSub, userHandle);
      if (deviceToken) releases.device_token = deviceToken;
      await s3Put("os/releases.json", JSON.stringify(releases, null, 2), "application/json");
      return res.json({ ok: true, name: buildName, version, sha256, size, url: `https://${host}/os/native-notepat-latest.vmlinuz`, user: userSub, userName, deviceToken: !!deviceToken });
    } catch (err) {
      return res.status(500).json({ error: `Finalize failed: ${err.message}` });
    }
  }

  if (req.headers["x-versioned-upload"] === "true") {
    try {
      const versionedKey = req.headers["x-versioned-key"] || `os/builds/${buildName}.vmlinuz`;
      return res.json({ step: "versioned-upload", versioned_put_url: presignUrl(versionedKey, "application/octet-stream", 1800), key: versionedKey, user: userSub });
    } catch (err) {
      return res.status(500).json({ error: `Versioned presign failed: ${err.message}` });
    }
  }

  if (req.headers["x-manifest-upload"] === "true") {
    try {
      return res.json({ step: "manifest-upload", manifest_put_url: presignUrl("os/latest-manifest.json", "application/json"), user: userSub });
    } catch (err) {
      return res.status(500).json({ error: `Manifest presign failed: ${err.message}` });
    }
  }

  if (req.headers["x-template-upload"] === "true") {
    try {
      return res.json({ step: "template-upload", image_put_url: presignUrl("os/native-notepat-latest.img", "application/octet-stream"), user: userSub });
    } catch (err) {
      return res.status(500).json({ error: `Template presign failed: ${err.message}` });
    }
  }

  // Step 1: Return presigned URL for vmlinuz upload
  try {
    return res.json({ step: "upload", vmlinuz_put_url: presignUrl("os/native-notepat-latest.vmlinuz", "application/octet-stream"), version, user: userSub, userName });
  } catch (err) {
    return res.status(500).json({ error: `Presign failed: ${err.message}` });
  }
});

// --- /api/os-image (ports Netlify edge function os-image.js) ---
app.get("/api/os-image", async (req, res) => {
  const authHeader = req.headers["authorization"] || "";
  if (!authHeader) return res.status(401).json({ error: "Authorization required. Log in at aesthetic.computer first." });

  try {
    const search = new URLSearchParams(req.query || {}).toString();
    const ovenUrl = "https://oven.aesthetic.computer/os-image" + (search ? `?${search}` : "");
    const ovenRes = await fetch(ovenUrl, {
      headers: { Authorization: authHeader },
    });
    res.status(ovenRes.status);
    res.set("Content-Type", ovenRes.headers.get("content-type") || "application/octet-stream");
    if (ovenRes.headers.get("content-disposition")) res.set("Content-Disposition", ovenRes.headers.get("content-disposition"));
    if (ovenRes.headers.get("content-length")) res.set("Content-Length", ovenRes.headers.get("content-length"));
    if (ovenRes.headers.get("x-ac-os-requested-layout")) res.set("X-AC-OS-Requested-Layout", ovenRes.headers.get("x-ac-os-requested-layout"));
    if (ovenRes.headers.get("x-ac-os-layout")) res.set("X-AC-OS-Layout", ovenRes.headers.get("x-ac-os-layout"));
    if (ovenRes.headers.get("x-ac-os-fallback")) res.set("X-AC-OS-Fallback", ovenRes.headers.get("x-ac-os-fallback"));
    if (ovenRes.headers.get("x-ac-os-fallback-reason")) res.set("X-AC-OS-Fallback-Reason", ovenRes.headers.get("x-ac-os-fallback-reason"));
    res.set("Access-Control-Allow-Origin", "*");
    const { Readable } = await import("stream");
    Readable.fromWeb(ovenRes.body).pipe(res);
  } catch (err) {
    return res.status(502).json({ error: `Oven unavailable: ${err.message}` });
  }
});

// --- /media/* handler (ports Netlify edge function media.js) ---
app.all("/media/*rest", async (req, res) => {
  const parts = req.path.split("/").filter(Boolean); // ["media", ...]
  parts.shift(); // remove "media"
  const resourcePath = parts.join("/");

  if (!resourcePath) return res.status(404).send("Missing media path");

  // Content type from extension
  const ext = resourcePath.split(".").pop()?.toLowerCase();
  const ctMap = { png: "image/png", jpg: "image/jpeg", jpeg: "image/jpeg", gif: "image/gif", webp: "image/webp", zip: "application/zip", mp4: "video/mp4", json: "application/json", mjs: "text/javascript", svg: "image/svg+xml" };

  // Helper: build a clean event for calling functions internally
  function mediaEvent(path, query) {
    return {
      httpMethod: "GET",
      headers: req.headers,
      body: null,
      queryStringParameters: query,
      path,
      rawUrl: `${req.protocol}://${req.get("host")}${path}`,
      isBase64Encoded: false,
    };
  }

  // /media/tapes/CODE → get-tape function → redirect to DO Spaces
  if (parts[0] === "tapes" && parts[1]) {
    const code = parts[1].replace(/\.zip$/, "");
    try {
      const result = await functions["get-tape"](mediaEvent("/api/get-tape", { code }), {});
      if (result.statusCode === 200) {
        const tape = JSON.parse(result.body);
        const bucket = tape.bucket || "art-aesthetic-computer";
        const key = tape.user ? `${tape.user}/${tape.slug}.zip` : `${tape.slug}.zip`;
        return res.redirect(302, `https://${bucket}.sfo3.digitaloceanspaces.com/${key}`);
      }
    } catch {}
    return res.status(404).send("Tape not found");
  }

  // /media/paintings/CODE → get-painting function → redirect
  if (parts[0] === "paintings" && parts[1]) {
    const code = parts[1].replace(/\.(png|zip)$/, "");
    try {
      const result = await functions["get-painting"]?.(mediaEvent("/api/get-painting", { code }), {});
      if (result?.statusCode === 200) {
        const painting = JSON.parse(result.body);
        const bucket = painting.user ? "user-aesthetic-computer" : "art-aesthetic-computer";
        const slug = painting.slug?.split(":")[0] || painting.slug;
        const key = painting.user ? `${painting.user}/${slug}.png` : `${slug}.png`;
        return res.redirect(302, `https://${bucket}.sfo3.digitaloceanspaces.com/${key}`);
      }
    } catch {}
    return res.status(404).send("Painting not found");
  }

  // /media/@handle/type/slug → resolve user ID → redirect to DO Spaces
  if (parts[0]?.startsWith("@") || parts[0]?.match(/^ac[a-z0-9]+$/i)) {
    const userIdentifier = parts[0];
    const subPath = parts.slice(1).join("/");

    // Resolve user ID via user function directly
    try {
      const query = userIdentifier.match(/^ac[a-z0-9]+$/i)
        ? { code: userIdentifier }
        : { from: userIdentifier };
      const event = {
        httpMethod: "GET",
        headers: req.headers,
        body: null,
        queryStringParameters: query,
        path: "/user",
        rawUrl: `${req.protocol}://${req.get("host")}/user`,
        isBase64Encoded: false,
      };
      const result = await functions["user"](event, {});
      if (result.statusCode === 200) {
        const user = JSON.parse(result.body);
        const userId = user.sub;
        if (userId) {
          const fullPath = `${userId}/${subPath}`;
          const baseUrl = ext === "mjs"
            ? "https://user-aesthetic-computer.sfo3.digitaloceanspaces.com"
            : "https://user.aesthetic.computer";
          const encoded = fullPath.split("/").map(encodeURIComponent).join("/");
          return res.redirect(302, `${baseUrl}/${encoded}`);
        }
      }
    } catch (err) {
      console.error("media user resolve error:", err.message);
    }
    return res.status(404).send("User media not found");
  }

  // Direct file path → proxy to DO Spaces
  const baseUrl = ext === "mjs"
    ? "https://user-aesthetic-computer.sfo3.digitaloceanspaces.com"
    : "https://user.aesthetic.computer";
  const encoded = resourcePath.split("/").map(encodeURIComponent).join("/");
  return res.redirect(302, `${baseUrl}/${encoded}`);
});

// API functions (matches Netlify redirect rules)
app.all("/api/:fn", handleFunctionResolved);
app.all("/api/:fn/*rest", handleFunctionResolved);
app.all("/.netlify/functions/:fn", handleFunction);

// Non-/api/ function routes (from netlify.toml)
function directFn(fnName) {
  return (req, res) => { req.params = { fn: fnName }; return handleFunction(req, res); };
}
app.all("/handle", directFn("handle"));
app.all("/user", directFn("user"));
app.all("/run", directFn("run"));
app.all("/reload/*rest", directFn("reload"));
app.all("/session/*rest", directFn("session"));
app.all("/authorized", directFn("authorized"));
app.all("/handles", directFn("handles"));
app.all("/redirect-proxy", directFn("redirect-proxy"));
app.all("/redirect-proxy-sotce", directFn("redirect-proxy"));
// Local dev upload fallback (used when S3 credentials are missing).
app.all("/local-upload/:filename", (req, res) => {
  if (req.method === "OPTIONS") return res.sendStatus(204);
  const body = req.rawBody || req.body;
  if (!body || body.length === 0) {
    console.error("❌ Local upload: empty body for", req.params.filename);
    return res.status(400).send("Empty body");
  }
  const dir = join(dirname(fileURLToPath(import.meta.url)), "..", "local-uploads");
  mkdirSync(dir, { recursive: true });
  const filepath = join(dir, req.params.filename);
  writeFileSync(filepath, body);
  console.log("📁 Local upload saved:", filepath, `(${body.length} bytes)`);
  res.status(200).send("OK");
});
app.use("/local-uploads", express.static(join(dirname(fileURLToPath(import.meta.url)), "..", "local-uploads")));
app.all("/presigned-upload-url/*rest", directFn("presigned-url"));
app.all("/presigned-download-url/*rest", directFn("presigned-url"));
app.all("/docs", directFn("docs"));
app.all("/docs.json", directFn("docs"));
app.all("/docs/*rest", directFn("docs"));
app.all("/media-collection", directFn("media-collection"));
app.all("/media-collection/*rest", directFn("media-collection"));
app.all("/device-login", directFn("device-login"));
app.all("/device-auth", directFn("device-auth"));
app.all("/mcp", directFn("mcp-remote"));
app.all("/m4l-plugins", directFn("m4l-plugins"));
app.all("/slash", directFn("slash"));
app.all("/sotce-blog/*rest", directFn("sotce-blog"));
app.all("/profile/*rest", directFn("profile"));

// Static files
app.use(express.static(PUBLIC, { extensions: ["html"], dotfiles: "allow" }));

// --- keeps-social: SSR meta tags for social crawlers on keep/buy.kidlisp.com ---
const CRAWLER_RE = /twitterbot|facebookexternalhit|linkedinbot|slackbot|discordbot|telegrambot|whatsapp|applebot/i;
const OBJKT_GRAPHQL = "https://data.objkt.com/v3/graphql";

async function keepsSocialMiddleware(req, res, next) {
  const host = (req.headers.host || "").split(":")[0].toLowerCase();
  const isBuy = host.includes("buy.kidlisp.com");
  const isKeep = host.includes("keep.kidlisp.com");
  if (!isBuy && !isKeep) return next();

  const seg = req.path.replace(/^\/+/, "").split("/")[0];
  if (!seg.startsWith("$") || seg.length < 2) return next();

  const ua = req.headers["user-agent"] || "";
  if (!CRAWLER_RE.test(ua)) return next();

  const code = seg.slice(1);
  try {
    const [tokenData, ogImage] = await Promise.all([
      fetchKeepsTokenData(code),
      resolveKeepsImageUrl(`https://oven.aesthetic.computer/preview/1200x630/$${code}.png`),
    ]);

    // Get the HTML from the index function
    if (!functions["index"]) return next();
    const event = toEvent(req);
    const result = await functions["index"](event, { clientContext: {} });
    let html = result.body || "";

    const title = `$${code}`;
    const subdomain = isBuy ? "buy" : "keep";
    const description = buildKeepsDescription(tokenData, isBuy);
    const permalink = `https://${subdomain}.kidlisp.com/$${code}`;

    html = html.replace(/<meta property="og:url"[^>]*\/>/, `<meta property="og:url" content="${permalink}" />`);
    html = html.replace(/<meta property="og:title"[^>]*\/>/, `<meta property="og:title" content="${escapeAttr(title)}" />`);
    html = html.replace(/<meta property="og:description"[^>]*\/>/, `<meta property="og:description" content="${escapeAttr(description)}" />`);
    html = html.replace(/<meta property="og:image" content="[^"]*"[^>]*\/>/, `<meta property="og:image" content="${ogImage}" />`);
    html = html.replace(/<meta name="twitter:title"[^>]*\/>/, `<meta name="twitter:title" content="${escapeAttr(title)}" />`);
    html = html.replace(/<meta name="twitter:description"[^>]*\/>/, `<meta name="twitter:description" content="${escapeAttr(description)}" />`);
    html = html.replace(/<meta name="twitter:image" content="[^"]*"[^>]*\/>/, `<meta name="twitter:image" content="${ogImage}" />`);

    res.set("Content-Type", "text/html; charset=utf-8");
    res.set("Cache-Control", "public, max-age=3600");
    return res.status(200).send(html);
  } catch (err) {
    console.error("[keeps-social] error:", err);
    return next();
  }
}

async function fetchKeepsTokenData(code) {
  const contract = "KT1Q1irsjSZ7EfUN4qHzAB2t7xLBPsAWYwBB";
  const query = `query { token(where: { fa_contract: { _eq: "${contract}" } name: { _eq: "$${code}" } }) { token_id name thumbnail_uri } listing_active(where: { fa_contract: { _eq: "${contract}" } token: { name: { _eq: "$${code}" } } } order_by: { price_xtz: asc } limit: 1) { price_xtz seller_address } }`;
  const r = await fetch(OBJKT_GRAPHQL, { method: "POST", headers: { "Content-Type": "application/json" }, body: JSON.stringify({ query }) });
  if (!r.ok) return null;
  const json = await r.json();
  const tokens = json?.data?.token || [];
  if (tokens.length === 0) return null;
  return { token: tokens[0], listing: (json?.data?.listing_active || [])[0] || null };
}

function buildKeepsDescription(tokenData, isBuy) {
  if (!tokenData) return isBuy ? "Buy KidLisp generative art on Tezos." : "KidLisp generative art preserved on Tezos.";
  const { listing } = tokenData;
  if (listing) {
    const xtz = (Number(listing.price_xtz) / 1_000_000).toFixed(2);
    return isBuy ? `Buy now — ${xtz} XTZ | KidLisp generative art on Tezos` : `For Sale — ${xtz} XTZ | KidLisp generative art on Tezos`;
  }
  return isBuy ? "Buy KidLisp generative art on Tezos." : "KidLisp generative art preserved on Tezos.";
}

function escapeAttr(str) {
  return str.replace(/&/g, "&amp;").replace(/"/g, "&quot;").replace(/</g, "&lt;").replace(/>/g, "&gt;");
}

async function resolveKeepsImageUrl(url) {
  try {
    const r = await fetch(url, { method: "HEAD", redirect: "follow" });
    if (r.ok && r.url) return r.url;
  } catch (e) {
    console.error("[keeps-social] image resolve error:", e);
  }
  return url;
}

app.use(keepsSocialMiddleware);

// SPA fallback → index function
app.use(async (req, res) => {
  if (functions["index"]) {
    req.params = { fn: "index" };
    return handleFunction(req, res);
  }
  res.status(404).send("Not found");
});

// --- Start server ---
let server;
if (DEV && HAS_SSL) {
  const opts = {
    cert: readFileSync(SSL_CERT),
    key: readFileSync(SSL_KEY),
  };
  server = createHttpsServer(opts, app).listen(PORT, () => {
    console.log(`lith listening on https://localhost:${PORT}`);
  });
} else {
  server = createHttpServer(app).listen(PORT, () => {
    console.log(`lith listening on http://localhost:${PORT}`);
  });
}

// --- Graceful shutdown ---
// On SIGTERM (sent by systemctl restart), stop accepting new connections
// and wait for in-flight requests to finish before exiting.
const DRAIN_TIMEOUT = 10_000; // 10s max wait

function gracefulShutdown(signal) {
  console.log(`[lith] ${signal} received, draining connections...`);
  server.close(() => {
    console.log("[lith] all connections drained, exiting");
    process.exit(0);
  });
  // Force exit if connections don't drain in time
  setTimeout(() => {
    console.warn("[lith] drain timeout, forcing exit");
    process.exit(1);
  }, DRAIN_TIMEOUT).unref();
}

process.on("SIGTERM", () => gracefulShutdown("SIGTERM"));
process.on("SIGINT", () => gracefulShutdown("SIGINT"));
