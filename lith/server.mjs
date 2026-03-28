// lith — AC monolith server
// Wraps Netlify function handlers in Express routes + serves static files.

// Shim awslambda before anything imports @netlify/functions.
// Netlify's stream() calls awslambda.streamifyResponse() at wrap time,
// which doesn't exist outside AWS Lambda. This shim makes the wrapped
// handler just call the original function and return its result.
if (typeof globalThis.awslambda === "undefined") {
  globalThis.awslambda = {
    streamifyResponse: (fn) => fn,
    HttpResponseStream: {
      from: (stream, _metadata) => stream,
    },
  };
}

import express from "express";
import { readdirSync, readFileSync, existsSync } from "fs";
import { join, dirname } from "path";
import { fileURLToPath } from "url";
import { createServer as createHttpsServer } from "https";
import { createServer as createHttpServer } from "http";

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

// --- Body parsing ---
app.use(express.json({ limit: "50mb" }));
app.use(express.urlencoded({ extended: true, limit: "50mb" }));
app.use(express.raw({ type: "*/*", limit: "50mb" }));

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
  // Reconstruct body as string (Netlify handlers expect string or null)
  let body = null;
  if (req.body) {
    body =
      typeof req.body === "string"
        ? req.body
        : Buffer.isBuffer(req.body)
          ? req.body.toString("utf-8")
          : JSON.stringify(req.body);
  }

  return {
    httpMethod: req.method,
    headers: req.headers,
    body,
    rawBody: req.body,
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
  if (!handler) return res.status(404).send("Function not found: " + name);

  try {
    const event = toEvent(req);
    const context = { clientContext: {} };
    const result = await handler(event, context);

    const statusCode = result.statusCode || 200;
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

    if (result.isBase64Encoded) {
      res.status(statusCode).send(Buffer.from(result.body, "base64"));
    } else {
      res.status(statusCode).send(result.body);
    }
  } catch (err) {
    console.error(`fn/${name} error:`, err);
    res.status(500).send("Internal Server Error");
  }
}

// --- Route aliases (from netlify.toml where URL path ≠ function name) ---
const ROUTE_ALIASES = {
  "verify-password": "verify-builds-password",
  "pack-telemetry": "bundle-telemetry",
  "pack-telemetry-query": "bundle-telemetry-query",
  "track-tape": "track-media",
};

// --- Nested API routes (e.g. /api/chat/messages → chat-messages) ---
const NESTED_ROUTES = {
  "chat/messages": "chat-messages",
  "chat/heart": "chat-heart",
  "auth/cli-callback": "auth-cli-callback",
  "news/toll": "news-toll",
  // /api/news/* (posts, updates, submit, etc.) → news-api function
  "news/": "news-api",
};

// Resolve function name from URL params
function resolveFunction(req) {
  const fn = req.params.fn;
  const rest = req.params.rest;

  // Check nested routes first (e.g., /api/chat/messages)
  if (rest) {
    const nested = `${fn}/${rest}`;
    for (const [pattern, target] of Object.entries(NESTED_ROUTES)) {
      if (nested === pattern || nested.startsWith(pattern)) {
        return target;
      }
    }
  }

  // Check aliases
  if (ROUTE_ALIASES[fn]) return ROUTE_ALIASES[fn];

  // ff1 dynamic routing: /api/ff1-proxy, /api/ff1-pair, /api/ff1-devices
  if (fn === "ff1" && rest) {
    const subFn = `ff1-${rest.split("/")[0]}`;
    if (functions[subFn]) return subFn;
  }

  return fn;
}

// --- Function handler (updated to use resolveFunction) ---
async function handleFunctionResolved(req, res) {
  req.params.fn = resolveFunction(req);
  return handleFunction(req, res);
}

// --- Routes ---

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

// SPA fallback → index function
app.use(async (req, res) => {
  if (functions["index"]) {
    req.params = { fn: "index" };
    return handleFunction(req, res);
  }
  res.status(404).send("Not found");
});

// --- Start server ---
if (DEV && HAS_SSL) {
  const opts = {
    cert: readFileSync(SSL_CERT),
    key: readFileSync(SSL_KEY),
  };
  createHttpsServer(opts, app).listen(PORT, () => {
    console.log(`lith listening on https://localhost:${PORT}`);
  });
} else {
  createHttpServer(app).listen(PORT, () => {
    console.log(`lith listening on http://localhost:${PORT}`);
  });
}
