#!/usr/bin/env node
// help - AI assistant for aesthetic.computer

import "dotenv/config";
import express from "express";
import http from "http";
import fs from "fs";
import path from "path";
import crypto from "crypto";
import { fileURLToPath } from "url";
import { MongoClient } from "mongodb";

const __dirname = path.dirname(fileURLToPath(import.meta.url));

const app = express();
const PORT = process.env.PORT || 3004;
const SERVER_START_TIME = Date.now();

// --- Auth0 ---
const AUTH0_DOMAIN = process.env.AUTH0_DOMAIN || "aesthetic.us.auth0.com";
const AUTH0_CLIENT_ID = process.env.AUTH0_CLIENT_ID || "";
const AUTH0_CUSTOM_DOMAIN = "hi.aesthetic.computer";
const ADMIN_SUB = process.env.ADMIN_SUB || "";
const AUTH_CACHE_TTL = 300_000;
const authCache = new Map();

// --- Anthropic OAuth ---
const ANTHROPIC_CLIENT_ID = "9d1c250a-e61b-44d9-88ed-5944d1962f5e";
const ANTHROPIC_AUTH_URL = "https://claude.ai/oauth/authorize";
const ANTHROPIC_TOKEN_URL = "https://console.anthropic.com/v1/oauth/token";
const ANTHROPIC_REDIRECT_URI = "https://console.anthropic.com/oauth/code/callback";
const ANTHROPIC_SCOPES = "org:create_api_key user:profile user:inference";

// In-memory PKCE state store (keyed by state, expires after 10 min)
const pendingOAuth = new Map();
setInterval(() => {
  const now = Date.now();
  for (const [k, v] of pendingOAuth) {
    if (now - v.createdAt > 600_000) pendingOAuth.delete(k);
  }
}, 60_000);

function generatePKCE() {
  const verifier = crypto.randomBytes(32).toString("base64url");
  const challenge = crypto.createHash("sha256").update(verifier).digest("base64url");
  return { verifier, challenge };
}

// --- MongoDB ---
let db = null;

async function connectMongo() {
  const uri = process.env.MONGODB_CONNECTION_STRING;
  if (!uri) { console.log("no MONGODB_CONNECTION_STRING — running without db"); return; }
  try {
    const client = new MongoClient(uri);
    await client.connect();
    db = client.db(process.env.MONGODB_NAME || "aesthetic");
    console.log("mongodb connected");
  } catch (err) {
    console.error("mongodb error:", err.message);
  }
}

// --- Landing Page ---
const INDEX_PATH = path.join(__dirname, "index.html");
let indexHtml = "";

function loadIndex() {
  indexHtml = fs.readFileSync(INDEX_PATH, "utf-8");
  console.log(`index loaded (${(indexHtml.length / 1024).toFixed(0)} KB)`);
}

// --- Auth ---
async function validateToken(authorization) {
  if (!authorization) return null;
  const cached = authCache.get(authorization);
  if (cached && Date.now() - cached.timestamp < AUTH_CACHE_TTL) return cached;
  try {
    const resp = await fetch(`https://${AUTH0_DOMAIN}/userinfo`, {
      headers: { Authorization: authorization },
    });
    if (!resp.ok) return null;
    const user = await resp.json();
    let handle = null, isAdmin = false;
    if (db) {
      const doc = await db.collection("@handles").findOne({ _id: user.sub });
      handle = doc?.handle;
      isAdmin = !!(user.email_verified && handle === "jeffrey" && user.sub === ADMIN_SUB);
    }
    const result = { user, handle, isAdmin, timestamp: Date.now() };
    authCache.set(authorization, result);
    return result;
  } catch (err) {
    console.error("token validation failed:", err.message);
    return null;
  }
}

async function requireAuth(req, res, next) {
  const auth = await validateToken(req.headers.authorization);
  if (!auth) return res.status(401).json({ error: "Unauthorized" });
  req.auth = auth;
  next();
}

// --- Routes ---
app.use(express.json());

app.get("/", (req, res) => {
  res.setHeader("Content-Type", "text/html");
  res.send(indexHtml);
});

app.get("/health", (req, res) => {
  res.json({
    status: "ok",
    uptime: Date.now() - SERVER_START_TIME,
    mongo: !!db,
  });
});

app.get("/auth/config", (req, res) => {
  res.json({ domain: AUTH0_CUSTOM_DOMAIN, clientId: AUTH0_CLIENT_ID });
});

app.get("/auth/me", async (req, res) => {
  const auth = await validateToken(req.headers.authorization);
  if (!auth) return res.status(401).json({ error: "Unauthorized" });
  res.json({ handle: auth.handle, isAdmin: auth.isAdmin });
});

// --- Anthropic OAuth ---

// Step 1: generate PKCE + return the Anthropic authorization URL
app.get("/api/anthropic/auth-url", requireAuth, (req, res) => {
  const { verifier, challenge } = generatePKCE();
  const state = crypto.randomBytes(16).toString("hex");

  pendingOAuth.set(state, { verifier, createdAt: Date.now(), sub: req.auth.user.sub });

  const params = new URLSearchParams({
    client_id: ANTHROPIC_CLIENT_ID,
    response_type: "code",
    redirect_uri: ANTHROPIC_REDIRECT_URI,
    scope: ANTHROPIC_SCOPES,
    code_challenge: challenge,
    code_challenge_method: "S256",
    state,
  });

  res.json({ url: `${ANTHROPIC_AUTH_URL}?${params}`, state });
});

// Step 2: exchange authorization code for tokens, store in MongoDB
app.post("/api/anthropic/connect", requireAuth, async (req, res) => {
  const { code, state } = req.body;
  if (!code || !state) return res.status(400).json({ error: "Missing code or state" });

  const pending = pendingOAuth.get(state);
  if (!pending) return res.status(400).json({ error: "Invalid or expired state" });
  if (pending.sub !== req.auth.user.sub) return res.status(403).json({ error: "State mismatch" });

  pendingOAuth.delete(state);

  try {
    const resp = await fetch(ANTHROPIC_TOKEN_URL, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({
        grant_type: "authorization_code",
        client_id: ANTHROPIC_CLIENT_ID,
        code,
        redirect_uri: ANTHROPIC_REDIRECT_URI,
        code_verifier: pending.verifier,
      }),
    });

    if (!resp.ok) {
      const err = await resp.text();
      console.error("anthropic token exchange failed:", err);
      return res.status(400).json({ error: "Token exchange failed", detail: err });
    }

    const tokens = await resp.json();
    const expiresAt = Date.now() + (tokens.expires_in || 28800) * 1000;

    if (db) {
      await db.collection("help-anthropic-tokens").updateOne(
        { _id: req.auth.user.sub },
        {
          $set: {
            accessToken: tokens.access_token,
            refreshToken: tokens.refresh_token,
            expiresAt,
            connectedAt: new Date(),
            handle: req.auth.handle,
          },
        },
        { upsert: true }
      );
    }

    res.json({ ok: true });
  } catch (err) {
    console.error("anthropic connect error:", err.message);
    res.status(500).json({ error: err.message });
  }
});

// Check Anthropic connection status
app.get("/api/anthropic/status", requireAuth, async (req, res) => {
  if (!db) return res.json({ connected: false });
  const doc = await db.collection("help-anthropic-tokens").findOne(
    { _id: req.auth.user.sub },
    { projection: { expiresAt: 1, connectedAt: 1 } }
  );
  if (!doc) return res.json({ connected: false });
  res.json({ connected: true, expiresAt: doc.expiresAt, connectedAt: doc.connectedAt });
});

// Disconnect Anthropic account
app.post("/api/anthropic/disconnect", requireAuth, async (req, res) => {
  if (db) {
    await db.collection("help-anthropic-tokens").deleteOne({ _id: req.auth.user.sub });
  }
  res.json({ ok: true });
});

// --- Start ---
loadIndex();

process.on("SIGHUP", () => {
  console.log("SIGHUP — reloading index");
  loadIndex();
});

await connectMongo();

const server = http.createServer(app);
server.listen(PORT, () => {
  console.log(`help listening on :${PORT}`);
});
