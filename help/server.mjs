#!/usr/bin/env node
// help - AI assistant for aesthetic.computer

import "dotenv/config";
import express from "express";
import http from "http";
import fs from "fs";
import path from "path";
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

// --- Start ---
loadIndex();

// Reload index on SIGHUP (zero-downtime dashboard update)
process.on("SIGHUP", () => {
  console.log("SIGHUP — reloading index");
  loadIndex();
});

await connectMongo();

const server = http.createServer(app);
server.listen(PORT, () => {
  console.log(`help listening on :${PORT}`);
});
