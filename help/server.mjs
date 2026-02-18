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

// --- Config ---
const AUTH0_DOMAIN = process.env.AUTH0_DOMAIN || "aesthetic.us.auth0.com";
const AUTH0_CLIENT_ID = process.env.AUTH0_CLIENT_ID || "";
const AUTH0_CUSTOM_DOMAIN = "hi.aesthetic.computer";
const ADMIN_SUB = process.env.ADMIN_SUB || "";
const ANTHROPIC_API_KEY = process.env.ANTHROPIC_API_KEY || "";
const MODEL = "claude-haiku-4-5-20251001";
const DAILY_LIMIT = 50; // messages per handle per day

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
const authCache = new Map();
const AUTH_CACHE_TTL = 300_000;

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

// --- Rate Limiting ---
async function checkRateLimit(handle) {
  if (!db) return { ok: true, used: 0, limit: DAILY_LIMIT };
  const today = new Date().toISOString().slice(0, 10); // YYYY-MM-DD UTC
  const doc = await db.collection("help-usage").findOneAndUpdate(
    { _id: `${handle}:${today}` },
    { $inc: { count: 1 }, $setOnInsert: { handle, date: today } },
    { upsert: true, returnDocument: "after" }
  );
  const used = doc.count;
  return { ok: used <= DAILY_LIMIT, used, limit: DAILY_LIMIT };
};

// --- System Prompt ---
const SYSTEM_PROMPT = `You are a helpful AI assistant for Aesthetic Computer (aesthetic.computer), a mobile-first creative computing platform.

You help users with:
- Writing pieces in JavaScript (.mjs) — the core lifecycle functions: boot, paint, act, sim, leave
- KidLisp (.lisp) — a minimal Lisp dialect for generative art with ~118 built-in functions
- The piece API: graphics (wipe, ink, line, box, circle), text (write, type), input (event, pen, hand), audio (sound, speaker), UI (ui.Button, ui.TextInput), system (screen, params, store, net, jump, send)
- Event handling: event.is("keyboard:down:space"), event.is("touch"), event.is("lift"), event.is("draw")
- Publishing, forking, sharing pieces
- General creative computing and generative art concepts

Keep answers concise and practical. When showing code, use valid AC piece syntax. For KidLisp, output only valid .lisp code without markdown fencing unless illustrating a point.`;

// --- Routes ---
app.use(express.json());

app.get("/", (req, res) => {
  res.setHeader("Content-Type", "text/html");
  res.send(indexHtml);
});

app.get("/health", (req, res) => {
  res.json({ status: "ok", uptime: Date.now() - SERVER_START_TIME, mongo: !!db });
});

app.get("/auth/config", (req, res) => {
  res.json({ domain: AUTH0_CUSTOM_DOMAIN, clientId: AUTH0_CLIENT_ID });
});

app.get("/auth/me", async (req, res) => {
  const auth = await validateToken(req.headers.authorization);
  if (!auth) return res.status(401).json({ error: "Unauthorized" });
  res.json({ handle: auth.handle, isAdmin: auth.isAdmin });
});

// --- Chat (streaming) ---
app.post("/api/chat", requireAuth, async (req, res) => {
  if (!ANTHROPIC_API_KEY) return res.status(500).json({ error: "API not configured" });

  const { messages } = req.body;
  if (!Array.isArray(messages) || messages.length === 0)
    return res.status(400).json({ error: "messages required" });

  const handle = req.auth.handle || req.auth.user.sub;
  const rate = await checkRateLimit(handle);
  if (!rate.ok) {
    return res.status(429).json({
      error: `Daily limit reached (${rate.limit} messages/day). Try again tomorrow.`,
    });
  }

  // Only keep user/assistant messages; inject system prompt server-side
  const chatMessages = messages.filter(m => m.role !== "system").slice(-20); // last 20 turns

  const payload = {
    model: MODEL,
    system: SYSTEM_PROMPT,
    messages: chatMessages,
    max_tokens: 2048,
    temperature: 1,
    stream: true,
  };

  let anthropicRes;
  try {
    anthropicRes = await fetch("https://api.anthropic.com/v1/messages", {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
        "x-api-key": ANTHROPIC_API_KEY,
        "anthropic-version": "2023-06-01",
      },
      body: JSON.stringify(payload),
    });
  } catch (err) {
    return res.status(502).json({ error: "Anthropic unreachable" });
  }

  if (!anthropicRes.ok) {
    const text = await anthropicRes.text();
    return res.status(anthropicRes.status).json({ error: text });
  }

  // Stream plain text back to client
  res.setHeader("Content-Type", "text/plain; charset=utf-8");
  res.setHeader("X-Rate-Limit-Used", rate.used);
  res.setHeader("X-Rate-Limit-Total", rate.limit);

  const reader = anthropicRes.body.getReader();
  const decoder = new TextDecoder();
  let buf = "";

  try {
    while (true) {
      const { done, value } = await reader.read();
      if (done) break;
      buf += decoder.decode(value, { stream: true });
      const lines = buf.split("\n");
      buf = lines.pop();
      for (const line of lines) {
        if (!line.startsWith("data: ")) continue;
        const data = line.slice(6).trim();
        if (data === "[DONE]") break;
        try {
          const evt = JSON.parse(data);
          if (evt.type === "content_block_delta" && evt.delta?.type === "text_delta") {
            res.write(evt.delta.text);
          }
        } catch { /* skip malformed */ }
      }
    }
  } finally {
    res.end();
  }
});

// Usage stats for the UI
app.get("/api/usage", requireAuth, async (req, res) => {
  const handle = req.auth.handle || req.auth.user.sub;
  if (!db) return res.json({ used: 0, limit: DAILY_LIMIT });
  const today = new Date().toISOString().slice(0, 10);
  const doc = await db.collection("help-usage").findOne({ _id: `${handle}:${today}` });
  res.json({ used: doc?.count || 0, limit: DAILY_LIMIT });
});

// --- Start ---
loadIndex();
process.on("SIGHUP", () => { console.log("SIGHUP — reloading index"); loadIndex(); });
await connectMongo();
http.createServer(app).listen(PORT, () => console.log(`help listening on :${PORT}`));
