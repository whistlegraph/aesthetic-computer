#!/usr/bin/env node
// aa-bridge — phone-to-claude bridge for @jeffrey
//
// POST /api/chat with Auth0 bearer
//   → validates token against Auth0 /userinfo
//   → checks sub against ADMIN_SUB env var
//   → spawns `claude --print --output-format stream-json` (resumes per-user session)
//   → streams claude events back as Server-Sent Events
//
// POST /api/reset      — clear stored session for this user (bearer required)
// GET  /health         — liveness probe
// GET  /api/session    — return current stored session id for this user
// GET  /api/history    — return prior user/assistant events for this user's session

import http from "http";
import { spawn } from "child_process";
import { readFile, writeFile, mkdir } from "fs/promises";
import { existsSync } from "fs";
import { homedir } from "os";
import { dirname, join } from "path";
import { randomUUID } from "crypto";

const PORT = parseInt(process.env.AA_PORT || "3004", 10);
const ADMIN_SUB = process.env.ADMIN_SUB;
const AUTH0_DOMAIN = process.env.AUTH0_DOMAIN || "aesthetic.us.auth0.com";
const CLAUDE_BIN = process.env.CLAUDE_BIN || "claude";
const WORK_DIR = process.env.AA_WORK_DIR || join(homedir(), "aesthetic-computer");
const SESSION_FILE =
  process.env.AA_SESSION_FILE || join(homedir(), ".aa-bridge", "sessions.json");
const PERMISSION_MODE = process.env.AA_PERMISSION_MODE || "bypassPermissions";
const MODEL = process.env.AA_MODEL || ""; // "" = use claude default; "sonnet" / "haiku" / full id
const DEV_BYPASS = process.env.AA_DEV === "1";
const ALLOWED_ORIGINS = (
  process.env.AA_ALLOWED_ORIGINS ||
  "https://aesthetic.computer,https://hi.aesthetic.computer,http://localhost:8888"
).split(",");

if (!ADMIN_SUB && !DEV_BYPASS) {
  console.error("ADMIN_SUB env var required (e.g. auth0|...). Set AA_DEV=1 to bypass for local testing.");
  process.exit(1);
}

// ───────── Auth0 token validation (cached) ─────────
const tokenCache = new Map();
const TOKEN_TTL = 5 * 60 * 1000;

async function validateBearer(authHeader) {
  if (DEV_BYPASS) return ADMIN_SUB || "dev|local";
  if (!authHeader?.startsWith("Bearer ")) return null;
  const token = authHeader.slice(7);
  const cached = tokenCache.get(token);
  if (cached && cached.expires > Date.now()) return cached.sub;
  try {
    const res = await fetch(`https://${AUTH0_DOMAIN}/userinfo`, {
      headers: { Authorization: authHeader },
    });
    if (!res.ok) return null;
    const user = await res.json();
    tokenCache.set(token, { sub: user.sub, expires: Date.now() + TOKEN_TTL });
    return user.sub;
  } catch (err) {
    console.error("auth0 userinfo failed:", err.message);
    return null;
  }
}

// ───────── Per-user session id persistence ─────────
async function loadSessions() {
  if (!existsSync(SESSION_FILE)) return {};
  try {
    return JSON.parse(await readFile(SESSION_FILE, "utf8"));
  } catch {
    return {};
  }
}

async function saveSessions(sessions) {
  await mkdir(dirname(SESSION_FILE), { recursive: true });
  await writeFile(SESSION_FILE, JSON.stringify(sessions, null, 2));
}

async function getSessionId(sub) {
  const s = await loadSessions();
  return s[sub] || null;
}

async function setSessionId(sub, sessionId) {
  const s = await loadSessions();
  s[sub] = sessionId;
  await saveSessions(s);
}

async function clearSession(sub) {
  const s = await loadSessions();
  delete s[sub];
  await saveSessions(s);
}

// ───────── SSE helpers ─────────
function corsHeaders(origin) {
  const allow = ALLOWED_ORIGINS.includes(origin) ? origin : ALLOWED_ORIGINS[0];
  return {
    "Access-Control-Allow-Origin": allow,
    "Access-Control-Allow-Credentials": "true",
    "Access-Control-Allow-Headers": "Authorization,Content-Type",
    "Access-Control-Allow-Methods": "GET,POST,OPTIONS",
  };
}

function sse(res, event, data) {
  res.write(`event: ${event}\ndata: ${JSON.stringify(data)}\n\n`);
}

function readJsonBody(req) {
  return new Promise((resolve, reject) => {
    let body = "";
    req.on("data", (c) => (body += c));
    req.on("end", () => {
      if (!body) return resolve({});
      try {
        resolve(JSON.parse(body));
      } catch (err) {
        reject(err);
      }
    });
    req.on("error", reject);
  });
}

// ───────── session transcript reader ─────────
// Claude stores per-project session transcripts as JSONL here:
//   ~/.claude/projects/<slash-replaced-cwd>/<sessionId>.jsonl
// We filter to user + assistant events (skipping queue-operation, attachment,
// last-prompt etc.) and return the raw rows — aa.mjs does the rendering.
async function readSessionTranscript(sessionId) {
  if (!/^[a-f0-9-]{36}$/i.test(sessionId)) throw new Error("invalid session id");
  const projectDir = WORK_DIR.replace(/\//g, "-");
  const path = join(homedir(), ".claude", "projects", projectDir, `${sessionId}.jsonl`);
  if (!existsSync(path)) return [];
  const content = await readFile(path, "utf8");
  const events = [];
  for (const line of content.split("\n")) {
    if (!line.trim()) continue;
    try {
      const o = JSON.parse(line);
      if (o.type === "user" || o.type === "assistant") events.push(o);
    } catch {}
  }
  return events;
}

// ───────── claude spawn ─────────
//
// Git attribution: commits made through this bridge keep the *author* as
// whatever the cwd's git config says (@jeffrey), but set the *committer*
// to the aa-bridge endpoint. This preserves the standard
// "authored-by-X, committed-by-Y" semantics, and makes these commits
// trivially filterable via `git log --committer=aa-bridge`.
const COMMITTER_NAME = process.env.AA_GIT_COMMITTER_NAME || "aa-bridge";
const COMMITTER_EMAIL = process.env.AA_GIT_COMMITTER_EMAIL || "aa@aesthetic.computer";

function spawnClaude(message, sessionId) {
  const args = [
    "--print",
    "--output-format",
    "stream-json",
    "--verbose",
    "--permission-mode",
    PERMISSION_MODE,
  ];
  if (MODEL) args.push("--model", MODEL);
  if (sessionId) {
    args.push("--resume", sessionId);
  } else {
    args.push("--session-id", randomUUID());
  }
  args.push(message);
  return spawn(CLAUDE_BIN, args, {
    cwd: WORK_DIR,
    env: {
      ...process.env,
      AA_BRIDGE: "1",
      GIT_COMMITTER_NAME: COMMITTER_NAME,
      GIT_COMMITTER_EMAIL: COMMITTER_EMAIL,
    },
    stdio: ["ignore", "pipe", "pipe"],
  });
}

// ───────── /api/chat ─────────
async function handleChat(req, res, origin) {
  const sub = await validateBearer(req.headers.authorization);
  if (!sub) {
    res.writeHead(401, { "Content-Type": "application/json", ...corsHeaders(origin) });
    res.end(JSON.stringify({ error: "invalid token" }));
    return;
  }
  if (!DEV_BYPASS && sub !== ADMIN_SUB) {
    res.writeHead(403, { "Content-Type": "application/json", ...corsHeaders(origin) });
    res.end(JSON.stringify({ error: "not admin" }));
    return;
  }

  let payload;
  try {
    payload = await readJsonBody(req);
  } catch {
    res.writeHead(400, { "Content-Type": "application/json", ...corsHeaders(origin) });
    res.end(JSON.stringify({ error: "invalid json" }));
    return;
  }

  const message = payload.message;
  if (!message || typeof message !== "string") {
    res.writeHead(400, { "Content-Type": "application/json", ...corsHeaders(origin) });
    res.end(JSON.stringify({ error: "message (string) required" }));
    return;
  }
  if (payload.reset === true) await clearSession(sub);

  res.writeHead(200, {
    "Content-Type": "text/event-stream",
    "Cache-Control": "no-cache",
    Connection: "keep-alive",
    "X-Accel-Buffering": "no",
    ...corsHeaders(origin),
  });

  const sessionId = await getSessionId(sub);
  sse(res, "start", { sessionId, cwd: WORK_DIR });

  const child = spawnClaude(message, sessionId);
  let buffer = "";
  let detectedSessionId = sessionId;

  // Heartbeat to keep proxies from closing the SSE
  const heartbeat = setInterval(() => res.write(": ping\n\n"), 15_000);

  child.stdout.on("data", (chunk) => {
    buffer += chunk.toString();
    const lines = buffer.split("\n");
    buffer = lines.pop();
    for (const line of lines) {
      if (!line.trim()) continue;
      try {
        const event = JSON.parse(line);
        if (event.session_id) detectedSessionId = event.session_id;
        sse(res, "claude", event);
      } catch {
        sse(res, "claude", { type: "raw", text: line });
      }
    }
  });

  child.stderr.on("data", (chunk) => {
    sse(res, "stderr", { text: chunk.toString() });
  });

  child.on("close", async (code) => {
    clearInterval(heartbeat);
    if (detectedSessionId && detectedSessionId !== sessionId) {
      await setSessionId(sub, detectedSessionId);
    }
    sse(res, "done", { code, sessionId: detectedSessionId });
    res.end();
  });

  child.on("error", (err) => {
    clearInterval(heartbeat);
    sse(res, "error", { message: err.message });
    res.end();
  });

  req.on("close", () => {
    clearInterval(heartbeat);
    if (!child.killed) child.kill("SIGTERM");
  });
}

// ───────── server ─────────
const server = http.createServer(async (req, res) => {
  const origin = req.headers.origin || "";

  if (req.method === "OPTIONS") {
    res.writeHead(204, corsHeaders(origin));
    res.end();
    return;
  }

  if (req.url === "/health") {
    res.writeHead(200, { "Content-Type": "application/json", ...corsHeaders(origin) });
    res.end(
      JSON.stringify({
        ok: true,
        service: "aa-bridge",
        version: "0.1.0",
        cwd: WORK_DIR,
        permissionMode: PERMISSION_MODE,
        devBypass: DEV_BYPASS,
      }),
    );
    return;
  }

  if (req.url === "/api/chat" && req.method === "POST") {
    return handleChat(req, res, origin);
  }

  if (req.url === "/api/session" && req.method === "GET") {
    const sub = await validateBearer(req.headers.authorization);
    if (!sub || (!DEV_BYPASS && sub !== ADMIN_SUB)) {
      res.writeHead(sub ? 403 : 401, corsHeaders(origin));
      res.end();
      return;
    }
    const sid = await getSessionId(sub);
    res.writeHead(200, { "Content-Type": "application/json", ...corsHeaders(origin) });
    res.end(JSON.stringify({ sessionId: sid }));
    return;
  }

  if (req.url === "/api/history" && req.method === "GET") {
    const sub = await validateBearer(req.headers.authorization);
    if (!sub || (!DEV_BYPASS && sub !== ADMIN_SUB)) {
      res.writeHead(sub ? 403 : 401, corsHeaders(origin));
      res.end();
      return;
    }
    const sid = await getSessionId(sub);
    if (!sid) {
      res.writeHead(200, { "Content-Type": "application/json", ...corsHeaders(origin) });
      res.end(JSON.stringify({ sessionId: null, events: [] }));
      return;
    }
    try {
      const events = await readSessionTranscript(sid);
      res.writeHead(200, { "Content-Type": "application/json", ...corsHeaders(origin) });
      res.end(JSON.stringify({ sessionId: sid, events }));
    } catch (err) {
      res.writeHead(500, { "Content-Type": "application/json", ...corsHeaders(origin) });
      res.end(JSON.stringify({ error: err.message }));
    }
    return;
  }

  if (req.url === "/api/reset" && req.method === "POST") {
    const sub = await validateBearer(req.headers.authorization);
    if (!sub || (!DEV_BYPASS && sub !== ADMIN_SUB)) {
      res.writeHead(sub ? 403 : 401, corsHeaders(origin));
      res.end();
      return;
    }
    await clearSession(sub);
    res.writeHead(200, { "Content-Type": "application/json", ...corsHeaders(origin) });
    res.end(JSON.stringify({ ok: true }));
    return;
  }

  res.writeHead(404, corsHeaders(origin));
  res.end();
});

server.listen(PORT, () => {
  console.log(`aa-bridge listening on :${PORT}`);
  console.log(`  cwd:       ${WORK_DIR}`);
  console.log(`  claude:    ${CLAUDE_BIN}`);
  console.log(`  perm mode: ${PERMISSION_MODE}`);
  console.log(`  admin sub: ${ADMIN_SUB ? ADMIN_SUB.slice(0, 14) + "…" : "(dev bypass)"}`);
  console.log(`  sessions:  ${SESSION_FILE}`);
});
