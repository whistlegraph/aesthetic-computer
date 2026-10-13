#!/usr/bin/env node
// choir-mcp.mjs — Fía's choir as tools: tester bug reports (with screenshots),
// the live event trail, and what's in Discover.
//
// Reads https://aesthetic.computer/api/choir with the admin AC login in
// ~/.ac-token (toolchain/mcp/ac-token.mjs). Bugs are filed from the app's red
// ladybug, which only @fifi sees (apple/choir/Sources/BugReport.swift).

import { serveStdio, serveHttp, httpPort } from "./http-front.mjs";
import { loadTokens, UA } from "./ac-token.mjs";

const API = process.env.CHOIR_API || "https://aesthetic.computer/api/choir";

async function call(query, { method = "GET", body, raw = false } = {}) {
  const { access_token } = await loadTokens();
  const r = await fetch(API + query, {
    method, headers: { Authorization: `Bearer ${access_token}`, "User-Agent": UA, ...(body ? { "Content-Type": "application/json" } : {}) },
    ...(body ? { body: JSON.stringify(body) } : {}),
  });
  if (raw) { if (!r.ok) throw new Error(`HTTP ${r.status}`); return Buffer.from(await r.arrayBuffer()); }
  const d = await r.json().catch(() => ({}));
  if (!r.ok) throw new Error(d.error || `HTTP ${r.status}`);
  return d;
}

const TOOLS = [
  { name: "choir_bugs", description: "List bug reports Fía filed from choir's red ladybug: id, when, who, her note, status, app build/device, whether there is a screenshot. Newest first.",
    inputSchema: { type: "object", properties: { status: { type: "string", enum: ["open", "fixed", "wontfix", "all"], description: "default open" }, limit: { type: "number", description: "default 20, max 100" } } } },
  { name: "choir_bug", description: "One bug report in full, with its screenshot attached as an image so you can see what Fía saw.",
    inputSchema: { type: "object", required: ["id"], properties: { id: { type: "string" } } } },
  { name: "choir_bug_update", description: "Mark a bug open, fixed or wontfix, with an optional note (e.g. the commit that fixed it).",
    inputSchema: { type: "object", required: ["id", "status"], properties: { id: { type: "string" }, status: { type: "string", enum: ["open", "fixed", "wontfix"] }, note: { type: "string" } } } },
  { name: "choir_events", description: "The live trail of what people do in choir (sign-ins, posts, pushes, curator runs, bug filings), newest first.",
    inputSchema: { type: "object", properties: { limit: { type: "number", description: "default 50, max 500" }, since: { type: "string", description: "ISO time" } } } },
  { name: "choir_discover", description: "What is in choir's Discover feed now: news and open calls (title, source, deadline).",
    inputSchema: { type: "object", properties: { limit: { type: "number", description: "default 30, max 100" } } } },
];

const text = (v) => [{ type: "text", text: typeof v === "string" ? v : JSON.stringify(v, null, 1) }];

async function callTool(name, args = {}) {
  if (name === "choir_bugs") {
    const { bugs } = await call("?bugs=1");
    const status = args.status || "open";
    const rows = bugs.filter((b) => status === "all" || (b.status || "open") === status).slice(0, Math.min(args.limit || 20, 100));
    return text(rows.map((b) => ({ id: b._id, at: b.at, from: b.handle, note: b.text, status: b.status || "open", fixNote: b.note, screenshot: b.hasImage, app: b.context })));
  }
  if (name === "choir_bug") {
    const id = String(args.id || "");
    const { bugs } = await call(`?bug=${encodeURIComponent(id)}&bugs=1`);
    const b = bugs[0];
    if (!b) throw new Error("No such bug");
    const content = text({ id: b._id, at: b.at, from: b.handle, note: b.text, status: b.status || "open", fixNote: b.note, app: b.context, session: b.context?.session });
    if (b.hasImage) {
      const jpg = await call(`?bug=${encodeURIComponent(id)}&image=1`, { raw: true });
      content.push({ type: "image", data: jpg.toString("base64"), mimeType: "image/jpeg" });
    }
    return content;
  }
  if (name === "choir_bug_update") return text(await call(`?bug=${encodeURIComponent(String(args.id))}`, { method: "PUT", body: { status: args.status, note: args.note } }));
  if (name === "choir_events") {
    const q = `?events=1&limit=${Math.min(args.limit || 50, 500)}${args.since ? `&since=${encodeURIComponent(args.since)}` : ""}`;
    return text((await call(q)).events);
  }
  if (name === "choir_discover") {
    const { posts } = await call(`?news=1&limit=${Math.min(args.limit || 30, 100)}`);
    return text(posts.map((p) => ({ title: p.preview?.title || p.text.split("\n")[0], url: p.links?.[0], source: p.preview?.siteName, kind: p.kind || "news", deadline: p.deadline, when: p.when, comments: p.replies })));
  }
  throw new Error(`Unknown tool: ${name}`);
}

async function handleMessage(message) {
  const { id, method, params } = message;
  try {
    switch (method) {
      case "initialize":
        return { jsonrpc: "2.0", id, result: { protocolVersion: params?.protocolVersion || "2024-11-05", capabilities: { tools: {} }, serverInfo: { name: "choir-mcp", version: "1.0.0" } } };
      case "notifications/initialized": return null;
      case "ping": return { jsonrpc: "2.0", id, result: {} };
      case "tools/list": return { jsonrpc: "2.0", id, result: { tools: TOOLS } };
      case "tools/call": return { jsonrpc: "2.0", id, result: { content: await callTool(params?.name, params?.arguments) } };
      default: return { jsonrpc: "2.0", id, error: { code: -32601, message: `Method not found: ${method}` } };
    }
  } catch (error) {
    if (method === "tools/call") return { jsonrpc: "2.0", id, result: { isError: true, content: text(String(error.message || error)) } };
    return { jsonrpc: "2.0", id, error: { code: -32000, message: String(error.message || error) } };
  }
}

const port = httpPort(process.argv, 0);
if (port) serveHttp({ handleMessage, port, banner: "🌼 choir-mcp shared daemon" });
else serveStdio({ handleMessage, banner: "🌼 choir-mcp started (choir_bugs, choir_bug, choir_bug_update, choir_events, choir_discover)" });
