#!/usr/bin/env node
// photo-mcp.mjs — private, read-only LLM context for Jeffrey's Photos library.

import { httpPort, serveHttp, serveStdio } from "../../toolchain/mcp/http-front.mjs";
import { allSongSheets, makeSheet, photoStatus, searchPhotos, songBriefs, songCandidates } from "./photo-platter.mjs";

const INSTRUCTIONS = "Private local photo context. Search is read-only and excludes people names, OCR, screenshots, hidden/deleted media, videos, and precise location. Do not infer permission to publish, upload, or identify people. Inspect only candidates relevant to the user's request.";

const TOOLS = [
  {
    name: "photo_status",
    description: "Report aggregate availability for the private local Photos platter without returning photo content.",
    inputSchema: { type: "object", properties: {} },
  },
  {
    name: "photo_search",
    description: "Search Jeffrey's private local Photos library using Apple scene labels and user-authored metadata. Returns compact candidate metadata and local paths; calling this deliberately brings only those results into model context.",
    inputSchema: {
      type: "object",
      properties: {
        query: { type: "string" },
        limit: { type: "integer", minimum: 1, maximum: 100, default: 20 },
      },
      required: ["query"],
    },
  },
  {
    name: "photo_sheet",
    description: "Create a private square-crop contact sheet for a scene query and return its local path plus numbered candidate manifest. SIDE EFFECT: writes private thumbnails beneath the local photo-platter state directory; originals are unchanged.",
    inputSchema: {
      type: "object",
      properties: {
        query: { type: "string" },
        limit: { type: "integer", minimum: 1, maximum: 30, default: 12 },
      },
      required: ["query"],
    },
  },
  {
    name: "photo_song_candidates",
    description: "Find private photo candidates for one released /pop song using its narrow diegetic cover brief. Pass sheet:true to create an inspectable contact sheet.",
    inputSchema: {
      type: "object",
      properties: {
        song: { type: "string" },
        limit: { type: "integer", minimum: 1, maximum: 30, default: 12 },
        sheet: { type: "boolean", default: false },
      },
      required: ["song"],
    },
  },
  {
    name: "photo_song_briefs",
    description: "List the released /pop songs and their diegetic photo-search contracts without opening the Photos library.",
    inputSchema: { type: "object", properties: {} },
  },
];

function text(value) { return [{ type: "text", text: typeof value === "string" ? value : JSON.stringify(value, null, 2) }]; }

async function callTool(name, args = {}) {
  if (name === "photo_status") return text(await photoStatus());
  if (name === "photo_search") return text(await searchPhotos(args.query, { limit: args.limit }));
  if (name === "photo_sheet") return text(await makeSheet(args.query, { limit: args.limit || 12 }));
  if (name === "photo_song_briefs") return text(await songBriefs());
  if (name === "photo_song_candidates") {
    const result = await songCandidates(args.song, { limit: args.limit || 12 });
    if (!args.sheet) return text(result);
    return text({ brief: result.brief, ...(await makeSheet(result.brief.terms, { limit: args.limit || 12, slug: result.brief.slug, candidates: result.candidates })) });
  }
  if (name === "photo_all_song_sheets") return text(await allSongSheets({ limit: args.limit || 12 }));
  throw new Error(`Unknown tool: ${name}`);
}

async function handleMessage(message) {
  const { id, method, params } = message;
  try {
    if (method === "initialize") return { jsonrpc: "2.0", id, result: { protocolVersion: "2024-11-05", capabilities: { tools: {} }, serverInfo: { name: "photo-mcp", version: "1.0.0" }, instructions: INSTRUCTIONS } };
    if (method === "initialized" || method === "notifications/initialized") return null;
    if (method === "ping") return { jsonrpc: "2.0", id, result: {} };
    if (method === "tools/list") return { jsonrpc: "2.0", id, result: { tools: TOOLS } };
    if (method === "tools/call") return { jsonrpc: "2.0", id, result: { content: await callTool(params?.name, params?.arguments || {}) } };
    return { jsonrpc: "2.0", id, error: { code: -32601, message: `Method not found: ${method}` } };
  } catch (error) {
    if (method === "tools/call") return { jsonrpc: "2.0", id, result: { isError: true, content: text(String(error.message || error)) } };
    return { jsonrpc: "2.0", id, error: { code: -32000, message: String(error.message || error) } };
  }
}

const port = httpPort(process.argv, 7779);
if (port) serveHttp({ handleMessage, port, banner: "📷 photo-mcp shared daemon" });
else serveStdio({ handleMessage, banner: "📷 photo-mcp started (photo_status, photo_search, photo_sheet, photo_song_candidates, photo_song_briefs)" });
