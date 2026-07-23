#!/usr/bin/env node
// MCP front door for the local signed/notarized Electron DMG pipeline.

import { buildDmg, planDmg, verifyArtifact } from "./dmgify.mjs";
import { httpPort, serveHttp, serveStdio } from "../../toolchain/mcp/http-front.mjs";

const sharedProperties = {
  source: { type: "string", description: "Absolute local directory containing the offline HTML app." },
  entry: { type: "string", default: "index.html", description: "HTML entry path relative to source." },
  name: { type: "string", description: "macOS product/display name." },
  bundleId: { type: "string", description: "Reverse-DNS application identifier." },
  version: { type: "string", default: "1.0.0", description: "Semantic version." },
  icon: { type: "string", description: "Optional absolute PNG or ICNS icon path." },
  output: { type: "string", description: "Absolute output directory; defaults to <source>/release." },
  include: { type: "array", items: { type: "string" }, description: "Optional electron-builder glob allowlist/denylist for bundled source files." },
  category: { type: "string", default: "public.app-category.photography" },
};

const TOOLS = [
  {
    name: "dmgify_plan",
    description: "Validate and size a local offline HTML directory before packaging. Read-only: reports entry/icon/builder/certificate/credential readiness and payload size.",
    inputSchema: { type: "object", properties: sharedProperties, required: ["source"] },
  },
  {
    name: "dmgify_build",
    description: "Build a universal Electron app, sign it with the local Developer ID, notarize and staple the app, create/sign/notarize/staple a drag-to-Applications DMG, verify Gatekeeper, and write a receipt. SIDE EFFECTS: writes app/DMG artifacts and submits both to Apple's notary service.",
    inputSchema: {
      type: "object",
      properties: {
        ...sharedProperties,
        credentials: { type: "string", description: "Optional Apple env file; defaults to the Menu Band vault credential path." },
        notarize: { type: "boolean", default: true, description: "Keep true for distributable builds; false is an explicit local-only escape hatch." },
      },
      required: ["source", "name", "bundleId"],
    },
  },
  {
    name: "dmgify_verify",
    description: "Verify a local .app or .dmg signature, notarization staple, and Gatekeeper acceptance. Read-only.",
    inputSchema: { type: "object", properties: { path: { type: "string" } }, required: ["path"] },
  },
];

const text = (value) => [{ type: "text", text: typeof value === "string" ? value : JSON.stringify(value, null, 2) }];

async function callTool(name, args) {
  if (name === "dmgify_plan") return text(await planDmg(args));
  if (name === "dmgify_build") return text(await buildDmg(args));
  if (name === "dmgify_verify") return text(await verifyArtifact(args?.path));
  throw new Error(`Unknown tool: ${name}`);
}

async function handleMessage(message) {
  const { id, method, params } = message;
  try {
    if (method === "initialize") return { jsonrpc: "2.0", id, result: {
      protocolVersion: "2024-11-05", capabilities: { tools: {} },
      serverInfo: { name: "dmgify-mcp", version: "1.0.0" },
    } };
    if (method === "initialized" || method === "notifications/initialized") return null;
    if (method === "ping") return { jsonrpc: "2.0", id, result: {} };
    if (method === "tools/list") return { jsonrpc: "2.0", id, result: { tools: TOOLS } };
    if (method === "tools/call") return { jsonrpc: "2.0", id, result: { content: await callTool(params?.name, params?.arguments || {}) } };
    return { jsonrpc: "2.0", id, error: { code: -32601, message: `Method not found: ${method}` } };
  } catch (error) {
    if (method === "tools/call") return { jsonrpc: "2.0", id, result: {
      isError: true, content: [{ type: "text", text: String(error.message || error) }],
    } };
    return { jsonrpc: "2.0", id, error: { code: -32000, message: String(error.message || error) } };
  }
}

const port = httpPort(process.argv, 7781);
if (port) serveHttp({ handleMessage, port, banner: "💿 dmgify-mcp shared daemon" });
else serveStdio({ handleMessage, banner: "💿 dmgify started (dmgify_plan, dmgify_build, dmgify_verify)" });
