#!/usr/bin/env node
// Expose cached PostHog plugin skills through three small, on-demand MCP tools.

import { existsSync, readFileSync, readdirSync, statSync } from "node:fs";
import { homedir } from "node:os";
import { join, relative, resolve, sep } from "node:path";
import * as readline from "node:readline";

const CACHE_ROOT = resolve(
  process.env.POSTHOG_PLUGIN_CACHE ||
    join(homedir(), ".codex", "plugins", "cache", "posthog", "posthog"),
);

function latestSkillsRoot() {
  if (!existsSync(CACHE_ROOT)) throw new Error(`PostHog plugin cache not found: ${CACHE_ROOT}`);
  const versions = readdirSync(CACHE_ROOT, { withFileTypes: true })
    .filter((entry) => entry.isDirectory() && existsSync(join(CACHE_ROOT, entry.name, "skills")))
    .map((entry) => entry.name)
    .sort((a, b) => b.localeCompare(a, undefined, { numeric: true }));
  if (!versions.length) throw new Error(`No cached PostHog plugin version found under ${CACHE_ROOT}`);
  return join(CACHE_ROOT, versions[0], "skills");
}

function unquote(value) {
  const trimmed = value.trim();
  if ((trimmed.startsWith('"') && trimmed.endsWith('"')) ||
      (trimmed.startsWith("'") && trimmed.endsWith("'"))) return trimmed.slice(1, -1);
  return trimmed;
}

function metadata(markdown, fallbackName) {
  const match = markdown.match(/^---\r?\n([\s\S]*?)\r?\n---(?:\r?\n|$)/);
  if (!match) return { name: fallbackName, description: "" };
  const lines = match[1].split(/\r?\n/);
  let name = fallbackName;
  let description = "";
  for (let index = 0; index < lines.length; index += 1) {
    const nameMatch = lines[index].match(/^name:\s*(.*)$/);
    if (nameMatch) name = unquote(nameMatch[1]) || fallbackName;
    const descriptionMatch = lines[index].match(/^description:\s*(.*)$/);
    if (!descriptionMatch) continue;
    const value = descriptionMatch[1].trim();
    if (!/^[>|][+-]?$/.test(value)) {
      description = unquote(value);
      continue;
    }
    const parts = [];
    while (index + 1 < lines.length && /^\s+/.test(lines[index + 1])) {
      parts.push(lines[index + 1].trim());
      index += 1;
    }
    description = parts.join(" ");
  }
  return { name, description };
}

function skillEntries() {
  const root = latestSkillsRoot();
  return readdirSync(root, { withFileTypes: true })
    .filter((entry) => entry.isDirectory() && existsSync(join(root, entry.name, "SKILL.md")))
    .map((entry) => {
      const path = join(root, entry.name, "SKILL.md");
      return { folder: entry.name, path, ...metadata(readFileSync(path, "utf8"), entry.name) };
    })
    .sort((a, b) => a.name.localeCompare(b.name));
}

function findSkill(name) {
  if (!/^[a-z0-9][a-z0-9-]*$/.test(String(name || ""))) throw new Error("name must be kebab-case");
  const entry = skillEntries().find((candidate) => candidate.name === name || candidate.folder === name);
  if (!entry) throw new Error(`Unknown PostHog skill: ${name}`);
  return entry;
}

function walkFiles(root, current = root, output = []) {
  for (const entry of readdirSync(current, { withFileTypes: true })) {
    const path = join(current, entry.name);
    if (entry.isDirectory()) walkFiles(root, path, output);
    else if (entry.isFile() && entry.name !== "SKILL.md") output.push(relative(root, path));
  }
  return output;
}

function safeFile(skillRoot, requested) {
  const path = resolve(skillRoot, String(requested || ""));
  const prefix = `${resolve(skillRoot)}${sep}`;
  if (!path.startsWith(prefix) || !existsSync(path) || !statSync(path).isFile()) {
    throw new Error(`Unknown bundled file: ${requested}`);
  }
  return path;
}

const TOOLS = [
  {
    name: "posthog_skill_list",
    description: "Search and rank cached PostHog workflow names and descriptions without loading their bodies.",
    inputSchema: { type: "object", properties: { search: { type: "string", description: "Natural keyword query." }, limit: { type: "integer", minimum: 1, maximum: 50, default: 12 } } },
  },
  {
    name: "posthog_skill_get",
    description: "Load one cached PostHog SKILL.md and its bundled-file manifest on demand.",
    inputSchema: { type: "object", properties: { name: { type: "string" } }, required: ["name"] },
  },
  {
    name: "posthog_skill_file_get",
    description: "Load one file referenced by a cached PostHog skill.",
    inputSchema: { type: "object", properties: { name: { type: "string" }, path: { type: "string" } }, required: ["name", "path"] },
  },
];

function callTool(name, args = {}) {
  if (name === "posthog_skill_list") {
    const terms = String(args.search || "").toLowerCase().split(/\s+/).filter(Boolean);
    const limit = Math.min(Math.max(Number(args.limit) || 12, 1), 50);
    const results = skillEntries()
      .map((entry) => {
        const nameText = entry.name.toLowerCase();
        const descriptionText = entry.description.toLowerCase();
        const score = terms.reduce((total, term) => total +
          (nameText.includes(term) ? 3 : 0) + (descriptionText.includes(term) ? 1 : 0), 0);
        return { ...entry, score };
      })
      .filter((entry) => !terms.length || entry.score > 0)
      .sort((a, b) => b.score - a.score || a.name.localeCompare(b.name))
      .slice(0, limit)
      .map(({ name: skillName, description }) => ({ name: skillName, description }));
    return { count: results.length, results };
  }
  if (name === "posthog_skill_get") {
    const entry = findSkill(args.name);
    const root = resolve(entry.path, "..");
    return { name: entry.name, description: entry.description, skill: readFileSync(entry.path, "utf8"), files: walkFiles(root).sort() };
  }
  if (name === "posthog_skill_file_get") {
    const entry = findSkill(args.name);
    const root = resolve(entry.path, "..");
    const path = safeFile(root, args.path);
    const size = statSync(path).size;
    if (size > 1_000_000) throw new Error(`Bundled file exceeds 1 MB: ${args.path}`);
    return { name: entry.name, path: relative(root, path), content: readFileSync(path, "utf8") };
  }
  throw new Error(`Unknown tool: ${name}`);
}

async function handle(message) {
  const { id, method, params } = message;
  try {
    if (method === "initialize") return { jsonrpc: "2.0", id, result: { protocolVersion: "2024-11-05", capabilities: { tools: {} }, serverInfo: { name: "posthog-skills", version: "0.1.0" } } };
    if (method === "initialized" || method === "notifications/initialized") return null;
    if (method === "ping") return { jsonrpc: "2.0", id, result: {} };
    if (method === "tools/list") return { jsonrpc: "2.0", id, result: { tools: TOOLS } };
    if (method === "tools/call") {
      const value = callTool(params?.name, params?.arguments || {});
      return { jsonrpc: "2.0", id, result: { content: [{ type: "text", text: JSON.stringify(value, null, 2) }] } };
    }
    return { jsonrpc: "2.0", id, error: { code: -32601, message: `Method not found: ${method}` } };
  } catch (error) {
    return { jsonrpc: "2.0", id, result: { isError: true, content: [{ type: "text", text: String(error.message || error) }] } };
  }
}

const rl = readline.createInterface({ input: process.stdin, terminal: false });
rl.on("line", async (line) => {
  if (!line.trim()) return;
  try {
    const response = await handle(JSON.parse(line));
    if (response) process.stdout.write(`${JSON.stringify(response)}\n`);
  } catch (error) {
    process.stdout.write(`${JSON.stringify({ jsonrpc: "2.0", id: null, error: { code: -32700, message: String(error.message || error) } })}\n`);
  }
});
