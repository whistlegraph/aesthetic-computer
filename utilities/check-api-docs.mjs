#!/usr/bin/env node

import { readFile, readdir } from "node:fs/promises";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { handler } from "../system/netlify/functions/api-docs.mjs";

const repoRoot = join(dirname(fileURLToPath(import.meta.url)), "..");

async function valuesFromSources(directory, pattern) {
  const values = [];
  for (const file of await readdir(join(repoRoot, directory))) {
    if (!file.endsWith(".ts")) continue;
    const source = await readFile(join(repoRoot, directory, file), "utf8");
    const match = source.match(pattern);
    if (!match) throw new Error(`Could not read ${pattern} from ${directory}/${file}`);
    values.push(match[1]);
  }
  return values.sort();
}

function assertEqual(label, actual, expected) {
  if (JSON.stringify(actual) !== JSON.stringify(expected)) {
    throw new Error(`${label} mismatch\nactual:   ${JSON.stringify(actual)}\nexpected: ${JSON.stringify(expected)}`);
  }
}

const response = await handler({ headers: {}, queryStringParameters: { format: "json" } }, {});
if (response.statusCode !== 200) throw new Error(`API docs returned ${response.statusCode}`);
const docs = JSON.parse(response.body);

assertEqual(
  "supported endpoints",
  docs.endpoints.map(({ method, path }) => `${method} ${path}`).sort(),
  [
    "GET /api/chat-messages",
    "POST /api/store-clock",
    "POST /api/store-kidlisp",
    "POST /api/store-piece",
    "POST /api/track-media",
  ],
);

assertEqual(
  "MCP tools",
  docs.mcp.tools.map(({ name }) => name).sort(),
  await valuesFromSources("mcp-server/src/tools", /name:\s*"([^"]+)"/),
);
assertEqual(
  "MCP resources",
  docs.mcp.resources.map(({ uri }) => uri).sort(),
  await valuesFromSources("mcp-server/src/resources", /uri:\s*"([^"]+)"/),
);
assertEqual(
  "MCP prompts",
  docs.mcp.prompts.map(({ name }) => name).sort(),
  await valuesFromSources("mcp-server/src/prompts", /name:\s*"([^"]+)"/),
);

for (const endpoint of docs.endpoints) {
  for (const example of endpoint.examples || []) {
    for (const language of ["curl", "javascript", "python"]) {
      if (!example[language]?.trim()) {
        throw new Error(`${endpoint.path} example "${example.title}" is missing ${language}`);
      }
    }
  }
}

for (const [client, configuration] of Object.entries(docs.mcp.configuration)) {
  const parsed = JSON.parse(configuration);
  const args = parsed.mcpServers?.["aesthetic-computer"]?.args;
  if (!args?.includes("@aesthetic.computer/mcp")) {
    throw new Error(`${client} configuration has the wrong npm package`);
  }
}

const jsonPathResponse = await handler({
  path: "/api-docs.json",
  headers: { accept: "text/html" },
  queryStringParameters: {},
}, {});
if (!jsonPathResponse.headers["Content-Type"].startsWith("application/json")) {
  throw new Error("/api-docs.json did not return JSON when the browser requested HTML");
}

console.log(`API docs are current: ${docs.endpoints.length} endpoints, ${docs.mcp.tools.length} MCP tools, ${docs.mcp.resources.length} resources, ${docs.mcp.prompts.length} prompts`);
