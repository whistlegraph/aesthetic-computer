#!/usr/bin/env node
// memory-mcp.mjs — an MCP server over the local-first agent memory, so any
// session can SEARCH OUR PROMPT HISTORY across the fleet by name, instead of
// remembering that `node memory/cli.mjs` exists on each Mac separately.
//
// The stores are local-first and encrypted per machine (AES-256-GCM under a key
// that never leaves the Mac that wrote it), so there is no central index to
// query. The fan-out inverts that: we ship the QUERY to each machine and let it
// decrypt and search its own store, then merge the (redacted) hits here. No key
// ever crosses the network, and a machine that's asleep simply drops out of the
// results instead of failing the search.
//
// Machines come from the registry `frame`/`puppet` already keep at
// ~/.config/slab/puppet.json — one fleet list, not three. A machine's name
// doubles as its ssh host unless it sets "sshHost"; set "repo" to override
// where the checkout lives (default ~/aesthetic-computer).
//
// Hand-rolled JSON-RPC over stdio, matching the house style of
// slab/bin/frame-mcp.mjs and artery/emacs-mcp.mjs — no SDK, node builtins only.
import { execFile } from "node:child_process";
import { existsSync, readFileSync } from "node:fs";
import { homedir, hostname } from "node:os";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

import { httpPort, serveHttp, serveStdio } from "../toolchain/mcp/http-front.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const HOME = homedir();
const REGISTRY = process.env.SLAB_PUPPET_CONFIG || join(HOME, ".config", "slab", "puppet.json");
const DEFAULT_REPO = "~/aesthetic-computer";

// The fleet, as frame/puppet already know it. `local` is always present — this
// Mac searches its own store in-process, no ssh.
function machines() {
  let registry = {};
  if (existsSync(REGISTRY)) {
    try {
      registry = JSON.parse(readFileSync(REGISTRY, "utf8")).machines || {};
    } catch {
      registry = {};
    }
  }
  const self = hostname().split(".")[0];
  const out = { ...registry };
  delete out._README;
  if (!out.local) out.local = { local: true };
  delete out[self]; // this Mac IS `local`; don't search it twice over ssh
  return out;
}

function isLocal(name, spec) {
  return name === "local" || spec?.local === true;
}

// Run the memory CLI somewhere and get its --json envelope back.
//
// Remote commands go over stdin to `bash -s` rather than as an ssh argument:
// the remote runs a login shell, and blueberry's is fish, which can't parse the
// bash we'd otherwise send. The query is base64'd across the wire so no quoting
// or shell metacharacter in a prompt ("$(", backticks, quotes — all common in
// our prompts!) can reach the remote shell.
function runCli(name, spec, args, { timeoutMs = 20000 } = {}) {
  const argv = ["memory/cli.mjs", ...args, "--json"];

  if (isLocal(name, spec)) {
    return execFileAsync("node", [join(HERE, "cli.mjs"), ...args, "--json"], {
      cwd: join(HERE, ".."),
      timeoutMs,
    });
  }

  const repo = spec.repo || DEFAULT_REPO;
  const host = spec.sshHost || name;
  const encoded = Buffer.from(JSON.stringify(argv), "utf8").toString("base64");
  const script = [
    `cd ${repo} || exit 3`,
    `printf %s '${encoded}' | base64 --decode > /tmp/.memory-mcp-argv.json`,
    `node -e 'const a=require("/tmp/.memory-mcp-argv.json");require("child_process").spawnSync(process.execPath,a,{stdio:"inherit"})'`,
    `rm -f /tmp/.memory-mcp-argv.json`,
  ].join("\n");

  return execFileAsync(
    "ssh",
    [
      "-o", "ControlMaster=auto",
      "-o", `ControlPath=${join(HOME, ".ssh", `cm-${host}`)}`,
      "-o", "ControlPersist=300",
      "-o", "BatchMode=yes",
      "-o", "ConnectTimeout=6",
      host,
      "bash -s",
    ],
    { input: script, timeoutMs },
  );
}

function execFileAsync(file, args, { cwd, input, timeoutMs } = {}) {
  return new Promise((resolve, reject) => {
    const child = execFile(
      file,
      args,
      { cwd, timeout: timeoutMs, encoding: "utf8", maxBuffer: 32 * 1024 * 1024 },
      (err, stdout, stderr) => {
        if (err && !stdout) return reject(new Error((stderr || err.message).trim()));
        resolve(stdout);
      },
    );
    if (input != null) {
      child.stdin.write(input);
      child.stdin.end();
    }
  });
}

// Ask every machine at once; an unreachable one contributes an error line, not
// a failure. A search that silently dropped a sleeping Mac would be worse than
// useless — you'd conclude the prompt was never written.
// A login shell on a fleet Mac is not a clean pipe: neo's node version manager
// greets every non-interactive command with "Using Node for alias lts-jod" on
// STDOUT. So take the JSON envelope, not the whole stream — parse from the first
// brace/bracket. If there's none, the remote said something that isn't a result
// (usage text, a stack trace); surface its first line as the error.
function parseEnvelope(stdout) {
  const at = stdout.search(/[[{]/);
  if (at < 0) throw new Error(stdout.trim().split("\n")[0] || "no output");
  return JSON.parse(stdout.slice(at));
}

async function fanOut(target, args) {
  const fleet = machines();
  const names = target ? [target] : Object.keys(fleet);
  const unknown = names.filter((name) => !fleet[name]);
  if (unknown.length) throw new Error(`unknown machine: ${unknown.join(", ")} (have: ${Object.keys(fleet).join(", ")})`);

  const settled = await Promise.all(
    names.map(async (name) => {
      try {
        return { name, result: parseEnvelope(await runCli(name, fleet[name], args)) };
      } catch (error) {
        return { name, error: String(error.message || error).split("\n")[0] };
      }
    }),
  );
  return settled;
}

function flag(name, value) {
  return value == null || value === false || value === "" ? [] : value === true ? [`--${name}`] : [`--${name}`, String(value)];
}

async function toolSearch(a) {
  if (!a.query) throw new Error("`query` is required");
  const args = [
    "search", "--query", String(a.query),
    ...flag("limit", a.limit ?? 10),
    ...flag("since", a.since),
    ...flag("project", a.project),
    ...flag("role", a.role),
    ...flag("regex", a.regex),
  ];

  const results = await fanOut(a.machine, args);
  const lines = [];
  const failures = [];
  const hits = [];

  for (const entry of results) {
    if (entry.error) {
      failures.push(`  ${entry.name}: ${entry.error}`);
      continue;
    }
    for (const hit of entry.result.hits || []) hits.push({ ...hit, machine: entry.name });
  }

  hits.sort((a, b) => new Date(b.when || 0) - new Date(a.when || 0));

  lines.push(`${hits.length} match${hits.length === 1 ? "" : "es"} for "${a.query}" across ${results.length - failures.length} machine(s)`);
  if (failures.length) lines.push(`\nunreachable (their history is NOT in these results):\n${failures.join("\n")}`);
  lines.push("");

  for (const hit of hits) {
    lines.push(`${hit.when}  [${hit.machine}]  ${hit.role || "?"}  ${hit.session_id}#${hit.seq}`);
    lines.push(`  ${hit.snippet}`);
  }
  if (!hits.length) lines.push("(nothing — try fewer words, or --regex, or widen --since)");

  return [{ type: "text", text: lines.join("\n") }];
}

async function toolSessions(a) {
  const args = ["list", ...flag("limit", a.limit ?? 15), ...flag("project", a.project)];
  const results = await fanOut(a.machine, args);
  const lines = [];

  for (const entry of results) {
    if (entry.error) {
      lines.push(`[${entry.name}] unreachable: ${entry.error}`);
      continue;
    }
    lines.push(`[${entry.name}]`);
    for (const s of entry.result || []) {
      lines.push(`  ${s.updated_at}  ${s.session_id}  seq=${s.last_seq}  project=${s.project}  "${s.title}"`);
    }
  }
  return [{ type: "text", text: lines.join("\n") || "(no sessions)" }];
}

async function toolMachines() {
  const fleet = machines();
  const lines = Object.entries(fleet).map(([name, spec]) =>
    isLocal(name, spec)
      ? `${name}  (this Mac — searched in-process)`
      : `${name}  ssh=${spec.sshHost || name}  repo=${spec.repo || DEFAULT_REPO}`,
  );
  lines.push(`\nregistry: ${REGISTRY}  (shared with frame/puppet — add a machine there and it's searchable here)`);
  return [{ type: "text", text: lines.join("\n") }];
}

const TOOLS = [
  {
    name: "memory_search",
    description:
      "Search OUR PROMPT HISTORY — every prompt @jeffrey has typed to Claude/Codex — across the fleet (this Mac + neo + any other registered machine). Each machine decrypts and searches its own local store, so nothing is missed just because it was said on a different Mac. Use this to answer 'when did we decide X', 'what did I say about Y', 'have we hit this bug before'. Results are snippets with machine, session id, and timestamp; secrets are redacted. Unreachable machines are named explicitly so you know what ISN'T covered.",
    inputSchema: {
      type: "object",
      properties: {
        query: { type: "string", description: "Text to find (case-insensitive substring, or a regex with regex:true)." },
        machine: { type: "string", description: "Limit to one machine (see memory_machines). Omit to search the whole fleet." },
        limit: { type: "number", description: "Max hits per machine (default 10)." },
        since: { type: "string", description: "Only prompts at/after this time — ISO or YYYY-MM-DD." },
        project: { type: "string", description: "Filter to one project." },
        role: { type: "string", description: "Filter by role, e.g. 'user' for things @jeffrey typed." },
        regex: { type: "boolean", description: "Treat query as a regular expression." },
      },
      required: ["query"],
    },
  },
  {
    name: "memory_sessions",
    description: "List recent agent sessions (title, project, last activity) per machine — the index behind memory_search. Use it to find a session id to search within, or to see what's been worked on lately and where.",
    inputSchema: {
      type: "object",
      properties: {
        machine: { type: "string", description: "One machine, or omit for the whole fleet." },
        limit: { type: "number", description: "Sessions per machine (default 15)." },
        project: { type: "string", description: "Filter to one project." },
      },
    },
  },
  {
    name: "memory_machines",
    description: "List the machines whose prompt history is searchable, and where each one's store lives. Machines come from the shared frame/puppet registry at ~/.config/slab/puppet.json.",
    inputSchema: { type: "object", properties: {} },
  },
];

async function callTool(name, args) {
  switch (name) {
    case "memory_search": return toolSearch(args || {});
    case "memory_sessions": return toolSessions(args || {});
    case "memory_machines": return toolMachines();
    default: throw new Error(`Unknown tool: ${name}`);
  }
}

async function handleMessage(message) {
  const { id, method, params } = message;
  try {
    switch (method) {
      case "initialize":
        return {
          jsonrpc: "2.0", id,
          result: {
            protocolVersion: "2024-11-05",
            capabilities: { tools: {} },
            serverInfo: { name: "memory-mcp", version: "1.0.0" },
          },
        };
      case "initialized":
      case "notifications/initialized":
        return null; // notification — no response
      case "ping":
        return { jsonrpc: "2.0", id, result: {} };
      case "tools/list":
        return { jsonrpc: "2.0", id, result: { tools: TOOLS } };
      case "tools/call": {
        const content = await callTool(params?.name, params?.arguments);
        return { jsonrpc: "2.0", id, result: { content } };
      }
      default:
        return { jsonrpc: "2.0", id, error: { code: -32601, message: `Method not found: ${method}` } };
    }
  } catch (error) {
    // Tool failures come back as an error-flagged result (visible to the model)
    // rather than a protocol error, so the agent can read and react to them.
    if (method === "tools/call") {
      return { jsonrpc: "2.0", id, result: { isError: true, content: [{ type: "text", text: String(error.message || error) }] } };
    }
    return { jsonrpc: "2.0", id, error: { code: -32000, message: String(error.message || error) } };
  }
}

const port = httpPort(process.argv, 7769);
if (port) serveHttp({ handleMessage, port, banner: "🧠 memory-mcp shared daemon" });
else serveStdio({ handleMessage, banner: "🧠 memory-mcp server started (memory_search, memory_sessions, memory_machines)" });
