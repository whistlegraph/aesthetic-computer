#!/usr/bin/env node
// memory/cli.mjs
// Local-first memory CLI for Claude/Codex session continuity.

import {
  buildEnvProfile,
  commitEvent,
  createCheckpoint,
  createSession,
  flushRemote,
  inspectStore,
  listSessionCheckpoints,
  listSessions,
  rememberSession,
  setDeviceId,
} from "./index.mjs";

function parseArgs(argv) {
  const out = { _: [] };
  for (let i = 0; i < argv.length; i += 1) {
    const token = argv[i];
    if (!token.startsWith("--")) {
      out._.push(token);
      continue;
    }

    const key = token.slice(2);
    const next = argv[i + 1];
    if (next && !next.startsWith("--")) {
      out[key] = next;
      i += 1;
    } else {
      out[key] = true;
    }
  }
  return out;
}

async function readStdinIfAny() {
  if (process.stdin.isTTY) return "";
  return new Promise((resolve, reject) => {
    let data = "";
    process.stdin.setEncoding("utf8");
    process.stdin.on("data", (chunk) => {
      data += chunk;
    });
    process.stdin.on("end", () => resolve(data));
    process.stdin.on("error", reject);
  });
}

function toInt(value, fallback) {
  const parsed = Number(value);
  if (Number.isFinite(parsed)) return parsed;
  return fallback;
}

function printUsage() {
  console.log(`agent-memory CLI

Commands:
  create [--session <id>] [--title <title>] [--provider <name>] [--project <name>]
  event [--session <id>] [--provider <name>] [--model <name>] [--role <role>] [--text <text>] [--source <source>] [--project <name>] [--ticket <id>]
  checkpoint --session <id> [--reason <reason>] [--summary <text>] [--max-events <n>]
  checkpoints --session <id>
  list [--limit <n>] [--project <name>] [--json]
  remember --from <session-id> [--checkpoint <id>] [--session <new-id>] [--title <title>] [--project <name>] [--provider <name>]
  set-device --id <device-id>
  doctor [--json]
  profile [--include-key] [--exports] [--project <name>] [--provider <name>] [--ticket <id>] [--session <id>] [--device <id>]
  flush-remote
`);
}

async function commandCreate(args) {
  const session = await createSession({
    sessionId: args.session || process.env.AGENT_SESSION_ID,
    title: args.title,
    provider: args.provider || process.env.AGENT_MEMORY_PROVIDER,
    project: args.project || process.env.AGENT_MEMORY_PROJECT,
  });
  console.log(JSON.stringify(session, null, 2));
}

async function commandEvent(args) {
  const stdinRaw = await readStdinIfAny();
  let stdinPayload = {};

  if (stdinRaw.trim()) {
    try {
      stdinPayload = JSON.parse(stdinRaw);
    } catch {
      stdinPayload = { text: stdinRaw.trim() };
    }
  }

  const payload = {
    ...stdinPayload,
    sessionId:
      args.session ||
      stdinPayload.sessionId ||
      stdinPayload.session_id ||
      process.env.AGENT_SESSION_ID,
    provider:
      args.provider ||
      stdinPayload.provider ||
      process.env.AGENT_MEMORY_PROVIDER,
    model: args.model || stdinPayload.model,
    role: args.role || stdinPayload.role || "user",
    source: args.source || stdinPayload.source || "manual",
    project:
      args.project ||
      stdinPayload.project ||
      process.env.AGENT_MEMORY_PROJECT,
    text: args.text || stdinPayload.text || stdinPayload.content || stdinPayload.message,
    content: stdinPayload.content,
    context: stdinPayload.context,
    tool_calls: stdinPayload.tool_calls,
    metadata: {
      ...(stdinPayload.metadata || {}),
      ...(args.ticket ? { ticket: args.ticket } : {}),
    },
    title: args.title || stdinPayload.title,
  };

  const result = await commitEvent(payload);
  console.log(JSON.stringify(result, null, 2));
}

async function commandCheckpoint(args) {
  const sessionId = args.session || process.env.AGENT_SESSION_ID;
  if (!sessionId) {
    throw new Error("--session is required (or set AGENT_SESSION_ID)");
  }

  const stdinRaw = await readStdinIfAny();
  const summary = args.summary || stdinRaw.trim() || undefined;

  const checkpoint = await createCheckpoint({
    sessionId,
    reason: args.reason || "manual",
    summary,
    maxEvents: toInt(args["max-events"], 40),
  });

  console.log(JSON.stringify(checkpoint, null, 2));
}

async function commandCheckpoints(args) {
  const sessionId = args.session || process.env.AGENT_SESSION_ID;
  if (!sessionId) {
    throw new Error("--session is required (or set AGENT_SESSION_ID)");
  }

  const checkpoints = await listSessionCheckpoints(sessionId);
  console.log(JSON.stringify(checkpoints, null, 2));
}

async function commandList(args) {
  const sessions = await listSessions({
    limit: toInt(args.limit, 30),
    project: args.project || process.env.AGENT_MEMORY_PROJECT,
  });

  if (args.json) {
    console.log(JSON.stringify(sessions, null, 2));
    return;
  }

  if (sessions.length === 0) {
    console.log("No sessions found.");
    return;
  }

  for (const session of sessions) {
    const remembered = session.remembered_from?.session_id
      ? ` remembered_from=${session.remembered_from.session_id}`
      : "";
    console.log(
      `${session.session_id}  project=${session.project}  updated=${session.updated_at}  seq=${session.last_seq}  title="${session.title}"${remembered}`
    );
  }
}

async function commandRemember(args) {
  if (!args.from) {
    throw new Error("--from is required");
  }

  const result = await rememberSession({
    fromSessionId: args.from,
    checkpointId: args.checkpoint,
    sessionId: args.session || process.env.AGENT_SESSION_ID,
    title: args.title,
    project: args.project || process.env.AGENT_MEMORY_PROJECT,
    provider: args.provider || process.env.AGENT_MEMORY_PROVIDER,
  });

  console.log(JSON.stringify(result, null, 2));
}

async function commandFlushRemote() {
  const result = await flushRemote();
  console.log(JSON.stringify(result, null, 2));
}

async function commandSetDevice(args) {
  if (!args.id) {
    throw new Error("--id is required");
  }

  const normalized = await setDeviceId(args.id);
  console.log(`AGENT_DEVICE_ID set to ${normalized}`);
}

async function commandDoctor(args) {
  const info = await inspectStore();
  if (args.json) {
    console.log(JSON.stringify(info, null, 2));
    return;
  }

  console.log(`home:            ${info.home}`);
  console.log(`device:          ${info.device_id}`);
  console.log(`key source:      ${info.key_source}`);
  console.log(`key fingerprint: ${info.key_fingerprint}`);
  console.log(`sessions:        ${info.sessions_count}`);
  console.log(`event streams:   ${info.event_streams_count}`);
  console.log(`checkpoints:     ${info.checkpoint_streams_count}`);
  console.log(`remote enabled:  ${info.remote_enabled}`);
  if (info.remote_enabled) {
    console.log(`remote endpoint: ${info.remote_endpoint}`);
  }
}

function formatProfileLine(line, useExports) {
  return useExports ? `export ${line}` : line;
}

async function commandProfile(args) {
  const includeKey = Boolean(args["include-key"]);
  const useExports = Boolean(args.exports);

  const profile = await buildEnvProfile({
    includeKey,
    deviceId: args.device,
    project: args.project,
    provider: args.provider,
    ticket: args.ticket,
    sessionId: args.session,
  });

  console.log(`# Agent memory profile (${profile.home})`);
  for (const line of profile.lines) {
    console.log(formatProfileLine(line, useExports));
  }
}

async function main() {
  const args = parseArgs(process.argv.slice(2));
  const command = args._[0] || "help";

  if (command === "help" || command === "--help" || command === "-h") {
    printUsage();
    return;
  }

  const handlers = {
    create: commandCreate,
    event: commandEvent,
    checkpoint: commandCheckpoint,
    checkpoints: commandCheckpoints,
    list: commandList,
    remember: commandRemember,
    "set-device": commandSetDevice,
    doctor: commandDoctor,
    profile: commandProfile,
    "flush-remote": commandFlushRemote,
  };

  const handler = handlers[command];
  if (!handler) {
    printUsage();
    process.exitCode = 1;
    return;
  }

  await handler(args);
}

main().catch((error) => {
  console.error(`agent-memory: ${error.message}`);
  process.exit(1);
});
