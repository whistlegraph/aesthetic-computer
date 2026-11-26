// agents.mjs — the other sessions in the fleet, from inside the terminal.
//
// Aesel runs one agent loop and has no subagent of its own. The Slab menubar
// already knows every Claude, Codex and Aesel session on the fleet and runs a
// small daemon that can list, launch, message and close them. This is a thin
// client for it, so `/agents` can show them and steer them. The daemon keeps
// its own refusals (it will not close the session calling it, and it only
// launches what it allows); nothing here widens them.
import { readFileSync } from "node:fs";
import { homedir } from "node:os";
import { join } from "node:path";

export const PROX = process.env.PROX_URL || "http://127.0.0.1:7773";
export const KINDS = ["claude", "codex", "aesel"];

export async function prox(name, args = {}, { fetchImpl = fetch, url = PROX } = {}) {
  let response;
  try {
    response = await fetchImpl(`${url}/`, {
      method: "POST",
      headers: { "content-type": "application/json" },
      body: JSON.stringify({ jsonrpc: "2.0", id: Date.now(), method: "tools/call", params: { name, arguments: args } }),
      signal: AbortSignal.timeout(30_000),
    });
  } catch (error) {
    throw new Error(`prox is not running on ${url} · start the Slab menubar (${error.message})`);
  }
  const body = await response.json();
  const text = (body?.result?.content || []).map((c) => c.text || "").join("\n").trim();
  if (body?.result?.isError || body?.error) throw new Error(text || body.error?.message || `${name} failed`);
  return text;
}

// "  host,name,status,kind,age,subject,alias" — the subject may hold commas, so
// the first five fields and the last one are split off and the rest is it.
export function parseRocks(text = "") {
  const rocks = [];
  for (const line of String(text).split("\n")) {
    if (!/^\s{2}\S/.test(line) || /^\s*rocks\[/.test(line)) continue;
    const parts = line.trim().split(",");
    if (parts.length < 7) continue;
    const [host, name, status, kind, age] = parts;
    const alias = parts[parts.length - 1];
    const subject = parts.slice(5, -1).join(",");
    const [type, agent = "claude"] = kind.split("·");
    rocks.push({ host, name, status, kind: type, agent, age, subject, alias, handle: `${host}:${name}` });
  }
  return rocks;
}

const RANK = { awaiting: 0, working: 1, rendering: 2, interrupted: 3, complete: 4, blank: 5 };
export const order = (rocks) => rocks.map((r, i) => [r, i]).sort(([a, i], [b, j]) => (RANK[a.status] ?? 9) - (RANK[b.status] ?? 9) || i - j).map(([r]) => r);

const clip = (s, n) => (s.length > n ? s.slice(0, n - 1) + "…" : s);

export function table(rocks, width = 100) {
  if (!rocks.length) return "No sessions · /agents new [claude|codex|aesel] [@host] [prompt]";
  const host = Math.min(12, Math.max(...rocks.map((r) => r.host.length)));
  const name = Math.max(...rocks.map((r) => r.name.length));
  const lines = rocks.map((r, i) => {
    const head = `${String(i + 1).padStart(2)}  ${clip(r.host, host).padEnd(host)}  ${r.name.padEnd(name)}  ${r.status.padEnd(11)}${r.agent.padEnd(7)}${r.age.padStart(5)}  `;
    return head + clip(r.subject, Math.max(16, width - head.length));
  });
  return lines.join("\n") + "\n/agents send N text · poke N · close N · new [claude|codex|aesel] [@host] [prompt]";
}

export function localHost(ledger = join(homedir(), ".config", "slab", "ledger", "local.json")) {
  try { return JSON.parse(readFileSync(ledger, "utf8")).host || null; } catch { return null; }
}

export function parseCommand(rest = "") {
  const words = rest.trim().split(/\s+/).filter(Boolean);
  const verb = (words.shift() || "list").toLowerCase();
  if (verb === "new" || verb === "launch" || verb === "spawn") {
    let agent = "aesel", host = null;
    while (words.length) {
      if (KINDS.includes(words[0].toLowerCase()) && agent === "aesel" && !host) agent = words.shift().toLowerCase();
      else if (/^@\S+/.test(words[0]) && !host) host = words.shift().slice(1);
      else break;
    }
    return { verb: "new", agent, host, prompt: words.join(" ") };
  }
  if (["send", "poke", "close", "kill", "show"].includes(verb)) {
    const target = words.shift() || "";
    return { verb: verb === "kill" ? "close" : verb, target, text: words.join(" ") };
  }
  if (verb === "all") return { verb: "list", all: true };
  if (KINDS.includes(verb)) return { verb: "list", agent: verb };
  if (verb === "list" || verb === "ls") return { verb: "list", all: words[0] === "all" };
  return { verb: "help" };
}

// One session per call, resolved from the last list so `send 2 …` means the
// second row the person is looking at, or from a host:name they typed.
export function resolve(target, rocks) {
  if (/^\d+$/.test(target)) return rocks[Number(target) - 1] || null;
  if (!target) return null;
  return rocks.find((r) => r.handle === target || r.alias === target) || rocks.filter((r) => r.name === target).length === 1 && rocks.find((r) => r.name === target) || null;
}

export class Agents {
  constructor({ call = prox, host = localHost } = {}) {
    this.call = call;
    this.host = host;
    this.rocks = [];
  }

  async list({ all = false, agent } = {}) {
    const args = { ...(all ? { all: true } : {}), ...(agent ? { agent } : {}) };
    this.rocks = order(parseRocks(await this.call("prox_list", args)));
    return this.rocks;
  }

  async run(rest, { width = 100 } = {}) {
    const cmd = parseCommand(rest);
    if (cmd.verb === "help") return "/agents [all|claude|codex|aesel] · send N text · poke N · close N · show N · new [claude|codex|aesel] [@host] [prompt]";
    if (cmd.verb === "list") return table(await this.list(cmd), width);
    if (cmd.verb === "new") {
      const host = cmd.host || this.host();
      if (!host) return "No local Slab host found · name one: /agents new aesel @neo";
      await this.call("prox_launch", { host, agent: cmd.agent, by: "aesel:agents", ...(cmd.prompt ? { prompt: cmd.prompt } : {}) });
      return `Launched ${cmd.agent} on ${host}${cmd.prompt ? " with your prompt" : ""} · /agents to find it`;
    }
    if (!this.rocks.length) await this.list();
    const rock = resolve(cmd.target, this.rocks);
    if (!rock) return `No session "${cmd.target}" · /agents lists them`;
    if (cmd.verb === "show") return await this.call("prox_find", { handle: rock.handle });
    if (cmd.verb === "poke") { await this.call("prox_poke", { handle: rock.handle, by: "aesel:agents" }); return `Poked ${rock.handle}`; }
    if (cmd.verb === "close") { await this.call("prox_close", { handle: rock.handle }); this.rocks = this.rocks.filter((r) => r !== rock); return `Closed ${rock.handle}`; }
    if (cmd.verb === "send") {
      if (!cmd.text) return `Say what to send · /agents send ${cmd.target} text`;
      await this.call("prox_send", { handle: rock.handle, text: cmd.text, by: "aesel:agents" });
      return `Sent to ${rock.handle}`;
    }
    return "";
  }
}
