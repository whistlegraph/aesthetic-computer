#!/usr/bin/env node
import { execFile } from "node:child_process";
import { connect } from "node:net";
import { existsSync } from "node:fs";
import { homedir } from "node:os";
import { join, resolve } from "node:path";

const SOCKET = process.env.JUKEWIZARD_SOCKET || join(homedir(), ".config", "jukewizard", "control.sock");
const WAKE_NOTIFICATION = "computer.aestheticcomputer.menuband.showJuke";

// The Juke lives inside Menu Band and starts lazily, so right after login or
// a Menu Band relaunch the control socket is missing — or worse, a stale
// socket file from the previous process refuses connections. Ask Menu Band to
// open its Juke, give it a moment to bind, and retry.
function wake() {
  return new Promise((resolvePromise) => {
    // nil must be $() here — a JS null makes the post silently vanish.
    const script = `ObjC.import("Foundation");
$.NSDistributedNotificationCenter.defaultCenter.postNotificationNameObjectUserInfoDeliverImmediately("${WAKE_NOTIFICATION}", $(), $(), true);`;
    execFile("/usr/bin/osascript", ["-l", "JavaScript", "-e", script], () => {
      setTimeout(resolvePromise, 1200);
    });
  });
}

export async function request(command) {
  const deadline = Date.now() + 10_000;
  let lastError;
  if (existsSync(SOCKET)) {
    try { return await send(command); } catch (error) { lastError = error; }
  }
  while (Date.now() < deadline) {
    await wake();
    try { return await send(command); } catch (error) { lastError = error; }
  }
  throw lastError;
}

function send(command) {
  return new Promise((resolvePromise, reject) => {
    const client = connect(SOCKET);
    let response = "";
    client.setEncoding("utf8");
    client.setTimeout(3000);
    client.on("connect", () => client.end(`${JSON.stringify(command)}\n`));
    client.on("data", (chunk) => { response += chunk; if (response.length > 4_000_000) client.destroy(new Error("response too large")); });
    client.on("timeout", () => client.destroy(new Error("Menu Band Juke control timed out")));
    client.on("error", (error) => reject(new Error(`Menu Band Juke is not reachable at ${SOCKET}: ${error.message}`)));
    client.on("end", () => {
      try {
        const value = JSON.parse(response.trim());
        if (!value.ok) reject(new Error(value.error || "Menu Band Juke command failed"));
        else resolvePromise(value);
      } catch (error) { reject(error); }
    });
  });
}

export function parse(argv) {
  const [name = "status", ...args] = argv;
  if (["status", "list", "pause", "stop", "toggle", "next", "previous", "prev", "detach"].includes(name)) {
    return { command: name === "prev" ? "previous" : name, ...(name === "list" && args[0] ? { limit: Number(args[0]) } : {}) };
  }
  if (name === "seek") return { command: "seek", seconds: Number(args[0]) };
  if (name === "speed") return { command: "speed", speed: Number(args[0]) };
  if (name === "source") {
    if (!args[0]) throw new Error("source requires local, aesthetic, spotify, or appleMusic");
    return { command: "source", source: args[0] };
  }
  if (name === "play" || name === "select") {
    if (!args.length) return { command: name };
    if (args[0] === "--title") return { command: name, title: args.slice(1).join(" ") };
    if (args[0] === "--index") return { command: name, index: Number(args[1]) };
    return { command: name, path: resolve(args.join(" ")) };
  }
  throw new Error("usage: jukewizard {status|list [limit]|play [path|--title title|--index n]|select ...|pause|stop|toggle|seek seconds|speed 0.5..1.5|next|previous|source name|detach}");
}

if (import.meta.url === `file://${process.argv[1]}`) {
  try { console.log(JSON.stringify(await request(parse(process.argv.slice(2))), null, 2)); }
  catch (error) { console.error(JSON.stringify({ ok: false, error: error.message })); process.exitCode = 1; }
}
