#!/usr/bin/env node
import { connect } from "node:net";
import { homedir } from "node:os";
import { join, resolve } from "node:path";

const SOCKET = process.env.JUKEWIZARD_SOCKET || join(homedir(), ".config", "jukewizard", "control.sock");

export function request(command) {
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
  if (["status", "list", "pause", "toggle", "next", "previous", "prev", "detach"].includes(name)) {
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
  throw new Error("usage: jukewizard {status|list [limit]|play [path|--title title|--index n]|select ...|pause|toggle|seek seconds|speed 0.5..1.5|next|previous|source name|detach}");
}

if (import.meta.url === `file://${process.argv[1]}`) {
  try { console.log(JSON.stringify(await request(parse(process.argv.slice(2))), null, 2)); }
  catch (error) { console.error(JSON.stringify({ ok: false, error: error.message })); process.exitCode = 1; }
}
