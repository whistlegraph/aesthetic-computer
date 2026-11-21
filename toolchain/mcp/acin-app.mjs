#!/usr/bin/env node
// acin-app.mjs — claude in to the native Aesthetic Computer app.
//
// The Mac app (apple/aesthetic.computer, ContentView.swift `ClaudeIn`) opens
// a door on its own loopback: where it is (`info`), a line of JavaScript run
// in its page (`eval`), a picture of it (`shot`). The port and the secret a
// request must carry are in ~/.ac-claude-in.json on the Mac the app runs on.
// For another Mac the request is made there, over ssh, so the secret never
// leaves that machine and nothing is forwarded.
//
//   node toolchain/mcp/acin-app.mjs [--machine blueberry] info
//   node toolchain/mcp/acin-app.mjs [--machine blueberry] eval 'return location.href'
//   node toolchain/mcp/acin-app.mjs [--machine blueberry] shot out.png

import { execFile } from "node:child_process";
import { writeFile } from "node:fs/promises";
import { fileURLToPath } from "node:url";

const PATHS = { info: ["GET", "/info"], eval: ["POST", "/eval"], shot: ["GET", "/shot"] };
// Run where the app is: read the card, make the one request, print the answer.
const KNOCK = `
card="$HOME/.ac-claude-in.json"
[ -f "$card" ] || { echo '{"ok":false,"error":"no door: the app is not running here, or is older than Claude in"}'; exit 0; }
port=$(plutil -extract port raw -o - "$card") && secret=$(plutil -extract secret raw -o - "$card") || exit 1
if [ "$1" = POST ]; then
  exec curl -s --max-time 20 -X POST --data-binary @- -H "Authorization: Bearer $secret" "http://127.0.0.1:$port$2"
fi
exec curl -s --max-time 20 -H "Authorization: Bearer $secret" "http://127.0.0.1:$port$2"
`;
const quote = (text) => `'${String(text).replace(/'/g, `'\\''`)}'`;

function run(machine, method, path, body) {
  const local = !machine || machine === "local";
  if (!local && !/^[a-z0-9][a-z0-9._-]{0,62}$/i.test(machine)) throw new Error("Invalid machine");
  const [command, args] = local
    ? ["bash", ["-c", KNOCK, "knock", method, path]]
    : ["ssh", ["-o", "ConnectTimeout=8", "-o", "BatchMode=yes", machine, `bash -c ${quote(KNOCK)} knock ${method} ${path}`]];
  return new Promise((resolve, reject) => {
    const child = execFile(command, args, { encoding: "buffer", maxBuffer: 64 << 20, timeout: 40000 }, (error, stdout) =>
      error && !stdout.length ? reject(new Error(`could not reach the app${local ? "" : ` on ${machine}`}: ${error.message.split("\n")[0]}`)) : resolve(stdout));
    child.stdin.end(body || "");
  });
}

// → the door's answer: an object, or for a shot { ok, png: Buffer }.
export async function appDoor({ machine, action, js } = {}) {
  const route = PATHS[action];
  if (!route) throw new Error("action is info, eval or shot");
  if (action === "eval" && !String(js || "").trim()) throw new Error("eval needs js: the inside of an async function, with a return");
  const out = await run(machine, ...route, action === "eval" ? String(js) : "");
  if (action === "shot" && out.subarray(0, 4).toString("latin1") === "\x89PNG") return { ok: true, png: out };
  try { return JSON.parse(out.toString("utf8")); } catch { return { ok: false, error: out.toString("utf8").slice(0, 200) || "no answer: the app is not listening" }; }
}

if (process.argv[1] === fileURLToPath(import.meta.url)) {
  const args = process.argv.slice(2), at = args.indexOf("--machine");
  const machine = at >= 0 ? args.splice(at, 2)[1] : null;
  const [action, rest] = args;
  try {
    const said = await appDoor({ machine, action, js: rest });
    if (said.png) { await writeFile(rest || "app.png", said.png); console.log(rest || "app.png"); }
    else console.log(JSON.stringify(said, null, 2));
    process.exit(said.ok ? 0 : 1);
  } catch (e) { console.error(e.message); process.exit(2); }
}
