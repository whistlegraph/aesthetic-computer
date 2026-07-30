#!/usr/bin/env node
import { createReadStream, createWriteStream, promises as fs } from "node:fs";
import { homedir } from "node:os";
import { basename, resolve } from "node:path";
import { Readable } from "node:stream";
import { pipeline } from "node:stream/promises";

const origin = (process.env.AC_API_ORIGIN || "https://aesthetic.computer").replace(/\/$/, "");
const endpoint = `${origin}/api/juke-cloud`;

function usage() {
  console.log(`usage:
  jukewizard login
  jukewizard cloud list [--json]
  jukewizard cloud push <audio-file> [...]
  jukewizard cloud pull <cloud-key> [destination]
  jukewizard cloud url <cloud-key>`);
}

async function session() {
  let value;
  try { value = JSON.parse(await fs.readFile(`${homedir()}/.ac-token`, "utf8")); }
  catch { throw new Error("Not signed in. Run: jukewizard login"); }
  if (!value.access_token) throw new Error("Not signed in. Run: jukewizard login");
  if (value.expires_at && value.expires_at <= Date.now()) {
    throw new Error("Your Aesthetic Computer login expired. Run: jukewizard login");
  }
  return value.access_token;
}

async function api(method = "GET", input) {
  const token = await session();
  const response = await fetch(endpoint, {
    method,
    headers: {
      Authorization: `Bearer ${token}`,
      ...(input ? { "Content-Type": "application/json" } : {}),
    },
    body: input ? JSON.stringify(input) : undefined,
  });
  let output = {};
  try { output = await response.json(); } catch {}
  if (!response.ok) throw new Error(output.error || `Cloud request failed (${response.status})`);
  return output;
}

async function list() {
  return (await api()).tracks || [];
}

async function push(path) {
  const absolute = resolve(path);
  const stat = await fs.stat(absolute);
  if (!stat.isFile()) throw new Error(`${path} is not a file`);
  const prepared = await api("POST", {
    action: "upload",
    filename: basename(absolute),
    bytes: stat.size,
  });
  const response = await fetch(prepared.uploadURL, {
    method: "PUT",
    headers: { ...prepared.headers, "Content-Length": String(stat.size) },
    body: Readable.toWeb(createReadStream(absolute)),
    duplex: "half",
  });
  if (!response.ok) throw new Error(`Upload failed (${response.status})`);
  return prepared.track;
}

function suggestedName(key) {
  return key.split("/").pop().replace(/^[0-9a-f-]{36}-/, "");
}

async function pull(key, destination) {
  const { url } = await api("POST", { action: "download", key });
  const response = await fetch(url);
  if (!response.ok || !response.body) throw new Error(`Download failed (${response.status})`);
  const path = resolve(destination || suggestedName(key));
  await pipeline(Readable.fromWeb(response.body), createWriteStream(path, { flags: "wx" }));
  return path;
}

async function main() {
  const [command, ...args] = process.argv.slice(2);
  if (command === "list") {
    const tracks = await list();
    if (args.includes("--json")) return console.log(JSON.stringify({ tracks }, null, 2));
    if (!tracks.length) return console.log("No cloud tracks yet.");
    for (const track of tracks) {
      console.log(`${track.name}\t${track.bytes} bytes\n  ${track.key}\n  ${track.command}`);
    }
    return;
  }
  if (command === "push") {
    if (!args.length) throw new Error("Choose at least one audio file.");
    for (const path of args) {
      const track = await push(path);
      console.log(`uploaded ${track.name}\n  ${track.key}\n  ${track.command}`);
    }
    return;
  }
  if (command === "pull") {
    if (!args[0]) throw new Error("Provide the cloud key shown by `jukewizard cloud list`.");
    console.log(await pull(args[0], args[1]));
    return;
  }
  if (command === "url") {
    if (!args[0]) throw new Error("Provide the cloud key shown by `jukewizard cloud list`.");
    const track = (await list()).find((item) => item.key === args[0]);
    if (!track) throw new Error("Cloud track not found.");
    console.log(track.url);
    return;
  }
  usage();
  if (command && command !== "help" && command !== "--help") process.exitCode = 2;
}

main().catch((error) => {
  console.error(`jukewizard: ${error.message}`);
  process.exitCode = 1;
});
