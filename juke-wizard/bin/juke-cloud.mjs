#!/usr/bin/env node
import { createReadStream, createWriteStream, promises as fs } from "node:fs";
import { request as httpRequest } from "node:http";
import { request as httpsRequest } from "node:https";
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
  jukewizard cloud publish <cloud-key>
  jukewizard cloud remove <cloud-key>
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

function uploadFile(url, path, headers, bytes) {
  return new Promise((resolve, reject) => {
    const target = new URL(url);
    const requester = target.protocol === "https:" ? httpsRequest : httpRequest;
    const request = requester(target, {
      method: "PUT",
      headers: { ...headers, "Content-Length": String(bytes) },
    }, (response) => {
      const chunks = [];
      response.on("data", (chunk) => chunks.push(chunk));
      response.on("end", () => {
        const status = response.statusCode || 0;
        if (status >= 200 && status < 300) return resolve();
        const body = Buffer.concat(chunks).toString("utf8");
        const detail = body.match(/<Message>([^<]+)<\/Message>/)?.[1];
        reject(new Error(`Upload failed (${status})${detail ? `: ${detail}` : ""}`));
      });
    });
    request.on("error", reject);
    const input = createReadStream(path);
    input.on("error", reject);
    input.pipe(request);
  });
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
  await uploadFile(prepared.uploadURL, absolute, prepared.headers, stat.size);
  await api("POST", { action: "publish", key: prepared.track.key });
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
    const existing = new Map((await list()).map((track) => [track.name, track]));
    for (const path of args) {
      const name = basename(resolve(path));
      if (existing.has(name)) {
        console.log(`exists ${name}`);
        continue;
      }
      const track = await push(path);
      existing.set(track.name, track);
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
  if (command === "remove") {
    if (!args[0]) throw new Error("Provide the cloud key shown by `jukewizard cloud list`.");
    await api("POST", { action: "delete", key: args[0] });
    console.log(`removed ${args[0]}`);
    return;
  }
  if (command === "publish") {
    if (!args[0]) throw new Error("Provide the cloud key shown by `jukewizard cloud list`.");
    const published = await api("POST", { action: "publish", key: args[0] });
    console.log(published.command);
    return;
  }
  usage();
  if (command && command !== "help" && command !== "--help") process.exitCode = 2;
}

main().catch((error) => {
  console.error(`jukewizard: ${error.message}`);
  process.exitCode = 1;
});
