#!/usr/bin/env node
// Device Portal control surface for the AC Native BIOS live JavaScript loop.
// Designed for blueberry, where the Xbox vault credentials already live.

import { spawnSync } from "node:child_process";
import { existsSync, readFileSync } from "node:fs";
import { homedir } from "node:os";
import { resolve } from "node:path";

const defaultEnv = resolve(homedir(),
  "aesthetic-computer/aesthetic-computer-vault/xbox/device-portal.env");

function parseEnv(path) {
  if (!existsSync(path)) return {};
  const result = {};
  for (const raw of readFileSync(path, "utf8").split(/\r?\n/)) {
    const line = raw.trim();
    if (!line || line.startsWith("#")) continue;
    const match = line.match(/^(?:export\s+)?([A-Za-z_][A-Za-z0-9_]*)=(.*)$/);
    if (!match) continue;
    let value = match[2].trim();
    if ((value.startsWith('"') && value.endsWith('"')) ||
        (value.startsWith("'") && value.endsWith("'"))) value = value.slice(1, -1);
    result[match[1]] = value;
  }
  return result;
}

const fileEnv = parseEnv(process.env.XBOX_DEVICE_PORTAL_ENV || defaultEnv);
const config = { ...fileEnv, ...process.env };
const host = config.XBOX_DEVICE_PORTAL_HOST;
const port = config.XBOX_DEVICE_PORTAL_PORT || "11443";
const username = config.XBOX_DEVICE_PORTAL_USERNAME;
const password = config.XBOX_DEVICE_PORTAL_PASSWORD;
if (!host || !username || !password) {
  console.error(`Missing Device Portal configuration. Expected ${defaultEnv}`);
  process.exit(2);
}

const base = `https://${host}:${port}`;
const auth = `${username}:${password}`;
const autoAuth = `auto-${username}:${password}`;

function curl(args, { json = false } = {}) {
  const result = spawnSync("curl", ["-k", "-sS", ...args], {
    encoding: "utf8", maxBuffer: 16 * 1024 * 1024,
  });
  if (result.status !== 0) throw new Error(result.stderr.trim() || `curl exited ${result.status}`);
  return json ? JSON.parse(result.stdout || "null") : result.stdout;
}

function packages() {
  return curl(["-u", auth, `${base}/api/app/packagemanager/packages`], { json: true })
    .InstalledPackages.filter((item) => item.PackageFamilyName === "AestheticComputer.NativeBios")
    .sort((a, b) => b.Version.Revision - a.Version.Revision);
}

function installed() {
  const item = packages()[0];
  if (!item) throw new Error("AC Native BIOS is not installed");
  return item;
}

function appFileUrl(item, filename) {
  const query = new URLSearchParams({ knownfolderid: "LocalAppData",
    packagefullname: item.PackageFullName, path: "\\LocalState",
    ...(filename ? { filename } : {}) });
  return `${base}/api/filesystem/apps/file?${query}`;
}

function status() {
  const item = installed();
  const processes = JSON.stringify(curl(["-u", auth,
    `${base}/api/resourcemanager/processes`], { json: true }));
  console.log(JSON.stringify({ host, package: item.PackageFullName,
    relativeId: item.PackageRelativeId, version: item.Version,
    running: processes.includes(item.PackageFullName) && processes.includes("NativeBios.exe") }, null, 2));
}

function launch() {
  const item = installed();
  const query = new URLSearchParams({
    appid: Buffer.from(item.PackageRelativeId).toString("base64"),
    package: Buffer.from(item.PackageFullName).toString("base64"),
  });
  curl(["-u", autoAuth, "-X", "POST", "-H", "Content-Length: 0",
    `${base}/api/taskmanager/app?${query}`]);
  console.log(`launched ${item.PackageFullName}`);
}

function publish(sourcePath) {
  if (!sourcePath) throw new Error("usage: xbox-live publish <piece.js>");
  const absolute = resolve(sourcePath);
  if (!existsSync(absolute)) throw new Error(`piece not found: ${absolute}`);
  const item = installed();
  curl(["-u", autoAuth, "-X", "POST", "-F",
    `file=@${absolute};filename=live-piece.js`, appFileUrl(item)]);
  console.log(JSON.stringify({ published: absolute, package: item.PackageFullName }));
}

function logs(tail = "80") {
  const count = Math.max(1, Math.min(5000, Number.parseInt(tail, 10) || 80));
  const content = curl(["-u", autoAuth, appFileUrl(installed(), "ac-native-bios.log")]);
  console.log(content.trimEnd().split(/\r?\n/).slice(-count).join("\n"));
}

function main() {
  const [command = "status", argument] = process.argv.slice(2);
  if (command === "status") status();
  else if (command === "launch") launch();
  else if (command === "publish") publish(argument);
  else if (command === "logs") logs(argument);
  else if (command === "deploy") { publish(argument); launch(); logs("20"); }
  else throw new Error("commands: status | launch | publish <piece.js> | logs [lines] | deploy <piece.js>");
}

try { main(); } catch (error) { console.error(error.message); process.exit(1); }
