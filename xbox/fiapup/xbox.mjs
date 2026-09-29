#!/usr/bin/env node
// Put fiapup on the Xbox by borrowing the installed oskiewar app, and give it
// back. Prepared, not yet run: see xbox/FIAPUP.md → "Getting it onto the Xbox".
//
// The installed package (AestheticComputer.NativeBios) runs whatever script
// sits in its LocalState as live-piece.js, and keeps running it across
// launches. So borrowing is: save the script that's there, publish
// fiapup.js in its place, launch. Giving back is: publish the saved bytes
// again. While fiapup is borrowed, the console is not running oskiewar, and
// the oskiewar release receipt is told so (its Xbox channel goes `pending`),
// so a later `npm run oskiewar:reconcile` would also put oskiewar back.
//
//   node xbox/fiapup/xbox.mjs plan              say what would happen; touches nothing
//   node xbox/fiapup/xbox.mjs borrow --yes      save oskiewar's live script, publish fiapup, launch
//   node xbox/fiapup/xbox.mjs restore --yes     publish the saved script again, launch
//
// Everything that talks to the console goes through xbox/tools/live.mjs,
// except the one download it has no command for.

import { createHash } from "node:crypto";
import { spawnSync } from "node:child_process";
import { existsSync, mkdirSync, readFileSync, renameSync, writeFileSync } from "node:fs";
import { homedir } from "node:os";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const root = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
const game = resolve(root, "xbox/fiapup/fiapup.js");
const live = resolve(root, "xbox/tools/live.mjs");
const common = resolve(root, spawnSync("git", ["rev-parse", "--git-common-dir"],
  { cwd: root, encoding: "utf8" }).stdout.trim() || ".git");
const receiptPath = resolve(common, "oskiewar-parity.json");
const backupPath = resolve(common, "fiapup-borrowed-live-piece.js");
const sha = (bytes) => createHash("sha256").update(bytes).digest("hex");

function portal() {
  const path = process.env.XBOX_DEVICE_PORTAL_ENV ||
    resolve(homedir(), "aesthetic-computer/aesthetic-computer-vault/xbox/device-portal.env");
  const env = {};
  if (existsSync(path)) for (const line of readFileSync(path, "utf8").split(/\r?\n/)) {
    const m = line.trim().match(/^(?:export\s+)?([A-Za-z_]\w*)=(.*)$/);
    if (m && !line.trim().startsWith("#")) env[m[1]] = m[2].trim().replace(/^(['"])(.*)\1$/, "$2");
  }
  const c = { ...env, ...process.env };
  return { host: c.XBOX_DEVICE_PORTAL_HOST, port: c.XBOX_DEVICE_PORTAL_PORT || "11443",
    user: c.XBOX_DEVICE_PORTAL_USERNAME, pass: c.XBOX_DEVICE_PORTAL_PASSWORD };
}

function liveCommand(...args) {
  const run = spawnSync("node", [live, ...args], { cwd: root, encoding: "utf8" });
  if (run.status !== 0) throw new Error(`live.mjs ${args[0]}: ${(run.stderr || run.stdout).trim()}`);
  return run.stdout;
}

function curl(args) {
  const run = spawnSync("curl", ["-k", "-sS", "--fail", "--connect-timeout", "5", "--max-time", "30", ...args],
    { encoding: "buffer", maxBuffer: 8 * 1024 * 1024 });
  if (run.status !== 0) throw new Error(run.stderr.toString().trim() || `curl exited ${run.status}`);
  return run.stdout;
}

// The script the console runs now: LocalState/live-piece.js of the newest
// NativeBios install. Empty when nothing was ever pushed (it runs the
// packaged oskiewar.js then).
function downloadLivePiece() {
  const { host, port, user, pass } = portal();
  const base = `https://${host}:${port}`;
  const packages = JSON.parse(curl(["-u", `${user}:${pass}`, `${base}/api/app/packagemanager/packages`]).toString())
    .InstalledPackages.filter((p) => p.PackageFamilyName === "AestheticComputer.NativeBios")
    .sort((a, b) => b.Version.Revision - a.Version.Revision);
  if (!packages[0]) throw new Error("oskiewar (AestheticComputer.NativeBios) is not installed");
  const query = new URLSearchParams({ knownfolderid: "LocalAppData", packagefullname: packages[0].PackageFullName,
    path: "\\LocalState", filename: "live-piece.js" });
  try { return curl(["-u", `auto-${user}:${pass}`, `${base}/api/filesystem/apps/file?${query}`]); }
  catch { return Buffer.alloc(0); }
}

function markReceipt(status, detail, hash = undefined) {
  if (!existsSync(receiptPath)) return;
  const receipt = JSON.parse(readFileSync(receiptPath, "utf8"));
  const xbox = receipt.channels?.xbox;
  if (!xbox) return;
  receipt.channels.xbox = { ...xbox, status, ...(hash !== undefined ? { hash } : {}),
    updatedAt: new Date().toISOString(), detail };
  const temporary = `${receiptPath}.${process.pid}`;
  writeFileSync(temporary, JSON.stringify(receipt, null, 2) + "\n");
  renameSync(temporary, receiptPath);
}

function plan() {
  const { host } = portal();
  const bytes = readFileSync(game);
  console.log(JSON.stringify({
    console: host || "(no Device Portal configured on this machine)",
    fiapup: { path: game, bytes: bytes.length, sha256: sha(bytes).slice(0, 12), limit: 2 * 1024 * 1024 },
    borrow: [
      `download LocalState/live-piece.js → ${backupPath}`,
      "mark the oskiewar receipt's xbox channel pending (borrowed by fiapup)",
      "node xbox/tools/live.mjs publish xbox/fiapup/fiapup.js",
      "node xbox/tools/live.mjs launch",
    ],
    restore: [`node xbox/tools/live.mjs publish ${backupPath} (or oskiewar:reconcile)`, "launch"],
    backup: existsSync(backupPath) ? "a borrowed script is saved; restore puts it back" : "none saved",
  }, null, 2));
}

function borrow() {
  const bytes = readFileSync(game);
  if (bytes.length > 2 * 1024 * 1024) throw new Error("fiapup.js is over the host's 2 MiB live limit");
  if (existsSync(backupPath)) throw new Error(`already borrowed: ${backupPath} exists; restore first`);
  const current = downloadLivePiece();
  mkdirSync(dirname(backupPath), { recursive: true });
  writeFileSync(backupPath, current);
  console.log(`saved the console's live script (${current.length} bytes, ${sha(current).slice(0, 12)})`);
  markReceipt("pending", "borrowed by fiapup; node xbox/fiapup/xbox.mjs restore --yes puts oskiewar back");
  console.log(liveCommand("publish", game).trim());
  console.log(liveCommand("launch").trim());
  console.log(liveCommand("logs", "20").trim());
}

function restore() {
  if (!existsSync(backupPath)) throw new Error("nothing borrowed: no saved script");
  const saved = readFileSync(backupPath);
  if (!saved.length) throw new Error("the console ran its packaged oskiewar.js before; run npm run oskiewar:reconcile to push the current one");
  // live.mjs refuses a file named oskiewar.js outside the unified release;
  // the saved copy has its own name, which is what publishes it verbatim.
  console.log(liveCommand("publish", backupPath).trim());
  console.log(liveCommand("launch").trim());
  const receipt = existsSync(receiptPath) ? JSON.parse(readFileSync(receiptPath, "utf8")) : null;
  if (receipt?.desired?.hash === sha(saved)) markReceipt("current", "restored after fiapup", sha(saved));
  renameSync(backupPath, `${backupPath}.restored-${Date.now()}`);
}

const [command = "plan", ...flags] = process.argv.slice(2);
try {
  if (command === "plan") plan();
  else if (!flags.includes("--yes")) throw new Error(`${command} touches the console; add --yes once it's been agreed`);
  else if (command === "borrow") borrow();
  else if (command === "restore") restore();
  else throw new Error("commands: plan | borrow --yes | restore --yes");
} catch (error) { console.error(error.message); process.exitCode = 1; }
