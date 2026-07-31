#!/usr/bin/env node
// dmgify.mjs — turn a local HTML directory into a signed, notarized Electron DMG.
//
// This is the reusable form of Menu Band's release discipline:
// Developer ID + hardened runtime → notarize/staple app → DMG + Applications
// alias → sign/notarize/staple DMG → Gatekeeper verification. Authentication
// is read from the same Apple app-specific-password env file and never copied
// into the app, build directory, receipt, or MCP result.

import { execFile } from "node:child_process";
import {
  access, cp, lstat, mkdir, mkdtemp, readFile, readdir, realpath, rm,
  stat, writeFile,
} from "node:fs/promises";
import { constants as fsConstants } from "node:fs";
import { homedir, tmpdir } from "node:os";
import { basename, dirname, extname, join, relative, resolve, sep } from "node:path";
import { fileURLToPath } from "node:url";
import { promisify } from "node:util";

const pexec = promisify(execFile);
const SCRIPT_DIR = dirname(fileURLToPath(import.meta.url));
export const REPO = resolve(SCRIPT_DIR, "../..");
const BUILDER = join(REPO, "node_modules/.bin/electron-builder");
const DEFAULT_CREDENTIALS = join(REPO, "aesthetic-computer-vault/apple/app-specific-password.env");
const MAX_OUTPUT = 16 * 1024 * 1024;

const safeName = (value) => String(value || "App").replace(/[^A-Za-z0-9._ -]+/g, "").trim() || "App";
const slug = (value) => safeName(value).replace(/\s+/g, "-");
const exists = async (path) => access(path, fsConstants.F_OK).then(() => true, () => false);

function assertPathUnderHome(path, label) {
  const absolute = resolve(path);
  const home = resolve(homedir());
  if (absolute !== home && !absolute.startsWith(`${home}${sep}`)) {
    throw new Error(`${label} must stay beneath ${home}: ${absolute}`);
  }
  return absolute;
}

async function walkStats(dir, skips = new Set([".git", "node_modules", "dist", "release", ".dmgify-work"])) {
  let files = 0;
  let bytes = 0;
  const stack = [dir];
  while (stack.length) {
    const current = stack.pop();
    for (const entry of await readdir(current, { withFileTypes: true })) {
      if (entry.name.startsWith(".") && entry.name !== ".well-known") continue;
      if (entry.isDirectory() && skips.has(entry.name)) continue;
      const path = join(current, entry.name);
      if (entry.isSymbolicLink()) continue;
      if (entry.isDirectory()) stack.push(path);
      else if (entry.isFile()) {
        const info = await stat(path);
        files += 1;
        bytes += info.size;
      }
    }
  }
  return { files, bytes };
}

function humanSize(bytes) {
  if (bytes >= 1e9) return `${(bytes / 1e9).toFixed(2)} GB`;
  if (bytes >= 1e6) return `${(bytes / 1e6).toFixed(2)} MB`;
  if (bytes >= 1e3) return `${(bytes / 1e3).toFixed(2)} KB`;
  return `${bytes} B`;
}

function normalizeOptions(options = {}) {
  if (!options.source) throw new Error("source is required");
  const source = assertPathUnderHome(options.source, "source");
  const entry = String(options.entry || "index.html").replace(/^\/+/, "");
  if (entry.includes("..")) throw new Error("entry may not traverse outside source");
  const name = safeName(options.name || basename(source));
  const bundleId = String(options.bundleId || `computer.aesthetic.${slug(name).toLowerCase()}`);
  if (!/^[A-Za-z0-9.-]+$/.test(bundleId) || !bundleId.includes(".")) throw new Error(`invalid bundleId: ${bundleId}`);
  const version = String(options.version || "1.0.0");
  if (!/^\d+\.\d+\.\d+(?:[-+][A-Za-z0-9.-]+)?$/.test(version)) throw new Error(`version must be semver: ${version}`);
  const output = assertPathUnderHome(options.output || join(source, "release"), "output");
  const icon = options.icon ? assertPathUnderHome(options.icon, "icon") : null;
  const credentials = options.credentials
    ? assertPathUnderHome(options.credentials, "credentials")
    : DEFAULT_CREDENTIALS;
  const include = Array.isArray(options.include) && options.include.length
    ? options.include.map(String)
    : ["**/*", "!release{,/**}", "!dist{,/**}", "!node_modules{,/**}", "!.git{,/**}", "!.dmgify-work{,/**}"];
  return {
    source, entry, name, bundleId, version, output, icon, credentials, include,
    notarize: options.notarize !== false,
    category: String(options.category || "public.app-category.photography"),
  };
}

export async function planDmg(options = {}) {
  const opts = normalizeOptions(options);
  if (!(await exists(opts.source))) throw new Error(`source directory not found: ${opts.source}`);
  if (!(await lstat(opts.source)).isDirectory()) throw new Error(`source is not a directory: ${opts.source}`);
  const entryPath = join(opts.source, opts.entry);
  if (!(await exists(entryPath))) throw new Error(`entry not found: ${entryPath}`);
  if (opts.icon && !(await exists(opts.icon))) throw new Error(`icon not found: ${opts.icon}`);
  if (!(await exists(BUILDER))) throw new Error(`electron-builder not installed: ${BUILDER}`);
  const payload = await walkStats(opts.source);
  const identity = await developerIdentity().catch(() => null);
  return {
    ...opts,
    entryPath,
    payload: { ...payload, human: humanSize(payload.bytes) },
    developerId: identity ? { hash: identity.hash, name: identity.name, teamId: identity.teamId } : null,
    credentialsAvailable: await exists(opts.credentials),
  };
}

async function developerIdentity() {
  const { stdout } = await pexec("security", ["find-identity", "-v", "-p", "codesigning"], { maxBuffer: MAX_OUTPUT });
  const match = stdout.match(/^\s*\d+\)\s+([A-F0-9]{40})\s+"(Developer ID Application: [^"]+ \(([A-Z0-9]{10})\))"/m);
  if (!match) throw new Error("no Developer ID Application identity found in the keychain");
  return { hash: match[1], name: match[2], teamId: match[3] };
}

async function credentials(path, teamId) {
  if (!(await exists(path))) throw new Error(`Apple notarization credentials not found: ${path}`);
  const parsed = {};
  for (const line of (await readFile(path, "utf8")).split(/\r?\n/)) {
    const match = line.match(/^\s*(?:export\s+)?([A-Z0-9_]+)=(.*)\s*$/);
    if (!match) continue;
    parsed[match[1]] = match[2].trim().replace(/^(['"])(.*)\1$/, "$2");
  }
  const appleId = process.env.APPLE_ID || parsed.APPLE_ID;
  const password = process.env.APPLE_APP_PASSWORD || process.env.APP_SPECIFIC_PASSWORD
    || parsed.APPLE_APP_PASSWORD || parsed.APP_SPECIFIC_PASSWORD;
  if (!appleId || !password) throw new Error(`credentials file must provide APPLE_ID and APP_SPECIFIC_PASSWORD: ${path}`);
  return { appleId, password, teamId: process.env.APPLE_TEAM_ID || teamId };
}

async function makeIcns(source, work) {
  if (!source) return null;
  if (extname(source).toLowerCase() === ".icns") {
    const target = join(work, "AppIcon.icns");
    await cp(source, target);
    return target;
  }
  const set = join(work, "AppIcon.iconset");
  await mkdir(set, { recursive: true });
  for (const size of [16, 32, 128, 256, 512]) {
    await pexec("sips", ["-z", String(size), String(size), source, "--out", join(set, `icon_${size}x${size}.png`)]);
    await pexec("sips", ["-z", String(size * 2), String(size * 2), source, "--out", join(set, `icon_${size}x${size}@2x.png`)]);
  }
  const target = join(work, "AppIcon.icns");
  await pexec("iconutil", ["-c", "icns", set, "-o", target]);
  return target;
}

function mainSource() {
  return `const { app, BrowserWindow, Menu, shell } = require("electron");
const { join } = require("node:path");
const entry = join(process.resourcesPath, "archive", ${JSON.stringify("__ENTRY__")});
function createWindow() {
  const win = new BrowserWindow({
    width: 1320, height: 900, minWidth: 760, minHeight: 560,
    backgroundColor: "#f4f1ec", show: false,
    webPreferences: { contextIsolation: true, sandbox: true, nodeIntegration: false, webSecurity: true },
  });
  win.webContents.setWindowOpenHandler(({ url }) => { if (/^https?:/i.test(url)) shell.openExternal(url); return { action: "deny" }; });
  win.webContents.on("will-navigate", (event, url) => { if (!url.startsWith("file:")) { event.preventDefault(); shell.openExternal(url); } });
  win.once("ready-to-show", () => win.show());
  win.loadFile(entry);
}
app.whenReady().then(() => {
  Menu.setApplicationMenu(Menu.buildFromTemplate([
    { role: "appMenu" }, { role: "fileMenu" }, { role: "editMenu" }, { role: "viewMenu" }, { role: "windowMenu" }, { role: "help" },
  ]));
  createWindow();
  app.on("activate", () => { if (BrowserWindow.getAllWindows().length === 0) createWindow(); });
});
app.on("window-all-closed", () => { if (process.platform !== "darwin") app.quit(); });
`;
}

const entitlements = `<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0"><dict>
  <key>com.apple.security.cs.allow-jit</key><true/>
  <key>com.apple.security.cs.allow-unsigned-executable-memory</key><true/>
</dict></plist>
`;

async function run(command, args, options = {}) {
  const result = await pexec(command, args, {
    cwd: options.cwd,
    env: options.env || process.env,
    timeout: options.timeout || 20 * 60_000,
    maxBuffer: MAX_OUTPUT,
  });
  return `${result.stdout || ""}${result.stderr || ""}`;
}

async function notarize(path, creds) {
  return run("xcrun", ["notarytool", "submit", path, "--apple-id", creds.appleId,
    "--team-id", creds.teamId, "--password", creds.password, "--wait"], { timeout: 30 * 60_000 });
}

async function createDmg(appPath, outPath, volumeName, identityHash) {
  const stage = await mkdtemp(join(tmpdir(), "dmgify-stage-"));
  try {
    await cp(appPath, join(stage, basename(appPath)), { recursive: true, preserveTimestamps: true });
    await pexec("ln", ["-s", "/Applications", join(stage, "Applications")]);
    if (await exists(outPath)) await rm(outPath);
    await run("hdiutil", ["create", "-fs", "HFS+", "-volname", volumeName,
      "-srcfolder", stage, "-ov", "-format", "UDZO", outPath]);
    await run("codesign", ["--force", "--sign", identityHash, "--timestamp", outPath]);
  } finally {
    await rm(stage, { recursive: true, force: true });
  }
}

export async function verifyArtifact(path) {
  const target = assertPathUnderHome(path, "artifact");
  if (!(await exists(target))) throw new Error(`artifact not found: ${target}`);
  const isDmg = extname(target).toLowerCase() === ".dmg";
  const checks = [];
  const check = async (name, command, args) => {
    try { checks.push({ name, ok: true, output: (await run(command, args)).trim().slice(-4000) }); }
    catch (error) { checks.push({ name, ok: false, output: String(error.stderr || error.stdout || error.message || error).slice(-4000) }); }
  };
  if (isDmg) {
    await check("dmg signature", "codesign", ["--verify", "--verbose=2", target]);
    await check("dmg staple", "xcrun", ["stapler", "validate", target]);
    await check("dmg Gatekeeper", "spctl", ["-a", "-vv", "-t", "open", "--context", "context:primary-signature", target]);
  } else {
    await check("app signature", "codesign", ["--verify", "--deep", "--strict", "--verbose=2", target]);
    await check("app staple", "xcrun", ["stapler", "validate", target]);
    await check("app Gatekeeper", "spctl", ["-a", "-vv", "-t", "exec", target]);
  }
  const info = await stat(target);
  return { artifact: target, kind: isDmg ? "dmg" : "app", bytes: info.size, human: humanSize(info.size), ok: checks.every((row) => row.ok), checks };
}

export async function buildDmg(options = {}) {
  const plan = await planDmg(options);
  if (!plan.developerId) throw new Error("dmgify requires a Developer ID Application certificate for distributable builds");
  // macOS exposes /var as a symlink to /private/var. electron-builder's ASAR
  // integrity guard compares the project path to realpath(file); passing the
  // non-canonical spelling makes an ordinary file look like an escaping
  // symlink. Canonicalize the temporary root before writing any project file.
  const work = await realpath(await mkdtemp(join(tmpdir(), "dmgify-build-")));
  const receipt = { schema: 1, startedAt: new Date().toISOString(), plan: {
    source: plan.source, entry: plan.entry, name: plan.name, bundleId: plan.bundleId,
    version: plan.version, payload: plan.payload, include: plan.include,
    developerId: plan.developerId.name,
  }, steps: [] };
  const step = async (name, fn) => {
    const started = Date.now();
    process.stderr.write(`dmgify: ${name}…\n`);
    try {
      const output = await fn();
      const elapsedMs = Date.now() - started;
      receipt.steps.push({ name, ok: true, elapsedMs,
        output: typeof output === "string" ? output.trim().slice(-8000) : undefined });
      process.stderr.write(`dmgify: ${name} ✓ (${(elapsedMs / 1000).toFixed(1)}s)\n`);
      return output;
    } catch (error) {
      receipt.steps.push({ name, ok: false, elapsedMs: Date.now() - started,
        output: String(error.stderr || error.stdout || error.message || error).slice(-8000) });
      throw error;
    }
  };
  try {
    await mkdir(plan.output, { recursive: true });
    const icon = await step("icon", () => makeIcns(plan.icon, work));
    await writeFile(join(work, "main.cjs"), mainSource().replace("__ENTRY__", plan.entry));
    await writeFile(join(work, "package.json"), JSON.stringify({
      name: slug(plan.name).toLowerCase(), version: plan.version, private: true, main: "main.cjs",
      devDependencies: { electron: "38.1.2", "electron-builder": "26.0.12" },
    }, null, 2) + "\n");
    await writeFile(join(work, "entitlements.mac.plist"), entitlements);
    const config = {
      appId: plan.bundleId,
      productName: plan.name,
      asar: true,
      directories: { output: plan.output, buildResources: work },
      files: ["main.cjs", "package.json"],
      extraResources: [{ from: plan.source, to: "archive", filter: plan.include }],
      mac: {
        category: plan.category, hardenedRuntime: true, gatekeeperAssess: false,
        entitlements: join(work, "entitlements.mac.plist"),
        entitlementsInherit: join(work, "entitlements.mac.plist"),
        icon: icon || undefined,
        // electron-osx-sign otherwise attempts a separate codesign invocation
        // for every JPG/JSON in a large extraResources archive. The outer app
        // signature still seals these bytes; they simply are not executable
        // code that needs its own nested signature.
        signIgnore: "^.*\\/Contents\\/Resources\\/archive(?:\\/.*)?$",
        target: ["dir"],
      },
    };
    const configPath = join(work, "electron-builder.json");
    await writeFile(configPath, JSON.stringify(config, null, 2) + "\n");
    await step("electron-builder universal app", () => run(BUILDER,
      ["--projectDir", work, "--config", configPath, "--mac", "dir", "--universal"],
      { cwd: REPO, timeout: 45 * 60_000 }));

    const appPath = join(plan.output, "mac-universal", `${plan.name}.app`);
    if (!(await exists(appPath))) throw new Error(`electron-builder did not produce ${appPath}`);
    await step("verify signed app", () => run("codesign", ["--verify", "--deep", "--strict", "--verbose=2", appPath]));

    let creds = null;
    if (plan.notarize) {
      creds = await credentials(plan.credentials, plan.developerId.teamId);
      const zip = join(work, `${slug(plan.name)}.zip`);
      await step("zip app for notarization", () => run("ditto", ["-c", "-k", "--keepParent", appPath, zip]));
      await step("notarize app", () => notarize(zip, creds));
      await step("staple app", () => run("xcrun", ["stapler", "staple", appPath]));
      await step("validate app staple", () => run("xcrun", ["stapler", "validate", appPath]));
    }

    const dmgPath = join(plan.output, `${slug(plan.name)}-${plan.version}.dmg`);
    await step("create and sign DMG", () => createDmg(appPath, dmgPath, `${plan.name} ${plan.version}`, plan.developerId.hash));
    if (plan.notarize) {
      await step("notarize DMG", () => notarize(dmgPath, creds));
      await step("staple DMG", () => run("xcrun", ["stapler", "staple", dmgPath]));
    }
    const verification = await verifyArtifact(dmgPath);
    if (!verification.ok) throw new Error(`final DMG verification failed: ${JSON.stringify(verification.checks)}`);
    receipt.finishedAt = new Date().toISOString();
    receipt.artifact = dmgPath;
    receipt.verification = verification;
    const receiptPath = `${dmgPath}.receipt.json`;
    await writeFile(receiptPath, JSON.stringify(receipt, null, 2) + "\n");
    return { artifact: dmgPath, app: appPath, receipt: receiptPath, verification };
  } catch (error) {
    receipt.finishedAt = new Date().toISOString();
    receipt.error = String(error.message || error);
    await mkdir(plan.output, { recursive: true });
    await writeFile(join(plan.output, `${slug(plan.name)}-${plan.version}.failed.json`), JSON.stringify(receipt, null, 2) + "\n");
    throw error;
  } finally {
    await rm(work, { recursive: true, force: true });
  }
}

function cliArgs(argv) {
  const [command = "plan", ...rest] = argv;
  const args = {};
  for (let i = 0; i < rest.length; i += 1) {
    if (!rest[i].startsWith("--")) continue;
    const [raw, inline] = rest[i].slice(2).split("=", 2);
    const key = ({ "bundle-id": "bundleId" })[raw] || raw;
    const value = inline ?? rest[++i];
    if (key === "include") (args.include ||= []).push(value);
    else if (key === "notarize") args.notarize = value !== "false";
    else args[key] = value;
  }
  return { command, args };
}

if (process.argv[1] && realpath(process.argv[1]).catch(() => process.argv[1]).then) {
  const invoked = await realpath(process.argv[1]).catch(() => resolve(process.argv[1]));
  if (invoked === fileURLToPath(import.meta.url)) {
    const { command, args } = cliArgs(process.argv.slice(2));
    const result = command === "build" ? await buildDmg(args)
      : command === "verify" ? await verifyArtifact(args.path)
      : await planDmg(args);
    console.log(JSON.stringify(result, null, 2));
  }
}
