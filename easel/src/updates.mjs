// updates — notice that a newer Easel exists, and become it.
//
// A tool installed by a shell script has no package manager behind it, so if it
// does not look after its own version nobody else will: the copy someone
// installed in September keeps telling a model about a piece API that moved in
// October, and the first symptom is bad advice rather than an error.
//
// Two rules shape everything here.
//
// It never updates a checkout. `install.json` is written into the tarball by
// bin/pack.mjs and exists nowhere else, so its absence means this Easel is a
// working copy of the repository — where overwriting src/ with a release would
// destroy someone's afternoon. Development is the case that must never be
// guessed wrong, so it is detected by a file that only a release can have,
// rather than by sniffing for .git and hoping.
//
// And it never installs bytes it did not verify. The manifest carries the
// tarball's sha256; the download is hashed before anything is unpacked, and a
// mismatch is refused rather than reported. This is code that will execute as
// the user on their next launch.
//
// The check is a courtesy, not a gate. Every failure path here is silent: no
// network, a wrong shape, a server error, a missing manifest — all of them mean
// "no update today" and none of them mean an error in front of someone trying
// to draw something.

import { execFile } from "node:child_process";
import { createHash } from "node:crypto";
import { existsSync, mkdirSync, mkdtempSync, readFileSync, renameSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { promisify } from "node:util";

const run = promisify(execFile);
const ROOT = join(dirname(fileURLToPath(import.meta.url)), "..");
const SITE = process.env.EASEL_SITE || "https://aesthetic.computer";

// Once a day. The version changes far less often than Easel opens, and a tool
// that phones home on every launch is a tool that is slow to start on a bad
// connection for no benefit.
const CHECK_INTERVAL_MS = 24 * 60 * 60 * 1000;
// Short enough that a hung endpoint never delays a session.
const TIMEOUT_MS = 4000;

const stampPath = join(ROOT, "install.json");
const statePath = join(ROOT, ".update-check.json");

// A release install, or a working checkout? Only the former may be replaced.
export function installed() {
  return existsSync(stampPath);
}

export function currentVersion() {
  try {
    return JSON.parse(readFileSync(join(ROOT, "package.json"), "utf8")).version || "";
  } catch {
    return "";
  }
}

// Semantic-ish: compare dotted integers left to right, so 0.10.0 is newer than
// 0.9.9 rather than alphabetically older. Anything unparseable compares equal,
// which fails toward not updating.
export function isNewer(latest, current) {
  const parse = (v) => String(v || "").split(".").map((n) => parseInt(n, 10));
  const a = parse(latest);
  const b = parse(current);
  if (a.some(Number.isNaN) || b.some(Number.isNaN) || !a.length || !b.length) return false;
  for (let i = 0; i < Math.max(a.length, b.length); i += 1) {
    const x = a[i] ?? 0;
    const y = b[i] ?? 0;
    if (x !== y) return x > y;
  }
  return false;
}

function lastCheckedAt() {
  try {
    return Number(JSON.parse(readFileSync(statePath, "utf8")).at) || 0;
  } catch {
    return 0;
  }
}

function noteCheck(now) {
  try {
    writeFileSync(statePath, JSON.stringify({ at: now }) + "\n");
  } catch {}
}

export async function fetchManifest({ fetch = globalThis.fetch, site = SITE } = {}) {
  const response = await fetch(`${site}/easel.json`, {
    signal: AbortSignal.timeout(TIMEOUT_MS),
    headers: { "Cache-Control": "no-cache" },
  });
  if (!response.ok) throw new Error(`manifest HTTP ${response.status}`);
  const manifest = await response.json();
  if (!manifest?.version || !manifest?.sha256) throw new Error("manifest is missing version or sha256");
  return manifest;
}

// Is there a newer Easel? Resolves null for every reason there might not be —
// including "not an install" and "asked recently" — so a caller can treat any
// non-null as news worth showing.
export async function checkForUpdate({
  fetch = globalThis.fetch,
  site = SITE,
  now = Date.now(),
  force = false,
} = {}) {
  if (!installed()) return null;
  if (!force && now - lastCheckedAt() < CHECK_INTERVAL_MS) return null;
  try {
    const manifest = await fetchManifest({ fetch, site });
    noteCheck(now);
    const current = currentVersion();
    if (!isNewer(manifest.version, current)) return null;
    return { current, ...manifest };
  } catch {
    // A failed check still counts, so a machine that is offline all week does
    // not retry on every single launch.
    noteCheck(now);
    return null;
  }
}

// Download, verify, and swap. Returns the version now installed.
//
// The swap is a rename of a fully unpacked directory, which is as close to
// atomic as this gets: at no point is there a half-written Easel at the path a
// terminal is about to launch.
export async function applyUpdate({ fetch = globalThis.fetch, site = SITE, manifest } = {}) {
  if (!installed()) throw new Error("this Easel is a checkout, not an install — use git");
  const target = manifest || (await fetchManifest({ fetch, site }));

  const response = await fetch(`${site}${target.tarball || "/easel.tar.gz"}`, {
    signal: AbortSignal.timeout(60_000),
  });
  if (!response.ok) throw new Error(`download HTTP ${response.status}`);
  const bytes = Buffer.from(await response.arrayBuffer());

  const got = createHash("sha256").update(bytes).digest("hex");
  if (got !== target.sha256) {
    throw new Error(`checksum mismatch — refusing to install (expected ${target.sha256.slice(0, 12)}…, got ${got.slice(0, 12)}…)`);
  }

  const work = mkdtempSync(join(tmpdir(), "easel-update-"));
  try {
    const archive = join(work, "easel.tar.gz");
    writeFileSync(archive, bytes);
    const unpacked = join(work, "unpacked");
    mkdirSync(unpacked);
    await run("tar", ["-xzf", archive, "-C", unpacked]);
    if (!existsSync(join(unpacked, "bin", "easel"))) {
      throw new Error("that archive does not look like Easel");
    }

    // Keep the previous install until the new one is in place, then drop it.
    const previous = `${ROOT}.previous`;
    rmSync(previous, { recursive: true, force: true });
    renameSync(ROOT, previous);
    try {
      renameSync(unpacked, ROOT);
    } catch (error) {
      renameSync(previous, ROOT); // put it back rather than leave nothing
      throw error;
    }
    rmSync(previous, { recursive: true, force: true });
    return target.version;
  } finally {
    rmSync(work, { recursive: true, force: true });
  }
}
