// Build ref, 2026.11.12
// The deployed commit and the files it serves, for the service worker's
// build cache (public/sw.js). lith's checkout is `git reset --hard` to the
// deployed commit before .commit-ref is written, so the bytes it serves under
// these roots are exactly the git blobs of that commit: a build is described
// by `git ls-tree`, with no hashing of our own.

import fs from "fs";
import path from "path";
import { execFile } from "child_process";
import { promisify } from "util";

const execFileAsync = promisify(execFile);

// URL roots a build owns. Everything the prompt's boot fetches from our own
// origin lives under these, except /api and the page shell itself.
export const BUILD_ROOTS = ["aesthetic.computer", "type", "manifest.json"];
// Hash prefix length sent to clients: plenty to tell two versions apart.
export const HASH_CHARS = 12;

const publicDirs = () => [
  path.join(process.cwd(), "public"),
  path.join(process.cwd(), "..", "system", "public"),
];

// The deployed commit, or null where there is no deploy (local dev), so the
// service worker keeps its plain network-first behaviour there.
export function deployedCommit() {
  for (const dir of publicDirs()) {
    try {
      const commit = fs.readFileSync(path.join(dir, ".commit-ref"), "utf8").trim();
      if (/^[0-9a-f]{40}$/.test(commit)) return commit;
    } catch {}
  }
  return null;
}

function repoRoot() {
  for (const candidate of [path.resolve(process.cwd(), ".."), process.cwd()]) {
    if (fs.existsSync(path.join(candidate, ".git"))) return candidate;
  }
  return null;
}

// commit → Promise<{ commit, files: { "/url/path": hashPrefix } }>
const manifests = new Map();

export function buildManifest(commit) {
  if (!manifests.has(commit)) {
    const pending = (async () => {
      const root = repoRoot();
      if (!root) throw new Error("no git checkout");
      const { stdout } = await execFileAsync(
        "git",
        ["-C", root, "ls-tree", "-r", "-z", "--full-tree", commit, "--",
          ...BUILD_ROOTS.map((r) => `system/public/${r}`)],
        { timeout: 10000, maxBuffer: 32 * 1024 * 1024 },
      );
      const files = {};
      // -z: NUL-separated, paths unquoted (git quotes non-ASCII names otherwise).
      for (const line of stdout.split("\0")) {
        // <mode> blob <sha>\t<path>
        const tab = line.indexOf("\t");
        if (tab < 0) continue;
        const [, type, sha] = line.slice(0, tab).split(" ");
        if (type !== "blob") continue;
        const file = line.slice(tab + 1);
        if (file.endsWith(".map")) continue;
        files[file.slice("system/public".length)] = sha.slice(0, HASH_CHARS);
      }
      return { commit, files };
    })();
    pending.catch(() => manifests.delete(commit));
    manifests.set(commit, pending);
    // Keep the last few: a deploy only ever asks for the newest.
    while (manifests.size > 4) manifests.delete(manifests.keys().next().value);
  }
  return manifests.get(commit);
}
