// The service worker trusts a cached file only if its bytes hash to the git
// blob that /api/build lists for the build (sw.js `blobMatches`).
import assert from "node:assert/strict";
import test from "node:test";
import { execFileSync } from "node:child_process";
import { createHash } from "node:crypto";
import path from "node:path";
import { fileURLToPath } from "node:url";

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "..");
process.chdir(path.join(root, "system"));
const { buildManifest, HASH_CHARS } = await import("../system/backend/build-ref.mjs");

const git = (...args) => execFileSync("git", ["-C", root, ...args]);

test("manifest hashes are git blobs of the served bytes", async () => {
  const commit = git("rev-parse", "HEAD").toString().trim();
  const { files } = await buildManifest(commit);
  for (const url of ["/aesthetic.computer/boot.mjs", "/aesthetic.computer/lib/num.mjs", "/manifest.json"]) {
    const bytes = git("show", `${commit}:system/public${url}`);
    // What sw.js computes: sha1("blob <length>\0" + bytes).
    const sha = createHash("sha1").update(`blob ${bytes.length}\0`).update(bytes).digest("hex");
    assert.equal(files[url], sha.slice(0, HASH_CHARS), url);
  }
  assert.ok(!Object.keys(files).some((f) => f.endsWith(".map")), "source maps stay out");
  assert.ok(Object.keys(files).every((f) => /^\/(aesthetic\.computer\/|type\/|manifest\.json$)/.test(f)));
});
