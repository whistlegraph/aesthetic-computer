import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import { createHash } from "node:crypto";
import { existsSync, mkdirSync, mkdtempSync, readFileSync, rmSync, symlinkSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join } from "node:path";
import test from "node:test";
import { fileURLToPath } from "node:url";
import { applyUpdate, checkForUpdate, currentVersion, fetchManifest, installed, isNewer } from "../src/updates.mjs";

const REPO = join(dirname(fileURLToPath(import.meta.url)), "..", "..");
const put = (file, text = "x") => { mkdirSync(dirname(file), { recursive: true }); writeFileSync(file, text); };

// A release as pack.mjs lays one out: the names an older updater and
// installer check for, and a version.
function release(t, version = "9.9.9") {
  const dir = mkdtempSync(join(tmpdir(), "aesel-release-"));
  t.after(() => rmSync(dir, { recursive: true, force: true }));
  const src = join(dir, "src");
  for (const name of ["easel", "aesel", "easel-desktop", "a", "aes"]) put(join(src, "bin", name), "#!/bin/sh\n");
  put(join(src, "package.json"), JSON.stringify({ version }));
  put(join(src, "install.json"), JSON.stringify({ version }));
  put(join(src, "src/tui.mjs"), "// new");
  const bytes = execFileSync("tar", ["-czf", "-", "-C", src, "bin", "package.json", "install.json", "src"]);
  return { dir, bytes, sha256: createHash("sha256").update(bytes).digest("hex"), version };
}

test("versions compare numerically, not alphabetically", () => {
  assert.equal(isNewer("0.10.0", "0.9.9"), true, "0.10 is newer than 0.9");
  assert.equal(isNewer("1.0.0", "0.99.99"), true);
  assert.equal(isNewer("0.4.0", "0.4.0"), false);
  assert.equal(isNewer("0.3.9", "0.4.0"), false);
});

// Every unparseable comparison must fail toward not updating. A tool that
// overwrites itself because it could not read a version number is worse than
// one that never updates at all.
test("anything unreadable means no update", () => {
  for (const [l, c] of [["", "0.4.0"], ["x.y.z", "0.4.0"], ["0.4.0", ""], [null, "0.4.0"], [undefined, undefined]]) {
    assert.equal(isNewer(l, c), false, `${JSON.stringify(l)} vs ${JSON.stringify(c)}`);
  }
});

// The rule that protects development: this repository is a checkout, so it must
// never see itself as updatable, whatever the server says.
test("a checkout is never an install, and never updates", async () => {
  assert.equal(installed(), false, "the repo copy must not carry an install stamp");
  const served = async () => ({
    ok: true,
    json: async () => ({ version: "99.0.0", sha256: "f".repeat(64), tarball: "/easel.tar.gz" }),
  });
  const update = await checkForUpdate({ fetch: served, force: true });
  assert.equal(update, null, "a checkout must refuse an update even when one exists");
});

test("a failed check is silent rather than an error", async () => {
  const broken = async () => { throw new Error("offline"); };
  assert.equal(await checkForUpdate({ fetch: broken, force: true }), null);
  const wrong = async () => ({ ok: true, json: async () => ({ nope: true }) });
  assert.equal(await checkForUpdate({ fetch: wrong, force: true }), null);
});

test("the current version is readable", () => {
  assert.match(currentVersion(), /^\d+\.\d+\.\d+$/);
});

test("aesel.json first, easel.json when a site has only the old name", async () => {
  const asked = [];
  const site = async (url) => {
    asked.push(url.split("/").pop());
    return url.endsWith("/aesel.json")
      ? { ok: false, status: 404 }
      : { ok: true, status: 200, json: async () => ({ version: "1.0.0", sha256: "f".repeat(64), tarball: "/easel.tar.gz" }) };
  };
  assert.equal((await fetchManifest({ fetch: site, site: "https://example.test" })).tarball, "/easel.tar.gz");
  assert.deepEqual(asked, ["aesel.json", "easel.json"]);
});

// An easel.sh install shares its folder with history; the swap used to drop it.
test("an update carries what the release does not ship into the new install", async (t) => {
  const r = release(t);
  const home = mkdtempSync(join(tmpdir(), "aesel-update-home-"));
  t.after(() => rmSync(home, { recursive: true, force: true }));
  const root = join(home, "easel");
  put(join(root, "install.json"), "{}");
  put(join(root, "bin/easel"), "old");
  put(join(root, "src/tui.mjs"), "// old");
  put(join(root, "history/abc/v1.json"), "kept");
  put(join(root, ".update-check.json"), "{}");
  const fetch = async () => ({ ok: true, arrayBuffer: async () => r.bytes });
  const version = await applyUpdate({ fetch, site: "https://example.test", root,
    manifest: { version: r.version, sha256: r.sha256, tarball: "/aesel.tar.gz" } });
  assert.equal(version, "9.9.9");
  assert.equal(readFileSync(join(root, "src/tui.mjs"), "utf8"), "// new");
  assert.equal(readFileSync(join(root, "history/abc/v1.json"), "utf8"), "kept");
  assert.ok(existsSync(join(root, ".update-check.json")));
  assert.ok(!existsSync(`${root}.previous`));
});

test("an install reached through a symlink is swapped at its real path", async (t) => {
  const r = release(t);
  const home = mkdtempSync(join(tmpdir(), "aesel-update-link-"));
  t.after(() => rmSync(home, { recursive: true, force: true }));
  const real = join(home, "aesel"), link = join(home, "easel");
  put(join(real, "install.json"), "{}");
  put(join(real, "bin/easel"), "old");
  symlinkSync(real, link);
  const fetch = async () => ({ ok: true, arrayBuffer: async () => r.bytes });
  await applyUpdate({ fetch, site: "https://example.test", root: link,
    manifest: { version: r.version, sha256: r.sha256 } });
  assert.equal(readFileSync(join(link, "src/tui.mjs"), "utf8"), "// new");
  assert.equal(readFileSync(join(real, "src/tui.mjs"), "utf8"), "// new");
});

// aesel.sh and easel.sh are one script under two names; each fetches its own
// tarball, installs beside history rather than over it, and carries anything
// an older install folder held.
test("the installer is the same script under both names and keeps what it finds", (t) => {
  const aesel = readFileSync(join(REPO, "system/public/aesel.sh"), "utf8");
  const easel = readFileSync(join(REPO, "system/public/easel.sh"), "utf8");
  assert.equal(easel, aesel.replace("TARBALL=aesel.tar.gz", "TARBALL=easel.tar.gz"));

  const r = release(t, "1.2.3");
  const home = join(r.dir, "home");
  writeFileSync(join(r.dir, "aesel.tar.gz"), r.bytes);
  const prefix = join(home, ".local/share/aesel/app");
  put(join(prefix, "bin/easel"), "old");
  put(join(prefix, "history/abc/v1.json"), "kept");
  const env = { PATH: process.env.PATH, HOME: home, AESEL_SITE: `file://${r.dir}` };
  execFileSync("sh", [join(REPO, "system/public/aesel.sh")], { env, stdio: "pipe" });
  assert.equal(readFileSync(join(prefix, "src/tui.mjs"), "utf8"), "// new");
  assert.equal(readFileSync(join(prefix, "history/abc/v1.json"), "utf8"), "kept");
  for (const name of ["ac", "a", "aes", "aesel", "easel"]) assert.ok(existsSync(join(home, ".local/bin", name)), name);
});
