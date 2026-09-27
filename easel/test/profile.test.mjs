import assert from "node:assert/strict";
import { mkdtemp, mkdir, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import test from "node:test";
import { exampleConfig, globMatch, resolveProfile } from "../src/profile.mjs";

// A fake $HOME with a config and a couple of directories under it, so `~` in
// a glob has something real to expand to.
async function fixture(context, config = JSON.parse(exampleConfig())) {
  const home = await mkdtemp(join(tmpdir(), "easel-profile-"));
  context.after(() => rm(home, { recursive: true, force: true }));
  await mkdir(join(home, ".config", "easel"), { recursive: true });
  await mkdir(join(home, "fuser", "app"), { recursive: true });
  await mkdir(join(home, "ac-worktrees", "fuser-vs"), { recursive: true });
  await mkdir(join(home, "art"), { recursive: true });
  if (config) {
    await writeFile(join(home, ".config", "easel", "profiles.json"), JSON.stringify(config));
  }
  return { home, env: { HOME: home } };
}

test("the default is a public piece session", async (context) => {
  const { home, env } = await fixture(context);
  const profile = resolveProfile({ cwd: join(home, "art"), env });
  assert.deepEqual(profile, {
    name: "piece",
    private: false,
    publish: true,
    advertise: "full",
    passthrough: false,
    reason: "piece: default",
  });
});

test("--pro turns publishing off and passes the engine through", async (context) => {
  const { home, env } = await fixture(context);
  const profile = resolveProfile({ cwd: join(home, "art"), flags: { pro: true }, env });
  assert.equal(profile.name, "pro");
  assert.equal(profile.publish, false);
  assert.equal(profile.passthrough, true);
  assert.equal(profile.private, false);
  assert.equal(profile.advertise, "full");
  assert.equal(profile.reason, "pro: --pro");
});

test("a cwd under a private glob with ~ is private, and pro when listed there too", async (context) => {
  const { home, env } = await fixture(context);
  const profile = resolveProfile({ cwd: join(home, "fuser", "app"), env });
  assert.equal(profile.name, "pro");
  assert.equal(profile.private, true);
  assert.equal(profile.publish, false);
  assert.equal(profile.advertise, "status");
  assert.equal(profile.passthrough, true);
  assert.equal(profile.reason, "pro: cwd matches ~/fuser/**; private: cwd matches ~/fuser/**");

  // The directory itself, not only its children.
  assert.equal(resolveProfile({ cwd: join(home, "fuser"), env }).private, true);
  // `*` stays inside a segment, `**` crosses.
  const worktree = resolveProfile({ cwd: join(home, "ac-worktrees", "fuser-vs"), env });
  assert.equal(worktree.private, true);
  assert.equal(worktree.name, "piece", "private without pro is still a piece session");
  assert.equal(worktree.publish, false);
  assert.equal(worktree.reason, "private: cwd matches ~/ac-worktrees/fuser*/**");
  assert.equal(resolveProfile({ cwd: join(home, "art"), env }).private, false);
});

test("--private and EASEL_PRIVATE=1 force privacy anywhere", async (context) => {
  const { home, env } = await fixture(context);
  const flagged = resolveProfile({ cwd: join(home, "art"), flags: { private: true }, env });
  assert.equal(flagged.private, true);
  assert.equal(flagged.advertise, "status");
  assert.equal(flagged.publish, false);
  assert.equal(flagged.reason, "private: --private");

  const fromEnv = resolveProfile({ cwd: join(home, "art"), env: { ...env, EASEL_PRIVATE: "1" } });
  assert.equal(fromEnv.private, true);
  assert.equal(fromEnv.reason, "private: EASEL_PRIVATE=1");
  assert.equal(resolveProfile({ cwd: join(home, "art"), env: { ...env, EASEL_PRIVATE: "0" } }).private, false);
});

test("a missing or broken config falls back to defaults without throwing", async (context) => {
  const { home, env } = await fixture(context, null);
  const missing = resolveProfile({ cwd: join(home, "fuser", "app"), env });
  assert.equal(missing.name, "piece");
  assert.equal(missing.private, false);

  await writeFile(join(home, ".config", "easel", "profiles.json"), "{ nope");
  assert.equal(resolveProfile({ cwd: join(home, "fuser", "app"), env }).private, false);

  await writeFile(join(home, ".config", "easel", "profiles.json"), JSON.stringify({ private: "~/fuser/**", pro: [42, "~/fuser/**"] }));
  const partial = resolveProfile({ cwd: join(home, "fuser", "app"), env });
  assert.equal(partial.private, false, "a non-array list is ignored");
  assert.equal(partial.name, "pro", "the string entries of a mixed list still count");

  // An explicit configPath wins over the one under $HOME.
  const elsewhere = join(home, "other.json");
  await writeFile(elsewhere, JSON.stringify({ private: ["~/art"] }));
  assert.equal(resolveProfile({ cwd: join(home, "art"), configPath: elsewhere, env }).private, true);
});

test("the matcher handles ~, *, ** and literal dots", () => {
  const home = "/Users/x";
  assert.equal(globMatch("/Users/x/fuser", "~/fuser/**", home), true);
  assert.equal(globMatch("/Users/x/fuser/a/b", "~/fuser/**", home), true);
  assert.equal(globMatch("/Users/x/fuserx", "~/fuser/**", home), false);
  assert.equal(globMatch("/Users/x/a/deep/b", "~/a/**/b", home), true);
  assert.equal(globMatch("/Users/x/a/b", "~/a/**/b", home), true);
  assert.equal(globMatch("/Users/x/a/b/c", "~/a/*", home), false);
  assert.equal(globMatch("/Users/x/a.b", "~/a.b", home), true);
  assert.equal(globMatch("/Users/x/aXb", "~/a.b", home), false);
  assert.equal(globMatch("/srv/site", "/srv/*", home), true);
  assert.equal(globMatch("/Users/x", "~", home), true);
});

test("exampleConfig is valid JSON with both lists", () => {
  const parsed = JSON.parse(exampleConfig());
  assert.deepEqual(Object.keys(parsed), ["private", "pro"]);
  assert.ok(parsed.private.includes("~/fuser/**"));
});
