import assert from "node:assert/strict";
import { existsSync, lstatSync, mkdirSync, mkdtempSync, readFileSync, readlinkSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import test from "node:test";
import { adoptLegacyEnv, bothNames } from "../src/env.mjs";
import { cacheDir, configDir, historyDir, journalDir, mediaDir, toolchainsDir, transcriptsDir, workspaceDir, workspaceName } from "../src/paths.mjs";

function home(t) {
  const dir = mkdtempSync(join(tmpdir(), "aesel-paths-"));
  t.after(() => rmSync(dir, { recursive: true, force: true }));
  return dir;
}
const put = (file, text = "x") => { mkdirSync(join(file, ".."), { recursive: true }); writeFileSync(file, text); };

test("an old ~/.config/easel moves across once and leaves a link behind", (t) => {
  const h = home(t), env = {};
  put(join(h, ".config/easel/profiles.json"), "{}");
  assert.equal(configDir({ home: h, env }), join(h, ".config/aesel"));
  assert.equal(readFileSync(join(h, ".config/aesel/profiles.json"), "utf8"), "{}");
  assert.ok(lstatSync(join(h, ".config/easel")).isSymbolicLink(), "older copies still find it");
  assert.equal(readlinkSync(join(h, ".config/easel")), join(h, ".config/aesel"));
  assert.equal(configDir({ home: h, env }), join(h, ".config/aesel"), "and the second run is a no-op");
});

test("when both config folders exist the new one wins and the old one is untouched", (t) => {
  const h = home(t), env = {};
  put(join(h, ".config/easel/old.json"));
  put(join(h, ".config/aesel/new.json"));
  assert.equal(configDir({ home: h, env }), join(h, ".config/aesel"));
  assert.ok(existsSync(join(h, ".config/easel/old.json")));
  assert.ok(!lstatSync(join(h, ".config/easel")).isSymbolicLink());
});

test("AESEL_CONFIG_DIR, or EASEL_CONFIG_DIR through the env shim, overrides", (t) => {
  const h = home(t);
  assert.equal(configDir({ home: h, env: { AESEL_CONFIG_DIR: "/somewhere" } }), "/somewhere");
  assert.equal(configDir({ home: h, env: adoptLegacyEnv({ EASEL_CONFIG_DIR: "/legacy" }) }), "/legacy");
});

test("~/.local/share moves one folder at a time, so an install there stays put", (t) => {
  const h = home(t), env = {}, old = join(h, ".local/share/easel");
  put(join(old, "bin/easel"));
  put(join(old, "install.json"));
  put(join(old, "history/abc/v1.json"));
  put(join(old, "transcripts/one.easel"));
  put(join(old, "toolchains/gbdk-4.5.0/gbdk/bin/lcc"));
  // transcript.mjs has written here for a while, so the new folder exists already.
  put(join(h, ".local/share/aesel/transcripts/session/events.jsonl"));

  assert.equal(historyDir({ home: h, env }), join(h, ".local/share/aesel/history"));
  assert.ok(existsSync(join(h, ".local/share/aesel/history/abc/v1.json")));
  assert.equal(journalDir({ home: h, env }), join(h, ".local/share/aesel/journal"));
  assert.ok(existsSync(join(h, ".local/share/aesel/journal/one.easel")), "the .easel journal keeps apart from the session log");
  assert.equal(transcriptsDir({ home: h, env }), join(h, ".local/share/aesel/transcripts"));
  assert.equal(toolchainsDir({ home: h, env }), join(h, ".local/share/aesel/toolchains"));

  assert.ok(existsSync(join(old, "bin/easel")) && existsSync(join(old, "install.json")), "the install is not moved");
  for (const name of ["history", "transcripts", "toolchains"]) {
    assert.ok(lstatSync(join(old, name)).isSymbolicLink(), `${name} is still reachable at the old path`);
  }
  assert.ok(existsSync(join(old, "history/abc/v1.json")));
});

test("AESEL_HISTORY_DIR and AESEL_TRANSCRIPTS still override", (t) => {
  const h = home(t);
  assert.equal(historyDir({ home: h, env: { AESEL_HISTORY_DIR: "/h" } }), "/h");
  assert.equal(transcriptsDir({ home: h, env: { AESEL_TRANSCRIPTS: "/t" } }), "/t");
});

test("the cache follows XDG_CACHE_HOME and moves like config", (t) => {
  const h = home(t), xdg = join(h, "xdg");
  put(join(xdg, "easel/models-claude.json"));
  assert.equal(cacheDir({ home: h, env: { XDG_CACHE_HOME: xdg } }), join(xdg, "aesel"));
  assert.ok(existsSync(join(xdg, "aesel/models-claude.json")));
  assert.equal(cacheDir({ home: h, env: {} }), join(h, ".cache/aesel"));
});

test("workspaces keep an .easel they have, prefer .aesel, and are never moved", (t) => {
  const fresh = home(t);
  assert.equal(workspaceDir(fresh), join(fresh, ".aesel"));
  assert.equal(mediaDir(fresh), join(fresh, ".aesel-media"));

  const legacy = home(t);
  mkdirSync(join(legacy, ".easel"));
  mkdirSync(join(legacy, ".easel-media"));
  assert.equal(workspaceDir(legacy), join(legacy, ".easel"));
  assert.equal(workspaceName(legacy), ".easel");
  assert.equal(mediaDir(legacy), join(legacy, ".easel-media"));
  assert.ok(!existsSync(join(legacy, ".aesel")), "a checked-in .easel is left where it is");

  mkdirSync(join(legacy, ".aesel"));
  assert.equal(workspaceDir(legacy), join(legacy, ".aesel"));
});

test("EASEL_* in the environment becomes AESEL_*, and the new name wins", () => {
  const env = adoptLegacyEnv({ EASEL_SITE: "old", EASEL_MOUSE: "0", AESEL_MOUSE: "1", PATH: "/bin" });
  assert.equal(env.AESEL_SITE, "old");
  assert.equal(env.AESEL_MOUSE, "1");
  assert.equal(env.EASEL_SITE, "old", "the old name is left for children that read it");
});

test("what Aesel hands a child goes out under both names", () => {
  assert.deepEqual(bothNames({ AESEL_SESSION_ID: "s", SLAB_AGENT_TYPE: "easel", AESEL_GONE: undefined }), {
    AESEL_SESSION_ID: "s", EASEL_SESSION_ID: "s", SLAB_AGENT_TYPE: "easel",
  });
});
