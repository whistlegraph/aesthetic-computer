import test from "node:test";
import assert from "node:assert/strict";
import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { playersFor, playerNamed, playerCommand, pickPlayer, choose, readChoices } from "../src/players.mjs";

const has = (...ids) => async (player) => ids.includes(player.id);

test("each platform lists the video players that can run there", () => {
  assert.deepEqual(playersFor("video", "darwin").map((p) => p.id), ["here", "quicktime", "vlc", "iina", "mpv"]);
  assert.deepEqual(playersFor("video", "linux").map((p) => p.id), ["here", "vlc", "mpv"]);
  assert.deepEqual(playersFor("paper", "darwin"), []);
  assert.equal(playerNamed("VLC").name, "VLC");
  assert.equal(playerNamed("game.nes"), undefined);
});

test("a player opens a file by app on macOS, by command elsewhere, and quietly on a reload", () => {
  const vlc = playerNamed("vlc");
  assert.deepEqual(playerCommand(vlc, "/a/clip.mp4", { platform: "darwin" }), ["open", ["-a", "VLC", "/a/clip.mp4"]]);
  assert.deepEqual(playerCommand(vlc, "/a/clip.mp4", { platform: "darwin", quiet: true }), ["open", ["-g", "-a", "VLC", "/a/clip.mp4"]]);
  assert.deepEqual(playerCommand(vlc, "/a/clip.mp4", { platform: "linux" }), ["vlc", ["/a/clip.mp4"]]);
  assert.throws(() => playerCommand(playerNamed("quicktime"), "/a/clip.mp4", { platform: "linux" }), /does not run/);
  assert.throws(() => playerCommand(playerNamed("here"), "/a/clip.mp4"), /not an app/);
  // QuickTime is opened by script so that it plays and loops; a reload does not bring it forward.
  const [command, args] = playerCommand(playerNamed("quicktime"), "/a/clip.mp4", { platform: "darwin" });
  assert.equal(command, "osascript");
  assert.ok(args.includes("set looping of film to true") && args.includes("activate"));
  assert.equal(args.at(-1), "/a/clip.mp4");
  assert.ok(!playerCommand(playerNamed("quicktime"), "/a/clip.mp4", { platform: "darwin", quiet: true })[1].includes("activate"));
});

test("the pick is the one asked for, else the one chosen before, else the first installed", async (t) => {
  const dir = await mkdtemp(join(tmpdir(), "aesel-players-"));
  t.after(() => rm(dir, { recursive: true, force: true }));
  const at = { env: { AESEL_CONFIG_DIR: dir }, platform: "darwin" };
  assert.equal((await pickPlayer("video", "", { ...at, isInstalled: has("here", "quicktime", "vlc") })).id, "here", "the terminal first, when ffmpeg is here");
  assert.equal((await pickPlayer("video", "", { ...at, isInstalled: has("quicktime", "vlc") })).id, "quicktime");
  assert.equal((await pickPlayer("video", "vlc", { ...at, isInstalled: has("quicktime", "vlc") })).id, "vlc");
  await choose("video", "vlc", at);
  assert.deepEqual(await readChoices(at), { video: "vlc" });
  assert.equal((await pickPlayer("video", "", { ...at, isInstalled: has("quicktime", "vlc") })).id, "vlc");
  assert.equal((await pickPlayer("video", "", { ...at, isInstalled: has("quicktime") })).id, "quicktime", "a chosen player that is gone falls back");
  await assert.rejects(pickPlayer("video", "vlc", { ...at, isInstalled: has("quicktime") }), /VLC is not installed/);
  await assert.rejects(pickPlayer("video", "", { ...at, isInstalled: has() }), /None of here, quicktime, vlc, iina, mpv is installed/);
  await assert.rejects(pickPlayer("paper", "", at), /No player is listed for paper/);
});
