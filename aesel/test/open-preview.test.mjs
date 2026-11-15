import test from "node:test";
import assert from "node:assert/strict";
import { mkdtemp, rm, writeFile, readFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { previewCommand, installHint, defaultOpener, refreshOpened, openedPath } from "../src/open-preview.mjs";

test("previews use the operating system's own opener and never name an app", () => {
  assert.deepEqual(previewCommand("/tmp/game.nes", "darwin"), ["open", ["/tmp/game.nes"]]);
  assert.deepEqual(previewCommand("https://aesthetic.computer/watch/?id=a", "linux"), ["xdg-open", ["https://aesthetic.computer/watch/?id=a"]]);
  assert.deepEqual(previewCommand("C:\\game.nes", "win32"), ["explorer", ["C:\\game.nes"]]);
});
test("a file nothing opens gets players to install, per platform; ordinary files get no hint", () => {
  assert.match(installHint("/x/game.nes", "darwin"), /opens \.nes files\. OpenEmu, Mesen, Nestopia UE can: install one, then \/open again\./);
  assert.match(installHint("/x/GAME.GB", "linux"), /SameBoy, mGBA/);
  assert.equal(installHint("/x/manuscript.pdf", "darwin"), "");
  assert.equal(installHint("/x/game.nes", "freebsd"), "");
});
test("the opened copy keeps one path and follows new versions only once it has been opened", async (t) => {
  const root = await mkdtemp(join(tmpdir(), "aesel-open-"));
  t.after(() => rm(root, { recursive: true, force: true }));
  const artifact = { kind: "nes", id: "0123456789abcdef" }, v1 = join(root, "v1.nes"), v2 = join(root, "v2.nes");
  await writeFile(v1, "one"); await writeFile(v2, "two");
  assert.equal(await refreshOpened(root, artifact, v1), null, "nothing is written before the first /open");
  const opened = await refreshOpened(root, artifact, v1, { create: true });
  assert.equal(opened, openedPath(root, artifact, v1));
  assert.equal(await refreshOpened(root, artifact, v2), opened);
  assert.equal(await readFile(opened, "utf8"), "two");
});
test("the default opener is reported, or unknown where it cannot be asked", { skip: process.platform !== "darwin" && "macOS LaunchServices" }, async (t) => {
  const root = await mkdtemp(join(tmpdir(), "aesel-open-"));
  t.after(() => rm(root, { recursive: true, force: true }));
  const text = join(root, "note.txt"); await writeFile(text, "hi");
  assert.match(await defaultOpener(text), /\.app$/);
  assert.equal(await defaultOpener(text, "win32"), null);
});
