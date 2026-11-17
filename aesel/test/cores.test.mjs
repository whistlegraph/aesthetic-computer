import test from "node:test";
import assert from "node:assert/strict";
import { mkdtemp, readFile, writeFile, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { assemble } from "../media/nes/asm.mjs";
import { Console, coreFor, play, CORES, MAX_FRAMES } from "../src/cores.mjs";
import { screenBlock, screenSize } from "../src/core-video.mjs";
import { runCore, handle, CORE_TOOL } from "../src/tools.mjs";
import { decode } from "../media/picture/png.mjs";

async function cartridge(t) {
  const dir = await mkdtemp(join(tmpdir(), "aesel-cores-"));
  t.after(() => rm(dir, { recursive: true, force: true }));
  const file = join(dir, "game.nes");
  await writeFile(file, assemble(await readFile(new URL("../media/nes/starter.asm", import.meta.url), "utf8")).bytes);
  return { dir, file };
}
// Leftmost column that is not the backdrop colour: where the starter's diamond is.
function left(pixels) {
  let min = 256;
  for (let i = 0; i < pixels.length; i += 4) {
    if (pixels[i] !== pixels[0] || pixels[i + 1] !== pixels[1] || pixels[i + 2] !== pixels[2]) min = Math.min(min, (i / 4) % 256);
  }
  return min;
}

test("the registry finds a core by extension and says so when none takes a file", async () => {
  assert.equal(coreFor("out/GAME.NES").id, "nes");
  assert.equal(coreFor("song.wav"), null);
  await assert.rejects(Console.open("song.wav"), /No core plays \.wav/);
  for (const row of CORES) {
    const core = (await row.load()).default;
    assert.equal(core.id, row.id);
    assert.deepEqual(core.extensions, row.extensions);
    assert.equal(core.mime, row.mime);
    for (const key of ["name", "engine", "width", "height", "fps", "buttons", "boot"]) assert.ok(core[key], `${row.id} has ${key}`);
  }
});

test("the NES core boots the starter, answers the D-pad and A, and repeats itself exactly", async (t) => {
  const { file } = await cartridge(t);
  const machine = await Console.open(file);
  machine.step(20);
  assert.equal(machine.frames, 20);
  assert.equal(left(machine.frame()), 124);
  const ran = play(machine, [{ frames: 40, hold: ["right"] }]);
  assert.ok(left(machine.frame()) > 140, "diamond moved right");
  assert.ok(ran.changedPixels > 0);
  assert.equal(ran.sound, false, "silent until A");
  assert.equal(play(machine, [{ frames: 5, hold: ["a"] }]).sound, true, "A makes a sound");
  assert.equal(machine.held.size, 0, "buttons come up when the sitting ends");

  const again = await Console.open(file);
  again.step(20);
  play(again, [{ frames: 40, hold: ["right"] }]);
  play(again, [{ frames: 5, hold: ["a"] }]);
  assert.deepEqual(Buffer.from(again.frame()), Buffer.from(machine.frame()), "same file and presses, same frame");

  assert.throws(() => machine.hold(["turbo"]), /no "turbo" button/);
  assert.throws(() => play(machine, []), /at least one step/);
  assert.throws(() => play(machine, [{ frames: MAX_FRAMES + 1 }]), /1–3600 frames/);
});

test("a console restarts only when the file's bytes change", async (t) => {
  const { file } = await cartridge(t);
  const machine = await Console.open(file);
  machine.step(30);
  assert.equal(await machine.reload(), false);
  assert.equal(machine.frames, 30);
  const bytes = await readFile(file);
  bytes[bytes.length - 1] ^= 1; // a CHR byte: a different cartridge that still boots
  await writeFile(file, bytes);
  assert.equal(await machine.reload(), true);
  assert.equal(machine.frames, 0);
});

test("a frame becomes half-block rows that keep the screen's shape", () => {
  assert.deepEqual(screenSize(256, 240, 64, 100), { columns: 64, rows: 30 });
  assert.deepEqual(screenSize(256, 240, 200, 15), { columns: 32, rows: 15 });
  const pixels = new Uint8Array(4 * 4 * 4);
  for (let i = 0; i < 16; i++) pixels.set(i < 8 ? [255, 0, 0, 255] : [0, 0, 255, 255], i * 4); // red over blue
  const block = screenBlock(pixels, 4, 4, 2, 1);
  assert.equal(block.width, 2);
  assert.equal(block.height, 1);
  assert.equal(block.lines[0], "\x1b[38;2;255;0;0m\x1b[48;2;0;0;255m▀▀\x1b[0m");
});

test("ac_core runs a file, carries on between calls, and stays inside the working directory", async (t) => {
  const { dir } = await cartridge(t);
  const first = await runCore(dir, { file: "game.nes", steps: [{ frames: 20 }] });
  const report = JSON.parse(first[0].text).untrustedCoreEvidence;
  assert.equal(report.core, "nes");
  assert.equal(report.restarted, "first run");
  assert.equal(report.frame, 20);
  assert.equal(report.analysis.width, 256);
  const png = decode(Buffer.from(first[1].data, "base64"));
  assert.equal(png.width, 256);
  assert.equal(left(png.data), 124);

  const second = JSON.parse((await runCore(dir, { file: "game.nes", steps: [{ frames: 40, hold: ["right"] }], image: false }))[0].text).untrustedCoreEvidence;
  assert.equal(second.restarted, false);
  assert.equal(second.frame, 60);
  assert.ok(second.changedPixels > 0);
  const reset = await runCore(dir, { file: "game.nes", steps: [{ frames: 1 }], reset: true, image: false });
  assert.equal(reset.length, 1);
  assert.equal(JSON.parse(reset[0].text).untrustedCoreEvidence.frame, 1);

  await assert.rejects(runCore(dir, { file: "../game.nes", steps: [{ frames: 1 }] }), /inside the working directory/);
  await assert.rejects(runCore(dir, { file: "notes.txt", steps: [{ frames: 1 }] }), /No core plays/);

  const listed = await handle({ id: 1, method: "tools/list" }, { cwd: dir });
  assert.ok(listed.result.tools.some((tool) => tool.name === CORE_TOOL.name));
  // The tool writes its cores and frame limit out by hand; keep them true.
  for (const core of CORES) assert.ok(CORE_TOOL.description.includes(`${core.id} (.${core.extensions.join(", .")})`), `description names ${core.id}`);
  assert.equal(CORE_TOOL.inputSchema.properties.steps.items.properties.frames.maximum, MAX_FRAMES);
  const called = await handle({ id: 2, method: "tools/call", params: { name: "ac_core", arguments: { file: "game.nes", steps: [{ frames: 2 }], image: false } } }, { cwd: dir });
  assert.equal(JSON.parse(called.result.content[0].text).untrustedCoreEvidence.frame, 3);
});
