import test from "node:test";
import assert from "node:assert/strict";
import { mkdtemp, rm, readFile } from "node:fs/promises";
import { existsSync } from "node:fs";
import { createRequire } from "node:module";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { fileURLToPath } from "node:url";
import { assemble } from "../media/nes/asm.mjs";
import { verifyNES } from "../media/nes/rom.mjs";
import { create, run } from "../src/media/nes.mjs";
import { Artifacts, MEDIA, MEDIA_TYPES, asksMediaTypes, mediaTypesText } from "../src/artifacts.mjs";

async function project(t) {
  const root = await mkdtemp(join(tmpdir(), "aesel-nes-test-"));
  t.after(() => rm(root, { recursive: true, force: true }));
  return root;
}
const hex = (source) => assemble(source).bytes.toString("hex");

test("assembler encodes addressing modes, sizes zero page, and resolves forward names", () => {
  assert.equal(
    hex('  .base $8000\nstart: lda #$10\n sta $2000\n sta $10\n lda tbl,x\n lda ($10),y\n lda ($10,x)\n jmp (start)\n bne start\n lsr a\n asl\ntbl: .db 1,2,"hi"\n .dw start, <tbl, >tbl'),
    "a9108d00208510bd1580b110a1106c0080d0ed4a0a0102686900801500800 0".replace(" ", ""),
  );
  // A zero-page name defined later is assembled as a full address, consistently.
  assert.equal(hex("  .base $8000\n lda later\n ldx early\nearly = $20\nlater = $30"), "ad3000ae2000");
  assert.equal(hex("early = $20\n  .base $8000\n ldx early\n stx early,y"), "a6209620");
  assert.equal(hex("  .base $8000\ntop:\n@loop: dex\n bne @loop\nnext:\n@loop: dey\n bne @loop"), "cad0fd88d0fd");
  assert.equal(hex(" .db 1\n .org $4\n .db 2\n .pad 8, $ff\n .hex 0a 0b\n .dsb 2, 7"), "010000000 2ffffff0a0b0707".replace(" ", ""));
  assert.equal(hex("v = (3 + 4) * 2 - 1\n .db v, v << 1, v & 9, 'A', %101, $ - 0"), "0d1a09410505");
});
test("assembler reports every bad line with its number", () => {
  assert.throws(() => assemble(" lda #$100\n foo\n jmp nowhere\n .incbin \"x\"\n sta #1"), (error) => {
    for (const part of ["line 1:", "line 2: Unknown instruction foo", "line 3: Unknown name nowhere", "line 4: Unknown directive .incbin", "line 5:"]) assert(error.message.includes(part), part);
    return true;
  });
  assert.throws(() => assemble(" .base $8000\na: .dsb 200\n bne a"), /Branch too far/);
  assert.throws(() => assemble("a:\na:"), /defined twice/);
});
test("verifier rejects truncated, mis-sized, unsupported and vectorless cartridges", async () => {
  const rom = assemble(await readFile(new URL("../media/nes/starter.asm", import.meta.url), "utf8")).bytes;
  assert.deepEqual((({ mapper, prg, chr, reset }) => ({ mapper, prg, chr, reset }))(verifyNES(rom)), { mapper: 0, prg: 16384, chr: 8192, reset: 0xc000 });
  assert.throws(() => verifyNES(rom.subarray(0, rom.length - 1)), /header declares/);
  assert.throws(() => verifyNES(Buffer.concat([rom, Buffer.alloc(1)])), /header declares/);
  assert.throws(() => verifyNES(Buffer.from("not a rom at all")), /iNES header/);
  const mapper = Buffer.from(rom); mapper[6] = 0xf0; mapper[7] = 0xf0;
  assert.throws(() => verifyNES(mapper), /Mapper 255/);
  const vector = Buffer.from(rom); vector.writeUInt16LE(0x0200, 16 + 16384 - 4);
  assert.throws(() => verifyNES(vector), /Reset vector/);
});
test("starter cartridge draws and answers the D-pad in AC's emulator", { skip: !existsSync(fileURLToPath(new URL("../../system/public/nes-player/vendor/jsnes-2.1.0.min.js", import.meta.url))) && "AC NES player not present" }, async () => {
  const jsnes = createRequire(import.meta.url)("../../system/public/nes-player/vendor/jsnes-2.1.0.min.js");
  const rom = assemble(await readFile(new URL("../media/nes/starter.asm", import.meta.url), "utf8")).bytes;
  let frame = null, low = 0, high = 0;
  const nes = new jsnes.NES({ onFrame: (f) => { frame = Array.from(f); }, onAudioSample: (l) => { low = Math.min(low, l); high = Math.max(high, l); } });
  nes.loadROM(rom.toString("binary"));
  const left = () => { const xs = []; frame.forEach((c, i) => { if (c !== frame[0]) xs.push(i % 256); }); return Math.min(...xs); };
  for (let i = 0; i < 20; i++) nes.frame();
  assert.equal(left(), 124);
  nes.buttonDown(1, jsnes.Controller.BUTTON_RIGHT);
  for (let i = 0; i < 20; i++) nes.frame();
  low = high = 0;
  for (let i = 0; i < 20; i++) nes.frame();
  nes.buttonUp(1, jsnes.Controller.BUTTON_RIGHT);
  assert(left() > 140, "diamond moved right");
  assert.equal(high - low, 0, "silent until A");
  nes.buttonDown(1, jsnes.Controller.BUTTON_A);
  for (let i = 0; i < 5; i++) nes.frame();
  assert(high - low > 0.01, "A makes a sound");
});
test("NES medium builds on create, keeps a broken draft beside the last good ROM, and refuses a stale export", async (t) => {
  const root = await project(t), made = await create({ root, name: "Cart" });
  assert.equal(made.preview.mime, "application/x-nes-rom");
  assert.deepEqual(made.files, ["main.asm", "nes.json", "game.nes", "build.json"]);
  const good = await readFile(join(root, "game.nes"));
  const broken = await run({ root, action: "write_source", input: { source: " lda nowhere\n" } });
  assert.match(broken.summary, /line 1: Unknown name nowhere/);
  assert.equal(broken.metadata.sourceAhead, true);
  assert.deepEqual(await readFile(join(root, "game.nes")), good);
  await assert.rejects(run({ root, action: "export" }), /Build before exporting/);
  await assert.rejects(run({ root, action: "build" }), /Unknown name/);
  const source = (await readFile(new URL("../media/nes/starter.asm", import.meta.url), "utf8")).replace("lda #124", "lda #40");
  const fixed = await run({ root, action: "write_source", input: { source } });
  assert.equal(fixed.metadata.sourceAhead, false);
  assert.notDeepEqual(await readFile(join(root, "game.nes")), good);
  assert.match((await run({ root, action: "export" })).summary, /Export ready/);
  await assert.rejects(run({ root, action: "write_source", input: { source: "" } }), /1–262144/);
});
test("artifact store creates, edits and exports an NES cartridge", async (t) => {
  const cwd = await project(t), store = new Artifacts(cwd);
  await store.create("nes", "Pocket");
  const current = await store.selected();
  assert.equal(current.kind, "nes");
  assert.deepEqual((await store.tools()).map((tool) => tool.name), ["artifact_write_source", "artifact_build", "artifact_verify", "artifact_export"]);
  assert.match(await store.context(), /main\.asm:/);
  assert.equal((await store.preview()).mime, "application/x-nes-rom");
  const out = join(cwd, "pocket.nes");
  assert.equal((await store.export(out)).source, "game.nes");
  verifyNES(await readFile(out));
});
test("the stock media answer lists every medium and only answers the bare question", () => {
  const text = mediaTypesText();
  for (const kind of MEDIA) { assert(text.includes(`/medium ${kind}`)); assert(text.includes(MEDIA_TYPES[kind].mime)); }
  for (const q of ["what mimes does aesel support", "What types does Aesel support?", "which media can you make"]) assert(asksMediaTypes(q), q);
  for (const q of ["make a nes rom", "what types does this function support and why is it slow", "hi"]) assert(!asksMediaTypes(q), q);
});
