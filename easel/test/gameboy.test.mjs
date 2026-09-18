import test from "node:test";
import assert from "node:assert/strict";
import {
  mkdtemp,
  rm,
  readFile,
  symlink,
  writeFile,
  mkdir,
  copyFile,
} from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import {
  create,
  run,
  findCompiler,
  lintSource,
} from "../src/media/gameboy.mjs";
import { verifyROM } from "../media/gameboy/rom.mjs";
async function project(t) {
  const root = await mkdtemp(join(tmpdir(), "easel-gb-test-"));
  t.after(() => rm(root, { recursive: true, force: true }));
  return root;
}
const compiler = await findCompiler();
test("GBDK C lane rejects host file reads, inline assembly and invalid source", () => {
  for (const source of [
    "",
    '#include "/etc/passwd"',
    "#include <../private.h>",
    "#include FILE",
    'void main(){ __asm .incbin "secret" __endasm; }',
    "#pragma bank 4",
    "#include_next <stdio.h>",
  ])
    assert.throws(() => lintSource(source));
  assert(
    lintSource("#include <gb/gb.h>\nvoid main(void){vsync();}").sourceHash
      .length === 64,
  );
});
test(
  "real GBDK compiles original starter and canonical AC hello; verifies cartridge bytes",
  { skip: !compiler && "Install GBDK to run actual compiler integration" },
  async (t) => {
    const root = await project(t),
      created = await create({ root, name: "Original" });
    assert.equal(created.preview.mime, "application/x-gameboy-rom");
    const rom = await readFile(join(root, "game.gb")),
      header = verifyROM(rom);
    assert.equal(header.title, "AESEL");
    assert.equal(header.bytes, 32768);
    assert.equal(header.cartridgeType, 0);
    assert.equal(header.color, false);
    const canonical = await readFile(
      new URL("../../kidlisp-gameboy/src/hello.c", import.meta.url),
    );
    assert.deepEqual(
      await readFile(new URL("../media/gameboy/hello.c", import.meta.url)),
      canonical,
    );
    await run({
      root,
      action: "write_source",
      input: { source: canonical.toString() },
    });
    assert.deepEqual(await readFile(join(root, "game.gb")), rom);
    const built = await run({ root, action: "build" });
    assert.equal(built.metadata.sourceAhead, false);
    assert.notEqual(built.metadata.sha256, header.sha256);
    assert.equal(
      (await run({ root, action: "export" })).preview.path,
      "game.gb",
    );
  },
);
test(
  "failed compile leaves last ROM intact; corrupt headers rejected",
  { skip: !compiler && "Install GBDK to run actual compiler integration" },
  async (t) => {
    const root = await project(t);
    await create({ root });
    const rom = await readFile(join(root, "game.gb"));
    await run({
      root,
      action: "write_source",
      input: { source: "void main(void) { this is not valid C; }" },
    });
    await assert.rejects(run({ root, action: "build" }), /GBDK build failed/);
    assert.deepEqual(await readFile(join(root, "game.gb")), rom);
    const bad = Buffer.from(rom);
    bad[0x104] ^= 1;
    assert.throws(() => verifyROM(bad), /logo/);
    const badSum = Buffer.from(rom);
    badSum[0x200] ^= 1;
    assert.throws(() => verifyROM(badSum), /checksum/);
  },
);
test("symlink source write cannot escape artifact", async (t) => {
  const root = await project(t),
    outside = join(root, "outside.c");
  await writeFile(outside, "keep");
  await symlink(outside, join(root, "main.c"));
  await assert.rejects(
    run({
      root,
      action: "write_source",
      input: { source: "void main(void){}" },
    }),
    /Symlinks/,
  );
  assert.equal(String(await readFile(outside)), "keep");
});
test(
  "adapter and scaffold work from extracted package without monorepo imports",
  { skip: !compiler && "Install GBDK to run actual compiler integration" },
  async (t) => {
    const root = await project(t),
      dist = join(root, "dist");
    await mkdir(join(dist, "src/media"), { recursive: true });
    await mkdir(join(dist, "media/gameboy"), { recursive: true });
    for (const path of [
      "src/media/gameboy.mjs",
      "media/gameboy/rom.mjs",
      "media/gameboy/starter.c",
    ])
      await copyFile(new URL("../" + path, import.meta.url), join(dist, path));
    const adapter = await import(
      "file://" + join(dist, "src/media/gameboy.mjs")
    );
    const output = await adapter.create({ root: join(root, "artifact") });
    assert.equal(output.preview.mime, "application/x-gameboy-rom");
    assert.equal(output.metadata.built, true);
  },
);
test("missing explicitly configured GBDK is source-only and gives installation instructions", async (t) => {
  const root = await project(t),
    previous = process.env.GBDK_HOME;
  process.env.GBDK_HOME = join(root, "not-installed");
  t.after(() => {
    if (previous === undefined) delete process.env.GBDK_HOME;
    else process.env.GBDK_HOME = previous;
  });
  assert.equal(await findCompiler(), null);
  const created = await create({ root });
  assert.equal(created.preview.path, "main.c");
  assert.equal(created.toolchain.available, false);
  assert(created.summary.includes("Install GBDK"));
  await assert.rejects(run({ root, action: "build" }), /Install GBDK/);
});
test(
  "export refuses source changes that have not built successfully",
  { skip: !compiler && "Install GBDK for build integration" },
  async (t) => {
    const root = await project(t);
    await create({ root });
    await run({
      root,
      action: "write_source",
      input: { source: "void main(void){}" },
    });
    await assert.rejects(
      run({ root, action: "export" }),
      /Build before exporting/,
    );
  },
);
