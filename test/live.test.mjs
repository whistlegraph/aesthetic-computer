import assert from "node:assert/strict";
import { mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { existsSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import test from "node:test";
import { LivePiece, pieceDirectory } from "../src/live.mjs";
import { randomChannel, randomSlug } from "../src/names.mjs";
import { qrBlock } from "../src/qr.mjs";
import { runtimeFor, runtimeForExtension, runtimeIds, runtimeMenu } from "../src/runtimes.mjs";

async function workspace(context) {
  const root = await mkdtemp(join(tmpdir(), "aesthetic-code-live-"));
  context.after(() => rm(root, { recursive: true, force: true }));
  return root;
}

const ok = () => new Response("Reloaded!", { status: 200 });

test("names are pronounceable and channels stay short enough to scan", () => {
  for (let attempt = 0; attempt < 200; attempt += 1) {
    assert.match(randomSlug(), /^[a-z]{4,6}$/);
    assert.match(randomChannel(), /^[A-Za-z0-9_-]{8}$/);
  }
  // A version-3 QR is the largest that fits a terminal corner: 33 columns.
  // Measure the real scan URL rather than a copy of it, so lengthening the URL
  // fails here instead of quietly growing the code to a version that no longer
  // fits an 80×24 window and is dropped from the frame.
  for (let attempt = 0; attempt < 50; attempt += 1) {
    const url = new LivePiece({ cwd: tmpdir() }).scanUrl;
    assert.ok(Buffer.byteLength(url) <= 53, `${url} is too long for a version-3 code`);
    const block = qrBlock(url);
    assert.equal(block.width, 33);
    assert.equal(block.height, 17);
  }
});

// The phone has to land on the prompt holding the channel as its own word.
// parse.mjs hands everything after `prompt~` to the prompt as ONE parameter,
// and the prompt splits its own arguments on spaces — a tilde glues the channel
// to the command name, so `channel` runs with no argument and joins nothing.
test("the scan URL separates the channel from the command with a space", () => {
  const live = new LivePiece({ cwd: tmpdir(), channel: "Ab0-_9Zz" });
  assert.equal(live.scanUrl, "aesthetic.computer/prompt~channel%20Ab0-_9Zz~!autorun");
  const [, command] = live.scanUrl.match(/\/prompt~(.*)~!autorun$/);
  assert.deepEqual(decodeURIComponent(command).split(" "), ["channel", "Ab0-_9Zz"]);
});

// Read the rendered block back into a grid of dark modules. A code that is
// transposed, mirrored, or off by a row still "looks like" a QR code in a
// terminal, so compare against the encoder rather than trusting the picture.
function decodeBlock(block) {
  const grid = [];
  for (const line of block.lines) {
    const cells = line.split("\u2580").slice(0, -1);
    const top = [];
    const bottom = [];
    for (const cell of cells) {
      const [, fg, bg] = cell.match(/38;5;(\d+)m\x1b\[48;5;(\d+)m$/);
      top.push(fg === "0");
      bottom.push(bg === "0");
    }
    grid.push(top, bottom);
  }
  return grid;
}

test("the rendered code matches the encoder module for module", async () => {
  const { qrcode, ErrorCorrectLevel } = await import("../src/vendor/qr.mjs");
  const url = "aesthetic.computer/prompt~channel%20Ab0-_9Zz~!autorun";
  const quiet = 2;
  const modules = qrcode(url, { errorCorrectLevel: ErrorCorrectLevel.L }).modules;
  const grid = decodeBlock(qrBlock(url, { quiet }));

  for (let row = 0; row < modules.length; row += 1) {
    for (let column = 0; column < modules.length; column += 1) {
      assert.equal(
        grid[row + quiet][column + quiet],
        Boolean(modules[row][column]),
        `module ${column},${row} does not match the encoder`,
      );
    }
  }

  // The quiet zone has to be light or a camera never finds the code.
  for (let column = 0; column < grid[0].length; column += 1) {
    assert.equal(grid[0][column], false);
    assert.equal(grid[1][column], false);
  }

  // A finder pattern is a 7x7 target: dark ring, light ring, dark 3x3 core.
  // Three corners carry one and the fourth never does, which is how a scanner
  // works out the code's orientation.
  const isFinder = (left, top) => {
    for (let y = 0; y < 7; y += 1) {
      for (let x = 0; x < 7; x += 1) {
        const ring = x === 0 || y === 0 || x === 6 || y === 6;
        const core = x >= 2 && x <= 4 && y >= 2 && y <= 4;
        if (grid[top + quiet + y][left + quiet + x] !== (ring || core)) return false;
      }
    }
    return true;
  };
  const last = modules.length - 7;
  assert.equal(isFinder(0, 0), true, "top-left finder");
  assert.equal(isFinder(last, 0), true, "top-right finder");
  assert.equal(isFinder(0, last), true, "bottom-left finder");
  assert.equal(isFinder(last, last), false, "no finder in the bottom-right");
});

test("every runtime carries a blank that names itself", () => {
  assert.deepEqual(runtimeIds(), ["mjs", "lisp", "lua"]);
  for (const id of runtimeIds()) {
    const runtime = runtimeFor(id);
    assert.match(runtime.blank("movika"), /movika/);
    assert.equal(runtimeForExtension(runtime.extension), runtime);
  }
  assert.equal(runtimeFor(".LISP").id, "lisp");
  assert.equal(runtimeFor("mjs").routable, true);
  assert.equal(runtimeFor("lua").routable, false);
  assert.throws(() => runtimeFor("rust"), /unknown runtime/);

  // The names a person reaches for resolve to the same three runtimes.
  assert.equal(runtimeFor("processing").id, "lua");
  assert.equal(runtimeFor("L5").id, "lua");
  assert.equal(runtimeFor("kidlisp").id, "lisp");
  assert.equal(runtimeFor("js").id, "mjs");
  assert.match(runtimeMenu(), /lua \(processing\)/);
});

// A live push carries no extension, so the client reads the source to decide
// it is Lua: it must open with a `--` comment and declare setup or draw. The
// Processing blank has to satisfy that on its own or it is compiled as
// JavaScript and the phone never sees the piece.
test("the Processing blank is recognisable as Lua from its source alone", () => {
  const source = runtimeFor("processing").blank("movika");
  assert.ok(source.trim().startsWith("--"), "must open with a Lua comment");
  assert.match(source, /(?:^|\n)\s*function\s+(setup|draw)\s*\(/);
  // Processing's vocabulary, not Aesthetic Computer's.
  assert.doesNotMatch(source, /function\s+paint\s*\(/);
});

test("a session mints a blank piece, pushes it, and cleans up after itself", async (context) => {
  const root = await workspace(context);
  const calls = [];
  const live = new LivePiece({
    cwd: root,
    slug: "movika",
    channel: "Ab0-_9Zz",
    fetch: async (url, options) => {
      calls.push({ url, ...JSON.parse(options.body) });
      return ok();
    },
  });

  assert.equal(live.file, join(root, "movika.mjs"));
  assert.equal(live.scanUrl, "aesthetic.computer/prompt~channel%20Ab0-_9Zz~!autorun");
  assert.equal(live.publishedUrl("jeffrey"), "https://aesthetic.computer/@jeffrey/movika");
  assert.equal(live.publishedUrl(""), "");

  live.create();
  assert.equal(existsSync(live.file), true);
  assert.equal(live.pristine, true);
  assert.match(await readFile(live.file, "utf8"), /movika/);

  assert.equal(await live.push(), true);
  assert.equal(calls.length, 1);
  assert.equal(calls[0].url, "https://aesthetic.computer/run");
  assert.equal(calls[0].piece, "movika");
  assert.equal(calls[0].codeChannel, "Ab0-_9Zz");
  assert.match(calls[0].source, /movika/);

  // An untouched blank leaves nothing behind.
  assert.equal(live.cleanup(), true);
  assert.equal(existsSync(live.file), false);
});

// The session server retains the last message a channel saw and replays it to
// late joiners, so the harness announces a piece once per save and no more. A
// watch that sees no edits must therefore be silent — and must not leave a
// timer running behind it.
test("a piece announces itself once per save, not on a beat", async (context) => {
  const root = await workspace(context);
  const calls = [];
  const live = new LivePiece({
    cwd: root,
    slug: "movika",
    fetch: async (url, options) => {
      calls.push(JSON.parse(options.body));
      return ok();
    },
  });
  live.create();
  live.watch();
  await new Promise((resolve) => setTimeout(resolve, 130));

  assert.equal(calls.length, 0, "an idle watch does not re-announce");

  await writeFile(live.file, "// touched\n");
  await new Promise((resolve) => setTimeout(resolve, 400));
  assert.equal(calls.length, 1, "a save announces exactly once");
  assert.match(calls[0].source, /touched/);
  assert.equal(live.pushes, 1, "and that push is counted as an edit");

  live.unwatch();
  await new Promise((resolve) => setTimeout(resolve, 100));
  assert.equal(calls.length, 1, "stopping the watch leaves no timer behind");
  live.cleanup();
});

test("an edited piece is never deleted on the way out", async (context) => {
  const root = await workspace(context);
  const live = new LivePiece({ cwd: root, slug: "movika", fetch: async () => ok() });
  live.create();
  await writeFile(live.file, "export function paint() {}\n");
  assert.equal(live.cleanup(), false);
  assert.equal(existsSync(live.file), true);
});

test("renaming and retargeting follow the piece without changing the channel", async (context) => {
  const root = await workspace(context);
  const live = new LivePiece({ cwd: root, slug: "movika", channel: "Ab0-_9Zz", fetch: async () => ok() });
  live.create();
  const scan = live.scanUrl;

  live.rename("smiley", "lisp");
  assert.equal(live.file, join(root, "smiley.lisp"));
  assert.equal(live.runtime.label, "kidlisp");
  assert.equal(existsSync(join(root, "movika.mjs")), false, "the untouched blank moved rather than piling up");
  assert.match(await readFile(live.file, "utf8"), /\(wipe "purple"\)/);
  assert.equal(live.scanUrl, scan, "the QR stays valid across a rename");

  assert.equal(live.retarget(join(root, "other.lisp")), true);
  assert.equal(live.slug, "other");
  assert.equal(live.retarget(join(root, "notes.txt")), false, "only piece files are tracked");
  assert.equal(live.slug, "other");
  assert.throws(() => live.rename("bad name"), /letters, digits/);
});

test("pieces land in the disks folder when the workspace is the AC repository", async (context) => {
  const root = await workspace(context);
  assert.equal(pieceDirectory(root), root);
  const disks = join(root, "system", "public", "aesthetic.computer", "disks");
  await rm(disks, { recursive: true, force: true });
  const { mkdir } = await import("node:fs/promises");
  await mkdir(disks, { recursive: true });
  assert.equal(pieceDirectory(root), disks);
  const live = new LivePiece({ cwd: root, slug: "movika", fetch: async () => ok() });
  assert.equal(live.file, join(disks, "movika.mjs"));
});

test("a failed push is reported rather than swallowed", async (context) => {
  const root = await workspace(context);
  const live = new LivePiece({
    cwd: root,
    slug: "movika",
    fetch: async () => new Response("nope", { status: 500 }),
  });
  live.create();
  await assert.rejects(() => live.push(), /HTTP 500/);
});
