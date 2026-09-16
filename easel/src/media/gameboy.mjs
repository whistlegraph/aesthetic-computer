import {
  readFile,
  writeFile,
  mkdir,
  realpath,
  lstat,
  access,
  rename,
  rm,
  mkdtemp,
} from "node:fs/promises";
import { constants } from "node:fs";
import {
  join,
  resolve,
  relative,
  dirname,
  isAbsolute,
  delimiter,
} from "node:path";
import { homedir } from "node:os";
import { fileURLToPath } from "node:url";
import { createHash } from "node:crypto";
import { spawn } from "node:child_process";
import { verifyROM } from "../../media/gameboy/rom.mjs";
export const kind = "gameboy";
const install =
  "Install GBDK-2020 from https://github.com/gbdk-2020/gbdk-2020/releases and set GBDK_HOME to its gbdk directory. Easel uses the same lcc compiler as kidlisp-gameboy/build.sh.";
const schema = (properties = {}, required = []) => ({
  type: "object",
  properties,
  required,
  additionalProperties: false,
});
export const actions = [
  {
    name: "write_source",
    description:
      "Replace main.c, preserving the last successful ROM preview. Uses the existing AC Game Boy GBDK C stack; no host scripts or compiler flags.",
    inputSchema: schema({ source: { type: "string", maxLength: 131072 } }, [
      "source",
    ]),
  },
  {
    name: "build",
    description:
      "Compile main.c with GBDK lcc into a real game.gb; verify cartridge header/checksums before replacing the preview. Missing compiler gives installation instructions.",
    inputSchema: schema(),
  },
  {
    name: "verify",
    description: "Verify the saved cartridge ROM header, size and checksums.",
    inputSchema: schema(),
  },
  {
    name: "export",
    description:
      "Verify and return game.gb for download or the existing AC WasmBoy emulator.",
    inputSchema: schema(),
  },
  {
    name: "toolchain",
    description:
      "Report availability of the existing GBDK compiler without downloading or building.",
    inputSchema: schema(),
  },
];
const hash = (bytes) => createHash("sha256").update(bytes).digest("hex");
async function safe(root, path) {
  const base = await realpath(root),
    target = resolve(base, path),
    rel = relative(base, target);
  if (isAbsolute(path) || rel === ".." || rel.startsWith("../"))
    throw new Error("Path escapes Game Boy artifact.");
  let p = target;
  while (p !== base) {
    try {
      if ((await lstat(p)).isSymbolicLink())
        throw new Error("Symlinks are not allowed in Game Boy artifacts.");
    } catch (e) {
      if (e.code !== "ENOENT") throw e;
    }
    p = dirname(p);
  }
  return target;
}
async function read(root, path, max = 8 * 1024 * 1024) {
  const file = await safe(root, path),
    stat = await lstat(file);
  if (!stat.isFile() || stat.size > max)
    throw new Error("Artifact file exceeds limit or is not a regular file.");
  return readFile(file);
}
async function put(root, path, bytes) {
  const file = await safe(root, path),
    temp = await safe(root, path + ".pending");
  await writeFile(temp, bytes);
  await rename(temp, file);
}
export function lintSource(source) {
  if (
    typeof source !== "string" ||
    !source.trim() ||
    Buffer.byteLength(source) > 131072
  )
    throw new Error("C source must be 1–131072 bytes.");
  if (
    source.includes("\0") ||
    source.includes("??") ||
    source.includes("##") ||
    source.includes("%:") ||
    /\\\r?\n/.test(source)
  )
    throw new Error(
      "NUL, trigraphs, token-pasting and continued directives are not supported.",
    );
  const plain = source
    .replace(/\/\*[\s\S]*?\*\//g, " ")
    .replace(/\/\/[^\n]*/g, "");
  if (
    /\b(?:__asm|__asm__|asm|INCBIN)\b|\.incbin\b|^\s*#\s*(?:pragma|include_next|embed)\b/im.test(
      plain,
    )
  )
    throw new Error(
      "Inline assembly, binary includes and compiler pragmas are not supported in the portable C lane.",
    );
  for (const line of plain.split("\n"))
    if (/^\s*#\s*include\b/.test(line)) {
      const match = line.match(/^\s*#\s*include\s*<([a-zA-Z0-9_\-/]+\.h)>\s*$/);
      if (!match || match[1].startsWith("/") || match[1].includes(".."))
        throw new Error(
          "Use GBDK/system angle-bracket headers only; project/absolute/macro includes are not supported.",
        );
    }
  return { bytes: Buffer.byteLength(source), sourceHash: hash(source) };
}
export async function findCompiler(env = process.env) {
  const repo = fileURLToPath(
    new URL("../../../kidlisp-gameboy/gbdk/bin/lcc", import.meta.url),
  );
  const candidates = env.GBDK_HOME
    ? [join(env.GBDK_HOME, "bin", "lcc")]
    : [
        repo,
        join(
          homedir(),
          ".local/share/easel/toolchains/gbdk-4.5.0/gbdk/bin/lcc",
        ),
        ...(env.PATH || "")
          .split(delimiter)
          .filter(Boolean)
          .map((p) => join(p, "lcc")),
      ];
  for (const file of candidates) {
    try {
      await access(file, constants.X_OK);
      const real = await realpath(file);
      await access(join(dirname(real), "../include/gb/gb.h"));
      return real;
    } catch {}
  }
  return null;
}
function compile(compiler, cwd) {
  return new Promise((yes, no) => {
    const bin = dirname(compiler),
      child = spawn(compiler, ["-Wm-ynEASEL", "-o", "game.gb", "main.c"], {
        cwd,
        detached: process.platform !== "win32",
        stdio: ["ignore", "pipe", "pipe"],
        env: {
          PATH: [bin, "/usr/bin", "/bin"].join(delimiter),
          TMPDIR: cwd,
          TMP: cwd,
          TEMP: cwd,
        },
      });
    let output = "",
      failure = null;
    const kill = () => {
      try {
        if (process.platform === "win32") child.kill("SIGKILL");
        else process.kill(-child.pid, "SIGKILL");
      } catch {}
    };
    const timer = setTimeout(() => {
      failure = new Error("GBDK build exceeded 20 seconds.");
      kill();
    }, 20000);
    const collect = (data) => {
      output += data.toString();
      if (output.length > 131072) {
        failure = new Error("GBDK diagnostics exceeded 128 KB.");
        kill();
      }
    };
    child.stdout.on("data", collect);
    child.stderr.on("data", collect);
    child.on("error", (e) => {
      clearTimeout(timer);
      no(e);
    });
    child.on("close", (code) => {
      clearTimeout(timer);
      if (failure) no(failure);
      else if (code !== 0)
        no(new Error(`GBDK build failed (${code}):\n${output.slice(-8000)}`));
      else yes(output);
    });
  });
}
async function result(root, summary, extra = {}) {
  const source = await read(root, "main.c", 131072);
  const files = ["main.c", "gameboy.json"];
  let preview = { path: "main.c", mime: "text/plain" },
    metadata = { sourceHash: hash(source), built: false };
  try {
    const bytes = await read(root, "game.gb"),
      build = JSON.parse(await read(root, "build.json"));
    metadata = {
      ...metadata,
      ...verifyROM(bytes),
      built: true,
      build,
      sourceAhead: build.sourceHash !== hash(source),
    };
    preview = { path: "game.gb", mime: "application/x-gameboy-rom" };
    files.push("game.gb", "build.json");
  } catch (e) {
    if (e.code !== "ENOENT") throw e;
  }
  return { files, preview, summary, metadata, ...extra };
}
export async function create({ root, name }) {
  await mkdir(root, { recursive: true });
  try {
    await read(root, "gameboy.json");
    throw new Error("Game Boy artifact already exists.");
  } catch (e) {
    if (e.code !== "ENOENT") throw e;
  }
  await put(
    root,
    "main.c",
    await readFile(new URL("../../media/gameboy/starter.c", import.meta.url)),
  );
  await put(
    root,
    "gameboy.json",
    JSON.stringify(
      {
        format: 1,
        name: name || "Game Boy sketch",
        language: "gbdk-c",
        target: "dmg",
        source: "main.c",
        rom: "game.gb",
        stack: "kidlisp-gameboy",
        emulator: "wasmboy",
      },
      null,
      2,
    ) + "\n",
  );
  if (await findCompiler()) return run({ root, action: "build" });
  return result(root, `Game Boy C source ready. ${install}`, {
    toolchain: { available: false, instructions: install },
  });
}
export async function run({ root, action, input = {} }) {
  if (action === "toolchain") {
    const compiler = await findCompiler();
    return result(root, compiler ? "GBDK compiler available." : install, {
      toolchain: {
        available: !!compiler,
        instructions: compiler ? undefined : install,
      },
    });
  }
  if (action === "write_source") {
    lintSource(input.source);
    await put(root, "main.c", input.source);
    return result(
      root,
      "C source saved. Build to update the cartridge preview.",
    );
  }
  if (action === "build") {
    const source = (await read(root, "main.c", 131072)).toString("utf8"),
      lint = lintSource(source),
      compiler = await findCompiler();
    if (!compiler) throw new Error(install);
    const directory = await mkdtemp(join(await realpath(root), ".gb-build-"));
    try {
      await writeFile(join(directory, "main.c"), source);
      const log = await compile(compiler, directory);
      const rom = await read(directory, "game.gb");
      const header = verifyROM(rom);
      await put(root, "game.gb", rom);
      await put(
        root,
        "build.json",
        JSON.stringify(
          {
            sourceHash: lint.sourceHash,
            romHash: header.sha256,
            compiler: "GBDK-2020 lcc",
            compilerSha256: hash(await readFile(compiler)),
            stack: "kidlisp-gameboy/build.sh",
            builtAt: new Date().toISOString(),
            diagnostics: log.slice(-8000),
          },
          null,
          2,
        ) + "\n",
      );
      return result(
        root,
        `Built real Game Boy cartridge (${rom.length} bytes).`,
      );
    } finally {
      await rm(directory, { recursive: true, force: true });
    }
  }
  if (action === "verify" || action === "export") {
    if (action === "export") {
      const built = JSON.parse(await read(root, "build.json"));
      if (built.sourceHash !== hash(await read(root, "main.c", 131072)))
        throw new Error(
          "Source changed since the last successful build. Build before exporting this version.",
        );
    }
    const header = verifyROM(await read(root, "game.gb"));
    return result(
      root,
      `${action === "export" ? "Export ready" : "Verified"}: game.gb (${header.bytes} bytes).`,
    );
  }
  throw new Error(`Unknown Game Boy action: ${action}`);
}
