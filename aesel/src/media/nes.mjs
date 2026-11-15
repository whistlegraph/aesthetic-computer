// NES cartridges: one asm6-style 6502 source, assembled in-process into a
// real .nes file. No compiler to install; the assembler ships with Aesel.
import { readFile, writeFile, mkdir, realpath, lstat, rename } from "node:fs/promises";
import { resolve, relative, dirname, isAbsolute } from "node:path";
import { createHash } from "node:crypto";
import { assemble } from "../../media/nes/asm.mjs";
import { verifyNES, NES_TYPE } from "../../media/nes/rom.mjs";

export const kind = "nes";
const MAX_SOURCE = 262144;
const DIALECT =
  "asm6-style 6502: labels end in a colon, @locals belong to the label above, name = value constants, " +
  "$hex %binary 'c' numbers, < and > take low and high bytes; directives .db .dw .dsb COUNT,FILL .hex " +
  ".base ADDR (set the address) .org ADDR and .pad ADDR (fill up to it) .align .fillvalue. " +
  "Official opcodes only; no macros, includes or .incbin, so graphics are .db rows in the same file. " +
  "The file is the whole cartridge: 16-byte iNES header, PRG ending in .dw nmi, reset, irq at $FFFA, then CHR.";
const schema = (properties = {}, required = []) => ({ type: "object", properties, required, additionalProperties: false });
export const actions = [
  {
    name: "write_source",
    description: `Replace main.asm and assemble it into game.nes. ${DIALECT} A source that fails to assemble is still saved, with its line-numbered errors, and the last good ROM stays the preview.`,
    inputSchema: schema({ source: { type: "string", maxLength: MAX_SOURCE } }, ["source"]),
  },
  { name: "build", description: "Assemble main.asm into game.nes and check the cartridge header, sizes, mapper and reset vector.", inputSchema: schema() },
  { name: "verify", description: "Check the saved game.nes: iNES header, declared sizes, supported mapper, reset vector.", inputSchema: schema() },
  { name: "export", description: "Verify and return game.nes for download, mime.ac, or any NES emulator or flash cartridge.", inputSchema: schema() },
];

const hash = (bytes) => createHash("sha256").update(bytes).digest("hex");
async function safe(root, path) {
  const base = await realpath(root), target = resolve(base, path), rel = relative(base, target);
  if (isAbsolute(path) || rel === ".." || rel.startsWith("../")) throw new Error("Path escapes NES artifact.");
  for (let p = target; p !== base; p = dirname(p)) {
    try { if ((await lstat(p)).isSymbolicLink()) throw new Error("Symlinks are not allowed in NES artifacts."); }
    catch (e) { if (e.code !== "ENOENT") throw e; }
  }
  return target;
}
async function read(root, path, max = 8 * 1024 * 1024) {
  const file = await safe(root, path), stat = await lstat(file);
  if (!stat.isFile() || stat.size > max) throw new Error("Artifact file exceeds limit or is not a regular file.");
  return readFile(file);
}
async function put(root, path, bytes) {
  const file = await safe(root, path), temp = await safe(root, `${path}.pending`);
  await writeFile(temp, bytes);
  await rename(temp, file);
}
export function lintSource(source) {
  if (typeof source !== "string" || !source.trim() || Buffer.byteLength(source) > MAX_SOURCE) throw new Error(`Assembly source must be 1–${MAX_SOURCE} bytes.`);
  if (source.includes("\0")) throw new Error("NUL bytes are not supported in assembly source.");
  return { sourceHash: hash(Buffer.from(source)) };
}

async function result(root, summary, extra = {}) {
  const source = await read(root, "main.asm", MAX_SOURCE);
  const files = ["main.asm", "nes.json"];
  let preview = { path: "main.asm", mime: "text/plain" }, metadata = { sourceHash: hash(source), built: false };
  try {
    const bytes = await read(root, "game.nes"), build = JSON.parse(await read(root, "build.json"));
    metadata = { ...metadata, ...verifyNES(bytes), built: true, build, sourceAhead: build.sourceHash !== hash(source) };
    preview = { path: "game.nes", mime: NES_TYPE };
    files.push("game.nes", "build.json");
  } catch (e) { if (e.code !== "ENOENT") throw e; }
  return { files, preview, summary, metadata, ...extra };
}
async function build(root) {
  const source = (await read(root, "main.asm", MAX_SOURCE)).toString("utf8"), lint = lintSource(source);
  const rom = assemble(source).bytes, header = verifyNES(rom);
  await put(root, "game.nes", rom);
  await put(root, "build.json", JSON.stringify({
    sourceHash: lint.sourceHash, romHash: header.sha256, assembler: "aesel 6502 (asm6 dialect)", builtAt: new Date().toISOString(),
  }, null, 2) + "\n");
  return result(root, `Built NES cartridge (${rom.length} bytes, mapper ${header.mapper}, ${header.prg / 1024} KB PRG, ${header.chr / 1024} KB CHR).`);
}

export async function create({ root, name }) {
  await mkdir(root, { recursive: true });
  try { await read(root, "nes.json"); throw new Error("NES artifact already exists."); }
  catch (e) { if (e.code !== "ENOENT") throw e; }
  await put(root, "main.asm", await readFile(new URL("../../media/nes/starter.asm", import.meta.url)));
  await put(root, "nes.json", JSON.stringify({
    format: 1, name: name || "NES sketch", language: "6502-asm6", target: "ntsc", source: "main.asm", rom: "game.nes", emulator: "jsnes",
  }, null, 2) + "\n");
  return build(root);
}
export async function run({ root, action, input = {} }) {
  if (action === "write_source") {
    lintSource(input.source);
    await put(root, "main.asm", input.source);
    try { return await build(root); }
    catch (error) {
      // Keep the draft so the next edit starts from it; the old ROM stays playable.
      return result(root, `Source saved; it does not build yet.\n${error.message}`, { diagnostics: error.message });
    }
  }
  if (action === "build") return build(root);
  if (action === "verify" || action === "export") {
    if (action === "export") {
      const built = JSON.parse(await read(root, "build.json"));
      if (built.sourceHash !== hash(await read(root, "main.asm", MAX_SOURCE)))
        throw new Error("Source changed since the last successful build. Build before exporting this version.");
    }
    const header = verifyNES(await read(root, "game.nes"));
    return result(root, `${action === "export" ? "Export ready" : "Verified"}: game.nes (${header.bytes} bytes).`);
  }
  throw new Error(`Unknown NES action: ${action}`);
}
