// Pack a game folder into one validated .oskie file.
//   node system/tests/oskie/pack.mjs <dir> [out.oskie]
// <dir>/manifest.json gives slug, title and optional description/author/engine.
// Every other file in the folder rides along: text for code, markdown, lisp and
// json; base64 for images and sounds. Scratch output (shots/, report.json,
// DONE.json, *.oskie, node_modules, dot files) stays out of the bundle.
import { readFile, readdir, stat, writeFile } from "node:fs/promises";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { validateGame, FORMAT, VERSION } from "../../public/oskie/format.mjs";

const TEXT = {
  ".mjs": "text/javascript", ".js": "text/javascript", ".lisp": "text/plain",
  ".md": "text/markdown", ".json": "application/json", ".txt": "text/plain",
  ".csv": "text/csv", ".svg": "image/svg+xml",
};
const BINARY = {
  ".png": "image/png", ".jpg": "image/jpeg", ".jpeg": "image/jpeg", ".gif": "image/gif",
  ".webp": "image/webp", ".wav": "audio/wav", ".mp3": "audio/mpeg", ".ogg": "audio/ogg",
  ".woff2": "font/woff2", ".ttf": "font/ttf",
};
const SKIP = new Set(["manifest.json", "report.json", "DONE.json", "node_modules", "shots"]);

export async function packDir(dir) {
  const manifest = JSON.parse(await readFile(path.join(dir, "manifest.json"), "utf8"));
  const files = {};
  async function walk(rel) {
    for (const name of await readdir(path.join(dir, rel))) {
      if (name.startsWith(".") || SKIP.has(name) || name.endsWith(".oskie")) continue;
      const p = rel ? rel + "/" + name : name, full = path.join(dir, p);
      if ((await stat(full)).isDirectory()) { await walk(p); continue; }
      const ext = path.extname(name).toLowerCase();
      if (TEXT[ext]) files[p] = { type: TEXT[ext], text: await readFile(full, "utf8") };
      else if (BINARY[ext]) files[p] = { type: BINARY[ext], base64: (await readFile(full)).toString("base64") };
      else console.warn("pack: skipping unknown file type", p);
    }
  }
  await walk("");
  if (!files["game.mjs"]) throw Error("pack: " + dir + " has no game.mjs");
  const bundle = {
    format: FORMAT, version: VERSION,
    manifest: {
      slug: manifest.slug || path.basename(dir),
      title: manifest.title || manifest.slug || path.basename(dir),
      ...(manifest.description ? { description: manifest.description } : {}),
      ...(manifest.author ? { author: manifest.author } : {}),
      mode: "own", runtime: "oskiewar-1", entry: "game.mjs",
      ...(manifest.engine ? { engine: manifest.engine } : {}),
    },
    files,
  };
  return validateGame(bundle);
}

if (process.argv[1] === fileURLToPath(import.meta.url)) {
  const [dir, out] = process.argv.slice(2);
  if (!dir) { console.error("usage: pack.mjs <dir> [out.oskie]"); process.exit(2); }
  const game = await packDir(path.resolve(dir));
  const target = out || path.join(path.resolve(dir), game.manifest.slug + ".oskie");
  const json = JSON.stringify(game);
  await writeFile(target, json);
  console.log(`packed ${game.manifest.slug} → ${target} (${Object.keys(game.files).length} files, ${(json.length / 1024).toFixed(1)} KB)`);
}
