import JSZip from "jszip";
import { existsSync, readFileSync, readdirSync, statSync, writeFileSync } from "node:fs";
import { basename, extname, join, relative, resolve, sep } from "node:path";

const TEXT_TYPES = new Set([".tex", ".bib", ".md", ".txt", ".sty", ".cls", ".json"]);

function safeRelative(root, candidate) {
  const absolute = resolve(root, candidate);
  const rel = relative(root, absolute);
  if (!rel || rel.startsWith(`..${sep}`) || rel === "..") return null;
  return rel.split(sep).join("/");
}

function configuredFiles(paperDir) {
  const configPath = join(paperDir, "source-bundle.json");
  if (!existsSync(configPath)) return [];
  const config = JSON.parse(readFileSync(configPath, "utf8"));
  if (!Array.isArray(config.include)) throw new Error("source-bundle.json must contain an include array");
  return config.include;
}

export async function createSourceBundle({ paperDir, texBase }) {
  const texName = `${texBase}.tex`;
  const defaults = [texName, `${texBase}.md`, "references.bib", "botted.json", "source-bundle.json"];
  for (const name of readdirSync(paperDir)) {
    if (name.endsWith(".sty") || name.endsWith(".cls")) defaults.push(name);
  }
  const names = [...new Set([...defaults, ...configuredFiles(paperDir)])];
  const files = [];
  for (const name of names) {
    const rel = safeRelative(paperDir, name);
    if (!rel) throw new Error(`source bundle path must stay inside the paper directory: ${name}`);
    const path = join(paperDir, rel);
    if (!existsSync(path) || !statSync(path).isFile()) continue;
    if (!TEXT_TYPES.has(extname(path).toLowerCase())) throw new Error(`source bundle only accepts text source files: ${rel}`);
    files.push({ rel, path });
  }
  files.sort((a, b) => a.rel.localeCompare(b.rel));
  if (!files.some((file) => file.rel === texName)) throw new Error(`missing primary source: ${texName}`);

  const readmeName = `${texBase}-source-README.txt`;
  const zipName = `${texBase}-source.zip`;
  const listed = files.map(({ rel }) => `- ${rel} (${extname(rel).slice(1).toUpperCase()} source)`).join("\n");
  const readme = `${basename(texBase)} — embedded source bundle\n\nIncluded files\n${listed}\n\nPurpose\nThis machine-readable bundle accompanies the rendered PDF for accessibility, audit, quotation, and reproducible editing. The PDF remains the canonical reading edition. The bundle is deliberately limited to text source files; private evidence, credentials, raw datasets, generated PDFs, and unlisted assets are excluded.\n`;

  const zip = new JSZip();
  const stableDate = new Date("2000-01-01T00:00:00Z");
  for (const { rel, path } of files) zip.file(rel, readFileSync(path), { date: stableDate, createFolders: false });
  zip.file(readmeName, readme, { date: stableDate, createFolders: false });
  const bytes = await zip.generateAsync({ type: "nodebuffer", compression: "DEFLATE", compressionOptions: { level: 9 }, platform: "UNIX" });
  writeFileSync(join(paperDir, readmeName), readme);
  writeFileSync(join(paperDir, zipName), bytes);
  return { zipName, readmeName, files: files.map(({ rel }) => rel) };
}
