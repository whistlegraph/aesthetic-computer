// finalize.mjs — top up seals, rebuild manifests from disk, build contact sheets.
import { writeFileSync, existsSync, readdirSync } from "node:fs";
import { execFileSync } from "node:child_process";
import { join } from "node:path";
import { META } from "./lib-data.mjs";

const HERE = new URL(".", import.meta.url).pathname;
const UA = "ac-research/1.0 (mail@aesthetic.computer) logo-reference-library";
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
const CATS = ["banks-finance", "modernist-canon", "seals-monograms", "surveillance-consent"];
const TITLE = {
  "banks-finance": "Banks & Finance",
  "modernist-canon": "Modernist Designer Canon",
  "seals-monograms": "Seals · Monograms · Notary",
  "surveillance-consent": "Surveillance · Security · Consent",
};

// reliable direct-filename top-ups for the thin seals category
const TOPUP = [
  ["seals-monograms", "scotus-seal", "Seal of the United States Supreme Court.svg"],
  ["seals-monograms", "doj-seal", "Seal of the United States Department of Justice.svg"],
  ["seals-monograms", "wordpress-w", "WordPress blue logo.svg"],
  ["seals-monograms", "yale-shield", "Yale University Shield 1.svg"],
];
async function grab(cat, slug, title) {
  const base = join(HERE, cat, slug);
  if (existsSync(`${base}.png`)) return console.log(`· ${slug} cached`);
  const url = `https://commons.wikimedia.org/wiki/Special:FilePath/${encodeURIComponent(title.replace(/ /g, "_"))}`;
  const r = await fetch(url, { headers: { "User-Agent": UA } });
  if (!r.ok) return console.log(`✗ ${slug}: ${r.status}`);
  const ext = title.match(/\.(svg|png|jpe?g)$/i)[1].toLowerCase();
  writeFileSync(`${base}.${ext}`, Buffer.from(await r.arrayBuffer()));
  if (ext === "svg") {
    try { execFileSync("rsvg-convert", ["-w", "600", "-b", "white", `${base}.svg`, "-o", `${base}.png`]); }
    catch { execFileSync("magick", ["-background", "white", "-density", "200", `${base}.svg`, "-resize", "600x", `${base}.png`]); }
  }
  console.log(`✓ ${slug}  ←  ${title}`);
}
for (const [c, s, t] of TOPUP) { await grab(c, s, t); await sleep(300); }

// rebuild manifests from disk + build contact sheets
const HELV = "/System/Library/Fonts/Helvetica.ttc";
let grand = 0;
for (const cat of CATS) {
  const dir = join(HERE, cat);
  const pngs = readdirSync(dir).filter((f) => /\.png$/.test(f) && !f.startsWith("_")).sort();
  const rows = pngs.map((f) => {
    const slug = f.replace(/\.png$/, "");
    const m = META[slug] || [slug, "—", "—", "—", "—", "—"];
    return { slug, f, name: m[0], by: m[1], year: m[2], motif: m[3], register: m[4], note: m[5] };
  });
  grand += rows.length;
  const head = `# ${TITLE[cat]}\n\n${rows.length} marks · Wikimedia Commons reference library for the REGARDE logo work.\n\n| mark | by | year | motif | aesthetic register | why it matters |\n|---|---|---|---|---|---|\n`;
  writeFileSync(join(dir, "manifest.md"),
    head + rows.map((r) => `| **${r.name}** | ${r.by} | ${r.year} | ${r.motif} | ${r.register} | ${r.note} |`).join("\n") + "\n");
  // contact sheet (labelled tiles)
  const tiles = rows.map((r) => join(dir, r.f));
  const cols = 4;
  execFileSync("magick", [
    "montage", ...tiles,
    "-font", HELV, "-pointsize", "20", "-fill", "#141414", "-label", "%t",
    "-tile", `${cols}x`, "-geometry", "300x300+10+12", "-background", "#EFEEE8",
    join(dir, `_sheet-${cat}.png`),
  ]);
  console.log(`— ${cat}: ${rows.length} marks → _sheet-${cat}.png`);
}
console.log(`done · ${grand} marks total`);
