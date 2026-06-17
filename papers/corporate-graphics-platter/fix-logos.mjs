// fix-logos.mjs — replace off-target search picks with direct Commons files.
// Tries candidate filenames in order; falls back to a refined search.
import { writeFileSync, existsSync, unlinkSync, readdirSync } from "node:fs";
import { execFileSync } from "node:child_process";
import { join } from "node:path";

const HERE = new URL(".", import.meta.url).pathname;
const UA = "ac-research/1.0 (mail@aesthetic.computer) logo-reference-library";
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

// slug → { cat, candidates:[exact Commons filenames], query (fallback) }
const FIXES = [
  ["rand-abc", "modernist-canon", ["American Broadcasting Company Logo.svg", "ABC 1962 logo.svg"], "American Broadcasting Company Paul Rand logo"],
  ["bass-united", "modernist-canon", ["United Airlines Logo (1974).svg", "United Airlines logo (1974-1993).svg", "Saul Bass United Airlines logo.svg"], "United Airlines tulip Saul Bass logo"],
  ["barclays-eagle", "banks-finance", ["Barclays logo.svg", "Barclays Bank logo.svg", "Barclays.svg"], "Barclays Bank eagle logo"],
  ["citi-arc", "banks-finance", ["Citi.svg", "Citibank logo.svg", "Citigroup.svg"], "Citibank red arc logo"],
  ["unilever-u", "seals-monograms", ["Unilever.svg", "Unilever logo.svg"], "Unilever U logo"],
  ["woolmark", "seals-monograms", ["Woolmark.svg", "Woolmark logo.svg"], "Woolmark logo"],
  ["penguin-roundel", "seals-monograms", ["Penguin Books logo.svg", "Penguin Books.svg"], "Penguin Books publisher logo"],
  ["mit-seal", "seals-monograms", ["MIT Seal.svg", "Seal of the Massachusetts Institute of Technology.svg"], "MIT seal logo"],
  ["good-housekeeping-seal", "seals-monograms", ["Good Housekeeping Seal.svg", "Good Housekeeping logo.svg"], "Good Housekeeping seal logo"],
  ["cg-natgeo", "modernist-canon", ["National Geographic logo.svg", "Natgeologo.svg"], "National Geographic yellow rectangle logo"],
];

async function exists(title) {
  const url = `https://commons.wikimedia.org/wiki/Special:FilePath/${encodeURIComponent(title.replace(/ /g, "_"))}`;
  const r = await fetch(url, { method: "HEAD", headers: { "User-Agent": UA }, redirect: "follow" });
  return r.ok ? title : null;
}
async function searchOne(query) {
  const url = `https://commons.wikimedia.org/w/api.php?action=query&list=search&srsearch=${encodeURIComponent(query)}&srnamespace=6&srlimit=10&format=json`;
  const r = await fetch(url, { headers: { "User-Agent": UA } });
  const j = await r.json();
  const hits = (j.query?.search || []).map((s) => s.title.replace(/^File:/, "")).filter((t) => /\.svg$/i.test(t) && /logo|seal|mark/i.test(t));
  return hits[0] || null;
}
async function grab(title, base) {
  const ext = title.match(/\.(svg|png|jpe?g)$/i)[1].toLowerCase();
  const url = `https://commons.wikimedia.org/wiki/Special:FilePath/${encodeURIComponent(title.replace(/ /g, "_"))}`;
  const r = await fetch(url, { headers: { "User-Agent": UA } });
  if (!r.ok) throw new Error(`${r.status}`);
  writeFileSync(`${base}.${ext}`, Buffer.from(await r.arrayBuffer()));
  if (ext === "svg") {
    try { execFileSync("rsvg-convert", ["-w", "600", "-b", "white", `${base}.svg`, "-o", `${base}.png`]); }
    catch { execFileSync("magick", ["-background", "white", "-density", "200", `${base}.svg`, "-resize", "600x", `${base}.png`]); }
  }
  return title;
}

for (const [slug, cat, candidates, query] of FIXES) {
  const dir = join(HERE, cat);
  const base = join(dir, slug);
  // wipe old twins
  for (const f of readdirSync(dir)) if (f.startsWith(slug + ".")) { try { unlinkSync(join(dir, f)); } catch {} }
  let chosen = null;
  for (const c of candidates) { if (await exists(c)) { chosen = c; break; } await sleep(150); }
  if (!chosen) chosen = await searchOne(query);
  if (!chosen) { console.log(`✗ ${slug}: no replacement found`); continue; }
  try { await grab(chosen, base); console.log(`✓ ${slug}  ←  ${chosen}`); }
  catch (e) { console.log(`✗ ${slug}: ${e.message}`); }
  await sleep(300);
}
console.log("fixes done");
