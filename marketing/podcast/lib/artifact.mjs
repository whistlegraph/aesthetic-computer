// artifact.mjs — the daily token's live artifact and its TZIP-21 metadata.
//
// The crawl's KidLisp $code, packed by the oven's own bundler exactly as a
// Keep is: one self-extracting page in PACK mode (runtime, fonts and source
// inlined). Package it as index.html + covers in a ZIP, and pin those files
// as an IPFS directory so HEN/Teia select their interactive viewer.

import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { gunzipSync } from "node:zlib";
import { createHash } from "node:crypto";
import AdmZip from "adm-zip";
import { crawlLayout } from "./crawl.mjs";
import { readRichDailySource } from "./daily-richtext.mjs";
import { teiaPackage, TEIA_FORMAT } from "../../../system/backend/whistlegraph-teia.mjs";

export { teiaPackage as crawlPackage, TEIA_FORMAT };

// Preserve canonical prose outside the compressed canvas runtime. These
// files can be read without JavaScript, OCR, fonts, or a running browser.
export function contentFiles({ title, body, date, code, source }) {
  if (![title, body, date, code, source].every(v => typeof v === "string" && v.trim())) {
    throw new Error("Daily content needs a title, body, date, code and source");
  }
  const prose = lines => lines.join(" ").replace(/\s+/g, " ").trim();
  if (/^\(flow\b/m.test(source)) {
    const document = readRichDailySource(source);
    if (document.title !== title || prose([document.body]) !== prose([body])) throw new Error("Rich text differs from the episode");
  } else {
    const written = [...source.matchAll(/\(write ("(?:[^"\\]|\\.)*")/g)].map(m => JSON.parse(m[1]));
    const intended = crawlLayout({ title, body, date }).lines.filter(Boolean).map(l => l.text.replace(/\\/g, ""));
    if (prose(written) !== prose(intended)) throw new Error("Crawl text differs from the episode");
  }
  const content = { title, body, date, code, source, bodySha256: createHash("sha256").update(body).digest("hex") };
  return [
    { name: "transcript.txt", mime: "text/plain", content: Buffer.from(`${title}\n\n${body}\n`) },
    { name: "content.json", mime: "application/json", content: Buffer.from(JSON.stringify(content, null, 2) + "\n") },
    { name: "source.lisp", mime: "text/plain", content: Buffer.from(source) },
  ];
}

export function dailyPackage(html, preview, episode) {
  const prose = contentFiles(episode);
  const structured = JSON.stringify({ "@context": "https://schema.org", "@type": "CreativeWork", name: episode.title, text: episode.body, dateCreated: episode.date }).replace(/</g, "\\u003c");
  const readable = html.replace(/<head(?:\s[^>]*)?>/i, head => `${head}\n<link rel="alternate" type="text/plain" href="transcript.txt">\n<script type="application/ld+json">${structured}</script>`);
  const base = teiaPackage(readable, preview);
  const files = [...base.files, ...prose];
  const zip = new AdmZip();
  for (const file of files) zip.addFile(file.name, file.content);
  return { files, zip: zip.toBuffer() };
}

// Read the actual pinned metadata and directory, including resumed receipts.
// A local successful pack is insufficient evidence for an immutable mint.
export async function checkPublishedContent({ metadataUri, metadata, files = [], gateway = "https://ipfs.aesthetic.computer", fetch = globalThis.fetch }) {
  const read = async (uri, name = "") => {
    if (!/^ipfs:\/\/[A-Za-z0-9]+$/.test(uri || "")) throw new Error("Expected an IPFS root URI");
    const response = await fetch(`${gateway.replace(/\/$/, "")}/ipfs/${uri.slice(7)}${name ? `/${name}` : ""}`, { signal: AbortSignal.timeout(30000) });
    if (!response.ok) throw new Error(`IPFS read-back failed (${response.status}): ${name || "metadata"}`);
    return Buffer.from(await response.arrayBuffer());
  };
  const actual = JSON.parse((await read(metadataUri)).toString("utf8"));
  for (const key of ["name", "description", "artifactUri", "displayUri", "thumbnailUri"]) {
    if (actual[key] !== metadata[key]) throw new Error(`Pinned metadata differs: ${key}`);
  }
  const hashes = {};
  for (const file of files) {
    const bytes = await read(actual.artifactUri, file.name);
    if (!bytes.equals(file.content)) throw new Error(`Pinned content differs: ${file.name}`);
    hashes[file.name] = createHash("sha256").update(bytes).digest("hex");
  }
  return { checkedAt: new Date().toISOString(), metadataUri, files: hashes };
}

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "..", "..", "..");

export const ARTIFACTS = ["zip", "gif"];

// Older appliance configs may still say html; they get the corrected package.
export function artifactMode(env = process.env) {
  const mode = (env.DAILY_ARTIFACT || "zip").trim().toLowerCase();
  if (mode === "html") return "zip";
  if (!ARTIFACTS.includes(mode)) throw new Error(`DAILY_ARTIFACT must be ${ARTIFACTS.join(" or ")}, not "${mode}"`);
  return mode;
}

// A saved metadata URI is immutable once submitted. Never resume an unminted
// legacy receipt into another bare-HTML mint, or change a submitted operation.
export function checkPinnedArtifact(receipt, artifact) {
  if (receipt.metadataUri && !receipt.mintOp && receipt.tokenId === undefined &&
      (receipt.artifact !== artifact || (artifact === "zip" && receipt.artifactMimeType !== TEIA_FORMAT))) {
    throw new Error("Unminted receipt has a different artifact format; inspect its pinned metadata before retrying");
  }
}

// The crawl sizes itself from the live screen, but AC still offsets its
// wrapper to centre the canvas. A pack carries no style.css, so the wrapper
// is given the position AC's stylesheet would, or it sits in the top-left.
export const CRAWL_STYLE = "#aesthetic-computer { position: relative; overflow: hidden; }";

// The bundler reads AC_SOURCE_DIR when it loads, so the repo's own runtime is
// named first. Its deps (terser, mongodb, sharp) live in oven/node_modules.
export async function crawlBundle(code, source) {
  process.env.AC_SOURCE_DIR ||= resolve(REPO, "system", "public", "aesthetic.computer");
  const { createBundleFromSource } = await import(resolve(REPO, "oven", "bundler.mjs"));
  const { html } = await createBundleFromSource(code, source, { authorHandle: "jeffrey", style: CRAWL_STYLE });
  return html;
}

// What a bundle must carry before it is pinned. Static, so it runs on
// jasellite (no Chrome there). The runtime marks name the fixes a long crawl
// needs: a pack that colours its hidden label, or colours it quadratically,
// drew once every few seconds in objkt's sandbox (the 09-28 $gne night).
export const RUNTIME_MARKS = {
  "linear syntax highlighter": /\btokenScan\(/,
  "hidden pack label not coloured": /window\.acPACK_MODE\s*&&\s*!window\.acKEEP_LABEL\)\s*return/,
};
const REQUIRED_FILES = ["boot.mjs", "bios.mjs", "lib/disk.mjs", "lib/kidlisp.mjs"];
const EXTERNAL = /\s(src|href)=["']?https?:/i;
export const MAX_BUNDLE_BYTES = 4 * 1024 * 1024;

export function checkBundle(html, { code, source }) {
  const problems = [];
  const fail = (m) => (problems.push(m), problems);
  if (Buffer.byteLength(html) > MAX_BUNDLE_BYTES) problems.push(`bundle is ${Buffer.byteLength(html)} bytes (> ${MAX_BUNDLE_BYTES})`);
  if (EXTERNAL.test(html)) problems.push("the shell loads something from the network");
  const b64 = html.match(/const b64='([A-Za-z0-9+/=]+)'/)?.[1];
  if (!b64) return fail("not a self-extracting gzip pack");
  let inner;
  try { inner = gunzipSync(Buffer.from(b64, "base64")).toString("utf8"); } catch (err) { return fail(`payload won't gunzip: ${err.message}`); }
  if (!inner.includes("window.acPACK_MODE = true;")) problems.push("not in PACK mode");
  if (!inner.includes(`window.acSTARTING_PIECE = "$${code}";`)) problems.push(`doesn't start $${code}`);
  if (!inner.includes(`window.acKIDLISP_SOURCE = ${JSON.stringify(source)};`)) problems.push("source differs from the stored $code");
  if (!inner.includes(CRAWL_STYLE)) problems.push("missing the letterbox wrapper style");
  if (EXTERNAL.test(inner)) problems.push("the page loads something from the network");
  let vfs;
  try { vfs = JSON.parse(inner.match(/window\.VFS = (\{.*?\});\n/s)[1].replace(/<\\\/script>/g, "</script>")); } catch { return fail("no readable VFS"); }
  for (const f of REQUIRED_FILES) if (!vfs[f]) problems.push(`VFS lacks ${f}`);
  if (vfs[`disks/${code}.lisp`]?.content !== source) problems.push("VFS crawl source differs from the stored $code");
  if (/^\(flow\b/m.test(source) && (!vfs["lib/rich-text.mjs"] || !/flow:\s*\(/.test(vfs["lib/kidlisp.mjs"]?.content || ""))) problems.push("VFS lacks rich-text support");
  if (!Object.keys(vfs).some((f) => f.startsWith("disks/drawings/font_1/"))) problems.push("VFS lacks the font_1 glyphs");
  const kidlisp = vfs["lib/kidlisp.mjs"]?.content || "";
  for (const [fix, mark] of Object.entries(RUNTIME_MARKS)) if (!mark.test(kidlisp)) problems.push(`runtime lacks the ${fix}`);
  return problems;
}

export function tokenMetadata({ title, body, date, episodeUrl, code, creator, artifact = "zip", uris, ac = "https://aesthetic.computer", size = 512 }) {
  const live = artifactMode({ DAILY_ARTIFACT: artifact }) === "zip";
  if (live && !uris.directory) throw new Error("Interactive artifact needs an IPFS directory URI");
  const gif = { uri: uris.gif, mimeType: "image/gif", dimensions: { value: `${size}x${size}`, unit: "px" } };
  const directory = { uri: uris.directory, mimeType: TEIA_FORMAT, dimensions: { value: "responsive", unit: "viewport" } };
  return {
    name: title,
    description: `${body}\n\n— Aesthetic Dot Computer, ${date}. Listen: ${episodeUrl}\nThe page as a live KidLisp piece: ${ac}/${code}`,
    tags: ["aesthetic.computer", "kidlisp", "podcast", "devlog", "pixelfont"],
    symbol: "OBJKT",
    artifactUri: live ? uris.directory : uris.gif,
    displayUri: uris.gif,
    thumbnailUri: uris.thumb,
    creators: [creator],
    formats: live ? [directory, gif] : [{ uri: uris.gif, mimeType: "image/gif" }],
    decimals: 0,
    isBooleanAmount: false,
    shouldPreferSymbol: false,
    date: new Date(`${date}T20:30:00-04:00`).toISOString(),
  };
}
