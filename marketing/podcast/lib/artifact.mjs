// artifact.mjs — the daily token's live artifact and its TZIP-21 metadata.
//
// The crawl's KidLisp $code, packed by the oven's own bundler exactly as a
// Keep is: one self-extracting text/html file in PACK mode (the runtime, fonts
// and source inlined as a VFS; fetch() answered from it), so it runs in
// objkt's sandbox with no network. The GIF stays on as the displayUri, for
// wallets and feeds that don't run HTML.

import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { gunzipSync } from "node:zlib";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "..", "..", "..");

export const ARTIFACTS = ["html", "gif"];

// DAILY_ARTIFACT=html|gif; anything else is a mistake worth stopping for.
export function artifactMode(env = process.env) {
  const mode = (env.DAILY_ARTIFACT || "html").trim().toLowerCase();
  if (!ARTIFACTS.includes(mode)) throw new Error(`DAILY_ARTIFACT must be ${ARTIFACTS.join(" or ")}, not "${mode}"`);
  return mode;
}

// The crawl pins its screen to the GIF's 512², which AC letterboxes by
// offsetting its wrapper. A pack carries no style.css, so the wrapper is
// given the position AC's stylesheet would, or it sits in the top-left.
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
  if (!Object.keys(vfs).some((f) => f.startsWith("disks/drawings/font_1/"))) problems.push("VFS lacks the font_1 glyphs");
  const kidlisp = vfs["lib/kidlisp.mjs"]?.content || "";
  for (const [fix, mark] of Object.entries(RUNTIME_MARKS)) if (!mark.test(kidlisp)) problems.push(`runtime lacks the ${fix}`);
  return problems;
}

export function tokenMetadata({ title, body, date, episodeUrl, code, creator, artifact = "html", uris, ac = "https://aesthetic.computer", size = 512 }) {
  const gif = { uri: uris.gif, mimeType: "image/gif", dimensions: { value: `${size}x${size}`, unit: "px" } };
  const html = { uri: uris.html, mimeType: "text/html", dimensions: { value: "responsive", unit: "viewport" } };
  const live = artifact === "html";
  return {
    name: title,
    description: `${body}\n\n— Aesthetic Dot Computer, ${date}. Listen: ${episodeUrl}\nThe page as a live KidLisp piece: ${ac}/${code}`,
    tags: ["aesthetic.computer", "kidlisp", "podcast", "devlog", "pixelfont"],
    symbol: "OBJKT",
    artifactUri: live ? uris.html : uris.gif,
    displayUri: uris.gif,
    thumbnailUri: uris.thumb,
    creators: [creator],
    formats: live ? [html, gif] : [{ uri: uris.gif, mimeType: "image/gif" }],
    decimals: 0,
    isBooleanAmount: false,
    shouldPreferSymbol: false,
    date: new Date(`${date}T20:30:00-04:00`).toISOString(),
  };
}
