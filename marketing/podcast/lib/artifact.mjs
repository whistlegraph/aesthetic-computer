// artifact.mjs — the daily token's live artifact and its TZIP-21 metadata.
//
// The crawl's KidLisp $code, packed by the oven's own bundler exactly as a
// Keep is: one self-extracting text/html file in PACK mode (the runtime, fonts
// and source inlined as a VFS; fetch() answered from it), so it runs in
// objkt's sandbox with no network. The GIF stays on as the displayUri, for
// wallets and feeds that don't run HTML.

import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";

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
