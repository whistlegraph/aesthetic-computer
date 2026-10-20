#!/usr/bin/env node
// Writes system/public/sitemap-pieces.xml: every piece the docs catalog
// lists (the ones with a meta export, nothing hidden, private or a test).
// robots.txt advertises it beside sitemap.xml, which stays the machine
// discovery pages that utilities/generate-machine-manifests.mjs owns and
// checks on deploy; this file is not deploy-gated, so run it when pieces
// land (npm run seo:sitemap) and let the next deploy carry it.
//   node toolchain/seo/sitemap.mjs              writes the file
//   node toolchain/seo/sitemap.mjs --check      exits 1 if the file is stale
//   node toolchain/seo/sitemap.mjs --out <path> writes somewhere else
import { readFileSync, writeFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

const ROOT = process.env.AC_ROOT || fileURLToPath(new URL("../../", import.meta.url));
const ORIGIN = "https://aesthetic.computer";
const outFlag = process.argv.indexOf("--out");
const OUT = outFlag > -1 ? process.argv[outFlag + 1] : `${ROOT}system/public/sitemap-pieces.xml`;

async function visiblePieces() {
  const { handler } = await import(`${ROOT}system/netlify/functions/docs.js`);
  const res = await handler({ httpMethod: "GET", path: "/docs.json", headers: {}, queryStringParameters: {} });
  if (res.statusCode !== 200) throw new Error(`docs.json answered ${res.statusCode}`);
  const { pieces = {} } = JSON.parse(res.body);
  // Hidden, private and test pieces stay out; so does anything with an odd name.
  return Object.keys(pieces)
    .filter(name => !pieces[name].hidden && /^[a-z0-9][a-z0-9-]*$/i.test(name) && !/(^|-)tests?($|-)/.test(name))
    .sort();
}

export function render(pieces) {
  const urls = pieces.map(p => `${ORIGIN}/${p}`);
  const escape = s => s.replace(/&/g, "&amp;");
  return [
    `<?xml version="1.0" encoding="UTF-8"?>`,
    `<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">`,
    ...urls.map(u => `  <url><loc>${escape(u)}</loc></url>`),
    `</urlset>`,
    ``,
  ].join("\n");
}

if (process.argv[1] && fileURLToPath(import.meta.url) === process.argv[1]) {
  const pieces = await visiblePieces();
  const xml = render(pieces);
  if (process.argv.includes("--check")) {
    let current = "";
    try { current = readFileSync(OUT, "utf8"); } catch {}
    if (current !== xml) { console.error(`sitemap-pieces.xml is stale: run node toolchain/seo/sitemap.mjs`); process.exit(1); }
    console.log(`sitemap-pieces.xml is current: ${pieces.length} pieces`);
  } else {
    writeFileSync(OUT, xml);
    console.log(`wrote ${OUT}: ${pieces.length} pieces`);
  }
}
