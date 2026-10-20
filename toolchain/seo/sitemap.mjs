#!/usr/bin/env node
// Writes system/public/sitemap.xml: the site's standing pages plus every
// piece the docs catalog lists (the ones with a meta export, nothing hidden
// or private). Run it when pieces land, before a deploy:
//   node toolchain/seo/sitemap.mjs              writes the file
//   node toolchain/seo/sitemap.mjs --check      exits 1 if the file is stale
//   node toolchain/seo/sitemap.mjs --out <path> writes somewhere else
import { readFileSync, writeFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

const ROOT = process.env.AC_ROOT || fileURLToPath(new URL("../../", import.meta.url));
const ORIGIN = "https://aesthetic.computer";
const outFlag = process.argv.indexOf("--out");
const OUT = outFlag > -1 ? process.argv[outFlag + 1] : `${ROOT}system/public/sitemap.xml`;

// Pages that are not pieces, in the order the old hand-written file had them.
const PAGES = ["", "about", "list", "docs", "api", "llms.txt", "llms-full.txt", "network.json"];

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
  const urls = [...PAGES.map(p => `${ORIGIN}/${p}`), ...pieces.map(p => `${ORIGIN}/${p}`)];
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
    if (current !== xml) { console.error(`sitemap.xml is stale: run node toolchain/seo/sitemap.mjs`); process.exit(1); }
    console.log(`sitemap.xml is current: ${PAGES.length + pieces.length} urls`);
  } else {
    writeFileSync(OUT, xml);
    console.log(`wrote ${OUT}: ${PAGES.length} pages + ${pieces.length} pieces`);
  }
}
