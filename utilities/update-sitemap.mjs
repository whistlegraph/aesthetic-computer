#!/usr/bin/env node

import { readFile, writeFile } from "node:fs/promises";
import { properties } from "./machine-properties.mjs";
import { publicPieceFiles } from "./public-piece-catalog.mjs";
import { handler as apiDocs } from "../system/netlify/functions/api-docs.mjs";

const sitemapPath = new URL("../system/public/sitemap.html", import.meta.url);
const escapeHTML = value => String(value).replaceAll("&", "&amp;").replaceAll("<", "&lt;").replaceAll(">", "&gt;").replaceAll('"', "&quot;");
const link = (title, url, description = "") => `<div class="route full-url"><a href="${escapeHTML(url)}">${escapeHTML(title)}</a>${description ? ` — ${escapeHTML(description)}` : ""}</div>`;
const card = (title, description, rows, { open = false, grid = false, id = "" } = {}) => `<div class="card"${id ? ` id="${escapeHTML(id)}"` : ""}>
<details${open ? " open" : ""}>
<summary><span class="section-title">${escapeHTML(title)}</span></summary>
${description ? `<div class="section-desc">${escapeHTML(description)}</div>` : ""}
<div class="routes${grid ? "" : " single-col"}">
${rows.join("\n")}
</div>
</details>
</div>`;

// Only curated descriptions and supported APIs enter this page. In particular,
// adding a handler or installing a private bundle cannot publish new API links.
const files = await publicPieceFiles();
const pieces = [...new Set(files.map(file => file.replace(/\.(mjs|lisp)$/, "")))].sort();
const response = await apiDocs({ headers: {}, queryStringParameters: { format: "json" } }, {});
if (response.statusCode !== 200) throw new Error("Could not load public API documentation");
const docs = JSON.parse(response.body);
const propertyLinks = p => p.links.map(([title, url, description]) =>
  title === "Website" ? link(new URL(url).host, url) : link(title, url, description));
const productCards = properties.filter(p => p.id).map(p => card(p.title,
  `${p.summary} ${p.access}`, propertyLinks(p), { open: true, id: p.id }));
const networkCards = properties.filter(p => !p.id).map(p => card(p.title, p.summary,
  propertyLinks(p)));
const cards = [
  ...productCards,
  card(`Public AC pieces (${pieces.length})`, "Drawing, music, games and creative tools.",
    pieces.map(name => link(name, `https://aesthetic.computer/${encodeURIComponent(name)}`)), { grid: true }),
  card("Public HTTP API", "Supported integrations with documented authentication and limits.", [
    ...docs.endpoints.map(e => link(`${e.method} ${e.path}`, `${docs.baseURL}/api#${e.path.split("/").pop()}`, e.name)),
    link("API documentation", `${docs.baseURL}/api`),
    link("Machine-readable reference", `${docs.baseURL}/api?format=json`),
  ]),
  ...networkCards,
];
const template = await readFile(new URL("./sitemap.template.html", import.meta.url), "utf8");
const html = template.replace("<!-- generated:directory -->", cards.join("\n\n"));
if (process.argv.includes("--check")) {
  if (await readFile(sitemapPath, "utf8") !== html) {
    console.error("sitemap.html is out of date; run npm run sitemap:update");
    process.exitCode = 1;
  } else console.log(`sitemap.html is current: ${properties.length} properties, ${pieces.length} public pieces, ${docs.endpoints.length} supported AC APIs`);
} else {
  await writeFile(sitemapPath, html);
  console.log(`Updated sitemap.html: ${properties.length} properties, ${pieces.length} public pieces, ${docs.endpoints.length} supported AC APIs`);
}
