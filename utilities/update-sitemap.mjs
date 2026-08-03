#!/usr/bin/env node

import { readFile, readdir, writeFile } from "node:fs/promises";
import { dirname, extname, join } from "node:path";
import { fileURLToPath } from "node:url";

const repoRoot = join(dirname(fileURLToPath(import.meta.url)), "..");
const sitemapPath = join(repoRoot, "system/public/sitemap.html");
const disksPath = join(repoRoot, "system/public/aesthetic.computer/disks");
const functionsPath = join(repoRoot, "system/netlify/functions");

const publicEndpoints = [
  ["POST", "store-kidlisp", "Publish KidLisp source"],
  ["POST", "store-clock", "Publish a clock melody"],
  ["GET", "chat-messages", "Read and search public chat"],
  ["POST", "store-piece", "Publish a JavaScript piece"],
  ["POST", "track-media", "Register uploaded media"],
];

function escapeHTML(value) {
  return value
    .replaceAll("&", "&amp;")
    .replaceAll("<", "&lt;")
    .replaceAll(">", "&gt;")
    .replaceAll('"', "&quot;");
}

function routeHref(prefix, name) {
  return `${prefix}/${name.split("/").map(encodeURIComponent).join("/")}`;
}

function routeList(names, prefix) {
  return names
    .map((name) => {
      const label = escapeHTML(name);
      const href = escapeHTML(routeHref(prefix, name));
      return `  <div class="route"><a href="${href}">${label}</a></div>`;
    })
    .join("\n");
}

function hasHandler(source) {
  return /export\s+(?:(?:async\s+)?function|const|let|var)\s+handler\b|export\s+default\s+|exports\.handler\s*=|module\.exports\s*=/.test(source);
}

const diskFiles = (await readdir(disksPath))
  .filter((name) => [".mjs", ".lisp"].includes(extname(name)))
  .sort((a, b) => a.localeCompare(b));
const javascriptDisks = diskFiles
  .filter((name) => name.endsWith(".mjs"))
  .map((name) => name.slice(0, -4));
const kidlispDisks = diskFiles
  .filter((name) => name.endsWith(".lisp"))
  .map((name) => name.slice(0, -5));
const javascriptRoutes = new Set(javascriptDisks);
const kidlispOnlyDisks = kidlispDisks.filter((name) => !javascriptRoutes.has(name));
const diskRouteCount = javascriptDisks.length + kidlispOnlyDisks.length;

const functionFiles = (await readdir(functionsPath))
  .filter((name) => [".mjs", ".js"].includes(extname(name)));
const functionHandlers = new Set();
for (const file of functionFiles) {
  const source = await readFile(join(functionsPath, file), "utf8");
  if (hasHandler(source)) functionHandlers.add(file.slice(0, -extname(file).length));
}
const handlers = [...functionHandlers].sort((a, b) => a.localeCompare(b));
const supportedNames = new Set(publicEndpoints.map(([, name]) => name));
const internalHandlers = handlers.filter((name) => !supportedNames.has(name));

const diskInventory = `<!-- generated:disk-routes:start -->
<div class="route-group-label">JavaScript (${javascriptDisks.length})</div>
<div class="routes">
${routeList(javascriptDisks, "")}
</div>
<div class="route-group-label">KidLisp-only (${kidlispOnlyDisks.length})</div>
<div class="routes">
${routeList(kidlispOnlyDisks, "")}
</div>
<div class="route-group-label">Platform &amp; legacy routes</div>
<div class="routes">
  <div class="route"><a href="/support">support</a></div>
  <div class="route"><a href="/privacy-policy.html">privacy-policy.html</a></div>
  <div class="route"><a href="/aesthetic-direct">aesthetic-direct</a></div>
  <div class="route"><a href="/bundle">bundle</a></div>
  <div class="route"><a href="/dollhouse">dollhouse</a></div>
</div>
<!-- generated:disk-routes:end -->
`;

const supportedRoutes = publicEndpoints
  .map(([method, name, description]) => `  <div class="route full-url"><a href="/api/${name}">${method} /api/${name}</a> &mdash; ${description}</div>`)
  .join("\n");

const apiCard = `<!-- API -->
<div class="card">
<details>
<summary>
  <img class="card-thumb" src="/icon/128x128/prompt.png" loading="lazy" onerror="this.style.display='none'" alt="">
  <span class="service-light service-light-unknown" data-service="Lith"></span>
  <span class="tag tag-api">API</span>
  <span class="section-title">api.aesthetic.computer</span>
  <span class="section-count">${publicEndpoints.length} supported endpoints · ${handlers.length} handlers</span>
</summary>
<div class="section-desc">The public contract is documented at <a href="https://api.aesthetic.computer">api.aesthetic.computer</a> and available as JSON. Lith also registers product, administration, and compatibility handlers under <code>/api/:fn</code>; those are not all stable public integrations.</div>
<div class="route-group-label">Supported public HTTP API</div>
<div class="routes single-col">
${supportedRoutes}
  <div class="route full-url"><a href="/api?format=json">GET /api?format=json</a> &mdash; Machine-readable reference</div>
</div>
<div class="route-group-label">Protocol and platform references</div>
<div class="routes">
  <div class="route"><a href="/mcp">mcp</a> &mdash; remote MCP server</div>
  <div class="route"><a href="/docs">docs</a> &mdash; piece API</div>
  <div class="route"><a href="/docs.json">docs.json</a> &mdash; piece API JSON</div>
</div>
<!-- generated:function-handlers:start -->
<div class="route-group-label">Other registered handlers (${internalHandlers.length})</div>
<div class="routes">
${routeList(internalHandlers, "/api")}
</div>
<!-- generated:function-handlers:end -->
</details>
</div>
`;

let html = await readFile(sitemapPath, "utf8");
html = html
  .replace(/<meta property="og:description" content="[^"]*">/, `<meta property="og:description" content="${diskRouteCount} disk routes, ${javascriptDisks.length} JavaScript disks, ${kidlispDisks.length} KidLisp disks, ${handlers.length} function handlers, and the Aesthetic Computer network.">`)
  .replace(/<div class="stats">[\s\S]*?<\/div>\n\n<input type="text"/, `<div class="stats">
  <div class="stat">Disk Routes: <strong>${diskRouteCount}</strong></div>
  <div class="stat">JavaScript Files: <strong>${javascriptDisks.length}</strong></div>
  <div class="stat">KidLisp Files: <strong>${kidlispDisks.length}</strong></div>
  <div class="stat">Function Handlers: <strong>${handlers.length}</strong></div>
</div>

<input type="text"`)
  .replace(/<span class="section-count">(?:~?\d+) disk routes<\/span>/, `<span class="section-count">${diskRouteCount} disk routes</span>`)
  .replace(/(<div class="section-desc">The main platform\.[\s\S]*?<\/div>\n)[\s\S]*?(<div class="route-group-label">Dynamic Route Patterns<\/div>)/, `$1${diskInventory}$2`)
  .replace(/<!-- API -->[\s\S]*?(?=<!-- news\.aesthetic\.computer -->)/, apiCard);

if (process.argv.includes("--check")) {
  const current = await readFile(sitemapPath, "utf8");
  if (current !== html) {
    console.error("sitemap.html is out of date; run npm run sitemap:update");
    process.exitCode = 1;
  } else {
    console.log(`sitemap.html is current: ${diskRouteCount} disk routes, ${handlers.length} function handlers`);
  }
} else {
  await writeFile(sitemapPath, html);
  console.log(`Updated sitemap.html: ${diskRouteCount} disk routes, ${handlers.length} function handlers`);
}
