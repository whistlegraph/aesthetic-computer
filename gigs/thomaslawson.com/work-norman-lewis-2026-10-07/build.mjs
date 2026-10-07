import fs from 'node:fs/promises';
import path from 'node:path';
import assert from 'node:assert/strict';

const [sourceDirectory, outputDirectory] = process.argv.slice(2);
assert.ok(sourceDirectory && outputDirectory, 'Usage: node build.mjs private-source-directory output-directory');
const data = JSON.parse(await fs.readFile(path.join(sourceDirectory, 'entry-source.json'), 'utf8'));
const root = new URL('./', import.meta.url);
const css = await fs.readFile(new URL('norman.css', root), 'utf8');
const js = await fs.readFile(new URL('index.js', root), 'utf8');
const escape = text => text.replace(/[&<>"']/g, c => ({'&':'&amp;','<':'&lt;','>':'&gt;','"':'&quot;',"'":'&#39;'}[c]));
const base = '/wp-content/uploads/tl-refresh/norman-lewis-1976/';
const route = '/art-in-context-norman-lewis/';
const title = 'Norman Lewis: A Retrospective';
const intro = escape(data.introduction);
assert.equal(data.title, title);
const content = `<main id="main" class="site-main tl-norman">
<header class="tl-norman-header">
<figure class="tl-norman-cover"><a href="${base}catalogue.pdf" aria-label="Read the exhibition catalogue (PDF)"><img src="${base}catalogue-cover.jpg" width="552" height="600" alt="Cover of Norman Lewis: A Retrospective, 1976"></a></figure>
<div class="tl-norman-copy"><a class="tl-norman-back" href="/art-in-a-broader-context/">Art in a Broader Context</a>
<h1>${title}</h1>
<p class="tl-norman-meta">October 12 – November 19, 1976<br>CUNY Graduate Center, New York · Mall and 18th Floor<br>Curated by Thomas Lawson</p>
<p class="tl-norman-intro">${intro}</p>
<p class="tl-norman-catalogue"><a href="${base}catalogue.pdf">Read the catalogue (PDF, 11 pages)</a><small>Essay by Thomas Lawson · Foreword by Milton W. Brown</small></p>
</div></header>
<section class="tl-norman-install" aria-labelledby="tl-norman-install-title"><h2 id="tl-norman-install-title">Installation views</h2><div class="tl-norman-views">
<figure><a href="${base}installation-1.png"><img src="${base}installation-1.png" width="400" height="233" loading="lazy" alt="Norman Lewis paintings installed around a concrete stair landing"></a></figure>
<figure><a href="${base}installation-2.png"><img src="${base}installation-2.png" width="400" height="232" loading="lazy" alt="Norman Lewis paintings arranged along adjoining gallery walls"></a></figure>
<figure><a href="${base}installation-3.png"><img src="${base}installation-3.png" width="400" height="200" loading="lazy" alt="A row of Norman Lewis paintings in the concrete-walled gallery"></a></figure>
</div></section>
<nav class="tl-norman-neighbors" aria-label="Other exhibitions"><a href="/art-in-context-reallife-presents/">← REALLIFE Magazine Presents</a><a href="/pat-douthewaite-art-in-context/">Pat Douthwaite →</a></nav>
</main>`;
const card = `<div id="tl-norman-index-fallback"><article id="tl-norman-index-entry"><a href="${route}" aria-label="Norman Lewis: A Retrospective"><img src="${base}installation-2.png" width="400" height="232" loading="lazy" alt=""></a><div><h2><a href="${route}">${title}</a></h2><p class="tl-oct-meta">CUNY Graduate Center, New York · 1976</p></div></article></div>`;
const template = await fs.readFile(new URL('plugin.php', root), 'utf8');
const replacements = {CONTENT: content, CARD: card, CSS: css, JS: js, SEARCH_TEXT: JSON.stringify(data.introduction)};
let plugin = template;
for (const [key, value] of Object.entries(replacements)) {
  assert.equal(plugin.split(`__${key}__`).length, 2, `Expected one ${key} placeholder`);
  plugin = plugin.replace(`__${key}__`, () => value);
}
await fs.mkdir(outputDirectory, {recursive: true});
await fs.writeFile(path.join(outputDirectory, 'zzzzzzz-tl-norman-lewis.php'), plugin);
await fs.writeFile(path.join(outputDirectory, 'content.html'), content);
await fs.writeFile(path.join(outputDirectory, 'card.html'), card);
