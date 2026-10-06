#!/usr/bin/env node
import { readFile, writeFile } from "node:fs/promises";
import { fileURLToPath } from "node:url";
import { resolve, dirname } from "node:path";
import { validateCatalog, filterItems, renderCard, makeCalendar, makeRSS, DISCIPLINES, KINDS } from "../../system/public/papers.aesthetic.computer/deadlines/core.mjs";

const root = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
const dir = resolve(root, "system/public/papers.aesthetic.computer/deadlines");
const catalog = validateCatalog(JSON.parse(await readFile(resolve(dir, "opportunities.json"), "utf8")));
const now = new Date(catalog.updated + "T12:00:00Z");
const items = filterItems(catalog.opportunities, {}, now);
const template = await readFile(resolve(root, "utilities/deadlines/template.html"), "utf8");
const files = {
  "index.html": template.replace("<!-- CARDS -->", items.map((x) => renderCard(x, now)).join("\n"))
    .replaceAll("{{UPDATED}}", catalog.updated)
    .replaceAll("{{COUNT}}", String(items.length))
    .replace("<!-- DISCIPLINES -->", DISCIPLINES.map((x) => `<option>${x}</option>`).join(""))
    .replace("<!-- KINDS -->", KINDS.map((x) => `<option>${x}</option>`).join("")),
  "feed.xml": makeRSS(catalog),
  "calendar.ics": makeCalendar(catalog.opportunities, catalog.updated),
};
let stale = false;
for (const [name, content] of Object.entries(files)) {
  if (process.argv.includes("--check")) {
    const existing = await readFile(resolve(dir, name), "utf8").catch(() => "");
    if (existing !== content) { console.error(`Stale generated file: ${name}`); stale = true; }
  } else await writeFile(resolve(dir, name), content);
}
if (stale) process.exitCode = 1;
else console.log(`${process.argv.includes("--check") ? "Verified" : "Built"} ${catalog.opportunities.length} opportunities, RSS and calendar.`);
