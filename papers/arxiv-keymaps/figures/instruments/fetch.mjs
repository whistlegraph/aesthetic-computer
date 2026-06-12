#!/usr/bin/env node
// figures/instruments/fetch.mjs — pull the four asymmetric-instrument
// artifact photos for fig:asymmetric-instruments from the Met's open
// access API (all CC0). Reproducible: re-run any time; writes the
// images + credits.json beside itself.
//
//   node papers/arxiv-keymaps/figures/instruments/fetch.mjs
//
// Picks (chosen 2026-06-12, with @jeffrey):
//   guitar   503283  Manuel Ramírez, Madrid, 1912 — plain 6-string EADGBE
//   violin   503057  Nicolò Amati, Cremona, 1669 — strict P5 strings
//   trumpet  503961  Courtois & Mille, Paris, 1881–85 — THREE piston
//                    valves (the 1845 candidate 503553 had four!)
//   ocarina  503242  Tairona vessel flute, 1300–1500 — the Mesoamerican
//                    root of the lineage the text runs to Donati 1853

import { writeFileSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const PICKS = { guitar: 503283, violin: 503057, trumpet: 503961, ocarina: 503242 };

const credits = {};
for (const [name, id] of Object.entries(PICKS)) {
  const meta = await (await fetch(`https://collectionapi.metmuseum.org/public/collection/v1/objects/${id}`)).json();
  if (!meta.isPublicDomain) throw new Error(`${name} (${id}) is not CC0!`);
  const buf = Buffer.from(await (await fetch(meta.primaryImage)).arrayBuffer());
  const out = resolve(HERE, `${name}.jpg`);
  writeFileSync(out, buf);
  credits[name] = {
    objectID: id, title: meta.title, maker: meta.artistDisplayName || meta.culture,
    date: meta.objectDate, place: meta.city || meta.country || "",
    accession: meta.accessionNumber, url: meta.objectURL, license: "CC0 (Met Open Access)",
  };
  console.log(`✓ ${name} · ${credits[name].maker} · ${meta.objectDate} · ${(buf.length / 1e6).toFixed(1)} MB`);
}
writeFileSync(resolve(HERE, "credits.json"), JSON.stringify(credits, null, 2));
console.log("✓ credits.json");
