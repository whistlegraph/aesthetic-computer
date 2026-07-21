#!/usr/bin/env node
// Build the small, rights-aware museum-object snapshot used by /artnom.
//
// Art Institute of Chicago is the first object source because its API exposes
// an explicit style taxonomy and IIIF images. Getty AAT URIs are the canonical
// round identities, matching AC's Linked Art records in backend/linked-art.mjs.
// Re-run from the repository root:
//   node system/scripts/import-artnom.mjs

// The generated manifest stores metadata + remote IIIF thumbnail URLs only;
// it does not copy museum image binaries into the repository.

// 2026.07.21

import { mkdir, writeFile } from "node:fs/promises";
import { dirname } from "node:path";
import { fileURLToPath } from "node:url";

const API = "https://api.artic.edu/api/v1";
const IIIF = "https://www.artic.edu/iiif/2";
const OUTPUT = fileURLToPath(
  new URL("../public/aesthetic.computer/assets/artnom/manifest.json", import.meta.url),
);
const USER_AGENT = "Aesthetic Computer artnom (hello@aesthetic.computer)";
const ARTWORKS_PER_CATEGORY = 10;
const MAX_PER_ARTIST = 3;

// AIC has reconciled two of these terms directly to AAT. The remaining AAT
// ids were verified through Getty/Wikidata authority links on 2026-07-21.
const CATEGORIES = [
  { slug: "cubism", label: "Cubism", aatId: "300021495", aicTerm: "TM-7539" },
  { slug: "impressionism", label: "Impressionism", aatId: "300021503", aicTerm: "TM-7543" },
  { slug: "post-impressionism", label: "Post-Impressionism", aatId: "300021508", aicTerm: "TM-7547" },
  { slug: "surrealism", label: "Surrealism", aatId: "300021512", aicTerm: "TM-7551" },
  { slug: "pop-art", label: "Pop Art", aatId: "300022205", aicTerm: "TM-7221" },
];

const FIELDS = [
  "id",
  "title",
  "artist_title",
  "date_display",
  "image_id",
  "is_public_domain",
  "style_title",
  "style_ids",
  "classification_title",
  "thumbnail",
].join(",");

async function json(url, options = {}) {
  const response = await fetch(url, {
    ...options,
    headers: { "AIC-User-Agent": USER_AGENT, ...options.headers },
  });
  if (!response.ok) throw new Error(`${response.status} ${response.statusText}: ${url}`);
  return response.json();
}

async function verifyTerm(category) {
  const result = await json(
    `${API}/category-terms/${encodeURIComponent(category.aicTerm)}` +
      "?fields=id,title,subtype,aat_id",
  );
  const term = result.data;
  if (term?.title !== category.label || term?.subtype !== "style") {
    throw new Error(`AIC term ${category.aicTerm} no longer resolves to ${category.label}`);
  }
  if (term.aat_id && String(term.aat_id) !== category.aatId) {
    throw new Error(
      `${category.label}: AIC now maps to AAT ${term.aat_id}, expected ${category.aatId}`,
    );
  }
}

async function fetchCategory(category) {
  await verifyTerm(category);
  const body = {
    size: 100,
    query: {
      bool: {
        must: [
          { term: { "style_title.keyword": category.label } },
          { exists: { field: "image_id" } },
        ],
      },
    },
  };
  const rows = [];
  for (let offset = 0; offset < 400; offset += 100) {
    const result = await json(
      `${API}/artworks/search?from=${offset}&size=100&fields=${FIELDS}`,
      {
        method: "POST",
        headers: { "content-type": "application/json" },
        body: JSON.stringify(body),
      },
    );
    rows.push(...(result.data || []));
    if (rows.length >= (result.pagination?.total || 0)) break;
  }

  // Keep the API's relevance order, but prevent one artist from swallowing a
  // whole round (particularly Picasso in Cubism and Warhol in Pop Art).
  const artistCounts = new Map();
  const selected = [];
  for (const work of rows) {
    if (!work.image_id || !work.title || !work.artist_title) continue;
    const artist = work.artist_title.trim();
    const count = artistCounts.get(artist) || 0;
    if (count >= MAX_PER_ARTIST) continue;
    artistCounts.set(artist, count + 1);
    selected.push({
      id: `artic:${work.id}`,
      source: "artic",
      sourceId: String(work.id),
      title: work.title,
      artist,
      date: work.date_display || "date unknown",
      classification: work.classification_title || "artwork",
      style: work.style_title,
      sourceTerms: (work.style_ids || []).map((id) => `artic:${id}`),
      aat: [`http://vocab.getty.edu/aat/${category.aatId}`],
      publicDomain: work.is_public_domain === true,
      rights: work.is_public_domain === true
        ? "https://creativecommons.org/publicdomain/mark/1.0/"
        : "http://rightsstatements.org/vocab/InC/1.0/",
      image: `${IIIF}/${work.image_id}/full/200,/0/default.jpg`,
      alt: work.thumbnail?.alt_text || `${work.title} by ${artist}`,
      api: `${API}/artworks/${work.id}`,
      url: `https://www.artic.edu/artworks/${work.id}`,
    });
    if (selected.length >= ARTWORKS_PER_CATEGORY) break;
  }
  if (selected.length < ARTWORKS_PER_CATEGORY) {
    throw new Error(`${category.label}: only found ${selected.length} usable artworks`);
  }
  return {
    id: `aat:${category.aatId}`,
    slug: category.slug,
    label: category.label,
    aatId: category.aatId,
    aat: `http://vocab.getty.edu/aat/${category.aatId}`,
    speech: category.label.toLowerCase(),
    sourceTerms: [{ source: "artic", id: category.aicTerm, label: category.label }],
    artworks: selected,
  };
}

const categories = [];
for (const category of CATEGORIES) categories.push(await fetchCategory(category));

const manifest = {
  schema: "https://aesthetic.computer/schemas/artnom-manifest-v1.json",
  generatedAt: new Date().toISOString(),
  license: "Museum metadata is CC0; image rights are recorded per object and images remain remote.",
  sources: [
    {
      id: "artic",
      label: "Art Institute of Chicago",
      api: API,
      url: "https://www.artic.edu/open-access/public-api",
      imageService: IIIF,
    },
    {
      id: "getty-aat",
      label: "Getty Art & Architecture Thesaurus",
      api: "https://vocab.getty.edu/sparql",
      url: "https://www.getty.edu/research/tools/vocabularies/aat/",
    },
  ],
  categories,
};

await mkdir(dirname(OUTPUT), { recursive: true });
await writeFile(OUTPUT, `${JSON.stringify(manifest, null, 2)}\n`);
console.log(
  `Wrote ${OUTPUT}: ${categories.length} categories, ` +
    `${categories.reduce((sum, category) => sum + category.artworks.length, 0)} artworks`,
);
