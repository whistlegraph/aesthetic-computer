#!/usr/bin/env node
// Scraper for thomaslawson.com — builds catalog.json from:
// 1. data/elementor-raw.json (from elementor-extract.py — primary source)
// 2. WP REST API media library (enrichment: credits, EXIF, wp IDs)
// 3. data/overrides.json (manual corrections)

import * as cheerio from "cheerio";
import { nanoid } from "nanoid";
import { writeFileSync, readFileSync, mkdirSync, existsSync } from "fs";
import { dirname, join } from "path";
import { fileURLToPath } from "url";

const __dirname = dirname(fileURLToPath(import.meta.url));
const BASE = "https://www.thomaslawson.com";
const API = BASE + "/wp-json/wp/v2";

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/**
 * Generate a short deterministic serial number for an artwork.
 * Based on a hash of title+year, produces "TL-XXXXX" (5 uppercase alphanumeric chars).
 * Consistent across re-scrapes for the same artwork.
 */
// Serial numbers are assigned sequentially as TL001, TL002, etc.
// Ordered chronologically (by year, then title) for stability.

function slugify(text) {
  const slug = text
    .toLowerCase()
    .replace(/['']/g, "")
    .replace(/[^a-z0-9]+/g, "-")
    .replace(/^-+|-+$/g, "");
  return slug || `untitled-${nanoid(4).toLowerCase()}`;
}

function now() {
  return new Date().toISOString();
}

function delay(ms) {
  return new Promise((r) => setTimeout(r, ms));
}

// ---------------------------------------------------------------------------
// Medium / surface parser
// ---------------------------------------------------------------------------

function parseMediumString(medium) {
  if (!medium) return { artMedium: null, artworkSurface: null };
  const lower = medium.toLowerCase();
  const onMatch = lower.match(/^(.+?)\s+on\s+(.+?)(?:,\s*with\s+.+)?$/);
  let artMedium = null;
  let artworkSurface = null;
  if (onMatch) {
    artMedium = onMatch[1].replace(/\s+and\s+/g, ", ").replace(/\s+&\s+/g, ", ").trim();
    artworkSurface = onMatch[2].trim();
  } else {
    artMedium = lower.replace(/\s+and\s+/g, ", ").replace(/\s+&\s+/g, ", ").trim();
  }
  return { artMedium, artworkSurface };
}

// ---------------------------------------------------------------------------
// Dimension parser
// ---------------------------------------------------------------------------

function parseDimensions(text) {
  if (!text) return null;
  const match = text.match(
    /(\d+(?:\.\d+)?)\s*[xX×]\s*(\d+(?:\.\d+)?)(?:\s*[xX×]\s*(\d+(?:\.\d+)?))?\s*(in\.?|cm\.?|ft\.?|m\.?)?/,
  );
  if (!match) return null;
  const width = parseFloat(match[1]);
  const height = parseFloat(match[2]);
  const depth = match[3] ? parseFloat(match[3]) : null;
  let unit = "in";
  if (match[4]) {
    const u = match[4].replace(".", "").toLowerCase();
    if (u === "cm") unit = "cm";
    else if (u === "ft") unit = "ft";
    else if (u === "m") unit = "m";
  }
  return { width, height, depth, unit, text };
}

function extractYear(text) {
  if (!text) return null;
  const m = text.match(/\b(19[5-9]\d|20[0-3]\d)\b/);
  return m ? parseInt(m[1], 10) : null;
}

function resolveImageUrl(src) {
  if (!src) return null;
  let url = src;
  url = url.replace(/-\d+x\d+(\.\w+)$/, "$1");
  if (url.startsWith("http://")) url = url.replace("http://", "https://");
  if (url.startsWith("/")) url = BASE + url;
  if (!url.startsWith("http")) return null;
  return url;
}

function isSiteChrome(src) {
  const lower = (src || "").toLowerCase();
  return (
    lower.includes("logo") ||
    lower.includes("footer") ||
    lower.includes("header") ||
    lower.includes("thomas-lawson-1.png") ||
    lower.includes("thomas-lawsonfooter") ||
    lower.includes("beyond-the-studio.jpg") ||
    lower.includes("4-beyond-the-studio")
  );
}

function detectSeries(title) {
  if (!title) return null;
  const t = title.toLowerCase();
  if (/^dis[a-z]/.test(t)) return "Dis-";
  if (t.includes("roman head")) return "Roman Head";
  if (t.includes("viennese painting")) return "Viennese Painting";
  if (t.includes("metropolis")) return "Metropolis";
  return null;
}

// ---------------------------------------------------------------------------
// Build artwork record
// ---------------------------------------------------------------------------

function makeArtwork({
  title,
  year,
  medium,
  dimensionsText,
  type = "painting",
  series = null,
  tags = [],
  imageUrl,
  wpMediaId = null,
  imageAlt = null,
  sourceUrl,
  section,
  period = null,
}) {
  const { artMedium, artworkSurface } = parseMediumString(medium);
  const dimensions = parseDimensions(dimensionsText);
  const slug = slugify(title + (year ? `-${year}` : ""));

  return {
    id: nanoid(8),
    serial: null, // assigned after dedup to ensure uniqueness
    title,
    slug,
    year: year || null,
    yearEnd: null,
    type,
    medium: medium || null,
    artMedium,
    artworkSurface,
    dimensions,
    artEdition: null,
    series: series || detectSeries(title),
    tags,
    images: [
      {
        url: imageUrl,
        caption: null,
        primary: true,
        isDetail: false,
        detailLabel: null,
        credit: null,
        wpMediaId: wpMediaId || null,
      },
    ],
    description: null,
    collection: null,
    provenance: [],
    exhibitions: [],
    writingIds: [],
    sourceUrl,
    section,
    period,
    createdAt: now(),
    updatedAt: now(),
  };
}

// ---------------------------------------------------------------------------
// Load Elementor data
// ---------------------------------------------------------------------------

function loadElementorData() {
  const path = join(__dirname, "data", "elementor-raw.json");
  if (!existsSync(path)) {
    console.error(
      "No data/elementor-raw.json found. Run `python3 elementor-extract.py` first.",
    );
    process.exit(1);
  }
  return JSON.parse(readFileSync(path, "utf-8"));
}

// ---------------------------------------------------------------------------
// Process Elementor artworks → catalog artworks
// ---------------------------------------------------------------------------

function processStudioArtworks(rawArtworks) {
  const artworks = [];

  for (const raw of rawArtworks) {
    const imageUrl = resolveImageUrl(raw.image_url);
    if (!imageUrl || isSiteChrome(imageUrl)) continue;

    let title = raw.title || raw.image_alt || "Untitled";
    // Don't use "-" as a title
    if (title === "-" || title === "–") title = "Untitled";

    const sourceUrl = `${BASE}/?p=${raw.page_id}`;

    artworks.push(
      makeArtwork({
        title,
        year: raw.year || null,
        medium: raw.medium || null,
        dimensionsText: raw.dimensions_text || null,
        imageUrl,
        wpMediaId: raw.image_id || null,
        imageAlt: raw.image_alt || null,
        sourceUrl,
        section: "in-the-studio",
        period: raw.period,
      }),
    );
  }

  return artworks;
}

function processBeyondArtworks(rawArtworks) {
  const artworks = [];

  for (const raw of rawArtworks) {
    const imageUrl = resolveImageUrl(raw.image_url);
    if (!imageUrl || isSiteChrome(imageUrl)) continue;

    let title = raw.title || raw.image_alt || "Untitled";
    if (title === "-" || title === "–") title = "Untitled";

    let type = "installation";
    const tag = raw.tag || "";
    if (tag.includes("theatre") || tag.includes("fashion")) type = "other";
    if (tag.includes("scottish")) type = "mixed-media";

    const sourceUrl = `${BASE}/?p=${raw.page_id}`;

    artworks.push(
      makeArtwork({
        title,
        year: raw.year || null,
        medium: raw.medium || null,
        dimensionsText: raw.dimensions_text || null,
        type,
        tags: tag ? [tag] : [],
        imageUrl,
        wpMediaId: raw.image_id || null,
        sourceUrl,
        section: "beyond-the-studio",
      }),
    );
  }

  return artworks;
}

// ---------------------------------------------------------------------------
// Process exhibitions
// ---------------------------------------------------------------------------

const EXHIBITION_META = [
  { id: 1205, title: "Pat Douthwaite", year: 1973 },
  { id: 1421, title: "REALLIFE Magazine Presents", year: 1981 },
  { id: 1233, title: "Critical Perspectives", year: 1982 },
  { id: 1252, title: "REALLIFE | Whitecolumns", year: 1982 },
  { id: 1280, title: "Livin' in the USA", year: 1984 },
  { id: 1292, title: "Nostalgia as Resistance", year: 1988 },
  { id: 1435, title: "Familie Beck", year: 1990 },
  { id: 1312, title: "The British Art Show", year: 1994 },
  { id: 1378, title: "Hot Coffee", year: 1997 },
  { id: 1458, title: "Shimmer", year: 1997 },
  { id: 1878, title: "Art School", year: 1997 },
  { id: 1343, title: "The Experimental Impulse", year: 2011 },
  { id: 1445, title: "Dissent", year: 2016 },
];

function processExhibitions(exhibitionImages) {
  const exhibitions = [];

  for (const meta of EXHIBITION_META) {
    const rawImages = exhibitionImages[String(meta.id)] || [];
    const images = [];

    for (const img of rawImages) {
      const url = resolveImageUrl(img.url);
      if (!url || isSiteChrome(url)) continue;
      images.push({
        url,
        caption: img.alt || null,
        primary: images.length === 0,
      });
    }

    exhibitions.push({
      id: nanoid(8),
      title: meta.title,
      slug: slugify(meta.title),
      year: meta.year,
      yearEnd: null,
      venue: null,
      city: null,
      role: "curator",
      description: null,
      images,
      artworkIds: [],
      sourceUrl: `${BASE}/?p=${meta.id}`,
      createdAt: now(),
      updatedAt: now(),
    });
  }

  return exhibitions;
}

// ---------------------------------------------------------------------------
// Process writings
// ---------------------------------------------------------------------------

const PUBLICATION_MAP = {
  afterall: "Afterall",
  artforum: "Artforum",
  "east-of-borneo": "East of Borneo",
  reallife: "REALLIFE",
};

function processWritings(rawWritings) {
  const writings = [];
  const seen = new Set();

  for (const raw of rawWritings) {
    const title = raw.title;
    if (!title || title.length < 3) continue;

    const key = title.toLowerCase();
    if (seen.has(key)) continue;
    seen.add(key);

    // Skip section headers
    if (
      key === "bookshelf" ||
      key === "home" ||
      key === "menu" ||
      key === "afterall" ||
      key === "artforum" ||
      key === "east of borneo" ||
      key === "reallife" ||
      key === "anthologies" ||
      key === "interviews"
    )
      continue;

    let defaultType = "essay";
    if (raw.category === "interviews") defaultType = "interview";
    if (raw.category === "anthologies") defaultType = "anthology";
    if (raw.category === "writings-about-tl") defaultType = "article";

    const defaultPublication = PUBLICATION_MAP[raw.category] || null;
    const author = raw.category === "writings-about-tl" ? "various" : null;

    // Parse pub_info from Elementor (e.g. "Artforum, October 1981")
    let publication = defaultPublication;
    let issue = null;
    if (raw.pub_info) {
      const parts = raw.pub_info.split(",").map((s) => s.trim());
      if (parts.length >= 2) {
        publication = parts[0];
        issue = parts.slice(1).join(", ");
      } else {
        publication = raw.pub_info;
      }
    }

    writings.push({
      id: nanoid(8),
      title,
      slug: slugify(title),
      year: raw.year || null,
      type: defaultType,
      author,
      publication,
      issue,
      url: raw.url || null,
      images: [],
      description: null,
      artworkIds: [],
      sourceUrl: null,
      category: raw.category,
      createdAt: now(),
      updatedAt: now(),
    });
  }

  return writings;
}

// ---------------------------------------------------------------------------
// Page metadata: fetch intros/framing text from WP REST API
// ---------------------------------------------------------------------------

const PAGE_META = {
  // In the Studio
  347:  { section: "in-the-studio",     label: "In the Studio: 1977–1979", period: "1977-1979" },
  401:  { section: "in-the-studio",     label: "In the Studio: 1980–1982", period: "1980-1982" },
  428:  { section: "in-the-studio",     label: "In the Studio: 1983–1987", period: "1983-1987" },
  457:  { section: "in-the-studio",     label: "In the Studio: 1987–1990", period: "1987-1990" },
  476:  { section: "in-the-studio",     label: "In the Studio: 1991–1993", period: "1991-1993" },
  486:  { section: "in-the-studio",     label: "In the Studio: 1994–1998", period: "1994-1998" },
  504:  { section: "in-the-studio",     label: "In the Studio: 1999–2006", period: "1999-2006" },
  524:  { section: "in-the-studio",     label: "In the Studio: 2006–2010", period: "2006-2010" },
  1998: { section: "in-the-studio",     label: "In the Studio: 2010–2015", period: "2010-2015" },
  544:  { section: "in-the-studio",     label: "In the Studio: 2015–2016", period: "2015-2016" },
  560:  { section: "in-the-studio",     label: "In the Studio: 2017–2020", period: "2017-2020" },
  // Beyond the Studio
  1552: { section: "beyond-the-studio", label: "Painted Installations",    tag: "painted installations" },
  1608: { section: "beyond-the-studio", label: "Early New York",           tag: "early new york" },
  1580: { section: "beyond-the-studio", label: "Dark Installations",      tag: "dark installations" },
  1622: { section: "beyond-the-studio", label: "Temporary Murals",        tag: "temporary murals" },
  1646: { section: "beyond-the-studio", label: "Glasgow Projects",        tag: "glasgow projects" },
  1660: { section: "beyond-the-studio", label: "Theatre, Dance & Fashion", tag: "theatre dance fashion" },
  1689: { section: "beyond-the-studio", label: "Los Angeles",             tag: "los angeles" },
  1711: { section: "beyond-the-studio", label: "The Scottish Project",    tag: "the scottish project" },
};

/**
 * Fetch a WP page and extract text blocks (not artwork captions).
 * Returns an array of { heading, text } paragraphs.
 */
async function fetchPageIntros(pageId) {
  const url = `${API}/pages/${pageId}?_fields=id,slug,title,content,link`;
  const res = await fetch(url);
  if (!res.ok) return { title: null, link: null, intros: [] };

  const data = await res.json();
  const html = data.content?.rendered || "";
  const $ = cheerio.load(html);
  const intros = [];

  // Text editor widgets contain the framing/intro paragraphs
  $(".elementor-widget-text-editor .elementor-widget-container").each((_, el) => {
    const $el = $(el);
    // Get the text, preserving paragraph breaks
    const paragraphs = [];
    $el.find("p").each((_, p) => {
      const text = $(p).text().trim();
      if (text && text.length > 20) paragraphs.push(text);
    });
    // If no <p> tags, get raw text
    if (paragraphs.length === 0) {
      const raw = $el.text().trim();
      if (raw && raw.length > 20) paragraphs.push(raw);
    }
    if (paragraphs.length === 0) return;

    // Check for a heading just before this text widget
    const $widget = $el.closest(".elementor-widget");
    const $prevHeading = $widget.prevAll(".elementor-widget-heading").first();
    const heading = $prevHeading.length
      ? $prevHeading.find(".elementor-heading-title").text().trim()
      : null;

    intros.push({
      heading: heading || null,
      text: paragraphs.join("\n\n"),
    });
  });

  return {
    title: data.title?.rendered || null,
    link: data.link || null,
    intros,
  };
}

/**
 * Fetch all page intros and build the sections array.
 */
async function fetchAllSections() {
  const sections = [];

  console.log("\nFetching page intros...");

  for (const [pageIdStr, meta] of Object.entries(PAGE_META)) {
    const pageId = parseInt(pageIdStr, 10);
    const { title, link, intros } = await fetchPageIntros(pageId);

    sections.push({
      pageId,
      label: meta.label,
      section: meta.section,
      period: meta.period || null,
      tag: meta.tag || null,
      wpTitle: title,
      sourceUrl: link || `${BASE}/?p=${pageId}`,
      intros,
    });

    const introCount = intros.length;
    if (introCount > 0) {
      console.log(`  → ${meta.label}: ${introCount} text blocks`);
    }

    await delay(250);
  }

  return sections;
}

// ---------------------------------------------------------------------------
// WP Media API enrichment
// ---------------------------------------------------------------------------

async function fetchMediaLibrary() {
  const mediaMap = new Map();
  let page = 1;
  let total = 0;

  console.log("\nFetching WP media library...");

  while (true) {
    const url = `${API}/media?per_page=100&page=${page}&_fields=id,title,caption,alt_text,source_url,media_details`;
    const res = await fetch(url);
    if (!res.ok) break;

    const items = await res.json();
    if (items.length === 0) break;

    for (const item of items) {
      const sourceUrl = item.source_url;
      if (!sourceUrl) continue;

      const captionHtml = item.caption?.rendered || "";
      const captionText = cheerio.load(captionHtml).text().trim();
      const credit =
        item.media_details?.image_meta?.credit ||
        item.media_details?.image_meta?.copyright ||
        null;

      const entry = {
        wpMediaId: item.id,
        caption: captionText || null,
        credit: credit || null,
      };

      mediaMap.set(sourceUrl, entry);
      // Also map http → https and full-res versions
      const https = sourceUrl.replace("http://", "https://");
      mediaMap.set(https, entry);
      const fullRes = sourceUrl.replace(/-\d+x\d+(\.\w+)$/, "$1");
      if (fullRes !== sourceUrl) {
        mediaMap.set(fullRes, entry);
        mediaMap.set(fullRes.replace("http://", "https://"), entry);
      }

      total++;
    }

    page++;
    await delay(200);
  }

  console.log(`  → ${total} media items indexed`);
  return mediaMap;
}

function enrichFromMedia(artworks, mediaMap) {
  let credits = 0;
  let captions = 0;
  let ids = 0;

  for (const artwork of artworks) {
    for (const img of artwork.images) {
      const media = mediaMap.get(img.url);
      if (!media) continue;

      if (!img.wpMediaId && media.wpMediaId) {
        img.wpMediaId = media.wpMediaId;
        ids++;
      }
      if (!img.credit && media.credit) {
        img.credit = media.credit;
        credits++;
      }
      if (!img.caption && media.caption) {
        img.caption = media.caption;
        captions++;
      }
    }
  }

  console.log(
    `  → Enriched: ${ids} media IDs, ${credits} credits, ${captions} captions`,
  );
}

// ---------------------------------------------------------------------------
// Merge detail images into parent artworks
// ---------------------------------------------------------------------------

/**
 * Find artworks whose titles indicate they're details of another artwork,
 * merge their images into the parent, and remove them from the list.
 *
 * Patterns detected:
 *   "Running Figures (detail from Formanesque)"
 *   "HE SHOT BEST BUDDY (DETAIL)"
 *   "Title (detail)"
 */
function mergeDetails(artworks) {
  // Build a title → artwork lookup (case-insensitive)
  const byTitle = new Map();
  for (const a of artworks) {
    byTitle.set(a.title.toLowerCase(), a);
  }

  const toRemove = new Set();
  let merged = 0;

  for (const a of artworks) {
    // Pattern: "X (detail from Y)" or "X (detail of Y)"
    const detailFromMatch = a.title.match(
      /^(.+?)\s*\(detail\s+(?:from|of)\s+(.+?)\)$/i,
    );
    if (detailFromMatch) {
      const detailLabel = detailFromMatch[1].trim();
      const parentTitle = detailFromMatch[2].trim().toLowerCase();
      const parent = byTitle.get(parentTitle);
      if (parent && parent !== a) {
        parent.images.push({
          ...a.images[0],
          primary: false,
          isDetail: true,
          detailLabel,
        });
        toRemove.add(a.id);
        merged++;
        continue;
      }
    }

    // Pattern: "TITLE (DETAIL)" — standalone detail, parent is "TITLE"
    const detailMatch = a.title.match(/^(.+?)\s*\(DETAIL\)$/i);
    if (detailMatch) {
      const parentTitle = detailMatch[1].trim().toLowerCase();
      const parent = byTitle.get(parentTitle);
      if (parent && parent !== a) {
        parent.images.push({
          ...a.images[0],
          primary: false,
          isDetail: true,
          detailLabel: "detail",
        });
        toRemove.add(a.id);
        merged++;
        continue;
      }
    }
  }

  const result = artworks.filter((a) => !toRemove.has(a.id));
  if (merged > 0) {
    console.log(
      `  → Merged ${merged} detail images into parent artworks`,
    );
  }
  return result;
}

// ---------------------------------------------------------------------------
// Manual overrides
// ---------------------------------------------------------------------------

function loadOverrides() {
  const path = join(__dirname, "data", "overrides.json");
  if (!existsSync(path)) return {};
  try {
    return JSON.parse(readFileSync(path, "utf-8"));
  } catch {
    console.warn("  ⚠ Could not parse overrides.json");
    return {};
  }
}

function applyOverrides(catalog, overrides) {
  let count = 0;
  for (const key of ["artworks", "exhibitions", "writings"]) {
    if (!overrides[key]) continue;
    for (const item of catalog[key]) {
      const ov = overrides[key][item.slug];
      if (!ov) continue;
      Object.assign(item, ov, { updatedAt: now() });
      count++;
    }
  }
  if (count) console.log(`  → Applied ${count} manual overrides`);
}

// ---------------------------------------------------------------------------
// Main
// ---------------------------------------------------------------------------

async function main() {
  console.log("Thomas Lawson Catalog Builder v4");
  console.log("================================\n");

  // Load Elementor data
  const raw = loadElementorData();
  console.log("Loaded elementor-raw.json");

  // Process artworks
  const studioArtworks = processStudioArtworks(raw.studio_artworks || []);
  const beyondArtworks = processBeyondArtworks(raw.beyond_artworks || []);
  const allArtworks = [...studioArtworks, ...beyondArtworks];

  console.log(`  Studio:  ${studioArtworks.length} artworks`);
  console.log(`  Beyond:  ${beyondArtworks.length} artworks`);

  // Deduplicate
  const deduped = new Map();
  for (const artwork of allArtworks) {
    const key = artwork.slug;
    if (deduped.has(key)) {
      const existing = deduped.get(key);
      for (const img of artwork.images) {
        if (!existing.images.some((e) => e.url === img.url)) {
          existing.images.push({ ...img, primary: false });
        }
      }
      if (!existing.year && artwork.year) existing.year = artwork.year;
      if (!existing.medium && artwork.medium) existing.medium = artwork.medium;
      if (!existing.dimensions && artwork.dimensions)
        existing.dimensions = artwork.dimensions;
      if (!existing.artMedium && artwork.artMedium)
        existing.artMedium = artwork.artMedium;
      if (!existing.artworkSurface && artwork.artworkSurface)
        existing.artworkSurface = artwork.artworkSurface;
    } else {
      deduped.set(key, artwork);
    }
  }
  let artworksList = [...deduped.values()];

  // Merge detail images into parent artworks
  console.log("\nMerging detail images...");
  artworksList = mergeDetails(artworksList);

  // Assign sequential serial numbers (TL001, TL002, ...)
  // Sort by year then title for stable ordering
  artworksList.sort((a, b) => {
    const ya = a.year || 9999;
    const yb = b.year || 9999;
    if (ya !== yb) return ya - yb;
    return (a.title || "").localeCompare(b.title || "");
  });
  for (let i = 0; i < artworksList.length; i++) {
    artworksList[i].serial = "TL" + (i + 1);
  }

  // Process exhibitions
  const exhibitions = processExhibitions(raw.exhibition_images || {});
  console.log(`  Exhibitions: ${exhibitions.length}`);

  // Process writings
  const writings = processWritings(raw.writings || []);
  console.log(`  Writings: ${writings.length}`);

  // Fetch page intros / sections
  const sections = await fetchAllSections();

  // Enrich from media library
  const mediaMap = await fetchMediaLibrary();
  console.log("\nEnriching artworks from media library...");
  enrichFromMedia(artworksList, mediaMap);

  // Build catalog
  const catalog = {
    artist: {
      name: "Thomas Lawson",
      born: 1951,
      birthplace: "Glasgow, Scotland",
      website: "https://www.thomaslawson.com",
      contact: "studio@thomaslawson.com",
    },
    sections,
    artworks: artworksList,
    exhibitions,
    writings,
    scrapedAt: now(),
    version: "5.0.0",
  };

  // Apply overrides
  const overrides = loadOverrides();
  applyOverrides(catalog, overrides);

  // Write output
  const outPath = join(__dirname, "data", "catalog.json");
  mkdirSync(dirname(outPath), { recursive: true });
  writeFileSync(outPath, JSON.stringify(catalog, null, 2));

  // Stats
  const total = catalog.artworks.length;
  const withYear = catalog.artworks.filter((a) => a.year).length;
  const withMedium = catalog.artworks.filter((a) => a.medium).length;
  const withDims = catalog.artworks.filter((a) => a.dimensions).length;
  const withSurface = catalog.artworks.filter((a) => a.artworkSurface).length;
  const withCredit = catalog.artworks.filter((a) =>
    a.images.some((i) => i.credit),
  ).length;

  console.log(`\n✓ Done!`);
  console.log(`  Artworks:        ${total}`);
  console.log(`    with year:     ${withYear} (${pct(withYear, total)})`);
  console.log(`    with medium:   ${withMedium} (${pct(withMedium, total)})`);
  console.log(`    with surface:  ${withSurface} (${pct(withSurface, total)})`);
  console.log(`    with dims:     ${withDims} (${pct(withDims, total)})`);
  console.log(`    with credit:   ${withCredit} (${pct(withCredit, total)})`);
  const withIntros = catalog.sections.filter((s) => s.intros.length > 0).length;
  console.log(`  Sections:        ${catalog.sections.length} (${withIntros} with intros)`);
  console.log(`  Exhibitions:     ${catalog.exhibitions.length}`);
  console.log(`  Writings:        ${catalog.writings.length}`);
  console.log(`  Written to:      ${outPath}`);
}

function pct(n, total) {
  return total ? Math.round((n / total) * 100) + "%" : "0%";
}

main().catch((err) => {
  console.error("Fatal error:", err);
  process.exit(1);
});
