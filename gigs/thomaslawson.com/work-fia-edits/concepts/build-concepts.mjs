#!/usr/bin/env node
import { mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const here = dirname(fileURLToPath(import.meta.url));
const tree = "https://www.thomaslawson.com/wp-content/uploads/2022/09/2010_Tree_HR.jpg";
const logo = "https://www.thomaslawson.com/wp-content/uploads/2022/07/THOMAS-LAWSON-1-301x99.png";

const bio = "Thomas Lawson (born 1951, Glasgow, Scotland) is an artist, writer, magazine editor and was the Dean of the School of Art at California Institute of the Arts from 1991–2022.";
const studioIntro = "The studio is a filter, a space in which experience is considered, and through a material process, transformed into art. I work in series, and each series is a response to a particular set of circumstances in my life and in the culture around me. I begin by collecting images; sometimes these are photographs I take, but more often they are already circulating in various print and digital media. Once a collection reaches a certain density I start to edit, crop, and combine. I decide on an appropriate scale, and then translate the imagery into paint on canvas, with all the attendant decisions about color and brushmark, representational style and painterly approach.";
const beyondIntro = "Studio work is by its very nature private, a personal exploration of ideas and themes. This offers tremendous freedom, but limits exposure to the relatively privileged world of galleries and museums. This section is about various projects and proposals to address a wider public, to bring the work to the streets. The challenge is to condense and simplify, without losing complexity.";
const aboutIntro = "I make art, and think about art, to better understand life. Over the years this work has taken many forms, but a consideration of painting has always remained central. But in chasing these questions about art and life I have also written essays and reviews, published and edited magazines, organized exhibitions, and devoted a lot of time and energy to helping younger artists figure out what they need to do.";

const pages = {
  home: {
    title: "Thomas Lawson",
    index: "00",
    hero: { src: tree, alt: "Tree, 2010, by Thomas Lawson" },
  },
  studio: {
    title: "In the Studio",
    index: "01",
    intro: studioIntro,
    hero: { src: tree, alt: "Tree, 2010, by Thomas Lawson" },
    gallery: [
      ["https://www.thomaslawson.com/wp-content/uploads/2022/06/006-Tlawson_Lone-Piper1978-1020x1024.jpg", "Lone Piper, 1978"],
      ["https://www.thomaslawson.com/wp-content/uploads/2022/05/007-Tlawson_Baby-Pink-on-Blue1978.jpg", "Baby Pink on Blue, 1978"],
      ["https://www.thomaslawson.com/wp-content/uploads/2022/06/009-Tlawson_Gold-Dog-Black-Orange1978-1024x717.jpg", "Gold Dog Black Orange, 1978"],
      ["https://www.thomaslawson.com/wp-content/uploads/2022/09/027-Tlawson_Shot-for-a-Bike1981.jpg", "Shot for a Bike, 1981"],
      ["https://www.thomaslawson.com/wp-content/uploads/2022/09/035-Tlawson_He-Shot-His-Best-Budy1982.jpg", "He Shot His Best Buddy, 1982"],
      ["https://www.thomaslawson.com/wp-content/uploads/2022/09/028-Tlawson_Burn-Burn-Burn1982-1024x1018.jpg", "Burn Burn Burn, 1982"],
    ],
  },
  beyond: {
    title: "Beyond the Studio",
    index: "02",
    intro: beyondIntro,
    hero: { src: "https://www.thomaslawson.com/wp-content/uploads/2023/09/4-beyond-the-studio.jpg", alt: "Public artwork documented in Beyond the Studio" },
    gallery: [
      ["https://www.thomaslawson.com/wp-content/uploads/2023/11/1987_Anthony-Reynolds-exhibition-catalogue-drawing_LR-1024x777.jpg", "Beyond the Studio project"],
      ["https://www.thomaslawson.com/wp-content/uploads/2023/08/civic_virtue_1.jpg", "Beyond the Studio project"],
      ["https://www.thomaslawson.com/wp-content/uploads/2023/08/1992_Tiefe-Nacht_LR.jpg", "Beyond the Studio project"],
      ["https://www.thomaslawson.com/wp-content/uploads/2023/08/portrait_16.jpg", "Beyond the Studio project"],
      ["https://www.thomaslawson.com/wp-content/uploads/2023/08/glasgow-green-install-3.jpg", "Beyond the Studio project"],
      ["https://www.thomaslawson.com/wp-content/uploads/2023/08/09-Til-You-Drop-Sept-2014.jpeg", "Beyond the Studio project"],
    ],
  },
  bookshelf: {
    title: "Bookshelf",
    index: "03",
    intro: studioIntro,
    hero: { src: "https://www.thomaslawson.com/wp-content/uploads/2022/06/5-bookshelf-scaled.jpg", alt: "Thomas Lawson's bookshelf" },
    gallery: [
      ["https://www.thomaslawson.com/wp-content/uploads/2023/02/2-Artforum-Marchh-1981-872x1024.png", "Artforum, March 1981"],
      ["https://www.thomaslawson.com/wp-content/uploads/2023/02/3-Artforum-April-1981-886x1024.png", "Artforum, April 1981"],
      ["https://www.thomaslawson.com/wp-content/uploads/2023/02/4-Artforum-May-1981-925x1024.png", "Artforum, May 1981"],
      ["https://www.thomaslawson.com/wp-content/uploads/2023/02/5-Artforum-September-1981-956x1024.png", "Artforum, September 1981"],
      ["https://www.thomaslawson.com/wp-content/uploads/2023/02/6-Artforum-October-1981-960x1024.png", "Artforum, October 1981"],
      ["https://www.thomaslawson.com/wp-content/uploads/2023/02/7-Artforum-December-1981-908x1024.png", "Artforum, December 1981"],
      ["https://www.thomaslawson.com/wp-content/uploads/2023/02/8-Artforum-January-1982-922x1024.png", "Artforum, January 1982"],
      ["https://www.thomaslawson.com/wp-content/uploads/2023/02/9-Artforum-May-1982-855x1024.png", "Artforum, May 1982"],
      ["https://www.thomaslawson.com/wp-content/uploads/2023/02/10-Artforum-Summer-1982-961x1024.png", "Artforum, Summer 1982"],
      ["https://www.thomaslawson.com/wp-content/uploads/2023/02/11-Artforum-October-1982-881x1024.png", "Artforum, October 1982"],
    ],
  },
  about: {
    title: "About",
    index: "04",
    intro: aboutIntro,
    hero: { src: "https://www.thomaslawson.com/wp-content/uploads/2022/05/Group-24.png", alt: "Thomas Lawson in his studio" },
    gallery: [
      ["https://www.thomaslawson.com/wp-content/uploads/2022/06/013-Tlawson_Gold-Dog1978-copy-835x1024.jpg", "Thomas Lawson artwork"],
      ["https://www.thomaslawson.com/wp-content/uploads/2022/06/REALLIFE-1-copy-683x1024.jpg", "REALLIFE Magazine"],
      ["https://www.thomaslawson.com/wp-content/uploads/2022/06/001-Tlawson_Forman-esque-25-drawings1977-copy-1.png", "Thomas Lawson drawing"],
      ["https://www.thomaslawson.com/wp-content/uploads/2022/05/014-Tlawson_Red-Shoe1979-1024x968.jpg", "Thomas Lawson artwork"],
    ],
  },
};

const pageOrder = ["home", "studio", "beyond", "bookshelf", "about"];
const pageLabels = { home: "Home", studio: "In the Studio", beyond: "Beyond", bookshelf: "Bookshelf", about: "About" };
const routeDescriptions = {
  "News": "plus selections from the archive",
  "In the Studio": "artworks",
  "Beyond the Studio": "exhibitions and public artworks",
  "Art in a Broader Context": "curatorial projects and pedagogy",
  "Bookshelf": "writings and publications",
};
const homeRoutes = [
  ["News", "https://www.thomaslawson.com/notes/"],
  ["In the Studio", "studio.html"],
  ["Beyond the Studio", "beyond.html"],
  ["Art in a Broader Context", "https://www.thomaslawson.com/art-in-a-broader-context/"],
  ["Bookshelf", "bookshelf.html"],
];

const tiers = [
  { id: "tier-1", report: "tier-1-refine.json", label: "Refine", scope: "CSS-only", strength: "Safest and quickest. It makes the existing site read as one authored archive.", tradeoff: "The structure remains familiar; it cannot solve every Elementor hierarchy problem." },
  { id: "tier-2", report: "tier-2-recompose.json", label: "Recompose", scope: "CSS + reversible DOM order", strength: "Best balance of distinction and feasibility. The black index makes every route visible.", tradeoff: "Requires a global navigation and small client-side restructuring." },
  { id: "tier-3", report: "tier-3-reframe.json", label: "Reframe", scope: "Template-level exhibition system", strength: "Most distinctive. One claim and one dominant artifact survive even at thumbnail scale.", tradeoff: "Largest build. Page templates and responsive evidence stages must be rebuilt." },
];

function escapeHtml(value = "") {
  return String(value).replace(/[&<>"']/g, (char) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;", "'": "&#039;" }[char]));
}

function conceptRoute(tier, pageKey) {
  const tierLinks = tiers.map((item) => `<a class="${item.id === tier.id ? "active" : ""}" href="../${item.id}/${pageKey}.html">${item.label}</a>`).join("");
  const pageLinks = pageOrder.map((key) => `<a class="${key === pageKey ? "active" : ""}" href="${key}.html">${pageLabels[key]}</a>`).join("");
  return `<div class="concept-route"><nav><span class="label">Tier</span>${tierLinks}</nav><nav><span class="label">Page</span>${pageLinks}</nav></div>`;
}

function siteHeader(pageKey) {
  const links = pageOrder.map((key, index) => `<a class="${key === pageKey ? "active" : ""}" data-index="0${index}" href="${key}.html">${pageLabels[key]}</a>`).join("");
  return `<header class="site-header"><a class="site-wordmark" href="home.html" aria-label="Thomas Lawson home"><img src="${logo}" alt="Thomas Lawson"></a><nav class="site-nav" aria-label="Primary">${links}<a data-index="05" href="https://www.thomaslawson.com/contact/">Contact</a></nav></header>`;
}

function homeMain(page) {
  const routes = homeRoutes.map(([label, href]) => `<li><a href="${href}"><strong>${escapeHtml(label)}</strong><span>${escapeHtml(routeDescriptions[label])}</span></a></li>`).join("");
  return `<main class="page-shell"><figure class="hero-media"><img src="${page.hero.src}" alt="${escapeHtml(page.hero.alt)}"></figure><section class="home-index"><ul class="route-list">${routes}</ul></section></main>`;
}

function interiorMain(page) {
  const gallery = (page.gallery || []).map(([src, alt]) => `<figure><img src="${src}" alt="${escapeHtml(alt)}" loading="lazy"><figcaption>${escapeHtml(alt)}</figcaption></figure>`).join("");
  return `<main class="page-shell"><section class="claim-row"><div class="claim-index">${page.index}</div><h1 class="page-title">${escapeHtml(page.title)}</h1></section><figure class="hero-media"><img src="${page.hero.src}" alt="${escapeHtml(page.hero.alt)}"></figure><section class="intro"><p>${escapeHtml(page.intro)}</p></section>${gallery ? `<section class="gallery">${gallery}</section>` : ""}</main>`;
}

function footer() {
  return `<footer class="site-footer"><p>${escapeHtml(bio)}</p><p>© 2026 Thomas Lawson.<br>studio@thomaslawson.com</p></footer>`;
}

function pageHtml(tier, pageKey) {
  const page = pages[pageKey];
  return `<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><meta name="robots" content="noindex"><title>${escapeHtml(page.title)} · ${tier.label} concept</title><link rel="stylesheet" href="../shared.css"><link rel="stylesheet" href="../${tier.id}.css"></head><body class="${tier.id} page-${pageKey}">${conceptRoute(tier, pageKey)}${siteHeader(pageKey)}${pageKey === "home" ? homeMain(page) : interiorMain(page)}${footer()}</body></html>`;
}

const reports = new Map(tiers.map((tier) => {
  const report = JSON.parse(readFileSync(join(here, "inference", tier.report), "utf8"));
  return [tier.id, report];
}));

for (const tier of tiers) {
  const dir = join(here, tier.id);
  mkdirSync(dir, { recursive: true });
  for (const pageKey of pageOrder) {
    writeFileSync(join(dir, `${pageKey}.html`), pageHtml(tier, pageKey));
  }
}

const directionHtml = tiers.map((tier, tierIndex) => {
  const report = reports.get(tier.id);
  const direction = report.direction;
  const thumbs = ["home", "studio", "bookshelf"].map((pageKey, index) => `<div class="thumb ${index ? "small" : ""}"><iframe src="${tier.id}/${pageKey}.html" title="${escapeHtml(direction.directionName)} · ${pageLabels[pageKey]}" loading="lazy" scrolling="no"></iframe><a href="${tier.id}/${pageKey}.html" aria-label="Open ${escapeHtml(direction.directionName)} ${pageLabels[pageKey]}"></a></div>`).join("");
  const links = pageOrder.map((key) => `<a href="${tier.id}/${key}.html">${pageLabels[key]}</a>`).join("");
  return `<section class="direction" id="${tier.id}"><div class="thumbs">${thumbs}</div><div class="direction-copy"><div class="tier">Tier ${tierIndex + 1} · ${tier.scope} · ${escapeHtml(report.returnedModel)}</div><h2>${escapeHtml(direction.directionName)}</h2><p class="claim">${escapeHtml(direction.claim)}</p><dl><dt>Rule</dt><dd>${escapeHtml(direction.contract.governingRule)}</dd><dt>Never</dt><dd>${escapeHtml(direction.contract.mustNever)}</dd><dt>Strength</dt><dd>${escapeHtml(tier.strength)}</dd><dt>Trade-off</dt><dd>${escapeHtml(tier.tradeoff)}</dd></dl><div class="page-links">${links}</div></div></section>`;
}).join("");

const indexHtml = `<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><meta name="robots" content="noindex"><title>Thomas Lawson · Three design tiers</title><link rel="stylesheet" href="shared.css"></head><body class="concept-index"><header class="index-head"><h1>Thomas Lawson<br>three design tiers</h1><p>The same five pages, words, and images. Only the visual grammar and implementation ceiling change. These concepts are local and do not change the live site.</p><nav class="index-map" aria-label="Design routes"><span class="root">Shared site</span>${tiers.map((tier, index) => `<a href="#${tier.id}">0${index + 1} ${tier.label}</a>`).join("")}</nav></header><main>${directionHtml}</main><footer class="index-foot">Current-site screenshots were reviewed independently by OpenAI GPT-5.6 Luna, Terra, and Sol, then converted into distinct buildable routes and rendered for comparison.</footer></body></html>`;
writeFileSync(join(here, "index.html"), indexHtml);

writeFileSync(join(here, "inference", "index.json"), `${JSON.stringify({
  schema: "thomas-lawson-visual-directions/v1",
  createdAt: new Date().toISOString(),
  reports: tiers.map((tier) => {
    const report = reports.get(tier.id);
    return {
      tier: report.tier,
      model: report.returnedModel,
      file: tier.report,
      directionName: report.direction.directionName,
      claim: report.direction.claim,
    };
  }),
}, null, 2)}\n`);

console.log(`built ${tiers.length * pageOrder.length} concept pages + index`);
