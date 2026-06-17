// fetch-logos.mjs — build the corporate-graphics reference library.
// For each curated entry: search Wikimedia Commons → pick the best image file
// → fetch original (svg/png) → rasterize svg to a 600px png for contact sheets.
// Writes per-category manifest.md + a fetch log. Idempotent-ish (skips existing).
//
//   node fetch-logos.mjs [category]   (omit to run all four)

import { writeFileSync, existsSync, mkdirSync } from "node:fs";
import { execFileSync } from "node:child_process";
import { join } from "node:path";

const HERE = new URL(".", import.meta.url).pathname;
const UA = "ac-research/1.0 (mail@aesthetic.computer) logo-reference-library";
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

// metadata is OURS (captions); the fetch only resolves the image.
// columns: slug, query, name, by, year, motif, register, note
const LIB = {
  "banks-finance": [
    ["chase-octagon", "Chase bank logo", "Chase", "Chermayeff & Geismar", "1960", "abstract octagon / vault aperture", "mid-century geometric", "first major US abstract bank mark — the negative center reads as a vault/aperture"],
    ["deutsche-bank-slash", "Deutsche Bank logo", "Deutsche Bank", "Anton Stankowski", "1974", "slash inside a square", "swiss constructivist", "growth (diagonal) held in a stable frame (square)"],
    ["hsbc-hexagon", "HSBC logo", "HSBC", "Henry Steiner", "1983", "hexagon from St Andrew's cross", "heraldic→geometric", "derived from a 19th-c house flag; hexagon = containment"],
    ["barclays-eagle", "Barclays logo", "Barclays", "—", "—", "spread eagle", "heraldic, flattened", "a centuries-old spread eagle modernized to flat"],
    ["lloyds-black-horse", "Lloyds Bank logo", "Lloyds Bank", "—", "1884 heritage", "black horse", "inherited shop-sign heraldry", "a literal goldsmith's-shop sign become a brand"],
    ["natwest-cubes", "NatWest logo", "NatWest", "—", "1968", "three cubes / arrows", "swiss geometric", "money moving three directions"],
    ["citi-arc", "Citi logo", "Citi", "Pentagram (Paula Scher)", "1998", "red arc over wordmark", "wordmark + gesture", "the arc as umbrella/protection (the napkin-sketch mark)"],
    ["santander-flame", "Santander logo", "Santander", "—", "—", "flame / spark", "contemporary flat", "warmth + energy in red"],
    ["mastercard-circles", "Mastercard logo", "Mastercard", "(Pentagram 2016 refresh)", "1968", "two overlapping circles", "gestalt overlap", "intersection = exchange"],
    ["amex-bluebox", "American Express logo", "American Express", "—", "1970s", "the Blue Box", "corporate trust blue", "the box as security / containment"],
    ["ubs-keys", "UBS logo", "UBS", "—", "—", "three keys", "heraldic", "keys = confidence, security, discretion"],
    ["ing-lion", "ING Group logo", "ING", "—", "—", "lion", "dutch heraldic, flat orange", "heritage animal flattened to friendly"],
    ["bbva-mark", "BBVA logo", "BBVA", "Landor", "2019", "geometric wordmark", "contemporary sans", "softened modern banking"],
    ["wells-fargo-stagecoach", "Wells Fargo logo", "Wells Fargo", "—", "—", "stagecoach", "americana heritage", "trust signalled through history"],
    ["bank-of-america", "Bank of America logo", "Bank of America", "Lippincott", "1998", "abstract flag stripes", "patriotic geometric", "flag fragment = nation-scale trust"],
    ["monte-paschi-siena", "Monte dei Paschi di Siena logo", "Banca Monte dei Paschi di Siena", "—", "—", "heraldic shield", "italian heraldry", "the world's oldest surviving bank's arms"],
  ],
  "modernist-canon": [
    ["rand-ibm", "IBM logo", "IBM", "Paul Rand", "1972", "8-bar striped letters", "rationalist", "stripes = scan-lines + unity; speed made stable"],
    ["rand-ups", "UPS logo Paul Rand", "UPS (old)", "Paul Rand", "1961", "shield + package + bow", "heraldic-modern", "a parcel made into a coat of arms"],
    ["rand-abc", "ABC logo", "ABC", "Paul Rand", "1962", "lowercase bauhaus circles", "bauhaus geometric", "minimal letterforms = quiet confidence"],
    ["rand-westinghouse", "Westinghouse logo", "Westinghouse", "Paul Rand", "1960", "circuit 'W'", "atomic-age", "a circuit board read as a monogram"],
    ["rand-next", "NeXT logo Paul Rand", "NeXT", "Paul Rand", "1986", "tilted cube", "playful-rational", "the famous $100k cube"],
    ["cg-mobil", "Mobil logo", "Mobil", "Chermayeff & Geismar", "1964", "red O wordmark", "minimalist", "the single red O does all the work"],
    ["cg-nbc-peacock", "NBC peacock logo", "NBC", "Chermayeff & Geismar", "1986", "six-feather peacock", "broadcast color", "color itself as the message"],
    ["cg-pbs", "PBS logo", "PBS", "Chermayeff & Geismar", "1984", "P-head profiles", "figure-ground", "many faces = the public"],
    ["cg-natgeo", "National Geographic logo", "National Geographic", "Chermayeff & Geismar", "—", "yellow rectangle", "portal", "the yellow frame as a window onto the world"],
    ["bass-bell", "Bell System logo Saul Bass", "Bell System", "Saul Bass", "1969", "bell in a circle", "simplification", "a century of marks reduced to one bell"],
    ["bass-att-globe", "AT&T globe logo", "AT&T", "Saul Bass", "1983", "striped globe", "line-network", "lines = the global network, wrapping a sphere"],
    ["bass-united", "United Airlines logo Saul Bass", "United Airlines", "Saul Bass", "1974", "tulip 'U'", "gestural geometric", "the U that flew for 36 years"],
    ["bass-minolta", "Minolta logo", "Minolta", "Saul Bass", "1978", "wordmark + curves", "japanese-modern", "soft motion in a tech wordmark"],
    ["vignelli-american-airlines", "American Airlines AA logo 1967", "American Airlines", "Massimo Vignelli", "1967", "AA + eagle", "helvetica + flag", "Helvetica discipline, lasted to 2013"],
    ["vignelli-knoll", "Knoll logo", "Knoll", "Massimo Vignelli", "—", "wordmark", "swiss typographic", "furniture as pure typography"],
    ["lippincott-walmart-spark", "Walmart logo", "Walmart", "Lippincott", "2008", "spark / sunburst", "friendly flat", "an asterisk of warmth"],
    ["fedex-arrow", "FedEx logo", "FedEx", "Lindon Leader (Landor)", "1994", "hidden arrow", "negative space", "the canonical negative-space mark"],
    ["nike-swoosh", "Nike logo", "Nike", "Carolyn Davidson", "1971", "swoosh", "single gesture", "motion as a one-stroke mark"],
    ["apple-logo", "Apple Inc logo", "Apple", "Rob Janoff", "1977", "bitten apple", "friendly silhouette", "a bite for scale + recognizability"],
    ["paul-rand-enron", "Enron logo Paul Rand", "Enron", "Paul Rand", "1996", "tilted color bars 'E'", "rand late-style", "Rand's last major mark"],
  ],
  "seals-monograms": [
    ["us-treasury-seal", "Seal of the United States Department of the Treasury", "US Treasury", "—", "1789", "scales + chevron of stars", "engraved federal seal", "balance + key = fiscal authority"],
    ["great-seal-us", "Great Seal of the United States", "Great Seal of the United States", "—", "1782", "eagle + shield", "engraved heraldry", "the ur-seal of American officialdom"],
    ["federal-reserve-seal", "Seal of the Federal Reserve System", "Federal Reserve", "—", "—", "eagle seal", "engraved", "monetary authority as engraved roundel"],
    ["ul-mark", "UL certification mark", "Underwriters Laboratories", "—", "1894", "UL in a circle", "certification roundel", "the literal safety-trust stamp on a billion products"],
    ["woolmark", "Woolmark", "Woolmark", "Francesco Saroglia", "1964", "skein swirl", "one of the great marks", "purity certification as continuous-line sculpture"],
    ["ce-mark", "CE marking", "CE marking", "—", "1985", "geometric C+E", "conformity geometry", "constructed from circles — pure compliance form"],
    ["bsi-kitemark", "BSI kitemark", "BSI Kitemark", "—", "1903", "kite shape", "british standard", "a monogram folded into a kite"],
    ["good-housekeeping-seal", "Good Housekeeping Seal", "Good Housekeeping", "—", "1909", "circular guarantee", "editorial seal", "a magazine's promise made into a stamp"],
    ["fairtrade-mark", "Fairtrade certification mark", "Fairtrade", "—", "2002", "yin-yang figure", "certification roundel", "a person + sky in a blue/green disc"],
    ["usda-organic", "USDA organic seal", "USDA Organic", "—", "2002", "circular badge", "government certification", "trust via federal roundel"],
    ["vw-monogram", "Volkswagen logo", "Volkswagen", "—", "1939", "V over W in a circle", "geometric monogram", "two letters locked in a wheel"],
    ["unilever-u", "Unilever logo", "Unilever", "Wolff Olins (Miles Newlyn)", "2004", "U built of 25 icons", "monogram-as-mosaic", "authority spelled from many small marks"],
    ["chanel-cc", "Chanel logo", "Chanel", "Coco Chanel", "1925", "interlocking C's", "luxury monogram", "mirror symmetry = the locked seal of luxury"],
    ["penguin-roundel", "Penguin Books logo", "Penguin Books", "Edward Young", "1935", "penguin in an oval", "publisher's device", "a colophon — the book-trade's seal"],
    ["mit-seal", "MIT seal", "MIT", "—", "1864", "artisan + scholar", "academic engraving", "the university seal as authority template"],
    ["penny-black", "Penny Black", "Penny Black (stamp)", "—", "1840", "engraved profile", "intaglio portrait", "the first postage stamp — a state-issued mark of paid trust"],
  ],
  "surveillance-consent": [
    ["norton-by-symantec", "Norton Symantec logo", "Norton", "—", "—", "yellow check in circle", "security yellow", "the check inside a ring = 'verified, protected'"],
    ["tor-onion", "Tor project logo", "Tor", "—", "—", "onion", "activist flat", "layers = anonymity"],
    ["signal-messenger", "Signal messenger logo", "Signal", "—", "—", "speech bubble", "friendly privacy", "privacy made warm, not cold"],
    ["eff", "Electronic Frontier Foundation logo", "EFF", "—", "—", "wordmark / fist", "digital-rights", "advocacy, not product"],
    ["lets-encrypt", "Let's Encrypt logo", "Let's Encrypt", "—", "2014", "open padlock", "open-security", "the lock made friendly + free"],
    ["mozilla", "Mozilla logo", "Mozilla", "johnson banks", "2017", "moz://a wordmark", "wordmark-as-URL", "the protocol slashes built into the name"],
    ["firefox", "Firefox logo", "Firefox", "—", "—", "fox around a globe", "warm tech", "guardian animal wrapping the web"],
    ["okta", "Okta logo", "Okta", "—", "—", "circular O", "identity SaaS blue", "the ring as gate/identity"],
    ["openid", "OpenID logo", "OpenID", "—", "—", "stylized ID", "standards mark", "decentralized identity, orange"],
    ["w3c", "W3C logo", "W3C", "—", "—", "wordmark", "standards body", "the web's own authority mark"],
    ["icann", "ICANN logo", "ICANN", "—", "—", "globe / wordmark", "governance", "who governs the namespace"],
    ["aadhaar", "Aadhaar logo", "Aadhaar (UIDAI)", "—", "2010", "sun + fingerprint", "national-ID", "a billion-person identity mark — the fingerprint sun"],
    ["digicert", "DigiCert logo", "DigiCert", "—", "—", "checkmark", "certificate authority", "the CA that issues web trust"],
    ["duckduckgo", "DuckDuckGo logo", "DuckDuckGo", "—", "—", "duck in a circle", "friendly privacy-search", "an animal, not a padlock"],
    ["fido-alliance", "FIDO Alliance logo", "FIDO Alliance", "—", "—", "checkmark / wordmark", "passwordless standard", "the body behind passkeys"],
    ["privacy-badger", "Privacy Badger logo", "Privacy Badger (EFF)", "—", "—", "badger", "activist mascot", "privacy as a scrappy animal"],
  ],
};

const BAD_EXT = /\.(webm|ogv|ogg|pdf|tif|tiff|gif|djvu|stl|mid|wav|webp)$/i;
async function searchCommons(query) {
  const url = `https://commons.wikimedia.org/w/api.php?action=query&list=search&srsearch=${encodeURIComponent(query)}&srnamespace=6&srlimit=10&format=json`;
  const r = await fetch(url, { headers: { "User-Agent": UA } });
  if (!r.ok) throw new Error(`search ${r.status}`);
  const j = await r.json();
  const hits = (j.query?.search || []).map((s) => s.title.replace(/^File:/, ""));
  // prefer svg, then png; prefer titles with "logo"/"seal"; skip media/docs
  const ok = hits.filter((t) => !BAD_EXT.test(t) && /\.(svg|png|jpe?g)$/i.test(t));
  const score = (t) => (/\.svg$/i.test(t) ? 0 : /\.png$/i.test(t) ? 1 : 2) + (/logo|seal|mark|wordmark/i.test(t) ? 0 : 0.5);
  ok.sort((a, b) => score(a) - score(b));
  return ok[0] || null;
}

async function fetchFile(title, destBase) {
  const ext = (title.match(/\.(svg|png|jpe?g)$/i)?.[1] || "png").toLowerCase();
  const url = `https://commons.wikimedia.org/wiki/Special:FilePath/${encodeURIComponent(title.replace(/ /g, "_"))}`;
  const r = await fetch(url, { headers: { "User-Agent": UA } });
  if (!r.ok) throw new Error(`fetch ${r.status}`);
  const buf = Buffer.from(await r.arrayBuffer());
  const orig = `${destBase}.${ext}`;
  writeFileSync(orig, buf);
  // ensure a png twin for contact sheets
  const png = `${destBase}.png`;
  if (ext === "svg") {
    try {
      execFileSync("rsvg-convert", ["-w", "600", "-b", "white", orig, "-o", png]);
    } catch {
      execFileSync("magick", ["-background", "white", "-density", "200", orig, "-resize", "600x", png]);
    }
  } else if (ext !== "png") {
    execFileSync("magick", [orig, "-resize", "600x", png]);
  }
  return { ext, title };
}

const onlyCat = process.argv[2];
const cats = onlyCat ? [onlyCat] : Object.keys(LIB);
for (const cat of cats) {
  const dir = join(HERE, cat);
  mkdirSync(dir, { recursive: true });
  const rows = [];
  const misses = [];
  for (const [slug, query, name, by, year, motif, register, note] of LIB[cat]) {
    const base = join(dir, slug);
    if (existsSync(`${base}.png`)) { console.log(`· ${cat}/${slug} cached`); continue; }
    try {
      const title = await searchCommons(query);
      if (!title) throw new Error("no image hit");
      const { ext, title: t } = await fetchFile(title, base);
      console.log(`✓ ${cat}/${slug}  ←  ${t}`);
      rows.push({ slug, name, by, year, motif, register, note, src: t, ext });
    } catch (e) {
      console.log(`✗ ${cat}/${slug}: ${e.message}`);
      misses.push({ slug, name, query, why: e.message });
    }
    await sleep(350);
  }
  // manifest
  const head = `# ${cat} — corporate-graphics reference\n\n${rows.length} marks fetched from Wikimedia Commons.\n\n| mark | by | year | motif | register | why it matters | source (commons) |\n|---|---|---|---|---|---|---|\n`;
  const body = rows.map((r) => `| **${r.name}** | ${r.by} | ${r.year} | ${r.motif} | ${r.register} | ${r.note} | ${r.src} |`).join("\n");
  writeFileSync(join(dir, "manifest.md"), head + body + "\n");
  if (misses.length) writeFileSync(join(dir, "MISSES.md"), `# misses\n\n` + misses.map((m) => `- ${m.slug} (${m.name}) — "${m.query}" — ${m.why}`).join("\n") + "\n");
  console.log(`— ${cat}: ${rows.length} fetched, ${misses.length} missed —`);
}
console.log("done");
