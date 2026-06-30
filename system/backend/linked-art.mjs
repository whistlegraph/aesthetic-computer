// linked-art.mjs
// Serialize Aesthetic Computer entities to Linked Art — the JSON-LD profile of
// CIDOC CRM that Getty and peer cultural-heritage institutions actually ingest.
// This is the read-side sibling of bluesky-mirror.mjs: instead of broadcasting a
// mood to a social firehose, we expose opted-in works as linked open data.
//
// These functions are PURE: they take already-resolved plain fields and return a
// JSON-LD object. All MongoDB lookups + the consent/rights gate live in the
// crm.mjs function. Keeping them pure makes them unit-testable against fixtures.
//
// Canonical identity URIs live on data.aesthetic.computer; each carries a link
// back to the human-facing page on the apex. See crm/SCORE.md.
// 2026.06.29

export const LA_CONTEXT = "https://linked.art/ns/v1/linked-art.json";
export const DATA_BASE = "https://data.aesthetic.computer";
export const WEB_BASE = "https://aesthetic.computer";

// Getty AAT vocabulary terms. Every id below was verified against the live
// Getty SPARQL endpoint (https://vocab.getty.edu/sparql) on 2026-06-29; labels
// are Getty's own preferred labels. (crm/SCORE.md §8, open decision 3 — closed.)
const AAT = {
  primaryName: ["300404670", "preferred terms"],
  paintings: ["300033618", "paintings (visual works)"],
  digitalArt: ["300386810", "digital art (visual works)"],
  digitalImages: ["300215302", "digital images"],
  software: ["300028566", "software"],
  briefTexts: ["300418049", "brief texts"],
  english: ["300388277", "English (language)"],
};

// Build a Getty AAT classification node.
function aat(key) {
  const [id, label] = AAT[key];
  return { id: `http://vocab.getty.edu/aat/${id}`, type: "Type", _label: label };
}

// Supported licenses → Creative Commons URIs. A work cannot be broadcast
// without one (Getty pipelines reject objects lacking a rights statement).
export const LICENSES = {
  "CC0-1.0": ["https://creativecommons.org/publicdomain/zero/1.0/", "CC0 1.0 Universal"],
  "CC-BY-4.0": ["https://creativecommons.org/licenses/by/4.0/", "CC BY 4.0"],
  "CC-BY-SA-4.0": ["https://creativecommons.org/licenses/by-sa/4.0/", "CC BY-SA 4.0"],
  "CC-BY-NC-4.0": ["https://creativecommons.org/licenses/by-nc/4.0/", "CC BY-NC 4.0"],
};

// Standardized rights statements (rightsstatements.org) used by Europeana/DPLA.
// "InC" is the default for works we describe but don't hold reuse rights to:
// the record is open + citable, but copyright (and the right to sell) stays
// entirely with the artist. CC keys above are an explicit opt-in upgrade.
export const RIGHTS_STATEMENTS = {
  InC: ["http://rightsstatements.org/vocab/InC/1.0/", "In Copyright"],
};

export const DEFAULT_RIGHTS = "InC";

export function isLicense(key) {
  return Object.prototype.hasOwnProperty.call(LICENSES, key);
}

// E30 Right → a `subject_to` rights statement. Accepts a CC license key or a
// rightsstatements.org key (e.g. "InC").
function rights(key) {
  const entry = LICENSES[key] || RIGHTS_STATEMENTS[key];
  if (!entry) return [];
  const [id, label] = entry;
  return [{ type: "Right", _label: label, classified_as: [{ id, type: "Type", _label: label }] }];
}

// E52 Time-Span from a Date or ISO string.
function timespan(when) {
  if (!when) return undefined;
  const iso = when instanceof Date ? when.toISOString() : new Date(when).toISOString();
  return { type: "TimeSpan", begin_of_the_begin: iso, end_of_the_end: iso };
}

// Public image URL for a painting. Two slug shapes exist:
//   "{sub}/{kind}/{ts}" (kind varies: chat, painting, …) → strip the sub prefix
//   "{ts}" (older bare-timestamp slugs)                   → live under painting/{ts}
// Either way the URL keys off @handle, not the auth0 sub.
export function paintingImageUrl(handle, slug, sub) {
  let mediaPath = String(slug || "");
  if (!mediaPath) return undefined;
  if (sub && mediaPath.startsWith(sub + "/")) {
    mediaPath = mediaPath.slice(sub.length + 1);
  } else if (!mediaPath.includes("/")) {
    mediaPath = "painting/" + mediaPath;
  }
  return `${WEB_BASE}/media/@${handle.replace(/^@/, "")}/${mediaPath}.png`;
}

// The author as an E21 Person reference (the hub every work points back to).
function personRef(handle) {
  const clean = handle.replace(/^@/, "");
  return { id: `${DATA_BASE}/@${clean}`, type: "Person", _label: `@${clean}` };
}

// E12 Production / E65 Creation event carrying authorship + time.
function madeBy(type, handle, when, place = true) {
  const ev = { type, carried_out_by: [personRef(handle)] };
  const ts = timespan(when);
  if (ts) ev.timespan = ts;
  if (place) {
    ev.took_place_at = [{ id: WEB_BASE, type: "Place", _label: "Aesthetic Computer" }];
  }
  return ev;
}

// A `subject_of` / seeAlso link back to the human-facing apex page.
function seeAlso(webUrl, label) {
  return {
    type: "LinguisticObject",
    _label: label,
    digitally_carried_by: [
      { type: "DigitalObject", access_point: [{ id: webUrl, type: "DigitalObject" }] },
    ],
  };
}

// ⸻ Person — @sat → E21 Person ⸻
export function personToLinkedArt({ handle, latestMood } = {}) {
  const clean = handle.replace(/^@/, "");
  const doc = {
    "@context": LA_CONTEXT,
    id: `${DATA_BASE}/@${clean}`,
    type: "Person",
    _label: `@${clean}`,
    identified_by: [
      { type: "Name", content: `@${clean}`, classified_as: [aat("primaryName")] },
    ],
    subject_of: [seeAlso(`${WEB_BASE}/@${clean}`, "Aesthetic Computer profile")],
  };
  if (latestMood?.mood) {
    doc.subject_of.push({
      type: "LinguisticObject",
      _label: "latest mood",
      classified_as: [aat("briefTexts")],
      content: latestMood.mood,
    });
  }
  return doc;
}

// ⸻ Painting → E22 Human-Made Object / D1 Digital Object ⸻
export function paintingToLinkedArt({ code, handle, when, imageUrl, license } = {}) {
  const clean = handle.replace(/^@/, "");
  return {
    "@context": LA_CONTEXT,
    id: `${DATA_BASE}/painting/${code}`,
    type: "DigitalObject",
    _label: `painting ${code} by @${clean}`,
    classified_as: [aat("paintings"), aat("digitalArt"), aat("digitalImages")],
    produced_by: madeBy("Production", handle, when),
    subject_to: rights(license),
    subject_of: [seeAlso(`${WEB_BASE}/painting/${code}`, "view on Aesthetic Computer")],
    digitally_shown_by: imageUrl
      ? [
          {
            type: "DigitalObject",
            format: "image/png",
            access_point: [{ id: imageUrl, type: "DigitalObject" }],
          },
        ]
      : undefined,
  };
}

// ⸻ Piece (KidLisp / .mjs) → E73 Information Object + E65 Creation ⸻
export function pieceToLinkedArt({ code, handle, when, source, hash, license } = {}) {
  const clean = handle.replace(/^@/, "");
  const doc = {
    "@context": LA_CONTEXT,
    id: `${DATA_BASE}/piece/${code}`,
    type: "DigitalObject",
    _label: `piece $${code} by @${clean}`,
    classified_as: [aat("software"), aat("digitalArt")],
    created_by: madeBy("Creation", handle, when),
    subject_to: rights(license),
    subject_of: [seeAlso(`${WEB_BASE}/${code}`, "run on Aesthetic Computer")],
  };
  if (hash) {
    // No Getty AAT term for "checksum"; an unclassified Identifier with a
    // descriptive label is the honest representation.
    doc.identified_by = [{ type: "Identifier", content: hash, _label: "sha-256 checksum" }];
  }
  if (source) {
    // No Getty AAT term for "source code"; carry the program text unclassified.
    doc.carries = [{ type: "LinguisticObject", content: source, _label: "source code" }];
  }
  return doc;
}

// ⸻ Mood → E33 Linguistic Object + E65 Creation ⸻
export function moodToLinkedArt({ handle, rkey, mood, when, blueskyUri, license } = {}) {
  const clean = handle.replace(/^@/, "");
  const doc = {
    "@context": LA_CONTEXT,
    id: `${DATA_BASE}/mood/${clean}/${rkey}`,
    type: "LinguisticObject",
    _label: `mood by @${clean}`,
    classified_as: [aat("briefTexts")],
    content: mood,
    language: [aat("english")],
    created_by: madeBy("Creation", handle, when, false),
    subject_to: rights(license),
    subject_of: [seeAlso(`${WEB_BASE}/moods~${clean}~${rkey}`, "view on Aesthetic Computer")],
  };
  if (blueskyUri) {
    doc.equivalent = [{ id: blueskyUri, type: "LinguisticObject", _label: "Bluesky post" }];
  }
  return doc;
}
