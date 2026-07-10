// hosted.mjs — the single source of truth for which readings are cleared to
// publish, and the CDN basename each one maps to.
//
// This is the PUBLISH ALLOWLIST. A local slug that is not a key here NEVER goes
// public — not to Buzzsprout, not to the CDN, not into the RSS feed. That is
// what keeps confidential / NDA'd readings (e.g. the REGARDE-adjacent
// "named-markets" — "The Market on Your Name") out of every distribution
// surface no matter what sits in out/.
//
// The value is the "siteName" the papers registry uses, so the same mp3 backs
// both the papers "podcast" link (assets.aesthetic.computer/podcast/<siteName>.mp3)
// and the feed. Convention: aesthetic-<slug>-essay. To clear a new episode,
// add one line here — a deliberate, per-episode act.

export const HOSTED = {
  "may-26": "aesthetic-may-26-essay",
  "june-26": "aesthetic-june-26-essay",
  "july-4-26": "aesthetic-july-4-26-essay",
  "july-10-26": "aesthetic-july-10-26-essay",
};

// slug → hosted basename, or null if the slug is not cleared to publish.
export const hosted = (slug) => HOSTED[slug] || null;
