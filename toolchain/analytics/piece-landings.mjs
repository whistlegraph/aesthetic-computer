// piece-landings — which pieces people land on, anonymous visitors included.
//
// Lith's boot telemetry (the `boots` collection, one row per page load) already
// keeps each load's host and URL path. The first-party visit tracker keeps
// only a broad surface ("play"), so this is where piece names come from for
// people who never sign in. A boot is a page load: moving between pieces
// inside one tab does not boot again, so this counts landings, not every
// piece a visitor touches. Signed-in piece opens live in account-activity.
import { visitProperty, visitSurface, automatedVisit, visitReferrer } from "../../system/public/aesthetic.computer/lib/visit-model.mjs";

// A public piece name from a URL path, or null for private and odd paths.
// Colon and tilde parameters are dropped, so `line:5` and `line~red` are `line`;
// a published user piece keeps its public `@handle/piece` address.
export function bootPiece(path = "/") {
  if (visitSurface(path) === null) return null;
  let decoded;
  try { decoded = decodeURIComponent(path); } catch { return null; }
  const parts = decoded.replace(/^\/+/, "").split("/").filter(Boolean);
  if (!parts.length || parts[0] === "index.html") return "prompt";
  const strip = segment => segment.split(/[:~?#]/)[0];
  const name = parts[0].startsWith("@") && parts[1] ? `${parts[0]}/${strip(parts[1])}` : strip(parts[0]);
  return /^[@$A-Za-z0-9_.-][A-Za-z0-9_.$@/-]{0,63}$/.test(name) ? name : "(other)";
}

export function foldPieceLandings(boots, { properties, limit = 50 }) {
  const rows = new Map();
  let counted = 0, skipped = 0;
  for (const { meta = {}, status } of boots) {
    const property = visitProperty(meta.host);
    const piece = property && properties.includes(property) ? bootPiece(meta.path) : null;
    if (!piece || meta.embedded || meta.packMode || meta.localDev || automatedVisit({ userAgent: meta.userAgent })) { skipped++; continue; }
    counted++;
    const key = `${property} ${piece}`;
    const row = rows.get(key) || { property, piece, boots: 0, anonymous: 0, signedIn: 0, completed: 0, referrers: {} };
    row.boots++;
    meta.user ? row.signedIn++ : row.anonymous++;
    if (status === "success") row.completed++;
    const referrer = visitReferrer(meta.referrer);
    if (referrer) row.referrers[referrer] = (row.referrers[referrer] || 0) + 1;
    rows.set(key, row);
  }
  const ranked = [...rows.values()].sort((a, b) => b.boots - a.boots);
  return {
    boots: counted, excluded: skipped,
    pieces: ranked.slice(0, limit).map(({ referrers, ...row }) => ({ ...row,
      referrers: Object.entries(referrers).sort((a, b) => b[1] - a[1]).slice(0, 5).map(([host, n]) => ({ host, boots: n })) })),
    truncated: ranked.length > limit,
  };
}
