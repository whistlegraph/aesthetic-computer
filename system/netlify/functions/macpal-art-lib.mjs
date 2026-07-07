// macpal-art-lib, 26.07.06
// The pure core of the macpal-art wire — name/SVG validation, pose merging, and
// the read-time view. No Redis, no auth: split out from the handler so it can be
// unit-tested without a backend round-trip (see spec/macpal-art-test or the
// scratch harness). The handler in macpal-art.mjs owns the HTTP + storage.

export const MAX_POSES = 16;
export const MAX_SVG = 96 * 1024; // 96KB per pose — glyphs are ~1–2KB, room to spare
export const MAX_NAME = 40;

// Pose names ride into filenames on the star (art/<name>.svg) and index the
// idle cycle, so keep them to tame slug characters.
export function cleanName(raw) {
  return (raw ?? "")
    .toString()
    .trim()
    .toLowerCase()
    .replace(/[^a-z0-9_-]/g, "")
    .slice(0, MAX_NAME);
}

// Recipient keys match the caption endpoint's — short lowercase slugs.
export function cleanKey(raw) {
  return (raw || "fia")
    .toString()
    .trim()
    .toLowerCase()
    .replace(/[^a-z0-9_-]/g, "")
    .slice(0, 64) || "fia";
}

// Keep only what looks like an SVG document, size-capped. Anything else (or an
// oversized blob) is rejected so the star never writes junk to disk. Returns
// the string when valid, null for an explicit delete, undefined when invalid.
export function cleanSVG(raw) {
  if (raw == null) return null; // an explicit delete
  const s = raw.toString();
  if (s.length > MAX_SVG || !/<svg[\s>]/i.test(s)) return undefined; // invalid
  return s;
}

// Merge a patch of poses onto the stored map: a string sets/adds a pose, an
// explicit null removes it, an invalid value is dropped. Returns the new map
// and whether anything actually changed, so a no-op POST doesn't bump rev.
export function mergePoses(current, patch) {
  const poses = { ...current };
  let changed = false;
  let rejected = 0;
  for (const [rawName, rawSVG] of Object.entries(patch)) {
    const name = cleanName(rawName);
    if (!name) { rejected++; continue; }
    const svg = cleanSVG(rawSVG);
    if (svg === undefined) { rejected++; continue; } // present but invalid
    if (svg === null) {
      if (name in poses) { delete poses[name]; changed = true; }
    } else if (poses[name] !== svg) {
      poses[name] = svg;
      changed = true;
    }
  }
  return { poses, changed, rejected };
}

// Idle-cycle names first (name-sorted), then "sing" — the order the star draws.
export function orderedNames(poses) {
  const names = Object.keys(poses);
  const idle = names.filter((n) => n !== "sing").sort();
  return names.includes("sing") ? [...idle, "sing"] : idle;
}

// Sanitize a star's self-report of what it's actually rendering. This is a
// compact *status* (no SVG bodies) so it can be accepted unauthenticated — the
// star has no admin token. Each pose carries its byte size, a short content
// hash (to match against the pushed art), and whether it came from the wire or
// the app bundle. Tightly capped so the device slot stays tiny.
export function cleanReport(report) {
  const list = Array.isArray(report?.poses) ? report.poses : [];
  const poses = list
    .slice(0, MAX_POSES)
    .map((p) => ({
      name: cleanName(p?.name),
      bytes: Math.max(0, Math.min(1_000_000, Math.round(Number(p?.bytes) || 0))),
      hash: (p?.hash ?? "").toString().toLowerCase().replace(/[^a-f0-9]/g, "").slice(0, 64),
      source: p?.source === "bundle" ? "bundle" : "wire",
    }))
    .filter((p) => p.name);
  return { rev: Math.max(0, Math.round(Number(report?.rev) || 0)), poses };
}

// What a GET returns: rev/at, the ordered pose names, and (optionally) bodies.
export function view(stored, { withBodies }) {
  if (!stored) return { rev: 0, at: null, names: [], ...(withBodies ? { poses: {} } : {}) };
  const poses = stored.poses || {};
  return {
    rev: stored.rev ?? 0,
    at: stored.at ?? null,
    names: orderedNames(poses),
    ...(withBodies ? { poses } : {}),
  };
}
