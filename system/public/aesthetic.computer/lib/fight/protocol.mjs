// Versioned contracts shared by the Menu Fighter coordinator and every client.

export const FIGHT_PROTOCOL_VERSION = 1;
export const FIGHT_BUILD_ID = "menu-fighter-dev-2026-07-20";
export const FIGHT_SIM_HASH = "fight-int32-v1";
export const FIGHT_RULES_HASH = "freefight-v1";
export const FIGHT_CONTENT_HASH = "base-roster-v1";

export const FIGHT_MANIFEST = Object.freeze({
  protocolVersion: FIGHT_PROTOCOL_VERSION,
  buildId: FIGHT_BUILD_ID,
  simHash: FIGHT_SIM_HASH,
  rulesHash: FIGHT_RULES_HASH,
  contentHash: FIGHT_CONTENT_HASH,
});

export function compatibleManifest(value, expected = FIGHT_MANIFEST) {
  if (!value || typeof value !== "object") return false;
  return Object.keys(expected).every((key) => value[key] === expected[key]);
}

export function manifestMismatch(value, expected = FIGHT_MANIFEST) {
  for (const key of Object.keys(expected)) {
    if (value?.[key] !== expected[key]) return key;
  }
  return null;
}

