// Client-specific knowledge that turns a generic DOM frame into a semantic one.
//
// Captutor owns the protocol; each client owns a small definition containing
// product terms, stable DOM contracts, behavior intent, and source provenance.
// Screenplays and app-frame inspectors consume the same definition so what Iris
// says, points at, and verifies cannot quietly drift apart.

function assertRecord(value, name) {
  if (!value || typeof value !== "object" || Array.isArray(value)) {
    throw new TypeError(`${name} must be an object`);
  }
}
function deepFreeze(value) {
  if (!value || typeof value !== "object" || Object.isFrozen(value)) return value;
  Object.freeze(value);
  for (const child of Object.values(value)) deepFreeze(child);
  return value;
}

export function defineAppIntelligence(definition) {
  assertRecord(definition, "app intelligence");
  for (const key of ["id", "schema", "hostMatch"]) {
    if (typeof definition[key] !== "string" || !definition[key]) {
      throw new TypeError(`app intelligence.${key} must be a non-empty string`);
    }
  }
  assertRecord(definition.source, "app intelligence.source");
  assertRecord(definition.glossary, "app intelligence.glossary");
  assertRecord(definition.concepts, "app intelligence.concepts");
  assertRecord(definition.behaviors, "app intelligence.behaviors");
  return deepFreeze(structuredClone(definition));
}

export function localizedConcepts(intelligence, t) {
  if (typeof t !== "function") throw new TypeError("localizedConcepts needs a translator");
  return Object.fromEntries(Object.entries(intelligence.concepts).map(([id, concept]) => [id, {
    ...concept,
    label:concept.labelKey ? t(concept.labelKey) : concept.label,
  }]));
}

export function confidenceFromChecks(checks) {
  const values = Object.values(checks || {});
  if (!values.length) return 0;
  return Number((values.filter(Boolean).length / values.length).toFixed(3));
}
