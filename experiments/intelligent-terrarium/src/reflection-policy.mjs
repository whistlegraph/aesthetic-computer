const ORGAN_NAMES = ["sensory", "spatial", "drive", "memory", "action", "voice"];
const ACTIONS = new Set(["attune", "broadcast"]);
const FAILURES = new Set(["disabled", "unavailable", "timeout", "malformed"]);

export function validateReflectionProposal(value) {
  if (!value || typeof value !== "object" || Array.isArray(value)) throw new TypeError("proposal must be an object");
  const keys = Object.keys(value).sort();
  if (keys.join(",") !== "action,intensity,schema,target") throw new TypeError("proposal fields do not match schema");
  if (value.schema !== 1) throw new TypeError("proposal schema must be 1");
  if (!ACTIONS.has(value.action)) throw new TypeError("proposal action is invalid");
  if (!ORGAN_NAMES.includes(value.target)) throw new TypeError("proposal target is invalid");
  if (!Number.isFinite(value.intensity) || value.intensity < -1 || value.intensity > 1) {
    throw new TypeError("proposal intensity must be within -1..1");
  }
  return {
    schema: 1,
    action: value.action,
    target: value.target,
    intensity: Math.round(value.intensity * 1000) / 1000,
  };
}

export function decideReflection({ proposal, failure } = {}) {
  if (failure !== undefined) {
    if (!FAILURES.has(failure) || proposal !== undefined) throw new TypeError("invalid reflection fallback");
    return { decision: "fallback", reason: failure, proposal: null };
  }
  const clean = validateReflectionProposal(proposal);
  if (clean.action !== "attune") return { decision: "rejected", reason: "action-not-authorized", proposal: clean };
  if (Math.abs(clean.intensity) > 0.25) return { decision: "rejected", reason: "intensity-outside-policy", proposal: clean };
  return { decision: "accepted", reason: "bounded-attunement", proposal: clean };
}
