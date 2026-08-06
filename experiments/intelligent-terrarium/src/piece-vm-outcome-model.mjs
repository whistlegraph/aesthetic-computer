import { PIECE_VM_MUTATION_CATALOG, pieceVmMutationFamily } from "./piece-vm-nursery.mjs";

const MAX_TRIALS = 384;
const CAPABILITIES = Object.freeze(["functions", "arguments", "layouts", "layoutBytes", "memory",
  "senses", "transforms", "capabilityLineage"]);
const MUTATIONS = Object.freeze(Object.values(PIECE_VM_MUTATION_CATALOG).flat());
const MUTATION_SET = new Set(MUTATIONS);

function finite(value, fallback = 0) {
  const number = Number(value);
  return Number.isFinite(number) ? number : fallback;
}

function clamp(value, low = 0, high = 1) {
  return Math.max(low, Math.min(high, finite(value)));
}

function normalizeDelta(value) {
  return Object.freeze(Object.fromEntries(CAPABILITIES.map((name) =>
    [name, Math.max(0, Math.min(65_535, Math.floor(finite(value?.[name]))))])));
}

function normalizeTrial(value) {
  const requestedMutation = String(value?.requestedMutation || "");
  if (!MUTATION_SET.has(requestedMutation)) return null;
  const preferredMutation = MUTATION_SET.has(String(value?.preferredMutation || "")) ?
    String(value.preferredMutation) : requestedMutation;
  const mutation = MUTATION_SET.has(String(value?.mutation || "")) ? String(value.mutation) : "unknown";
  const honored = mutation === requestedMutation;
  return Object.freeze({
    at: Math.max(0, Math.floor(finite(value.at))), preferredMutation, requestedMutation, mutation,
    family: pieceVmMutationFamily(requestedMutation), honored,
    compatibilityFallback: preferredMutation !== requestedMutation,
    parentId: String(value.parentId || "").slice(0, 24),
    candidateId: String(value.candidateId || "").slice(0, 24),
    nativeValid: Boolean(value.nativeValid), admitted: Boolean(value.admitted),
    capabilityDelta: normalizeDelta(value.capabilityDelta),
    phenotypeReady: Boolean(value.phenotypeReady), phenotypeScore: clamp(value.phenotypeScore),
  });
}

function capabilityBreadth(trial) {
  return CAPABILITIES.filter((name) => trial.capabilityDelta[name] > 0).length / CAPABILITIES.length;
}

function reward(trial) {
  const components = [trial.honored && trial.nativeValid ? 1 : 0,
    trial.honored && trial.admitted ? 1 : 0, trial.honored ? capabilityBreadth(trial) : 0];
  if (trial.honored && trial.phenotypeReady) components.push(trial.phenotypeScore);
  return components.reduce((sum, value) => sum + value, 0) / components.length;
}

export class PieceVmOutcomeModel {
  constructor(stored = null) {
    this.trials = (stored?.trials || []).map(normalizeTrial).filter(Boolean).slice(-MAX_TRIALS);
  }

  static fromJSON(value) {
    return new PieceVmOutcomeModel(value);
  }

  record(value) {
    const trial = normalizeTrial(value);
    if (!trial) return null;
    this.trials.push(trial);
    if (this.trials.length > MAX_TRIALS) this.trials.splice(0, this.trials.length - MAX_TRIALS);
    return trial;
  }

  observePhenotypes(summaries = []) {
    const ready = new Map((summaries || []).filter((value) => value?.ready)
      .map((value) => [String(value.id), clamp(value.score)]));
    let updated = 0;
    this.trials = this.trials.map((trial) => {
      const score = ready.get(trial.candidateId);
      if (score === undefined || trial.phenotypeReady && trial.phenotypeScore === score) return trial;
      updated += 1;
      return normalizeTrial({ ...trial, phenotypeReady: true, phenotypeScore: score });
    });
    return updated;
  }

  stats() {
    return MUTATIONS.map((mutation) => {
      const trials = this.trials.filter((trial) => trial.requestedMutation === mutation);
      const preferenceMisses = this.trials.filter((trial) =>
        trial.preferredMutation === mutation && trial.requestedMutation !== mutation).length;
      const preferences = trials.filter((trial) => trial.preferredMutation === mutation).length + preferenceMisses;
      const exposures = trials.length + preferenceMisses;
      const direct = trials.filter((trial) => trial.honored);
      const rewards = trials.map(reward);
      const deltas = Object.fromEntries(CAPABILITIES.map((name) =>
        [name, direct.reduce((sum, trial) => sum + trial.capabilityDelta[name], 0)]));
      const capabilityGains = direct.filter((trial) => capabilityBreadth(trial) > 0).length;
      return Object.freeze({ mutation, family: pieceVmMutationFamily(mutation), exposures,
        preferences, compatibilityMisses: preferenceMisses,
        compatibilityRate: (preferences - preferenceMisses) / Math.max(1, preferences), requests: trials.length,
        honored: direct.length, nativeValid: direct.filter((trial) => trial.nativeValid).length,
        admissions: direct.filter((trial) => trial.admitted).length, capabilityGains,
        phenotypeReady: direct.filter((trial) => trial.phenotypeReady).length,
        meanReward: rewards.reduce((sum, value) => sum + value, 0) / Math.max(1, exposures),
        availabilityRate: direct.length / Math.max(1, trials.length),
        admissionRate: direct.filter((trial) => trial.admitted).length / Math.max(1, direct.length),
        capabilityRate: capabilityGains / Math.max(1, direct.length), deltas });
    });
  }

  rank(family, virtual = new Map()) {
    const choices = this.stats().filter((value) => value.family === family);
    if (!choices.length) return [];
    const total = Math.max(1, this.trials.length + [...virtual.values()].reduce((sum, value) => sum + value, 0));
    return choices.map((stats, order) => {
      const exposures = stats.exposures + (virtual.get(stats.mutation) || 0);
      const ucb1 = exposures ? stats.meanReward + Math.sqrt(2 * Math.log(Math.max(2, total)) / exposures) : Infinity;
      return { mutation: stats.mutation, ucb1, order };
    }).sort((left, right) => right.ucb1 - left.ucb1 || left.order - right.order)
      .map((value) => value.mutation);
  }

  bonusMutation(family, virtual = new Map()) {
    return this.rank(family, virtual)[0] || null;
  }

  preferenceSchedule(families = []) {
    const virtual = new Map();
    return families.map((family) => {
      const ranked = this.rank(family, virtual);
      if (ranked[0]) virtual.set(ranked[0], (virtual.get(ranked[0]) || 0) + 1);
      return ranked;
    });
  }

  schedule(families = []) {
    return this.preferenceSchedule(families).map((values) => values[0] || null);
  }

  snapshot() {
    const mutations = this.stats();
    return { schema: 1, algorithm: "ucb1", dimension: "requested-mutation",
      reward: "mean(native-valid, admitted, capability-breadth[, ready-native-phenotype-score])",
      maxTrials: MAX_TRIALS, trials: this.trials.length, mutations,
      next: Object.fromEntries(Object.keys(PIECE_VM_MUTATION_CATALOG)
        .map((family) => [family, this.bonusMutation(family)])) };
  }

  toJSON() {
    return { schema: 1, trials: this.trials.map((value) => ({ ...value,
      capabilityDelta: { ...value.capabilityDelta } })) };
  }
}

export const PIECE_VM_OUTCOME_MODEL = Object.freeze({ mutations: MUTATIONS, capabilities: CAPABILITIES,
  maxTrials: MAX_TRIALS });
