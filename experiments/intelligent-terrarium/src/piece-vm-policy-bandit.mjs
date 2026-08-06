const POLICIES = Object.freeze(["phenotype-lead", "champion-control", "branch-diversity"]);
const POLICY_SET = new Set(POLICIES);
const MAX_TRIALS = 192;

function finite(value, fallback = 0) {
  const number = Number(value);
  return Number.isFinite(number) ? number : fallback;
}

function clamp(value, low = 0, high = 1) {
  return Math.max(low, Math.min(high, finite(value)));
}

function normalizeTrial(value, policySet = POLICY_SET) {
  const policy = String(value?.policy || "");
  if (!policySet.has(policy)) return null;
  return Object.freeze({
    at: Math.max(0, Math.floor(finite(value.at))), policy,
    parentId: String(value.parentId || "").slice(0, 24),
    candidateId: String(value.candidateId || "").slice(0, 24),
    mutation: String(value.mutation || "unknown").slice(0, 32),
    nativeValid: Boolean(value.nativeValid), admitted: Boolean(value.admitted),
    capabilityGain: Boolean(value.capabilityGain), staticDelta: clamp(value.staticDelta, -1, 1),
    phenotypeReady: Boolean(value.phenotypeReady), phenotypeScore: clamp(value.phenotypeScore),
  });
}

function reward(trial) {
  const components = [trial.admitted ? 1 : 0, trial.capabilityGain ? 1 : 0];
  if (trial.phenotypeReady) components.push(trial.phenotypeScore);
  return components.reduce((sum, value) => sum + value, 0) / components.length;
}

export class PieceVmPolicyBandit {
  constructor(stored = null, { policies = POLICIES, maxTrials = MAX_TRIALS, dimension = "cultivation-policy" } = {}) {
    this.policies = Object.freeze([...policies]);
    this.policySet = new Set(this.policies);
    this.maxTrials = Math.max(1, Math.min(1024, Math.floor(finite(maxTrials, MAX_TRIALS))));
    this.dimension = String(dimension).slice(0, 32);
    this.trials = (stored?.trials || []).map((value) => normalizeTrial(value, this.policySet))
      .filter(Boolean).slice(-this.maxTrials);
  }

  static fromJSON(value) {
    return new PieceVmPolicyBandit(value);
  }

  record(value) {
    const trial = normalizeTrial(value, this.policySet);
    if (!trial) return null;
    this.trials.push(trial);
    if (this.trials.length > this.maxTrials) this.trials.splice(0, this.trials.length - this.maxTrials);
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
      return normalizeTrial({ ...trial, phenotypeReady: true, phenotypeScore: score }, this.policySet);
    });
    return updated;
  }

  #policyStats(policy) {
    const trials = this.trials.filter((trial) => trial.policy === policy);
    const rewards = trials.map(reward);
    return {
      policy, trials: trials.length,
      admissions: trials.filter((trial) => trial.admitted).length,
      nativeValid: trials.filter((trial) => trial.nativeValid).length,
      capabilityGains: trials.filter((trial) => trial.capabilityGain).length,
      phenotypeReady: trials.filter((trial) => trial.phenotypeReady).length,
      meanReward: rewards.reduce((sum, value) => sum + value, 0) / Math.max(1, rewards.length),
    };
  }

  stats() {
    const total = Math.max(1, this.trials.length);
    return this.policies.map((policy) => {
      const stats = this.#policyStats(policy);
      const exploration = stats.trials ? Math.sqrt(2 * Math.log(Math.max(2, total)) / stats.trials) : null;
      return Object.freeze({ ...stats,
        admissionRate: stats.admissions / Math.max(1, stats.trials),
        capabilityRate: stats.capabilityGains / Math.max(1, stats.trials),
        ucb1: exploration === null ? null : stats.meanReward + exploration,
        exploring: exploration === null,
      });
    });
  }

  bonusPolicy(virtual = new Map()) {
    const total = Math.max(1, this.trials.length + [...virtual.values()].reduce((sum, value) => sum + value, 0));
    return this.stats().map((stats, order) => {
      const extra = virtual.get(stats.policy) || 0;
      const trials = stats.trials + extra;
      const ucb1 = trials ? stats.meanReward + Math.sqrt(2 * Math.log(Math.max(2, total)) / trials) : Infinity;
      return { policy: stats.policy, ucb1, order };
    }).sort((left, right) => right.ucb1 - left.ucb1 || left.order - right.order)[0].policy;
  }

  schedule(count) {
    const size = Math.max(0, Math.min(32, Math.floor(finite(count))));
    const schedule = this.policies.slice(0, Math.min(size, this.policies.length));
    const virtual = new Map();
    while (schedule.length < size) {
      const policy = this.bonusPolicy(virtual);
      schedule.push(policy);
      virtual.set(policy, (virtual.get(policy) || 0) + 1);
    }
    return schedule;
  }

  snapshot() {
    return { schema: 1, algorithm: "ucb1", dimension: this.dimension, maxTrials: this.maxTrials,
      reward: "mean(admitted, capability-gain[, ready-native-phenotype-score])",
      trials: this.trials.length, policies: this.stats(), nextBonus: this.bonusPolicy() };
  }

  toJSON() {
    return { schema: 1, trials: this.trials.map((value) => ({ ...value })) };
  }
}

export const PIECE_VM_POLICY_BANDIT = Object.freeze({ policies: POLICIES, maxTrials: MAX_TRIALS });
