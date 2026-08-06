const MAX_TRIALS = 192;
const CAPABILITIES = Object.freeze(["named-memory", "temporal-memory", "abstraction", "sensing", "ecology"]);

function finite(value, fallback = 0) {
  const number = Number(value);
  return Number.isFinite(number) ? number : fallback;
}

function clamp(value, low = 0, high = 1) {
  return Math.max(low, Math.min(high, finite(value)));
}

export function pieceVmDevelopment(record = null) {
  const structure = record?.structure || {};
  const flags = Object.freeze({
    "named-memory": Number(structure.layouts || 0) > 0 && Number(structure.layoutBytes || 0) > 0,
    "temporal-memory": Number(structure.memory || 0) >= 4,
    abstraction: Number(structure.functions || 0) > 0 && Number(structure.arguments || 0) > 0,
    sensing: Number(structure.senses || 0) > 0,
    ecology: (record?.capabilityLineage?.length || 0) > 0,
  });
  const attained = CAPABILITIES.filter((name) => flags[name]);
  return Object.freeze({ schema: 1, flags, attained: Object.freeze(attained),
    missing: Object.freeze(CAPABILITIES.filter((name) => !flags[name])),
    breadth: attained.length, signature: CAPABILITIES.map((name) => flags[name] ? "1" : "0").join("") });
}

export function pieceVmCurriculumEvidence(parent, child) {
  const before = pieceVmDevelopment(parent), after = pieceVmDevelopment(child);
  const gained = CAPABILITIES.filter((name) => !before.flags[name] && after.flags[name]);
  const lost = CAPABILITIES.filter((name) => before.flags[name] && !after.flags[name]);
  return Object.freeze({ schema: 1, parentId: String(parent?.id || "").slice(0, 24),
    before: before.signature, after: after.signature, beforeBreadth: before.breadth,
    afterBreadth: after.breadth, gained: Object.freeze(gained), lost: Object.freeze(lost),
    retained: Object.freeze(CAPABILITIES.filter((name) => before.flags[name] && after.flags[name])),
    advancement: gained.length, regression: lost.length,
    compound: after.breadth >= 3, complete: after.breadth === CAPABILITIES.length });
}

export function pieceVmCurriculumParent(records = []) {
  return [...records].sort((left, right) => {
    const leftDevelopment = pieceVmDevelopment(left), rightDevelopment = pieceVmDevelopment(right);
    const registerCost = { "named-memory": 0, "temporal-memory": 3, abstraction: 1, sensing: 1, ecology: 0 };
    const leftTarget = leftDevelopment.missing[0], rightTarget = rightDevelopment.missing[0];
    const leftCompatible = !leftTarget || Number(left?.registerCount || 0) + registerCost[leftTarget] <= 32;
    const rightCompatible = !rightTarget || Number(right?.registerCount || 0) + registerCost[rightTarget] <= 32;
    return rightDevelopment.breadth - leftDevelopment.breadth ||
      Number(rightCompatible) - Number(leftCompatible) ||
      Number(left?.registerCount || 0) - Number(right?.registerCount || 0) ||
      Number(left?.instructionCount || 0) - Number(right?.instructionCount || 0) ||
      Number(right?.generation || 0) - Number(left?.generation || 0) ||
      Number(right?.score || 0) - Number(left?.score || 0) ||
      String(left?.id || "").localeCompare(String(right?.id || ""));
  })[0] || null;
}

export function pieceVmCurriculumTarget(record) {
  const development = pieceVmDevelopment(record);
  const targets = {
    "named-memory": { family: "machinery", mutations: ["data-layout"] },
    "temporal-memory": { family: "machinery", mutations: ["memory-oscillator"] },
    abstraction: { family: "machinery", mutations: ["argument-function-graft", "function-graft"] },
    sensing: { family: "machinery", mutations: ["sense-graft"] },
    ecology: { family: "exchange", mutations: ["environment-graft", "lineage-crossover"] },
  };
  const capability = development.missing[0] || null;
  return capability ? Object.freeze({ capability, ...targets[capability] }) : null;
}

export function prioritizePieceVmCurriculum(candidates, adaptivePolicy, adaptiveOperator, curriculumLead = false) {
  if (!Array.isArray(candidates)) throw new TypeError("PieceVM curriculum priority needs candidates");
  const adaptive = (candidate) => Number(candidate?.selectionEvidence?.policy === adaptivePolicy) +
    Number(candidate?.operatorFamily === adaptiveOperator);
  const development = (candidate) => {
    const evidence = candidate?.curriculumEvidence;
    if (!evidence) return 0;
    return evidence.regression ? -evidence.regression * 2 :
      evidence.advancement * 3 + evidence.afterBreadth / CAPABILITIES.length;
  };
  return candidates.sort((left, right) => {
    const leftChain = Number(left?.selectionEvidence?.policy === "curriculum-chain");
    const rightChain = Number(right?.selectionEvidence?.policy === "curriculum-chain");
    const leftScore = curriculumLead ? leftChain * 1_000 + development(left) * 10 + adaptive(left) :
      adaptive(left) * 10 + development(left);
    const rightScore = curriculumLead ? rightChain * 1_000 + development(right) * 10 + adaptive(right) :
      adaptive(right) * 10 + development(right);
    return rightScore - leftScore;
  });
}

function normalizeTrial(value) {
  const evidence = value?.evidence || {};
  const gained = CAPABILITIES.filter((name) => evidence.gained?.includes(name));
  const lost = CAPABILITIES.filter((name) => evidence.lost?.includes(name));
  const afterBreadth = Math.max(0, Math.min(CAPABILITIES.length, Math.floor(finite(evidence.afterBreadth))));
  return Object.freeze({ at: Math.max(0, Math.floor(finite(value?.at))),
    parentId: String(value?.parentId || evidence.parentId || "").slice(0, 24),
    candidateId: String(value?.candidateId || "").slice(0, 24),
    mutation: String(value?.mutation || "unknown").slice(0, 32),
    lead: Boolean(value?.lead), nativeValid: Boolean(value?.nativeValid), admitted: Boolean(value?.admitted),
    evidence: Object.freeze({ schema: 1, before: String(evidence.before || "00000").slice(0, 5),
      after: String(evidence.after || "00000").slice(0, 5),
      beforeBreadth: Math.max(0, Math.min(CAPABILITIES.length, Math.floor(finite(evidence.beforeBreadth)))),
      afterBreadth, gained: Object.freeze(gained), lost: Object.freeze(lost),
      retained: Object.freeze(CAPABILITIES.filter((name) => evidence.retained?.includes(name))),
      advancement: gained.length, regression: lost.length,
      compound: afterBreadth >= 3, complete: afterBreadth === CAPABILITIES.length }),
    phenotypeReady: Boolean(value?.phenotypeReady), phenotypeScore: clamp(value?.phenotypeScore),
  });
}

export class PieceVmCurriculum {
  constructor(stored = null) {
    this.trials = (stored?.trials || []).map(normalizeTrial).slice(-MAX_TRIALS);
  }

  static fromJSON(value) {
    return new PieceVmCurriculum(value);
  }

  shouldLead(iteration, batch = 4) {
    return Math.floor(Math.max(0, finite(iteration)) / Math.max(1, Math.floor(finite(batch, 4)))) % 4 === 3;
  }

  evidence(parent, child) {
    return pieceVmCurriculumEvidence(parent, child);
  }

  record(value) {
    const trial = normalizeTrial(value);
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

  snapshot(champion = null, iteration = 0, batch = 4) {
    const admitted = this.trials.filter((trial) => trial.admitted);
    const advanced = admitted.filter((trial) => trial.evidence.advancement > 0 && !trial.evidence.regression);
    const championDevelopment = pieceVmDevelopment(champion);
    return { schema: 1, strategy: "retained-capability-breadth", leadEvery: 4,
      maxTrials: MAX_TRIALS, trials: this.trials.length,
      admissions: admitted.length, advancements: advanced.length,
      regressions: admitted.filter((trial) => trial.evidence.regression > 0).length,
      compoundAdmissions: admitted.filter((trial) => trial.evidence.compound).length,
      completeAdmissions: admitted.filter((trial) => trial.evidence.complete).length,
      maxBreadth: admitted.reduce((max, trial) => Math.max(max, trial.evidence.afterBreadth), 0),
      phenotypeReady: admitted.filter((trial) => trial.phenotypeReady).length,
      champion: championDevelopment, nextLead: this.shouldLead(iteration, batch) };
  }

  toJSON() {
    return { schema: 1, trials: this.trials.map((trial) => ({ ...trial,
      evidence: { ...trial.evidence, gained: [...trial.evidence.gained], lost: [...trial.evidence.lost],
        retained: [...trial.evidence.retained] } })) };
  }
}

export const PIECE_VM_CURRICULUM = Object.freeze({ capabilities: CAPABILITIES, maxTrials: MAX_TRIALS,
  leadEvery: 4 });
