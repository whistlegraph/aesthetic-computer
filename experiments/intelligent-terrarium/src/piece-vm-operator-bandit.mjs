import { PieceVmPolicyBandit } from "./piece-vm-policy-bandit.mjs";

const FAMILIES = Object.freeze(["variation", "machinery", "exchange"]);
const MAX_TRIALS = 192;

export class PieceVmOperatorBandit extends PieceVmPolicyBandit {
  constructor(stored = null) {
    super(stored, { policies: FAMILIES, maxTrials: MAX_TRIALS, dimension: "mutation-operator-family" });
  }

  static fromJSON(value) {
    return new PieceVmOperatorBandit(value);
  }
}

export const PIECE_VM_OPERATOR_BANDIT = Object.freeze({ families: FAMILIES, maxTrials: MAX_TRIALS });
