/**
 * Keeps FA2 v5 Production Contract - Offline Source Checks
 *
 * These tests intentionally avoid network access.
 * They validate local v5 contract source invariants required for production launch.
 */

import fs from 'node:fs';
import path from 'node:path';

describe('🚀 Keeps FA2 v5 Production Contract - Source Checks', () => {
  const v5ContractPath = path.join(process.cwd(), 'tezos', 'keeps_fa2_v5.py');
  const launchPlanPath = path.join(process.cwd(), 'tezos', 'V5-LAUNCH-PLAN.md');

  let v5ContractSource;
  let launchPlanSource;

  beforeAll(() => {
    v5ContractSource = fs.readFileSync(v5ContractPath, 'utf8');
    launchPlanSource = fs.readFileSync(launchPlanPath, 'utf8');
  });

  it('uses v5 default keep fee of 2.5 XTZ', () => {
    expect(v5ContractSource).toContain('self.data.keep_fee = sp.mutez(2500000)');
  });

  it('has explicit v5 pause/edit rejection messages', () => {
    expect(v5ContractSource).toContain('"MINTING_PAUSED"');
    expect(v5ContractSource).toContain('"EDITING_PAUSED"');
    expect(v5ContractSource).toContain('"INSUFFICIENT_FEE"');
  });

  it('preserves v4 safety entrypoints in v5 contract', () => {
    const requiredEntrypoints = [
      'def pause(',
      'def unpause(',
      'def set_default_royalty(',
      'def admin_transfer('
    ];

    requiredEntrypoints.forEach((entrypoint) => {
      expect(v5ContractSource).toContain(entrypoint);
    });
  });

  it('keeps duplicate prevention and creator tracking', () => {
    expect(v5ContractSource).toContain('self.data.content_hashes');
    expect(v5ContractSource).toContain('self.data.token_creators');
    expect(v5ContractSource).toContain('"DUPLICATE_CONTENT_HASH"');
  });

  it('documents v5 launch as the production path', () => {
    expect(launchPlanSource).toContain('Keeps FA2 v5 Launch Plan');
    expect(launchPlanSource).toContain('Goal:** Deploy final production v5 contract');
    expect(launchPlanSource).toContain('Create `keeps_fa2_v5.py` from v4');
  });
});
