/**
 * Keeps v7 Production - Source Invariants
 *
 * Offline checks for v7 rollout semantics:
 * - deploy tooling supports v7 profile/artifacts
 * - edit_metadata allows owner full edits + creator refresh-only updates
 * - immutable fields remain protected
 */

import fs from 'node:fs';
import path from 'node:path';

describe('🚀 Keeps v7 Production - Source Checks', () => {
  const keepsCliPath = path.join(process.cwd(), 'tezos', 'keeps.mjs');
  const compilePath = path.join(process.cwd(), 'tezos', 'compile.fish');
  const v7ContractPath = path.join(process.cwd(), 'tezos', 'kidlisp_keeps_fa2_v7.py');
  const v7CompiledTzPath = path.join(process.cwd(), 'tezos', 'KeepsFA2v7', 'step_002_cont_0_contract.tz');

  let keepsCliSource;
  let compileSource;
  let v7ContractSource;
  let v7CompiledTzSource;

  beforeAll(() => {
    keepsCliSource = fs.readFileSync(keepsCliPath, 'utf8');
    compileSource = fs.readFileSync(compilePath, 'utf8');
    v7ContractSource = fs.readFileSync(v7ContractPath, 'utf8');
    v7CompiledTzSource = fs.readFileSync(v7CompiledTzPath, 'utf8');
  });

  const entrypointBlock = (signature) => v7ContractSource.split(signature)[1]?.split('@sp.entrypoint')[0] || '';

  it('has v7 deploy profile wired in keeps CLI', () => {
    expect(keepsCliSource).toContain('const CONTRACT_PROFILES =');
    expect(keepsCliSource).toContain('v7:');
    expect(keepsCliSource).toContain("artifactKey: 'v7'");
    expect(keepsCliSource).toContain("KeepsFA2v7/step_002_cont_0_contract.tz");
    expect(keepsCliSource).toContain("version: '7.0.0'");
    expect(keepsCliSource).toContain("resolveContractProfile(rawProfile = 'v7')");
    expect(keepsCliSource).toContain("production: 'v7'");
  });

  it('supports v7 as compile target and default in compile.fish', () => {
    expect(compileSource).toContain('./compile.fish            # default: v7');
    expect(compileSource).toContain('./compile.fish v7');
    expect(compileSource).toContain('set -l target v7');
    expect(compileSource).toContain('case v7');
    expect(compileSource).toContain('set source_file kidlisp_keeps_fa2_v7.py');
    expect(compileSource).toContain('set output_dir KeepsFA2v7');
  });

  it('stores both metadata pointer keys during keep', () => {
    const keepBlock = entrypointBlock('def keep(self, params):');
    expect(keepBlock).toContain('"metadata_uri": params.metadata_uri');
    expect(keepBlock).toContain('"": params.metadata_uri');
  });

  it('uses owner-or-creator auth with refresh-only creator path in edit_metadata', () => {
    const editBlock = entrypointBlock('def edit_metadata(self, params):');

    expect(editBlock).toContain('is_owner = self.data.ledger.get(params.token_id');
    expect(editBlock).toContain('is_creator = self.data.token_creators.get(params.token_id');
    expect(editBlock).toContain('assert is_owner or is_creator, "NOT_AUTHORIZED"');

    expect(editBlock).toContain('if is_owner:');
    expect(editBlock).toContain('mutable_refresh_fields = [');
    expect(editBlock).toContain('"artifactUri"');
    expect(editBlock).toContain('"displayUri"');
    expect(editBlock).toContain('"thumbnailUri"');
    expect(editBlock).toContain('"metadata_uri"');
    expect(editBlock).toContain('if params.token_info.contains("metadata_uri")');
    expect(editBlock).toContain('if params.token_info.contains("")');
  });

  it('keeps content_hash and royalties immutable in v7 edit_metadata', () => {
    const editBlock = entrypointBlock('def edit_metadata(self, params):');
    expect(editBlock).toContain('original_hash = existing_info.get("content_hash"');
    expect(editBlock).toContain('original_royalties = existing_info.get("royalties"');
    expect(editBlock).toContain('token_info["content_hash"] = original_hash');
    expect(editBlock).toContain('token_info["royalties"] = original_royalties');
  });

  it('preserves FA2 compatibility and bypass guards in compiled artifact', () => {
    const expectedEntrypoints = [
      '%keep',
      '%edit_metadata',
      '%lock_metadata',
      '%set_contract_metadata',
      '%lock_contract_metadata',
      '%set_keep_fee',
      '%withdraw_fees',
      '%burn_keep',
      '%pause',
      '%unpause',
      '%set_default_royalty',
      '%admin_transfer',
      '%transfer',
      '%balance_of',
      '%update_operators',
      '%set_administrator',
      '%mint',
      '%burn',
    ];

    expectedEntrypoints.forEach((entrypoint) => expect(v7CompiledTzSource).toContain(entrypoint));
    expect(v7CompiledTzSource).toContain('MINT_DISABLED_USE_KEEP');
    expect(v7CompiledTzSource).toContain('BURN_DISABLED_USE_BURN_KEEP');
    expect(v7CompiledTzSource).toContain('NOT_AUTHORIZED');
  });

  it('keeps burn_keep owner-only', () => {
    const burnBlock = entrypointBlock('def burn_keep(self, token_id):');
    expect(burnBlock).toContain('assert current_owner == sp.sender, "NOT_TOKEN_OWNER"');
    expect(burnBlock).not.toContain('FA2_NOT_ADMIN');
  });
});
