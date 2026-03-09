/**
 * Keeps v9 Production - Source Invariants
 *
 * Offline checks for v9 rollout semantics:
 * - deploy tooling supports v9 profile/artifacts
 * - keep requires signed backend permits
 * - owner/creator metadata policy is preserved
 * - immutable metadata fields remain protected
 */

import fs from 'node:fs';
import path from 'node:path';

describe('🚀 Keeps v9 Production - Source Checks', () => {
  const keepsCliPath = path.join(process.cwd(), 'tezos', 'keeps.mjs');
  const compilePath = path.join(process.cwd(), 'tezos', 'compile.fish');
  const v9ContractPath = path.join(process.cwd(), 'tezos', 'kidlisp_keeps_fa2_v9.py');
  const v9CompiledTzPath = path.join(process.cwd(), 'tezos', 'KeepsFA2v9', 'step_002_cont_0_contract.tz');

  let keepsCliSource;
  let compileSource;
  let v9ContractSource;
  let v9CompiledTzSource;

  beforeAll(() => {
    keepsCliSource = fs.readFileSync(keepsCliPath, 'utf8');
    compileSource = fs.readFileSync(compilePath, 'utf8');
    v9ContractSource = fs.readFileSync(v9ContractPath, 'utf8');
    v9CompiledTzSource = fs.readFileSync(v9CompiledTzPath, 'utf8');
  });

  const entrypointBlock = (signature) => v9ContractSource.split(signature)[1]?.split('@sp.entrypoint')[0] || '';

  it('has v9 deploy profile wired in keeps CLI', () => {
    expect(keepsCliSource).toContain('const CONTRACT_PROFILES =');
    expect(keepsCliSource).toContain('v9:');
    expect(keepsCliSource).toContain("artifactKey: 'v9'");
    expect(keepsCliSource).toContain("KeepsFA2v9/step_002_cont_0_contract.tz");
    expect(keepsCliSource).toContain("version: '9.0.0'");
    expect(keepsCliSource).toContain("resolveContractProfile(rawProfile = 'v9')");
    expect(keepsCliSource).toContain("production: 'v9'");
  });

  it('supports v9 as compile target and default in compile.fish', () => {
    expect(compileSource).toContain('./compile.fish            # default: v9');
    expect(compileSource).toContain('./compile.fish v9');
    expect(compileSource).toContain('set -l target v9');
    expect(compileSource).toContain('case v9');
    expect(compileSource).toContain('set source_file kidlisp_keeps_fa2_v9.py');
    expect(compileSource).toContain('set output_dir KeepsFA2v9');
  });

  it('requires signed keep permits in keep entrypoint', () => {
    const keepBlock = entrypointBlock('def keep(self, params):');
    expect(keepBlock).toContain('permit_deadline=sp.timestamp');
    expect(keepBlock).toContain('keep_permit=sp.signature');
    expect(keepBlock).toContain('assert sp.now <= params.permit_deadline, "PERMIT_EXPIRED"');
    expect(keepBlock).toContain('assert sp.check_signature(');
    expect(keepBlock).toContain('KEEP_PERMIT_SIGNER');
    expect(keepBlock).toContain('), "INVALID_KEEP_PERMIT"');
  });

  it('keeps fee + self-mint enforcement for non-admin callers', () => {
    const keepBlock = entrypointBlock('def keep(self, params):');
    expect(keepBlock).toContain('assert sp.amount >= self.data.keep_fee, "INSUFFICIENT_FEE"');
    expect(keepBlock).toContain('assert params.owner == sp.sender, "MUST_MINT_TO_SELF"');
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

  it('keeps content_hash and royalties immutable in v9 edit_metadata', () => {
    const editBlock = entrypointBlock('def edit_metadata(self, params):');
    expect(editBlock).toContain('original_hash = existing_info.get("content_hash"');
    expect(editBlock).toContain('original_royalties = existing_info.get("royalties"');
    expect(editBlock).toContain('token_info["content_hash"] = original_hash');
    expect(editBlock).toContain('token_info["royalties"] = original_royalties');
  });

  it('keeps burn_keep owner-only', () => {
    const burnBlock = entrypointBlock('def burn_keep(self, token_id):');
    expect(burnBlock).toContain('assert current_owner == sp.sender, "NOT_TOKEN_OWNER"');
    expect(burnBlock).not.toContain('FA2_NOT_ADMIN');
  });

  it('preserves FA2 compatibility in compiled v9 artifact', () => {
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

    expectedEntrypoints.forEach((entrypoint) => expect(v9CompiledTzSource).toContain(entrypoint));
    expect(v9CompiledTzSource).toContain('INVALID_KEEP_PERMIT');
    expect(v9CompiledTzSource).toContain('MUST_MINT_TO_SELF');
    expect(v9CompiledTzSource).toContain('NOT_TOKEN_OWNER');
    expect(v9CompiledTzSource).toContain('NOT_AUTHORIZED');
    expect(v9CompiledTzSource).toContain('FA2_NOT_ADMIN');
  });
});
