/**
 * Keeps v6 Launch Prep - Source Invariants
 *
 * Offline checks to keep launch-prep assumptions stable:
 * - v6 metadata policy is reflected in live mint/update code paths
 * - deploy tooling supports v6 profile with production defaults
 */

import fs from 'node:fs';
import path from 'node:path';

describe('🚀 Keeps v6 Launch Prep - Source Checks', () => {
  const keepMintPath = path.join(process.cwd(), 'system', 'netlify', 'functions', 'keep-mint.mjs');
  const keepUpdatePath = path.join(process.cwd(), 'system', 'netlify', 'functions', 'keep-update.mjs');
  const keepUpdateConfirmPath = path.join(process.cwd(), 'system', 'netlify', 'functions', 'keep-update-confirm.mjs');
  const kidlispKeepPath = path.join(process.cwd(), 'system', 'netlify', 'functions', 'kidlisp-keep.mjs');
  const keepsCliPath = path.join(process.cwd(), 'tezos', 'keeps.mjs');
  const compilePath = path.join(process.cwd(), 'tezos', 'compile.fish');
  const v6ContractPath = path.join(process.cwd(), 'tezos', 'kidlisp_keeps_fa2_v6.py');

  let keepMintSource;
  let keepUpdateSource;
  let keepUpdateConfirmSource;
  let kidlispKeepSource;
  let keepsCliSource;
  let compileSource;
  let v6ContractSource;

  beforeAll(() => {
    keepMintSource = fs.readFileSync(keepMintPath, 'utf8');
    keepUpdateSource = fs.readFileSync(keepUpdatePath, 'utf8');
    keepUpdateConfirmSource = fs.readFileSync(keepUpdateConfirmPath, 'utf8');
    kidlispKeepSource = fs.readFileSync(kidlispKeepPath, 'utf8');
    keepsCliSource = fs.readFileSync(keepsCliPath, 'utf8');
    compileSource = fs.readFileSync(compilePath, 'utf8');
    v6ContractSource = fs.readFileSync(v6ContractPath, 'utf8');
  });

  it('enforces KidLisp-only tags in mint/update pipelines', () => {
    expect(keepMintSource).toContain('const tags = ["KidLisp"]');
    expect(keepUpdateSource).toContain('const tags = ["KidLisp"]');
  });

  it('uses v6 attribute naming and removes analyzer metadata fields', () => {
    expect(keepMintSource).toContain('{ name: "Handle", value:');
    expect(keepMintSource).toContain('{ name: "User", value:');
    expect(keepMintSource).toContain('{ name: "Packed on", value:');

    expect(keepMintSource).not.toContain('Analyzer Version');
    expect(keepMintSource).not.toContain('Author Handle');
    expect(keepMintSource).not.toContain('Author Code');
    expect(keepUpdateSource).not.toContain('Analyzer Version');
  });

  it('has deploy profile support for v6 production metadata and fee defaults', () => {
    expect(keepsCliSource).toContain('const CONTRACT_PROFILES =');
    expect(keepsCliSource).toContain("v6:");
    expect(keepsCliSource).toContain("artifactKey: 'v6'");
    expect(keepsCliSource).toContain("KeepsFA2v6/step_002_cont_0_contract.tz");
    expect(keepsCliSource).toContain("name: 'KidLisp'");
    expect(keepsCliSource).toContain("version: '6.0.0'");
    expect(keepsCliSource).toContain("description: 'https://keeps.kidlisp.com'");
    expect(keepsCliSource).toContain('keepFeeMutez: 2_500_000');
    expect(keepsCliSource).toContain("await deployContract(getNetwork(1), { contractProfile })");
  });

  it('includes staging deprecation housekeeping workflow in keeps CLI', () => {
    expect(keepsCliSource).toContain("async function deprecateStagingContracts");
    expect(keepsCliSource).toContain("case 'deprecate-staging'");
    expect(keepsCliSource).toContain('--replacement=<KT1...>');
    expect(keepsCliSource).toContain('--addresses=<KT1,KT1,...>');
  });

  it('supports dedicated v6 compile target in compile.fish', () => {
    expect(compileSource).toContain('./compile.fish v6');
    expect(compileSource).toContain('case v6');
    expect(compileSource).toContain('set source_file kidlisp_keeps_fa2_v6.py');
    expect(compileSource).toContain('set output_dir KeepsFA2v6');
  });

  it('keeps royalties immutable in v6 contract edit_metadata', () => {
    expect(v6ContractSource).toContain('original_royalties = existing_info.get("royalties"');
    expect(v6ContractSource).toContain('token_info["royalties"] = original_royalties');
  });

  it('restricts v6 edit_metadata to token owner only', () => {
    const editBlock = v6ContractSource.split('def edit_metadata(self, params):')[1]?.split('@sp.entrypoint')[0] || '';
    expect(editBlock).toContain('is_owner = self.data.ledger.get(params.token_id');
    expect(editBlock).toContain('assert is_owner, "NOT_TOKEN_OWNER"');
    expect(editBlock).not.toContain('is_admin = self.is_administrator_()');
    expect(editBlock).not.toContain('is_creator = self.data.token_creators.get(params.token_id');
    expect(editBlock).not.toContain('NOT_AUTHORIZED');
  });

  it('restricts v6 burn_keep to token owner (not admin)', () => {
    const burnBlock = v6ContractSource.split('def burn_keep(self, token_id):')[1]?.split('# =====================================================================')[0] || '';
    expect(burnBlock).toContain('assert current_owner == sp.sender, "NOT_TOKEN_OWNER"');
    expect(burnBlock).not.toContain('FA2_NOT_ADMIN');
  });

  it('uses contract default_royalty_bps in mint pipeline and preserves royalties on update', () => {
    expect(keepMintSource).toContain('storage.default_royalty_bps');
    expect(keepMintSource).toContain('const royalties = buildRoyalties(');
    expect(keepUpdateSource).toContain('parseRoyaltiesFromHex');
    expect(keepUpdateSource).toContain('preservedRoyalties || buildRoyalties(');
  });

  it('resolves keeps contract address from Mongo secrets (not Netlify env var)', () => {
    expect(keepMintSource).toContain('getKeepsContractAddress');
    expect(keepUpdateSource).toContain('getKeepsContractAddress');
    expect(keepUpdateConfirmSource).toContain('getKeepsContractAddress');
    expect(kidlispKeepSource).toContain('getKeepsContractAddress');

    expect(keepMintSource).not.toContain('TEZOS_KEEPS_CONTRACT');
    expect(keepUpdateSource).not.toContain('TEZOS_KEEPS_CONTRACT');
    expect(keepUpdateConfirmSource).not.toContain('TEZOS_KEEPS_CONTRACT');
    expect(kidlispKeepSource).not.toContain('TEZOS_KEEPS_CONTRACT');
  });
});
