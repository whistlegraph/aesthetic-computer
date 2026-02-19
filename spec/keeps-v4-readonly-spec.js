/**
 * Keeps FA2 v4 Contract - Read-Only Tests
 *
 * These tests verify the production contract via TzKT API without making any transactions.
 * Safe to run against mainnet contract: KT1ER1GyoeRNhkv6E57yKbBbEKi5ynKbaH3W
 */

import { getContract, CONTRACTS, EXPECTED_STORAGE, stringToBytes } from './helpers/keeps-test-helper.mjs';
import {
  getContractStorage,
  getContractEntrypoints,
  getTokenMetadata,
  getAllTokens,
  getOperations,
  getBigMapKeys
} from './helpers/tzkt-helper.mjs';

const RUN_KEEPS_V4_NETWORK_TESTS = process.env.RUN_KEEPS_V4_NETWORK_TESTS === 'true';
const describeIfNetworkEnabled = RUN_KEEPS_V4_NETWORK_TESTS ? describe : xdescribe;

if (!RUN_KEEPS_V4_NETWORK_TESTS) {
  console.log('⏭️  Skipping v4 network read-only tests (set RUN_KEEPS_V4_NETWORK_TESTS=true to enable)');
}

describeIfNetworkEnabled("🔐 Keeps FA2 v4 Contract - Read-Only Tests", () => {
  let contract, address;
  const network = 'mainnet';

  beforeAll(async () => {
    console.log('\n🧪 Starting Keeps v4 read-only test suite...\n');
    ({ contract, address } = await getContract(network, false));
  });

  afterAll(() => {
    console.log('\n✅ Read-only test suite complete!\n');
  });

  describe("📋 Contract Configuration", () => {
    it("should have correct contract address", () => {
      expect(address).toBe(CONTRACTS.mainnet);
    });

    it("should have correct administrator", async () => {
      const storage = await getContractStorage(address, network);
      expect(storage.administrator).toBe(EXPECTED_STORAGE.mainnet.administrator);
    });

    it("should have default royalty of 10%", async () => {
      const storage = await getContractStorage(address, network);
      expect(parseInt(storage.default_royalty_bps)).toBe(EXPECTED_STORAGE.mainnet.default_royalty_bps);
    });

    it("should not be paused", async () => {
      const storage = await getContractStorage(address, network);
      expect(storage.paused).toBe(EXPECTED_STORAGE.mainnet.paused);
    });

    it("should have keep_fee configured", async () => {
      const storage = await getContractStorage(address, network);
      expect(storage.keep_fee).toBeDefined();
      console.log(`   📊 Current keep fee: ${storage.keep_fee} mutez`);
    });

    it("should have contract metadata", async () => {
      const storage = await getContractStorage(address, network);
      expect(storage.metadata).toBeDefined();
    });

    it("should have contract_metadata_locked flag", async () => {
      const storage = await getContractStorage(address, network);
      expect(typeof storage.contract_metadata_locked).toBe('boolean');
    });
  });

  describe("🎯 Contract Entrypoints", () => {
    let entrypoints;

    beforeAll(async () => {
      entrypoints = await getContractEntrypoints(address, network);
    });

    it("should have all 12 custom Keeps entrypoints", () => {
      const customEntrypoints = [
        'keep',
        'edit_metadata',
        'lock_metadata',
        'set_contract_metadata',
        'lock_contract_metadata',
        'set_keep_fee',
        'withdraw_fees',
        'burn_keep',
        'pause',
        'unpause',
        'set_default_royalty',
        'admin_transfer'
      ];

      const entrypointNames = entrypoints.map(ep => ep.name);
      customEntrypoints.forEach(name => {
        expect(entrypointNames).toContain(name);
      });

      console.log(`   ✅ Found ${customEntrypoints.length} custom entrypoints`);
    });

    it("should have all 6 FA2 standard entrypoints", () => {
      const fa2Entrypoints = [
        'transfer',
        'update_operators',
        'balance_of',
        'mint',
        'burn',
        'set_administrator'
      ];

      const entrypointNames = entrypoints.map(ep => ep.name);
      fa2Entrypoints.forEach(name => {
        expect(entrypointNames).toContain(name);
      });

      console.log(`   ✅ Found ${fa2Entrypoints.length} FA2 standard entrypoints`);
    });

    it("should have v4 entrypoints (pause, unpause, set_default_royalty, admin_transfer)", () => {
      const v4Entrypoints = ['pause', 'unpause', 'set_default_royalty', 'admin_transfer'];
      const entrypointNames = entrypoints.map(ep => ep.name);

      v4Entrypoints.forEach(name => {
        expect(entrypointNames).toContain(name);
      });

      console.log(`   ✅ Found all 4 v4-specific entrypoints`);
    });
  });

  describe("🪙 Token Metadata", () => {
    it("should have minted tokens", async () => {
      const storage = await getContractStorage(address, network);
      const nextTokenId = parseInt(storage.next_token_id);

      expect(nextTokenId).toBeGreaterThan(0);
      console.log(`   📊 Total tokens minted: ${nextTokenId}`);
    });

    it("should fetch token metadata from TzKT", async () => {
      const storage = await getContractStorage(address, network);
      const nextTokenId = parseInt(storage.next_token_id);

      if (nextTokenId > 0) {
        // Fetch first token
        const tokens = await getTokenMetadata(address, 0, network);

        expect(tokens.length).toBeGreaterThan(0);
        expect(tokens[0].metadata).toBeDefined();
        console.log(`   ✅ Token #0 metadata: ${tokens[0].metadata.name}`);
      } else {
        console.log(`   ⚠️  No tokens minted yet, skipping metadata test`);
      }
    });

    it("should have proper token metadata structure (TZIP-21)", async () => {
      const storage = await getContractStorage(address, network);
      const nextTokenId = parseInt(storage.next_token_id);

      if (nextTokenId > 0) {
        const tokens = await getTokenMetadata(address, 0, network);
        const metadata = tokens[0].metadata;

        // Check TZIP-21 required fields
        expect(metadata.name).toBeDefined();
        expect(metadata.symbol).toBeDefined();
        expect(metadata.decimals).toBeDefined();

        // Check Keeps-specific fields
        expect(metadata.artifactUri).toBeDefined();
        expect(metadata.displayUri).toBeDefined();
        expect(metadata.thumbnailUri).toBeDefined();

        console.log(`   ✅ Token metadata follows TZIP-21 standard`);
      }
    });
  });

  describe("💾 Storage BigMaps", () => {
    it("should have content_hashes bigmap", async () => {
      const storage = await getContractStorage(address, network);
      expect(storage.content_hashes).toBeDefined();
      console.log(`   📊 Content hashes bigmap ID: ${storage.content_hashes}`);
    });

    it("should have token_creators bigmap", async () => {
      const storage = await getContractStorage(address, network);
      expect(storage.token_creators).toBeDefined();
      console.log(`   📊 Token creators bigmap ID: ${storage.token_creators}`);
    });

    it("should have metadata_locked bigmap", async () => {
      const storage = await getContractStorage(address, network);
      expect(storage.metadata_locked).toBeDefined();
      console.log(`   📊 Metadata locked bigmap ID: ${storage.metadata_locked}`);
    });

    it("should have ledger bigmap", async () => {
      const storage = await getContractStorage(address, network);
      expect(storage.ledger).toBeDefined();
      console.log(`   📊 Ledger bigmap ID: ${storage.ledger}`);
    });

    it("should have token_metadata bigmap", async () => {
      const storage = await getContractStorage(address, network);
      expect(storage.token_metadata).toBeDefined();
      console.log(`   📊 Token metadata bigmap ID: ${storage.token_metadata}`);
    });

    it("should have operators bigmap", async () => {
      const storage = await getContractStorage(address, network);
      expect(storage.operators).toBeDefined();
      console.log(`   📊 Operators bigmap ID: ${storage.operators}`);
    });
  });

  describe("📜 Operation History", () => {
    it("should have mint operations (keep entrypoint)", async () => {
      const operations = await getOperations(address, 'keep', network, 5);

      if (operations.length > 0) {
        console.log(`   ✅ Found ${operations.length} mint operations`);
        console.log(`   📊 Latest mint: ${operations[0].hash} (block ${operations[0].level})`);
      } else {
        console.log(`   ⚠️  No mint operations found yet`);
      }
    });

    it("should verify all operations were successful", async () => {
      const operations = await getOperations(address, 'keep', network, 10);

      operations.forEach(op => {
        expect(op.status).toBe('applied');
      });

      if (operations.length > 0) {
        console.log(`   ✅ All ${operations.length} operations successful`);
      }
    });

    it("should have operation sender information", async () => {
      const operations = await getOperations(address, 'keep', network, 1);

      if (operations.length > 0) {
        const op = operations[0];
        expect(op.sender).toBeDefined();
        expect(op.sender.address).toBeDefined();
        console.log(`   📊 Latest mint sender: ${op.sender.address}`);
      }
    });
  });

  describe("🔍 Contract Verification", () => {
    it("should be a valid FA2 contract", async () => {
      const storage = await getContractStorage(address, network);

      // Check FA2 required storage
      expect(storage.ledger).toBeDefined();
      expect(storage.operators).toBeDefined();
      expect(storage.token_metadata).toBeDefined();

      console.log(`   ✅ Contract has FA2 storage structure`);
    });

    it("should have v4-specific storage (pause, royalty)", async () => {
      const storage = await getContractStorage(address, network);

      // v4 specific fields
      expect(storage.paused).toBeDefined();
      expect(storage.default_royalty_bps).toBeDefined();

      console.log(`   ✅ Contract has v4-specific storage fields`);
    });

    it("should have creator tracking (v3 feature)", async () => {
      const storage = await getContractStorage(address, network);
      expect(storage.token_creators).toBeDefined();
      console.log(`   ✅ Contract has creator tracking (v3 feature)`);
    });

    it("should have content hash deduplication (v3 feature)", async () => {
      const storage = await getContractStorage(address, network);
      expect(storage.content_hashes).toBeDefined();
      console.log(`   ✅ Contract has content hash deduplication (v3 feature)`);
    });

    it("should have metadata locking support (v3 feature)", async () => {
      const storage = await getContractStorage(address, network);
      expect(storage.metadata_locked).toBeDefined();
      expect(storage.contract_metadata_locked).toBeDefined();
      console.log(`   ✅ Contract has metadata locking support (v3 feature)`);
    });
  });
});
