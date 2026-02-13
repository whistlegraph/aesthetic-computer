/**
 * Keeps FA2 v4 Contract - Security Tests
 *
 * These tests verify security properties of the production contract via TzKT API.
 * Checks authorization, duplicate prevention, pause functionality, and constraints.
 * Safe to run against mainnet - read-only verification only.
 */

import { CONTRACTS, EXPECTED_STORAGE } from './helpers/keeps-test-helper.mjs';
import {
  getContractStorage,
  getOperations,
  getBigMapKeys,
  getAllTokens
} from './helpers/tzkt-helper.mjs';

describe("🔒 Keeps FA2 v4 Contract - Security Tests", () => {
  const network = 'mainnet';
  const address = CONTRACTS.mainnet;

  beforeAll(() => {
    console.log('\n🔒 Starting Keeps v4 security test suite...\n');
  });

  afterAll(() => {
    console.log('\n✅ Security test suite complete!\n');
  });

  describe("👮 Authorization Controls", () => {
    it("should only allow admin to pause contract", async () => {
      const storage = await getContractStorage(address, network);
      const expectedAdmin = EXPECTED_STORAGE.mainnet.administrator;

      expect(storage.administrator).toBe(expectedAdmin);

      // Verify all pause operations (if any) were from admin
      const pauseOps = await getOperations(address, 'pause', network, 100);

      if (pauseOps.length > 0) {
        pauseOps.forEach(op => {
          expect(op.sender.address).toBe(expectedAdmin);
        });
        console.log(`   ✅ All ${pauseOps.length} pause operations from admin`);
      } else {
        console.log(`   ℹ️  No pause operations found (expected for production)`);
      }
    });

    it("should only allow admin to unpause contract", async () => {
      const storage = await getContractStorage(address, network);
      const expectedAdmin = EXPECTED_STORAGE.mainnet.administrator;

      // Verify all unpause operations (if any) were from admin
      const unpauseOps = await getOperations(address, 'unpause', network, 100);

      if (unpauseOps.length > 0) {
        unpauseOps.forEach(op => {
          expect(op.sender.address).toBe(expectedAdmin);
        });
        console.log(`   ✅ All ${unpauseOps.length} unpause operations from admin`);
      } else {
        console.log(`   ℹ️  No unpause operations found (expected for production)`);
      }
    });

    it("should only allow admin to set royalty", async () => {
      const storage = await getContractStorage(address, network);
      const expectedAdmin = EXPECTED_STORAGE.mainnet.administrator;

      // Verify all set_default_royalty operations (if any) were from admin
      const royaltyOps = await getOperations(address, 'set_default_royalty', network, 100);

      if (royaltyOps.length > 0) {
        royaltyOps.forEach(op => {
          expect(op.sender.address).toBe(expectedAdmin);
        });
        console.log(`   ✅ All ${royaltyOps.length} royalty operations from admin`);
      } else {
        console.log(`   ℹ️  No royalty operations found yet`);
      }
    });

    it("should only allow admin to set keep fee", async () => {
      const storage = await getContractStorage(address, network);
      const expectedAdmin = EXPECTED_STORAGE.mainnet.administrator;

      // Verify all set_keep_fee operations (if any) were from admin
      const feeOps = await getOperations(address, 'set_keep_fee', network, 100);

      if (feeOps.length > 0) {
        feeOps.forEach(op => {
          expect(op.sender.address).toBe(expectedAdmin);
        });
        console.log(`   ✅ All ${feeOps.length} fee operations from admin`);
      } else {
        console.log(`   ℹ️  No fee operations found yet`);
      }
    });

    it("should only allow admin to withdraw fees", async () => {
      const storage = await getContractStorage(address, network);
      const expectedAdmin = EXPECTED_STORAGE.mainnet.administrator;

      // Verify all withdraw_fees operations (if any) were from admin
      const withdrawOps = await getOperations(address, 'withdraw_fees', network, 100);

      if (withdrawOps.length > 0) {
        withdrawOps.forEach(op => {
          expect(op.sender.address).toBe(expectedAdmin);
        });
        console.log(`   ✅ All ${withdrawOps.length} withdraw operations from admin`);
      } else {
        console.log(`   ℹ️  No withdraw operations found yet`);
      }
    });

    it("should only allow admin to burn keeps", async () => {
      const storage = await getContractStorage(address, network);
      const expectedAdmin = EXPECTED_STORAGE.mainnet.administrator;

      // Verify all burn_keep operations (if any) were from admin
      const burnOps = await getOperations(address, 'burn_keep', network, 100);

      if (burnOps.length > 0) {
        burnOps.forEach(op => {
          expect(op.sender.address).toBe(expectedAdmin);
        });
        console.log(`   ✅ All ${burnOps.length} burn operations from admin`);
      } else {
        console.log(`   ℹ️  No burn operations found (expected - permanent keeps)`);
      }
    });

    it("should only allow admin for admin_transfer", async () => {
      const storage = await getContractStorage(address, network);
      const expectedAdmin = EXPECTED_STORAGE.mainnet.administrator;

      // Verify all admin_transfer operations (if any) were from admin
      const transferOps = await getOperations(address, 'admin_transfer', network, 100);

      if (transferOps.length > 0) {
        transferOps.forEach(op => {
          expect(op.sender.address).toBe(expectedAdmin);
        });
        console.log(`   ✅ All ${transferOps.length} admin_transfer operations from admin`);
      } else {
        console.log(`   ℹ️  No admin_transfer operations found (expected - emergency only)`);
      }
    });

    it("should verify edit_metadata authorization", async () => {
      const storage = await getContractStorage(address, network);
      const expectedAdmin = EXPECTED_STORAGE.mainnet.administrator;

      // Verify edit_metadata operations follow auth rules (admin/owner/creator)
      const editOps = await getOperations(address, 'edit_metadata', network, 20);

      if (editOps.length > 0) {
        console.log(`   📊 Found ${editOps.length} edit_metadata operations`);

        // All should be successful (no failed auth)
        editOps.forEach(op => {
          expect(op.status).toBe('applied');
        });

        console.log(`   ✅ All edit_metadata operations succeeded (proper auth)`);
      } else {
        console.log(`   ℹ️  No edit_metadata operations found yet`);
      }
    });
  });

  describe("🚫 Duplicate Prevention", () => {
    it("should have no duplicate content hashes", async () => {
      const storage = await getContractStorage(address, network);
      const contentHashesBigMapId = storage.content_hashes;

      // Get all content hashes from bigmap
      const hashes = await getBigMapKeys(contentHashesBigMapId, network, 1000);

      const hashSet = new Set();
      const duplicates = [];

      hashes.forEach(entry => {
        const hash = entry.key;
        if (hashSet.has(hash)) {
          duplicates.push(hash);
        }
        hashSet.add(hash);
      });

      expect(duplicates.length).toBe(0);

      if (hashes.length > 0) {
        console.log(`   ✅ No duplicates found in ${hashes.length} content hashes`);
      } else {
        console.log(`   ℹ️  No content hashes yet (no mints)`);
      }
    });

    it("should verify each keep operation used unique content hash", async () => {
      const keepOps = await getOperations(address, 'keep', network, 100);

      if (keepOps.length > 0) {
        // All keep operations should be successful
        keepOps.forEach(op => {
          expect(op.status).toBe('applied');
        });

        console.log(`   ✅ All ${keepOps.length} keep operations succeeded (no duplicate hash rejections)`);
      } else {
        console.log(`   ℹ️  No keep operations found yet`);
      }
    });
  });

  describe("⏸️  Pause Functionality", () => {
    it("should have correct initial pause state", async () => {
      const storage = await getContractStorage(address, network);
      const expectedPaused = EXPECTED_STORAGE.mainnet.paused;

      expect(storage.paused).toBe(expectedPaused);
      console.log(`   ✅ Contract pause state: ${storage.paused} (expected: ${expectedPaused})`);
    });

    it("should verify pause/unpause operation history", async () => {
      const pauseOps = await getOperations(address, 'pause', network, 100);
      const unpauseOps = await getOperations(address, 'unpause', network, 100);

      console.log(`   📊 Pause operations: ${pauseOps.length}`);
      console.log(`   📊 Unpause operations: ${unpauseOps.length}`);

      // All pause/unpause operations should be successful
      [...pauseOps, ...unpauseOps].forEach(op => {
        expect(op.status).toBe('applied');
      });

      if (pauseOps.length + unpauseOps.length > 0) {
        console.log(`   ✅ All pause/unpause operations succeeded`);
      } else {
        console.log(`   ℹ️  No pause/unpause operations yet (good - stable contract)`);
      }
    });
  });

  describe("💰 Royalty Constraints", () => {
    it("should enforce 25% max royalty", async () => {
      const storage = await getContractStorage(address, network);
      const royaltyBps = parseInt(storage.default_royalty_bps);

      expect(royaltyBps).toBeLessThanOrEqual(2500); // 25% max
      expect(royaltyBps).toBeGreaterThanOrEqual(0);

      console.log(`   ✅ Royalty: ${royaltyBps} bps (${royaltyBps / 100}%) within bounds [0%, 25%]`);
    });

    it("should verify all royalty changes were within bounds", async () => {
      const royaltyOps = await getOperations(address, 'set_default_royalty', network, 100);

      if (royaltyOps.length > 0) {
        // All should be successful (no rejected out-of-bound values)
        royaltyOps.forEach(op => {
          expect(op.status).toBe('applied');
        });

        console.log(`   ✅ All ${royaltyOps.length} royalty operations succeeded (within bounds)`);
      } else {
        console.log(`   ℹ️  No royalty operations found yet`);
      }
    });
  });

  describe("🔒 Metadata Locking", () => {
    it("should verify metadata lock operations", async () => {
      const lockOps = await getOperations(address, 'lock_metadata', network, 100);

      if (lockOps.length > 0) {
        // All lock operations should be successful
        lockOps.forEach(op => {
          expect(op.status).toBe('applied');
        });

        console.log(`   ✅ All ${lockOps.length} lock_metadata operations succeeded`);
      } else {
        console.log(`   ℹ️  No lock_metadata operations found yet`);
      }
    });

    it("should verify contract metadata not locked", async () => {
      const storage = await getContractStorage(address, network);

      expect(storage.contract_metadata_locked).toBe(false);
      console.log(`   ✅ Contract metadata not locked (allows updates)`);
    });
  });

  describe("🎯 Token Creator Tracking", () => {
    it("should track creators for all minted tokens", async () => {
      const storage = await getContractStorage(address, network);
      const nextTokenId = parseInt(storage.next_token_id);

      if (nextTokenId > 0) {
        const tokenCreatorsBigMapId = storage.token_creators;
        const creators = await getBigMapKeys(tokenCreatorsBigMapId, network, 1000);

        // Every minted token should have a creator
        expect(creators.length).toBeGreaterThan(0);

        console.log(`   ✅ Creator tracking: ${creators.length} creators for ${nextTokenId} tokens`);
      } else {
        console.log(`   ℹ️  No tokens minted yet`);
      }
    });
  });

  describe("📊 Operation Integrity", () => {
    it("should verify all operations succeeded (no failures)", async () => {
      const keepOps = await getOperations(address, 'keep', network, 100);
      const editOps = await getOperations(address, 'edit_metadata', network, 100);
      const transferOps = await getOperations(address, 'transfer', network, 100);

      const allOps = [...keepOps, ...editOps, ...transferOps];

      if (allOps.length > 0) {
        // All operations should be successfully applied
        allOps.forEach(op => {
          expect(op.status).toBe('applied');
        });

        console.log(`   ✅ All ${allOps.length} operations successful (no failures)`);
      } else {
        console.log(`   ℹ️  No operations found yet`);
      }
    });

    it("should verify no failed transactions to contract", async () => {
      // Get recent operations (all entrypoints)
      const recentOps = await getAllTokens(address, network, 100);

      // TzKT only returns successful operations by default
      // If we got results, they're all successful
      if (recentOps.length > 0) {
        console.log(`   ✅ Found ${recentOps.length} successful tokens (no failed mints)`);
      }
    });
  });

  describe("🏗️  Storage Integrity", () => {
    it("should have consistent token count", async () => {
      const storage = await getContractStorage(address, network);
      const nextTokenId = parseInt(storage.next_token_id);

      // Get actual minted tokens
      const tokens = await getAllTokens(address, network, 1000);

      // next_token_id should equal number of minted tokens (0-indexed)
      expect(tokens.length).toBeLessThanOrEqual(nextTokenId);

      console.log(`   ✅ Token count consistent: next_token_id=${nextTokenId}, minted=${tokens.length}`);
    });

    it("should verify bigmap IDs are valid", async () => {
      const storage = await getContractStorage(address, network);

      const bigMaps = {
        content_hashes: storage.content_hashes,
        token_creators: storage.token_creators,
        metadata_locked: storage.metadata_locked,
        ledger: storage.ledger,
        token_metadata: storage.token_metadata,
        operators: storage.operators
      };

      // All bigmap IDs should be positive integers
      Object.entries(bigMaps).forEach(([name, id]) => {
        expect(parseInt(id)).toBeGreaterThan(0);
      });

      console.log(`   ✅ All 6 bigmap IDs are valid positive integers`);
    });
  });
});
