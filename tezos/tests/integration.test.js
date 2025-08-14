/**
 * Integration Tests for KidLisp Meme Coin
 */

import { test, describe } from 'node:test';
import assert from 'node:assert';
import KidLispMemeCoinClient, { AestheticComputerIntegration } from '../src/index.js';

describe('KidLisp Meme Coin Integration', () => {
  test('should hash KidLisp code consistently', () => {
    const code1 = '(wipe blue)(line 10 10 50 50)';
    const code2 = '(wipe blue)(line 10 10 50 50)';
    const code3 = '(wipe red)(line 10 10 50 50)';
    
    const hash1 = KidLispMemeCoinClient.hashKidLispCode(code1);
    const hash2 = KidLispMemeCoinClient.hashKidLispCode(code2);
    const hash3 = KidLispMemeCoinClient.hashKidLispCode(code3);
    
    assert.strictEqual(hash1, hash2, 'Same code should produce same hash');
    assert.notStrictEqual(hash1, hash3, 'Different code should produce different hash');
    assert.strictEqual(hash1.length, 64, 'Hash should be 64 characters (SHA-256)');
  });

  test('should validate code minting criteria', () => {
    const validCode = '(wipe blue)(line 10 10 50 50)';
    const tooShort = '(wipe)';
    const onlyComments = '; This is just a comment\n; Another comment';
    const noFunctionCall = 'just some text';
    
    assert.strictEqual(AestheticComputerIntegration.shouldMintToken(validCode), true);
    assert.strictEqual(AestheticComputerIntegration.shouldMintToken(tooShort), false);
    assert.strictEqual(AestheticComputerIntegration.shouldMintToken(onlyComments), false);
    assert.strictEqual(AestheticComputerIntegration.shouldMintToken(noFunctionCall), false);
  });

  test('should generate consistent wallet addresses', async () => {
    const handle1 = 'testuser';
    const userSub1 = 'auth0|123456789';
    
    const wallet1a = await AestheticComputerIntegration.generateUserWallet(handle1, userSub1);
    const wallet1b = await AestheticComputerIntegration.generateUserWallet(handle1, userSub1);
    
    assert.strictEqual(wallet1a.privateKey, wallet1b.privateKey, 'Same user should get same private key');
    assert.strictEqual(wallet1a.address, wallet1b.address, 'Same user should get same address');
    
    const wallet2 = await AestheticComputerIntegration.generateUserWallet('otheruser', userSub1);
    assert.notStrictEqual(wallet1a.address, wallet2.address, 'Different users should get different addresses');
  });

  test('should create valid metadata URIs', () => {
    const codeHash = 'a1b2c3d4e5f6789012345678901234567890123456789012345678901234567890';
    const code = '(wipe blue)(line 10 10 50 50)';
    const creator = 'testuser';
    
    const uri = AestheticComputerIntegration.createMetadataUri(codeHash, code, creator);
    
    assert.ok(uri.startsWith('https://'), 'URI should be HTTPS');
    assert.ok(uri.includes(codeHash), 'URI should contain code hash');
  });
});

describe('Wallet Generation', () => {
  test('should generate deterministic wallets', async () => {
    const testCases = [
      { handle: 'alice', userSub: 'auth0|111' },
      { handle: 'bob', userSub: 'auth0|222' },
      { handle: 'charlie', userSub: 'auth0|333' }
    ];
    
    for (const testCase of testCases) {
      const wallet1 = await AestheticComputerIntegration.generateUserWallet(testCase.handle, testCase.userSub);
      const wallet2 = await AestheticComputerIntegration.generateUserWallet(testCase.handle, testCase.userSub);
      
      assert.strictEqual(wallet1.privateKey, wallet2.privateKey, `Wallet for ${testCase.handle} should be deterministic`);
      assert.ok(wallet1.privateKey.length > 0, 'Private key should not be empty');
      assert.ok(wallet1.address.length > 0, 'Address should not be empty');
    }
  });
});

describe('Code Hash System', () => {
  test('should handle edge cases in code hashing', () => {
    const testCases = [
      { code: '(wipe)', expected: 'should hash minimal code' },
      { code: '  (wipe blue)  \n\n  ', expected: 'should trim whitespace' },
      { code: '(wipe blue)(line 1 2 3 4)(circle 5)', expected: 'should hash complex code' },
      { code: '(def x 10)(later draw (line x x 20 20))', expected: 'should hash code with variables' }
    ];
    
    for (const testCase of testCases) {
      const hash = KidLispMemeCoinClient.hashKidLispCode(testCase.code);
      assert.strictEqual(hash.length, 64, `${testCase.expected} - hash should be 64 chars`);
      assert.ok(/^[a-f0-9]+$/.test(hash), `${testCase.expected} - hash should be hexadecimal`);
    }
  });

  test('should detect duplicate vs unique code', () => {
    const baseCode = '(wipe blue)(line 10 10 50 50)';
    const variations = [
      '(wipe blue)(line 10 10 50 50)', // exact same
      ' (wipe blue)(line 10 10 50 50) ', // with whitespace
      '(wipe blue)(line 10 10 50 50)\n', // with newline
      '(wipe red)(line 10 10 50 50)', // different color
      '(wipe blue)(line 20 20 50 50)' // different coordinates
    ];
    
    const baseHash = KidLispMemeCoinClient.hashKidLispCode(baseCode);
    
    // First 3 should be same (whitespace trimmed)
    assert.strictEqual(KidLispMemeCoinClient.hashKidLispCode(variations[0]), baseHash);
    assert.strictEqual(KidLispMemeCoinClient.hashKidLispCode(variations[1]), baseHash);
    assert.strictEqual(KidLispMemeCoinClient.hashKidLispCode(variations[2]), baseHash);
    
    // Last 2 should be different
    assert.notStrictEqual(KidLispMemeCoinClient.hashKidLispCode(variations[3]), baseHash);
    assert.notStrictEqual(KidLispMemeCoinClient.hashKidLispCode(variations[4]), baseHash);
  });
});
