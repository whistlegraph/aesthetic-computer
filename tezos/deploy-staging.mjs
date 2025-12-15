#!/usr/bin/env node
/**
 * deploy-staging.mjs - Deploy a staging contract to mainnet
 * 
 * Uses the staging wallet (not kidlisp/keeps.tez) for testing
 * Contract name is randomized to be unsearchable
 */

import { TezosToolkit } from '@taquito/taquito';
import { InMemorySigner } from '@taquito/signer';
import { Parser } from '@taquito/michel-codec';
import fs from 'fs';
import crypto from 'crypto';

// Generate random obscure name
const randomId = crypto.randomBytes(4).toString('hex');
const obscureName = 'dev_' + randomId;

console.log('\n╔══════════════════════════════════════════════════════════════╗');
console.log('║  🚀 Deploying Mainnet STAGING Keeps Contract                  ║');
console.log('╚══════════════════════════════════════════════════════════════╝\n');
console.log('Contract name:', obscureName);
console.log('(Obscured for staging - unsearchable)\n');

// Staging wallet credentials - MUST be set via environment variable
// Never commit private keys to the repository!
const stagingKey = process.env.TEZOS_STAGING_KEY;
const stagingAddress = process.env.TEZOS_STAGING_ADDRESS || 'tz1dfoQDuxjwSgxdqJnisyKUxDHweade4Gzt';

if (!stagingKey) {
  console.error('❌ Missing TEZOS_STAGING_KEY environment variable');
  console.error('   Set it with: export TEZOS_STAGING_KEY=edsk...');
  process.exit(1);
}

const tezos = new TezosToolkit('https://mainnet.api.tez.ie');
tezos.setSignerProvider(new InMemorySigner(stagingKey));

// Helper to convert string to hex bytes
function stringToBytes(str) {
  return Buffer.from(str, 'utf8').toString('hex');
}

async function deploy() {
  console.log('📡 Network: Mainnet');
  console.log('👤 Administrator:', stagingAddress);
  
  // Check balance
  const balance = await tezos.tz.getBalance(stagingAddress);
  const balanceXTZ = balance.toNumber() / 1_000_000;
  console.log('💰 Balance:', balanceXTZ.toFixed(2), 'XTZ\n');
  
  if (balanceXTZ < 1) {
    throw new Error('Need at least 1 XTZ for deployment');
  }
  
  // Check if public key is revealed (required for origination)
  try {
    const pkh = await tezos.rpc.getManagerKey(stagingAddress);
    if (!pkh) {
      console.log('🔑 Revealing public key (one-time operation)...');
      // Need to reveal first - done automatically by taquito on first operation
    }
  } catch (e) {
    console.log('🔑 Will reveal public key with deployment...');
  }
  
  // Load contract
  const contractSource = fs.readFileSync('./KeepsFA2v2/step_002_cont_0_contract.tz', 'utf8');
  console.log('📄 Contract loaded: KeepsFA2v2');
  
  const parser = new Parser();
  const parsedContract = parser.parseScript(contractSource);
  
  // Build obscure metadata (no mention of 'keeps' or 'aesthetic')
  const contractMetadataJson = JSON.stringify({
    name: obscureName,
    version: '2.0.0-staging',
    interfaces: ['TZIP-012', 'TZIP-016', 'TZIP-021']
  });
  const contractMetadataBytes = stringToBytes(contractMetadataJson);
  const tezosStoragePointer = stringToBytes('tezos-storage:content');
  
  // Initial storage
  const initialStorageMichelson = `(Pair "${stagingAddress}" (Pair {} (Pair False (Pair {} (Pair {Elt "" 0x${tezosStoragePointer}; Elt "content" 0x${contractMetadataBytes}} (Pair {} (Pair 0 (Pair {} {}))))))))`;
  
  const parsedStorage = parser.parseMichelineExpression(initialStorageMichelson);
  
  console.log('\n📤 Deploying contract...');
  console.log('   (This may take 1-2 minutes...)\n');
  
  const originationOp = await tezos.contract.originate({
    code: parsedContract,
    init: parsedStorage
  });
  
  console.log('⏳ Operation:', originationOp.hash);
  console.log('⏳ Waiting for confirmation...\n');
  
  await originationOp.confirmation(1);
  
  const contractAddress = originationOp.contractAddress;
  
  console.log('╔══════════════════════════════════════════════════════════════╗');
  console.log('║  ✅ STAGING Contract Deployed!                               ║');
  console.log('╚══════════════════════════════════════════════════════════════╝\n');
  console.log('📍 Contract:', contractAddress);
  console.log('🔗 Explorer: https://tzkt.io/' + contractAddress);
  console.log('\n💾 Save this address for testing!');
  
  // Save to staging file
  fs.writeFileSync('./staging-contract-address.txt', contractAddress);
  console.log('   Saved to: staging-contract-address.txt');
}

deploy().catch(err => {
  console.error('❌ Error:', err.message);
  process.exit(1);
});
