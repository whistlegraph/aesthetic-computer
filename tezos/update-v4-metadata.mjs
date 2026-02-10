#!/usr/bin/env node

/**
 * Update v4 contract metadata
 */

import { TezosToolkit } from '@taquito/taquito';
import { InMemorySigner } from '@taquito/signer';
import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

// V4 staging contract
const V4_CONTRACT = 'KT1ER1GyoeRNhkv6E57yKbBbEKi5ynKbaH3W';

// Load staging wallet credentials
const stagingEnvPath = path.join(__dirname, 'staging/.env');
const envContent = fs.readFileSync(stagingEnvPath, 'utf8');
let stagingAddress, stagingKey;
for (const line of envContent.split('\n')) {
  if (line.startsWith('STAGING_ADDRESS=') || line.startsWith('ADDRESS=')) {
    stagingAddress = line.split('=')[1].trim().replace(/"/g, '');
  } else if (line.startsWith('STAGING_KEY=') || line.startsWith('KEY=') || line.startsWith('SECRET_KEY=')) {
    stagingKey = line.split('=')[1].trim().replace(/"/g, '');
  }
}

if (!stagingAddress || !stagingKey) {
  console.error('❌ Could not load staging wallet credentials');
  process.exit(1);
}

const tezos = new TezosToolkit('https://mainnet.api.tez.ie');
tezos.setProvider({ signer: new InMemorySigner(stagingKey) });

console.log('\n╔══════════════════════════════════════════════════════════════╗');
console.log('║  📝 Update v4 Contract Metadata                              ║');
console.log('╚══════════════════════════════════════════════════════════════╝\n');

console.log(`📍 Contract: ${V4_CONTRACT}`);
console.log(`👤 Admin: ${stagingAddress}\n`);

// Read new metadata
const metadataJson = JSON.parse(fs.readFileSync('/tmp/v4-metadata.json', 'utf8'));
const metadataString = JSON.stringify(metadataJson);
const metadataBytes = Buffer.from(metadataString, 'utf8').toString('hex');

console.log('📄 New metadata:');
console.log(JSON.stringify(metadataJson, null, 2));
console.log();

// Prepare contract call
console.log('📤 Calling set_contract_metadata...');

try {
  const contract = await tezos.contract.at(V4_CONTRACT);

  // Call set_contract_metadata with [{ key: "content", value: bytes }]
  const op = await contract.methods.set_contract_metadata([{
    key: 'content',
    value: metadataBytes
  }]).send();

  console.log(`   ⏳ Operation hash: ${op.hash}`);
  console.log('   ⏳ Waiting for confirmation...');

  await op.confirmation(1);

  console.log('\n✅ Contract metadata updated!');
  console.log(`   🔗 Explorer: https://tzkt.io/${op.hash}\n`);

} catch (error) {
  console.error('\n❌ Update failed!');
  console.error(`   Error: ${error.message}\n`);
  process.exit(1);
}
