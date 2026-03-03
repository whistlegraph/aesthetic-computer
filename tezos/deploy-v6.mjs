#!/usr/bin/env node
/**
 * deploy-v6.mjs - Deploy production KidLisp Keeps v6 contract
 *
 * Deploys to mainnet using the kidlisp wallet (keeps.tez).
 *
 * Contract metadata:
 *   name: "KidLisp"
 *   version: "6.0.0"
 *   description: "https://keeps.kidlisp.com"
 *   homepage: "https://kidlisp.com"
 *
 * Usage:
 *   # Set env vars from tezos/kidlisp/.env first:
 *   export TEZOS_KIDLISP_KEY=edsk...
 *   export TEZOS_KIDLISP_ADDRESS=tz1Lc2DzTjDPyWFj1iuAVGGZWNjK67Wun2dC
 *
 *   node deploy-v6.mjs                    # Deploy with kidlisp wallet
 *   node deploy-v6.mjs --wallet=staging   # Deploy with staging wallet (for testing)
 *   node deploy-v6.mjs --dry-run          # Show what would happen without deploying
 */

import { TezosToolkit } from '@taquito/taquito';
import { InMemorySigner } from '@taquito/signer';
import { Parser } from '@taquito/michel-codec';
import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

// Parse CLI args
const args = process.argv.slice(2);
const walletArg = args.find(a => a.startsWith('--wallet='));
const walletName = walletArg ? walletArg.split('=')[1] : 'kidlisp';
const dryRun = args.includes('--dry-run');

// Wallet configurations
const WALLETS = {
  kidlisp: {
    envFile: path.join(__dirname, 'kidlisp/.env'),
    addressKey: 'KIDLISP_ADDRESS',
    keyKey: 'KIDLISP_KEY',
    fallbackAddress: 'tz1Lc2DzTjDPyWFj1iuAVGGZWNjK67Wun2dC',
  },
  staging: {
    envFile: path.join(__dirname, 'staging/.env'),
    addressKey: 'STAGING_ADDRESS',
    keyKey: 'STAGING_KEY',
    fallbackAddress: 'tz1dfoQDuxjwSgxdqJnisyKUxDHweade4Gzt',
  },
};

const walletConfig = WALLETS[walletName];
if (!walletConfig) {
  console.error(`Unknown wallet: ${walletName}. Use: kidlisp or staging`);
  process.exit(1);
}

// Load credentials from .env file
function loadEnv(envPath, addressKey, keyKey, fallbackAddress) {
  if (!fs.existsSync(envPath)) {
    // Fall back to environment variables
    const address = process.env[`TEZOS_${addressKey}`] || process.env[addressKey] || fallbackAddress;
    const secretKey = process.env[`TEZOS_${keyKey}`] || process.env[keyKey];
    if (!secretKey) {
      throw new Error(`No credentials found. Set ${keyKey} env var or create ${envPath}`);
    }
    return { address, secretKey };
  }

  const content = fs.readFileSync(envPath, 'utf8');
  const vars = {};
  for (const line of content.split('\n')) {
    const trimmed = line.trim();
    if (!trimmed || trimmed.startsWith('#')) continue;
    const [key, ...rest] = trimmed.split('=');
    if (key && rest.length > 0) {
      vars[key.trim()] = rest.join('=').trim().replace(/^["']|["']$/g, '');
    }
  }

  const address = vars[addressKey] || vars.ADDRESS || fallbackAddress;
  const secretKey = vars[keyKey] || vars.KEY || vars.SECRET_KEY;
  if (!secretKey) throw new Error(`No secret key found in ${envPath}`);
  return { address, secretKey };
}

function stringToBytes(str) {
  return Buffer.from(str, 'utf8').toString('hex');
}

async function deploy() {
  console.log('\n╔══════════════════════════════════════════════════════════════╗');
  console.log('║  🚀 Deploying KidLisp Keeps v6 — Production Contract         ║');
  console.log('╚══════════════════════════════════════════════════════════════╝\n');

  const { address, secretKey } = loadEnv(
    walletConfig.envFile,
    walletConfig.addressKey,
    walletConfig.keyKey,
    walletConfig.fallbackAddress
  );

  console.log(`📡 Network: Mainnet`);
  console.log(`👤 Wallet: ${walletName} (${address})`);
  if (dryRun) console.log(`🔒 DRY RUN — no transaction will be sent\n`);

  const tezos = new TezosToolkit('https://mainnet.api.tez.ie');
  tezos.setSignerProvider(new InMemorySigner(secretKey));

  // Check balance
  const balance = await tezos.tz.getBalance(address);
  const balanceXTZ = balance.toNumber() / 1_000_000;
  console.log(`💰 Balance: ${balanceXTZ.toFixed(6)} XTZ\n`);

  if (balanceXTZ < 3) {
    throw new Error(`Need at least 3 XTZ for deployment. Have ${balanceXTZ.toFixed(6)} XTZ.`);
  }

  // Contract-level metadata (TZIP-016)
  const contractMetadata = {
    name: "KidLisp",
    version: "6.0.0",
    description: "https://keeps.kidlisp.com",
    homepage: "https://kidlisp.com",
    authors: ["aesthetic.computer"],
    interfaces: ["TZIP-012", "TZIP-016", "TZIP-021"],
    imageUri: "https://oven.aesthetic.computer/keeps/latest",
  };

  console.log('📋 Contract metadata:');
  console.log(`   name: "${contractMetadata.name}"`);
  console.log(`   version: "${contractMetadata.version}"`);
  console.log(`   description: "${contractMetadata.description}"`);
  console.log(`   homepage: "${contractMetadata.homepage}"`);
  console.log(`   authors: ${JSON.stringify(contractMetadata.authors)}`);
  console.log(`   imageUri: "${contractMetadata.imageUri}"`);
  console.log('');

  // Load compiled contract (use v5 compiled output — same bytecode)
  // v6 Python source is identical logic, just different docstrings
  const contractPath = path.join(__dirname, 'KeepsFA2v5/step_002_cont_0_contract.tz');
  if (!fs.existsSync(contractPath)) {
    throw new Error(`Compiled contract not found: ${contractPath}\nRun SmartPy compilation first, or use KeepsFA2v5 compiled output.`);
  }

  const contractSource = fs.readFileSync(contractPath, 'utf8');
  console.log('📄 Contract loaded: KeepsFA2v5 (same bytecode as v6)');

  const parser = new Parser();
  const parsedContract = parser.parseScript(contractSource);

  // Build storage with contract metadata embedded
  const contractMetadataBytes = stringToBytes(JSON.stringify(contractMetadata));
  const tezosStoragePointer = stringToBytes('tezos-storage:content');

  // Initial storage: admin, content_hashes, contract_metadata_locked, default_royalty_bps,
  //                  keep_fee, ledger, metadata, metadata_locked, next_token_id,
  //                  operators, paused, token_creators, token_metadata
  // keep_fee = 2500000 mutez = 2.5 XTZ
  const initialStorageMichelson = `(Pair "${address}" (Pair {} (Pair False (Pair 1000 (Pair 2500000 (Pair {} (Pair {Elt "" 0x${tezosStoragePointer}; Elt "content" 0x${contractMetadataBytes}} (Pair {} (Pair 0 (Pair {} (Pair False (Pair {} {}))))))))))))`;

  console.log(`\n💰 Keep fee: 2.5 XTZ (2500000 mutez)`);
  console.log(`🎨 Royalties: 10% (1000 bps)`);
  console.log(`⏸️  Paused: false`);
  console.log(`🔓 Metadata locked: false\n`);

  if (dryRun) {
    console.log('╔══════════════════════════════════════════════════════════════╗');
    console.log('║  🔒 DRY RUN COMPLETE — No transaction sent                   ║');
    console.log('╚══════════════════════════════════════════════════════════════╝\n');
    console.log('Storage Michelson (first 200 chars):');
    console.log('  ', initialStorageMichelson.substring(0, 200), '...\n');
    return;
  }

  const parsedStorage = parser.parseMichelineExpression(initialStorageMichelson);

  console.log('📤 Deploying contract...');
  console.log('   (This may take 1-2 minutes...)\n');

  const originationOp = await tezos.contract.originate({
    code: parsedContract,
    init: parsedStorage,
  });

  console.log(`⏳ Operation: ${originationOp.hash}`);
  console.log('⏳ Waiting for confirmation...\n');

  await originationOp.confirmation(1);

  const contractAddress = originationOp.contractAddress;

  console.log('╔══════════════════════════════════════════════════════════════╗');
  console.log('║  ✅ KidLisp v6 Production Contract Deployed!                 ║');
  console.log('╚══════════════════════════════════════════════════════════════╝\n');
  console.log(`📍 Contract: ${contractAddress}`);
  console.log(`🔗 Explorer: https://tzkt.io/${contractAddress}`);
  console.log(`🎨 objkt:    https://objkt.com/collection/${contractAddress}`);
  console.log(`\n📝 Next steps:`);
  console.log(`   1. Update constants.mjs: contract = "${contractAddress}"`);
  console.log(`   2. Set KEEPS_STAGING = false`);
  console.log(`   3. Update env var fallbacks in keep-mint.mjs, keep-update.mjs, etc.`);
  console.log(`   4. Deploy to Netlify`);
  console.log(`   5. Smoke test: first production mint\n`);

  // Save address
  const outputFile = path.join(__dirname, 'v6-contract-address.txt');
  fs.writeFileSync(outputFile, contractAddress);
  console.log(`💾 Saved to: ${outputFile}`);

  // Also check new balance
  const newBalance = await tezos.tz.getBalance(address);
  const newBalanceXTZ = newBalance.toNumber() / 1_000_000;
  console.log(`💰 New balance: ${newBalanceXTZ.toFixed(6)} XTZ\n`);
}

deploy().catch(err => {
  console.error('\n❌ Deployment failed!');
  console.error(`   Error: ${err.message}\n`);
  process.exit(1);
});
