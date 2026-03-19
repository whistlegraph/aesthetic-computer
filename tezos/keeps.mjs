#!/usr/bin/env node
/**
 * 🔮 Keeps - Tezos FA2 Contract Management with Taquito
 * 
 * A comprehensive Node.js module for deploying and managing
 * the Aesthetic Computer "Keeps" FA2 contract on Tezos.
 * 
 * Usage:
 *   node keeps.mjs deploy [network]    - Deploy contract (supports --contract profile)
 *   node keeps.mjs keep <piece>        - Keep (preserve) a KidLisp piece
 *   node keeps.mjs status              - Check contract status
 *   node keeps.mjs balance             - Check wallet balance
 *   node keeps.mjs tokens              - List wallet tokens for active keeps contract
 *   node keeps.mjs market              - Show Objkt market snapshot
 *   node keeps.mjs sell <token> <xtz>  - List a token on Objkt marketplace
 *   node keeps.mjs accept <offer_id>   - Accept a specific Objkt offer
 *   node keeps.mjs accept:auto ...      - Accept best offers above thresholds
 *   node keeps.mjs buy <ask_id>        - Buy a listed token (fulfill_ask)
 *   node keeps.mjs upload <piece>      - Upload bundle to IPFS
 */

import { TezosToolkit, MichelsonMap } from '@taquito/taquito';
import { InMemorySigner } from '@taquito/signer';
import { Parser, packDataBytes } from '@taquito/michel-codec';
import { MongoClient } from 'mongodb';
import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';
import crypto from 'crypto';
import readline from 'readline';

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const KEEPS_SECRET_ID = process.env.KEEPS_SECRET_ID || 'tezos-kidlisp';
const KEEP_PERMIT_TTL_MS = Number.parseInt(process.env.KEEP_PERMIT_TTL_MS || '1200000', 10); // 20 minutes
const OBJKT_DATA_API = 'https://data.objkt.com/v3/graphql';
const OBJKT_MARKETPLACE_FALLBACK = {
  mainnet: 'KT1SwbTqhSKF6Pdokiu1K4Fpi17ahPPzmt1X', // objktcom marketplace v6.2
};

// ============================================================================
// Configuration
// ============================================================================

const CONFIG = {
  // Network settings
  ghostnet: {
    rpc: 'https://rpc.ghostnet.teztnets.com',  // Changed from ecadinfra
    name: 'Ghostnet (Testnet)',
    explorer: 'https://ghostnet.tzkt.io'
  },
  mainnet: {
    rpc: 'https://mainnet.api.tez.ie',  // Changed from ecadinfra for better deployment support
    name: 'Mainnet',
    explorer: 'https://tzkt.io'
  },
  
  // IPFS settings
  pinata: {
    apiUrl: 'https://api.pinata.cloud',
    gateway: 'https://ipfs.aesthetic.computer'
  },
  
  // Oven service for thumbnails
  oven: {
    url: process.env.OVEN_URL || 'https://oven.aesthetic.computer'
  },
  
  // Contract paths
  paths: {
    // Compiled contract artifacts by generation
    compiled: {
      v11: path.join(__dirname, 'KeepsFA2v11/step_002_cont_0_contract.tz'),
      v10: path.join(__dirname, 'KeepsFA2v10/step_002_cont_0_contract.tz'),
      v9: path.join(__dirname, 'KeepsFA2v9/step_002_cont_0_contract.tz'),
      v8: path.join(__dirname, 'KeepsFA2v8/step_002_cont_0_contract.tz'),
      v7: path.join(__dirname, 'KeepsFA2v7/step_002_cont_0_contract.tz'),
      v6: path.join(__dirname, 'KeepsFA2v6/step_002_cont_0_contract.tz'),
      v2: path.join(__dirname, 'KeepsFA2v2/step_002_cont_0_contract.tz'),
      v3: path.join(__dirname, 'KeepsFA2v3/step_002_cont_0_contract.tz'),
      v4: path.join(__dirname, 'KeepsFA2v4/step_002_cont_0_contract.tz'),
      v5: path.join(__dirname, 'KeepsFA2v5/step_002_cont_0_contract.tz'),
    },
    // Backward-compatible defaults
    contract: path.join(__dirname, 'KeepsFA2v9/step_002_cont_0_contract.tz'),
    storage: path.join(__dirname, 'KeepsFA2v9/step_002_cont_0_storage.tz'),
    // Legacy direct paths
    v3Contract: path.join(__dirname, 'KeepsFA2v3/step_002_cont_0_contract.tz'),
    v2Contract: path.join(__dirname, 'KeepsFA2v2/step_002_cont_0_contract.tz'),
    // Legacy contract path
    legacyContract: path.join(__dirname, 'michelson-lib/keeps-fa2-complete.tz'),
    // Network-specific contract addresses
    contractAddresses: {
      ghostnet: path.join(__dirname, 'contract-address-ghostnet.txt'),
      mainnet: path.join(__dirname, 'contract-address-mainnet.txt'),
    },
    // Legacy single file (deprecated)
    contractAddress: path.join(__dirname, 'contract-address.txt'),
    vault: path.join(__dirname, '../aesthetic-computer-vault')
  }
};

const CONTRACT_PROFILES = {
  v11: {
    key: 'v11',
    label: 'KidLisp v11 — user-only minting, no admin path',
    artifactKey: 'v11',
    metadata: {
      name: 'KidLisp',
      version: '11.0.0',
      description: 'https://keep.kidlisp.com',
      homepage: 'https://keep.kidlisp.com',
      interfaces: ['TZIP-012', 'TZIP-016', 'TZIP-021'],
      authors: ['aesthetic.computer'],
      imageUri: 'https://oven.aesthetic.computer/keeps/latest',
    },
    keepFeeMutez: 2_500_000,
    artistRoyaltyBps: 900,
    platformRoyaltyBps: 100,
    paused: false,
  },
  v10: {
    key: 'v10',
    label: 'KidLisp v10 — no admin_transfer, split royalties',
    artifactKey: 'v10',
    metadata: {
      name: 'KidLisp',
      version: '10.0.0',
      description: 'https://keep.kidlisp.com',
      homepage: 'https://keep.kidlisp.com',
      interfaces: ['TZIP-012', 'TZIP-016', 'TZIP-021'],
      authors: ['aesthetic.computer'],
      imageUri: 'https://oven.aesthetic.computer/keeps/latest',
    },
    keepFeeMutez: 2_500_000,
    artistRoyaltyBps: 900,
    platformRoyaltyBps: 100,
    paused: false,
  },
  v9: {
    key: 'v9',
    label: 'KidLisp v9 final production',
    artifactKey: 'v9',
    metadata: {
      name: 'KidLisp',
      version: '9.0.0',
      description: 'https://keep.kidlisp.com',
      homepage: 'https://keep.kidlisp.com',
      interfaces: ['TZIP-012', 'TZIP-016', 'TZIP-021'],
      authors: ['aesthetic.computer'],
      imageUri: 'https://oven.aesthetic.computer/keeps/latest',
    },
    keepFeeMutez: 2_500_000,
    defaultRoyaltyBps: 1000,
    paused: false,
  },
  v8: {
    key: 'v8',
    label: 'KidLisp v8 signed-permit production',
    artifactKey: 'v8',
    metadata: {
      name: 'KidLisp',
      version: '8.0.0',
      description: 'https://keep.kidlisp.com',
      homepage: 'https://keep.kidlisp.com',
      interfaces: ['TZIP-012', 'TZIP-016', 'TZIP-021'],
      authors: ['aesthetic.computer'],
      imageUri: 'https://oven.aesthetic.computer/keeps/latest',
    },
    keepFeeMutez: 2_500_000,
    defaultRoyaltyBps: 1000,
    paused: false,
  },
  v7: {
    key: 'v7',
    label: 'KidLisp v7 final production',
    artifactKey: 'v7',
    metadata: {
      name: 'KidLisp',
      version: '7.0.0',
      description: 'https://keep.kidlisp.com',
      homepage: 'https://kidlisp.com',
      interfaces: ['TZIP-012', 'TZIP-016', 'TZIP-021'],
      authors: ['aesthetic.computer'],
      imageUri: 'https://oven.aesthetic.computer/keeps/latest',
    },
    keepFeeMutez: 2_500_000,
    defaultRoyaltyBps: 1000,
    paused: false,
  },
  v6: {
    key: 'v6',
    label: 'KidLisp v6 production (legacy)',
    artifactKey: 'v6',
    metadata: {
      name: 'KidLisp',
      version: '6.0.0',
      description: 'https://keep.kidlisp.com',
      homepage: 'https://kidlisp.com',
      interfaces: ['TZIP-012', 'TZIP-016', 'TZIP-021'],
      authors: ['aesthetic.computer'],
      imageUri: 'https://oven.aesthetic.computer/keeps/latest',
    },
    keepFeeMutez: 2_500_000,
    defaultRoyaltyBps: 1000,
    paused: false,
  },
  v5rc: {
    key: 'v5rc',
    label: 'KidLisp v5 release candidate',
    artifactKey: 'v5',
    metadata: {
      name: 'KidLisp Keeps RC',
      version: '5.0.0-rc',
      description: 'https://keep.kidlisp.com/rc',
      homepage: 'https://kidlisp.com',
      interfaces: ['TZIP-012', 'TZIP-016', 'TZIP-021'],
      authors: ['aesthetic.computer'],
      imageUri: 'https://oven.aesthetic.computer/keeps/latest',
    },
    keepFeeMutez: 2_500_000,
    defaultRoyaltyBps: 1000,
    paused: false,
  },
  v4: {
    key: 'v4',
    label: 'Keeps v4 legacy',
    artifactKey: 'v4',
    metadata: {
      name: 'KidLisp Keeps Beta',
      version: '4.0.0',
      interfaces: ['TZIP-012', 'TZIP-016', 'TZIP-021'],
      authors: ['aesthetic.computer'],
      homepage: 'https://aesthetic.computer',
      imageUri: 'https://oven.aesthetic.computer/keeps/latest',
    },
    keepFeeMutez: 0,
    defaultRoyaltyBps: 1000,
    paused: false,
  },
};

function resolveContractProfile(rawProfile = 'v9') {
  const normalized = String(rawProfile || 'v9').trim().toLowerCase();
  const aliasMap = {
    rc: 'v5rc',
    v5: 'v5rc',
    production: 'v11',
    latest: 'v11',
  };
  const key = aliasMap[normalized] || normalized;
  const profile = CONTRACT_PROFILES[key];
  if (!profile) {
    const supported = Object.keys(CONTRACT_PROFILES).join(', ');
    throw new Error(`❌ Unknown contract profile: ${rawProfile}. Use one of: ${supported}`);
  }
  return profile;
}

function getMongoSecretsConfig() {
  return {
    connectionString: process.env.MONGODB_CONNECTION_STRING,
    dbName: process.env.MONGODB_NAME,
  };
}

async function syncActiveKeepsSecret({ network = 'mainnet', contractAddress, profile }) {
  const { connectionString, dbName } = getMongoSecretsConfig();
  if (!connectionString || !dbName) {
    console.log('   ⚠️  Mongo secrets sync skipped (set MONGODB_CONNECTION_STRING + MONGODB_NAME to enable).');
    return { synced: false, reason: 'missing-mongo-env' };
  }

  const client = new MongoClient(connectionString, {
    serverSelectionTimeoutMS: 10000,
    connectTimeoutMS: 10000,
  });

  try {
    await client.connect();
    const secrets = client.db(dbName).collection('secrets');
    const now = new Date().toISOString();

    const update = await secrets.updateOne(
      { _id: KEEPS_SECRET_ID },
      {
        $set: {
          [`keepsContract.${network}`]: contractAddress,
          currentKeepsContract: contractAddress,
          currentKeepsNetwork: network,
          currentKeepsProfile: profile.key,
          currentKeepsVersion: profile.metadata?.version || null,
          currentKeepsUpdatedAt: now,
        },
      }
    );

    if (!update.matchedCount) {
      console.log(`   ⚠️  Mongo secrets sync skipped (no secrets.${KEEPS_SECRET_ID} document found).`);
      return { synced: false, reason: 'secret-not-found' };
    }

    console.log(`   🗄️  Synced secrets.${KEEPS_SECRET_ID} -> ${contractAddress} (${profile.key} on ${network})`);
    return { synced: true };
  } catch (error) {
    console.log(`   ⚠️  Mongo secrets sync failed: ${error.message}`);
    return { synced: false, reason: 'sync-error', error: error.message };
  } finally {
    await client.close().catch(() => {});
  }
}

async function syncCurrentContractToSecrets(network = 'mainnet', options = {}) {
  const profile = resolveContractProfile(options.contractProfile || options.profile || 'v9');
  const addressPath = getContractAddressPath(network);
  if (!fs.existsSync(addressPath)) {
    throw new Error(`❌ No saved contract address for ${network} at ${addressPath}`);
  }

  const contractAddress = fs.readFileSync(addressPath, 'utf8').trim();
  if (!isKt1Address(contractAddress)) {
    throw new Error(`❌ Invalid contract address in ${addressPath}: ${contractAddress}`);
  }

  console.log(`\n🗄️ Syncing secrets from ${addressPath}`);
  console.log(`   Contract: ${contractAddress}`);
  console.log(`   Profile:  ${profile.key}`);
  console.log(`   Network:  ${network}`);

  return syncActiveKeepsSecret({ network, contractAddress, profile });
}

// ============================================================================
// Credential Loading
// ============================================================================

// Current wallet selection (can be changed via --wallet flag)
let currentWallet = 'staging'; // default for mainnet staging contract

function setWallet(wallet) {
  currentWallet = wallet;
}

function loadCredentials() {
  const credentials = {};
  
  // Load Tezos wallet credentials based on current wallet selection
  // Note: aesthetic wallet keys are stored in kidlisp/.env for convenience
  const walletPaths = {
    keeps: { path: 'tezos/kidlisp/.env', addressKey: 'KEEPS_ADDRESS', secretKey: 'KEEPS_KEY' },
    kidlisp: { path: 'tezos/kidlisp/.env', addressKey: 'KIDLISP_ADDRESS', secretKey: 'KIDLISP_KEY' },
    aesthetic: { path: 'tezos/kidlisp/.env', addressKey: 'AESTHETIC_ADDRESS', secretKey: 'AESTHETIC_KEY' },
    staging: { path: 'tezos/staging/.env', addressKey: 'STAGING_ADDRESS', secretKey: 'STAGING_KEY' }
  };
  
  const walletConfig = walletPaths[currentWallet] || walletPaths.kidlisp;
  const tezosEnvPath = path.join(CONFIG.paths.vault, walletConfig.path);
  
  if (fs.existsSync(tezosEnvPath)) {
    const content = fs.readFileSync(tezosEnvPath, 'utf8');
    for (const line of content.split('\n')) {
      // Try both specific keys and generic ADDRESS/KEY patterns
      if (line.startsWith(walletConfig.addressKey + '=') || line.startsWith('ADDRESS=')) {
        credentials.address = line.split('=')[1].trim().replace(/"/g, '');
      } else if (line.startsWith(walletConfig.secretKey + '=') || line.startsWith('KEY=') || line.startsWith('SECRET_KEY=')) {
        credentials.secretKey = line.split('=')[1].trim().replace(/"/g, '');
      }
    }
  }
  
  // Load Pinata credentials
  const pinataEnvPath = path.join(CONFIG.paths.vault, '.env.pinata');
  if (fs.existsSync(pinataEnvPath)) {
    const content = fs.readFileSync(pinataEnvPath, 'utf8');
    for (const line of content.split('\n')) {
      if (line.startsWith('PINATA_API_KEY=')) {
        credentials.pinataKey = line.split('=')[1].trim().replace(/"/g, '');
      } else if (line.startsWith('PINATA_API_SECRET=')) {
        credentials.pinataSecret = line.split('=')[1].trim().replace(/"/g, '');
      }
    }
  }
  
  credentials.wallet = currentWallet;
  return credentials;
}

// Get contract address file path for a network
function getContractAddressPath(network = 'mainnet') {
  // Use network-specific paths if available
  if (CONFIG.paths.contractAddresses[network]) {
    return CONFIG.paths.contractAddresses[network];
  }
  // Fall back to legacy single file
  return CONFIG.paths.contractAddress;
}

function isKt1Address(value) {
  return typeof value === 'string' && /^KT1[1-9A-HJ-NP-Za-km-z]{33}$/.test(value.trim());
}

function formatExtendedError(error) {
  if (!error || typeof error !== 'object') return '';

  const payload = error.body ?? error.errors ?? error.data ?? null;
  if (!payload) return '';

  try {
    return JSON.stringify(payload, null, 2);
  } catch {
    return String(payload);
  }
}

function tzktApiBase(network = 'mainnet') {
  return network === 'mainnet' ? 'https://api.tzkt.io' : `https://api.${network}.tzkt.io`;
}

async function objktGraphQL(query, variables = {}) {
  const response = await fetch(OBJKT_DATA_API, {
    method: 'POST',
    headers: { 'content-type': 'application/json' },
    body: JSON.stringify({ query, variables }),
  });

  if (!response.ok) {
    throw new Error(`Objkt data API returned ${response.status}`);
  }

  const payload = await response.json();
  if (Array.isArray(payload.errors) && payload.errors.length > 0) {
    throw new Error(`Objkt data API error: ${payload.errors[0].message || 'Unknown error'}`);
  }

  return payload.data || {};
}

function parseVersionFromLabel(label = '') {
  const match = String(label).match(/v(\d+(?:\.\d+)*)/i);
  if (!match) return [];
  return match[1].split('.').map((part) => Number.parseInt(part, 10)).filter(Number.isFinite);
}

function compareVersionArrays(a = [], b = []) {
  const maxLength = Math.max(a.length, b.length);
  for (let i = 0; i < maxLength; i += 1) {
    const av = a[i] ?? 0;
    const bv = b[i] ?? 0;
    if (av !== bv) return av - bv;
  }
  return 0;
}

async function resolveObjktMarketplaceContract({
  network = 'mainnet',
  keepsContract,
  explicitContract = null,
}) {
  if (explicitContract) return explicitContract;

  if (network !== 'mainnet') {
    throw new Error(
      `Objkt marketplace auto-discovery only supports mainnet. Pass --marketplace=<KT1...> for ${network}.`
    );
  }

  try {
    const existingData = await objktGraphQL(
      `
      query($contract:String!) {
        listing_active(
          where:{fa_contract:{_eq:$contract}}
          order_by:{timestamp:desc}
          limit:1
        ) {
          marketplace_contract
          marketplace { name }
        }
      }
      `,
      { contract: keepsContract }
    );

    const existing = existingData?.listing_active?.[0];
    if (isKt1Address(existing?.marketplace_contract)) {
      return existing.marketplace_contract;
    }
  } catch {
    // Fallback to registry lookup below.
  }

  const registryData = await objktGraphQL(`
    query {
      marketplace_contract(
        where:{
          group:{_eq:"objktcom"},
          subgroup:{_eq:"marketplace"},
          name:{_ilike:"objktcom marketplace v%"}
        }
      ) {
        contract
        name
      }
    }
  `);

  const rows = Array.isArray(registryData?.marketplace_contract)
    ? registryData.marketplace_contract
    : [];

  const ranked = rows
    .filter((row) => isKt1Address(row?.contract))
    .map((row) => ({
      ...row,
      parsedVersion: parseVersionFromLabel(row?.name),
    }))
    .sort((a, b) => compareVersionArrays(b.parsedVersion, a.parsedVersion));

  if (ranked.length > 0) {
    return ranked[0].contract;
  }

  const fallback = OBJKT_MARKETPLACE_FALLBACK[network];
  if (isKt1Address(fallback)) return fallback;

  throw new Error('Could not resolve Objkt marketplace contract.');
}

function loadContractAddress(network = 'mainnet') {
  const addressPath = getContractAddressPath(network);
  if (!fs.existsSync(addressPath)) {
    throw new Error(`❌ No contract deployed on ${network}. Run: node keeps.mjs deploy ${network}`);
  }
  return fs.readFileSync(addressPath, 'utf8').trim();
}

function parsePriceToMutez(priceInput) {
  const asNumber = Number.parseFloat(String(priceInput));
  if (!Number.isFinite(asNumber) || asNumber <= 0) {
    throw new Error(`Invalid price "${priceInput}". Expected a positive XTZ value.`);
  }
  return Math.round(asNumber * 1_000_000);
}

function parseOptionalIsoTimestamp(value, label) {
  if (value == null || value === '') return null;
  const date = new Date(value);
  if (Number.isNaN(date.getTime())) {
    throw new Error(`Invalid ${label} timestamp "${value}". Use ISO-8601 format.`);
  }
  return date.toISOString();
}

async function resolveTokenIdFromReference(tokenReference, { contractAddress, network = 'mainnet' }) {
  const raw = String(tokenReference || '').trim();
  if (!raw) {
    throw new Error('Token reference is required (token id or piece code like $bip).');
  }

  if (/^\d+$/.test(raw)) {
    return Number.parseInt(raw, 10);
  }

  const pieceName = raw.replace(/^\$/, '');
  const duplicate = await checkDuplicatePiece(pieceName, contractAddress, network);
  if (duplicate.exists) {
    return Number.parseInt(duplicate.tokenId, 10);
  }

  throw new Error(`Could not resolve token reference "${tokenReference}" to a token id.`);
}

function normalizeShareMap(shares = {}) {
  const normalized = {};
  for (const [address, amount] of Object.entries(shares || {})) {
    const nat = Number.parseInt(String(amount), 10);
    if (nat > 0) {
      normalized[address] = nat.toString();
    }
  }
  return normalized;
}

async function fetchTokenFromTzkt(contractAddress, tokenId, network = 'mainnet') {
  const apiBase = tzktApiBase(network);
  const url = `${apiBase}/v1/tokens?contract=${contractAddress}&tokenId=${tokenId}`;
  const response = await fetch(url);
  if (!response.ok) {
    throw new Error(`Failed to fetch token #${tokenId} from TzKT (${response.status})`);
  }
  const rows = await response.json();
  return rows?.[0] || null;
}

async function assertWalletOwnsToken(contractAddress, tokenId, ownerAddress, network = 'mainnet') {
  const apiBase = tzktApiBase(network);
  const url = `${apiBase}/v1/tokens/balances?token.contract=${contractAddress}&token.tokenId=${tokenId}&account=${ownerAddress}&balance.gt=0&limit=1`;
  const response = await fetch(url);
  if (!response.ok) {
    throw new Error(`Failed to verify ownership for token #${tokenId} (${response.status})`);
  }
  const rows = await response.json();
  if (!Array.isArray(rows) || rows.length === 0) {
    throw new Error(`Wallet ${ownerAddress} does not currently hold token #${tokenId}.`);
  }
}

async function loadActiveListingForToken(contractAddress, tokenId, sellerAddress) {
  const data = await objktGraphQL(
    `
    query($contract:String!, $tokenId:String!, $seller:String!) {
      listing_active(
        where:{
          fa_contract:{_eq:$contract},
          seller_address:{_eq:$seller},
          token:{token_id:{_eq:$tokenId}}
        }
        order_by:{timestamp:desc}
        limit:1
      ) {
        id
        bigmap_key
        price_xtz
        marketplace_contract
        marketplace { name }
      }
    }
    `,
    { contract: contractAddress, tokenId: String(tokenId), seller: sellerAddress }
  );
  return data?.listing_active?.[0] || null;
}

async function loadBestActiveOfferForToken(contractAddress, tokenId) {
  const data = await objktGraphQL(
    `
    query($contract:String!, $tokenId:String!) {
      offer_active(
        where:{
          fa_contract:{_eq:$contract},
          token:{token_id:{_eq:$tokenId}}
        }
        order_by:{price_xtz:desc}
        limit:1
      ) {
        id
        bigmap_key
        price_xtz
        buyer_address
        marketplace_contract
        amount_left
        timestamp
        token { token_id name fa_contract }
      }
    }
    `,
    { contract: contractAddress, tokenId: String(tokenId) }
  );
  return data?.offer_active?.[0] || null;
}

async function loadActiveOfferById(offerId) {
  const numericId = Number.parseInt(String(offerId), 10);
  if (!Number.isInteger(numericId) || numericId < 0) {
    throw new Error(`Invalid offer id "${offerId}".`);
  }

  const data = await objktGraphQL(
    `
    query {
      offer_active(
        where:{
          _or:[
            {id:{_eq:${numericId}}},
            {bigmap_key:{_eq:${numericId}}}
          ]
        }
        order_by:{timestamp:desc}
        limit:5
      ) {
        id
        bigmap_key
        price_xtz
        buyer_address
        marketplace_contract
        amount_left
        timestamp
        token { token_id name fa_contract }
      }
    }
    `
  );

  const rows = Array.isArray(data?.offer_active) ? data.offer_active : [];
  if (rows.length === 0) return null;

  // Prefer direct row-id match first, then on-chain offer-id (bigmap key).
  return rows.find((row) => Number.parseInt(String(row?.id), 10) === numericId)
    || rows.find((row) => Number.parseInt(String(row?.bigmap_key), 10) === numericId)
    || rows[0];
}

// ============================================================================
// Tezos Client Setup
// ============================================================================

async function createTezosClient(network = 'mainnet') {
  const credentials = loadCredentials();
  
  if (!credentials.address || !credentials.secretKey) {
    throw new Error('❌ Tezos credentials not found in vault');
  }
  
  const config = CONFIG[network];
  const tezos = new TezosToolkit(config.rpc);
  
  // Set up signer
  tezos.setProvider({
    signer: new InMemorySigner(credentials.secretKey)
  });
  
  return { tezos, credentials, config };
}

// ============================================================================
// Contract Deployment
// ============================================================================

async function deployContract(network = 'mainnet', options = {}) {
  const profile = resolveContractProfile(options.contractProfile || options.profile || 'v9');
  const contractPath = CONFIG.paths.compiled[profile.artifactKey] || CONFIG.paths.contract;
  const feeXTZ = (profile.keepFeeMutez / 1_000_000).toFixed(6);
  const pausedMichelson = profile.paused ? 'True' : 'False';

  console.log('\n╔══════════════════════════════════════════════════════════════╗');
  console.log(`║  🚀 Deploying ${profile.label.padEnd(48)}║`);
  console.log('╚══════════════════════════════════════════════════════════════╝\n');

  const { tezos, credentials, config } = await createTezosClient(network);

  console.log(`📡 Network: ${config.name}`);
  console.log(`📍 RPC: ${config.rpc}`);
  console.log(`👤 Administrator: ${credentials.address}`);
  console.log(`🧱 Profile: ${profile.key}\n`);

  // Check balance
  console.log('💰 Checking balance...');
  const balance = await tezos.tz.getBalance(credentials.address);
  const balanceXTZ = balance.toNumber() / 1_000_000;
  console.log(`   Balance: ${balanceXTZ.toFixed(2)} XTZ`);

  if (balanceXTZ < 1) {
    throw new Error('❌ Insufficient balance. Need at least 1 XTZ for deployment.');
  }

  // Load contract code (SmartPy compiled Michelson)
  console.log('\n📄 Loading contract...');
  if (!fs.existsSync(contractPath)) {
    throw new Error(`❌ Contract file not found: ${contractPath}\n   Compile the selected artifact before deploy.`);
  }

  const contractSource = fs.readFileSync(contractPath, 'utf8');
  console.log(`   ✓ Contract loaded: ${path.relative(__dirname, contractPath)}`);

  // Parse the contract using michel-codec
  const parser = new Parser();
  const parsedContract = parser.parseScript(contractSource);

  console.log('\n💾 Creating initial storage...');

  const contractMetadataJson = JSON.stringify(profile.metadata);
  const contractMetadataBytes = stringToBytes(contractMetadataJson);
  const tezosStoragePointer = stringToBytes('tezos-storage:content');

  // Storage layout varies by version (SmartPy sorts fields alphabetically).
  // v10 adds: artist_royalty_bps, platform_royalty_bps, treasury_address
  //           removes: default_royalty_bps
  // v10 order: administrator, artist_royalty_bps, content_hashes,
  //            contract_metadata_locked, keep_fee, ledger, metadata,
  //            metadata_locked, next_token_id, operators, paused,
  //            platform_royalty_bps, token_creators, token_metadata, treasury_address
  const isV10 = profile.key === 'v10' || profile.key === 'v11';
  const treasuryAddress = credentials.treasuryAddress || credentials.address;

  let initialStorageMichelson;
  if (isV10) {
    initialStorageMichelson = `(Pair "${credentials.address}" (Pair ${profile.artistRoyaltyBps} (Pair {} (Pair False (Pair ${profile.keepFeeMutez} (Pair {} (Pair {Elt "" 0x${tezosStoragePointer}; Elt "content" 0x${contractMetadataBytes}} (Pair {} (Pair 0 (Pair {} (Pair ${pausedMichelson} (Pair ${profile.platformRoyaltyBps} (Pair {} (Pair {} "${treasuryAddress}")))))))))))))))`;
  } else {
    // v9 and earlier: administrator, content_hashes, contract_metadata_locked,
    // default_royalty_bps, keep_fee, ledger, metadata, metadata_locked,
    // next_token_id, operators, paused, token_creators, token_metadata
    initialStorageMichelson = `(Pair "${credentials.address}" (Pair {} (Pair False (Pair ${profile.defaultRoyaltyBps} (Pair ${profile.keepFeeMutez} (Pair {} (Pair {Elt "" 0x${tezosStoragePointer}; Elt "content" 0x${contractMetadataBytes}} (Pair {} (Pair 0 (Pair {} (Pair ${pausedMichelson} (Pair {} {}))))))))))))`;
  }
  const parsedStorage = parser.parseMichelineExpression(initialStorageMichelson);

  console.log(`   ✓ Name: ${profile.metadata.name}`);
  console.log(`   ✓ Version: ${profile.metadata.version}`);
  if (profile.metadata.description) {
    console.log(`   ✓ Description: ${profile.metadata.description}`);
  }
  console.log(`   ✓ Homepage: ${profile.metadata.homepage}`);
  console.log(`   ✓ Initial token ID: 0`);
  console.log(`   ✓ Keep fee: ${profile.keepFeeMutez} mutez (${feeXTZ} XTZ)`);
  if (profile.artistRoyaltyBps !== undefined) {
    console.log(`   ✓ Artist royalty: ${profile.artistRoyaltyBps} bps / Platform: ${profile.platformRoyaltyBps} bps`);
    console.log(`   ✓ Treasury: ${treasuryAddress}`);
  } else {
    console.log(`   ✓ Default royalty: ${profile.defaultRoyaltyBps} bps`);
  }
  console.log(`   ✓ Paused: ${profile.paused}`);

  console.log('\n📤 Deploying contract...');
  console.log('   (This may take 1-2 minutes...)\n');

  try {
    const originationOp = await tezos.contract.originate({
      code: parsedContract,
      init: parsedStorage
    });

    console.log(`   ⏳ Operation hash: ${originationOp.hash}`);
    console.log('   ⏳ Waiting for confirmation...');

    await originationOp.confirmation(1);

    const contractAddress = originationOp.contractAddress;

    console.log('\n╔══════════════════════════════════════════════════════════════╗');
    console.log('║  ✅ Contract Deployed Successfully!                          ║');
    console.log('╚══════════════════════════════════════════════════════════════╝\n');

    console.log(`   📍 Contract Address: ${contractAddress}`);
    console.log(`   🔗 Explorer: ${config.explorer}/${contractAddress}`);
    console.log(`   🖼️  Objkt: https://${network === 'mainnet' ? '' : 'ghostnet.'}objkt.com/collection/${contractAddress}`);
    console.log(`   📝 Operation: ${config.explorer}/${originationOp.hash}\n`);

    // Save contract address (network-specific file)
    const addressPath = getContractAddressPath(network);
    fs.writeFileSync(addressPath, contractAddress);
    console.log(`   💾 Saved address to: ${addressPath}\n`);

    await syncActiveKeepsSecret({ network, contractAddress, profile });
    console.log('');

    return { address: contractAddress, hash: originationOp.hash, profile: profile.key };

  } catch (error) {
    console.error('\n❌ Deployment failed!');
    console.error(`   Error: ${error.message}`);
    if (error.message.includes('bad_stack')) {
      console.error('\n   💡 This usually means storage format mismatch.');
      console.error('   Check that the storage matches the selected contract artifact.');
    }
    throw error;
  }
}

// ============================================================================
// Contract Status
// ============================================================================

async function getContractStatus(network = 'mainnet') {
  const { tezos, config } = await createTezosClient(network);
  
  // Load contract address (network-specific)
  const addressPath = getContractAddressPath(network);
  if (!fs.existsSync(addressPath)) {
    throw new Error(`❌ No contract deployed on ${network}. Run: node keeps.mjs deploy ${network}`);
  }
  
  const contractAddress = fs.readFileSync(addressPath, 'utf8').trim();
  
  console.log('\n╔══════════════════════════════════════════════════════════════╗');
  console.log('║  📊 Contract Status                                          ║');
  console.log('╚══════════════════════════════════════════════════════════════╝\n');
  
  console.log(`📡 Network: ${config.name}`);
  console.log(`📍 Contract: ${contractAddress}`);
  console.log(`🔗 Explorer: ${config.explorer}/${contractAddress}\n`);
  
  try {
    const contract = await tezos.contract.at(contractAddress);
    const storage = await contract.storage();
    
    console.log('📦 Storage:');
    console.log(`   Administrator: ${storage.administrator}`);
    console.log(`   Next Token ID: ${storage.next_token_id.toString()}`);
    console.log(`   Total Keeps: ${storage.next_token_id.toString()}`);
    
    const totalTokens = storage.next_token_id.toNumber();
    
    // For large collections, use TzKT API with pagination
    // Only show recent tokens to avoid O(n) RPC calls
    const MAX_DISPLAY = 10;
    if (totalTokens > 0) {
      console.log(`\n🎨 Recent Tokens (showing last ${Math.min(MAX_DISPLAY, totalTokens)} of ${totalTokens}):`);
      
      // Fetch recent tokens via TzKT (efficient, paginated)
      const tzktUrl = `https://api.${network}.tzkt.io/v1/contracts/${contractAddress}/bigmaps/token_metadata/keys?limit=${MAX_DISPLAY}&sort.desc=id`;
      const objktBase = network === 'mainnet' ? 'https://objkt.com' : 'https://ghostnet.objkt.com';
      try {
        const response = await fetch(tzktUrl);
        if (response.ok) {
          const tokens = await response.json();
          for (const token of tokens.reverse()) {
            const tokenId = token.key;
            const tokenInfo = token.value?.token_info || {};
            const name = tokenInfo.name ? Buffer.from(tokenInfo.name, 'hex').toString() : `#${tokenId}`;
            // Check if metadata is locked (bigmap entry must exist AND be true)
            const lockValue = await storage.metadata_locked?.get?.(parseInt(tokenId));
            const locked = lockValue === true ? ' 🔒' : '';
            const objktUrl = `${objktBase}/asset/${contractAddress}/${tokenId}`;
            console.log(`   [${tokenId}] ${name}${locked}`);
            console.log(`       🔗 ${objktUrl}`);
          }
        } else {
          console.log('   (Use TzKT explorer to view all tokens)');
        }
      } catch (e) {
        console.log('   (Could not fetch token list from TzKT)');
      }
      
      if (totalTokens > MAX_DISPLAY) {
        console.log(`   ... and ${totalTokens - MAX_DISPLAY} more`);
      }
    }
    
    return { address: contractAddress, storage };
    
  } catch (error) {
    console.error(`\n❌ Failed to get contract status: ${error.message}`);
    throw error;
  }
}

// ============================================================================
// Wallet Balance
// ============================================================================

async function getBalance(network = 'mainnet') {
  const { tezos, credentials, config } = await createTezosClient(network);
  
  console.log('\n╔══════════════════════════════════════════════════════════════╗');
  console.log('║  💰 Wallet Balance                                           ║');
  console.log('╚══════════════════════════════════════════════════════════════╝\n');
  
  console.log(`📡 Network: ${config.name}`);
  console.log(`👤 Address: ${credentials.address}\n`);
  
  const balance = await tezos.tz.getBalance(credentials.address);
  const balanceXTZ = balance.toNumber() / 1_000_000;
  
  console.log(`💵 Balance: ${balanceXTZ.toFixed(6)} XTZ`);
  console.log(`🔗 Explorer: ${config.explorer}/${credentials.address}\n`);
  
  return { address: credentials.address, balance: balanceXTZ };
}

// ============================================================================
// Content Type Detection
// ============================================================================

/**
 * Detect if a piece is kidlisp or JavaScript mjs
 * KidLisp pieces start with $ and are stored via the store-kidlisp API
 * JavaScript pieces exist as .mjs files in disks/
 */
async function detectContentType(piece) {
  const pieceName = piece.replace(/^\$/, '');
  
  // Check if it's a kidlisp piece (codes like 39j, abc, etc.)
  // KidLisp codes are 2-4 alphanumeric characters
  if (/^[a-z0-9]{2,4}$/i.test(pieceName)) {
    // Try to fetch from kidlisp API to confirm
    try {
      const response = await fetch(`https://aesthetic.computer/api/store-kidlisp?code=${pieceName}`);
      const data = await response.json();
      if (data.source && !data.error) {
        return { type: 'kidlisp', source: data.source };
      }
    } catch (e) {
      // Fall through to check mjs
    }
  }
  
  // Check if it exists as an mjs piece
  try {
    const response = await fetch(`https://aesthetic.computer/aesthetic.computer/disks/${pieceName}.mjs`, {
      method: 'HEAD'
    });
    if (response.ok) {
      return { type: 'mjs', source: null };
    }
  } catch (e) {
    // Continue
  }
  
  // Default to assuming it's a kidlisp code
  return { type: 'kidlisp', source: null };
}

// ============================================================================
// Bundle Generation via Netlify Endpoint
// ============================================================================

/**
 * Fetch a proper self-contained HTML bundle from the Netlify endpoint
 * This bundles all JS, CSS, and assets into a single file that works offline
 */
// Check if a piece name already exists in the contract
async function checkDuplicatePiece(pieceName, contractAddress, network = 'mainnet') {
  if (!contractAddress) {
    // Load contract address (network-specific) if not provided
    const addressPath = getContractAddressPath(network);
    if (!fs.existsSync(addressPath)) {
      return { exists: false }; // No contract deployed yet
    }
    contractAddress = fs.readFileSync(addressPath, 'utf8').trim();
  }
  
  // Query the content_hashes big_map via TzKT
  // Key is the piece name as hex bytes
  const keyBytes = stringToBytes(pieceName);
  // Use network-appropriate API endpoint
  const apiBase = network === 'mainnet' ? 'https://api.tzkt.io' : `https://api.${network}.tzkt.io`;
  const url = `${apiBase}/v1/contracts/${contractAddress}/bigmaps/content_hashes/keys/${keyBytes}`;
  
  try {
    const response = await fetch(url);
    if (response.status === 200) {
      const data = await response.json();
      // Check if the key is still active (not burned)
      if (data.active) {
        return { exists: true, tokenId: data.value };
      }
    }
    return { exists: false };
  } catch (error) {
    // If query fails, assume not duplicate (will fail at contract level if it is)
    return { exists: false };
  }
}

async function fetchBundleFromNetlify(piece, contentType) {
  const pieceName = piece.replace(/^\$/, '');
  
  console.log(`📦 Fetching bundle from Netlify (${contentType})...`);
  
  // Use local dev server if --local flag is set or LOCAL_BUNDLE env var
  const useLocal = process.env.LOCAL_BUNDLE === '1' || process.argv.includes('--local');
  const baseUrl = useLocal 
    ? 'https://localhost:8888/api/bundle-html'
    : 'https://aesthetic.computer/api/bundle-html';
  
  // Build the correct endpoint URL based on content type
  let url;
  if (contentType === 'kidlisp') {
    url = `${baseUrl}?code=${pieceName}&format=json`;
  } else {
    url = `${baseUrl}?piece=${pieceName}&format=json`;
  }
  
  console.log(`   URL: ${url}${useLocal ? ' (local)' : ''}`);
  
  let response;
  if (useLocal) {
    // For local dev server with self-signed cert, use https module directly
    const https = await import('https');
    response = await new Promise((resolve, reject) => {
      const req = https.get(url, { rejectUnauthorized: false }, (res) => {
        let data = '';
        res.on('data', chunk => data += chunk);
        res.on('end', () => {
          resolve({
            ok: res.statusCode >= 200 && res.statusCode < 300,
            status: res.statusCode,
            json: () => Promise.resolve(JSON.parse(data)),
            text: () => Promise.resolve(data)
          });
        });
      });
      req.on('error', reject);
    });
  } else {
    response = await fetch(url);
  }
  
  if (!response.ok) {
    const errorText = await response.text();
    throw new Error(`Bundle generation failed: ${errorText}`);
  }
  
  const data = await response.json();
  
  if (data.error) {
    throw new Error(`Bundle error: ${data.error}`);
  }
  
  // Decode base64 content
  const html = Buffer.from(data.content, 'base64').toString('utf8');
  
  console.log(`   ✓ Bundle received: ${data.sizeKB} KB`);
  console.log(`   ✓ Filename: ${data.filename}`);
  if (data.sourceCode) {
    console.log(`   ✓ Source lines: ${data.sourceCode.split('\n').length}`);
  }
  if (data.authorHandle) {
    console.log(`   ✓ Author: ${data.authorHandle}`);
  }
  if (data.userCode) {
    console.log(`   ✓ User: ${data.userCode}`);
  }
  if (data.depCount > 0) {
    console.log(`   ✓ Dependencies: ${data.depCount}`);
  }
  
  return {
    html,
    filename: data.filename,
    sizeKB: data.sizeKB,
    sourceCode: data.sourceCode,
    authorHandle: data.authorHandle,
    userCode: data.userCode,
    packDate: data.packDate,
    depCount: data.depCount,
  };
}

// ============================================================================
// IPFS Upload
// ============================================================================

/**
 * Upload a JSON object to IPFS via Pinata
 */
async function uploadJsonToIPFS(jsonData, name, credentials) {
  const jsonString = JSON.stringify(jsonData, null, 2);
  const blob = new Blob([jsonString], { type: 'application/json' });
  
  const formData = new FormData();
  formData.append('file', blob, 'metadata.json');
  formData.append('pinataMetadata', JSON.stringify({ name }));
  
  const response = await fetch(`${CONFIG.pinata.apiUrl}/pinning/pinFileToIPFS`, {
    method: 'POST',
    headers: {
      'pinata_api_key': credentials.pinataKey,
      'pinata_secret_api_key': credentials.pinataSecret
    },
    body: formData
  });
  
  if (!response.ok) {
    const error = await response.text();
    throw new Error(`Pinata JSON upload failed: ${error}`);
  }
  
  const result = await response.json();
  return `ipfs://${result.IpfsHash}`;
}

/**
 * Generate and upload thumbnail to IPFS via oven's grab-ipfs endpoint
 */
async function generateThumbnail(piece, credentials, options = {}) {
  const {
    format = 'webp',
    width = 96,         // Small thumbnail (was 512)
    height = 96,
    duration = 8000,    // 8 seconds
    fps = 10,           // 10fps capture
    playbackFps = 20,   // 20fps playback = 2x speed
    density = 2,        // 2x density for crisp pixels
    quality = 70,       // Lower quality for smaller files
    keepId = null,      // Tezos keep token ID for tracking
  } = options;
  
  console.log('\n📸 Generating thumbnail...');
  console.log(`   Oven: ${CONFIG.oven.url}`);
  
  // For local dev with self-signed certs, we need to disable cert verification
  const fetchOptions = {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
    },
    body: JSON.stringify({
      piece,
      format,
      width,
      height,
      duration,
      fps,
      playbackFps,
      density,
      quality,
      pinataKey: credentials.pinataKey,
      pinataSecret: credentials.pinataSecret,
      source: 'keep',
      keepId,
    }),
  };
  
  // Disable SSL verification for localhost (self-signed certs)
  if (CONFIG.oven.url.includes('localhost')) {
    const https = await import('https');
    fetchOptions.agent = new https.Agent({ rejectUnauthorized: false });
  }
  
  const response = await fetch(`${CONFIG.oven.url}/grab-ipfs`, fetchOptions);
  
  if (!response.ok) {
    const error = await response.text();
    throw new Error(`Thumbnail generation failed: ${error}`);
  }
  
  const result = await response.json();
  
  if (!result.success) {
    throw new Error(`Thumbnail generation failed: ${result.error}`);
  }
  
  console.log(`   ✅ Thumbnail uploaded: ${result.ipfsUri}`);
  console.log(`   Size: ${(result.size / 1024).toFixed(2)} KB`);
  
  return {
    ipfsUri: result.ipfsUri,
    mimeType: result.mimeType,
    size: result.size,
  };
}

async function uploadToIPFS(piece, options = {}) {
  const credentials = loadCredentials();
  
  if (!credentials.pinataKey || !credentials.pinataSecret) {
    throw new Error('❌ Pinata credentials not found in vault');
  }
  
  console.log('\n╔══════════════════════════════════════════════════════════════╗');
  console.log('║  📤 Uploading to IPFS via Pinata                             ║');
  console.log('╚══════════════════════════════════════════════════════════════╝\n');
  
  const pieceName = piece.replace(/^\$/, '');
  console.log(`📦 Piece: ${pieceName}`);
  
  // Detect content type if not provided
  let contentType = options.contentType;
  if (!contentType) {
    console.log('🔍 Detecting content type...');
    const detection = await detectContentType(piece);
    contentType = detection.type;
    console.log(`   ✓ Detected: ${contentType}`);
  }
  
  // Get bundle from Netlify endpoint (proper self-contained bundle)
  let bundleHtml;
  let bundleMeta = {};
  if (options.bundleHtml) {
    bundleHtml = options.bundleHtml;
    console.log('   Using provided bundle HTML');
  } else {
    const bundle = await fetchBundleFromNetlify(piece, contentType);
    bundleHtml = bundle.html;
    bundleMeta = {
      sourceCode: bundle.sourceCode,
      authorHandle: bundle.authorHandle,
      userCode: bundle.userCode,
      packDate: bundle.packDate,
      depCount: bundle.depCount,
    };
  }
  
  // Use piece name as the unique identifier (simple and deterministic)
  console.log(`🔐 Piece ID: ${pieceName}`);
  
  // Upload to Pinata
  const formData = new FormData();
  const blob = new Blob([bundleHtml], { type: 'text/html' });
  formData.append('file', blob, 'index.html');
  
  formData.append('pinataMetadata', JSON.stringify({
    name: `aesthetic.computer-keep-${pieceName}`
  }));
  
  formData.append('pinataOptions', JSON.stringify({
    wrapWithDirectory: true
  }));
  
  console.log('📤 Uploading to IPFS...');
  
  const response = await fetch(`${CONFIG.pinata.apiUrl}/pinning/pinFileToIPFS`, {
    method: 'POST',
    headers: {
      'pinata_api_key': credentials.pinataKey,
      'pinata_secret_api_key': credentials.pinataSecret
    },
    body: formData
  });
  
  if (!response.ok) {
    const error = await response.text();
    throw new Error(`Pinata upload failed: ${error}`);
  }
  
  const result = await response.json();
  const cid = result.IpfsHash;
  
  console.log(`\n✅ Uploaded to IPFS!`);
  console.log(`   CID: ${cid}`);
  console.log(`   Gateway: ${CONFIG.pinata.gateway}/ipfs/${cid}`);
  console.log(`   IPFS URI: ipfs://${cid}\n`);
  
  return {
    cid,
    contentHash: pieceName,  // Use piece name as unique key
    contentType,
    gatewayUrl: `${CONFIG.pinata.gateway}/ipfs/${cid}`,
    ipfsUri: `ipfs://${cid}`,
    // Bundle metadata for KidLisp pieces
    ...bundleMeta,
  };
}

// ============================================================================
// Mint Token
// ============================================================================

// Helper to convert string to hex bytes (TZIP-21 format - raw UTF-8 hex, no pack prefix)
function stringToBytes(str) {
  return Buffer.from(str, 'utf8').toString('hex');
}

const KEEP_PERMIT_PAYLOAD_TYPE = {
  prim: 'pair',
  args: [
    { prim: 'address', annots: ['%contract'] },
    {
      prim: 'pair',
      args: [
        { prim: 'address', annots: ['%owner'] },
        {
          prim: 'pair',
          args: [
            { prim: 'bytes', annots: ['%content_hash'] },
            { prim: 'timestamp', annots: ['%permit_deadline'] },
          ],
        },
      ],
    },
  ],
};

async function loadPermitSigner() {
  const connectionString = process.env.MONGODB_CONNECTION_STRING;
  const dbName = process.env.MONGODB_NAME;
  if (!connectionString || !dbName) return null;
  const client = new MongoClient(connectionString, { serverSelectionTimeoutMS: 8000, connectTimeoutMS: 8000 });
  try {
    await client.connect();
    const secrets = await client.db(dbName).collection('secrets').findOne({ _id: KEEPS_SECRET_ID });
    const privateKey = secrets?.keepPermitSignerPrivateKey || secrets?.keepPermitPrivateKey || secrets?.privateKey;
    if (!privateKey) return null;
    return new InMemorySigner(privateKey);
  } catch {
    return null;
  } finally {
    await client.close().catch(() => {});
  }
}

async function buildKeepPermit({ signer, contractAddress, owner, contentHashBytes, deadlineIso = null }) {
  if (!contractAddress || !owner || !contentHashBytes) {
    throw new Error('Missing keep permit fields (contractAddress, owner, contentHashBytes)');
  }

  const permitDeadline = deadlineIso || new Date(Date.now() + KEEP_PERMIT_TTL_MS).toISOString();
  const payloadData = {
    prim: 'Pair',
    args: [
      { string: contractAddress },
      {
        prim: 'Pair',
        args: [
          { string: owner },
          {
            prim: 'Pair',
            args: [
              { bytes: contentHashBytes },
              { string: permitDeadline },
            ],
          },
        ],
      },
    ],
  };

  const packed = packDataBytes(payloadData, KEEP_PERMIT_PAYLOAD_TYPE).bytes;
  const signature = await signer.sign(packed);

  return {
    permit_deadline: permitDeadline,
    keep_permit: signature.prefixSig,
  };
}

async function mintToken(piece, options = {}) {
  const { network = 'mainnet', generateThumbnail: shouldGenerateThumbnail = false, recipient = null, skipConfirm = false } = options;
  
  const { tezos, credentials, config } = await createTezosClient(network);
  
  // Determine owner: recipient if specified, otherwise the server wallet
  const ownerAddress = recipient || credentials.address;
  const allCredentials = loadCredentials(); // For Pinata access
  
  // Load contract address (network-specific)
  const addressPath = getContractAddressPath(network);
  if (!fs.existsSync(addressPath)) {
    throw new Error(`❌ No contract deployed on ${network}. Run: node keeps.mjs deploy ${network}`);
  }
  
  const contractAddress = fs.readFileSync(addressPath, 'utf8').trim();
  const pieceName = piece.replace(/^\$/, '');
  
  // Fetch contract storage for status info
  const contract = await tezos.contract.at(contractAddress);
  const storage = await contract.storage();
  const nextTokenId = storage.next_token_id.toNumber();
  const keepFee = storage.keep_fee ? storage.keep_fee.toNumber() / 1_000_000 : 0;
  
  // Fetch contract balance
  const contractBalance = await tezos.tz.getBalance(contractAddress);
  const contractBalanceXTZ = contractBalance.toNumber() / 1_000_000;
  
  // Get objkt base URL
  const objktBase = network === 'mainnet' ? 'https://objkt.com' : 'https://ghostnet.objkt.com';
  
  console.log('\n╔══════════════════════════════════════════════════════════════╗');
  console.log('║  📜 Keeping a KidLisp Piece                                  ║');
  console.log('╚══════════════════════════════════════════════════════════════╝\n');
  
  console.log('Keep your KidLisp as a unique digital token.\n');
  
  console.log('─────────────────── Piece ───────────────────');
  console.log(`   Code:        $${pieceName}`);
  console.log(`   Preview:     https://aesthetic.computer/$${pieceName}`);
  if (ownerAddress !== credentials.address) {
    console.log(`   Recipient:   ${ownerAddress}`);
  }
  console.log(`   Thumbnail:   ${shouldGenerateThumbnail ? 'Animated WebP (via Oven)' : 'Static PNG (HTTP grab)'}`);
  
  console.log('\n─────────────────── Contract ───────────────────');
  console.log(`   Address:     ${contractAddress}`);
  console.log(`   Network:     ${config.name}`);
  console.log(`   Explorer:    ${config.explorer}/${contractAddress}`);
  console.log(`   Collection:  ${objktBase}/collection/${contractAddress}`);
  console.log(`   Admin:       ${storage.administrator}`);
  console.log(`   Balance:     ${contractBalanceXTZ.toFixed(6)} XTZ`);
  console.log(`   Keeps:       ${nextTokenId} total`);
  console.log(`   Next ID:     #${nextTokenId}`);
  if (keepFee > 0) {
    console.log(`   Keep Fee:    ${keepFee} XTZ`);
  }
  
  console.log('\n─────────────────── Wallet ───────────────────');
  console.log(`   Address:     ${credentials.address}`);
  console.log(`   Wallet:      ${credentials.wallet || 'default'}`);
  const walletBalance = await tezos.tz.getBalance(credentials.address);
  console.log(`   Balance:     ${(walletBalance.toNumber() / 1_000_000).toFixed(6)} XTZ\n`);
  
  // Confirmation prompt (unless skipped)
  if (!skipConfirm) {
    const rl = readline.createInterface({ input: process.stdin, output: process.stdout });
    const answer = await new Promise(resolve => {
      rl.question('Keep this piece? (y/N): ', resolve);
    });
    rl.close();
    
    if (answer.toLowerCase() !== 'y' && answer.toLowerCase() !== 'yes') {
      console.log('\n❌ Cancelled.\n');
      process.exit(0);
    }
    console.log('');
  }
  
  // Check for duplicate BEFORE uploading to IPFS
  console.log('🔍 Checking for duplicates on-chain...');
  const duplicate = await checkDuplicatePiece(pieceName, contractAddress, network);
  if (duplicate.exists) {
    throw new Error(`Duplicate! $${pieceName} was already kept as token #${duplicate.tokenId}`);
  }
  console.log('   ✓ No duplicate found');
  
  // Detect content type
  let contentType = options.contentType;
  if (!contentType) {
    console.log('🔍 Detecting content type...');
    const detection = await detectContentType(piece);
    contentType = detection.type;
    console.log(`   ✓ Detected: ${contentType}`);
  }
  console.log(`📁 Content Type: ${contentType}`);
  
  // Upload HTML bundle to IPFS if not provided
  let artifactUri = options.ipfsUri;
  let artifactCid = options.contentHash;
  let contentHash = null;  // Source-based hash for duplicate prevention
  let sourceCode = null;
  let authorHandle = null;
  let userCode = null;
  let packDate = null;
  let depCount = 0;
  
  if (!artifactUri) {
    console.log('\n📤 Uploading HTML bundle to IPFS...');
    const upload = await uploadToIPFS(piece, { contentType });
    artifactUri = `ipfs://${upload.cid}`;  // Use ipfs:// URI for artifact
    artifactCid = upload.cid;
    contentHash = upload.contentHash;  // Source-based hash for uniqueness
    contentType = upload.contentType;
    // Capture bundle metadata for KidLisp pieces
    sourceCode = upload.sourceCode;
    authorHandle = upload.authorHandle;
    userCode = upload.userCode;
    packDate = upload.packDate;
    depCount = upload.depCount || 0;
  }
  
  console.log(`🔗 Artifact URI: ${artifactUri}`);
  console.log(`💾 Artifact CID: ${artifactCid}`);
  console.log(`🔐 Content Hash: ${contentHash}`);
  
  // Build TZIP-21 compliant JSON metadata for objkt
  // Token name is just the code (e.g., "$roz")
  const tokenName = `$${pieceName}`;
  const acUrl = `https://aesthetic.computer/$${pieceName}`;
  
  // Build author display name for attributes
  let authorDisplayName = null;
  if (authorHandle && authorHandle !== '@anon') {
    authorDisplayName = authorHandle;
  }
  
  // Description is ONLY the KidLisp source code (clean and simple)
  const description = sourceCode || `A KidLisp piece preserved on Tezos`;
  
  // v9 metadata policy: single canonical tag only
  const tags = ['KidLisp'];
  
  // Generate and upload thumbnail to IPFS if requested
  let thumbnailUri = `https://grab.aesthetic.computer/preview/400x400/$${pieceName}.png`; // HTTP fallback
  let thumbnailMimeType = 'image/png';
  
  if (shouldGenerateThumbnail) {
    try {
      const thumbnail = await generateThumbnail(piece, allCredentials, {
        format: 'webp',
        width: 512,
        height: 512,
        duration: 12000,
        fps: 7.5,
        playbackFps: 15,
        quality: 90,
      });
      thumbnailUri = thumbnail.ipfsUri;
      thumbnailMimeType = thumbnail.mimeType;
    } catch (error) {
      console.warn(`   ⚠️ Thumbnail generation failed: ${error.message}`);
      console.warn(`   Using HTTP fallback: ${thumbnailUri}`);
    }
  }
  
  // creators array contains just the wallet address for on-chain attribution
  // objkt.com uses firstMinter for artist display
  const creatorsArray = [credentials.address];
  
  const metadataJson = {
    name: tokenName,
    description: description,
    artifactUri: artifactUri,
    displayUri: artifactUri,
    thumbnailUri: thumbnailUri,
    decimals: 0,
    symbol: 'KEEP',
    isBooleanAmount: true,
    shouldPreferSymbol: false,
    minter: authorHandle || credentials.address,
    creators: creatorsArray,
    rights: '© All rights reserved',
    mintingTool: 'https://kidlisp.com',
    formats: [{
      uri: artifactUri,
      mimeType: 'text/html',
      dimensions: { value: 'responsive', unit: 'viewport' }
    }],
    tags: tags,
    attributes: [
      { name: 'Language', value: 'KidLisp' },
      { name: 'Code', value: `$${pieceName}` },
      ...(authorDisplayName ? [{ name: 'Author', value: authorDisplayName }] : []),
      ...(userCode ? [{ name: 'User', value: userCode }] : []),
      ...(sourceCode ? [{ name: 'Lines of Code', value: String(sourceCode.split('\n').length) }] : []),
      ...(packDate ? [{ name: 'Packed on', value: packDate }] : []),
      { name: 'Interactive', value: 'Yes' },
      { name: 'Platform', value: 'Aesthetic Computer' },
    ]
  };
  
  // Upload JSON metadata to IPFS
  console.log('\n📤 Uploading JSON metadata to IPFS...');
  const metadataUri = await uploadJsonToIPFS(
    metadataJson, 
    `aesthetic.computer-keep-${pieceName}-metadata`,
    allCredentials
  );
  console.log(`📋 Metadata URI: ${metadataUri}`);
  
  // For the contract, we only need to store the metadata URI in the "" key
  // All other fields will be fetched by indexers from the JSON
  const onChainMetadata = {
    name: stringToBytes(tokenName),
    description: stringToBytes(description),
    artifactUri: stringToBytes(artifactUri),
    displayUri: stringToBytes(artifactUri),
    thumbnailUri: stringToBytes(thumbnailUri),
    decimals: stringToBytes('0'),
    symbol: stringToBytes('KEEP'),
    isBooleanAmount: stringToBytes('true'),
    shouldPreferSymbol: stringToBytes('false'),
    formats: stringToBytes(JSON.stringify(metadataJson.formats)),
    tags: stringToBytes(JSON.stringify(metadataJson.tags)),
    attributes: stringToBytes(JSON.stringify(metadataJson.attributes)),
    creators: stringToBytes(JSON.stringify(creatorsArray)),
    rights: stringToBytes('© All rights reserved'),
    content_type: stringToBytes('KidLisp'),
    content_hash: stringToBytes(contentHash),  // Source-based hash for uniqueness
    // IMPORTANT: This is the off-chain metadata URI that objkt will fetch
    metadata_uri: stringToBytes(metadataUri),
  };
  
  // Call keep entrypoint
  console.log('\n📤 Preserving on Tezos blockchain...');

  try {
    // Use the backend permit signer key (from MongoDB), not the wallet key
    const permitSigner = await loadPermitSigner() || tezos.signer;
    const keepPermit = await buildKeepPermit({
      signer: permitSigner,
      contractAddress,
      owner: ownerAddress,
      contentHashBytes: onChainMetadata.content_hash,
    });

    // Build royalties JSON (objkt standard: decimals 4, shares in bps)
    const storage = await contract.storage();
    const artistBps = storage.artist_royalty_bps ? storage.artist_royalty_bps.toNumber() : (storage.default_royalty_bps ? storage.default_royalty_bps.toNumber() : 1000);
    const platformBps = storage.platform_royalty_bps ? storage.platform_royalty_bps.toNumber() : 0;
    const treasuryAddr = storage.treasury_address || null;
    const royaltiesObj = { decimals: 4, shares: { [ownerAddress]: String(artistBps) } };
    if (platformBps > 0 && treasuryAddr) royaltiesObj.shares[treasuryAddr] = String(platformBps);
    const royaltiesBytes = stringToBytes(JSON.stringify(royaltiesObj));

    const op = await contract.methodsObject.keep({
      artifactUri: onChainMetadata.artifactUri,
      content_hash: onChainMetadata.content_hash,
      creators: onChainMetadata.creators,
      decimals: onChainMetadata.decimals,
      description: onChainMetadata.description,
      displayUri: onChainMetadata.displayUri,
      metadata_uri: onChainMetadata.metadata_uri,
      name: onChainMetadata.name,
      owner: ownerAddress,
      royalties: royaltiesBytes,
      symbol: onChainMetadata.symbol,
      thumbnailUri: onChainMetadata.thumbnailUri,
      permit_deadline: keepPermit.permit_deadline,
      keep_permit: keepPermit.keep_permit,
    }).send();
    
    console.log(`   ⏳ Operation hash: ${op.hash}`);
    console.log('   ⏳ Waiting for confirmation...');
    
    await op.confirmation(1);
    
    // Get the token ID from contract storage (next_token_id - 1)
    // This is O(1) and scales to millions of tokens
    const updatedStorage = await contract.storage();
    const tokenId = updatedStorage.next_token_id.toNumber() - 1;
    
    console.log('\n╔══════════════════════════════════════════════════════════════╗');
    console.log('║  ✅ Piece Kept Successfully!                                 ║');
    console.log('╚══════════════════════════════════════════════════════════════╝\n');
    
    console.log(`   🎨 Token ID: #${tokenId}`);
    console.log(`   📦 Piece: $${pieceName}`);
    console.log(`   🔗 Artifact: ${artifactUri}`);
    console.log(`   📝 Operation: ${config.explorer}/${op.hash}`);
    console.log(`   🖼️  View on Objkt: ${objktBase}/asset/${contractAddress}/${tokenId}\n`);
    
    return { tokenId, hash: op.hash, artifactUri };
    
  } catch (error) {
    console.error('\n❌ Keep failed!');
    console.error(`   Error: ${error.message}`);
    throw error;
  }
}

// ============================================================================
// Update Metadata
// ============================================================================

async function updateMetadata(tokenId, piece, options = {}) {
  const { network = 'mainnet', generateThumbnail: shouldGenerateThumbnail = false } = options;
  
  const { tezos, credentials, config } = await createTezosClient(network);
  const allCredentials = loadCredentials(); // For Pinata access
  
  // Load contract address (network-specific)
  const addressPath = getContractAddressPath(network);
  if (!fs.existsSync(addressPath)) {
    throw new Error(`❌ No contract deployed on ${network}. Run: node keeps.mjs deploy ${network}`);
  }
  
  const contractAddress = fs.readFileSync(addressPath, 'utf8').trim();
  const pieceName = piece.replace(/^\$/, '');
  const acUrl = `https://aesthetic.computer/$${pieceName}`;
  
  console.log('\n╔══════════════════════════════════════════════════════════════╗');
  console.log('║  🔄 Updating Token Metadata                                  ║');
  console.log('╚══════════════════════════════════════════════════════════════╝\n');
  
  console.log(`📡 Network: ${config.name}`);
  console.log(`📍 Contract: ${contractAddress}`);
  console.log(`🎨 Token ID: ${tokenId}`);
  console.log(`📦 Piece: ${pieceName}`);
  
  // Detect content type
  console.log('🔍 Detecting content type...');
  const detection = await detectContentType(piece);
  const contentType = detection.type;
  console.log(`   ✓ Detected: ${contentType}`);
  
  // Upload new bundle to IPFS (skip duplicate check since we're updating)
  console.log('\n📤 Uploading new bundle to IPFS...');
  const upload = await uploadToIPFS(piece, { contentType, skipDuplicateCheck: true });
  const artifactUri = `ipfs://${upload.cid}`;
  const artifactCid = upload.cid;
  
  // Get bundle metadata
  const sourceCode = upload.sourceCode;
  const authorHandle = upload.authorHandle;
  const userCode = upload.userCode;
  const packDate = upload.packDate;
  const depCount = upload.depCount || 0;
  
  console.log(`🔗 New Artifact URI: ${artifactUri}`);
  
  // Build author display name for attributes
  let authorDisplayName = null;
  if (authorHandle && authorHandle !== '@anon') {
    authorDisplayName = authorHandle;
  }
  
  // Description is ONLY the KidLisp source code (clean and simple)
  const description = sourceCode || `A KidLisp piece preserved on Tezos`;
  
  // v9 metadata policy: single canonical tag only
  const tags = ['KidLisp'];
  
  // Build improved attributes
  const attributes = [
    { name: 'Language', value: 'KidLisp' },
    { name: 'Code', value: `$${pieceName}` },
    ...(authorDisplayName ? [{ name: 'Author', value: authorDisplayName }] : []),
    ...(userCode ? [{ name: 'User', value: userCode }] : []),
    ...(sourceCode ? [{ name: 'Lines of Code', value: String(sourceCode.split('\n').length) }] : []),
    ...(packDate ? [{ name: 'Packed on', value: packDate }] : []),
    { name: 'Interactive', value: 'Yes' },
    { name: 'Platform', value: 'Aesthetic Computer' },
  ];
  
  // Preserve the ORIGINAL creator from firstMinter on TzKT
  // This ensures artist attribution is maintained on updates
  let originalCreator = credentials.address; // fallback to current wallet
  try {
    const tzktBase = network === 'mainnet' ? 'https://api.tzkt.io' : `https://api.${network}.tzkt.io`;
    const tokenUrl = `${tzktBase}/v1/tokens?contract=${contractAddress}&tokenId=${tokenId}`;
    const tokenResponse = await fetch(tokenUrl);
    if (tokenResponse.ok) {
      const tokens = await tokenResponse.json();
      if (tokens[0]?.firstMinter?.address) {
        originalCreator = tokens[0].firstMinter.address;
        console.log(`   ✓ Preserving original creator: ${originalCreator}`);
      }
    }
  } catch (e) {
    console.warn(`   ⚠ Could not fetch original creator, using current wallet`);
  }
  
  const creatorsArray = [originalCreator];
  
  // Generate thumbnail via oven if requested, otherwise use HTTP fallback
  let thumbnailUri = `https://grab.aesthetic.computer/preview/400x400/$${pieceName}.png`;
  
  if (shouldGenerateThumbnail) {
    try {
      const thumbnail = await generateThumbnail(pieceName, allCredentials, {
        width: 256,
        height: 256,
        duration: 8000,
        fps: 10,
        playbackFps: 20,
        density: 2,
        quality: 70,
      });
      thumbnailUri = thumbnail.ipfsUri;
      console.log(`   🖼️  Thumbnail: ${thumbnailUri}`);
    } catch (err) {
      console.warn(`   ⚠ Thumbnail generation failed, using HTTP fallback: ${err.message}`);
    }
  }
  
  // Build metadata JSON for IPFS
  const tokenName = `$${pieceName}`;
  const metadataJson = {
    name: tokenName,
    description: description,
    artifactUri: artifactUri,
    displayUri: artifactUri,
    thumbnailUri: thumbnailUri,
    decimals: 0,
    symbol: 'KEEP',
    isBooleanAmount: true,
    shouldPreferSymbol: false,
    minter: authorHandle || credentials.address,
    creators: creatorsArray,
    rights: '© All rights reserved',
    mintingTool: 'https://kidlisp.com',
    formats: [{
      uri: artifactUri,
      mimeType: 'text/html',
      dimensions: { value: 'responsive', unit: 'viewport' }
    }],
    tags: tags,
    attributes: attributes
  };
  
  // Upload JSON metadata to IPFS
  console.log('\n📤 Uploading JSON metadata to IPFS...');
  const metadataUri = await uploadJsonToIPFS(
    metadataJson, 
    `aesthetic.computer-keep-${pieceName}-metadata-updated`,
    allCredentials
  );
  console.log(`📋 Metadata URI: ${metadataUri}`);
  
  // Build on-chain token_info
  const tokenInfo = {
    name: stringToBytes(tokenName),
    description: stringToBytes(description),
    artifactUri: stringToBytes(artifactUri),
    displayUri: stringToBytes(artifactUri),
    thumbnailUri: stringToBytes(metadataJson.thumbnailUri),
    decimals: stringToBytes('0'),
    symbol: stringToBytes('KEEP'),
    isBooleanAmount: stringToBytes('true'),
    shouldPreferSymbol: stringToBytes('false'),
    formats: stringToBytes(JSON.stringify(metadataJson.formats)),
    tags: stringToBytes(JSON.stringify(tags)),
    attributes: stringToBytes(JSON.stringify(attributes)),
    creators: stringToBytes(JSON.stringify(creatorsArray)),
    rights: stringToBytes('© All rights reserved'),
    content_type: stringToBytes('KidLisp'),
    content_hash: stringToBytes(pieceName),
    '': stringToBytes(metadataUri)
  };
  
  // Call edit_metadata entrypoint
  console.log('\n📤 Calling edit_metadata entrypoint...');
  
  try {
    const contract = await tezos.contract.at(contractAddress);
    
    const op = await contract.methodsObject.edit_metadata({
      token_id: tokenId,
      token_info: tokenInfo
    }).send();
    
    console.log(`   ⏳ Operation hash: ${op.hash}`);
    console.log('   ⏳ Waiting for confirmation...');
    
    await op.confirmation(1);
    
    console.log('\n╔══════════════════════════════════════════════════════════════╗');
    console.log('║  ✅ Metadata Updated Successfully!                           ║');
    console.log('╚══════════════════════════════════════════════════════════════╝\n');
    
    console.log(`   🎨 Token ID: ${tokenId}`);
    console.log(`   🔗 New Artifact: ${artifactUri}`);
    console.log(`   📝 Operation: ${config.explorer}/${op.hash}\n`);
    
    return { tokenId, hash: op.hash, artifactUri };
    
  } catch (error) {
    console.error('\n❌ Update failed!');
    console.error(`   Error: ${error.message}`);
    if (error.message.includes('METADATA_LOCKED')) {
      console.error('\n   💡 This token\'s metadata has been locked and cannot be updated.');
    }
    throw error;
  }
}

// ============================================================================
// Redact Token (Censor)
// ============================================================================

async function redactToken(tokenId, options = {}) {
  const { network = 'mainnet', reason = 'Content has been redacted.' } = options;
  
  const { tezos, credentials, config } = await createTezosClient(network);
  const allCredentials = loadCredentials();
  
  // Load contract address (network-specific)
  const addressPath = getContractAddressPath(network);
  if (!fs.existsSync(addressPath)) {
    throw new Error(`❌ No contract deployed on ${network}. Run: node keeps.mjs deploy ${network}`);
  }
  
  const contractAddress = fs.readFileSync(addressPath, 'utf8').trim();
  
  console.log('\n╔══════════════════════════════════════════════════════════════╗');
  console.log('║  🚫 Redacting Token                                          ║');
  console.log('╚══════════════════════════════════════════════════════════════╝\n');
  
  console.log(`📡 Network: ${config.name}`);
  console.log(`📍 Contract: ${contractAddress}`);
  console.log(`🎨 Token ID: ${tokenId}`);
  console.log(`📝 Reason: ${reason}`);
  console.log('\n⚠️  This will replace all content with a redacted placeholder.\n');
  
  // Generate a red "REDACTED" image
  console.log('🖼️  Generating redacted thumbnail...');
  
  // Create a simple red HTML page for the artifact
  const redactedHtml = `<!DOCTYPE html>
<html>
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>REDACTED</title>
  <style>
    * { margin: 0; padding: 0; box-sizing: border-box; }
    body {
      background: #1a0000;
      color: #ff0000;
      font-family: monospace;
      display: flex;
      align-items: center;
      justify-content: center;
      min-height: 100vh;
      text-align: center;
    }
    .container {
      padding: 2rem;
    }
    h1 {
      font-size: 3rem;
      letter-spacing: 0.5em;
      margin-bottom: 1rem;
      text-shadow: 0 0 20px #ff0000;
    }
    p {
      font-size: 1rem;
      opacity: 0.7;
    }
    .bars {
      display: flex;
      flex-direction: column;
      gap: 8px;
      margin-top: 2rem;
    }
    .bar {
      height: 20px;
      background: #ff0000;
      opacity: 0.3;
    }
    .bar:nth-child(1) { width: 80%; }
    .bar:nth-child(2) { width: 60%; }
    .bar:nth-child(3) { width: 90%; }
    .bar:nth-child(4) { width: 45%; }
  </style>
</head>
<body>
  <div class="container">
    <h1>REDACTED</h1>
    <p>${reason}</p>
    <div class="bars">
      <div class="bar"></div>
      <div class="bar"></div>
      <div class="bar"></div>
      <div class="bar"></div>
    </div>
  </div>
</body>
</html>`;

  // Upload redacted HTML to IPFS
  console.log('📤 Uploading redacted artifact to IPFS...');
  
  const formData = new FormData();
  const blob = new Blob([redactedHtml], { type: 'text/html' });
  formData.append('file', blob, 'index.html');
  formData.append('pinataMetadata', JSON.stringify({
    name: `aesthetic.computer-redacted-${tokenId}`
  }));
  formData.append('pinataOptions', JSON.stringify({
    wrapWithDirectory: true
  }));
  
  const uploadResponse = await fetch(`${CONFIG.pinata.apiUrl}/pinning/pinFileToIPFS`, {
    method: 'POST',
    headers: {
      'pinata_api_key': allCredentials.pinataKey,
      'pinata_secret_api_key': allCredentials.pinataSecret
    },
    body: formData
  });
  
  if (!uploadResponse.ok) {
    throw new Error(`Failed to upload redacted artifact: ${await uploadResponse.text()}`);
  }
  
  const uploadResult = await uploadResponse.json();
  const artifactCid = uploadResult.IpfsHash;
  const artifactUri = `ipfs://${artifactCid}`;
  console.log(`   ✓ Artifact: ${artifactUri}`);
  
  // Generate red thumbnail via Oven
  console.log('📸 Generating redacted thumbnail via Oven...');
  let thumbnailUri = 'https://grab.aesthetic.computer/preview/400x400/redacted.png';
  
  try {
    // Use oven to capture the redacted page
    const ovenResponse = await fetch(`${CONFIG.oven.url}/grab-ipfs`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({
        url: `${CONFIG.pinata.gateway}/ipfs/${artifactCid}`,
        format: 'webp',
        width: 96,
        height: 96,
        density: 2,
        duration: 1000,
        fps: 1,
        quality: 80,
        pinataKey: allCredentials.pinataKey,
        pinataSecret: allCredentials.pinataSecret,
      }),
    });
    
    if (ovenResponse.ok) {
      const ovenResult = await ovenResponse.json();
      if (ovenResult.success) {
        thumbnailUri = ovenResult.ipfsUri;
        console.log(`   ✓ Thumbnail: ${thumbnailUri}`);
      }
    }
  } catch (e) {
    console.log(`   ⚠️ Thumbnail generation failed, using fallback`);
  }
  
  // Build redacted metadata
  const tokenName = '[REDACTED]';
  const description = `[REDACTED]\n\n${reason}`;
  
  const tags = ['REDACTED', 'censored'];
  const attributes = [
    { name: 'Status', value: 'REDACTED' },
    { name: 'Reason', value: reason },
    { name: 'Platform', value: 'Aesthetic Computer' },
  ];
  
  // For redacted content, use admin wallet as creator (censorship action)
  const creatorsArray = [credentials.address];
  
  // Upload metadata JSON
  const metadataJson = {
    name: tokenName,
    description: description,
    artifactUri: artifactUri,
    displayUri: artifactUri,
    thumbnailUri: thumbnailUri,
    decimals: 0,
    symbol: 'KEEP',
    isBooleanAmount: true,
    shouldPreferSymbol: false,
    minter: '@aesthetic',
    creators: creatorsArray,
    rights: '© All rights reserved',
    mintingTool: 'https://kidlisp.com',
    formats: [{
      uri: artifactUri,
      mimeType: 'text/html',
      dimensions: { value: 'responsive', unit: 'viewport' }
    }],
    tags: tags,
    attributes: attributes
  };
  
  console.log('📤 Uploading redacted metadata to IPFS...');
  const metadataUri = await uploadJsonToIPFS(
    metadataJson, 
    `aesthetic.computer-redacted-${tokenId}-metadata`,
    allCredentials
  );
  console.log(`   ✓ Metadata: ${metadataUri}`);
  
  // Build on-chain token_info
  const tokenInfo = {
    name: stringToBytes(tokenName),
    description: stringToBytes(description),
    artifactUri: stringToBytes(artifactUri),
    displayUri: stringToBytes(artifactUri),
    thumbnailUri: stringToBytes(thumbnailUri),
    decimals: stringToBytes('0'),
    symbol: stringToBytes('KEEP'),
    isBooleanAmount: stringToBytes('true'),
    shouldPreferSymbol: stringToBytes('false'),
    formats: stringToBytes(JSON.stringify(metadataJson.formats)),
    tags: stringToBytes(JSON.stringify(tags)),
    attributes: stringToBytes(JSON.stringify(attributes)),
    creators: stringToBytes(JSON.stringify(creatorsArray)),
    rights: stringToBytes('© All rights reserved'),
    content_type: stringToBytes('REDACTED'),
    content_hash: stringToBytes('REDACTED'),
    '': stringToBytes(metadataUri)
  };
  
  // Call edit_metadata entrypoint
  console.log('\n📤 Calling edit_metadata entrypoint...');
  
  try {
    const contract = await tezos.contract.at(contractAddress);
    
    const op = await contract.methodsObject.edit_metadata({
      token_id: tokenId,
      token_info: tokenInfo
    }).send();
    
    console.log(`   ⏳ Operation hash: ${op.hash}`);
    console.log('   ⏳ Waiting for confirmation...');
    
    await op.confirmation(1);
    
    console.log('\n╔══════════════════════════════════════════════════════════════╗');
    console.log('║  🚫 Token Redacted Successfully!                             ║');
    console.log('╚══════════════════════════════════════════════════════════════╝\n');
    
    console.log(`   🎨 Token ID: ${tokenId}`);
    console.log(`   🚫 Status: REDACTED`);
    console.log(`   📝 Operation: ${config.explorer}/${op.hash}\n`);
    
    return { tokenId, hash: op.hash, redacted: true };
    
  } catch (error) {
    console.error('\n❌ Redaction failed!');
    console.error(`   Error: ${error.message}`);
    if (error.message.includes('METADATA_LOCKED')) {
      console.error('\n   💡 This token\'s metadata has been locked and cannot be redacted.');
    }
    throw error;
  }
}

// ============================================================================
// Set Collection Media (Contract-level Metadata)
// ============================================================================

async function setCollectionMedia(options = {}) {
  const { 
    network = 'mainnet',
    name,           // Collection name
    imageUri,       // Collection icon/logo (IPFS URI or URL)
    homepage,       // Collection homepage URL
    description,    // Collection description
    raw = {}        // Raw key-value pairs to set
  } = options;
  
  const { tezos, credentials, config } = await createTezosClient(network);
  
  // Load contract address (network-specific)
  const addressPath = getContractAddressPath(network);
  if (!fs.existsSync(addressPath)) {
    throw new Error(`❌ No contract deployed on ${network}. Run: node keeps.mjs deploy ${network}`);
  }
  
  const contractAddress = fs.readFileSync(addressPath, 'utf8').trim();
  
  console.log('\n╔══════════════════════════════════════════════════════════════╗');
  console.log('║  🎨 Setting Collection Media                                 ║');
  console.log('╚══════════════════════════════════════════════════════════════╝\n');
  
  console.log(`📡 Network: ${config.name}`);
  console.log(`📍 Contract: ${contractAddress}\n`);
  
  // Build the metadata updates
  const updates = [];
  const contract = await tezos.contract.at(contractAddress);

  // Load existing contract metadata so partial updates don't wipe fields.
  let existingMetadata = {};
  try {
    const storage = await contract.storage();
    const existingContent = await storage.metadata.get('content');
    const decoded = decodeContractMetadataBytes(existingContent);
    if (decoded && typeof decoded === 'object') {
      existingMetadata = decoded;
    }
  } catch (error) {
    console.warn(`   ⚠️  Could not read existing metadata, using defaults: ${error.message}`);
  }

  // Build new contract metadata JSON (merge existing + updates)
  const currentMetadata = {
    ...existingMetadata,
    name: name || existingMetadata.name || "KidLisp Keeps",
    version: existingMetadata.version || "2.0.0",
    interfaces: existingMetadata.interfaces || ["TZIP-012", "TZIP-016", "TZIP-021"],
    authors: existingMetadata.authors || ["aesthetic.computer"],
    homepage: homepage || existingMetadata.homepage || "https://keep.kidlisp.com"
  };
  
  if (options.name) {
    console.log(`   📛 Name: ${options.name}`);
  }
  
  if (imageUri) {
    currentMetadata.imageUri = imageUri;
    currentMetadata.thumbnailUri = imageUri;
    console.log(`   🖼️  Image URI: ${imageUri}`);
  }
  
  if (description) {
    currentMetadata.description = description;
    console.log(`   📝 Description: ${description.substring(0, 80)}...`);
  }

  if (homepage) {
    console.log(`   🏠 Homepage: ${homepage}`);
  }
  
  // Add any raw fields
  for (const [key, value] of Object.entries(raw)) {
    currentMetadata[key] = value;
    console.log(`   📦 ${key}: ${String(value).substring(0, 50)}`);
  }
  
  // Update the "content" key with new metadata JSON
  const metadataJson = JSON.stringify(currentMetadata);
  const metadataBytes = stringToBytes(metadataJson);
  
  updates.push({ key: 'content', value: metadataBytes });
  
  console.log(`\n📤 Updating contract metadata...`);
  
  try {
    // Format for set_contract_metadata: list of { key: string, value: bytes }
    // Bytes must be hex string prefixed with 0x for Taquito
    const params = updates.map(u => ({
      key: u.key,
      value: '0x' + u.value
    }));
    
    const op = await contract.methods.set_contract_metadata(params).send();
    
    console.log(`   ⏳ Operation hash: ${op.hash}`);
    console.log('   ⏳ Waiting for confirmation...');
    
    await op.confirmation(1);
    
    console.log('\n╔══════════════════════════════════════════════════════════════╗');
    console.log('║  ✅ Collection Media Updated!                                ║');
    console.log('╚══════════════════════════════════════════════════════════════╝\n');
    
    console.log(`   📝 Operation: ${config.explorer}/${op.hash}\n`);
    
    return { hash: op.hash, metadata: currentMetadata };
    
  } catch (error) {
    console.error('\n❌ Update failed!');
    console.error(`   Error: ${error.message}`);
    throw error;
  }
}

// ============================================================================
// Lock Collection Metadata
// ============================================================================

async function lockCollectionMetadata(options = {}) {
  const { network = 'mainnet' } = options;
  
  const { tezos, credentials, config } = await createTezosClient(network);
  
  // Load contract address (network-specific)
  const addressPath = getContractAddressPath(network);
  if (!fs.existsSync(addressPath)) {
    throw new Error(`❌ No contract deployed on ${network}. Run: node keeps.mjs deploy ${network}`);
  }
  
  const contractAddress = fs.readFileSync(addressPath, 'utf8').trim();
  
  console.log('\n╔══════════════════════════════════════════════════════════════╗');
  console.log('║  🔒 Locking Collection Metadata                              ║');
  console.log('╚══════════════════════════════════════════════════════════════╝\n');
  
  console.log(`📡 Network: ${config.name}`);
  console.log(`📍 Contract: ${contractAddress}`);
  console.log('\n⚠️  WARNING: This action is PERMANENT.');
  console.log('   Collection metadata (name, description, image) cannot be updated after locking.\n');
  
  try {
    const contract = await tezos.contract.at(contractAddress);
    
    const op = await contract.methods.lock_contract_metadata().send();
    
    console.log(`   ⏳ Operation hash: ${op.hash}`);
    console.log('   ⏳ Waiting for confirmation...');
    
    await op.confirmation(1);
    
    console.log('\n╔══════════════════════════════════════════════════════════════╗');
    console.log('║  ✅ Collection Metadata Locked!                              ║');
    console.log('╚══════════════════════════════════════════════════════════════╝\n');
    
    console.log(`   🔒 Status: PERMANENTLY LOCKED`);
    console.log(`   📝 Operation: ${config.explorer}/${op.hash}\n`);
    
    return { hash: op.hash, locked: true };
    
  } catch (error) {
    console.error('\n❌ Lock failed!');
    console.error(`   Error: ${error.message}`);
    throw error;
  }
}

// ============================================================================
// Lock Metadata
// ============================================================================

async function lockMetadata(tokenId, options = {}) {
  const { network = 'mainnet' } = options;
  
  const { tezos, credentials, config } = await createTezosClient(network);
  
  // Load contract address (network-specific)
  const addressPath = getContractAddressPath(network);
  if (!fs.existsSync(addressPath)) {
    throw new Error(`❌ No contract deployed on ${network}. Run: node keeps.mjs deploy ${network}`);
  }
  
  const contractAddress = fs.readFileSync(addressPath, 'utf8').trim();
  
  console.log('\n╔══════════════════════════════════════════════════════════════╗');
  console.log('║  🔒 Locking Token Metadata                                   ║');
  console.log('╚══════════════════════════════════════════════════════════════╝\n');
  
  console.log(`📡 Network: ${config.name}`);
  console.log(`📍 Contract: ${contractAddress}`);
  console.log(`🎨 Token ID: ${tokenId}`);
  console.log('\n⚠️  WARNING: This action is PERMANENT. Metadata cannot be updated after locking.\n');
  
  try {
    const contract = await tezos.contract.at(contractAddress);
    
    const op = await contract.methods.lock_metadata(tokenId).send();
    
    console.log(`   ⏳ Operation hash: ${op.hash}`);
    console.log('   ⏳ Waiting for confirmation...');
    
    await op.confirmation(1);
    
    console.log('\n╔══════════════════════════════════════════════════════════════╗');
    console.log('║  ✅ Metadata Locked Successfully!                            ║');
    console.log('╚══════════════════════════════════════════════════════════════╝\n');
    
    console.log(`   🎨 Token ID: ${tokenId}`);
    console.log(`   🔒 Status: PERMANENTLY LOCKED`);
    console.log(`   📝 Operation: ${config.explorer}/${op.hash}\n`);
    
    return { tokenId, hash: op.hash, locked: true };
    
  } catch (error) {
    console.error('\n❌ Lock failed!');
    console.error(`   Error: ${error.message}`);
    throw error;
  }
}

// ============================================================================
// Burn Token
// ============================================================================

async function burnToken(tokenId, options = {}) {
  const { network = 'mainnet' } = options;
  
  const { tezos, credentials, config } = await createTezosClient(network);
  
  // Load contract address (network-specific)
  const addressPath = getContractAddressPath(network);
  if (!fs.existsSync(addressPath)) {
    throw new Error(`❌ No contract deployed on ${network}. Run: node keeps.mjs deploy ${network}`);
  }
  
  const contractAddress = fs.readFileSync(addressPath, 'utf8').trim();
  
  console.log('\n╔══════════════════════════════════════════════════════════════╗');
  console.log('║  🔥 Burning Token                                            ║');
  console.log('╚══════════════════════════════════════════════════════════════╝\n');
  
  console.log(`📡 Network: ${config.name}`);
  console.log(`📍 Contract: ${contractAddress}`);
  console.log(`🎨 Token ID: ${tokenId}`);
  console.log('\n⚠️  WARNING: This action is PERMANENT. The token will be destroyed.\n');
  console.log('   The piece name will become available for re-keeping.\n');
  
  try {
    const contract = await tezos.contract.at(contractAddress);
    
    const op = await contract.methods.burn_keep(tokenId).send();
    
    console.log(`   ⏳ Operation hash: ${op.hash}`);
    console.log('   ⏳ Waiting for confirmation...');
    
    await op.confirmation(1);
    
    console.log('\n╔══════════════════════════════════════════════════════════════╗');
    console.log('║  ✅ Token Burned Successfully!                               ║');
    console.log('╚══════════════════════════════════════════════════════════════╝\n');
    
    console.log(`   🔥 Token ID: ${tokenId} - DESTROYED`);
    console.log(`   📝 Operation: ${config.explorer}/${op.hash}\n`);
    
    return { tokenId, hash: op.hash, burned: true };
    
  } catch (error) {
    console.error('\n❌ Burn failed!');
    console.error(`   Error: ${error.message}`);
    throw error;
  }
}

// ============================================================================
// Fee Management
// ============================================================================

async function getKeepFee(network = 'mainnet') {
  const { tezos, config } = await createTezosClient(network);
  
  const addressPath = getContractAddressPath(network);
  if (!fs.existsSync(addressPath)) {
    throw new Error(`❌ No contract deployed on ${network}. Run: node keeps.mjs deploy ${network}`);
  }
  
  const contractAddress = fs.readFileSync(addressPath, 'utf8').trim();
  const contract = await tezos.contract.at(contractAddress);
  const storage = await contract.storage();
  
  // keep_fee is stored in mutez
  const feeInMutez = storage.keep_fee?.toNumber?.() ?? storage.keep_fee ?? 0;
  const feeInTez = feeInMutez / 1_000_000;
  
  return { feeInMutez, feeInTez, contractAddress };
}

async function setKeepFee(feeInTez, options = {}) {
  const { network = 'mainnet' } = options;
  
  const { tezos, config } = await createTezosClient(network);
  
  const addressPath = getContractAddressPath(network);
  if (!fs.existsSync(addressPath)) {
    throw new Error(`❌ No contract deployed on ${network}. Run: node keeps.mjs deploy ${network}`);
  }
  
  const contractAddress = fs.readFileSync(addressPath, 'utf8').trim();
  const feeInMutez = Math.floor(feeInTez * 1_000_000);
  
  console.log('\n╔══════════════════════════════════════════════════════════════╗');
  console.log('║  💰 Setting Keep Fee                                         ║');
  console.log('╚══════════════════════════════════════════════════════════════╝\n');
  
  console.log(`📡 Network: ${config.name}`);
  console.log(`📍 Contract: ${contractAddress}`);
  console.log(`💵 New Fee: ${feeInTez} XTZ (${feeInMutez} mutez)\n`);
  
  try {
    const contract = await tezos.contract.at(contractAddress);
    
    const op = await contract.methods.set_keep_fee(feeInMutez).send();
    
    console.log(`   ⏳ Operation hash: ${op.hash}`);
    console.log('   ⏳ Waiting for confirmation...');
    
    await op.confirmation(1);
    
    console.log('\n╔══════════════════════════════════════════════════════════════╗');
  console.log('║  ✅ Keep Fee Updated Successfully!                           ║');
    console.log('╚══════════════════════════════════════════════════════════════╝\n');
    
    console.log(`   💵 New keep fee: ${feeInTez} XTZ`);
    console.log(`   📝 Operation: ${config.explorer}/${op.hash}\n`);
    
    return { feeInTez, feeInMutez, hash: op.hash };
    
  } catch (error) {
    console.error('\n❌ Set fee failed!');
    console.error(`   Error: ${error.message}`);
    throw error;
  }
}

async function setAdministrator(newAdmin, options = {}) {
  const { network = 'mainnet' } = options;
  
  const { tezos, config } = await createTezosClient(network);
  
  const addressPath = getContractAddressPath(network);
  if (!fs.existsSync(addressPath)) {
    throw new Error(`❌ No contract deployed on ${network}. Run: node keeps.mjs deploy ${network}`);
  }
  
  const contractAddress = fs.readFileSync(addressPath, 'utf8').trim();
  
  console.log('\n╔══════════════════════════════════════════════════════════════╗');
  console.log('║  👑 Setting Contract Administrator                           ║');
  console.log('╚══════════════════════════════════════════════════════════════╝\n');
  
  console.log(`📡 Network: ${config.name}`);
  console.log(`📍 Contract: ${contractAddress}`);
  console.log(`👤 New Admin: ${newAdmin}\n`);
  
  try {
    const contract = await tezos.contract.at(contractAddress);
    
    const op = await contract.methods.set_administrator(newAdmin).send();
    
    console.log(`   ⏳ Operation hash: ${op.hash}`);
    console.log('   ⏳ Waiting for confirmation...');
    
    await op.confirmation(1);
    
    console.log('\n╔══════════════════════════════════════════════════════════════╗');
    console.log('║  ✅ Administrator Changed Successfully!                      ║');
    console.log('╚══════════════════════════════════════════════════════════════╝\n');
    
    console.log(`   👤 New admin: ${newAdmin}`);
    console.log(`   📝 Operation: ${config.explorer}/${op.hash}\n`);
    console.log('   ⚠️  WARNING: Only the new admin can call admin functions now!\n');
    
    return { newAdmin, hash: op.hash };
    
  } catch (error) {
    console.error('\n❌ Set administrator failed!');
    console.error(`   Error: ${error.message}`);
    throw error;
  }
}

async function withdrawFees(destination, options = {}) {
  const { network = 'mainnet' } = options;
  
  const { tezos, credentials, config } = await createTezosClient(network);
  
  const addressPath = getContractAddressPath(network);
  if (!fs.existsSync(addressPath)) {
    throw new Error(`❌ No contract deployed on ${network}. Run: node keeps.mjs deploy ${network}`);
  }
  
  const contractAddress = fs.readFileSync(addressPath, 'utf8').trim();
  const dest = destination || credentials.address; // Default to admin address
  
  // Check contract balance first
  const contractBalance = await tezos.tz.getBalance(contractAddress);
  const balanceXTZ = contractBalance.toNumber() / 1_000_000;
  
  console.log('\n╔══════════════════════════════════════════════════════════════╗');
  console.log('║  💸 Withdrawing Fees                                         ║');
  console.log('╚══════════════════════════════════════════════════════════════╝\n');
  
  console.log(`📡 Network: ${config.name}`);
  console.log(`📍 Contract: ${contractAddress}`);
  console.log(`💰 Contract Balance: ${balanceXTZ.toFixed(6)} XTZ`);
  console.log(`📤 Destination: ${dest}\n`);
  
  if (balanceXTZ === 0) {
    console.log('   ℹ️  No fees to withdraw (balance is 0)\n');
    return { withdrawn: 0, hash: null };
  }
  
  try {
    const contract = await tezos.contract.at(contractAddress);
    
    const op = await contract.methods.withdraw_fees(dest).send();
    
    console.log(`   ⏳ Operation hash: ${op.hash}`);
    console.log('   ⏳ Waiting for confirmation...');
    
    await op.confirmation(1);
    
    console.log('\n╔══════════════════════════════════════════════════════════════╗');
    console.log('║  ✅ Fees Withdrawn Successfully!                             ║');
    console.log('╚══════════════════════════════════════════════════════════════╝\n');
    
    console.log(`   💸 Withdrawn: ${balanceXTZ.toFixed(6)} XTZ`);
    console.log(`   📤 To: ${dest}`);
    console.log(`   📝 Operation: ${config.explorer}/${op.hash}\n`);
    
    return { withdrawn: balanceXTZ, destination: dest, hash: op.hash };
    
  } catch (error) {
    console.error('\n❌ Withdrawal failed!');
    console.error(`   Error: ${error.message}`);
    throw error;
  }
}

// ============================================================================
// v4 NEW FEATURES - Royalty, Pause, Admin Transfer
// ============================================================================

async function getRoyalty(network = 'mainnet') {
  console.log('\n╔══════════════════════════════════════════════════════════════╗');
  console.log('║  🎨 Current Royalty Configuration                           ║');
  console.log('╚══════════════════════════════════════════════════════════════╝\n');

  const { tezos, config } = await createTezosClient(network);
  const addressPath = getContractAddressPath(network);

  if (!fs.existsSync(addressPath)) {
    throw new Error(`No contract address found for ${network}. Deploy contract first.`);
  }

  const contractAddress = fs.readFileSync(addressPath, 'utf8').trim();
  const contract = await tezos.contract.at(contractAddress);
  const storage = await contract.storage();

  const royaltyBps = storage.default_royalty_bps ? storage.default_royalty_bps.toNumber() : 1000;
  const royaltyPercent = (royaltyBps / 100).toFixed(1);

  console.log(`   Contract:  ${contractAddress}`);
  console.log(`   Network:   ${config.name}`);
  console.log(`   Royalty:   ${royaltyPercent}% (${royaltyBps} basis points)`);
  console.log(`   Explorer:  ${config.explorer}/${contractAddress}\n`);

  return { contractAddress, royaltyBps, royaltyPercent };
}

async function setRoyalty(percentage, options = {}) {
  const network = options.network || 'mainnet';

  console.log('\n╔══════════════════════════════════════════════════════════════╗');
  console.log('║  🎨 Setting Default Royalty                                  ║');
  console.log('╚══════════════════════════════════════════════════════════════╝\n');

  if (percentage < 0 || percentage > 25) {
    throw new Error('Royalty must be between 0% and 25%');
  }

  const bps = Math.round(percentage * 100); // Convert to basis points

  const { tezos, credentials, config } = await createTezosClient(network);
  const addressPath = getContractAddressPath(network);

  if (!fs.existsSync(addressPath)) {
    throw new Error(`No contract address found for ${network}. Deploy contract first.`);
  }

  const contractAddress = fs.readFileSync(addressPath, 'utf8').trim();

  console.log(`   Setting royalty to ${percentage}% (${bps} basis points)...`);
  console.log(`   Contract: ${contractAddress}`);
  console.log(`   Admin:    ${credentials.address}\n`);

  const contract = await tezos.contract.at(contractAddress);
  const op = await contract.methodsObject.set_default_royalty(bps).send();

  console.log(`   Transaction: ${op.hash}`);
  console.log(`   Waiting for confirmation...`);

  await op.confirmation(1);

  console.log(`\n✅ Royalty set to ${percentage}%`);
  console.log(`🔗 ${config.explorer}/${op.hash}\n`);
}

async function pauseContract(options = {}) {
  const network = options.network || 'mainnet';

  console.log('\n╔══════════════════════════════════════════════════════════════╗');
  console.log('║  🚨 EMERGENCY PAUSE                                          ║');
  console.log('╚══════════════════════════════════════════════════════════════╝\n');

  const { tezos, credentials, config } = await createTezosClient(network);
  const addressPath = getContractAddressPath(network);

  if (!fs.existsSync(addressPath)) {
    throw new Error(`No contract address found for ${network}. Deploy contract first.`);
  }

  const contractAddress = fs.readFileSync(addressPath, 'utf8').trim();

  console.log(`   ⚠️  This will stop all minting and metadata edits`);
  console.log(`   Contract: ${contractAddress}`);
  console.log(`   Admin:    ${credentials.address}\n`);

  // Confirmation prompt
  const rl = readline.createInterface({ input: process.stdin, output: process.stdout });
  const answer = await new Promise(resolve => {
    rl.question('Pause contract? (y/N): ', resolve);
  });
  rl.close();

  if (answer.toLowerCase() !== 'y' && answer.toLowerCase() !== 'yes') {
    console.log('\n❌ Cancelled.\n');
    return;
  }

  const contract = await tezos.contract.at(contractAddress);
  const op = await contract.methodsObject.pause().send();

  console.log(`\n   Transaction: ${op.hash}`);
  console.log(`   Waiting for confirmation...`);

  await op.confirmation(1);

  console.log(`\n✅ Contract PAUSED`);
  console.log(`🔗 ${config.explorer}/${op.hash}\n`);
  console.log(`⚠️  Minting and metadata edits are now disabled`);
  console.log(`   Use "node keeps.mjs unpause" to resume operations\n`);
}

async function unpauseContract(options = {}) {
  const network = options.network || 'mainnet';

  console.log('\n╔══════════════════════════════════════════════════════════════╗');
  console.log('║  ✅ UNPAUSE CONTRACT                                         ║');
  console.log('╚══════════════════════════════════════════════════════════════╝\n');

  const { tezos, credentials, config } = await createTezosClient(network);
  const addressPath = getContractAddressPath(network);

  if (!fs.existsSync(addressPath)) {
    throw new Error(`No contract address found for ${network}. Deploy contract first.`);
  }

  const contractAddress = fs.readFileSync(addressPath, 'utf8').trim();

  console.log(`   Resuming normal operations...`);
  console.log(`   Contract: ${contractAddress}`);
  console.log(`   Admin:    ${credentials.address}\n`);

  const contract = await tezos.contract.at(contractAddress);
  const op = await contract.methodsObject.unpause().send();

  console.log(`   Transaction: ${op.hash}`);
  console.log(`   Waiting for confirmation...`);

  await op.confirmation(1);

  console.log(`\n✅ Contract UNPAUSED`);
  console.log(`🔗 ${config.explorer}/${op.hash}\n`);
  console.log(`   Minting and metadata edits are now enabled\n`);
}

async function adminTransfer(tokenId, fromAddress, toAddress, options = {}) {
  const network = options.network || 'mainnet';

  console.log('\n╔══════════════════════════════════════════════════════════════╗');
  console.log('║  🔄 Admin Emergency Transfer                                 ║');
  console.log('╚══════════════════════════════════════════════════════════════╝\n');

  const { tezos, credentials, config } = await createTezosClient(network);
  const addressPath = getContractAddressPath(network);

  if (!fs.existsSync(addressPath)) {
    throw new Error(`No contract address found for ${network}. Deploy contract first.`);
  }

  const contractAddress = fs.readFileSync(addressPath, 'utf8').trim();

  console.log(`   Token ID:  #${tokenId}`);
  console.log(`   From:      ${fromAddress}`);
  console.log(`   To:        ${toAddress}`);
  console.log(`   Contract:  ${contractAddress}`);
  console.log(`   Admin:     ${credentials.address}\n`);

  // Confirmation prompt
  const rl = readline.createInterface({ input: process.stdin, output: process.stdout });
  const answer = await new Promise(resolve => {
    rl.question('Transfer token? (y/N): ', resolve);
  });
  rl.close();

  if (answer.toLowerCase() !== 'y' && answer.toLowerCase() !== 'yes') {
    console.log('\n❌ Cancelled.\n');
    return;
  }

  const contract = await tezos.contract.at(contractAddress);
  const op = await contract.methodsObject.admin_transfer({
    token_id: tokenId,
    from_: fromAddress,
    to_: toAddress
  }).send();

  console.log(`\n   Transaction: ${op.hash}`);
  console.log(`   Waiting for confirmation...`);

  await op.confirmation(1);

  console.log(`\n✅ Token transferred`);
  console.log(`🔗 ${config.explorer}/${op.hash}`);
  console.log(`📊 ${config.explorer}/${contractAddress}/tokens/${tokenId}\n`);
}

// ============================================================================
// Send XTZ
// ============================================================================

async function sendTez(toAddress, amount, network = 'mainnet') {
  const { tezos, credentials, config } = await createTezosClient(network);

  let resolvedAddress = toAddress;
  if (toAddress.endsWith('.tez')) {
    console.log(`\n🔍 Resolving ${toAddress}...`);
    resolvedAddress = await resolveTezDomain(toAddress);
    console.log(`   → ${resolvedAddress}`);
  }

  const mutez = Math.floor(amount * 1_000_000);

  console.log('\n╔══════════════════════════════════════════════════════════════╗');
  console.log('║  💸 Send XTZ                                                ║');
  console.log('╚══════════════════════════════════════════════════════════════╝\n');

  console.log(`📡 Network:  ${config.name}`);
  console.log(`📤 From:     ${credentials.address}`);
  console.log(`📥 To:       ${resolvedAddress}${toAddress !== resolvedAddress ? ` (${toAddress})` : ''}`);
  console.log(`💵 Amount:   ${amount} XTZ\n`);

  const op = await tezos.contract.transfer({ to: resolvedAddress, amount: mutez, mutez: true });

  console.log(`   ⏳ Operation: ${op.hash}`);
  console.log('   ⏳ Waiting for confirmation...');
  await op.confirmation(1);

  console.log('\n✅ Sent!');
  console.log(`   🔗 ${config.explorer}/${op.hash}\n`);
  return { hash: op.hash };
}

// ============================================================================
// FA2 Transfer
// ============================================================================

async function resolveTezDomain(name) {
  const resp = await fetch(`https://api.tzkt.io/v1/domains?name=${encodeURIComponent(name)}`);
  if (!resp.ok) throw new Error(`Tezos Domains lookup failed: ${resp.status}`);
  const data = await resp.json();
  const entry = data?.[0];
  if (!entry?.owner?.address) throw new Error(`Could not resolve ${name}`);
  return entry.owner.address;
}

async function transferToken(tokenId, toAddress, network = 'mainnet') {
  const { tezos, credentials, config } = await createTezosClient(network);
  const contractAddress = loadContractAddress(network);

  let resolvedAddress = toAddress;
  if (toAddress.endsWith('.tez')) {
    console.log(`\n🔍 Resolving ${toAddress}...`);
    resolvedAddress = await resolveTezDomain(toAddress);
    console.log(`   → ${resolvedAddress}`);
  }

  console.log('\n╔══════════════════════════════════════════════════════════════╗');
  console.log('║  🎁 Transfer Token                                          ║');
  console.log('╚══════════════════════════════════════════════════════════════╝\n');

  console.log(`📡 Network:  ${config.name}`);
  console.log(`📍 Contract: ${contractAddress}`);
  console.log(`🎨 Token:    #${tokenId}`);
  console.log(`📤 From:     ${credentials.address}`);
  console.log(`📥 To:       ${resolvedAddress}${toAddress !== resolvedAddress ? ` (${toAddress})` : ''}\n`);

  // Check for active listing and retract if needed
  const listing = await loadActiveListingForToken(contractAddress, tokenId, credentials.address);
  const contract = await tezos.contract.at(contractAddress);

  if (listing) {
    const askId = Number.parseInt(String(listing.bigmap_key), 10) || listing.id;
    const askPrice = (Number(listing.price_xtz || 0) / 1_000_000).toFixed(6);
    console.log(`♻️  Retracting active listing #${askId} (${askPrice} XTZ)...`);

    const marketplaceContract = listing.marketplace_contract || OBJKT_MARKETPLACE_FALLBACK[network];
    const marketContract = await tezos.contract.at(marketplaceContract);

    const op = await tezos.contract.batch()
      .withContractCall(marketContract.methods.retract_ask(Number(askId)))
      .withContractCall(contract.methods.transfer([{
        from_: credentials.address,
        txs: [{ to_: resolvedAddress, token_id: tokenId, amount: 1 }]
      }]))
      .send();

    console.log(`   ⏳ Operation: ${op.hash}`);
    console.log('   ⏳ Waiting for confirmation...');
    await op.confirmation(1);

    console.log('\n✅ Listing retracted + token transferred!');
    console.log(`   🔗 ${config.explorer}/${op.hash}\n`);
    return { hash: op.hash, retracted: askId };
  }

  const op = await contract.methods.transfer([{
    from_: credentials.address,
    txs: [{ to_: resolvedAddress, token_id: tokenId, amount: 1 }]
  }]).send();

  console.log(`   ⏳ Operation: ${op.hash}`);
  console.log('   ⏳ Waiting for confirmation...');
  await op.confirmation(1);

  console.log('\n✅ Token transferred!');
  console.log(`   🔗 ${config.explorer}/${op.hash}\n`);
  return { hash: op.hash };
}

// ============================================================================
// Marketplace Commands (Objkt)
// ============================================================================

async function listOwnedTokens(network = 'mainnet', options = {}) {
  const { credentials, config } = await createTezosClient(network);
  const contractAddress = loadContractAddress(network);
  const limit = Number.isFinite(Number(options.limit)) ? Math.max(1, Number(options.limit)) : 200;

  const apiBase = tzktApiBase(network);
  const url = `${apiBase}/v1/tokens/balances?token.contract=${contractAddress}&account=${credentials.address}&balance.gt=0&limit=${limit}`;
  const response = await fetch(url);
  if (!response.ok) {
    throw new Error(`Failed to load owned tokens (${response.status})`);
  }

  const rows = await response.json();

  console.log('\n╔══════════════════════════════════════════════════════════════╗');
  console.log('║  🧾 Wallet Token Inventory                                   ║');
  console.log('╚══════════════════════════════════════════════════════════════╝\n');

  console.log(`📡 Network:  ${config.name}`);
  console.log(`📍 Contract: ${contractAddress}`);
  console.log(`👤 Wallet:   ${credentials.address}`);
  console.log(`📦 Tokens:   ${rows.length}\n`);

  if (!rows.length) {
    console.log('   (No tokens in this wallet for current keeps contract)\n');
    return [];
  }

  const objktBase = network === 'mainnet' ? 'https://objkt.com' : 'https://ghostnet.objkt.com';
  const normalized = rows
    .map((row) => ({
      tokenId: Number.parseInt(row?.token?.tokenId ?? row?.token?.token_id, 10),
      name: row?.token?.metadata?.name || row?.token?.metadata?.symbol || `#${row?.token?.tokenId}`,
      balance: Number.parseInt(row?.balance ?? '0', 10),
      lastTime: row?.lastTime || null,
    }))
    .filter((row) => Number.isInteger(row.tokenId))
    .sort((a, b) => a.tokenId - b.tokenId);

  for (const token of normalized) {
    console.log(`   [${token.tokenId}] ${token.name} (balance: ${token.balance})`);
    console.log(`       🔗 ${objktBase}/tokens/${contractAddress}/${token.tokenId}`);
  }
  console.log('');

  return normalized;
}

async function listTokenForSale(tokenReference, priceInXTZ, options = {}) {
  const network = options.network || 'mainnet';
  const apply = options.apply === true;
  const referralBonusBpsRaw = Number.parseInt(options.referralBonusBps ?? 500, 10);
  const referralBonusBps = Number.isFinite(referralBonusBpsRaw)
    ? Math.max(0, Math.min(10000, referralBonusBpsRaw))
    : 500;
  const startTime = options.startTime || null;
  const expiryTime = options.expiryTime || null;
  const replaceExisting = options.replaceExisting === true;

  const { tezos, credentials, config } = await createTezosClient(network);
  const contractAddress = loadContractAddress(network);
  const tokenId = await resolveTokenIdFromReference(tokenReference, { contractAddress, network });
  const priceMutez = parsePriceToMutez(priceInXTZ);
  const priceXTZ = (priceMutez / 1_000_000).toFixed(6);

  await assertWalletOwnsToken(contractAddress, tokenId, credentials.address, network);

  const token = await fetchTokenFromTzkt(contractAddress, tokenId, network);
  if (!token) {
    throw new Error(`Token #${tokenId} not found on ${network}.`);
  }

  const tokenName = token?.metadata?.name || token?.metadata?.symbol || `#${tokenId}`;
  const shares = normalizeShareMap(token?.metadata?.royalties?.shares);
  if (Object.keys(shares).length === 0) {
    shares[credentials.address] = '1000';
  }

  const marketplaceContract = await resolveObjktMarketplaceContract({
    network,
    keepsContract: contractAddress,
    explicitContract: options.marketplaceContract || null,
  });

  const existingListing = await loadActiveListingForToken(contractAddress, tokenId, credentials.address)
    .catch(() => null);

  if (existingListing && !replaceExisting) {
    const existingPrice = Number(existingListing.price_xtz || 0) / 1_000_000;
    throw new Error(
      `Token #${tokenId} already has an active listing (${existingPrice} XTZ). Use --replace to update it.`
    );
  }

  const askPayload = {
    token: {
      address: contractAddress,
      token_id: tokenId.toString(),
    },
    currency: {
      tez: {},
    },
    amount: priceMutez.toString(),
    editions: '1',
    shares,
    start_time: startTime,
    expiry_time: expiryTime,
    referral_bonus: referralBonusBps.toString(),
    condition: null,
  };

  const objktBase = network === 'mainnet' ? 'https://objkt.com' : 'https://ghostnet.objkt.com';
  const tokenUrl = `${objktBase}/tokens/${contractAddress}/${tokenId}`;
  const existingAskOnChainId = Number.parseInt(String(existingListing?.bigmap_key), 10);
  const existingAskDisplayId = Number.isInteger(existingAskOnChainId)
    ? existingAskOnChainId
    : existingListing?.id;

  console.log('\n╔══════════════════════════════════════════════════════════════╗');
  console.log('║  🏷️  List Token For Sale                                      ║');
  console.log('╚══════════════════════════════════════════════════════════════╝\n');

  console.log(`📡 Network:      ${config.name}`);
  console.log(`📍 Keeps:        ${contractAddress}`);
  console.log(`🛒 Marketplace:  ${marketplaceContract}`);
  console.log(`🎨 Token:        [${tokenId}] ${tokenName}`);
  console.log(`💵 Ask Price:    ${priceXTZ} XTZ (${priceMutez} mutez)`);
  console.log(`👤 Seller:       ${credentials.address}`);
  console.log(`🔗 View:         ${tokenUrl}`);
  console.log(`📈 Shares:       ${JSON.stringify(shares)}`);
  if (existingListing) {
    console.log(
      `♻️ Existing ask: #${existingAskDisplayId} (${(Number(existingListing.price_xtz || 0) / 1_000_000).toFixed(6)} XTZ)`
    );
  }
  console.log('');

  if (!apply) {
    console.log('⚠️  DRY RUN: no transaction sent. Add --yes to list on chain.\n');
    return {
      tokenId,
      tokenName,
      priceMutez,
      priceXTZ: Number(priceXTZ),
      contractAddress,
      marketplaceContract,
      dryRun: true,
      replaced: Boolean(existingListing),
    };
  }

  const tokenContract = await tezos.contract.at(contractAddress);
  const marketContract = await tezos.contract.at(marketplaceContract);

  const askMethod = marketContract.methodsObject.ask(askPayload);
  const retractMethod = existingListing
    ? marketContract.methods.retract_ask(Number(existingAskDisplayId))
    : null;

  let op;
  let mode = 'ask-only';

  if (retractMethod) {
    mode = 'retract+ask';
    op = await tezos.contract
      .batch()
      .withContractCall(retractMethod)
      .withContractCall(askMethod)
      .send();
  } else {
    const addOperatorMethod = tokenContract.methods.update_operators([
      {
        add_operator: {
          owner: credentials.address,
          operator: marketplaceContract,
          token_id: tokenId,
        },
      },
    ]);

    try {
      mode = 'add_operator+ask';
      op = await tezos.contract
        .batch()
        .withContractCall(addOperatorMethod)
        .withContractCall(askMethod)
        .send();
    } catch (error) {
      const message = String(error?.message || error);
      if (/FA2_OPERATOR_ALREADY_EXISTS|operator/i.test(message)) {
        mode = 'ask-only';
        op = await tezos.contract
          .batch()
          .withContractCall(askMethod)
          .send();
      } else {
        throw error;
      }
    }
  }

  console.log(`   Transaction: ${op.hash}`);
  console.log('   Waiting for confirmation...');
  await op.confirmation(1);

  console.log('\n✅ Listed successfully');
  console.log(`   Mode: ${mode}`);
  console.log(`   Explorer: ${config.explorer}/${op.hash}`);
  console.log(`   Objkt:    ${tokenUrl}\n`);

  return {
    tokenId,
    tokenName,
    priceMutez,
    priceXTZ: Number(priceXTZ),
    contractAddress,
    marketplaceContract,
    hash: op.hash,
    mode,
    replaced: Boolean(existingListing),
  };
}

async function listBatchForSale(items = [], options = {}) {
  if (!Array.isArray(items) || items.length === 0) {
    throw new Error('No items to list. Use format: <token|piece>=<priceXTZ>');
  }

  const results = [];
  for (const item of items) {
    const [tokenReference, priceInXTZ] = item.split('=');
    if (!tokenReference || !priceInXTZ) {
      throw new Error(`Invalid batch item "${item}". Expected "<token|piece>=<priceXTZ>".`);
    }

    const result = await listTokenForSale(tokenReference, priceInXTZ, options);
    results.push(result);
  }

  return results;
}

async function acceptOffer(offerIdInput, options = {}) {
  const network = options.network || 'mainnet';
  const apply = options.apply === true;
  const minPriceMutez = options.minPriceMutez != null
    ? Number.parseInt(String(options.minPriceMutez), 10)
    : null;

  const { tezos, credentials, config } = await createTezosClient(network);
  const contractAddress = loadContractAddress(network);
  const offerId = Number.parseInt(String(offerIdInput), 10);

  if (!Number.isInteger(offerId) || offerId < 0) {
    throw new Error(`Invalid offer id "${offerIdInput}".`);
  }

  const offer = await loadActiveOfferById(offerId);
  if (!offer) {
    throw new Error(`Offer #${offerId} is not active.`);
  }

  const tokenId = Number.parseInt(String(offer?.token?.token_id), 10);
  const tokenName = offer?.token?.name || `#${offer?.token?.token_id ?? '?'}`;
  const offerRowId = Number.parseInt(String(offer?.id), 10);
  const offerOnChainId = Number.parseInt(String(offer?.bigmap_key), 10);
  const chainOfferId = Number.isInteger(offerOnChainId) ? offerOnChainId : offerId;
  const faContract = offer?.token?.fa_contract;
  if (!isKt1Address(faContract) || faContract !== contractAddress) {
    throw new Error(
      `Offer #${offerId} belongs to ${faContract || 'unknown contract'}, not current keeps contract ${contractAddress}.`
    );
  }

  const bidMutez = Number.parseInt(String(offer?.price_xtz || 0), 10);
  if (!Number.isInteger(bidMutez) || bidMutez <= 0) {
    throw new Error(`Offer #${offerId} has invalid bid amount.`);
  }

  if (Number.isInteger(minPriceMutez) && bidMutez <= minPriceMutez) {
    throw new Error(
      `Offer #${offerId} bid ${(bidMutez / 1_000_000).toFixed(6)} XTZ is not above threshold ${(minPriceMutez / 1_000_000).toFixed(6)} XTZ.`
    );
  }

  await assertWalletOwnsToken(contractAddress, tokenId, credentials.address, network);

  const marketplaceContract = options.marketplaceContract
    || offer?.marketplace_contract
    || await resolveObjktMarketplaceContract({
      network,
      keepsContract: contractAddress,
      explicitContract: null,
    });

  const bidXTZ = (bidMutez / 1_000_000).toFixed(6);
  const objktBase = network === 'mainnet' ? 'https://objkt.com' : 'https://ghostnet.objkt.com';
  const tokenUrl = `${objktBase}/tokens/${contractAddress}/${tokenId}`;

  console.log('\n╔══════════════════════════════════════════════════════════════╗');
  console.log('║  ✅ Accept Offer                                              ║');
  console.log('╚══════════════════════════════════════════════════════════════╝\n');
  console.log(`📡 Network:      ${config.name}`);
  console.log(`📍 Keeps:        ${contractAddress}`);
  console.log(`🛒 Marketplace:  ${marketplaceContract}`);
  console.log(`🎨 Token:        [${tokenId}] ${tokenName}`);
  console.log(`🧾 Offer ID:     ${chainOfferId}`);
  if (Number.isInteger(offerRowId) && offerRowId !== chainOfferId) {
    console.log(`🗂️  Offer Row ID: ${offerRowId}`);
  }
  console.log(`💵 Bid:          ${bidXTZ} XTZ (${bidMutez} mutez)`);
  console.log(`👤 Buyer:        ${offer?.buyer_address || 'unknown'}`);
  console.log(`👤 Seller:       ${credentials.address}`);
  console.log(`🔗 View:         ${tokenUrl}\n`);

  if (!apply) {
    console.log('⚠️  DRY RUN: no transaction sent. Add --yes to accept on chain.\n');
    return {
      offerId: chainOfferId,
      offerRowId: Number.isInteger(offerRowId) ? offerRowId : null,
      tokenId,
      tokenName,
      bidMutez,
      bidXTZ: Number(bidXTZ),
      buyerAddress: offer?.buyer_address || null,
      marketplaceContract,
      contractAddress,
      dryRun: true,
    };
  }

  const tokenContract = await tezos.contract.at(contractAddress);
  const marketContract = await tezos.contract.at(marketplaceContract);

  const addOperatorMethod = tokenContract.methods.update_operators([
    {
      add_operator: {
        owner: credentials.address,
        operator: marketplaceContract,
        token_id: tokenId,
      },
    },
  ]);

  const fulfillMethod = marketContract.methodsObject.fulfill_offer({
    offer_id: chainOfferId,
    token_id: tokenId,
    condition_extra: null,
  });

  let op;
  let mode = 'add_operator+fulfill_offer';

  try {
    op = await tezos.contract
      .batch()
      .withContractCall(addOperatorMethod)
      .withContractCall(fulfillMethod)
      .send();
  } catch (error) {
    const message = String(error?.message || error);
    if (/FA2_OPERATOR_ALREADY_EXISTS|operator/i.test(message)) {
      mode = 'fulfill_offer-only';
      op = await tezos.contract
        .batch()
        .withContractCall(fulfillMethod)
        .send();
    } else {
      throw error;
    }
  }

  console.log(`   Transaction: ${op.hash}`);
  console.log('   Waiting for confirmation...');
  await op.confirmation(1);

  console.log('\n✅ Offer accepted');
  console.log(`   Mode: ${mode}`);
  console.log(`   Explorer: ${config.explorer}/${op.hash}`);
  console.log(`   Objkt:    ${tokenUrl}\n`);

  return {
    offerId: chainOfferId,
    offerRowId: Number.isInteger(offerRowId) ? offerRowId : null,
    tokenId,
    tokenName,
    bidMutez,
    bidXTZ: Number(bidXTZ),
    buyerAddress: offer?.buyer_address || null,
    marketplaceContract,
    contractAddress,
    hash: op.hash,
    mode,
  };
}

async function acceptOffersAboveThreshold(items = [], options = {}) {
  if (!Array.isArray(items) || items.length === 0) {
    throw new Error('No items provided. Use format: <token|piece>=<minimumXTZ>');
  }

  const network = options.network || 'mainnet';
  const apply = options.apply === true;
  const contractAddress = loadContractAddress(network);

  const checks = [];
  for (const item of items) {
    const [tokenReference, minXTZ] = item.split('=');
    if (!tokenReference || !minXTZ) {
      throw new Error(`Invalid item "${item}". Expected "<token|piece>=<minimumXTZ>".`);
    }

    const tokenId = await resolveTokenIdFromReference(tokenReference, { contractAddress, network });
    const minPriceMutez = parsePriceToMutez(minXTZ);
    const bestOffer = await loadBestActiveOfferForToken(contractAddress, tokenId);

    checks.push({
      tokenReference,
      tokenId,
      minPriceMutez,
      minXTZ: minPriceMutez / 1_000_000,
      offer: bestOffer,
      qualifies: Number(bestOffer?.price_xtz || 0) > minPriceMutez,
    });
  }

  console.log('\n╔══════════════════════════════════════════════════════════════╗');
  console.log('║  🤝 Accept Offers Above Threshold                            ║');
  console.log('╚══════════════════════════════════════════════════════════════╝\n');
  console.log(`📡 Network: ${network}`);
  console.log(`📍 Keeps:   ${contractAddress}\n`);

  for (const row of checks) {
    if (!row.offer) {
      console.log(`   [${row.tokenId}] no active offer (threshold: ${row.minXTZ.toFixed(6)} XTZ)`);
      continue;
    }

    const bidMutez = Number(row.offer.price_xtz || 0);
    const bidXTZ = bidMutez / 1_000_000;
    const pass = row.qualifies ? '✅' : '❌';
    const onChainOfferId = Number.parseInt(String(row.offer.bigmap_key), 10);
    const displayOfferId = Number.isInteger(onChainOfferId) ? onChainOfferId : row.offer.id;
    console.log(
      `   [${row.tokenId}] ${row.offer?.token?.name || '#'} offer #${displayOfferId} bid ${bidXTZ.toFixed(6)} XTZ vs threshold ${row.minXTZ.toFixed(6)} XTZ ${pass}`
    );
  }
  console.log('');

  const qualifying = checks.filter((row) => row.offer && row.qualifies);
  if (qualifying.length === 0) {
    console.log('No qualifying offers above thresholds.\n');
    return [];
  }

  if (!apply) {
    console.log('⚠️  DRY RUN: no transaction sent. Add --yes to accept qualifying offers.\n');
    return qualifying.map((row) => ({
      tokenId: row.tokenId,
      tokenName: row.offer?.token?.name || null,
      offerId: Number.parseInt(String(row.offer?.bigmap_key), 10) || Number(row.offer.id),
      offerRowId: Number(row.offer.id),
      bidMutez: Number(row.offer.price_xtz || 0),
      bidXTZ: Number(row.offer.price_xtz || 0) / 1_000_000,
      thresholdXTZ: row.minXTZ,
      buyerAddress: row.offer?.buyer_address || null,
      dryRun: true,
    }));
  }

  const results = [];
  for (const row of qualifying) {
    const onChainOfferId = Number.parseInt(String(row.offer?.bigmap_key), 10);
    const offerIdentifier = Number.isInteger(onChainOfferId) ? onChainOfferId : row.offer.id;
    const result = await acceptOffer(offerIdentifier, {
      network,
      apply: true,
      minPriceMutez: row.minPriceMutez,
      marketplaceContract: options.marketplaceContract || row.offer?.marketplace_contract || null,
    });
    results.push(result);
  }

  return results;
}

// ============================================================================
// Buy (fulfill_ask)
// ============================================================================

async function loadActiveAskById(askId) {
  const numericId = Number.parseInt(String(askId), 10);
  if (!Number.isInteger(numericId) || numericId < 0) {
    throw new Error(`Invalid ask id "${askId}".`);
  }

  const data = await objktGraphQL(
    `
    query {
      listing_active(
        where:{
          _or:[
            {id:{_eq:${numericId}}},
            {bigmap_key:{_eq:${numericId}}}
          ]
        }
        order_by:{timestamp:desc}
        limit:5
      ) {
        id
        bigmap_key
        price_xtz
        seller_address
        marketplace_contract
        marketplace { name }
        amount_left
        timestamp
        token { token_id name fa_contract }
      }
    }
    `
  );

  const rows = Array.isArray(data?.listing_active) ? data.listing_active : [];
  if (rows.length === 0) return null;

  return rows.find((row) => Number.parseInt(String(row?.id), 10) === numericId)
    || rows.find((row) => Number.parseInt(String(row?.bigmap_key), 10) === numericId)
    || rows[0];
}

async function buyToken(askIdInput, options = {}) {
  const network = options.network || 'mainnet';
  const apply = options.apply === true;

  const { tezos, credentials, config } = await createTezosClient(network);
  const contractAddress = loadContractAddress(network);
  const askId = Number.parseInt(String(askIdInput), 10);

  if (!Number.isInteger(askId) || askId < 0) {
    throw new Error(`Invalid ask id "${askIdInput}".`);
  }

  // Try objkt GraphQL first, fall back to TzKT bigmap lookup
  let listing = await loadActiveAskById(askId);
  let priceMutez, tokenId, tokenName, sellerAddress, marketplaceContract, faContract;

  if (listing) {
    priceMutez = Number.parseInt(String(listing.price_xtz || 0), 10);
    tokenId = Number.parseInt(String(listing.token?.token_id), 10);
    tokenName = listing.token?.name || `#${tokenId}`;
    sellerAddress = listing.seller_address;
    marketplaceContract = listing.marketplace_contract;
    faContract = listing.token?.fa_contract;
  } else {
    // Fallback: read directly from TzKT bigmap
    const bigmapId = 684371; // objktcom marketplace v6.2 asks bigmap
    const resp = await fetch(`https://api.tzkt.io/v1/bigmaps/${bigmapId}/keys/${askId}`);
    if (!resp.ok) throw new Error(`Ask #${askId} not found (TzKT ${resp.status}).`);
    const entry = await resp.json();
    if (!entry?.active) throw new Error(`Ask #${askId} is no longer active.`);
    const val = entry.value;
    priceMutez = Number.parseInt(String(val?.amount || 0), 10);
    tokenId = Number.parseInt(String(val?.token?.token_id), 10);
    tokenName = `#${tokenId}`;
    sellerAddress = val?.creator;
    faContract = val?.token?.address;
    marketplaceContract = 'KT1SwbTqhSKF6Pdokiu1K4Fpi17ahPPzmt1X';
  }

  if (!Number.isInteger(priceMutez) || priceMutez <= 0) {
    throw new Error(`Ask #${askId} has invalid price.`);
  }

  if (faContract && faContract !== contractAddress) {
    // Allow buying from any FA2, but warn if not the current keeps contract
    console.log(`⚠️  Token is from ${faContract}, not current keeps contract ${contractAddress}.`);
  }

  const priceXTZ = (priceMutez / 1_000_000).toFixed(6);
  const balance = await tezos.tz.getBalance(credentials.address);
  const balanceXTZ = balance.toNumber() / 1_000_000;

  if (balanceXTZ < priceMutez / 1_000_000) {
    throw new Error(
      `Insufficient balance: ${balanceXTZ.toFixed(6)} XTZ available, need ${priceXTZ} XTZ.`
    );
  }

  const objktBase = network === 'mainnet' ? 'https://objkt.com' : 'https://ghostnet.objkt.com';
  const tokenUrl = faContract
    ? `${objktBase}/tokens/${faContract}/${tokenId}`
    : `${objktBase}/tokens/${contractAddress}/${tokenId}`;

  console.log('\n╔══════════════════════════════════════════════════════════════╗');
  console.log('║  🛒 Buy Token (fulfill_ask)                                  ║');
  console.log('╚══════════════════════════════════════════════════════════════╝\n');
  console.log(`📡 Network:      ${config.name}`);
  console.log(`📍 Contract:     ${faContract || contractAddress}`);
  console.log(`🛒 Marketplace:  ${marketplaceContract}`);
  console.log(`🎨 Token:        [${tokenId}] ${tokenName}`);
  console.log(`🧾 Ask ID:       ${askId}`);
  console.log(`💵 Price:        ${priceXTZ} XTZ (${priceMutez} mutez)`);
  console.log(`👤 Seller:       ${sellerAddress || 'unknown'}`);
  console.log(`👤 Buyer:        ${credentials.address}`);
  console.log(`💰 Balance:      ${balanceXTZ.toFixed(6)} XTZ`);
  console.log(`🔗 View:         ${tokenUrl}\n`);

  if (!apply) {
    console.log('⚠️  DRY RUN: no transaction sent. Add --yes to buy on chain.\n');
    return {
      askId,
      tokenId,
      tokenName,
      priceMutez,
      priceXTZ: Number(priceXTZ),
      sellerAddress,
      marketplaceContract,
      dryRun: true,
    };
  }

  const marketContract = await tezos.contract.at(marketplaceContract);

  const op = await marketContract.methodsObject.fulfill_ask({
    ask_id: askId,
    amount: 1,               // editions to buy
    proxy_for: null,
    condition_extra: null,
    referrers: new MichelsonMap(),
  }).send({ amount: priceMutez, mutez: true });

  console.log(`   ⏳ Transaction: ${op.hash}`);
  console.log('   ⏳ Waiting for confirmation...');
  await op.confirmation(1);

  console.log('\n✅ Purchase complete!');
  console.log(`   🎨 Token: [${tokenId}] ${tokenName}`);
  console.log(`   💵 Paid: ${priceXTZ} XTZ`);
  console.log(`   🔗 Explorer: ${config.explorer}/${op.hash}`);
  console.log(`   🔗 Objkt: ${tokenUrl}\n`);

  return {
    askId,
    tokenId,
    tokenName,
    priceMutez,
    priceXTZ: Number(priceXTZ),
    sellerAddress,
    marketplaceContract,
    hash: op.hash,
  };
}

async function showMarketSnapshot(network = 'mainnet', options = {}) {
  if (network !== 'mainnet') {
    throw new Error('Market snapshot currently supports mainnet only.');
  }

  const contractAddress = loadContractAddress(network);
  const listingsLimit = Number.isFinite(Number(options.listingsLimit)) ? Math.max(1, Number(options.listingsLimit)) : 20;
  const salesLimit = Number.isFinite(Number(options.salesLimit)) ? Math.max(1, Number(options.salesLimit)) : 10;

  const collectionData = await objktGraphQL(
    `
    query($contract:String!, $limit:Int!) {
      fa(where:{contract:{_eq:$contract}}) {
        contract
        name
        items
        owners
        floor_price
        volume_24h
        volume_total
      }
      listing_active(
        where:{fa_contract:{_eq:$contract}}
        order_by:{price_xtz:asc}
        limit:$limit
      ) {
        id
        bigmap_key
        price_xtz
        seller_address
        token { token_id name }
        marketplace { name }
        timestamp
      }
      offer_active(
        where:{fa_contract:{_eq:$contract}}
        order_by:{price_xtz:desc}
        limit:$limit
      ) {
        id
        bigmap_key
        price_xtz
        buyer_address
        token { token_id name }
        marketplace { name }
        timestamp
      }
    }
    `,
    { contract: contractAddress, limit: listingsLimit }
  );

  let salesData = { listing_sale: [] };
  let salesLoadError = null;
  try {
    salesData = await objktGraphQL(
      `
      query($contract:String!, $limit:Int!) {
        listing_sale(
          where:{token:{fa_contract:{_eq:$contract}}}
          order_by:{timestamp:desc}
          limit:$limit
        ) {
          timestamp
          price_xtz
          buyer_address
          seller_address
          token { token_id name }
          marketplace { name }
        }
      }
      `,
      { contract: contractAddress, limit: salesLimit }
    );
  } catch (error) {
    salesLoadError = error;
  }

  const collection = collectionData?.fa?.[0] || null;
  const listings = Array.isArray(collectionData?.listing_active) ? collectionData.listing_active : [];
  const offers = Array.isArray(collectionData?.offer_active) ? collectionData.offer_active : [];
  const sales = Array.isArray(salesData?.listing_sale) ? salesData.listing_sale : [];

  console.log('\n╔══════════════════════════════════════════════════════════════╗');
  console.log('║  📈 Objkt Market Snapshot                                    ║');
  console.log('╚══════════════════════════════════════════════════════════════╝\n');

  console.log(`📍 Contract: ${contractAddress}`);
  console.log(`🎨 Collection: ${collection?.name || 'Unknown'}`);
  console.log(`🧱 Items: ${collection?.items ?? 'n/a'} | 👥 Owners: ${collection?.owners ?? 'n/a'}`);
  console.log(`🏷️  Floor: ${collection?.floor_price != null ? (Number(collection.floor_price) / 1_000_000).toFixed(6) : 'n/a'} XTZ`);
  console.log(`📊 Volume 24h: ${collection?.volume_24h != null ? (Number(collection.volume_24h) / 1_000_000).toFixed(6) : 'n/a'} XTZ`);
  console.log(`📚 Volume total: ${collection?.volume_total != null ? (Number(collection.volume_total) / 1_000_000).toFixed(6) : 'n/a'} XTZ\n`);

  console.log(`🛒 Active Listings (${listings.length}):`);
  if (listings.length === 0) {
    console.log('   (none)');
  } else {
    for (const row of listings) {
      const askId = Number.parseInt(String(row?.bigmap_key), 10);
      const displayAskId = Number.isInteger(askId) ? askId : row?.id;
      console.log(
        `   [${row?.token?.token_id}] ${row?.token?.name || '#'} ask #${displayAskId} @ ${(Number(row?.price_xtz || 0) / 1_000_000).toFixed(6)} XTZ (${row?.seller_address})`
      );
    }
  }
  console.log('');

  console.log(`🤝 Active Offers (${offers.length}):`);
  if (offers.length === 0) {
    console.log('   (none)');
  } else {
    for (const row of offers) {
      const offerId = Number.parseInt(String(row?.bigmap_key), 10);
      const displayOfferId = Number.isInteger(offerId) ? offerId : row?.id;
      console.log(
        `   [${row?.token?.token_id}] ${row?.token?.name || '#'} offer #${displayOfferId} bid ${(Number(row?.price_xtz || 0) / 1_000_000).toFixed(6)} XTZ (${row?.buyer_address})`
      );
    }
  }
  console.log('');

  console.log(`💸 Recent Sales (${sales.length}):`);
  if (sales.length === 0) {
    if (salesLoadError) {
      console.log(`   (unavailable: ${salesLoadError.message})`);
    } else {
      console.log('   (none)');
    }
  } else {
    for (const row of sales) {
      console.log(
        `   [${row?.token?.token_id}] ${row?.token?.name || '#'} sold ${(Number(row?.price_xtz || 0) / 1_000_000).toFixed(6)} XTZ @ ${row?.timestamp}`
      );
    }
  }
  console.log('');

  return {
    contractAddress,
    collection,
    listings,
    offers,
    sales,
  };
}

async function discoverContractsByManager(managerAddress, network = 'mainnet') {
  const apiBase = tzktApiBase(network);
  const url = `${apiBase}/v1/accounts/${managerAddress}/contracts?limit=200&sort.desc=creationLevel`;
  const response = await fetch(url);
  if (!response.ok) {
    throw new Error(`Failed to load contracts for ${managerAddress}: ${response.status}`);
  }
  const contracts = await response.json();
  return contracts
    .map((entry) => entry?.address)
    .filter((address) => isKt1Address(address));
}

function decodeContractMetadataBytes(contentBytes) {
  if (!contentBytes) return null;

  const raw = typeof contentBytes === 'string'
    ? contentBytes
    : (typeof contentBytes?.toString === 'function' ? contentBytes.toString() : '');
  const normalized = raw.startsWith('0x') ? raw.slice(2) : raw;
  if (!normalized) return null;

  try {
    return JSON.parse(Buffer.from(normalized, 'hex').toString('utf8'));
  } catch {
    return null;
  }
}

function buildDeprecatedCollectionMetadata(existing = {}, options = {}) {
  const replacementContract = options.replacementContract;
  const nowIso = new Date().toISOString();
  const nowDate = nowIso.slice(0, 10);
  const priorDescription = typeof existing.description === 'string' ? existing.description.trim() : '';
  const replacementText = replacementContract
    ? `Replacement contract: ${replacementContract}`
    : 'Replacement contract: not set';
  const notice = options.notice || `Deprecated staging contract as of ${nowDate}. ${replacementText}`;

  const deprecatedName = typeof existing.name === 'string' && existing.name.trim()
    ? (existing.name.includes('[DEPRECATED]') ? existing.name : `${existing.name} [DEPRECATED]`)
    : 'KidLisp Keeps [DEPRECATED]';

  return {
    ...existing,
    name: deprecatedName,
    description: [priorDescription, notice].filter(Boolean).join('\n\n'),
    deprecated: true,
    deprecatedAt: nowIso,
    deprecatedReplacement: replacementContract || null,
    status: 'deprecated-staging',
  };
}

async function fetchActiveTokenIds(contractAddress, network = 'mainnet') {
  const apiBase = tzktApiBase(network);
  const limit = 1000;
  let offset = 0;
  const tokenIds = [];

  while (true) {
    const url = `${apiBase}/v1/contracts/${contractAddress}/bigmaps/token_metadata/keys?active=true&select=key&limit=${limit}&offset=${offset}`;
    const response = await fetch(url);
    if (!response.ok) {
      throw new Error(`Failed to fetch token list for ${contractAddress}: ${response.status}`);
    }

    const keys = await response.json();
    if (!Array.isArray(keys) || keys.length === 0) break;

    for (const key of keys) {
      const tokenId = Number(key);
      if (Number.isInteger(tokenId) && tokenId >= 0) {
        tokenIds.push(tokenId);
      }
    }

    if (keys.length < limit) break;
    offset += keys.length;
  }

  return [...new Set(tokenIds)].sort((a, b) => a - b);
}

async function deprecateStagingContracts(options = {}) {
  const network = options.network || 'mainnet';
  const apply = options.apply === true;
  const providedAddresses = Array.isArray(options.addresses)
    ? options.addresses.filter((address) => isKt1Address(address))
    : [];
  const replacementContract = isKt1Address(options.replacementContract)
    ? options.replacementContract
    : null;
  const burnLimitRaw = Number(options.burnLimit);
  const burnLimit = Number.isFinite(burnLimitRaw) && burnLimitRaw > 0
    ? Math.floor(burnLimitRaw)
    : Number.POSITIVE_INFINITY;

  const { tezos, credentials, config } = await createTezosClient(network);

  let contractAddresses = providedAddresses;
  if (contractAddresses.length === 0) {
    contractAddresses = await discoverContractsByManager(credentials.address, network);
  }

  if (contractAddresses.length === 0) {
    throw new Error(`No contracts found for ${credentials.address} on ${network}`);
  }

  console.log('\n╔══════════════════════════════════════════════════════════════╗');
  console.log('║  🧹 Deprecating Staging Contracts                            ║');
  console.log('╚══════════════════════════════════════════════════════════════╝\n');

  console.log(`📡 Network: ${config.name}`);
  console.log(`👤 Wallet: ${credentials.address}`);
  console.log(`📦 Contracts: ${contractAddresses.length}`);
  if (!apply) {
    console.log('⚠️  DRY RUN (no transactions will be sent). Use --yes to apply.\n');
  } else {
    console.log('⚠️  APPLY MODE (transactions will be sent).\n');
  }

  const summary = [];

  for (const contractAddress of contractAddresses) {
    const row = {
      contractAddress,
      paused: null,
      metadataLocked: null,
      activeTokenCount: 0,
      burned: 0,
      operations: [],
      skipped: false,
      errors: [],
    };

    summary.push(row);
    console.log(`\n──────────────────────────────────────────────────────────────`);
    console.log(`📍 ${contractAddress}`);
    console.log(`🔗 ${config.explorer}/${contractAddress}`);

    try {
      const contract = await tezos.contract.at(contractAddress);
      const storage = await contract.storage();

      const looksLikeKeeps = storage
        && storage.metadata
        && storage.token_metadata
        && storage.keep_fee !== undefined
        && storage.default_royalty_bps !== undefined;

      if (!looksLikeKeeps) {
        row.skipped = true;
        console.log('   ⏭️  Skipping (does not match Keeps storage shape)');
        continue;
      }

      row.paused = storage.paused === true;
      row.metadataLocked = storage.contract_metadata_locked === true;
      const nextTokenId = Number(storage.next_token_id?.toNumber?.() ?? storage.next_token_id ?? 0);
      const tokenIds = await fetchActiveTokenIds(contractAddress, network);
      row.activeTokenCount = tokenIds.length;

      console.log(`   • Paused: ${row.paused}`);
      console.log(`   • Metadata locked: ${row.metadataLocked}`);
      console.log(`   • Next token id: ${nextTokenId}`);
      console.log(`   • Active tokens: ${row.activeTokenCount}`);

      if (!apply) {
        continue;
      }

      if (!row.metadataLocked) {
        try {
          const existingContent = await storage.metadata.get('content');
          const existingMetadata = decodeContractMetadataBytes(existingContent) || {};
          const deprecatedMetadata = buildDeprecatedCollectionMetadata(existingMetadata, {
            replacementContract,
          });
          const metadataBytes = stringToBytes(JSON.stringify(deprecatedMetadata));
          const metadataOp = await contract.methods.set_contract_metadata([
            { key: 'content', value: `0x${metadataBytes}` },
          ]).send();
          console.log(`   ⏳ Deprecation metadata op: ${metadataOp.hash}`);
          await metadataOp.confirmation(1);
          row.operations.push({ step: 'set_contract_metadata', hash: metadataOp.hash });
          console.log('   ✅ Collection metadata marked deprecated');
        } catch (error) {
          row.errors.push(`set_contract_metadata: ${error.message}`);
          console.log(`   ⚠️  Could not set deprecation metadata: ${error.message}`);
        }
      } else {
        console.log('   ℹ️  Metadata already locked; cannot write deprecation notice');
      }

      if (!row.paused) {
        try {
          const pauseOp = await contract.methodsObject.pause().send();
          console.log(`   ⏳ Pause op: ${pauseOp.hash}`);
          await pauseOp.confirmation(1);
          row.operations.push({ step: 'pause', hash: pauseOp.hash });
          row.paused = true;
          console.log('   ✅ Contract paused');
        } catch (error) {
          row.errors.push(`pause: ${error.message}`);
          console.log(`   ⚠️  Could not pause: ${error.message}`);
        }
      } else {
        console.log('   ℹ️  Already paused');
      }

      const burnQueue = Number.isFinite(burnLimit)
        ? tokenIds.slice(0, burnLimit)
        : tokenIds;

      if (burnQueue.length > 0) {
        console.log(`   🔥 Burning ${burnQueue.length} token(s)...`);
      }

      for (const tokenId of burnQueue) {
        try {
          const burnOp = await contract.methods.burn_keep(tokenId).send();
          console.log(`      ⏳ burn #${tokenId}: ${burnOp.hash}`);
          await burnOp.confirmation(1);
          row.operations.push({ step: 'burn_keep', tokenId, hash: burnOp.hash });
          row.burned += 1;
        } catch (error) {
          row.errors.push(`burn_keep(${tokenId}): ${error.message}`);
          console.log(`      ⚠️  burn #${tokenId} failed: ${error.message}`);
        }
      }

      if (!row.metadataLocked) {
        try {
          const lockOp = await contract.methods.lock_contract_metadata().send();
          console.log(`   ⏳ Lock metadata op: ${lockOp.hash}`);
          await lockOp.confirmation(1);
          row.operations.push({ step: 'lock_contract_metadata', hash: lockOp.hash });
          row.metadataLocked = true;
          console.log('   ✅ Collection metadata locked');
        } catch (error) {
          row.errors.push(`lock_contract_metadata: ${error.message}`);
          console.log(`   ⚠️  Could not lock metadata: ${error.message}`);
        }
      } else {
        console.log('   ℹ️  Metadata already locked');
      }
    } catch (error) {
      row.errors.push(error.message);
      console.log(`   ❌ Contract processing failed: ${error.message}`);
    }
  }

  console.log('\n══════════════════════════════════════════════════════════════');
  console.log('📋 Staging Deprecation Summary');
  for (const row of summary) {
    console.log(`   ${row.contractAddress}`);
    if (row.skipped) {
      console.log('      - skipped');
      continue;
    }
    console.log(`      - paused: ${row.paused}`);
    console.log(`      - metadataLocked: ${row.metadataLocked}`);
    console.log(`      - activeTokens(before): ${row.activeTokenCount}`);
    console.log(`      - burned: ${row.burned}`);
    console.log(`      - ops: ${row.operations.length}`);
    if (row.errors.length > 0) {
      console.log(`      - errors: ${row.errors.length}`);
      row.errors.forEach((message) => console.log(`        • ${message}`));
    }
  }
  console.log('');

  return {
    network,
    wallet: credentials.address,
    apply,
    contracts: summary,
  };
}

// ============================================================================
// CLI Interface
// ============================================================================

async function main() {
  const rawArgs = process.argv.slice(2);
  
  // Separate flags from positional arguments
  const flags = rawArgs.filter(a => a.startsWith('--'));
  const args = rawArgs.filter(a => !a.startsWith('--'));
  const command = args[0];
  const contractFlag = flags.find(f => f.startsWith('--contract=') || f.startsWith('--profile='));
  const contractProfile = contractFlag ? contractFlag.split('=').slice(1).join('=').trim() : 'v9';
  
  // Parse --wallet flag
  const walletFlag = flags.find(f => f.startsWith('--wallet='));
  if (walletFlag) {
    const wallet = walletFlag.split('=')[1];
    if (['keeps', 'kidlisp', 'aesthetic', 'staging'].includes(wallet)) {
      setWallet(wallet);
      console.log(`🔑 Using wallet: ${wallet}\n`);
    } else {
      console.error(`❌ Unknown wallet: ${wallet}. Use: keeps, kidlisp, aesthetic, or staging`);
      process.exit(1);
    }
  }
  
  // Helper to get network from args (defaults to mainnet)
  const getNetwork = (argIndex) => {
    const val = args[argIndex];
    if (!val || val.startsWith('--')) return 'mainnet';
    return val;
  };
  
  try {
    switch (command) {
      case 'deploy':
        await deployContract(getNetwork(1), { contractProfile });
        break;

      case 'sync-secrets':
        await syncCurrentContractToSecrets(getNetwork(1), { contractProfile });
        break;
        
      case 'status':
        await getContractStatus(getNetwork(1));
        break;
        
      case 'balance':
        await getBalance(getNetwork(1));
        break;

      case 'tokens': {
        const limitFlag = flags.find(f => f.startsWith('--limit='));
        const limit = limitFlag ? Number.parseInt(limitFlag.split('=')[1], 10) : undefined;
        await listOwnedTokens(getNetwork(1), { limit });
        break;
      }

      case 'market': {
        const listingsLimitFlag = flags.find(f => f.startsWith('--listings='));
        const salesLimitFlag = flags.find(f => f.startsWith('--sales='));
        const listingsLimit = listingsLimitFlag ? Number.parseInt(listingsLimitFlag.split('=')[1], 10) : undefined;
        const salesLimit = salesLimitFlag ? Number.parseInt(salesLimitFlag.split('=')[1], 10) : undefined;
        await showMarketSnapshot(getNetwork(1), { listingsLimit, salesLimit });
        break;
      }

      case 'sell': {
        if (!args[1] || !args[2]) {
          console.error('Usage: node keeps.mjs sell <token_id|$piece> <price_xtz> [network] [--marketplace=<KT1...>] [--replace] [--yes]');
          process.exit(1);
        }

        const marketplaceFlag = flags.find(f => f.startsWith('--marketplace='));
        const referralFlag = flags.find(f => f.startsWith('--referral-bps='));
        const startFlag = flags.find(f => f.startsWith('--start='));
        const expiryFlag = flags.find(f => f.startsWith('--expiry='));
        const marketplaceContract = marketplaceFlag ? marketplaceFlag.split('=').slice(1).join('=').trim() : null;
        const referralBonusBps = referralFlag ? Number.parseInt(referralFlag.split('=')[1], 10) : undefined;
        const startTime = parseOptionalIsoTimestamp(startFlag ? startFlag.split('=').slice(1).join('=') : null, 'start');
        const expiryTime = parseOptionalIsoTimestamp(expiryFlag ? expiryFlag.split('=').slice(1).join('=') : null, 'expiry');

        await listTokenForSale(args[1], args[2], {
          network: getNetwork(3),
          marketplaceContract,
          referralBonusBps,
          startTime,
          expiryTime,
          replaceExisting: flags.includes('--replace'),
          apply: flags.includes('--yes') || flags.includes('--apply'),
        });
        break;
      }

      case 'sell:batch': {
        const inputItems = args.slice(1);
        if (inputItems.length === 0) {
          console.error('Usage: node keeps.mjs sell:batch <token|piece=price_xtz> [...] [network] [--marketplace=<KT1...>] [--replace] [--yes]');
          process.exit(1);
        }

        let network = 'mainnet';
        if (['mainnet', 'ghostnet'].includes(inputItems[inputItems.length - 1])) {
          network = inputItems.pop();
        }

        if (inputItems.length === 0) {
          console.error('❌ No batch items supplied. Example: node keeps.mjs sell:batch \'$faim=5.5\' \'$tezz=5.8\' \'$bip=6\' --yes');
          process.exit(1);
        }

        const marketplaceFlag = flags.find(f => f.startsWith('--marketplace='));
        const referralFlag = flags.find(f => f.startsWith('--referral-bps='));
        const startFlag = flags.find(f => f.startsWith('--start='));
        const expiryFlag = flags.find(f => f.startsWith('--expiry='));
        const marketplaceContract = marketplaceFlag ? marketplaceFlag.split('=').slice(1).join('=').trim() : null;
        const referralBonusBps = referralFlag ? Number.parseInt(referralFlag.split('=')[1], 10) : undefined;
        const startTime = parseOptionalIsoTimestamp(startFlag ? startFlag.split('=').slice(1).join('=') : null, 'start');
        const expiryTime = parseOptionalIsoTimestamp(expiryFlag ? expiryFlag.split('=').slice(1).join('=') : null, 'expiry');

        await listBatchForSale(inputItems, {
          network,
          marketplaceContract,
          referralBonusBps,
          startTime,
          expiryTime,
          replaceExisting: flags.includes('--replace'),
          apply: flags.includes('--yes') || flags.includes('--apply'),
        });
        break;
      }

      case 'accept': {
        if (!args[1]) {
          console.error('Usage: node keeps.mjs accept <offer_id> [network] [--marketplace=<KT1...>] [--min=<xtz>] [--yes]');
          process.exit(1);
        }

        const marketplaceFlag = flags.find(f => f.startsWith('--marketplace='));
        const minFlag = flags.find(f => f.startsWith('--min='));
        const marketplaceContract = marketplaceFlag ? marketplaceFlag.split('=').slice(1).join('=').trim() : null;

        let minPriceMutez = null;
        if (minFlag) {
          const raw = minFlag.split('=').slice(1).join('=').trim();
          const minXTZ = Number.parseFloat(raw);
          if (!Number.isFinite(minXTZ) || minXTZ < 0) {
            console.error(`❌ Invalid --min value "${raw}". Expected a non-negative XTZ amount.`);
            process.exit(1);
          }
          minPriceMutez = Math.round(minXTZ * 1_000_000);
        }

        await acceptOffer(args[1], {
          network: getNetwork(2),
          marketplaceContract,
          minPriceMutez,
          apply: flags.includes('--yes') || flags.includes('--apply'),
        });
        break;
      }

      case 'accept:auto': {
        const inputItems = args.slice(1);
        if (inputItems.length === 0) {
          console.error('Usage: node keeps.mjs accept:auto <token|piece=min_xtz> [...] [network] [--marketplace=<KT1...>] [--yes]');
          process.exit(1);
        }

        let network = 'mainnet';
        if (['mainnet', 'ghostnet'].includes(inputItems[inputItems.length - 1])) {
          network = inputItems.pop();
        }

        if (inputItems.length === 0) {
          console.error('❌ No accept:auto items supplied. Example: node keeps.mjs accept:auto \'$faim=8\' \'$tezz=9.5\' --yes');
          process.exit(1);
        }

        const marketplaceFlag = flags.find(f => f.startsWith('--marketplace='));
        const marketplaceContract = marketplaceFlag ? marketplaceFlag.split('=').slice(1).join('=').trim() : null;

        await acceptOffersAboveThreshold(inputItems, {
          network,
          marketplaceContract,
          apply: flags.includes('--yes') || flags.includes('--apply'),
        });
        break;
      }
        
      case 'buy': {
        if (!args[1]) {
          console.error('Usage: node keeps.mjs buy <ask_id> [network] [--marketplace=<KT1...>] [--yes]');
          console.error('');
          console.error('Fulfill an active Objkt marketplace listing (ask) to buy a token.');
          console.error('');
          console.error('Examples:');
          console.error('  node keeps.mjs buy 12589569 --wallet=aesthetic           # Dry run');
          console.error('  node keeps.mjs buy 12589569 --wallet=aesthetic --yes     # Live purchase');
          process.exit(1);
        }

        const buyMarketFlag = flags.find(f => f.startsWith('--marketplace='));
        const buyMarketplace = buyMarketFlag ? buyMarketFlag.split('=').slice(1).join('=').trim() : null;

        await buyToken(args[1], {
          network: getNetwork(2),
          marketplaceContract: buyMarketplace,
          apply: flags.includes('--yes') || flags.includes('--apply'),
        });
        break;
      }

      case 'upload':
        if (!args[1]) {
          console.error('Usage: node keeps.mjs upload <piece>');
          process.exit(1);
        }
        await uploadToIPFS(args[1]);
        break;
        
      case 'mint':
      case 'keep': {
        if (!args[1]) {
          console.error('Usage: node keeps.mjs keep <piece> [network] [--thumbnail] [--to=<address>] [--yes]');
          process.exit(1);
        }
        const toFlag = flags.find(f => f.startsWith('--to='));
        const recipientAddr = toFlag ? toFlag.split('=')[1] : null;
        await mintToken(args[1], { 
          network: getNetwork(2),
          generateThumbnail: flags.includes('--thumbnail'),
          recipient: recipientAddr,
          skipConfirm: flags.includes('--yes') || flags.includes('-y')
        });
        break;
      }
      
      case 'update':
        if (!args[1] || !args[2]) {
          console.error('Usage: node keeps.mjs update <token_id> <piece> [--thumbnail]');
          process.exit(1);
        }
        await updateMetadata(parseInt(args[1]), args[2], { 
          network: getNetwork(3),
          generateThumbnail: flags.includes('--thumbnail')
        });
        break;
      
      case 'lock':
        if (!args[1]) {
          console.error('Usage: node keeps.mjs lock <token_id>');
          process.exit(1);
        }
        await lockMetadata(parseInt(args[1]), { network: getNetwork(2) });
        break;
      
      case 'burn':
        if (!args[1]) {
          console.error('Usage: node keeps.mjs burn <token_id>');
          process.exit(1);
        }
        await burnToken(parseInt(args[1]), { network: getNetwork(2) });
        break;
      
      case 'redact': {
        if (!args[1]) {
          console.error('Usage: node keeps.mjs redact <token_id> [--reason="..."]');
          process.exit(1);
        }
        const reasonFlag = flags.find(f => f.startsWith('--reason='));
        const reason = reasonFlag ? reasonFlag.split('=').slice(1).join('=') : 'Content has been redacted.';
        await redactToken(parseInt(args[1]), { network: getNetwork(2), reason });
        break;
      }
      
      case 'set-collection-media': {
        // Parse --name=<text>, --image=<uri>, --homepage=<url> and --description=<text> flags
        const nameFlag = flags.find(f => f.startsWith('--name='));
        const imageFlag = flags.find(f => f.startsWith('--image='));
        const homepageFlag = flags.find(f => f.startsWith('--homepage='));
        const descFlag = flags.find(f => f.startsWith('--description='));
        
        const name = nameFlag ? nameFlag.split('=').slice(1).join('=') : undefined;
        const imageUri = imageFlag ? imageFlag.split('=').slice(1).join('=') : undefined;
        const homepage = homepageFlag ? homepageFlag.split('=').slice(1).join('=') : undefined;
        const description = descFlag ? descFlag.split('=').slice(1).join('=') : undefined;
        
        if (!name && !imageUri && !homepage && !description) {
          console.error('Usage: node keeps.mjs set-collection-media [--name=<text>] [--image=<ipfs-uri>] [--homepage=<url>] [--description=<text>]');
          console.error('');
          console.error('Examples:');
          console.error('  node keeps.mjs set-collection-media --name="KidLisp Keeps (Staging)"');
          console.error('  node keeps.mjs set-collection-media --image=ipfs://Qm...');
          console.error('  node keeps.mjs set-collection-media --image=https://oven.aesthetic.computer/keeps/latest');
          console.error('  node keeps.mjs set-collection-media --homepage=https://keep.kidlisp.com');
          console.error('  node keeps.mjs set-collection-media --description="KidLisp generative art collection"');
          process.exit(1);
        }
        
        await setCollectionMedia({ 
          network: getNetwork(1),
          name,
          imageUri,
          homepage,
          description 
        });
        break;
      }
      
      case 'lock-collection':
        await lockCollectionMetadata({ network: getNetwork(1) });
        break;

      case 'deprecate-staging': {
        const addressesFlag = flags.find(f => f.startsWith('--addresses='));
        const addresses = addressesFlag
          ? addressesFlag.split('=').slice(1).join('=').split(',').map(v => v.trim()).filter(Boolean)
          : [];
        const replacementFlag = flags.find(f => f.startsWith('--replacement='));
        const replacementContract = replacementFlag ? replacementFlag.split('=').slice(1).join('=').trim() : null;
        const burnLimitFlag = flags.find(f => f.startsWith('--burn-limit='));
        const burnLimit = burnLimitFlag ? Number.parseInt(burnLimitFlag.split('=')[1], 10) : undefined;
        const apply = flags.includes('--yes') || flags.includes('--apply');

        await deprecateStagingContracts({
          network: getNetwork(1),
          addresses,
          replacementContract,
          burnLimit,
          apply,
        });
        break;
      }
      
      case 'fee':
        // Show current keep fee
        const feeInfo = await getKeepFee(getNetwork(1));
        console.log('\n╔══════════════════════════════════════════════════════════════╗');
        console.log('║  💰 Current Keep Fee                                         ║');
        console.log('╚══════════════════════════════════════════════════════════════╝\n');
        console.log(`   Contract: ${feeInfo.contractAddress}`);
        console.log(`   Keep Fee: ${feeInfo.feeInTez} XTZ (${feeInfo.feeInMutez} mutez)\n`);
        break;
      
      case 'set-fee': {
        if (!args[1]) {
          console.error('Usage: node keeps.mjs set-fee <amount_in_tez>');
          console.error('');
          console.error('Examples:');
          console.error('  node keeps.mjs set-fee 5       # Set fee to 5 XTZ');
          console.error('  node keeps.mjs set-fee 0       # Free keeping');
          console.error('  node keeps.mjs set-fee 0.5     # Set fee to 0.5 XTZ');
          process.exit(1);
        }
        const feeAmount = parseFloat(args[1]);
        if (isNaN(feeAmount) || feeAmount < 0) {
          console.error('❌ Invalid fee amount. Must be a non-negative number.');
          process.exit(1);
        }
        await setKeepFee(feeAmount, { network: getNetwork(2) });
        break;
      }
      
      case 'withdraw': {
        const dest = args[1]; // Optional destination address
        await withdrawFees(dest, { network: getNetwork(dest ? 2 : 1) });
        break;
      }
      
      case 'set-admin': {
        if (!args[1]) {
          console.error('Usage: node keeps.mjs set-admin <new_admin_address>');
          console.error('');
          console.error('This changes the contract administrator. Only the current admin can call this.');
          console.error('');
          console.error('Examples:');
          console.error('  node keeps.mjs set-admin tz1abc...   # Set new admin');
          process.exit(1);
        }
        const newAdmin = args[1];
        if (!newAdmin.startsWith('tz1') && !newAdmin.startsWith('tz2') && !newAdmin.startsWith('tz3')) {
          console.error('❌ Invalid Tezos address. Must start with tz1, tz2, or tz3.');
          process.exit(1);
        }
        await setAdministrator(newAdmin, { network: getNetwork(2) });
        break;
      }

      // v4 NEW COMMANDS
      case 'royalty':
      case 'royalty:get':
        await getRoyalty(getNetwork(1));
        break;

      case 'royalty:set': {
        if (!args[1]) {
          console.error('Usage: node keeps.mjs royalty:set <percentage> [network]');
          console.error('');
          console.error('Examples:');
          console.error('  node keeps.mjs royalty:set 10      # Set royalty to 10%');
          console.error('  node keeps.mjs royalty:set 15      # Set royalty to 15%');
          console.error('  node keeps.mjs royalty:set 0       # No royalties');
          console.error('');
          console.error('Maximum: 25%');
          process.exit(1);
        }
        const percentage = parseFloat(args[1]);
        if (isNaN(percentage)) {
          console.error('❌ Invalid percentage. Must be a number.');
          process.exit(1);
        }
        await setRoyalty(percentage, { network: getNetwork(2) });
        break;
      }

      case 'pause':
        await pauseContract({ network: getNetwork(1) });
        break;

      case 'unpause':
        await unpauseContract({ network: getNetwork(1) });
        break;

      case 'send': {
        if (!args[1] || !args[2]) {
          console.error('Usage: node keeps.mjs send <to_address> <amount_xtz> [network]');
          console.error('');
          console.error('Examples:');
          console.error('  node keeps.mjs send aesthetic.tez 3 --wallet=staging --yes');
          process.exit(1);
        }
        const sendTo = args[1];
        const sendAmt = parseFloat(args[2]);
        if (isNaN(sendAmt) || sendAmt <= 0) {
          console.error('❌ Invalid amount.');
          process.exit(1);
        }
        await sendTez(sendTo, sendAmt, getNetwork(3));
        break;
      }

      case 'transfer': {
        if (!args[1] || !args[2]) {
          console.error('Usage: node keeps.mjs transfer <token_id> <to_address> [network]');
          console.error('');
          console.error('Examples:');
          console.error('  node keeps.mjs transfer 53 reas.tez --wallet=aesthetic --yes');
          console.error('');
          console.error('Automatically retracts active Objkt listing if one exists.');
          process.exit(1);
        }
        const xferTokenId = parseInt(args[1]);
        const xferTo = args[2];
        if (!xferTo.startsWith('tz') && !xferTo.endsWith('.tez')) {
          console.error('❌ Invalid Tezos address.');
          process.exit(1);
        }
        await transferToken(xferTokenId, xferTo, getNetwork(3));
        break;
      }

      case 'transfer:admin': {
        console.error('⚠️  transfer:admin is deprecated (v10 contract has no admin_transfer).');
        console.error('   Use: node keeps.mjs transfer <token_id> <to_address> --wallet=aesthetic');
        process.exit(1);
      }

      case 'help':
      default:
        console.log(`
╔══════════════════════════════════════════════════════════════╗
║  🔮 Keeps - Tezos FA2 Contract Manager                       ║
╚══════════════════════════════════════════════════════════════╝

Usage: node keeps.mjs <command> [options]

Commands:
  deploy [network]              Deploy contract (default profile: v10)
  sync-secrets [network]        Sync active contract/profile to Mongo secrets
  status [network]              Show contract status
  balance [network]             Check wallet balance
  tokens [network]              List tokens held by current wallet
  market [network]              Show Objkt listings/offers/sales snapshot
  upload <piece>                Upload bundle to IPFS
  mint <piece> [network]        Mint a new keep
  sell <token> <price> [network]  List token on Objkt (dry-run unless --yes)
  sell:batch <ref=price>...     Batch-list multiple tokens on Objkt
  accept <offer_id> [network]   Accept one active Objkt offer (dry-run unless --yes)
  accept:auto <ref=min>...      Accept best offers above per-token thresholds
  buy <ask_id> [network]        Buy a listed token (fulfill_ask, dry-run unless --yes)
  update <token_id> <piece>     Update token metadata (re-upload bundle)
  lock <token_id>               Permanently lock token metadata
  burn <token_id>               Burn token (allows re-keeping piece)
  redact <token_id>             Censor token (replace with redacted content)
  set-collection-media          Set collection icon/description
  lock-collection               Permanently lock collection metadata
  deprecate-staging [network]   Pause + deprecate + burn staging contracts
  fee [network]                 Show current keep fee
  set-fee <tez> [network]       Set keep fee (admin only)
  set-admin <address>           Change contract administrator (admin only)
  withdraw [dest] [network]     Withdraw accumulated fees to address

v4 Commands (Royalties, Pause, Admin Transfer):
  royalty [network]             Show current default royalty percentage
  royalty:set <pct> [network]   Set default royalty (0-25%, admin only)
  pause [network]               Emergency pause (stops minting, admin only)
  unpause [network]             Resume operations (admin only)
  transfer <id> <to>               Transfer token (auto-unlists, supports .tez domains)

  help                          Show this help

Networks:
  mainnet                       Tezos mainnet (default)
  ghostnet                      Tezos ghostnet (testnet)

Flags:
  --contract=<profile>          Deploy profile: v10 | v9 | v8 | v7 | v6 | v5rc | v4
  --thumbnail                   Generate animated WebP thumbnail via Oven
                               and upload to IPFS (requires Oven service)
  --to=<address>                Recipient wallet address (default: server wallet)
  --limit=<n>                   Max rows for tokens command
  --listings=<n>                Max rows for market listings/offers
  --sales=<n>                   Max rows for market sales
  --marketplace=<KT1...>        Override marketplace contract for sell/accept
  --replace                     Retract existing ask before listing new price
  --min=<xtz>                   Minimum bid filter for accept <offer_id>
  --referral-bps=<n>            Referral bonus bps for marketplace ask (default: 500)
  --start=<iso8601>             Scheduled listing start time
  --expiry=<iso8601>            Listing expiry time
  --image=<uri>                 Collection image URI (IPFS or URL)
  --homepage=<url>              Collection homepage URL
  --description=<text>          Collection description
  --addresses=<KT1,KT1,...>     Explicit contract list for deprecate-staging
  --replacement=<KT1...>        Replacement contract for deprecation notice
  --burn-limit=<n>              Limit number of burns per contract
  --yes / --apply               Send live transactions for destructive commands

Examples:
  node keeps.mjs deploy mainnet --wallet=kidlisp --contract=v9
  node keeps.mjs sync-secrets mainnet --contract=v9
  node keeps.mjs deploy mainnet --wallet=kidlisp --contract=v8
  node keeps.mjs deploy mainnet --wallet=kidlisp --contract=v7
  node keeps.mjs deploy mainnet --wallet=kidlisp --contract=v6
  node keeps.mjs deploy mainnet --wallet=staging --contract=v5rc
  node keeps.mjs deploy ghostnet --wallet=aesthetic --contract=v4
  node keeps.mjs balance
  node keeps.mjs tokens --wallet=aesthetic
  node keeps.mjs market
  node keeps.mjs mint wand --thumbnail      # With IPFS thumbnail
  node keeps.mjs mint wand --to=tz1abc...   # Mint to specific wallet
  node keeps.mjs sell '$bip' 6 --wallet=aesthetic --yes
  node keeps.mjs sell:batch '$faim=5.5' '$tezz=5.8' '$bip=6' --wallet=aesthetic --yes
  node keeps.mjs accept 12179750 --wallet=aesthetic --min=13 --yes
  node keeps.mjs accept:auto '19=8.5' '20=9' '21=10' '22=10.5' '23=12' '24=13' --wallet=aesthetic --yes
  node keeps.mjs buy 12589569 --wallet=aesthetic --yes
  node keeps.mjs update 0 wand              # Re-upload bundle & update metadata
  node keeps.mjs lock 0                     # Permanently lock token 0
  node keeps.mjs burn 0                     # Burn token 0 (allows re-mint)

v4 Examples:
  node keeps.mjs royalty:set 10             # Set royalty to 10%
  node keeps.mjs royalty                    # View current royalty
  node keeps.mjs pause                      # Emergency pause
  node keeps.mjs unpause                    # Resume operations
  node keeps.mjs transfer:admin 5 tz1... tz1...  # Emergency transfer
  
  # Fee management
  node keeps.mjs fee                        # Show current keep fee
  node keeps.mjs set-fee 5                  # Set keep fee to 5 XTZ
  node keeps.mjs set-fee 0                  # Make keeping free
  node keeps.mjs withdraw                   # Withdraw fees to admin wallet
  node keeps.mjs withdraw tz1abc...         # Withdraw fees to specific address
  
  # Collection media (use live endpoint for dynamic thumbnail)
  node keeps.mjs set-collection-media --image=https://oven.aesthetic.computer/keeps/latest
  node keeps.mjs set-collection-media --homepage=https://keep.kidlisp.com
  node keeps.mjs set-collection-media --image=ipfs://QmXxx --description="KidLisp art"
  node keeps.mjs lock-collection            # Lock collection metadata forever
  node keeps.mjs deprecate-staging --wallet=staging                     # Dry run
  node keeps.mjs deprecate-staging --wallet=staging --yes --replacement=KT1...

Environment:
  OVEN_URL                      Oven service URL (default: https://oven.aesthetic.computer)
`);
    }
  } catch (error) {
    console.error(`\n❌ Error: ${error.message}\n`);
    const details = formatExtendedError(error);
    if (details) {
      console.error(`${details}\n`);
    }
    process.exit(1);
  }
}

// Run CLI if executed directly
if (process.argv[1] === fileURLToPath(import.meta.url)) {
  main();
}

// Export for use as module
export {
  createTezosClient,
  deployContract,
  syncCurrentContractToSecrets,
  getContractStatus,
  getBalance,
  listOwnedTokens,
  showMarketSnapshot,
  listTokenForSale,
  listBatchForSale,
  acceptOffer,
  acceptOffersAboveThreshold,
  buyToken,
  uploadToIPFS,
  mintToken,
  updateMetadata,
  lockMetadata,
  burnToken,
  redactToken,
  setCollectionMedia,
  lockCollectionMetadata,
  getKeepFee,
  setKeepFee,
  setAdministrator,
  withdrawFees,
  deprecateStagingContracts,
  detectContentType,
  loadCredentials,
  CONFIG
};
