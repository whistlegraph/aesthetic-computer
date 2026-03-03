/**
 * Keeps v4 Test Helper
 *
 * Shared utilities for testing the Keeps FA2 v4 contract.
 * Provides Tezos client setup, credential management, and common helpers.
 */

import fs from 'fs';
import path from 'path';
import dotenv from 'dotenv';
import { TezosToolkit } from '@taquito/taquito';
import { InMemorySigner } from '@taquito/signer';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

function envBool(value, fallback = false) {
  if (value === undefined) return fallback;
  return String(value).toLowerCase() === 'true';
}

function envNat(value, fallback) {
  if (value === undefined || value === '') return fallback;
  const parsed = Number.parseInt(String(value), 10);
  return Number.isNaN(parsed) ? fallback : parsed;
}

const MAINNET_CONTRACT_DEFAULT = 'KT1QdGZP8jzqaxXDia3U7DYEqFYhfqGRHido'; // v5 RC
const MAINNET_ADMIN_DEFAULT = 'tz1dfoQDuxjwSgxdqJnisyKUxDHweade4Gzt';
const MAINNET_RPC_DEFAULT = 'https://mainnet.api.tez.ie';
const GHOSTNET_RPC_DEFAULT = 'https://rpc.ghostnet.teztnets.com';

// Contract addresses (override for v6 audits with env vars)
export const CONTRACTS = {
  mainnet:
    process.env.KEEPS_AUDIT_CONTRACT ||
    process.env.KEEPS_CONTRACT ||
    process.env.TEZOS_KEEPS_CONTRACT ||
    MAINNET_CONTRACT_DEFAULT,
  mainnet_v4: 'KT1ER1GyoeRNhkv6E57yKbBbEKi5ynKbaH3W', // v4 staging (legacy)
  ghostnet: process.env.KEEPS_GHOSTNET_CONTRACT || null // To be deployed for testing
};

// RPC endpoints (override per network if needed)
export const RPCS = {
  mainnet: process.env.KEEPS_MAINNET_RPC || MAINNET_RPC_DEFAULT,
  ghostnet: process.env.KEEPS_GHOSTNET_RPC || GHOSTNET_RPC_DEFAULT
};

// Expected storage values (override for deployed v6 contract checks)
export const EXPECTED_STORAGE = {
  mainnet: {
    administrator:
      process.env.KEEPS_EXPECTED_ADMIN ||
      process.env.KEEPS_ADMIN ||
      MAINNET_ADMIN_DEFAULT,
    default_royalty_bps: envNat(process.env.KEEPS_EXPECTED_ROYALTY_BPS, 1000), // 10%
    paused: envBool(process.env.KEEPS_EXPECTED_PAUSED, false)
  }
};

/**
 * Load credentials from staging/.env file
 * @param {string} wallet - Wallet name (default: 'staging')
 * @returns {Promise<{address: string, secretKey: string}>}
 */
export async function loadCredentials(wallet = 'staging') {
  const envPath = path.join(
    __dirname,
    '../../tezos/staging/.env'
  );

  if (!fs.existsSync(envPath)) {
    throw new Error(`Credentials not found: ${envPath}`);
  }

  const envContent = fs.readFileSync(envPath, 'utf8');
  const config = {};

  // Parse .env file manually
  envContent.split('\n').forEach(line => {
    const trimmed = line.trim();
    if (!trimmed || trimmed.startsWith('#')) return;

    const [key, ...valueParts] = trimmed.split('=');
    if (key && valueParts.length > 0) {
      config[key.trim()] = valueParts.join('=').trim().replace(/^["']|["']$/g, '');
    }
  });

  const address = config.STAGING_ADDRESS || config.ADDRESS;
  const secretKey = config.STAGING_KEY || config.KEY || config.SECRET_KEY;

  if (!address || !secretKey) {
    throw new Error('Could not load address or secret key from credentials file');
  }

  return { address, secretKey };
}

/**
 * Create Tezos client (read-only or with signer)
 * @param {string} network - Network name ('mainnet' or 'ghostnet')
 * @param {boolean} withSigner - Whether to add signer for write operations
 * @returns {Promise<TezosToolkit>}
 */
export async function createTezosClient(network = 'mainnet', withSigner = false) {
  const rpc = RPCS[network];
  if (!rpc) {
    throw new Error(`Unknown network: ${network}`);
  }

  const tezos = new TezosToolkit(rpc);

  if (withSigner) {
    const creds = await loadCredentials();
    const signer = new InMemorySigner(creds.secretKey);
    tezos.setProvider({ signer });
    console.log(`✅ Tezos client created for ${network} with signer (${creds.address})`);
  } else {
    console.log(`✅ Tezos client created for ${network} (read-only)`);
  }

  return tezos;
}

/**
 * Get contract instance
 * @param {string} network - Network name ('mainnet' or 'ghostnet')
 * @param {boolean} withSigner - Whether to add signer for write operations
 * @returns {Promise<{tezos: TezosToolkit, contract: any, address: string}>}
 */
export async function getContract(network = 'mainnet', withSigner = false) {
  const tezos = await createTezosClient(network, withSigner);
  const address = CONTRACTS[network];

  if (!address) {
    throw new Error(`No contract address for network: ${network}`);
  }

  console.log(`📋 Loading contract at ${address}...`);
  const contract = await tezos.contract.at(address);
  console.log(`✅ Contract loaded successfully`);

  return { tezos, contract, address };
}

/**
 * Convert string to hex bytes (for contract parameters)
 * @param {string} str - String to convert
 * @returns {string} Hex string
 */
export function stringToBytes(str) {
  return Buffer.from(str, 'utf8').toString('hex');
}

/**
 * Convert hex bytes to string
 * @param {string} bytes - Hex string
 * @returns {string} UTF-8 string
 */
export function bytesToString(bytes) {
  return Buffer.from(bytes, 'hex').toString('utf8');
}

/**
 * Wait for operation confirmation
 * @param {any} operation - Taquito operation
 * @param {number} confirmations - Number of confirmations to wait for
 * @returns {Promise<any>}
 */
export async function waitForConfirmation(operation, confirmations = 1) {
  console.log(`⏳ Waiting for ${confirmations} confirmation(s)...`);
  await operation.confirmation(confirmations);
  console.log(`✅ Operation confirmed: ${operation.hash}`);
  return operation;
}

/**
 * Create valid keep parameters for testing
 * @param {object} overrides - Override default parameters
 * @returns {object} Keep parameters
 */
export function createKeepParams(overrides = {}) {
  const defaults = {
    name: stringToBytes("Test Keep"),
    description: stringToBytes("Test description"),
    artifactUri: stringToBytes("ipfs://QmTest123"),
    displayUri: stringToBytes("ipfs://QmTest123"),
    thumbnailUri: stringToBytes("ipfs://QmTestThumb"),
    decimals: stringToBytes("0"),
    symbol: stringToBytes("KEEP"),
    isBooleanAmount: stringToBytes("true"),
    shouldPreferSymbol: stringToBytes("false"),
    formats: stringToBytes('[{"uri":"ipfs://QmTest123","mimeType":"text/html"}]'),
    tags: stringToBytes('["test"]'),
    attributes: stringToBytes('[]'),
    creators: stringToBytes('[{"address":"tz1test","share":"100"}]'),
    royalties: stringToBytes('[{"address":"tz1test","share":"100"}]'),
    rights: stringToBytes("© 2025 Test"),
    content_type: stringToBytes("text/html"),
    content_hash: stringToBytes(`test-hash-${Date.now()}`),
    metadata_uri: stringToBytes("ipfs://QmTestMeta"),
    owner: "tz1dfoQDuxjwSgxdqJnisyKUxDHweade4Gzt"
  };

  return { ...defaults, ...overrides };
}

/**
 * Sleep for specified milliseconds
 * @param {number} ms - Milliseconds to sleep
 * @returns {Promise<void>}
 */
export function sleep(ms) {
  return new Promise(resolve => setTimeout(resolve, ms));
}
