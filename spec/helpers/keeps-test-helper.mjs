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

// Contract addresses
export const CONTRACTS = {
  mainnet: 'KT1QdGZP8jzqaxXDia3U7DYEqFYhfqGRHido', // v5 RC
  mainnet_v4: 'KT1ER1GyoeRNhkv6E57yKbBbEKi5ynKbaH3W', // v4 staging (legacy)
  ghostnet: null // To be deployed for testing
};

// RPC endpoints
export const RPCS = {
  mainnet: 'https://mainnet.api.tez.ie',
  ghostnet: 'https://rpc.ghostnet.teztnets.com'
};

// Expected storage values
export const EXPECTED_STORAGE = {
  mainnet: {
    administrator: 'tz1dfoQDuxjwSgxdqJnisyKUxDHweade4Gzt',
    default_royalty_bps: 1000, // 10%
    paused: false
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
