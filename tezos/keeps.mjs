#!/usr/bin/env node
/**
 * 🔮 Keeps - Tezos FA2 Contract Management with Taquito
 * 
 * A comprehensive Node.js module for deploying and managing
 * the Aesthetic Computer "Keeps" FA2 contract on Tezos.
 * 
 * Usage:
 *   node keeps.mjs deploy              - Deploy contract to Ghostnet
 *   node keeps.mjs keep <piece>        - Keep (preserve) a KidLisp piece
 *   node keeps.mjs status              - Check contract status
 *   node keeps.mjs balance             - Check wallet balance
 *   node keeps.mjs tokens              - List all kept tokens
 *   node keeps.mjs upload <piece>      - Upload bundle to IPFS
 */

import { TezosToolkit, MichelsonMap } from '@taquito/taquito';
import { InMemorySigner } from '@taquito/signer';
import { Parser } from '@taquito/michel-codec';
import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';
import crypto from 'crypto';
import readline from 'readline';

const __dirname = path.dirname(fileURLToPath(import.meta.url));

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
    // SmartPy v4 contract (PRODUCTION: royalties, pause, admin transfer)
    contract: path.join(__dirname, 'KeepsFA2v4/step_002_cont_0_contract.tz'),
    storage: path.join(__dirname, 'KeepsFA2v4/step_002_cont_0_storage.tz'),
    // Legacy v3 contract path
    v3Contract: path.join(__dirname, 'KeepsFA2v3/step_002_cont_0_contract.tz'),
    // Legacy v2 contract path
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

async function deployContract(network = 'mainnet') {
  console.log('\n╔══════════════════════════════════════════════════════════════╗');
  console.log('║  🚀 Deploying Keeps FA2 v4 Contract (PRODUCTION)             ║');
  console.log('║  (Royalties + Pause + Admin Transfer)                       ║');
  console.log('╚══════════════════════════════════════════════════════════════╝\n');
  
  const { tezos, credentials, config } = await createTezosClient(network);
  
  console.log(`📡 Network: ${config.name}`);
  console.log(`📍 RPC: ${config.rpc}`);
  console.log(`👤 Administrator: ${credentials.address}\n`);
  
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
  if (!fs.existsSync(CONFIG.paths.contract)) {
    throw new Error(`❌ Contract file not found: ${CONFIG.paths.contract}\n   Run: cd tezos && ./compile.fish`);
  }
  
  const contractSource = fs.readFileSync(CONFIG.paths.contract, 'utf8');
  console.log('   ✓ Contract loaded: KeepsFA2v4 (Production: royalties, pause, admin transfer)');
  
  // Parse the contract using michel-codec
  const parser = new Parser();
  const parsedContract = parser.parseScript(contractSource);
  
  // Create initial storage
  // Storage structure from SmartPy FA2 v4:
  // Layout: ("administrator", ("content_hashes", ("contract_metadata_locked", ("default_royalty_bps",
  //   ("keep_fee", ("ledger", ("metadata", ("metadata_locked", ("next_token_id",
  //   ("operators", ("paused", ("token_creators", "token_metadata"))))))))))))
  console.log('\n💾 Creating initial storage...');

  // Build contract metadata (TZIP-16)
  const contractMetadataJson = JSON.stringify({
    name: "KidLisp Keeps Beta",
    version: "4.0.0",
    interfaces: ["TZIP-012", "TZIP-016", "TZIP-021"],
    authors: ["aesthetic.computer"],
    homepage: "https://aesthetic.computer",
    imageUri: "https://oven.aesthetic.computer/keeps/latest"
  });
  const contractMetadataBytes = stringToBytes(contractMetadataJson);
  const tezosStoragePointer = stringToBytes("tezos-storage:content");

  // Initial storage Michelson - v4 format with royalties and pause
  // Format: (Pair admin (Pair {} (Pair False (Pair 1000 (Pair 0 (Pair {} (Pair metadata (Pair {} (Pair 0 (Pair {} (Pair False (Pair {} {}))))))))))))
  const initialStorageMichelson = `(Pair "${credentials.address}" (Pair {} (Pair False (Pair 1000 (Pair 0 (Pair {} (Pair {Elt "" 0x${tezosStoragePointer}; Elt "content" 0x${contractMetadataBytes}} (Pair {} (Pair 0 (Pair {} (Pair False (Pair {} {}))))))))))))`;

  const parsedStorage = parser.parseMichelineExpression(initialStorageMichelson);

  console.log(`   ✓ Administrator: ${credentials.address}`);
  console.log('   ✓ Initial token ID: 0');
  console.log('   ✓ Keep fee: 0 mutez (free)');
  console.log('   ✓ Default royalty: 10% (1000 basis points)');
  console.log('   ✓ Paused: false');
  console.log('   ✓ Contract metadata: TZIP-16 compliant');
  console.log('   ✓ Collection image: https://oven.aesthetic.computer/keeps/latest');
  console.log('   ✓ Content hash uniqueness: enabled');
  console.log('   ✓ Token creators tracking: enabled (v4)');
  
  // Deploy
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
    console.log(`   📝 Operation: ${config.explorer}/${originationOp.hash}\n`);
    
    // Save contract address (network-specific file)
    const addressPath = getContractAddressPath(network);
    fs.writeFileSync(addressPath, contractAddress);
    console.log(`   💾 Saved address to: ${addressPath}\n`);
    
    return { address: contractAddress, hash: originationOp.hash };
    
  } catch (error) {
    console.error('\n❌ Deployment failed!');
    console.error(`   Error: ${error.message}`);
    if (error.message.includes('bad_stack')) {
      console.error('\n   💡 This usually means storage format mismatch.');
      console.error('   Check that the storage matches the contract type.');
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
    console.log(`   ✓ User Code: ${data.userCode}`);
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
  
  // Build tags (include userCode if available)
  const tags = [
    `$${pieceName}`,           // The code itself as a tag
    'KidLisp',                 // Capitalized
    'Aesthetic.Computer',      // Capitalized with dot
    'interactive',
  ];
  if (userCode) {
    tags.push(userCode);       // Add permanent user code as tag
  }
  
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
    mintingTool: 'https://aesthetic.computer',
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
      ...(userCode ? [{ name: 'User Code', value: userCode }] : []),
      ...(sourceCode ? [{ name: 'Lines of Code', value: String(sourceCode.split('\n').length) }] : []),
      ...(depCount > 0 ? [{ name: 'Dependencies', value: String(depCount) }] : []),
      ...(packDate ? [{ name: 'Packed', value: packDate }] : []),
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
    const op = await contract.methodsObject.keep({
      artifactUri: onChainMetadata.artifactUri,
      attributes: onChainMetadata.attributes,
      content_hash: onChainMetadata.content_hash,
      content_type: onChainMetadata.content_type,
      creators: onChainMetadata.creators,
      decimals: onChainMetadata.decimals,
      description: onChainMetadata.description,
      displayUri: onChainMetadata.displayUri,
      formats: onChainMetadata.formats,
      isBooleanAmount: onChainMetadata.isBooleanAmount,
      metadata_uri: onChainMetadata.metadata_uri,
      name: onChainMetadata.name,
      owner: ownerAddress,
      rights: onChainMetadata.rights,
      shouldPreferSymbol: onChainMetadata.shouldPreferSymbol,
      symbol: onChainMetadata.symbol,
      tags: onChainMetadata.tags,
      thumbnailUri: onChainMetadata.thumbnailUri
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
  
  // Build tags
  const tags = [
    `$${pieceName}`,
    'KidLisp',
    'Aesthetic.Computer',
    'interactive',
  ];
  if (userCode) {
    tags.push(userCode);
  }
  
  // Build improved attributes
  const attributes = [
    { name: 'Language', value: 'KidLisp' },
    { name: 'Code', value: `$${pieceName}` },
    ...(authorDisplayName ? [{ name: 'Author', value: authorDisplayName }] : []),
    ...(userCode ? [{ name: 'User Code', value: userCode }] : []),
    ...(sourceCode ? [{ name: 'Lines of Code', value: String(sourceCode.split('\n').length) }] : []),
    ...(depCount > 0 ? [{ name: 'Dependencies', value: String(depCount) }] : []),
    ...(packDate ? [{ name: 'Packed', value: packDate }] : []),
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
    mintingTool: 'https://aesthetic.computer',
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
    mintingTool: 'https://aesthetic.computer',
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
  
  // Build new contract metadata JSON
  const currentMetadata = {
    name: options.name || "KidLisp Keeps",
    version: "2.0.0",
    interfaces: ["TZIP-012", "TZIP-016", "TZIP-021"],
    authors: ["aesthetic.computer"],
    homepage: "https://aesthetic.computer"
  };
  
  if (options.name) {
    console.log(`   📛 Name: ${options.name}`);
  }
  
  if (imageUri) {
    currentMetadata.imageUri = imageUri;
    console.log(`   🖼️  Image URI: ${imageUri}`);
  }
  
  if (description) {
    currentMetadata.description = description;
    console.log(`   📝 Description: ${description.substring(0, 80)}...`);
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
    const contract = await tezos.contract.at(contractAddress);
    
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
// CLI Interface
// ============================================================================

async function main() {
  const rawArgs = process.argv.slice(2);
  
  // Separate flags from positional arguments
  const flags = rawArgs.filter(a => a.startsWith('--'));
  const args = rawArgs.filter(a => !a.startsWith('--'));
  const command = args[0];
  
  // Parse --wallet flag
  const walletFlag = flags.find(f => f.startsWith('--wallet='));
  if (walletFlag) {
    const wallet = walletFlag.split('=')[1];
    if (['kidlisp', 'aesthetic', 'staging'].includes(wallet)) {
      setWallet(wallet);
      console.log(`🔑 Using wallet: ${wallet}\n`);
    } else {
      console.error(`❌ Unknown wallet: ${wallet}. Use: kidlisp, aesthetic, or staging`);
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
        await deployContract(getNetwork(1));
        break;
        
      case 'status':
        await getContractStatus(getNetwork(1));
        break;
        
      case 'balance':
        await getBalance(getNetwork(1));
        break;
        
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
        // Parse --name=<text>, --image=<uri> and --description=<text> flags
        const nameFlag = flags.find(f => f.startsWith('--name='));
        const imageFlag = flags.find(f => f.startsWith('--image='));
        const descFlag = flags.find(f => f.startsWith('--description='));
        
        const name = nameFlag ? nameFlag.split('=').slice(1).join('=') : undefined;
        const imageUri = imageFlag ? imageFlag.split('=').slice(1).join('=') : undefined;
        const description = descFlag ? descFlag.split('=').slice(1).join('=') : undefined;
        
        if (!name && !imageUri && !description) {
          console.error('Usage: node keeps.mjs set-collection-media [--name=<text>] [--image=<ipfs-uri>] [--description=<text>]');
          console.error('');
          console.error('Examples:');
          console.error('  node keeps.mjs set-collection-media --name="KidLisp Keeps (Staging)"');
          console.error('  node keeps.mjs set-collection-media --image=ipfs://Qm...');
          console.error('  node keeps.mjs set-collection-media --image=https://oven.aesthetic.computer/keeps/latest');
          console.error('  node keeps.mjs set-collection-media --description="KidLisp generative art collection"');
          process.exit(1);
        }
        
        await setCollectionMedia({ 
          network: getNetwork(1),
          name,
          imageUri,
          description 
        });
        break;
      }
      
      case 'lock-collection':
        await lockCollectionMetadata({ network: getNetwork(1) });
        break;
      
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

      case 'transfer:admin': {
        if (!args[1] || !args[2] || !args[3]) {
          console.error('Usage: node keeps.mjs transfer:admin <token_id> <from_address> <to_address> [network]');
          console.error('');
          console.error('Examples:');
          console.error('  node keeps.mjs transfer:admin 5 tz1abc... tz1def...');
          console.error('');
          console.error('Note: This is for customer service / emergency use only.');
          console.error('      Requires admin authorization.');
          process.exit(1);
        }
        const tokenId = parseInt(args[1]);
        const fromAddr = args[2];
        const toAddr = args[3];

        if (!fromAddr.startsWith('tz') || !toAddr.startsWith('tz')) {
          console.error('❌ Invalid Tezos address. Must start with tz1, tz2, or tz3.');
          process.exit(1);
        }

        await adminTransfer(tokenId, fromAddr, toAddr, { network: getNetwork(4) });
        break;
      }

      case 'help':
      default:
        console.log(`
╔══════════════════════════════════════════════════════════════╗
║  🔮 Keeps - Tezos FA2 Contract Manager                       ║
╚══════════════════════════════════════════════════════════════╝

Usage: node keeps.mjs <command> [options]

Commands:
  deploy [network]              Deploy contract (default: mainnet)
  status [network]              Show contract status
  balance [network]             Check wallet balance
  upload <piece>                Upload bundle to IPFS
  mint <piece> [network]        Mint a new keep
  update <token_id> <piece>     Update token metadata (re-upload bundle)
  lock <token_id>               Permanently lock token metadata
  burn <token_id>               Burn token (allows re-keeping piece)
  redact <token_id>             Censor token (replace with redacted content)
  set-collection-media          Set collection icon/description
  lock-collection               Permanently lock collection metadata
  fee [network]                 Show current keep fee
  set-fee <tez> [network]       Set keep fee (admin only)
  set-admin <address>           Change contract administrator (admin only)
  withdraw [dest] [network]     Withdraw accumulated fees to address

v4 Commands (Royalties, Pause, Admin Transfer):
  royalty [network]             Show current default royalty percentage
  royalty:set <pct> [network]   Set default royalty (0-25%, admin only)
  pause [network]               Emergency pause (stops minting, admin only)
  unpause [network]             Resume operations (admin only)
  transfer:admin <id> <from> <to>  Emergency token transfer (admin only)

  help                          Show this help

Networks:
  mainnet                       Tezos mainnet (default)
  mainnet                       Tezos mainnet

Flags:
  --thumbnail                   Generate animated WebP thumbnail via Oven
                               and upload to IPFS (requires Oven service)
  --to=<address>                Recipient wallet address (default: server wallet)
  --image=<uri>                 Collection image URI (IPFS or URL)
  --description=<text>          Collection description

Examples:
  node keeps.mjs deploy
  node keeps.mjs balance
  node keeps.mjs mint wand --thumbnail      # With IPFS thumbnail
  node keeps.mjs mint wand --to=tz1abc...   # Mint to specific wallet
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
  node keeps.mjs set-collection-media --image=ipfs://QmXxx --description="KidLisp art"
  node keeps.mjs lock-collection            # Lock collection metadata forever

Environment:
  OVEN_URL                      Oven service URL (default: https://oven.aesthetic.computer)
`);
    }
  } catch (error) {
    console.error(`\n❌ Error: ${error.message}\n`);
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
  getContractStatus,
  getBalance,
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
  detectContentType,
  loadCredentials,
  CONFIG
};
