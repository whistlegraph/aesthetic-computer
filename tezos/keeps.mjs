#!/usr/bin/env node
/**
 * 🔮 Keeps - Tezos FA2 Contract Management with Taquito
 * 
 * A comprehensive Node.js module for deploying, minting, and managing
 * the Aesthetic Computer "Keeps" FA2 contract on Tezos.
 * 
 * Usage:
 *   node keeps.mjs deploy              - Deploy contract to Ghostnet
 *   node keeps.mjs mint <piece>        - Mint a new keep
 *   node keeps.mjs status              - Check contract status
 *   node keeps.mjs balance             - Check wallet balance
 *   node keeps.mjs tokens              - List all minted tokens
 *   node keeps.mjs upload <piece>      - Upload bundle to IPFS
 */

import { TezosToolkit, MichelsonMap } from '@taquito/taquito';
import { InMemorySigner } from '@taquito/signer';
import { Parser } from '@taquito/michel-codec';
import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';
import crypto from 'crypto';

const __dirname = path.dirname(fileURLToPath(import.meta.url));

// ============================================================================
// Configuration
// ============================================================================

const CONFIG = {
  // Network settings
  ghostnet: {
    rpc: 'https://ghostnet.ecadinfra.com',
    name: 'Ghostnet (Testnet)',
    explorer: 'https://ghostnet.tzkt.io'
  },
  mainnet: {
    rpc: 'https://mainnet.ecadinfra.com',
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
    // New SmartPy v2 contract (FA2 library based)
    contract: path.join(__dirname, 'KeepsFA2v2/step_002_cont_0_contract.tz'),
    storage: path.join(__dirname, 'KeepsFA2v2/step_002_cont_0_storage.tz'),
    // Legacy contract path
    legacyContract: path.join(__dirname, 'michelson-lib/keeps-fa2-complete.tz'),
    contractAddress: path.join(__dirname, 'contract-address.txt'),
    vault: path.join(__dirname, '../aesthetic-computer-vault')
  }
};

// ============================================================================
// Credential Loading
// ============================================================================

function loadCredentials() {
  const credentials = {};
  
  // Load Tezos wallet credentials
  const tezosEnvPath = path.join(CONFIG.paths.vault, 'tezos/kidlisp/.env');
  if (fs.existsSync(tezosEnvPath)) {
    const content = fs.readFileSync(tezosEnvPath, 'utf8');
    for (const line of content.split('\n')) {
      if (line.startsWith('KIDLISP_ADDRESS=')) {
        credentials.address = line.split('=')[1].trim().replace(/"/g, '');
      } else if (line.startsWith('KIDLISP_KEY=')) {
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
  
  return credentials;
}

// ============================================================================
// Tezos Client Setup
// ============================================================================

async function createTezosClient(network = 'ghostnet') {
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

async function deployContract(network = 'ghostnet') {
  console.log('\n╔══════════════════════════════════════════════════════════════╗');
  console.log('║  🚀 Deploying Keeps FA2 v2 Contract                          ║');
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
  console.log('   ✓ Contract loaded: KeepsFA2v2 (SmartPy FA2 library)');
  
  // Parse the contract using michel-codec
  const parser = new Parser();
  const parsedContract = parser.parseScript(contractSource);
  
  // Create initial storage
  // Storage structure from SmartPy FA2 library:
  // Storage layout from SmartPy:
  // (pair %administrator address 
  //   (pair %contract_metadata_locked bool
  //     (pair %ledger (big_map nat address)
  //       (pair %metadata (big_map string bytes)
  //         (pair %metadata_locked (big_map nat bool)
  //           (pair %next_token_id nat
  //             (pair %operators (big_map (pair address address nat) unit)
  //               %token_metadata (big_map nat (pair nat (map string bytes))))))))))
  console.log('\n💾 Creating initial storage...');
  
  // Build contract metadata (TZIP-16)
  const contractMetadataJson = JSON.stringify({
    name: "KidLisp Keeps",
    version: "2.0.0",
    interfaces: ["TZIP-012", "TZIP-016", "TZIP-021"],
    authors: ["aesthetic.computer"],
    homepage: "https://aesthetic.computer"
  });
  const contractMetadataBytes = stringToBytes(contractMetadataJson);
  const tezosStoragePointer = stringToBytes("tezos-storage:content");
  
  // Initial storage Michelson - includes contract_metadata_locked = False
  // Format: (Pair admin (Pair contract_metadata_locked (Pair ledger (Pair metadata (Pair metadata_locked (Pair next_token_id (Pair operators token_metadata)))))))
  const initialStorageMichelson = `(Pair "${credentials.address}" (Pair False (Pair {} (Pair {Elt "" 0x${tezosStoragePointer}; Elt "content" 0x${contractMetadataBytes}} (Pair {} (Pair 0 (Pair {} {})))))))`;
  
  const parsedStorage = parser.parseMichelineExpression(initialStorageMichelson);
  
  console.log(`   ✓ Administrator: ${credentials.address}`);
  console.log('   ✓ Initial token ID: 0');
  console.log('   ✓ Contract metadata: TZIP-16 compliant');
  console.log('   ✓ Contract metadata locked: false');
  
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
    
    // Save contract address
    fs.writeFileSync(CONFIG.paths.contractAddress, contractAddress);
    console.log(`   💾 Saved address to: ${CONFIG.paths.contractAddress}\n`);
    
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

async function getContractStatus(network = 'ghostnet') {
  const { tezos, config } = await createTezosClient(network);
  
  // Load contract address
  if (!fs.existsSync(CONFIG.paths.contractAddress)) {
    throw new Error('❌ No contract deployed. Run: node keeps.mjs deploy');
  }
  
  const contractAddress = fs.readFileSync(CONFIG.paths.contractAddress, 'utf8').trim();
  
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
    console.log(`   Tokens Minted: ${storage.next_token_id.toString()}`);
    
    // List all tokens
    if (storage.next_token_id.toNumber() > 0) {
      console.log('\n🎨 Minted Tokens:');
      for (let i = 0; i < storage.next_token_id.toNumber(); i++) {
        const metadata = await storage.token_metadata.get(i);
        if (metadata) {
          const info = metadata.token_info;
          const contentType = info.get('content_type');
          const frozen = info.has('__frozen') ? ' 🔒' : '';
          console.log(`   [${i}] ${contentType ? Buffer.from(contentType.slice(2), 'hex').toString() : 'unknown'}${frozen}`);
        }
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

async function getBalance(network = 'ghostnet') {
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
    width = 512,
    height = 512,
    duration = 8000,    // 8 seconds (was 12)
    fps = 10,           // 10fps capture (was 7.5)
    playbackFps = 20,   // 20fps playback = 2x speed
    quality = 70,       // Lower quality for smaller files (was 90)
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
      quality,
      pinataKey: credentials.pinataKey,
      pinataSecret: credentials.pinataSecret,
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
  
  // Calculate content hash
  const contentHash = crypto.createHash('sha256').update(bundleHtml).digest('hex').slice(0, 16);
  console.log(`🔐 Content Hash: ${contentHash}`);
  
  // Upload to Pinata
  const formData = new FormData();
  const blob = new Blob([bundleHtml], { type: 'text/html' });
  formData.append('file', blob, 'index.html');
  
  formData.append('pinataMetadata', JSON.stringify({
    name: `aesthetic.computer-keep-${pieceName}-${contentHash}`
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
    contentHash,
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
  const { network = 'ghostnet', generateThumbnail: shouldGenerateThumbnail = false } = options;
  
  const { tezos, credentials, config } = await createTezosClient(network);
  const allCredentials = loadCredentials(); // For Pinata access
  
  // Load contract address
  if (!fs.existsSync(CONFIG.paths.contractAddress)) {
    throw new Error('❌ No contract deployed. Run: node keeps.mjs deploy');
  }
  
  const contractAddress = fs.readFileSync(CONFIG.paths.contractAddress, 'utf8').trim();
  const pieceName = piece.replace(/^\$/, '');
  
  console.log('\n╔══════════════════════════════════════════════════════════════╗');
  console.log('║  🎨 Minting New Keep                                         ║');
  console.log('╚══════════════════════════════════════════════════════════════╝\n');
  
  console.log(`📡 Network: ${config.name}`);
  console.log(`📍 Contract: ${contractAddress}`);
  console.log(`📦 Piece: ${pieceName}`);
  console.log(`📸 Thumbnail: ${shouldGenerateThumbnail ? 'Generate via Oven' : 'HTTP fallback'}`);
  
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
    contentType = upload.contentType;
    // Capture bundle metadata for KidLisp pieces
    sourceCode = upload.sourceCode;
    authorHandle = upload.authorHandle;
    userCode = upload.userCode;
    packDate = upload.packDate;
    depCount = upload.depCount || 0;
  }
  
  console.log(`🔗 Artifact URI: ${artifactUri}`);
  console.log(`🔐 Artifact CID: ${artifactCid}`);
  
  // Build TZIP-21 compliant JSON metadata for objkt
  // Token name is just the code (e.g., "$roz")
  const tokenName = `$${pieceName}`;
  const acUrl = `https://aesthetic.computer/$${pieceName}`;
  
  // Build description: source code first, then attribution
  // Build author display for description
  let authorDisplayName = null;
  if (authorHandle && authorHandle !== '@anon') {
    authorDisplayName = authorHandle;
  }
  
  let description = '';
  if (sourceCode) {
    // Start with source code
    description = sourceCode;
    // Add attribution on separate lines
    if (authorDisplayName) {
      description += `\n\nby ${authorDisplayName}`;
    }
    if (userCode) {
      description += `\n${userCode}`;
    }
    if (packDate) {
      description += `\nThis copy was packed on ${packDate}`;
    }
  } else {
    description = `A KidLisp piece preserved on Tezos`;
  }
  
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
    minter: credentials.address,
    creators: [credentials.address],
    rights: '© All rights reserved',
    mintingTool: 'https://aesthetic.computer',
    formats: [{
      uri: artifactUri,
      mimeType: 'text/html',
      dimensions: { value: 'responsive', unit: 'viewport' }
    }],
    tags: tags,
    attributes: [
      { name: 'Content Type', value: 'KidLisp' },
      { name: 'Piece Name', value: `$${pieceName}` },
      { name: 'AC URL', value: acUrl }
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
    creators: stringToBytes(JSON.stringify([credentials.address])),
    rights: stringToBytes('© All rights reserved'),
    content_type: stringToBytes('KidLisp'),
    content_hash: stringToBytes(artifactCid),
    // IMPORTANT: This is the off-chain metadata URI that objkt will fetch
    metadata_uri: stringToBytes(metadataUri),
  };
  
  // Call keep entrypoint
  console.log('\n📤 Calling keep entrypoint...');
  
  try {
    const contract = await tezos.contract.at(contractAddress);
    
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
      owner: credentials.address,
      rights: onChainMetadata.rights,
      shouldPreferSymbol: onChainMetadata.shouldPreferSymbol,
      symbol: onChainMetadata.symbol,
      tags: onChainMetadata.tags,
      thumbnailUri: onChainMetadata.thumbnailUri
    }).send();
    
    console.log(`   ⏳ Operation hash: ${op.hash}`);
    console.log('   ⏳ Waiting for confirmation...');
    
    await op.confirmation(1);
    
    // Get the new token ID from TzKT
    console.log('   ⏳ Fetching token ID...');
    await new Promise(r => setTimeout(r, 3000)); // Wait for indexing
    
    const response = await fetch(`https://api.ghostnet.tzkt.io/v1/contracts/${contractAddress}/bigmaps/token_metadata/keys?limit=100`);
    const tokens = await response.json();
    const tokenId = tokens.length > 0 ? tokens[tokens.length - 1].key : '?';
    
    console.log('\n╔══════════════════════════════════════════════════════════════╗');
    console.log('║  ✅ Token Minted Successfully!                               ║');
    console.log('╚══════════════════════════════════════════════════════════════╝\n');
    
    console.log(`   🎨 Token ID: ${tokenId}`);
    console.log(`   📦 Piece: ${pieceName}`);
    console.log(`   🔗 Artifact: ${artifactUri}`);
    console.log(`   📝 Operation: ${config.explorer}/${op.hash}`);
    console.log(`   🖼️  View on Objkt: https://ghostnet.objkt.com/asset/${contractAddress}/${tokenId}\n`);
    
    return { tokenId, hash: op.hash, artifactUri };
    
  } catch (error) {
    console.error('\n❌ Minting failed!');
    console.error(`   Error: ${error.message}`);
    throw error;
  }
}

// ============================================================================
// Update Metadata
// ============================================================================

async function updateMetadata(tokenId, piece, options = {}) {
  const { network = 'ghostnet' } = options;
  
  const { tezos, credentials, config } = await createTezosClient(network);
  
  // Load contract address
  if (!fs.existsSync(CONFIG.paths.contractAddress)) {
    throw new Error('❌ No contract deployed. Run: node keeps.mjs deploy');
  }
  
  const contractAddress = fs.readFileSync(CONFIG.paths.contractAddress, 'utf8').trim();
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
  
  // Upload new bundle to IPFS
  console.log('\n📤 Uploading new bundle to IPFS...');
  const upload = await uploadToIPFS(piece, { contentType });
  const artifactUri = upload.gatewayUrl;
  const hash = upload.cid;
  
  console.log(`🔗 New Artifact URI: ${artifactUri}`);
  console.log(`🔐 New Content Hash: ${hash}`);
  
  // Build updated TZIP-21 metadata
  const tokenName = `Aesthetic Computer Keep - ${pieceName}`;
  const description = `An aesthetic.computer ${contentType} piece preserved on Tezos`;
  
  const tokenInfo = {
    name: stringToBytes(tokenName),
    description: stringToBytes(description),
    artifactUri: stringToBytes(artifactUri),
    displayUri: stringToBytes(artifactUri),
    thumbnailUri: stringToBytes(`${acUrl}?thumbnail=true`),
    decimals: stringToBytes('0'),
    symbol: stringToBytes('KEEP'),
    isBooleanAmount: stringToBytes('true'),
    shouldPreferSymbol: stringToBytes('false'),
    formats: stringToBytes(JSON.stringify([{
      uri: artifactUri,
      mimeType: 'application/x-directory',
      dimensions: { value: 'responsive', unit: 'viewport' }
    }])),
    tags: stringToBytes(JSON.stringify([contentType, 'aesthetic.computer', 'interactive'])),
    attributes: stringToBytes(JSON.stringify([
      { name: 'content_type', value: contentType },
      { name: 'piece_name', value: pieceName },
      { name: 'ac_url', value: acUrl },
      { name: 'content_hash', value: hash }
    ])),
    creators: stringToBytes(JSON.stringify([credentials.address])),
    rights: stringToBytes('© All rights reserved'),
    content_type: stringToBytes(contentType),
    content_hash: stringToBytes(hash),
    '': stringToBytes(acUrl)  // metadata_uri (empty key)
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
// Set Collection Media (Contract-level Metadata)
// ============================================================================

async function setCollectionMedia(options = {}) {
  const { 
    network = 'ghostnet',
    imageUri,       // Collection icon/logo (IPFS URI or URL)
    description,    // Collection description
    raw = {}        // Raw key-value pairs to set
  } = options;
  
  const { tezos, credentials, config } = await createTezosClient(network);
  
  // Load contract address
  if (!fs.existsSync(CONFIG.paths.contractAddress)) {
    throw new Error('❌ No contract deployed. Run: node keeps.mjs deploy');
  }
  
  const contractAddress = fs.readFileSync(CONFIG.paths.contractAddress, 'utf8').trim();
  
  console.log('\n╔══════════════════════════════════════════════════════════════╗');
  console.log('║  🎨 Setting Collection Media                                 ║');
  console.log('╚══════════════════════════════════════════════════════════════╝\n');
  
  console.log(`📡 Network: ${config.name}`);
  console.log(`📍 Contract: ${contractAddress}\n`);
  
  // Build the metadata updates
  const updates = [];
  
  // Build new contract metadata JSON
  const currentMetadata = {
    name: "KidLisp Keeps",
    version: "2.0.0",
    interfaces: ["TZIP-012", "TZIP-016", "TZIP-021"],
    authors: ["aesthetic.computer"],
    homepage: "https://aesthetic.computer"
  };
  
  if (imageUri) {
    currentMetadata.imageUri = imageUri;
    console.log(`   🖼️  Image URI: ${imageUri}`);
  }
  
  if (description) {
    currentMetadata.description = description;
    console.log(`   📝 Description: ${description.substring(0, 50)}...`);
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
  const { network = 'ghostnet' } = options;
  
  const { tezos, credentials, config } = await createTezosClient(network);
  
  // Load contract address
  if (!fs.existsSync(CONFIG.paths.contractAddress)) {
    throw new Error('❌ No contract deployed. Run: node keeps.mjs deploy');
  }
  
  const contractAddress = fs.readFileSync(CONFIG.paths.contractAddress, 'utf8').trim();
  
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
  const { network = 'ghostnet' } = options;
  
  const { tezos, credentials, config } = await createTezosClient(network);
  
  // Load contract address
  if (!fs.existsSync(CONFIG.paths.contractAddress)) {
    throw new Error('❌ No contract deployed. Run: node keeps.mjs deploy');
  }
  
  const contractAddress = fs.readFileSync(CONFIG.paths.contractAddress, 'utf8').trim();
  
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
// CLI Interface
// ============================================================================

async function main() {
  const rawArgs = process.argv.slice(2);
  
  // Separate flags from positional arguments
  const flags = rawArgs.filter(a => a.startsWith('--'));
  const args = rawArgs.filter(a => !a.startsWith('--'));
  const command = args[0];
  
  // Helper to get network from args (defaults to ghostnet)
  const getNetwork = (argIndex) => {
    const val = args[argIndex];
    if (!val || val.startsWith('--')) return 'ghostnet';
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
        if (!args[1]) {
          console.error('Usage: node keeps.mjs mint <piece> [network] [--thumbnail]');
          process.exit(1);
        }
        await mintToken(args[1], { 
          network: getNetwork(2),
          generateThumbnail: flags.includes('--thumbnail')
        });
        break;
      
      case 'update':
        if (!args[1] || !args[2]) {
          console.error('Usage: node keeps.mjs update <token_id> <piece>');
          process.exit(1);
        }
        await updateMetadata(parseInt(args[1]), args[2], { network: getNetwork(3) });
        break;
      
      case 'lock':
        if (!args[1]) {
          console.error('Usage: node keeps.mjs lock <token_id>');
          process.exit(1);
        }
        await lockMetadata(parseInt(args[1]), { network: getNetwork(2) });
        break;
      
      case 'set-collection-media': {
        // Parse --image=<uri> and --description=<text> flags
        const imageFlag = flags.find(f => f.startsWith('--image='));
        const descFlag = flags.find(f => f.startsWith('--description='));
        
        const imageUri = imageFlag ? imageFlag.split('=').slice(1).join('=') : undefined;
        const description = descFlag ? descFlag.split('=').slice(1).join('=') : undefined;
        
        if (!imageUri && !description) {
          console.error('Usage: node keeps.mjs set-collection-media --image=<ipfs-uri> [--description=<text>]');
          console.error('');
          console.error('Examples:');
          console.error('  node keeps.mjs set-collection-media --image=ipfs://Qm...');
          console.error('  node keeps.mjs set-collection-media --image=https://oven.aesthetic.computer/keeps/latest');
          console.error('  node keeps.mjs set-collection-media --description="KidLisp generative art collection"');
          process.exit(1);
        }
        
        await setCollectionMedia({ 
          network: getNetwork(1),
          imageUri,
          description 
        });
        break;
      }
      
      case 'lock-collection':
        await lockCollectionMetadata({ network: getNetwork(1) });
        break;
        
      case 'help':
      default:
        console.log(`
╔══════════════════════════════════════════════════════════════╗
║  🔮 Keeps - Tezos FA2 Contract Manager                       ║
╚══════════════════════════════════════════════════════════════╝

Usage: node keeps.mjs <command> [options]

Commands:
  deploy [network]              Deploy contract (default: ghostnet)
  status [network]              Show contract status
  balance [network]             Check wallet balance
  upload <piece>                Upload bundle to IPFS
  mint <piece> [network]        Mint a new keep
  update <token_id> <piece>     Update token metadata (re-upload bundle)
  lock <token_id>               Permanently lock token metadata
  set-collection-media          Set collection icon/description
  lock-collection               Permanently lock collection metadata
  help                          Show this help

Networks:
  ghostnet                      Tezos testnet (default)
  mainnet                       Tezos mainnet

Flags:
  --thumbnail                   Generate animated WebP thumbnail via Oven
                               and upload to IPFS (requires Oven service)
  --image=<uri>                 Collection image URI (IPFS or URL)
  --description=<text>          Collection description

Examples:
  node keeps.mjs deploy
  node keeps.mjs balance
  node keeps.mjs mint wand --thumbnail      # With IPFS thumbnail
  node keeps.mjs update 0 wand              # Re-upload bundle & update metadata
  node keeps.mjs lock 0                     # Permanently lock token 0
  
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
  setCollectionMedia,
  lockCollectionMetadata,
  detectContentType,
  loadCredentials,
  CONFIG
};
