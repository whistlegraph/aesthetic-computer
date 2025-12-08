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
  // (pair %administrator address 
  //   (pair %ledger (big_map nat address)
  //     (pair %metadata (big_map string bytes)
  //       (pair %metadata_locked (big_map nat bool)
  //         (pair %next_token_id nat
  //           (pair %operators (big_map (pair address address nat) unit)
  //             %token_metadata (big_map nat (pair nat (map string bytes)))))))))
  console.log('\n💾 Creating initial storage...');
  
  // Build contract metadata (TZIP-16)
  const contractMetadataJson = JSON.stringify({
    name: "Aesthetic.computer Keeps",
    description: "FA2 NFT contract for aesthetic.computer keeps",
    version: "2.0.0",
    interfaces: ["TZIP-012", "TZIP-016", "TZIP-021"],
    authors: ["aesthetic.computer"],
    homepage: "https://aesthetic.computer"
  });
  const contractMetadataBytes = stringToBytes(contractMetadataJson);
  const tezosStoragePointer = stringToBytes("tezos-storage:content");
  
  // Initial storage Michelson
  // Format: (Pair admin (Pair ledger (Pair metadata (Pair metadata_locked (Pair next_token_id (Pair operators token_metadata))))))
  const initialStorageMichelson = `(Pair "${credentials.address}" (Pair {} (Pair {Elt "" 0x${tezosStoragePointer}; Elt "content" 0x${contractMetadataBytes}} (Pair {} (Pair 0 (Pair {} {}))))))`;
  
  const parsedStorage = parser.parseMichelineExpression(initialStorageMichelson);
  
  console.log(`   ✓ Administrator: ${credentials.address}`);
  console.log('   ✓ Initial token ID: 0');
  console.log('   ✓ Contract metadata: TZIP-16 compliant');
  
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
  
  // Build the correct endpoint URL based on content type
  let url;
  if (contentType === 'kidlisp') {
    url = `https://aesthetic.computer/api/bundle-html?code=${pieceName}&format=json`;
  } else {
    url = `https://aesthetic.computer/api/bundle-html?piece=${pieceName}&format=json`;
  }
  
  console.log(`   URL: ${url}`);
  
  const response = await fetch(url);
  
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
  
  return {
    html,
    filename: data.filename,
    sizeKB: data.sizeKB
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
  if (options.bundleHtml) {
    bundleHtml = options.bundleHtml;
    console.log('   Using provided bundle HTML');
  } else {
    const bundle = await fetchBundleFromNetlify(piece, contentType);
    bundleHtml = bundle.html;
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
    ipfsUri: `ipfs://${cid}`
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
  const { network = 'ghostnet' } = options;
  
  const { tezos, credentials, config } = await createTezosClient(network);
  const allCredentials = loadCredentials(); // For Pinata access
  
  // Load contract address
  if (!fs.existsSync(CONFIG.paths.contractAddress)) {
    throw new Error('❌ No contract deployed. Run: node keeps.mjs deploy');
  }
  
  const contractAddress = fs.readFileSync(CONFIG.paths.contractAddress, 'utf8').trim();
  const pieceName = piece.replace(/^\$/, '');
  const acUrl = `https://aesthetic.computer/${pieceName}`;
  
  console.log('\n╔══════════════════════════════════════════════════════════════╗');
  console.log('║  🎨 Minting New Keep                                         ║');
  console.log('╚══════════════════════════════════════════════════════════════╝\n');
  
  console.log(`📡 Network: ${config.name}`);
  console.log(`📍 Contract: ${contractAddress}`);
  console.log(`📦 Piece: ${pieceName}`);
  
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
  
  if (!artifactUri) {
    console.log('\n📤 Uploading HTML bundle to IPFS...');
    const upload = await uploadToIPFS(piece, { contentType });
    artifactUri = `ipfs://${upload.cid}`;  // Use ipfs:// URI for artifact
    artifactCid = upload.cid;
    contentType = upload.contentType;
  }
  
  console.log(`🔗 Artifact URI: ${artifactUri}`);
  console.log(`🔐 Artifact CID: ${artifactCid}`);
  
  // Build TZIP-21 compliant JSON metadata for objkt
  const tokenName = `Aesthetic Computer Keep - ${pieceName}`;
  const description = `An aesthetic.computer ${contentType} piece preserved on Tezos`;
  
  const metadataJson = {
    name: tokenName,
    description: description,
    artifactUri: artifactUri,
    displayUri: artifactUri,
    thumbnailUri: `${acUrl}?thumbnail=true`,
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
    tags: [contentType, 'aesthetic.computer', 'interactive'],
    attributes: [
      { name: 'Content Type', value: contentType },
      { name: 'Piece Name', value: pieceName },
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
    thumbnailUri: stringToBytes(`${acUrl}?thumbnail=true`),
    decimals: stringToBytes('0'),
    symbol: stringToBytes('KEEP'),
    isBooleanAmount: stringToBytes('true'),
    shouldPreferSymbol: stringToBytes('false'),
    formats: stringToBytes(JSON.stringify(metadataJson.formats)),
    tags: stringToBytes(JSON.stringify(metadataJson.tags)),
    attributes: stringToBytes(JSON.stringify(metadataJson.attributes)),
    creators: stringToBytes(JSON.stringify([credentials.address])),
    rights: stringToBytes('© All rights reserved'),
    content_type: stringToBytes(contentType),
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
  const acUrl = `https://aesthetic.computer/${pieceName}`;
  
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
  const args = process.argv.slice(2);
  const command = args[0];
  
  try {
    switch (command) {
      case 'deploy':
        await deployContract(args[1] || 'ghostnet');
        break;
        
      case 'status':
        await getContractStatus(args[1] || 'ghostnet');
        break;
        
      case 'balance':
        await getBalance(args[1] || 'ghostnet');
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
          console.error('Usage: node keeps.mjs mint <piece>');
          process.exit(1);
        }
        await mintToken(args[1], { network: args[2] || 'ghostnet' });
        break;
      
      case 'update':
        if (!args[1] || !args[2]) {
          console.error('Usage: node keeps.mjs update <token_id> <piece>');
          process.exit(1);
        }
        await updateMetadata(parseInt(args[1]), args[2], { network: args[3] || 'ghostnet' });
        break;
      
      case 'lock':
        if (!args[1]) {
          console.error('Usage: node keeps.mjs lock <token_id>');
          process.exit(1);
        }
        await lockMetadata(parseInt(args[1]), { network: args[2] || 'ghostnet' });
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
  help                          Show this help

Networks:
  ghostnet                      Tezos testnet (default)
  mainnet                       Tezos mainnet

Examples:
  node keeps.mjs deploy
  node keeps.mjs balance
  node keeps.mjs upload wand
  node keeps.mjs mint wand
  node keeps.mjs update 0 wand      # Re-upload bundle & update metadata
  node keeps.mjs lock 0             # Permanently lock token 0
  node keeps.mjs status
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
  detectContentType,
  loadCredentials,
  CONFIG
};
