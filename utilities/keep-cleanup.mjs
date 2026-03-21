#!/usr/bin/env node
/**
 * Keep IPFS Cleanup Utility
 * 
 * Identifies and removes orphaned IPFS pins from previous rebakes/syncs.
 * 
 * Usage:
 *   node utilities/keep-cleanup.mjs list              # List all keep-related pins
 *   node utilities/keep-cleanup.mjs analyze           # Show orphaned pins
 *   node utilities/keep-cleanup.mjs analyze $piece    # Analyze specific piece
 *   node utilities/keep-cleanup.mjs clean --dry-run   # Preview cleanup
 *   node utilities/keep-cleanup.mjs clean             # Remove orphaned pins
 *   node utilities/keep-cleanup.mjs clean $piece      # Clean specific piece
 * 
 * How it works:
 *   1. Fetches all pinned IPFS hashes from Pinata
 *   2. Fetches on-chain URIs from TzKT (what's actively used)
 *   3. Fetches database records (ipfsMedia, pendingRebake)
 *   4. Identifies pins that are NOT referenced anywhere (orphans)
 *   5. Optionally unpins orphaned artifacts
 * 
 * Environment variables:
 *   MONGODB_CONNECTION_STRING - MongoDB connection (or source system/.env)
 */

import { MongoClient } from "mongodb";

const MONGODB_URI = process.env.MONGODB_CONNECTION_STRING || process.env.MONGODB_URI || process.env.DATABASE_URL;
const MONGODB_NAME = process.env.MONGODB_NAME || "aesthetic";

// Contract configuration - v5 RC mainnet by default
const CONTRACT_ADDRESS = process.env.TEZOS_KEEPS_CONTRACT || "KT1QdGZP8jzqaxXDia3U7DYEqFYhfqGRHido";
const NETWORK = process.env.TEZOS_NETWORK || "mainnet";
const TZKT_API = NETWORK === "mainnet" ? "https://api.tzkt.io/v1" : `https://api.${NETWORK}.tzkt.io/v1`;

// ============================================================================
// Pinata API
// ============================================================================

async function getPinataCredentials() {
  if (!MONGODB_URI) {
    throw new Error("MONGODB_CONNECTION_STRING not set. Run: source system/.env");
  }
  
  const client = new MongoClient(MONGODB_URI);
  await client.connect();
  const db = client.db(MONGODB_NAME);
  const secrets = await db.collection("secrets").findOne({ _id: "pinata" });
  await client.close();
  
  if (!secrets?.apiKey || !secrets?.apiSecret) {
    throw new Error("Pinata credentials not found in database");
  }
  
  return { apiKey: secrets.apiKey, apiSecret: secrets.apiSecret };
}

async function listAllPins(nameFilter = null) {
  const { apiKey, apiSecret } = await getPinataCredentials();
  const allPins = [];
  let pageOffset = 0;
  const pageLimit = 1000;
  
  while (true) {
    let url = `https://api.pinata.cloud/data/pinList?status=pinned&pageLimit=${pageLimit}&pageOffset=${pageOffset}`;
    if (nameFilter) {
      url += `&metadata[name]=${encodeURIComponent(nameFilter)}`;
    }
    
    const response = await fetch(url, {
      headers: {
        pinata_api_key: apiKey,
        pinata_secret_api_key: apiSecret,
      },
    });
    
    if (!response.ok) {
      throw new Error(`Pinata API error: ${response.status}`);
    }
    
    const data = await response.json();
    allPins.push(...data.rows);
    
    if (data.rows.length < pageLimit) break;
    pageOffset += pageLimit;
  }
  
  return allPins;
}

async function unpinHash(hash) {
  const { apiKey, apiSecret } = await getPinataCredentials();
  
  const response = await fetch(`https://api.pinata.cloud/pinning/unpin/${hash}`, {
    method: "DELETE",
    headers: {
      pinata_api_key: apiKey,
      pinata_secret_api_key: apiSecret,
    },
  });
  
  if (!response.ok) {
    throw new Error(`Failed to unpin ${hash}: ${response.status}`);
  }
  
  return true;
}

// ============================================================================
// TzKT API - Fetch on-chain data
// ============================================================================

async function fetchOnChainTokens() {
  const tokens = [];
  let offset = 0;
  const limit = 100;
  
  while (true) {
    const url = `${TZKT_API}/tokens?contract=${CONTRACT_ADDRESS}&limit=${limit}&offset=${offset}`;
    const response = await fetch(url);
    
    if (!response.ok) {
      throw new Error(`TzKT API error: ${response.status}`);
    }
    
    const batch = await response.json();
    tokens.push(...batch);
    
    if (batch.length < limit) break;
    offset += limit;
  }
  
  return tokens;
}

// Extract IPFS hashes from token metadata
function extractIpfsHashes(token) {
  const hashes = new Set();
  const metadata = token.metadata || {};
  
  // Extract from various metadata fields
  const uriFields = ['artifactUri', 'displayUri', 'thumbnailUri'];
  for (const field of uriFields) {
    const uri = metadata[field];
    if (uri && uri.startsWith('ipfs://')) {
      hashes.add(uri.replace('ipfs://', ''));
    }
  }
  
  // Check formats array
  if (metadata.formats) {
    for (const fmt of metadata.formats) {
      if (fmt.uri && fmt.uri.startsWith('ipfs://')) {
        hashes.add(fmt.uri.replace('ipfs://', ''));
      }
    }
  }
  
  return hashes;
}

// ============================================================================
// Database - Fetch stored media info
// ============================================================================

async function fetchDatabaseRecords(pieceName = null) {
  const client = new MongoClient(MONGODB_URI);
  await client.connect();
  const db = client.db(MONGODB_NAME);
  
  const query = pieceName ? { code: pieceName.replace(/^\$/, '') } : {};
  const pieces = await db.collection("kidlisp").find(query).toArray();
  
  await client.close();
  return pieces;
}

// Extract IPFS hashes from database record
function extractDbHashes(piece) {
  const hashes = new Set();
  
  // Current ipfsMedia
  if (piece.ipfsMedia) {
    if (piece.ipfsMedia.artifactUri?.startsWith('ipfs://')) {
      hashes.add(piece.ipfsMedia.artifactUri.replace('ipfs://', ''));
    }
    if (piece.ipfsMedia.thumbnailUri?.startsWith('ipfs://')) {
      hashes.add(piece.ipfsMedia.thumbnailUri.replace('ipfs://', ''));
    }
  }
  
  // Pending rebake
  if (piece.pendingRebake) {
    if (piece.pendingRebake.artifactUri?.startsWith('ipfs://')) {
      hashes.add(piece.pendingRebake.artifactUri.replace('ipfs://', ''));
    }
    if (piece.pendingRebake.thumbnailUri?.startsWith('ipfs://')) {
      hashes.add(piece.pendingRebake.thumbnailUri.replace('ipfs://', ''));
    }
  }
  
  // Contract-specific data
  if (piece.tezos?.contracts) {
    for (const contractData of Object.values(piece.tezos.contracts)) {
      for (const field of ['artifactUri', 'thumbnailUri', 'metadataUri', 'pendingArtifactUri', 'pendingThumbnailUri', 'pendingMetadataUri']) {
        const uri = contractData[field];
        if (uri?.startsWith('ipfs://')) {
          hashes.add(uri.replace('ipfs://', ''));
        }
      }
    }
  }
  
  // Legacy tezos fields
  if (piece.tezos) {
    for (const field of ['artifactUri', 'thumbnailUri', 'metadataUri']) {
      const uri = piece.tezos[field];
      if (uri?.startsWith('ipfs://')) {
        hashes.add(uri.replace('ipfs://', ''));
      }
    }
  }
  
  // Media history (future feature)
  if (piece.mediaHistory) {
    for (const entry of piece.mediaHistory) {
      if (entry.artifactUri?.startsWith('ipfs://')) {
        hashes.add(entry.artifactUri.replace('ipfs://', ''));
      }
      if (entry.thumbnailUri?.startsWith('ipfs://')) {
        hashes.add(entry.thumbnailUri.replace('ipfs://', ''));
      }
    }
  }
  
  return hashes;
}

// ============================================================================
// Analysis
// ============================================================================

// Naming patterns for keep-related pins
const KEEP_PATTERNS = [
  /-artifact\.html$/i,
  /-thumbnail\.webp$/i,
  /-metadata.*\.json$/i,
  /^\$[a-zA-Z0-9_-]+/,  // $piecename prefix
];

function isKeepRelatedPin(pin) {
  const name = pin.metadata?.name || '';
  return KEEP_PATTERNS.some(pattern => pattern.test(name));
}

function formatBytes(bytes) {
  if (!bytes) return "0 B";
  const k = 1024;
  const sizes = ["B", "KB", "MB", "GB"];
  const i = Math.floor(Math.log(bytes) / Math.log(k));
  return (bytes / Math.pow(k, i)).toFixed(1) + " " + sizes[i];
}

function formatDate(dateStr) {
  return new Date(dateStr).toLocaleDateString('en-US', { 
    month: 'short', 
    day: 'numeric',
    year: 'numeric'
  });
}

// ============================================================================
// Commands
// ============================================================================

async function cmdList(args) {
  console.log("📌 Fetching all pins from Pinata...");
  const allPins = await listAllPins();
  
  // Filter to keep-related pins
  const keepPins = allPins.filter(isKeepRelatedPin);
  
  console.log(`\n📊 Found ${allPins.length} total pins, ${keepPins.length} keep-related\n`);
  
  // Group by piece name
  const byPiece = new Map();
  for (const pin of keepPins) {
    const name = pin.metadata?.name || '(unnamed)';
    // Extract piece name from various formats
    const match = name.match(/^\$?([a-zA-Z0-9_-]+)/);
    const pieceName = match ? match[1] : 'unknown';
    
    if (!byPiece.has(pieceName)) byPiece.set(pieceName, []);
    byPiece.get(pieceName).push(pin);
  }
  
  for (const [pieceName, pins] of byPiece) {
    console.log(`\n$${pieceName} (${pins.length} pins):`);
    for (const pin of pins) {
      const hash = pin.ipfs_pin_hash.slice(0, 16) + '...';
      const size = formatBytes(pin.size);
      const date = formatDate(pin.date_pinned);
      const name = pin.metadata?.name || '(unnamed)';
      console.log(`  ${hash}  ${size.padStart(10)}  ${date}  ${name}`);
    }
  }
}

async function cmdAnalyze(pieceName = null) {
  console.log("🔍 Analyzing keep IPFS artifacts...\n");
  
  // Step 1: Fetch all data sources
  console.log("📌 Fetching Pinata pins...");
  const allPins = await listAllPins();
  const keepPins = allPins.filter(isKeepRelatedPin);
  console.log(`   Found ${keepPins.length} keep-related pins`);
  
  console.log("⛓️  Fetching on-chain tokens from TzKT...");
  const tokens = await fetchOnChainTokens();
  console.log(`   Found ${tokens.length} tokens on ${NETWORK}`);
  
  console.log("🗄️  Fetching database records...");
  const dbRecords = await fetchDatabaseRecords(pieceName);
  console.log(`   Found ${dbRecords.length} piece records\n`);
  
  // Step 2: Build set of "active" hashes (referenced on-chain or in DB)
  const activeHashes = new Set();
  
  // From on-chain
  for (const token of tokens) {
    for (const hash of extractIpfsHashes(token)) {
      activeHashes.add(hash);
    }
  }
  console.log(`   ${activeHashes.size} hashes referenced on-chain`);
  
  // From database
  for (const record of dbRecords) {
    for (const hash of extractDbHashes(record)) {
      activeHashes.add(hash);
    }
  }
  console.log(`   ${activeHashes.size} total active hashes (on-chain + DB)\n`);
  
  // Step 3: Identify orphaned pins
  const orphaned = [];
  const active = [];
  
  for (const pin of keepPins) {
    const hash = pin.ipfs_pin_hash;
    if (activeHashes.has(hash)) {
      active.push(pin);
    } else {
      orphaned.push(pin);
    }
  }
  
  // Calculate sizes
  const orphanedSize = orphaned.reduce((sum, p) => sum + (p.size || 0), 0);
  const activeSize = active.reduce((sum, p) => sum + (p.size || 0), 0);
  
  console.log("═".repeat(60));
  console.log(`📊 Analysis Results:`);
  console.log(`   ✅ Active:   ${active.length} pins (${formatBytes(activeSize)})`);
  console.log(`   🗑️  Orphaned: ${orphaned.length} pins (${formatBytes(orphanedSize)})`);
  console.log("═".repeat(60));
  
  if (orphaned.length > 0) {
    console.log("\n🗑️  Orphaned pins (not referenced anywhere):");
    for (const pin of orphaned) {
      const hash = pin.ipfs_pin_hash;
      const size = formatBytes(pin.size);
      const date = formatDate(pin.date_pinned);
      const name = pin.metadata?.name || '(unnamed)';
      console.log(`   ${hash.slice(0, 20)}...  ${size.padStart(10)}  ${date}  ${name}`);
    }
    console.log(`\n💡 Run 'node utilities/keep-cleanup.mjs clean --dry-run' to preview removal`);
  } else {
    console.log("\n✨ No orphaned pins found! Everything is clean.");
  }
  
  return { orphaned, active, orphanedSize, activeSize };
}

async function cmdClean(pieceName = null, dryRun = false) {
  const { orphaned, orphanedSize } = await cmdAnalyze(pieceName);
  
  if (orphaned.length === 0) {
    return;
  }
  
  console.log("");
  
  if (dryRun) {
    console.log("⚠️  DRY RUN - No pins will be removed.");
    console.log(`   Would remove ${orphaned.length} pins (${formatBytes(orphanedSize)})`);
    console.log("\n   Run without --dry-run to actually delete.");
    return;
  }
  
  // Confirm before deleting
  console.log(`\n🗑️  Removing ${orphaned.length} orphaned pins (${formatBytes(orphanedSize)})...\n`);
  
  let removed = 0;
  let failed = 0;
  
  for (const pin of orphaned) {
    const hash = pin.ipfs_pin_hash;
    const name = pin.metadata?.name || '';
    process.stdout.write(`   Unpinning ${hash.slice(0, 16)}... `);
    
    try {
      await unpinHash(hash);
      console.log("✅");
      removed++;
    } catch (e) {
      console.log(`❌ ${e.message}`);
      failed++;
    }
  }
  
  console.log(`\n✅ Cleanup complete!`);
  console.log(`   Removed: ${removed} pins (${formatBytes(orphanedSize)})`);
  if (failed > 0) {
    console.log(`   Failed:  ${failed} pins`);
  }
}

// ============================================================================
// Main
// ============================================================================

async function main() {
  const args = process.argv.slice(2);
  const command = args[0] || 'help';
  
  // Parse flags
  const dryRun = args.includes('--dry-run');
  const pieceName = args.find(a => a.startsWith('$'));
  
  try {
    switch (command) {
      case 'list':
        await cmdList(args);
        break;
        
      case 'analyze':
        await cmdAnalyze(pieceName);
        break;
        
      case 'clean':
        await cmdClean(pieceName, dryRun);
        break;
        
      case 'help':
      default:
        console.log(`
Keep IPFS Cleanup Utility

Identifies and removes orphaned IPFS artifacts from previous rebakes/syncs.

Usage:
  node utilities/keep-cleanup.mjs <command> [options]

Commands:
  list                    List all keep-related pins in Pinata
  analyze                 Analyze pins and show orphaned ones
  analyze $piece          Analyze specific piece
  clean [--dry-run]       Remove orphaned pins
  clean $piece            Clean specific piece only

Examples:
  node utilities/keep-cleanup.mjs list
  node utilities/keep-cleanup.mjs analyze
  node utilities/keep-cleanup.mjs analyze $flower
  node utilities/keep-cleanup.mjs clean --dry-run
  node utilities/keep-cleanup.mjs clean

Note: Requires MONGODB_CONNECTION_STRING env var.
      Run: source system/.env && node utilities/keep-cleanup.mjs
`);
    }
  } catch (error) {
    console.error(`\n❌ Error: ${error.message}`);
    process.exit(1);
  }
}

main();
