#!/usr/bin/env node
/**
 * Pinata Pin Management Utility
 * 
 * Usage:
 *   node utilities/pinata-manage.mjs list              # List all pins
 *   node utilities/pinata-manage.mjs list --filter=kidlisp  # Filter by name
 *   node utilities/pinata-manage.mjs stats             # Show storage stats
 *   node utilities/pinata-manage.mjs unpin <hash>      # Unpin a specific hash
 *   node utilities/pinata-manage.mjs cleanup --dry-run # Preview cleanup
 *   node utilities/pinata-manage.mjs cleanup           # Remove old test pins
 * 
 * Environment variables needed:
 *   MONGODB_CONNECTION_STRING - MongoDB connection string
 *   MONGODB_NAME - Database name (default: aesthetic)
 */

import { MongoClient } from "mongodb";

const MONGODB_URI = process.env.MONGODB_CONNECTION_STRING || process.env.MONGODB_URI || process.env.DATABASE_URL;
const MONGODB_NAME = process.env.MONGODB_NAME || "aesthetic";

async function getPinataCredentials() {
  if (!MONGODB_URI) {
    throw new Error("MONGODB_CONNECTION_STRING not set. Set env vars from system/.env first.");
  }
  
  const client = new MongoClient(MONGODB_URI);
  await client.connect();
  const db = client.db(MONGODB_NAME);
  const secrets = await db.collection("secrets").findOne({ _id: "pinata" });
  await client.close();
  
  if (!secrets?.apiKey || !secrets?.apiSecret) {
    throw new Error("Pinata credentials not found in database (secrets collection, _id: 'pinata')");
  }
  
  return { apiKey: secrets.apiKey, apiSecret: secrets.apiSecret };
}

async function listPins(filter = null, limit = 100) {
  const { apiKey, apiSecret } = await getPinataCredentials();
  
  let url = `https://api.pinata.cloud/data/pinList?status=pinned&pageLimit=${limit}`;
  if (filter) {
    url += `&metadata[name]=${encodeURIComponent(filter)}`;
  }
  
  const response = await fetch(url, {
    headers: {
      pinata_api_key: apiKey,
      pinata_secret_api_key: apiSecret,
    },
  });
  
  if (!response.ok) {
    throw new Error(`Failed to list pins: ${response.status} ${await response.text()}`);
  }
  
  return response.json();
}

async function getStats() {
  const { apiKey, apiSecret } = await getPinataCredentials();
  
  const response = await fetch("https://api.pinata.cloud/data/userPinnedDataTotal", {
    headers: {
      pinata_api_key: apiKey,
      pinata_secret_api_key: apiSecret,
    },
  });
  
  if (!response.ok) {
    throw new Error(`Failed to get stats: ${response.status}`);
  }
  
  return response.json();
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

function formatBytes(bytes) {
  if (bytes === 0) return "0 B";
  const k = 1024;
  const sizes = ["B", "KB", "MB", "GB"];
  const i = Math.floor(Math.log(bytes) / Math.log(k));
  return `${(bytes / Math.pow(k, i)).toFixed(2)} ${sizes[i]}`;
}

function formatDate(isoString) {
  return new Date(isoString).toLocaleDateString("en-US", {
    year: "numeric",
    month: "short",
    day: "numeric",
    hour: "2-digit",
    minute: "2-digit",
  });
}

async function main() {
  const args = process.argv.slice(2);
  const command = args[0] || "help";
  
  try {
    switch (command) {
      case "list": {
        const filterArg = args.find(a => a.startsWith("--filter="));
        const filter = filterArg ? filterArg.split("=")[1] : null;
        
        console.log(`\n📌 Listing pinned files${filter ? ` (filter: ${filter})` : ""}...\n`);
        
        const data = await listPins(filter);
        
        if (data.rows.length === 0) {
          console.log("No pins found.");
          return;
        }
        
        let totalSize = 0;
        for (const pin of data.rows) {
          const name = pin.metadata?.name || "(unnamed)";
          const size = pin.size || 0;
          totalSize += size;
          const date = formatDate(pin.date_pinned);
          console.log(`${pin.ipfs_pin_hash.slice(0, 12)}...  ${formatBytes(size).padStart(10)}  ${date.padStart(20)}  ${name}`);
        }
        
        console.log(`\n📊 Total: ${data.rows.length} pins, ${formatBytes(totalSize)}`);
        if (data.count > data.rows.length) {
          console.log(`   (showing ${data.rows.length} of ${data.count} total)`);
        }
        break;
      }
      
      case "stats": {
        console.log("\n📊 Pinata Storage Stats...\n");
        const stats = await getStats();
        console.log(`Total Pinned: ${formatBytes(stats.pin_size_total)}`);
        console.log(`Pin Count: ${stats.pin_count}`);
        break;
      }
      
      case "unpin": {
        const hash = args[1];
        if (!hash) {
          console.error("Usage: pinata-manage.mjs unpin <ipfs-hash>");
          process.exit(1);
        }
        
        console.log(`\n🗑️  Unpinning ${hash}...`);
        await unpinHash(hash);
        console.log("✅ Unpinned successfully");
        break;
      }
      
      case "cleanup": {
        const dryRun = args.includes("--dry-run");
        const keepDays = parseInt(args.find(a => a.startsWith("--keep-days="))?.split("=")[1] || "7");
        
        console.log(`\n🧹 Cleanup Mode ${dryRun ? "(DRY RUN)" : ""}`);
        console.log(`   Keeping pins from last ${keepDays} days\n`);
        
        // Get all pins
        const data = await listPins(null, 1000);
        
        const cutoffDate = new Date();
        cutoffDate.setDate(cutoffDate.getDate() - keepDays);
        
        // PROTECTED FILES - never delete these
        const protectedPatterns = [
          "radical-digital-painting",  // 8.42 GB important file
          "feral-file",                 // Any feral file related pins
        ];
        
        // Identify pins to remove (old test files, not production keeps)
        const toRemove = [];
        const toKeep = [];
        const protectedFiles = [];
        
        for (const pin of data.rows) {
          const pinDate = new Date(pin.date_pinned);
          const name = pin.metadata?.name || "";
          
          // Check if protected
          const isProtected = protectedPatterns.some(pattern => 
            name.toLowerCase().includes(pattern.toLowerCase())
          );
          
          if (isProtected) {
            protectedFiles.push(pin);
            continue;
          }
          
          // Keep criteria:
          // 1. Recent pins (within keepDays)
          // 2. Files with "-metadata.json" suffix (final NFT metadata)
          const isRecent = pinDate > cutoffDate;
          const isFinalMetadata = name.endsWith("-metadata.json");
          
          if (isRecent || isFinalMetadata) {
            toKeep.push(pin);
          } else {
            toRemove.push(pin);
          }
        }
        
        console.log(`📊 Analysis:`);
        console.log(`   🛡️  Protected: ${protectedFiles.length} pins (never removed)`);
        for (const pin of protectedFiles) {
          console.log(`      - ${pin.metadata?.name || pin.ipfs_pin_hash} (${formatBytes(pin.size)})`);
        }
        console.log(`   ✅ Keep: ${toKeep.length} pins (recent or final metadata)`);
        console.log(`   🗑️  Remove: ${toRemove.length} pins\n`);
        
        if (toRemove.length === 0) {
          console.log("Nothing to clean up!");
          return;
        }
        
        console.log("Files to remove:");
        let removeSize = 0;
        for (const pin of toRemove) {
          removeSize += pin.size || 0;
          const name = pin.metadata?.name || "(unnamed)";
          console.log(`   ${pin.ipfs_pin_hash.slice(0, 12)}... ${formatBytes(pin.size).padStart(10)}  ${name}`);
        }
        console.log(`\n   Total to remove: ${formatBytes(removeSize)}`);
        
        if (dryRun) {
          console.log("\n⚠️  DRY RUN - no files removed. Run without --dry-run to delete.");
        } else {
          console.log("\n🗑️  Removing...");
          let removed = 0;
          for (const pin of toRemove) {
            process.stdout.write(`   Unpinning ${pin.ipfs_pin_hash.slice(0, 12)}...`);
            try {
              await unpinHash(pin.ipfs_pin_hash);
              console.log(" ✅");
              removed++;
            } catch (e) {
              console.log(` ❌ ${e.message}`);
            }
          }
          console.log(`\n✅ Cleanup complete! Removed ${removed} pins, freed ${formatBytes(removeSize)}`);
        }
        break;
      }
      
      case "help":
      default:
        console.log(`
Pinata Pin Management Utility

Usage:
  node utilities/pinata-manage.mjs <command> [options]

Commands:
  list [--filter=<name>]    List all pinned files
  stats                     Show storage statistics
  unpin <hash>              Unpin a specific IPFS hash
  cleanup [--dry-run]       Remove old test pins
          [--keep-days=N]   Keep pins from last N days (default: 7)

Examples:
  node utilities/pinata-manage.mjs list
  node utilities/pinata-manage.mjs list --filter=kidlisp
  node utilities/pinata-manage.mjs stats
  node utilities/pinata-manage.mjs cleanup --dry-run
  node utilities/pinata-manage.mjs cleanup --keep-days=30
  node utilities/pinata-manage.mjs unpin QmXxxx...

Note: Requires MONGODB_URI or DATABASE_URL environment variable.
      Run: source system/.env && node utilities/pinata-manage.mjs
        `);
    }
  } catch (error) {
    console.error(`\n❌ Error: ${error.message}`);
    process.exit(1);
  }
}

main();
