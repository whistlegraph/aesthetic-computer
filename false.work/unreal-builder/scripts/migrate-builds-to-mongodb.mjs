#!/usr/bin/env node

// One-time migration script to seed MongoDB with existing builds from HTML
// Run with: node migrate-builds-to-mongodb.mjs

import { MongoClient } from "mongodb";

const MONGODB_CONNECTION_STRING = process.env.MONGODB_CONNECTION_STRING;
const MONGODB_NAME = process.env.MONGODB_NAME || "aesthetic";

if (!MONGODB_CONNECTION_STRING) {
  console.error("❌ MONGODB_CONNECTION_STRING environment variable is required");
  console.error("   Set it in your environment or run with:");
  console.error("   MONGODB_CONNECTION_STRING='mongodb+srv://...' node migrate-builds-to-mongodb.mjs");
  process.exit(1);
}

// Existing builds extracted from index.html
const existingBuilds = [
  {
    platform: "ios",
    version: "2025.11.26-1019",
    timestamp: new Date("2025-11-26T10:19:00-08:00"),
    sizeMB: 879,
    level: "L_VerticalSlice_Demo",
    ueVersion: "UE_5.6",
    downloadUrl: "https://assets-aesthetic-computer.sfo3.cdn.digitaloceanspaces.com/false.work/ios/spiderlily-ios-device-2025.11.26-1019.ipa",
    changelist: "CL#689",
    buildType: "device"
  },
  {
    platform: "windows",
    version: "2025.11.21-1654",
    timestamp: new Date("2025-11-21T16:58:04-08:00"),
    sizeMB: 1342,
    level: "L_VerticalSlice_Demo",
    ueVersion: "UE_5.6",
    downloadUrl: "https://assets.aesthetic.computer/false.work/spiderlily-windows-2025.11.21-1654.zip",
    logUrl: "https://assets.aesthetic.computer/false.work/spiderlily-windows-2025.11.21-1654.txt"
  },
  {
    platform: "mac",
    version: "2025.11.21-2133",
    timestamp: new Date("2025-11-21T21:36:58"),
    sizeMB: 3458,
    level: "L_VerticalSlice_Demo",
    ueVersion: "UE_5.6",
    downloadUrl: "https://assets.aesthetic.computer/false.work/spiderlily-mac-2025.11.21-2133.zip",
    logUrl: "https://assets.aesthetic.computer/false.work/spiderlily-mac-2025.11.21-2133.txt"
  },
  {
    platform: "windows",
    version: "2025.11.13-1609",
    timestamp: new Date("2025-11-13T16:15:03"),
    sizeMB: 1329,
    level: "L_VerticalSlice_Demo",
    ueVersion: "UE_5.6",
    downloadUrl: "https://assets.aesthetic.computer/false.work/spiderlily-windows-2025.11.13-1609.zip",
    logUrl: "https://assets.aesthetic.computer/false.work/spiderlily-windows-2025.11.13-1609.txt"
  },
  {
    platform: "windows",
    version: "2025.11.12-1202",
    timestamp: new Date("2025-11-12T12:09:51"),
    sizeMB: 1224,
    level: "L_VerticalSlice_Demo",
    ueVersion: "UE_5.6",
    downloadUrl: "https://assets.aesthetic.computer/false.work/spiderlily-windows-2025.11.12-1202.zip",
    logUrl: "https://assets.aesthetic.computer/false.work/spiderlily-windows-2025.11.12-1202.txt"
  },
  {
    platform: "windows",
    version: "2025.11.11-1817",
    timestamp: new Date("2025-11-11T18:26:20"),
    sizeMB: 1163,
    level: "L_VerticalSlice_Demo",
    ueVersion: "UE_5.6",
    downloadUrl: "https://assets.aesthetic.computer/false.work/spiderlily-windows-2025.11.11-1817.zip",
    logUrl: "https://assets.aesthetic.computer/false.work/spiderlily-windows-2025.11.11-1817.txt"
  },
  {
    platform: "windows",
    version: "2025.11.10-1505",
    timestamp: new Date("2025-11-10T15:09:48"),
    sizeMB: 961,
    level: "L_VerticalSlice_MainMenu",
    ueVersion: "UE_5.6",
    downloadUrl: "https://assets.aesthetic.computer/false.work/spiderlily-windows-2025.11.10-1505.zip",
    logUrl: "https://assets.aesthetic.computer/false.work/spiderlily-windows-2025.11.10-1505.txt"
  },
  {
    platform: "windows",
    version: "2025.11.08-1532",
    timestamp: new Date("2025-11-08T15:32:00"),
    sizeMB: 850,
    level: "L_VerticalSlice_Intro",
    ueVersion: "UE_5.6",
    downloadUrl: "https://assets.aesthetic.computer/false.work/spiderlily-windows-2025.11.08-1532.zip",
    logUrl: "https://assets.aesthetic.computer/false.work/spiderlily-windows-2025.11.08-1532.txt"
  },
  {
    platform: "windows",
    version: "2025.11.08-1347",
    timestamp: new Date("2025-11-08T13:47:00"),
    sizeMB: 849,
    level: "L_VerticalSlice_MainMenu",
    ueVersion: "UE_5.6",
    downloadUrl: "https://assets.aesthetic.computer/false.work/spiderlily-windows-2025.11.08-1347.zip",
    logUrl: "https://assets.aesthetic.computer/false.work/spiderlily-windows-2025.11.08-1347.txt"
  },
  {
    platform: "windows",
    version: "2025.11.08-1255",
    timestamp: new Date("2025-11-08T12:55:00"),
    sizeMB: 845,
    level: "L_VerticalSlice_MainMenu",
    ueVersion: "UE_5.6",
    downloadUrl: "https://assets.aesthetic.computer/false.work/spiderlily-windows-2025.11.08-1255.zip",
    logUrl: "https://assets.aesthetic.computer/false.work/spiderlily-windows-2025.11.08-1255.txt"
  },
  {
    platform: "windows",
    version: "2025.11.07-0207",
    timestamp: new Date("2025-11-07T02:07:00"),
    sizeMB: 446,
    level: "L_VerticalSlice_Demo",
    ueVersion: "UE_5.6",
    downloadUrl: "https://assets.aesthetic.computer/false.work/spiderlily-windows-2025.11.07-0207.zip",
    logUrl: "https://assets.aesthetic.computer/false.work/spiderlily-windows-2025.11.07-0207.txt"
  }
];

async function migrate() {
  const client = new MongoClient(MONGODB_CONNECTION_STRING);

  try {
    await client.connect();
    console.log("✅ Connected to MongoDB");

    const db = client.db(MONGODB_NAME);
    const builds = db.collection("false.work-builds");

    // Create indexes
    console.log("📋 Creating indexes...");
    await builds.createIndex({ timestamp: -1 });
    await builds.createIndex({ platform: 1, version: 1 }, { unique: true });
    console.log("✅ Indexes created");

    // Insert builds (skip duplicates)
    let inserted = 0;
    let skipped = 0;

    for (const build of existingBuilds) {
      try {
        await builds.insertOne({
          ...build,
          createdAt: new Date()
        });
        console.log(`  ✅ Inserted: ${build.platform} ${build.version}`);
        inserted++;
      } catch (error) {
        if (error.code === 11000) {
          console.log(`  ⏭️  Skipped (exists): ${build.platform} ${build.version}`);
          skipped++;
        } else {
          throw error;
        }
      }
    }

    console.log("");
    console.log("========================================");
    console.log(`✅ Migration complete!`);
    console.log(`   Inserted: ${inserted}`);
    console.log(`   Skipped:  ${skipped}`);
    console.log("========================================");

    // Verify
    const count = await builds.countDocuments();
    console.log(`   Total builds in collection: ${count}`);

  } finally {
    await client.close();
  }
}

migrate().catch(console.error);
