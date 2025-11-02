#!/usr/bin/env node
// Admin migration script - run migrations outside of Netlify functions
// Usage: node scripts/admin-migrate.mjs painting
// Usage: node scripts/admin-migrate.mjs piece

import { listAndSaveMedia } from "../backend/database.mjs";

const mediaType = process.argv[2];

if (!mediaType || !["painting", "piece"].includes(mediaType)) {
  console.error('Usage: node scripts/admin-migrate.mjs [painting|piece]');
  process.exit(1);
}

console.log(`🔄 Migrating ${mediaType}s from buckets to database...`);

try {
  await listAndSaveMedia(mediaType);
  console.log(`✅ Migration complete for ${mediaType}s!`);
} catch (error) {
  console.error(`❌ Migration failed:`, error);
  process.exit(1);
}
