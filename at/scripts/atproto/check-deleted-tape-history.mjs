#!/usr/bin/env node

/**
 * Check oven-bakes history for deleted tapes
 */

import { connect } from '../../../system/backend/database.mjs';
import { config } from 'dotenv';

config({ path: '../../../system/.env' });

async function main() {
  const database = await connect();

  try {
    const ovenBakes = database.db.collection('oven-bakes');
    
    const deletedCodes = ['j75', 'egl', 'wk7', 'yi7', 'znz', 'nj9'];
    
    console.log(`\n${'═'.repeat(80)}`);
    console.log(`🔍 CHECKING OVEN-BAKES HISTORY FOR DELETED TAPES`);
    console.log(`${'═'.repeat(80)}\n`);

    for (const code of deletedCodes) {
      const bake = await ovenBakes.findOne({ code });
      if (bake) {
        console.log(`${'─'.repeat(80)}`);
        console.log(`Code: ${code}`);
        console.log(`Slug: ${bake.slug}`);
        console.log(`MongoDB ID: ${bake.mongoId}`);
        console.log(`MP4 URL: ${bake.mp4Url || 'NONE'}`);
        console.log(`Thumbnail: ${bake.thumbnailUrl || 'NONE'}`);
        console.log(`Success: ${bake.success}`);
        console.log(`Completed: ${bake.completedAt}`);
        console.log(`ATProto rkey: ${bake.atprotoRkey || 'NONE'}`);
      } else {
        console.log(`Code: ${code} - NO OVEN-BAKES RECORD`);
      }
    }

    console.log(`\n${'═'.repeat(80)}\n`);

    await database.disconnect();
    process.exit(0);

  } catch (error) {
    console.error('❌ Fatal error:', error);
    await database.disconnect();
    process.exit(1);
  }
}

main().catch(err => {
  console.error('💥 Fatal error:', err);
  process.exit(1);
});
