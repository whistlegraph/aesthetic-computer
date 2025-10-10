#!/usr/bin/env node
// get-jeffrey-info.mjs
// Quick script to get jeffrey's user info from MongoDB

import { connect } from '../../../system/backend/database.mjs';
import { shell } from '../../../system/backend/shell.mjs';

async function main() {
  try {
    const database = await connect();
    const handles = database.db.collection('@handles');
    
    const jeffrey = await handles.findOne({ handle: 'jeffrey' });
    
    if (jeffrey) {
      console.log('\n✨ Jeffrey\'s User Info:\n');
      console.log('═══════════════════════════════════════');
      console.log(`Handle:      @${jeffrey.handle}`);
      console.log(`Sub (ID):    ${jeffrey._id}`);
      console.log(`ATProto DID: ${jeffrey.atproto_did || '(not set)'}`);
      console.log('═══════════════════════════════════════\n');
    } else {
      console.log('❌ @jeffrey handle not found in database');
    }
    
    await database.disconnect();
    
  } catch (error) {
    console.error(`Error: ${error.message}`);
    process.exit(1);
  }
}

main();
