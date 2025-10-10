#!/usr/bin/env node
// check-handle-structure.mjs - See what handle records actually look like

import { config } from 'dotenv';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';
import { connect } from '../../../system/backend/database.mjs';

// Load environment from vault
const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const vaultEnvPath = join(__dirname, '../../../aesthetic-computer-vault/at/.env');
config({ path: vaultEnvPath });

try {
  const database = await connect();
  const handles = database.db.collection('@handles');
  
  console.log('\n🔍 Sampling @handles records:\n');
  
  // Get 5 records with handles
  const samples = await handles.find({ handle: { $exists: true } }).limit(5).toArray();
  
  samples.forEach((record, i) => {
    console.log(`Sample ${i + 1}:`);
    console.log(JSON.stringify(record, null, 2));
    console.log('');
  });
  
  await database.disconnect();
} catch (error) {
  console.error('❌ Error:', error.message);
  process.exit(1);
}
