#!/usr/bin/env node
// explore-collections.mjs - See what collections exist and sample their schema

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
  
  console.log('\n🗄️  Database Collections:\n');
  
  // List all collections
  const collections = await database.db.listCollections().toArray();
  
  console.log(`Found ${collections.length} collections:\n`);
  
  for (const coll of collections) {
    console.log(`📁 ${coll.name}`);
    
    // Get a sample document
    const collection = database.db.collection(coll.name);
    const sample = await collection.findOne({});
    
    if (sample) {
      console.log('   Sample document:');
      const keys = Object.keys(sample);
      keys.forEach(key => {
        const value = sample[key];
        const type = Array.isArray(value) ? 'array' : typeof value;
        const preview = type === 'string' && value.length > 50 
          ? value.substring(0, 47) + '...' 
          : type === 'object' && value !== null
          ? '{...}'
          : value;
        console.log(`     ${key}: ${type} = ${JSON.stringify(preview)}`);
      });
    } else {
      console.log('   (empty collection)');
    }
    console.log('');
  }
  
  await database.disconnect();
} catch (error) {
  console.error('❌ Error:', error.message);
  process.exit(1);
}
