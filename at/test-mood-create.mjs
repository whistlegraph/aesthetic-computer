// Test creating a mood via the API
// This will test if new moods sync to ATProto

import { readFileSync } from 'fs';

async function testMoodCreation() {
  console.log('🧪 Testing mood creation and ATProto sync...\n');
  
  // You'll need a valid auth token to test this
  // For now, let's just trace through what should happen
  
  console.log('📋 Flow when creating a new mood:');
  console.log('1. POST /api/mood with { mood: "test mood" }');
  console.log('2. Function authorizes user via JWT');
  console.log('3. Inserts mood into MongoDB moods collection');
  console.log('4. Calls createMoodOnAtproto() which:');
  console.log('   - Looks up user ATProto credentials (did + password)');
  console.log('   - Logs into ATProto PDS');
  console.log('   - Creates record in computer.aesthetic.mood collection');
  console.log('   - Updates MongoDB with the rkey');
  console.log('5. Sends Firebase notification\n');
  
  console.log('⚠️  Potential issues:');
  console.log('1. User might not have ATProto credentials in MongoDB users collection');
  console.log('2. ATProto password might be invalid/expired');
  console.log('3. PDS might be unreachable');
  console.log('4. Error handling swallows ATProto errors (mood still saved)\n');
  
  // Let's check if we can see the mood function code for debugging
  const moodFunctionPath = './system/netlify/functions/mood.mjs';
  console.log('📄 Checking mood function error handling...\n');
  
  const code = readFileSync(moodFunctionPath, 'utf-8');
  
  // Find the atproto error handling
  const errorHandlingMatch = code.match(/catch \(atprotoError\)[^}]+}/);
  if (errorHandlingMatch) {
    console.log('Current ATProto error handling:');
    console.log(errorHandlingMatch[0]);
    console.log('\n');
  }
  
  console.log('💡 Recommendation:');
  console.log('Check Netlify function logs for these messages:');
  console.log('  - "🔄 Syncing mood to ATProto..."');
  console.log('  - "✅ Mood synced to ATProto with rkey: XXX"');
  console.log('  - "⚠️  Failed to sync mood to ATProto: [error]"');
  console.log('\nIf seeing failures, check:');
  console.log('  - MongoDB users collection for atproto.did and atproto.password');
  console.log('  - PDS_URL environment variable');
  console.log('  - PDS service status\n');
}

testMoodCreation();
