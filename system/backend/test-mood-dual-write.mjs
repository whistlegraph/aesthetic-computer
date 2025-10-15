#!/usr/bin/env node
// Test posting a mood via the API to verify dual-write works

import { connect } from './database.mjs';
import { userIDFromHandle } from './authorization.mjs';

const handle = process.argv[2] || 'jeffrey';
const moodText = process.argv[3] || `Test dual-write @ ${new Date().toISOString()}`;

console.log(`\n🧪 Testing mood dual-write for @${handle}...`);
console.log(`   Mood: "${moodText}"\n`);

const database = await connect();
const sub = await userIDFromHandle(handle, database);
const moods = database.db.collection('moods');

// Count before
const countBefore = await moods.countDocuments({ user: sub, deleted: { $ne: true } });
console.log(`📊 Before: ${countBefore} moods\n`);

// Post mood via API
const API_URL = process.env.API_URL || 'http://localhost:8888';
const response = await fetch(`${API_URL}/api/mood`, {
  method: 'POST',
  headers: {
    'Content-Type': 'application/json',
    // This would normally come from Auth0, but for testing we can check the backend
  },
  body: JSON.stringify({ mood: moodText, sub })
});

const result = await response.json();
console.log(`📬 API Response: ${response.status}`);
console.log(`   ${JSON.stringify(result)}\n`);

// Check MongoDB
const countAfter = await moods.countDocuments({ user: sub, deleted: { $ne: true } });
const latestMood = await moods.findOne({ user: sub }, { sort: { when: -1 } });

console.log(`📊 After: ${countAfter} moods`);
console.log(`   Latest mood: "${latestMood.mood.substring(0, 50)}"`);
console.log(`   MongoDB _id: ${latestMood._id}`);
console.log(`   ATProto rkey: ${latestMood.atproto?.rkey || '❌ MISSING'}`);

if (latestMood.atproto?.rkey) {
  console.log(`\n✅ Dual-write SUCCESS! Mood synced to ATProto\n`);
} else {
  console.log(`\n❌ Dual-write FAILED - no ATProto rkey\n`);
}

await database.disconnect();
process.exit(0);
