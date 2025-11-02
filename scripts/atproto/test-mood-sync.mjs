import { createMediaRecord } from '../system/backend/media-atproto.mjs';
import { connect } from '../system/backend/database.mjs';

const database = await connect();
const users = database.db.collection('users');
const moods = database.db.collection('moods');

// Get an unsynced mood
const moodDoc = await moods.findOne({ 
  'atproto.rkey': { $exists: false },
  deleted: false
});

if (!moodDoc) {
  console.log('No unsynced moods found');
  await database.disconnect();
  process.exit(1);
}

// Get the user for this mood
const user = await users.findOne({ _id: moodDoc.user });

if (!user) {
  console.log('User not found for mood');
  await database.disconnect();
  process.exit(1);
}

console.log('User:', user.handle);
console.log('Mood text:', moodDoc.mood);
console.log('Mood ID:', moodDoc._id);
console.log('ATProto handle:', user.atproto?.handle);
console.log('ATProto DID:', user.atproto?.did);

try {
  const result = await createMediaRecord(database, 'mood', moodDoc, { userSub: user._id });
  console.log('✅ Result:', result);
} catch (error) {
  console.error('❌ Error:', error.message);
  console.error('Stack:', error.stack);
}

await database.disconnect();
