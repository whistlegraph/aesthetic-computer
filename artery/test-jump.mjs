#!/usr/bin/env node
// test-jump.mjs - Test jumping to different pieces using CDP

import Artery from './artery.mjs';

console.log('🧪 Testing piece navigation...\n');

const client = new Artery();
await client.connect();
console.log('✅ Connected to AC\n');

// Get current piece
const currentPiece = await client.getCurrentPiece();
console.log(`📍 Current piece: ${currentPiece || '(none)'}`);

// Jump to a different piece
const testPiece = 'line';
console.log(`\n🎯 Jumping to: ${testPiece}...`);
await client.jump(testPiece);

// Wait a moment for navigation
await new Promise(r => setTimeout(r, 1000));

// Verify we jumped
const newPiece = await client.getCurrentPiece();
console.log(`📍 New piece: ${newPiece}`);

if (newPiece && newPiece.includes(testPiece)) {
  console.log('✅ Jump successful!\n');
} else {
  console.log('⚠️  Jump may have failed\n');
}

// Jump back to prompt
console.log('🔙 Jumping back to prompt...');
await client.jump('prompt');
await new Promise(r => setTimeout(r, 1000));

const finalPiece = await client.getCurrentPiece();
console.log(`📍 Final piece: ${finalPiece}`);
console.log('✅ Navigation test complete\n');

client.close();
