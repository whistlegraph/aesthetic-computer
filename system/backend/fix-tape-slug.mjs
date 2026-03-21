#!/usr/bin/env node
// Fix tape slug in ATProto record - remove trailing Z

import { AtpAgent } from '@atproto/api';

const PDS_URL = process.env.PDS_URL || 'https://jeffrey.at.aesthetic.computer';
const ATPROTO_PASSWORD = process.env.ATPROTO_PASSWORD;
const ATPROTO_HANDLE = 'jeffrey.at.aesthetic.computer';

async function main() {
  const rkey = process.argv[2];
  const newSlug = process.argv[3];
  
  if (!rkey || !newSlug) {
    console.error('Usage: node fix-tape-slug.mjs <rkey> <new-slug>');
    console.error('Example: node fix-tape-slug.mjs 3m4hn7do6n22e 2025.10.31.04.27.43.689');
    process.exit(1);
  }
  
  if (!ATPROTO_PASSWORD) {
    console.error('❌ ATPROTO_PASSWORD environment variable not set');
    process.exit(1);
  }
  
  console.log(`🔧 Fixing tape slug for rkey: ${rkey}`);
  console.log(`📝 New slug: ${newSlug}`);
  
  // Login
  const agent = new AtpAgent({ service: PDS_URL });
  await agent.login({
    identifier: ATPROTO_HANDLE,
    password: ATPROTO_PASSWORD
  });
  console.log('✅ Logged in to PDS');
  
  // Get existing record
  const { data: record } = await agent.com.atproto.repo.getRecord({
    repo: agent.session.did,
    collection: 'computer.aesthetic.tape',
    rkey
  });
  
  console.log('📼 Current record:', JSON.stringify(record.value, null, 2));
  
  // Update slug
  const updatedValue = {
    ...record.value,
    slug: newSlug
  };
  
  console.log('📝 Updated record:', JSON.stringify(updatedValue, null, 2));
  
  // Put updated record
  await agent.com.atproto.repo.putRecord({
    repo: agent.session.did,
    collection: 'computer.aesthetic.tape',
    rkey,
    record: updatedValue
  });
  
  console.log('✅ ATProto record updated successfully!');
}

main().catch(err => {
  console.error('❌ Error:', err);
  process.exit(1);
});
