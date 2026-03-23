#!/usr/bin/env node

/**
 * Test All Tools
 * 
 * Quick test of all ATProto tools to verify setup.
 */

console.log('\n🧪 Testing ATProto Tools\n')
console.log('═══════════════════════════════════════\n')

import { AtpAgent } from '@atproto/api'

const testHandle = 'aesthetic.computer'
const service = 'https://public.api.bsky.app'

console.log('1️⃣  Testing Profile Query...')
try {
  const response = await fetch(`${service}/xrpc/app.bsky.actor.getProfile?actor=${testHandle}`)
  if (!response.ok) throw new Error('Failed')
  const profile = await response.json()
  console.log(`   ✅ Profile query works`)
  console.log(`   📝 Found: @${profile.handle} (DID: ${profile.did})`)
  console.log(`   📊 ${profile.followersCount || 0} followers, ${profile.postsCount || 0} posts\n`)
} catch (error) {
  console.log(`   ❌ Profile query failed: ${error.message}\n`)
}

console.log('2️⃣  Testing Feed Query...')
try {
  const response = await fetch(`${service}/xrpc/app.bsky.feed.getAuthorFeed?actor=${testHandle}&limit=1`)
  if (!response.ok) throw new Error('Failed')
  const feed = await response.json()
  console.log(`   ✅ Feed query works`)
  console.log(`   📝 Found ${feed.feed.length} post(s)\n`)
} catch (error) {
  console.log(`   ❌ Feed query failed: ${error.message}\n`)
}

console.log('3️⃣  Testing Auth (if credentials provided)...')
try {
  if (!process.env.BSKY_IDENTIFIER || !process.env.BSKY_APP_PASSWORD) {
    console.log('   ⏭️  Skipped (no credentials in .env)')
    console.log('   💡 To test: Copy .env.example to .env and add credentials\n')
  } else {
    const agent = new AtpAgent({ service: 'https://bsky.social' })
    await agent.login({
      identifier: process.env.BSKY_IDENTIFIER,
      password: process.env.BSKY_APP_PASSWORD
    })
    console.log(`   ✅ Authentication works`)
    console.log(`   👤 Logged in as @${agent.session.handle}\n`)
  }
} catch (error) {
  console.log(`   ❌ Authentication failed: ${error.message}`)
  console.log('   💡 Check credentials in .env\n')
}

console.log('═══════════════════════════════════════')
console.log('✨ Testing Complete!\n')
console.log('Available commands:')
console.log('  node query-profile.mjs aesthetic.computer')
console.log('  node query-posts.mjs aesthetic.computer')
console.log('  node explore-lexicons.mjs')
console.log('  node post-to-bluesky.mjs "Hello!" (requires .env setup)')
console.log()
