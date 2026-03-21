#!/usr/bin/env node

/**
 * Query P    console.log('═══════════════════════════════════════')
    console.log('Profile Information')
    console.log('═══════════════════════════════════════')
    console.log(`Handle:       ${p.handle}`)
    console.log(`DID:          ${p.did}`)
    console.log(`Display Name: ${p.displayName || '(not set)'}`)
    console.log(`Description:  ${p.description || '(not set)'}`)
    console.log(`Followers:    ${p.followersCount || 0}`)
    console.log(`Following:    ${p.followsCount || 0}`)
    console.log(`Posts:        ${p.postsCount || 0}`)* Introspective tool to query ATProto profile information.
 * 
 * Usage:
 *   node query-profile.mjs aesthetic.computer
 *   node query-profile.mjs did:plc:z72i7hdynmk6r22z27h6tvur
 */

import { BskyAgent } from '@atproto/api'
import { config } from 'dotenv'

config() // Load .env if it exists

const BSKY_SERVICE = process.env.BSKY_SERVICE || 'https://public.api.bsky.app'

async function queryProfile(actor) {
  console.log(`\n🔍 Querying profile for: ${actor}`)
  console.log(`📡 Using service: ${BSKY_SERVICE}\n`)

  const agent = new BskyAgent({ service: BSKY_SERVICE })

  try {
    // Use unauthenticated API endpoint
    const response = await fetch(`${BSKY_SERVICE}/xrpc/app.bsky.actor.getProfile?actor=${actor}`)
    
    if (!response.ok) {
      throw new Error(`HTTP ${response.status}: ${response.statusText}`)
    }
    
    const profile = await response.json()
    const p = profile

    console.log('═══════════════════════════════════════')
    console.log('Profile Information')
    console.log('═══════════════════════════════════════')
    console.log(`Handle:       ${p.handle}`)
    console.log(`DID:          ${p.did}`)
    console.log(`Display Name: ${p.displayName || '(not set)'}`)
    console.log(`Description:  ${p.description || '(not set)'}`)
    console.log(`Followers:    ${p.followersCount || 0}`)
    console.log(`Following:    ${p.followsCount || 0}`)
    console.log(`Posts:        ${p.postsCount || 0}`)
    
    if (p.avatar) {
      console.log(`Avatar:       ${p.avatar}`)
    }
    
    if (p.banner) {
      console.log(`Banner:       ${p.banner}`)
    }

    if (p.indexedAt) {
      console.log(`Indexed:      ${new Date(p.indexedAt).toLocaleString()}`)
    }

    console.log('═══════════════════════════════════════\n')

    // Get some recent posts
    console.log('📝 Recent Posts (last 5):\n')
    const feedResponse = await fetch(`${BSKY_SERVICE}/xrpc/app.bsky.feed.getAuthorFeed?actor=${actor}&limit=5`)
    
    if (feedResponse.ok) {
      const feedData = await feedResponse.json()
      
      if (!feedData.feed || feedData.feed.length === 0) {
        console.log('   (no posts yet)')
      } else {
        feedData.feed.forEach((item, i) => {
          const post = item.post
          const text = post.record.text || '(no text)'
          const date = new Date(post.indexedAt).toLocaleDateString()
          const likes = post.likeCount || 0
          const replies = post.replyCount || 0
          const reposts = post.repostCount || 0
          
          console.log(`${i + 1}. [${date}] ${text.slice(0, 60)}${text.length > 60 ? '...' : ''}`)
          console.log(`   ❤️  ${likes}  💬 ${replies}  🔁 ${reposts}`)
          console.log(`   URI: ${post.uri}\n`)
        })
      }
    } else {
      console.log('   (could not fetch posts)')
    }

    return profile

  } catch (error) {
    console.error('❌ Error querying profile:', error.message)
    
    if (error.status === 400) {
      console.error('\n💡 Tip: Make sure the handle or DID is valid')
      console.error('   Examples: aesthetic.computer, did:plc:...')
    }
    
    process.exit(1)
  }
}

// CLI
const actor = process.argv[2]

if (!actor) {
  console.error('Usage: node query-profile.mjs <handle-or-did>')
  console.error('\nExamples:')
  console.error('  node query-profile.mjs aesthetic.computer')
  console.error('  node query-profile.mjs bsky.app')
  console.error('  node query-profile.mjs did:plc:z72i7hdynmk6r22z27h6tvur')
  process.exit(1)
}

queryProfile(actor)
