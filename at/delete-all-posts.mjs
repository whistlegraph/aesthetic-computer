#!/usr/bin/env node

/**
 * Delete All Bluesky Posts
 * 
 * Delete all posts from the @aesthetic.computer Bluesky account.
 * 
 * Usage:
 *   node delete-all-posts.mjs
 *   node delete-all-posts.mjs --confirm
 */

import { AtpAgent } from '@atproto/api'
import { config } from 'dotenv'

config()

const BSKY_SERVICE = process.env.BSKY_SERVICE || 'https://bsky.social'
const BSKY_IDENTIFIER = process.env.BSKY_IDENTIFIER
const BSKY_APP_PASSWORD = process.env.BSKY_APP_PASSWORD

const args = process.argv.slice(2)
const confirmed = args.includes('--confirm')

async function deleteAllPosts() {
  if (!BSKY_IDENTIFIER || !BSKY_APP_PASSWORD) {
    console.error('❌ Error: Missing Bluesky credentials')
    process.exit(1)
  }

  console.log(`\n🗑️  Delete All Posts from @${BSKY_IDENTIFIER}\n`)
  console.log('═'.repeat(50) + '\n')

  const agent = new AtpAgent({ service: BSKY_SERVICE })

  try {
    // Login
    console.log('🔐 Logging in...')
    await agent.login({
      identifier: BSKY_IDENTIFIER,
      password: BSKY_APP_PASSWORD
    })
    console.log('✅ Logged in successfully\n')

    // Fetch all posts
    console.log('📥 Fetching all posts...')
    const feed = await agent.getAuthorFeed({
      actor: BSKY_IDENTIFIER,
      limit: 100 // Bluesky max
    })

    const posts = feed.data.feed
    console.log(`✅ Found ${posts.length} posts\n`)

    if (posts.length === 0) {
      console.log('✨ No posts to delete!')
      return
    }

    // Show preview
    console.log('📋 Posts to delete:\n')
    posts.forEach((item, i) => {
      const post = item.post
      const text = post.record.text.substring(0, 60)
      console.log(`   ${i + 1}. ${text}${post.record.text.length > 60 ? '...' : ''}`)
    })
    console.log()

    if (!confirmed) {
      console.log('⚠️  DRY RUN MODE')
      console.log('💡 Run with --confirm to actually delete posts')
      console.log(`\n   node delete-all-posts.mjs --confirm\n`)
      return
    }

    // Confirm deletion
    console.log('🚨 DELETING POSTS...\n')

    let deleted = 0
    let failed = 0

    for (let i = 0; i < posts.length; i++) {
      const item = posts[i]
      const uri = item.post.uri
      
      try {
        await agent.deletePost(uri)
        console.log(`   ✅ [${i + 1}/${posts.length}] Deleted: ${uri.split('/').pop()}`)
        deleted++
        
        // Small delay to avoid rate limits
        await new Promise(resolve => setTimeout(resolve, 200))
      } catch (error) {
        console.error(`   ❌ [${i + 1}/${posts.length}] Failed: ${error.message}`)
        failed++
      }
    }

    console.log('\n' + '═'.repeat(50))
    console.log('✨ Summary\n')
    console.log(`✅ Deleted: ${deleted}`)
    console.log(`❌ Failed: ${failed}`)
    console.log(`📊 Total: ${posts.length}\n`)

  } catch (error) {
    console.error('\n💥 Error:', error.message)
    process.exit(1)
  }
}

deleteAllPosts()
