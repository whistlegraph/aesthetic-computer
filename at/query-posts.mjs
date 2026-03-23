#!/usr/bin/env node

/**
 * Query Posts
 * 
 * Introspective tool to fetch and display posts from an ATProto account.
 * 
 * Usage:
 *   node query-posts.mjs aesthetic.computer
 *   node query-posts.mjs aesthetic.computer --limit 20
 */

import { AtpAgent } from '@atproto/api'
import { config } from 'dotenv'

config()

const BSKY_SERVICE = process.env.BSKY_SERVICE || 'https://bsky.social'

async function queryPosts(actor, limit = 10) {
  console.log(`\n📝 Querying posts from: ${actor}`)
  console.log(`📡 Using service: ${BSKY_SERVICE}`)
  console.log(`📊 Limit: ${limit}\n`)

  const agent = new AtpAgent({ service: BSKY_SERVICE })

  try {
    const feed = await agent.getAuthorFeed({ actor, limit })
    
    console.log('═══════════════════════════════════════')
    console.log(`Found ${feed.data.feed.length} posts`)
    console.log('═══════════════════════════════════════\n')

    if (feed.data.feed.length === 0) {
      console.log('(no posts found)')
      return
    }

    feed.data.feed.forEach((item, i) => {
      const post = item.post
      const record = post.record
      const author = post.author
      
      console.log(`\n━━━ Post ${i + 1} ━━━━━━━━━━━━━━━━━━━━━━`)
      console.log(`Author:  @${author.handle}`)
      console.log(`Posted:  ${new Date(post.indexedAt).toLocaleString()}`)
      console.log(`URI:     ${post.uri}`)
      console.log()
      console.log('Text:')
      console.log(record.text || '(no text)')
      console.log()
      
      // Engagement
      console.log(`❤️  ${post.likeCount || 0} likes`)
      console.log(`💬 ${post.replyCount || 0} replies`)
      console.log(`🔁 ${post.repostCount || 0} reposts`)
      
      // Embeds
      if (post.embed) {
        console.log()
        console.log('Embed:')
        if (post.embed.images) {
          console.log(`  📷 ${post.embed.images.length} image(s)`)
          post.embed.images.forEach((img, j) => {
            console.log(`     ${j + 1}. ${img.alt || '(no alt text)'}`)
            console.log(`        ${img.thumb}`)
          })
        } else if (post.embed.external) {
          console.log(`  🔗 Link: ${post.embed.external.title}`)
          console.log(`     ${post.embed.external.uri}`)
        } else if (post.embed.record) {
          console.log(`  📄 Quoted post: ${post.embed.record.uri}`)
        } else {
          console.log(`  ${post.embed.$type}`)
        }
      }

      // Reply info
      if (record.reply) {
        console.log()
        console.log('↩️  Reply to:', record.reply.parent.uri)
      }

      // Facets (mentions, links, hashtags)
      if (record.facets && record.facets.length > 0) {
        console.log()
        console.log('Features:')
        record.facets.forEach(facet => {
          facet.features.forEach(feature => {
            if (feature.$type === 'app.bsky.richtext.facet#mention') {
              console.log(`  @${feature.did}`)
            } else if (feature.$type === 'app.bsky.richtext.facet#link') {
              console.log(`  🔗 ${feature.uri}`)
            } else if (feature.$type === 'app.bsky.richtext.facet#tag') {
              console.log(`  #${feature.tag}`)
            }
          })
        })
      }
    })

    console.log('\n═══════════════════════════════════════\n')

  } catch (error) {
    console.error('❌ Error querying posts:', error.message)
    process.exit(1)
  }
}

// CLI
const args = process.argv.slice(2)
const actor = args.find(arg => !arg.startsWith('--'))
const limitArg = args.find(arg => arg.startsWith('--limit'))
const limit = limitArg ? parseInt(limitArg.split('=')[1]) : 10

if (!actor) {
  console.error('Usage: node query-posts.mjs <handle-or-did> [--limit=N]')
  console.error('\nExamples:')
  console.error('  node query-posts.mjs aesthetic.computer')
  console.error('  node query-posts.mjs aesthetic.computer --limit=20')
  process.exit(1)
}

queryPosts(actor, limit)
