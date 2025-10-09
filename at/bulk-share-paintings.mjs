#!/usr/bin/env node

/**
 * Bulk Share Paintings to Bluesky
 * 
 * Post multiple paintings from the aesthetic.computer TV feed to Bluesky.
 * 
 * Usage:
 *   node bulk-share-paintings.mjs 10
 *   node bulk-share-paintings.mjs 10 --delay 3000
 */

import { BskyAgent, RichText } from '@atproto/api'
import { config } from 'dotenv'

config()

const BSKY_SERVICE = process.env.BSKY_SERVICE || 'https://bsky.social'
const BSKY_IDENTIFIER = process.env.BSKY_IDENTIFIER
const BSKY_APP_PASSWORD = process.env.BSKY_APP_PASSWORD
const AC_API = 'https://aesthetic.computer'

const args = process.argv.slice(2)
const count = parseInt(args[0]) || 10
const delayIndex = args.indexOf('--delay')
const delay = delayIndex !== -1 ? parseInt(args[delayIndex + 1]) : 2000

let agent = null

async function login() {
  if (!BSKY_IDENTIFIER || !BSKY_APP_PASSWORD) {
    console.error('❌ Error: Missing Bluesky credentials')
    process.exit(1)
  }

  console.log(`🔐 Logging in as @${BSKY_IDENTIFIER}...`)
  agent = new BskyAgent({ service: BSKY_SERVICE })
  await agent.login({
    identifier: BSKY_IDENTIFIER,
    password: BSKY_APP_PASSWORD
  })
  console.log('✅ Logged in successfully\n')
}

async function fetchPaintings(limit) {
  console.log(`📡 Fetching ${limit} paintings from TV feed...\n`)
  
  const response = await fetch(`${AC_API}/api/tv?limit=${limit}`)
  if (!response.ok) {
    throw new Error(`HTTP ${response.status}: ${response.statusText}`)
  }
  
  const data = await response.json()
  
  if (!data.media || !data.media.paintings) {
    throw new Error('No paintings found')
  }
  
  // Filter out paintings we might have already posted (basic check)
  const paintings = data.media.paintings.filter(p => p.owner.handle)
  
  console.log(`✅ Found ${paintings.length} paintings with handles\n`)
  return paintings
}

async function downloadPainting(url) {
  const response = await fetch(url)
  if (!response.ok) {
    throw new Error(`HTTP ${response.status}`)
  }
  const arrayBuffer = await response.arrayBuffer()
  return Buffer.from(arrayBuffer)
}

async function postPainting(painting, index, total) {
  const handle = painting.owner.handle.replace('@', '')
  const slug = painting.slug
  const acUrl = `https://aesthetic.computer/painting~@${handle}/${slug}`
  
  console.log(`\n[${index + 1}/${total}] 🎨 Posting painting by @${handle}`)
  console.log(`    Slug: ${slug}`)
  console.log(`    URL: ${acUrl}`)
  
  try {
    // Download painting
    const imageBuffer = await downloadPainting(painting.media.url)
    console.log(`    ✅ Downloaded ${imageBuffer.length} bytes`)
    
    // Upload to Bluesky
    const uploadResponse = await agent.uploadBlob(imageBuffer, {
      encoding: 'image/png'
    })
    console.log(`    ✅ Uploaded to Bluesky`)
    
    // Create post with RichText for proper link formatting
    const postText = `New painting by @${handle} 🎨✨\n\n${acUrl}`
    
    const rt = new RichText({ text: postText })
    await rt.detectFacets(agent)
    
    const postRecord = {
      text: rt.text,
      facets: rt.facets,
      createdAt: new Date().toISOString(),
      embed: {
        $type: 'app.bsky.embed.images',
        images: [{
          image: uploadResponse.data.blob,
          alt: `Painting by @${handle} on aesthetic.computer`
        }]
      }
    }
    
    const response = await agent.post(postRecord)
    const postId = response.uri.split('/').pop()
    const bskyUrl = `https://bsky.app/profile/${BSKY_IDENTIFIER}/post/${postId}`
    
    console.log(`    ✅ Posted! ${bskyUrl}`)
    
    return { success: true, handle, slug, bskyUrl }
  } catch (error) {
    console.error(`    ❌ Failed: ${error.message}`)
    return { success: false, handle, slug, error: error.message }
  }
}

async function sleep(ms) {
  return new Promise(resolve => setTimeout(resolve, ms))
}

async function main() {
  console.log(`\n🚀 Bulk Share Paintings to Bluesky\n`)
  console.log(`   Count: ${count}`)
  console.log(`   Delay: ${delay}ms between posts\n`)
  console.log('═'.repeat(50) + '\n')
  
  try {
    // Login once
    await login()
    
    // Fetch paintings
    const paintings = await fetchPaintings(count * 2) // Fetch extra in case some fail
    
    if (paintings.length === 0) {
      console.error('❌ No paintings to post')
      process.exit(1)
    }
    
    // Post paintings
    const results = []
    const toPost = paintings.slice(0, count)
    
    for (let i = 0; i < toPost.length; i++) {
      const result = await postPainting(toPost[i], i, toPost.length)
      results.push(result)
      
      // Delay between posts (except for the last one)
      if (i < toPost.length - 1) {
        console.log(`    ⏳ Waiting ${delay}ms...`)
        await sleep(delay)
      }
    }
    
    // Summary
    console.log('\n' + '═'.repeat(50))
    console.log('✨ Summary\n')
    
    const successful = results.filter(r => r.success).length
    const failed = results.filter(r => !r.success).length
    
    console.log(`✅ Successful: ${successful}`)
    console.log(`❌ Failed: ${failed}`)
    console.log(`📊 Total: ${results.length}\n`)
    
    if (successful > 0) {
      console.log('🔗 Posted paintings:')
      results.filter(r => r.success).forEach((r, i) => {
        console.log(`   ${i + 1}. @${r.handle} - ${r.bskyUrl}`)
      })
    }
    
    if (failed > 0) {
      console.log('\n⚠️  Failed paintings:')
      results.filter(r => !r.success).forEach((r, i) => {
        console.log(`   ${i + 1}. @${r.handle}/${r.slug} - ${r.error}`)
      })
    }
    
    console.log()
  } catch (error) {
    console.error('\n💥 Error:', error.message)
    process.exit(1)
  }
}

main()
