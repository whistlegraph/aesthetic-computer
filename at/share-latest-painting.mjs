#!/usr/bin/env node

/**
 * Share Latest Painting to Bluesky
 * 
 * Fetch the latest painting from a user's aesthetic.computer gallery
 * and post it to Bluesky with the image embedded.
 * 
 * Usage:
 *   node share-latest-painting.mjs @handle
 *   node share-latest-painting.mjs @handle --preview
 *   node share-latest-painting.mjs @handle --message "Custom message"
 */

import { BskyAgent, RichText } from '@atproto/api'
import { config } from 'dotenv'
import { writeFileSync } from 'fs'

config()

const BSKY_SERVICE = process.env.BSKY_SERVICE || 'https://bsky.social'
const BSKY_IDENTIFIER = process.env.BSKY_IDENTIFIER
const BSKY_APP_PASSWORD = process.env.BSKY_APP_PASSWORD
const AC_API = 'https://aesthetic.computer'

// Parse command line arguments
const args = process.argv.slice(2)
const handle = args[0]
const isPreview = args.includes('--preview')
const messageIndex = args.indexOf('--message')
const customMessage = messageIndex !== -1 ? args[messageIndex + 1] : null

async function fetchLatestPainting(userHandle) {
  console.log(`\n🔍 Fetching paintings for: ${userHandle}`)
  
  // Remove @ if present
  const cleanHandle = userHandle.startsWith('@') ? userHandle.slice(1) : userHandle
  
  // Use TV API to get paintings, then filter by handle
  const tvUrl = `${AC_API}/api/tv?limit=500`
  console.log(`📡 Querying TV feed...\n`)
  
  try {
    const response = await fetch(tvUrl)
    if (!response.ok) {
      throw new Error(`HTTP ${response.status}: ${response.statusText}`)
    }
    
    const data = await response.json()
    
    if (!data.media || !data.media.paintings) {
      throw new Error('No paintings found in TV feed')
    }
    
    // Filter paintings by handle
    const userPaintings = data.media.paintings.filter(painting => {
      const paintingHandle = painting.owner.handle?.replace('@', '') || null
      return paintingHandle === cleanHandle
    })
    
    if (userPaintings.length === 0) {
      throw new Error(`No paintings found for @${cleanHandle}`)
    }
    
    const latest = userPaintings[0]
    
    console.log(`✅ Found ${userPaintings.length} paintings for @${cleanHandle}`)
    console.log(`🎨 Latest: ${latest.slug}`)
    console.log(`📅 When: ${new Date(latest.when).toLocaleString()}`)
    console.log(`🔗 URL: ${latest.media.url}\n`)
    
    return {
      url: latest.media.url,
      handle: cleanHandle,
      slug: latest.slug,
      when: latest.when,
      totalPaintings: userPaintings.length
    }
  } catch (error) {
    console.error('❌ Failed to fetch paintings:', error.message)
    throw error
  }
}

async function downloadPainting(url) {
  console.log(`📥 Downloading painting from: ${url}`)
  
  try {
    const response = await fetch(url)
    if (!response.ok) {
      throw new Error(`HTTP ${response.status}: ${response.statusText}`)
    }
    
    const arrayBuffer = await response.arrayBuffer()
    const buffer = Buffer.from(arrayBuffer)
    
    console.log(`✅ Downloaded ${buffer.length} bytes\n`)
    
    return buffer
  } catch (error) {
    console.error('❌ Failed to download painting:', error.message)
    throw error
  }
}

async function postToBluesky(paintingData, imageBuffer, message) {
  if (!BSKY_IDENTIFIER || !BSKY_APP_PASSWORD) {
    console.error('❌ Error: Missing Bluesky credentials')
    console.error('\nPlease set in .env:')
    console.error('  BSKY_IDENTIFIER=aesthetic.computer')
    console.error('  BSKY_APP_PASSWORD=your-app-password')
    process.exit(1)
  }

  console.log(`📤 Posting to Bluesky as @${BSKY_IDENTIFIER}`)
  console.log(`📡 Using service: ${BSKY_SERVICE}\n`)

  const agent = new BskyAgent({ service: BSKY_SERVICE })

  try {
    // Login
    console.log('🔐 Logging in...')
    await agent.login({
      identifier: BSKY_IDENTIFIER,
      password: BSKY_APP_PASSWORD
    })
    console.log('✅ Logged in successfully\n')

    // Upload image as blob
    console.log('📤 Uploading painting to Bluesky...')
    const uploadResponse = await agent.uploadBlob(imageBuffer, {
      encoding: 'image/png'
    })
    console.log('✅ Image uploaded as blob\n')

    // Create post with embedded image and AC URL
    const acUrl = `https://aesthetic.computer/painting~@${paintingData.handle}/${paintingData.slug}`
    const postText = message || `New painting by @${paintingData.handle} 🎨✨\n\n${acUrl}`
    
    console.log('📝 Creating post...')
    console.log('───────────────────────────')
    console.log(postText)
    console.log('───────────────────────────\n')

    // Use RichText to automatically detect and format URLs as links
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
          alt: `Painting by @${paintingData.handle} on aesthetic.computer`
        }]
      }
    }

    const response = await agent.post(postRecord)

    console.log('✅ Post created successfully!')
    console.log(`🔗 URI: ${response.uri}`)
    console.log(`🆔 CID: ${response.cid}`)
    
    // Extract post ID from URI
    const postId = response.uri.split('/').pop()
    console.log(`🌐 View: https://bsky.app/profile/${BSKY_IDENTIFIER}/post/${postId}\n`)
    
    return response
  } catch (error) {
    console.error('❌ Failed to post to Bluesky:', error.message)
    throw error
  }
}

async function main() {
  if (!handle || handle.startsWith('--')) {
    console.error('\n❌ Usage: node share-latest-painting.mjs @handle [--preview] [--message "text"]')
    console.error('\nExamples:')
    console.error('  node share-latest-painting.mjs @jeffrey')
    console.error('  node share-latest-painting.mjs @jeffrey --preview')
    console.error('  node share-latest-painting.mjs @jeffrey --message "Check out this artwork! 🎨"')
    process.exit(1)
  }

  try {
    // Fetch latest painting metadata
    const paintingData = await fetchLatestPainting(handle)
    
    // Download the painting image
    const imageBuffer = await downloadPainting(paintingData.url)
    
    if (isPreview) {
      // Save preview to disk
      const previewPath = './preview-painting.png'
      writeFileSync(previewPath, imageBuffer)
      console.log(`👀 Preview saved to: ${previewPath}`)
      console.log(`\n📊 Painting Info:`)
      console.log(`   Handle: @${paintingData.handle}`)
      console.log(`   URL: ${paintingData.url}`)
      console.log(`   Total paintings: ${paintingData.totalPaintings}`)
      console.log(`   Size: ${imageBuffer.length} bytes`)
      console.log(`\n💡 Run without --preview to post to Bluesky`)
    } else {
      // Post to Bluesky
      await postToBluesky(paintingData, imageBuffer, customMessage)
    }
    
  } catch (error) {
    console.error('\n💥 Error:', error.message)
    process.exit(1)
  }
}

main()
