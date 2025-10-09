#!/usr/bin/env node

/**
 * Share Random Painting to Bluesky
 * 
 * Pick a random painting from all-time aesthetic.computer gallery
 * and post it to Bluesky.
 * 
 * Usage:
 *   node share-random-painting.mjs
 *   node share-random-painting.mjs --preview
 */

import { BskyAgent, RichText } from '@atproto/api'
import { config } from 'dotenv'
import { writeFileSync } from 'fs'
import sharp from 'sharp'

config()

const BSKY_SERVICE = process.env.BSKY_SERVICE || 'https://bsky.social'
const BSKY_IDENTIFIER = process.env.BSKY_IDENTIFIER
const BSKY_APP_PASSWORD = process.env.BSKY_APP_PASSWORD
const AC_API = 'https://aesthetic.computer'

const args = process.argv.slice(2)
const isPreview = args.includes('--preview')

async function fetchRandomPainting() {
  console.log(`\n🎲 Fetching random painting from all time...\n`)
  
  // Fetch a large sample from TV feed
  const tvUrl = `${AC_API}/api/tv?limit=500`
  
  try {
    const response = await fetch(tvUrl)
    if (!response.ok) {
      throw new Error(`HTTP ${response.status}: ${response.statusText}`)
    }
    
    const data = await response.json()
    
    if (!data.media || !data.media.paintings) {
      throw new Error('No paintings found in TV feed')
    }
    
    // Filter paintings with handles
    const paintings = data.media.paintings.filter(p => p.owner.handle)
    
    if (paintings.length === 0) {
      throw new Error('No paintings with handles found')
    }
    
    // Pick random painting
    const randomIndex = Math.floor(Math.random() * paintings.length)
    const painting = paintings[randomIndex]
    
    const handle = painting.owner.handle.replace('@', '')
    
    console.log(`✅ Found ${paintings.length} paintings`)
    console.log(`🎲 Randomly selected #${randomIndex + 1}\n`)
    console.log(`👤 Artist: @${handle}`)
    console.log(`🎨 Slug: ${painting.slug}`)
    console.log(`📅 Date: ${new Date(painting.when).toLocaleString()}`)
    console.log(`🔗 URL: ${painting.media.url}\n`)
    
    return {
      url: painting.media.url,
      handle,
      slug: painting.slug,
      when: painting.when,
      totalAvailable: paintings.length,
      selectedIndex: randomIndex + 1
    }
  } catch (error) {
    console.error('❌ Failed to fetch paintings:', error.message)
    throw error
  }
}

async function downloadPainting(url) {
  console.log(`📥 Downloading painting...`)
  
  try {
    const response = await fetch(url)
    if (!response.ok) {
      throw new Error(`HTTP ${response.status}: ${response.statusText}`)
    }
    
    const arrayBuffer = await response.arrayBuffer()
    const buffer = Buffer.from(arrayBuffer)
    
    console.log(`✅ Downloaded ${buffer.length} bytes`)
    
    return buffer
  } catch (error) {
    console.error('❌ Failed to download painting:', error.message)
    throw error
  }
}

async function upscalePaintingIfNeeded(imageBuffer) {
  try {
    const image = sharp(imageBuffer)
    const metadata = await image.metadata()
    const { width, height } = metadata
    
    console.log(`📐 Original size: ${width}x${height}`)
    
    // If image is smaller than 256px in either dimension, upscale it
    if (width < 256 || height < 256) {
      // Calculate scale factor (3x or enough to get to at least 1024px)
      const scaleFactor = Math.max(3, Math.ceil(1024 / Math.max(width, height)))
      const newWidth = width * scaleFactor
      const newHeight = height * scaleFactor
      
      console.log(`🔍 Upscaling ${scaleFactor}x to ${newWidth}x${newHeight} (nearest neighbor)`)
      
      const upscaled = await image
        .resize(newWidth, newHeight, {
          kernel: 'nearest', // Nearest neighbor for pixel art
          fit: 'fill'
        })
        .png()
        .toBuffer()
      
      console.log(`✅ Upscaled to ${upscaled.length} bytes`)
      return upscaled
    }
    
    console.log(`✅ Size OK, no upscaling needed`)
    return imageBuffer
  } catch (error) {
    console.error('❌ Failed to process image:', error.message)
    console.log('⚠️  Using original image')
    return imageBuffer
  }
}

async function processImage(imageBuffer) {
  try {
    const image = sharp(imageBuffer)
    const metadata = await image.metadata()
    
    console.log(`📐 Original size: ${metadata.width}x${metadata.height}`)
    
    // Check if image is small (< 256px on either dimension)
    const needsUpscale = metadata.width < 256 || metadata.height < 256
    
    if (needsUpscale) {
      // Calculate scale factor (3x or enough to get to 1024px)
      const targetSize = 1024
      const scaleFactor = Math.max(
        3,
        Math.ceil(targetSize / Math.max(metadata.width, metadata.height))
      )
      
      const newWidth = metadata.width * scaleFactor
      const newHeight = metadata.height * scaleFactor
      
      console.log(`🔼 Upscaling ${scaleFactor}x to ${newWidth}x${newHeight} (nearest-neighbor)`)
      
      // Upscale with nearest-neighbor to preserve pixel art look
      const upscaled = await image
        .resize(newWidth, newHeight, {
          kernel: 'nearest',
          fit: 'fill'
        })
        .png()
        .toBuffer()
      
      console.log(`✅ Upscaled to ${upscaled.length} bytes\n`)
      return upscaled
    } else {
      console.log(`✅ Size ok, no upscaling needed\n`)
      // Convert to PNG if needed
      const processed = await image.png().toBuffer()
      return processed
    }
  } catch (error) {
    console.error('⚠️  Image processing failed, using original:', error.message)
    return imageBuffer
  }
}

async function postToBluesky(paintingData, imageBuffer) {
  if (!BSKY_IDENTIFIER || !BSKY_APP_PASSWORD) {
    console.error('❌ Error: Missing Bluesky credentials')
    process.exit(1)
  }

  console.log(`📤 Posting to Bluesky as @${BSKY_IDENTIFIER}\n`)

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

    // Create post with embedded image and AC URL with custom link text
    const acUrl = `https://aesthetic.computer/painting~@${paintingData.handle}/${paintingData.slug}`
    const linkText = `@${paintingData.handle}'s painting`
    const postText = `New painting by @${paintingData.handle} 🎨✨\n\nView: ${linkText}`
    
    console.log('📝 Creating post...')
    console.log('───────────────────────────')
    console.log(postText)
    console.log(`(Link: ${acUrl})`)
    console.log('───────────────────────────\n')

    // Use RichText to create custom link with different display text
    const rt = new RichText({ text: postText })
    await rt.detectFacets(agent)
    
    // Find and replace the link text facet with custom URL
    const linkTextIndex = postText.indexOf(linkText)
    if (linkTextIndex !== -1) {
      // Create a custom facet for the link with display text
      const byteStart = new TextEncoder().encode(postText.substring(0, linkTextIndex)).length
      const byteEnd = byteStart + new TextEncoder().encode(linkText).length
      
      rt.facets = rt.facets || []
      rt.facets.push({
        index: {
          byteStart,
          byteEnd
        },
        features: [{
          $type: 'app.bsky.richtext.facet#link',
          uri: acUrl
        }]
      })
    }

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
  console.log('\n🎨 Share Random Painting to Bluesky')
  console.log('═'.repeat(50))
  
  try {
    // Fetch random painting
    const paintingData = await fetchRandomPainting()
    
    // Download the painting image
    let imageBuffer = await downloadPainting(paintingData.url)
    
    // Process/upscale image if needed
    imageBuffer = await processImage(imageBuffer)
    
    if (isPreview) {
      // Save preview to disk
      const previewPath = './preview-random-painting.png'
      writeFileSync(previewPath, imageBuffer)
      console.log(`👀 Preview saved to: ${previewPath}`)
      console.log(`\n📊 Painting Info:`)
      console.log(`   Artist: @${paintingData.handle}`)
      console.log(`   Slug: ${paintingData.slug}`)
      console.log(`   Date: ${new Date(paintingData.when).toLocaleString()}`)
      console.log(`   Size: ${imageBuffer.length} bytes`)
      console.log(`   Selected: #${paintingData.selectedIndex} of ${paintingData.totalAvailable}`)
      console.log(`   AC URL: https://aesthetic.computer/painting~@${paintingData.handle}/${paintingData.slug}`)
      console.log(`\n💡 Run without --preview to post to Bluesky\n`)
    } else {
      // Post to Bluesky
      await postToBluesky(paintingData, imageBuffer)
    }
    
  } catch (error) {
    console.error('\n💥 Error:', error.message)
    process.exit(1)
  }
}

main()
