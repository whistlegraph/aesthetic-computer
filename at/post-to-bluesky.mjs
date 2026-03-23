#!/usr/bin/env node

/**
 * Post to Bluesky
 * 
 * Post to the official @aesthetic.computer Bluesky account.
 * Requires BSKY_IDENTIFIER and BSKY_APP_PASSWORD in .env
 * 
 * Usage:
 *   node post-to-bluesky.mjs "Hello from Aesthetic Computer! 🎨"
 *   node post-to-bluesky.mjs "New painting!" --image painting.png
 */

import { AtpAgent } from '@atproto/api'
import { config } from 'dotenv'
import { readFileSync } from 'fs'

config()

const BSKY_SERVICE = process.env.BSKY_SERVICE || 'https://bsky.social'
const BSKY_IDENTIFIER = process.env.BSKY_IDENTIFIER
const BSKY_APP_PASSWORD = process.env.BSKY_APP_PASSWORD

async function postToBluesky(text, options = {}) {
  if (!BSKY_IDENTIFIER || !BSKY_APP_PASSWORD) {
    console.error('❌ Error: Missing credentials')
    console.error('\nPlease set in .env:')
    console.error('  BSKY_IDENTIFIER=aesthetic.computer')
    console.error('  BSKY_APP_PASSWORD=your-app-password')
    console.error('\nGet app password from: https://bsky.app/settings/app-passwords')
    process.exit(1)
  }

  console.log(`\n📤 Posting to Bluesky as @${BSKY_IDENTIFIER}`)
  console.log(`📡 Using service: ${BSKY_SERVICE}\n`)

  const agent = new AtpAgent({ service: BSKY_SERVICE })

  try {
    // Login
    console.log('🔐 Logging in...')
    await agent.login({
      identifier: BSKY_IDENTIFIER,
      password: BSKY_APP_PASSWORD
    })
    console.log('✅ Logged in successfully\n')

    // Prepare post
    const postRecord = {
      text,
      createdAt: new Date().toISOString()
    }

    // Add image if provided
    if (options.imagePath) {
      console.log(`📷 Uploading image: ${options.imagePath}`)
      const imageData = readFileSync(options.imagePath)
      const { data } = await agent.uploadBlob(imageData, { encoding: 'image/png' })
      console.log('✅ Image uploaded\n')

      postRecord.embed = {
        $type: 'app.bsky.embed.images',
        images: [{
          image: data.blob,
          alt: options.altText || 'Image from Aesthetic Computer'
        }]
      }
    }

    // Post
    console.log('📝 Creating post...')
    console.log('───────────────────────────')
    console.log(text)
    console.log('───────────────────────────\n')

    const response = await agent.post(postRecord)

    console.log('✅ Post created successfully!')
    console.log(`🔗 URI: ${response.uri}`)
    console.log(`🆔 CID: ${response.cid}`)
    
    // Construct Bluesky web URL
    const rkey = response.uri.split('/').pop()
    const webUrl = `https://bsky.app/profile/${BSKY_IDENTIFIER}/post/${rkey}`
    console.log(`🌐 View: ${webUrl}\n`)

    return response

  } catch (error) {
    console.error('❌ Error posting:', error.message)
    
    if (error.status === 401) {
      console.error('\n💡 Tip: Check your app password is correct')
    }
    
    process.exit(1)
  }
}

// CLI
const args = process.argv.slice(2)
const text = args.find(arg => !arg.startsWith('--'))
const imageArg = args.find(arg => arg.startsWith('--image'))
const altTextArg = args.find(arg => arg.startsWith('--alt'))

if (!text) {
  console.error('Usage: node post-to-bluesky.mjs <text> [--image=path] [--alt=text]')
  console.error('\nExamples:')
  console.error('  node post-to-bluesky.mjs "Hello Bluesky! 🎨"')
  console.error('  node post-to-bluesky.mjs "New artwork" --image=painting.png')
  console.error('  node post-to-bluesky.mjs "Check this out" --image=art.png --alt="Abstract painting"')
  process.exit(1)
}

const options = {}
if (imageArg) options.imagePath = imageArg.split('=')[1]
if (altTextArg) options.altText = altTextArg.split('=')[1]

postToBluesky(text, options)
