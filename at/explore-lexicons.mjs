#!/usr/bin/env node

/**
 * Explore Lexicons
 * 
 * Introspective tool to explore ATProto lexicons and their schemas.
 * 
 * Usage:
 *   node explore-lexicons.mjs
 */

console.log('\n🔍 ATProto Lexicon Explorer\n')
console.log('═══════════════════════════════════════\n')

// Standard Bluesky Lexicons
const blueskyLexicons = {
  'app.bsky.feed.post': {
    description: 'A post/status update on Bluesky',
    fields: {
      text: 'string (required) - The text content, max 300 graphemes',
      entities: 'array (optional) - Links, mentions, tags',
      reply: 'object (optional) - Parent/root URIs if this is a reply',
      embed: 'union (optional) - images | external | record | recordWithMedia',
      langs: 'array (optional) - Language codes (e.g., ["en"])',
      createdAt: 'datetime (required) - ISO 8601 timestamp',
      facets: 'array (optional) - Rich text features (mentions, links, tags)'
    },
    example: {
      $type: 'app.bsky.feed.post',
      text: 'Hello Bluesky! 🎨',
      createdAt: new Date().toISOString()
    }
  },

  'app.bsky.feed.like': {
    description: 'A like on a post',
    fields: {
      subject: 'ref (required) - AT-URI of the post being liked',
      createdAt: 'datetime (required) - When the like was created'
    }
  },

  'app.bsky.graph.follow': {
    description: 'A follow relationship',
    fields: {
      subject: 'string (required) - DID of the account being followed',
      createdAt: 'datetime (required) - When the follow was created'
    }
  },

  'app.bsky.embed.images': {
    description: 'Image embed (up to 4 images)',
    fields: {
      images: 'array (required) - Array of image objects',
      'images[].image': 'blob (required) - Image blob',
      'images[].alt': 'string (required) - Alt text for accessibility'
    }
  },

  'app.bsky.embed.external': {
    description: 'External link embed (link preview)',
    fields: {
      uri: 'string (required) - URL of the external link',
      title: 'string (required) - Title of the link',
      description: 'string (required) - Description of the link',
      thumb: 'blob (optional) - Thumbnail image'
    }
  }
}

console.log('📦 Standard Bluesky Lexicons\n')

for (const [name, schema] of Object.entries(blueskyLexicons)) {
  console.log(`\n━━━ ${name} ━━━`)
  console.log(`📝 ${schema.description}`)
  
  if (schema.fields) {
    console.log('\nFields:')
    for (const [field, desc] of Object.entries(schema.fields)) {
      console.log(`  • ${field}`)
      console.log(`    ${desc}`)
    }
  }

  if (schema.example) {
    console.log('\nExample:')
    console.log(JSON.stringify(schema.example, null, 2).split('\n').map(l => '  ' + l).join('\n'))
  }
}

// Proposed Aesthetic Computer Lexicons
console.log('\n\n═══════════════════════════════════════')
console.log('🎨 Proposed Aesthetic Computer Lexicons')
console.log('═══════════════════════════════════════\n')

const acLexicons = {
  'computer.aesthetic.painting': {
    description: 'A painting created in Aesthetic Computer',
    fields: {
      title: 'string (required) - Painting title',
      description: 'string (optional) - Description of the painting',
      media: 'blob (required) - The image file',
      dimensions: 'object (required) - { width, height } in pixels',
      tools: 'array (optional) - Tools used (brush, stamp, filter, etc.)',
      layers: 'number (optional) - Number of layers',
      brushStrokes: 'number (optional) - Approximate brush stroke count',
      timeSpent: 'number (optional) - Time spent in seconds',
      tags: 'array (optional) - User-defined tags',
      originalUrl: 'string (optional) - URL to full-res on DigitalOcean',
      bskyPostUri: 'string (optional) - Link to Bluesky post if shared',
      createdAt: 'datetime (required) - When created'
    },
    notes: [
      'Stores rich metadata that Bluesky posts cannot hold',
      'Links to Bluesky post for federation',
      'Original high-res stays on DigitalOcean Spaces'
    ]
  },

  'computer.aesthetic.piece': {
    description: 'A piece (interactive program) in Aesthetic Computer',
    fields: {
      slug: 'string (required) - The piece identifier (e.g., "wand")',
      title: 'string (required) - Display title',
      description: 'string (optional) - What the piece does',
      code: 'string (optional) - Source code (if public)',
      thumbnail: 'blob (optional) - Preview image',
      tags: 'array (optional) - Categories/tags',
      author: 'string (required) - DID of the creator',
      isPublic: 'boolean (required) - Whether others can use it',
      createdAt: 'datetime (required)',
      updatedAt: 'datetime (required)'
    }
  },

  'computer.aesthetic.kidlisp': {
    description: 'A KidLisp code snippet',
    fields: {
      title: 'string (required) - Snippet title',
      code: 'string (required) - The KidLisp code',
      description: 'string (optional) - What it does',
      tags: 'array (optional) - Tags for discovery',
      isPublic: 'boolean (required) - Share publicly?',
      forkOf: 'string (optional) - AT-URI if forked from another',
      createdAt: 'datetime (required)',
      updatedAt: 'datetime (required)'
    }
  },

  'computer.aesthetic.mood': {
    description: 'A mood/emotion entry',
    fields: {
      text: 'string (required) - The mood text',
      intensity: 'number (optional) - 1-10 scale',
      timestamp: 'datetime (required) - When felt',
      context: 'string (optional) - Additional context',
      isPrivate: 'boolean (required) - Private moods not federated'
    }
  }
}

for (const [name, schema] of Object.entries(acLexicons)) {
  console.log(`\n━━━ ${name} ━━━`)
  console.log(`📝 ${schema.description}`)
  
  console.log('\nFields:')
  for (const [field, desc] of Object.entries(schema.fields)) {
    console.log(`  • ${field}`)
    console.log(`    ${desc}`)
  }

  if (schema.notes) {
    console.log('\nNotes:')
    schema.notes.forEach(note => console.log(`  ℹ️  ${note}`))
  }
}

console.log('\n\n═══════════════════════════════════════')
console.log('💡 Key Insights')
console.log('═══════════════════════════════════════\n')

console.log('1. Bluesky lexicons (app.bsky.*) appear on Bluesky')
console.log('2. Custom lexicons (computer.aesthetic.*) are AC-only')
console.log('3. Best strategy: Publish both for maximum reach')
console.log('4. Custom lexicons store rich metadata Bluesky cannot')
console.log('5. Users can browse all their data via ATProto tools')
console.log()

console.log('Example: Publishing a painting')
console.log('  1. Upload to DigitalOcean Spaces (high-res original)')
console.log('  2. Create app.bsky.feed.post (appears on Bluesky)')
console.log('  3. Create computer.aesthetic.painting (full metadata)')
console.log('  4. Store in MongoDB (AC features like search/gallery)')
console.log()

console.log('Result:')
console.log('  ✅ Visible on Bluesky')
console.log('  ✅ Full metadata in ATProto')
console.log('  ✅ AC features work')
console.log('  ✅ User owns and can export data')
console.log()
