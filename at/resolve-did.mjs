#!/usr/bin/env node

/**
 * Resolve DID
 * 
 * Introspective tool to resolve DIDs and inspect their documents.
 * Shows PDS endpoints, handle verification, keys, etc.
 * 
 * Usage:
 *   node resolve-did.mjs did:plc:k3k3wknzkcnekbnyde4dbatz
 *   node resolve-did.mjs aesthetic.computer
 */

import { config } from 'dotenv'

config()

const BSKY_SERVICE = process.env.BSKY_SERVICE || 'https://public.api.bsky.app'

async function resolveDID(input) {
  console.log(`\n🔍 Resolving: ${input}\n`)

  let did = input

  // If input looks like a handle, resolve it to DID first
  if (!input.startsWith('did:')) {
    console.log('📝 Input looks like a handle, resolving to DID...')
    try {
      const response = await fetch(`${BSKY_SERVICE}/xrpc/app.bsky.actor.getProfile?actor=${input}`)
      if (!response.ok) throw new Error('Profile not found')
      const profile = await response.json()
      did = profile.did
      console.log(`✅ Resolved @${input} → ${did}\n`)
    } catch (error) {
      console.error(`❌ Could not resolve handle: ${error.message}`)
      process.exit(1)
    }
  }

  // Resolve DID document
  console.log('📄 Fetching DID document...\n')
  
  try {
    // Try PLC directory for did:plc:* DIDs
    if (did.startsWith('did:plc:')) {
      const plcUrl = `https://plc.directory/${did}`
      const response = await fetch(plcUrl)
      
      if (!response.ok) throw new Error('DID not found in PLC directory')
      
      const doc = await response.json()
      
      console.log('═══════════════════════════════════════')
      console.log('DID Document (PLC Directory)')
      console.log('═══════════════════════════════════════')
      console.log(`DID:          ${did}`)
      
      if (doc.alsoKnownAs && doc.alsoKnownAs.length > 0) {
        console.log(`Also Known As:`)
        doc.alsoKnownAs.forEach(aka => {
          if (aka.startsWith('at://')) {
            console.log(`  • @${aka.slice(5)} (verified handle)`)
          } else {
            console.log(`  • ${aka}`)
          }
        })
      }
      
      console.log('\nServices:')
      if (doc.service && doc.service.length > 0) {
        doc.service.forEach(svc => {
          console.log(`  • ${svc.type}: ${svc.serviceEndpoint}`)
          if (svc.type === 'AtprotoPersonalDataServer') {
            console.log(`    ⭐ This is the PDS endpoint`)
          }
        })
      } else {
        console.log('  (no services)')
      }
      
      console.log('\nVerification Methods:')
      if (doc.verificationMethod && doc.verificationMethod.length > 0) {
        doc.verificationMethod.forEach(vm => {
          console.log(`  • ${vm.id}`)
          console.log(`    Type: ${vm.type}`)
          if (vm.publicKeyMultibase) {
            console.log(`    Key:  ${vm.publicKeyMultibase.slice(0, 20)}...`)
          }
        })
      } else {
        console.log('  (no verification methods)')
      }
      
      console.log('═══════════════════════════════════════\n')
      
      // Extract PDS endpoint
      const pds = doc.service?.find(s => s.type === 'AtprotoPersonalDataServer')
      if (pds) {
        console.log('🏠 PDS Information:')
        console.log(`   Endpoint: ${pds.serviceEndpoint}`)
        
        if (pds.serviceEndpoint === 'https://bsky.social') {
          console.log('   Provider: Bluesky (official PDS)')
        } else if (pds.serviceEndpoint.includes('aesthetic.computer')) {
          console.log('   Provider: Aesthetic Computer (own PDS) ⭐')
        } else {
          console.log('   Provider: Third-party PDS')
        }
        console.log()
      }
      
      // Full document
      console.log('📋 Full Document (JSON):\n')
      console.log(JSON.stringify(doc, null, 2))
      
    } else if (did.startsWith('did:web:')) {
      const domain = did.replace('did:web:', '')
      const webUrl = `https://${domain}/.well-known/did.json`
      console.log(`Fetching from: ${webUrl}`)
      
      const response = await fetch(webUrl)
      if (!response.ok) throw new Error('DID document not found')
      
      const doc = await response.json()
      console.log(JSON.stringify(doc, null, 2))
      
    } else {
      console.error('❌ Unsupported DID method')
      console.error('   Supported: did:plc:*, did:web:*')
      process.exit(1)
    }
    
  } catch (error) {
    console.error(`❌ Error resolving DID: ${error.message}`)
    process.exit(1)
  }
}

// CLI
const input = process.argv[2]

if (!input) {
  console.error('Usage: node resolve-did.mjs <did-or-handle>')
  console.error('\nExamples:')
  console.error('  node resolve-did.mjs did:plc:k3k3wknzkcnekbnyde4dbatz')
  console.error('  node resolve-did.mjs aesthetic.computer')
  console.error('  node resolve-did.mjs bsky.app')
  process.exit(1)
}

resolveDID(input)
