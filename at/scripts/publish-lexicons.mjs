#!/usr/bin/env node

/**
 * Lexicon Publication Script
 * 
 * Publishes computer.aesthetic.* lexicon schemas to ATProto according to spec:
 * https://atproto.com/specs/lexicon#lexicon-publication-and-resolution
 * 
 * Features:
 * - Publishes all lexicons in /at/lexicons/computer/aesthetic/
 * - Creates com.atproto.lexicon.schema records with NSID as rkey
 * - Supports dry-run mode for safety
 * - Validates schema structure before publishing
 * - Lists existing published schemas
 * - Supports selective publication and updates
 * - Future-proof for new lexicon development
 * 
 * Usage:
 *   node publish-lexicons.mjs --dry-run          # Preview changes
 *   node publish-lexicons.mjs                    # Publish all (includes DNS setup)
 *   node publish-lexicons.mjs --list             # List published schemas
 *   node publish-lexicons.mjs --schema painting  # Publish specific schema
 *   node publish-lexicons.mjs --force            # Force republish all
 *   node publish-lexicons.mjs --skip-dns         # Skip DNS configuration
 *   node publish-lexicons.mjs --dns-only         # Only configure DNS, don't publish
 *   node publish-lexicons.mjs --verify-dns       # Verify DNS is configured correctly
 */

import { BskyAgent } from '@atproto/api';
import { readdir, readFile } from 'fs/promises';
import path from 'path';
import { fileURLToPath } from 'url';
import { config } from 'dotenv';
import { createOrUpdateTXTRecord, verifyTXTRecord } from './cloudflare-dns.mjs';

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const AT_DIR = path.resolve(__dirname, '..');

// Load environment variables
config({ path: path.join(AT_DIR, '.env') });

// Parse command line arguments
const args = process.argv.slice(2);
const dryRun = args.includes('--dry-run');
const listOnly = args.includes('--list');
const force = args.includes('--force');
const skipDns = args.includes('--skip-dns');
const dnsOnly = args.includes('--dns-only');
const verifyDns = args.includes('--verify-dns');

// Find specific schema argument
let specificSchema = null;
const schemaArgIndex = args.findIndex(arg => arg.startsWith('--schema=') || arg === '--schema');
if (schemaArgIndex !== -1) {
  const arg = args[schemaArgIndex];
  specificSchema = arg.startsWith('--schema=') 
    ? arg.split('=')[1]
    : args[schemaArgIndex + 1];
}

// ANSI colors for pretty output
const colors = {
  reset: '\x1b[0m',
  bright: '\x1b[1m',
  dim: '\x1b[2m',
  green: '\x1b[32m',
  yellow: '\x1b[33m',
  blue: '\x1b[34m',
  magenta: '\x1b[35m',
  cyan: '\x1b[36m',
  red: '\x1b[31m',
};

function log(message, color = 'reset') {
  console.log(`${colors[color]}${message}${colors.reset}`);
}

function logSection(title) {
  console.log();
  log(`━━━ ${title} ━━━`, 'cyan');
}

/**
 * Validate lexicon schema structure
 */
function validateSchema(schema, filename) {
  const errors = [];
  
  if (!schema.lexicon || schema.lexicon !== 1) {
    errors.push('Missing or invalid "lexicon" field (must be 1)');
  }
  
  if (!schema.id) {
    errors.push('Missing "id" field (NSID)');
  } else if (!schema.id.startsWith('computer.aesthetic.')) {
    errors.push(`Invalid NSID: ${schema.id} (must start with computer.aesthetic.)`);
  }
  
  if (!schema.defs || typeof schema.defs !== 'object') {
    errors.push('Missing or invalid "defs" field (must be object)');
  }
  
  if (errors.length > 0) {
    log(`✗ Schema validation failed for ${filename}:`, 'red');
    errors.forEach(err => log(`  - ${err}`, 'red'));
    return false;
  }
  
  return true;
}

/**
 * Load all lexicon schemas from disk
 */
async function loadLexicons() {
  const lexiconsDir = path.join(AT_DIR, 'lexicons', 'computer', 'aesthetic');
  const files = await readdir(lexiconsDir);
  const lexicons = [];
  
  for (const file of files.filter(f => f.endsWith('.json'))) {
    const schemaPath = path.join(lexiconsDir, file);
    const content = await readFile(schemaPath, 'utf-8');
    const schema = JSON.parse(content);
    
    if (validateSchema(schema, file)) {
      lexicons.push({
        filename: file,
        path: schemaPath,
        schema
      });
    }
  }
  
  return lexicons;
}

/**
 * Create schema record for publication
 */
function createSchemaRecord(schema) {
  return {
    $type: 'com.atproto.lexicon.schema',
    lexicon: schema.lexicon,
    id: schema.id,
    defs: schema.defs
  };
}

/**
 * List currently published schemas
 */
async function listPublishedSchemas(agent) {
  logSection('Published Lexicon Schemas');
  
  try {
    const response = await agent.api.com.atproto.repo.listRecords({
      repo: agent.session.did,
      collection: 'com.atproto.lexicon.schema',
      limit: 100
    });
    
    if (response.data.records.length === 0) {
      log('No schemas published yet', 'yellow');
      return [];
    }
    
    log(`Found ${response.data.records.length} published schema(s):\n`, 'bright');
    
    const published = [];
    for (const record of response.data.records) {
      const nsid = record.value.id;
      const uri = record.uri;
      published.push(nsid);
      
      log(`  ✓ ${nsid}`, 'green');
      log(`    URI: ${uri}`, 'dim');
      log(`    Lexicon Version: ${record.value.lexicon}`, 'dim');
      log(`    Definitions: ${Object.keys(record.value.defs).length}`, 'dim');
      console.log();
    }
    
    return published;
  } catch (error) {
    log(`Error listing schemas: ${error.message}`, 'red');
    return [];
  }
}

/**
 * Check if schema is already published
 */
async function isSchemaPublished(agent, nsid) {
  try {
    const response = await agent.api.com.atproto.repo.getRecord({
      repo: agent.session.did,
      collection: 'com.atproto.lexicon.schema',
      rkey: nsid
    });
    return response.data ? true : false;
  } catch (error) {
    // 404 means not published, any other error should be thrown
    if (error.message && error.message.includes('Could not locate record')) {
      return false;
    }
    throw error;
  }
}

/**
 * Publish a single lexicon schema
 */
async function publishSchema(agent, lexicon, isUpdate = false) {
  const nsid = lexicon.schema.id;
  const record = createSchemaRecord(lexicon.schema);
  
  try {
    await agent.api.com.atproto.repo.putRecord({
      repo: agent.session.did,
      collection: 'com.atproto.lexicon.schema',
      rkey: nsid,
      record: record
    });
    
    const action = isUpdate ? 'Updated' : 'Published';
    log(`  ✓ ${action} ${nsid}`, 'green');
    
    return true;
  } catch (error) {
    log(`  ✗ Failed to publish ${nsid}: ${error.message}`, 'red');
    return false;
  }
}

/**
 * Configure DNS TXT record for lexicon publication
 */
async function configureDNS(dryRun = false) {
  const recordName = '_lexicon.aesthetic.computer';
  const rootDomain = 'aesthetic.computer';
  const did = 'did:plc:k3k3wknzkcnekbnyde4dbatz';
  const content = `did=${did}`;
  
  log(`Setting up DNS TXT record for lexicon resolution`, 'bright');
  log(`  Record: ${recordName}`, 'dim');
  log(`  Value: ${content}`, 'dim');
  console.log();
  
  try {
    const result = await createOrUpdateTXTRecord(recordName, content, rootDomain, dryRun);
    
    if (result.action === 'error') {
      log(`✗ ${result.message}`, 'red');
      return false;
    } else if (result.action === 'none') {
      log(`✓ ${result.message}`, 'green');
      return true;
    } else if (dryRun) {
      log(`[DRY RUN] ${result.message}`, 'yellow');
      return true;
    } else {
      log(`✓ ${result.message}`, 'green');
      log(`  This enables external apps to discover your lexicon schemas`, 'dim');
      return true;
    }
  } catch (error) {
    log(`✗ DNS configuration failed: ${error.message}`, 'red');
    return false;
  }
}

/**
 * Verify DNS configuration
 */
async function verifyDNSConfiguration() {
  const recordName = '_lexicon.aesthetic.computer';
  const rootDomain = 'aesthetic.computer';
  const expectedContent = 'did=did:plc:k3k3wknzkcnekbnyde4dbatz';
  
  log(`Checking DNS TXT record...`, 'dim');
  
  try {
    const result = await verifyTXTRecord(recordName, expectedContent, rootDomain);
    
    if (!result.exists) {
      log(`✗ ${result.message}`, 'red');
      log(`  Run with --dns-only to create the record`, 'dim');
      return false;
    } else if (!result.matches) {
      log(`✗ ${result.message}`, 'yellow');
      log(`  Expected: ${result.expectedContent}`, 'dim');
      log(`  Actual: ${result.actualContent}`, 'dim');
      log(`  Run with --dns-only to fix`, 'dim');
      return false;
    } else {
      log(`✓ ${result.message}`, 'green');
      log(`  Content: ${result.actualContent}`, 'dim');
      return true;
    }
  } catch (error) {
    log(`✗ DNS verification failed: ${error.message}`, 'red');
    return false;
  }
}

/**
 * Main publication workflow
 */
async function main() {
  // Display header
  console.log();
  log('╔═══════════════════════════════════════╗', 'cyan');
  log('║   Lexicon Publication Script          ║', 'cyan');
  log('║   aesthetic.computer → ATProto        ║', 'cyan');
  log('╚═══════════════════════════════════════╝', 'cyan');
  
  if (dryRun) {
    log('\n🔍 DRY RUN MODE - No changes will be made', 'yellow');
  }
  
  // DNS-only mode: just configure DNS and exit
  if (dnsOnly) {
    logSection('DNS Configuration Only');
    await configureDNS(dryRun);
    return;
  }
  
  // Verify DNS mode: just check DNS and exit
  if (verifyDns) {
    logSection('Verifying DNS Configuration');
    await verifyDNSConfiguration();
    return;
  }
  
  // Initialize agent
  logSection('Initializing ATProto Agent');
  
  const pdsUrl = process.env.BSKY_SERVICE || 'https://bsky.social';
  const identifier = process.env.BSKY_IDENTIFIER;
  const password = process.env.BSKY_APP_PASSWORD;
  
  if (!identifier || !password) {
    log('✗ Missing credentials in .env file', 'red');
    log('  Required: BSKY_IDENTIFIER, BSKY_APP_PASSWORD', 'dim');
    process.exit(1);
  }
  
  log(`PDS: ${pdsUrl}`, 'dim');
  log(`Identifier: ${identifier}`, 'dim');
  log(`Note: Publishing lexicons to main account (aesthetic.computer)`, 'yellow');
  
  const agent = new BskyAgent({ service: pdsUrl });
  
  try {
    await agent.login({
      identifier,
      password
    });
    log(`✓ Logged in as ${agent.session.handle}`, 'green');
    log(`  DID: ${agent.session.did}`, 'dim');
  } catch (error) {
    log(`✗ Login failed: ${error.message}`, 'red');
    process.exit(1);
  }
  
  // List mode
  if (listOnly) {
    await listPublishedSchemas(agent);
    return;
  }
  
  // Load lexicon schemas
  logSection('Loading Lexicon Schemas');
  
  const lexicons = await loadLexicons();
  
  if (lexicons.length === 0) {
    log('✗ No valid lexicon schemas found', 'red');
    process.exit(1);
  }
  
  log(`Found ${lexicons.length} valid schema(s):\n`, 'bright');
  for (const lex of lexicons) {
    log(`  • ${lex.schema.id}`, 'blue');
    log(`    File: ${lex.filename}`, 'dim');
  }
  
  // Filter to specific schema if requested
  let schemasToPublish = lexicons;
  if (specificSchema) {
    schemasToPublish = lexicons.filter(lex => 
      lex.schema.id === specificSchema || 
      lex.schema.id === `computer.aesthetic.${specificSchema}`
    );
    
    if (schemasToPublish.length === 0) {
      log(`\n✗ Schema not found: ${specificSchema}`, 'red');
      process.exit(1);
    }
    
    log(`\n🎯 Publishing only: ${schemasToPublish[0].schema.id}`, 'yellow');
  }
  
  // Check existing publications
  logSection('Checking Existing Publications');
  
  const publicationStatus = [];
  for (const lex of schemasToPublish) {
    const isPublished = await isSchemaPublished(agent, lex.schema.id);
    publicationStatus.push({
      lexicon: lex,
      isPublished,
      willUpdate: isPublished && force
    });
    
    const status = isPublished ? 
      (force ? '⟳ Will update' : '✓ Already published') : 
      '+ Will publish';
    const color = isPublished ? (force ? 'yellow' : 'green') : 'blue';
    
    log(`  ${status}: ${lex.schema.id}`, color);
  }
  
  // Filter out already published (unless force mode)
  const toPublish = publicationStatus.filter(ps => !ps.isPublished || force);
  
  if (toPublish.length === 0) {
    log('\n✓ All schemas already published. Use --force to republish.', 'green');
    return;
  }
  
  // DNS Configuration phase (unless skipped)
  if (!skipDns) {
    logSection(dryRun ? 'Preview DNS Configuration' : 'Configuring DNS');
    
    const dnsSuccess = await configureDNS(dryRun);
    
    if (!dnsSuccess && !dryRun) {
      log('\n⚠️  DNS configuration failed, but continuing with schema publication...', 'yellow');
      log('   You can configure DNS later with: --dns-only', 'dim');
    }
  } else {
    log('\n⚠️  Skipping DNS configuration (--skip-dns)', 'yellow');
  }
  
  // Publication phase
  logSection(dryRun ? 'Preview Publication' : 'Publishing Schemas');
  
  const stats = {
    total: toPublish.length,
    published: 0,
    updated: 0,
    failed: 0
  };
  
  for (const { lexicon, isPublished } of toPublish) {
    const nsid = lexicon.schema.id;
    
    if (dryRun) {
      log(`\n[DRY RUN] Would ${isPublished ? 'update' : 'publish'}: ${nsid}`, 'yellow');
      log('Record structure:', 'dim');
      console.log(JSON.stringify(createSchemaRecord(lexicon.schema), null, 2));
    } else {
      const success = await publishSchema(agent, lexicon, isPublished);
      if (success) {
        if (isPublished) {
          stats.updated++;
        } else {
          stats.published++;
        }
      } else {
        stats.failed++;
      }
    }
  }
  
  // Summary
  logSection('Summary');
  
  if (dryRun) {
    log(`Would publish ${toPublish.length} schema(s)`, 'yellow');
    log('\nRun without --dry-run to publish', 'dim');
  } else {
    log(`Published: ${stats.published}`, stats.published > 0 ? 'green' : 'dim');
    log(`Updated: ${stats.updated}`, stats.updated > 0 ? 'green' : 'dim');
    log(`Failed: ${stats.failed}`, stats.failed > 0 ? 'red' : 'dim');
    log(`Total: ${stats.total}`, 'bright');
    
    if (stats.failed === 0) {
      console.log();
      log('✓ All schemas published successfully!', 'green');
      log('\nNext steps:', 'bright');
      log('1. Verify DNS propagation: node scripts/publish-lexicons.mjs --verify-dns', 'dim');
      log('2. Test resolution with external clients', 'dim');
      log('3. Update documentation with published NSIDs', 'dim');
    }
  }
  
  console.log();
}

// Run main function
main().catch(error => {
  console.error();
  log(`Fatal error: ${error.message}`, 'red');
  if (error.stack) {
    console.error(error.stack);
  }
  process.exit(1);
});
