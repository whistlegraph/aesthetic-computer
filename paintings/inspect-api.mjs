#!/usr/bin/env node

/**
 * Inspect API Endpoints
 * 
 * Test painting-related APIs on local or live server.
 * 
 * Usage:
 *   node inspect-api.mjs --tv
 *   node inspect-api.mjs --painting-code waf
 *   node inspect-api.mjs --media-collection @fifi/painting
 *   node inspect-api.mjs --endpoint /api/custom
 */

import { config } from 'dotenv';

config();

const AC_API = process.env.AC_API || 'http://localhost:8888';
const args = process.argv.slice(2);

async function testEndpoint(path, label) {
  console.log(`\n🔍 Testing: ${label}`);
  console.log(`   ${AC_API}${path}`);
  console.log('─'.repeat(70) + '\n');
  
  try {
    const start = Date.now();
    const response = await fetch(`${AC_API}${path}`);
    const duration = Date.now() - start;
    
    console.log(`📊 Response:`);
    console.log(`   Status: ${response.status} ${response.statusText}`);
    console.log(`   Duration: ${duration}ms`);
    console.log(`   Content-Type: ${response.headers.get('content-type')}`);
    console.log();
    
    if (response.ok) {
      const data = await response.json();
      console.log('✅ Success\n');
      console.log('📦 Response Data:');
      console.log(JSON.stringify(data, null, 2).split('\n').slice(0, 30).join('\n'));
      
      // Show summary based on endpoint
      if (data.media?.paintings) {
        console.log(`\n📊 Found ${data.media.paintings.length} paintings`);
        const withCodes = data.media.paintings.filter(p => p.code).length;
        console.log(`   With codes: ${withCodes}`);
      }
      
      console.log();
      return { ok: true, duration, data };
    } else {
      console.log('❌ Error Response\n');
      const text = await response.text();
      console.log(text);
      console.log();
      return { ok: false, duration, error: text };
    }
  } catch (error) {
    console.error(`❌ Error: ${error.message}\n`);
    return { ok: false, error: error.message };
  }
}

async function testTV() {
  const result = await testEndpoint('/api/tv?limit=10', 'TV Feed API');
  
  if (result.ok && result.data?.media?.paintings) {
    console.log('📸 Sample Paintings:\n');
    
    result.data.media.paintings.slice(0, 5).forEach((p, i) => {
      console.log(`   ${i + 1}. ${p.slug}`);
      console.log(`      User: ${p.owner?.handle || p.user}`);
      console.log(`      Code: ${p.code || '(none)'}`);
      console.log(`      URL: ${p.media?.url}`);
      console.log();
    });
  }
}

async function testPaintingCode(code) {
  const cleanCode = code.replace('#', '');
  await testEndpoint(`/api/painting-code/${cleanCode}`, `Painting Code Lookup: ${code}`);
}

async function testMediaCollection(path) {
  await testEndpoint(`/api/media-collection?for=${path}`, `Media Collection: ${path}`);
}

async function testCustomEndpoint(path) {
  await testEndpoint(path, `Custom Endpoint: ${path}`);
}

async function testAll() {
  console.log('\n🧪 Running All API Tests');
  console.log('═'.repeat(70));
  
  const tests = [
    { name: 'TV Feed', fn: () => testTV() },
    { name: 'Painting Code (if available)', fn: () => testPaintingCode('waf') },
    { name: 'Media Collection', fn: () => testMediaCollection('@fifi/painting') }
  ];
  
  const results = [];
  
  for (const test of tests) {
    try {
      const result = await test.fn();
      results.push({ name: test.name, success: result?.ok !== false });
    } catch (error) {
      results.push({ name: test.name, success: false });
    }
  }
  
  console.log('\n═'.repeat(70));
  console.log('📊 Test Summary:\n');
  
  results.forEach((result, i) => {
    const icon = result.success ? '✅' : '❌';
    console.log(`   ${icon} ${result.name}`);
  });
  
  console.log();
}

async function checkHealth() {
  console.log(`\n🏥 Checking API Health`);
  console.log(`   Server: ${AC_API}`);
  console.log('─'.repeat(70) + '\n');
  
  const endpoints = [
    '/api/tv',
    '/api/media-collection',
    '/.netlify/functions/tv',
    '/.netlify/functions/track-media'
  ];
  
  console.log('Testing endpoints:\n');
  
  for (const endpoint of endpoints) {
    try {
      const start = Date.now();
      const response = await fetch(`${AC_API}${endpoint}`);
      const duration = Date.now() - start;
      
      const icon = response.ok ? '✅' : '❌';
      console.log(`   ${icon} ${endpoint} (${response.status}, ${duration}ms)`);
    } catch (error) {
      console.log(`   ❌ ${endpoint} (${error.message})`);
    }
  }
  
  console.log();
}

async function main() {
  console.log(`\n🌐 API Inspector`);
  console.log(`   Target: ${AC_API}`);
  console.log(`   Environment: ${AC_API.includes('localhost') ? 'LOCAL' : 'LIVE'}`);
  console.log();
  
  try {
    if (args.includes('--tv')) {
      await testTV();
    } else if (args.includes('--painting-code')) {
      const codeIndex = args.indexOf('--painting-code');
      const code = args[codeIndex + 1];
      if (!code) {
        console.error('❌ Error: --painting-code requires a code');
        process.exit(1);
      }
      await testPaintingCode(code);
    } else if (args.includes('--media-collection')) {
      const pathIndex = args.indexOf('--media-collection');
      const path = args[pathIndex + 1];
      if (!path) {
        console.error('❌ Error: --media-collection requires a path');
        process.exit(1);
      }
      await testMediaCollection(path);
    } else if (args.includes('--endpoint')) {
      const endpointIndex = args.indexOf('--endpoint');
      const endpoint = args[endpointIndex + 1];
      if (!endpoint) {
        console.error('❌ Error: --endpoint requires a path');
        process.exit(1);
      }
      await testCustomEndpoint(endpoint);
    } else if (args.includes('--health')) {
      await checkHealth();
    } else if (args.includes('--all')) {
      await testAll();
    } else {
      console.log('Usage:');
      console.log('  node inspect-api.mjs --tv                         # Test TV feed');
      console.log('  node inspect-api.mjs --painting-code waf          # Test code lookup');
      console.log('  node inspect-api.mjs --media-collection @fifi/painting');
      console.log('  node inspect-api.mjs --endpoint /api/custom       # Test custom endpoint');
      console.log('  node inspect-api.mjs --health                     # Check API health');
      console.log('  node inspect-api.mjs --all                        # Run all tests');
      console.log();
      console.log('Environment:');
      console.log(`  AC_API=${AC_API}`);
      console.log();
    }
  } catch (error) {
    console.error('\n💥 Error:', error.message);
    process.exit(1);
  }
}

main();
