#!/usr/bin/env node
// Oven CLI Test Script
// Tests all oven endpoints with optional sixel image output

import { execSync, spawn } from 'child_process';
import { writeFileSync, unlinkSync } from 'fs';
import { tmpdir } from 'os';
import { join } from 'path';

const OVEN_URL = process.env.OVEN_URL || 'https://oven.aesthetic.computer';
const SIXEL_ENABLED = process.env.SIXEL !== '0' && process.stdout.isTTY;

// ANSI colors
const c = {
  reset: '\x1b[0m',
  green: '\x1b[32m',
  red: '\x1b[31m',
  yellow: '\x1b[33m',
  cyan: '\x1b[36m',
  dim: '\x1b[2m',
};

function log(msg, color = '') {
  console.log(`${color}${msg}${c.reset}`);
}

function success(msg) { log(`✅ ${msg}`, c.green); }
function error(msg) { log(`❌ ${msg}`, c.red); }
function info(msg) { log(`ℹ️  ${msg}`, c.cyan); }
function warn(msg) { log(`⚠️  ${msg}`, c.yellow); }

// Display image in terminal using sixel (requires img2sixel or chafa)
async function displaySixel(buffer, label = '') {
  if (!SIXEL_ENABLED) return;
  
  const tmpFile = join(tmpdir(), `oven-test-${Date.now()}.png`);
  try {
    writeFileSync(tmpFile, buffer);
    
    // Try img2sixel first, fall back to chafa
    try {
      execSync(`img2sixel -w 200 "${tmpFile}"`, { stdio: 'inherit' });
    } catch {
      try {
        execSync(`chafa -s 30x15 "${tmpFile}"`, { stdio: 'inherit' });
      } catch {
        // No sixel support available
      }
    }
  } finally {
    try { unlinkSync(tmpFile); } catch {}
  }
}

// Fetch with timeout
async function fetchWithTimeout(url, options = {}, timeout = 30000) {
  const controller = new AbortController();
  const id = setTimeout(() => controller.abort(), timeout);
  
  try {
    const response = await fetch(url, { ...options, signal: controller.signal });
    clearTimeout(id);
    return response;
  } catch (e) {
    clearTimeout(id);
    throw e;
  }
}

// Test functions
async function testHealth() {
  info('Testing /health endpoint...');
  const start = Date.now();
  const res = await fetchWithTimeout(`${OVEN_URL}/health`);
  const data = await res.json();
  const duration = Date.now() - start;
  
  if (res.ok && data.status === 'ok') {
    success(`Health check passed (${duration}ms)`);
    return true;
  } else {
    error(`Health check failed: ${JSON.stringify(data)}`);
    return false;
  }
}

async function testIcon(piece = 'prompt') {
  info(`Testing /icon/128x128/${piece}.png ...`);
  const start = Date.now();
  const res = await fetchWithTimeout(`${OVEN_URL}/icon/128x128/${piece}.png`, {}, 60000);
  const duration = Date.now() - start;
  
  if (res.ok) {
    const buffer = Buffer.from(await res.arrayBuffer());
    const bakeId = res.headers.get('X-Bake-Id');
    success(`Icon generated: ${buffer.length} bytes, ${duration}ms ${bakeId ? `(${bakeId})` : ''}`);
    await displaySixel(buffer, `${piece} icon`);
    return true;
  } else {
    const text = await res.text();
    error(`Icon failed: ${res.status} - ${text}`);
    return false;
  }
}

async function testPreview(piece = 'prompt', size = '1200x630') {
  info(`Testing /preview/${size}/${piece}.png ...`);
  const start = Date.now();
  const res = await fetchWithTimeout(`${OVEN_URL}/preview/${size}/${piece}.png`, {}, 60000);
  const duration = Date.now() - start;
  
  if (res.ok) {
    const buffer = Buffer.from(await res.arrayBuffer());
    const bakeId = res.headers.get('X-Bake-Id');
    success(`Preview generated: ${buffer.length} bytes, ${duration}ms ${bakeId ? `(${bakeId})` : ''}`);
    await displaySixel(buffer, `${piece} preview`);
    return true;
  } else {
    const text = await res.text();
    error(`Preview failed: ${res.status} - ${text}`);
    return false;
  }
}

async function testGrab(piece = 'prompt', format = 'webp') {
  info(`Testing /grab/${format}/512/512/${piece} ...`);
  const start = Date.now();
  const res = await fetchWithTimeout(`${OVEN_URL}/grab/${format}/512/512/${piece}?duration=3000`, {}, 60000);
  const duration = Date.now() - start;
  
  if (res.ok) {
    const buffer = Buffer.from(await res.arrayBuffer());
    const grabId = res.headers.get('X-Grab-Id');
    success(`Grab generated: ${buffer.length} bytes, ${duration}ms ${grabId ? `(${grabId})` : ''}`);
    if (format === 'png') {
      await displaySixel(buffer, `${piece} grab`);
    }
    return true;
  } else {
    const text = await res.text();
    error(`Grab failed: ${res.status} - ${text}`);
    return false;
  }
}

async function testGrabStatus() {
  info('Testing /grab-status endpoint...');
  const res = await fetchWithTimeout(`${OVEN_URL}/grab-status`);
  const data = await res.json();
  
  if (res.ok) {
    success(`Grab status: ${data.active?.length || 0} active, ${data.recent?.length || 0} recent`);
    return true;
  } else {
    error(`Grab status failed: ${JSON.stringify(data)}`);
    return false;
  }
}

async function testKeepsLatest() {
  info('Testing /keeps/latest endpoint...');
  const res = await fetchWithTimeout(`${OVEN_URL}/keeps/latest`, { redirect: 'manual' });
  
  if (res.status === 302) {
    const location = res.headers.get('location');
    success(`Keeps latest redirects to: ${location}`);
    return true;
  } else if (res.status === 404) {
    warn('No keeps captured yet (expected if no mints with --thumbnail)');
    return true;
  } else {
    error(`Keeps latest unexpected status: ${res.status}`);
    return false;
  }
}

// Run all tests
async function runTests() {
  console.log('\n' + '='.repeat(60));
  log(`🔥 OVEN TEST SUITE`, c.yellow);
  log(`   Target: ${OVEN_URL}`, c.dim);
  log(`   Sixel: ${SIXEL_ENABLED ? 'enabled' : 'disabled'}`, c.dim);
  console.log('='.repeat(60) + '\n');
  
  const results = [];
  
  // Core endpoints
  results.push(['health', await testHealth()]);
  results.push(['grab-status', await testGrabStatus()]);
  results.push(['keeps-latest', await testKeepsLatest()]);
  
  // Image generation (slower)
  const testPiece = process.argv[2] || 'prompt';
  results.push(['icon', await testIcon(testPiece)]);
  results.push(['preview', await testPreview(testPiece)]);
  results.push(['grab-png', await testGrab(testPiece, 'png')]);
  // results.push(['grab-webp', await testGrab(testPiece, 'webp')]); // Skip animated for speed
  
  // Summary
  console.log('\n' + '='.repeat(60));
  log('📊 RESULTS', c.yellow);
  console.log('='.repeat(60));
  
  let passed = 0, failed = 0;
  for (const [name, result] of results) {
    if (result) {
      log(`  ✅ ${name}`, c.green);
      passed++;
    } else {
      log(`  ❌ ${name}`, c.red);
      failed++;
    }
  }
  
  console.log('='.repeat(60));
  log(`Total: ${passed}/${results.length} passed`, passed === results.length ? c.green : c.red);
  console.log('='.repeat(60) + '\n');
  
  process.exit(failed > 0 ? 1 : 0);
}

// CLI
const args = process.argv.slice(2);

if (args.includes('--help') || args.includes('-h')) {
  console.log(`
🔥 Oven Test Script

Usage:
  node test-oven.mjs [piece] [options]

Examples:
  node test-oven.mjs              # Test with 'prompt' piece
  node test-oven.mjs roz          # Test with '$roz' piece
  node test-oven.mjs meme --local # Test local server

Options:
  --local     Test against localhost:3002
  --prod      Test against oven.aesthetic.computer (default)
  --no-sixel  Disable sixel image output
  -h, --help  Show this help

Environment:
  OVEN_URL    Override the oven URL
  SIXEL=0     Disable sixel output
`);
  process.exit(0);
}

if (args.includes('--local')) {
  process.env.OVEN_URL = 'http://localhost:3002';
}

if (args.includes('--no-sixel')) {
  process.env.SIXEL = '0';
}

runTests().catch(e => {
  error(`Test suite failed: ${e.message}`);
  process.exit(1);
});
