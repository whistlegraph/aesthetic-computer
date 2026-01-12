#!/usr/bin/env node
/**
 * 📦 WebSocket Module Loader Test
 * 
 * Tests the WebSocket-based module streaming system:
 * 1. Opens prompt in VS Code
 * 2. Checks console logs for module loader activity
 * 3. Verifies modules are being loaded via WebSocket
 * 4. Tests cache hit/miss behavior
 */

import { getArtery } from './artery-auto.mjs';
import WebSocket from 'ws';

// Colors
const PURPLE_BG = '\x1b[45m';
const WHITE = '\x1b[97m';
const RESET = '\x1b[0m';
const GREEN = '\x1b[92m';
const CYAN = '\x1b[96m';
const YELLOW = '\x1b[93m';
const RED = '\x1b[91m';
const DIM = '\x1b[2m';

const testLog = (msg) => console.log(`${PURPLE_BG}${WHITE}📦${RESET} ${msg}`);
const successLog = (msg) => console.log(`${GREEN}✅ ${msg}${RESET}`);
const warnLog = (msg) => console.log(`${YELLOW}⚠️  ${msg}${RESET}`);
const errorLog = (msg) => console.log(`${RED}❌ ${msg}${RESET}`);
const dimLog = (msg) => console.log(`${DIM}   ${msg}${RESET}`);

// Test results
const results = {
  sessionServerConnected: false,
  moduleLoaderInitialized: false,
  modulesLoadedViaWS: [],
  modulesLoadedViaHTTP: [],
  cacheHits: 0,
  errors: [],
};

// Session server WebSocket URL
const SESSION_WS_URL = 'wss://localhost:8889';

/**
 * Test 1: Direct session server module streaming
 */
async function testSessionServerModules() {
  testLog('Test 1: Session Server Module Streaming\n');
  
  return new Promise((resolve) => {
    const ws = new WebSocket(SESSION_WS_URL, { rejectUnauthorized: false });
    let modulesReceived = 0;
    const testModules = ['lib/parse.mjs', 'lib/num.mjs', 'lib/geo.mjs'];
    
    const timeout = setTimeout(() => {
      warnLog('Session server test timed out');
      ws.close();
      resolve(false);
    }, 10000);
    
    ws.on('open', () => {
      results.sessionServerConnected = true;
      dimLog('Connected to session server');
      
      // Request module list first
      ws.send(JSON.stringify({ type: 'module:list' }));
    });
    
    ws.on('message', (data) => {
      try {
        const msg = JSON.parse(data.toString());
        
        if (msg.type === 'module:list') {
          dimLog(`Available modules: ${msg.modules.length}`);
          msg.modules.forEach(m => {
            dimLog(`  ${m.path} - ${(m.size / 1024).toFixed(1)}KB`);
          });
          
          // Now request specific modules
          testModules.forEach(path => {
            ws.send(JSON.stringify({ type: 'module:request', path }));
          });
        }
        
        if (msg.type === 'module:response') {
          modulesReceived++;
          dimLog(`Received: ${msg.path} (${msg.content.length} bytes, hash: ${msg.hash})`);
          results.modulesLoadedViaWS.push(msg.path);
          
          if (modulesReceived >= testModules.length) {
            clearTimeout(timeout);
            ws.close();
            successLog(`Session server streaming works! (${modulesReceived} modules)\n`);
            resolve(true);
          }
        }
        
        if (msg.type === 'module:error') {
          errorLog(`Module error: ${msg.path} - ${msg.error}`);
          results.errors.push(msg.error);
        }
      } catch (e) {
        // Ignore non-JSON messages
      }
    });
    
    ws.on('error', (err) => {
      clearTimeout(timeout);
      errorLog(`Session server connection failed: ${err.message}`);
      resolve(false);
    });
  });
}

/**
 * Test 2: Browser module loader via CDP
 */
async function testBrowserModuleLoader() {
  testLog('Test 2: Browser Module Loader Integration\n');
  
  try {
    const Artery = await getArtery();
    
    // Open panel if needed
    await Artery.openPanelStandalone?.();
    await new Promise(r => setTimeout(r, 500));
    
    // Connect via CDP
    const client = new Artery();
    await client.connect();
    dimLog('Connected via CDP');
    
    // Enable console logging
    await client.send('Runtime.enable');
    await client.send('Console.enable');
    
    // Collect console logs
    const logs = [];
    client.on('Console.messageAdded', (params) => {
      const text = params.message.text;
      const level = params.message.level || 'log';
      logs.push({ level, text });
      if (text.includes('📦')) {
        dimLog(`[console] ${text}`);
      }
    });
    
    client.on('Runtime.consoleAPICalled', (params) => {
      const text = params.args.map(arg => {
        if (arg.value !== undefined) return String(arg.value);
        if (arg.description) return arg.description;
        return JSON.stringify(arg);
      }).join(' ');
      const level = params.type || 'log';
      logs.push({ level, text });
      if (text.includes('📦')) {
        dimLog(`[console] ${text}`);
      }
    });
    
    // Navigate to prompt (triggers full boot)
    await client.jump('prompt');
    dimLog('Navigated to prompt');
    
    // Wait for boot to complete
    await new Promise(r => setTimeout(r, 4000));
    
    // Check if module loader initialized
    const loaderStatus = await client.send('Runtime.evaluate', {
      expression: `
        (function() {
          const loader = window.acModuleLoader;
          if (!loader) return { initialized: false, reason: 'not found' };
          return {
            initialized: true,
            connected: loader.connected,
            cachedModules: Array.from(loader.modules.keys()),
            wsUrl: loader.wsUrl
          };
        })()
      `,
      returnByValue: true
    });
    
    const status = loaderStatus.result?.value;
    if (status?.initialized) {
      results.moduleLoaderInitialized = true;
      dimLog(`Module loader initialized`);
      dimLog(`  WebSocket URL: ${status.wsUrl}`);
      dimLog(`  Connected: ${status.connected}`);
      dimLog(`  Cached modules: ${status.cachedModules.length}`);
      status.cachedModules.forEach(m => dimLog(`    - ${m}`));
      
      if (status.connected) {
        successLog('Module loader is connected via WebSocket!\n');
      } else {
        warnLog('Module loader fell back to HTTP (WebSocket not connected)\n');
      }
    } else {
      warnLog(`Module loader not initialized: ${status?.reason || 'unknown'}\n`);
    }
    
    // Check boot timings
    const timings = await client.send('Runtime.evaluate', {
      expression: `window._bootTimings || []`,
      returnByValue: true
    });
    
    const bootTimings = timings.result?.value || [];
    const wsLoads = bootTimings.filter(t => t.message.includes('via ws'));
    const httpLoads = bootTimings.filter(t => t.message.includes('loading') && !t.message.includes('via ws'));
    
    if (wsLoads.length > 0) {
      dimLog(`Modules loaded via WebSocket: ${wsLoads.length}`);
      wsLoads.forEach(t => dimLog(`  ${t.message} (+${t.elapsed}ms)`));
      results.modulesLoadedViaWS.push(...wsLoads.map(t => t.message));
    }
    
    // Check for any module loader errors in logs
    const errorLogs = logs.filter(l => l.level === 'error' && l.text.includes('module'));
    if (errorLogs.length > 0) {
      errorLogs.forEach(e => results.errors.push(e.text));
    }
    
    // Test cache behavior by requesting a module that should be cached
    const cacheTest = await client.send('Runtime.evaluate', {
      expression: `
        (async function() {
          const loader = window.acModuleLoader;
          if (!loader) return { error: 'no loader' };
          
          // Check if parse.mjs is cached
          const cached = loader.modules.get('lib/parse.mjs');
          if (cached) {
            return { cached: true, hash: cached.hash };
          }
          
          // Try to load it
          const start = performance.now();
          const url = await loader.load('lib/parse.mjs');
          const elapsed = performance.now() - start;
          
          return { 
            cached: false, 
            loaded: true, 
            elapsed: Math.round(elapsed),
            url: url.substring(0, 50) + '...'
          };
        })()
      `,
      returnByValue: true,
      awaitPromise: true
    });
    
    const cacheResult = cacheTest.result?.value;
    if (cacheResult?.cached) {
      results.cacheHits++;
      dimLog(`Cache hit for lib/parse.mjs (hash: ${cacheResult.hash})`);
    } else if (cacheResult?.loaded) {
      dimLog(`Loaded lib/parse.mjs in ${cacheResult.elapsed}ms`);
    }
    
    // Also test IndexedDB persistence while we have the panel open
    const dbCheck = await client.send('Runtime.evaluate', {
      expression: `
        (async function() {
          return new Promise((resolve) => {
            const req = indexedDB.open('ac-module-cache', 2);
            req.onerror = () => resolve({ error: 'IndexedDB not available' });
            req.onsuccess = () => {
              const db = req.result;
              if (!db.objectStoreNames.contains('modules')) {
                resolve({ error: 'No modules store' });
                return;
              }
              const tx = db.transaction('modules', 'readonly');
              const store = tx.objectStore('modules');
              const getAllReq = store.getAll();
              
              getAllReq.onsuccess = () => {
                const modules = getAllReq.result || [];
                resolve({
                  count: modules.length,
                  modules: modules.map(m => ({
                    path: m.path,
                    hash: m.hash,
                    size: m.content?.length || 0,
                    cachedAt: m.cachedAt
                  }))
                });
              };
            };
          });
        })()
      `,
      returnByValue: true,
      awaitPromise: true
    });
    
    const cacheStatus = dbCheck.result?.value;
    if (cacheStatus?.count > 0) {
      successLog(`IndexedDB cache has ${cacheStatus.count} modules persisted`);
      cacheStatus.modules.forEach(m => {
        dimLog(`  ${m.path} - ${(m.size / 1024).toFixed(1)}KB`);
      });
    } else if (cacheStatus?.error) {
      warnLog(`IndexedDB: ${cacheStatus.error}`);
    } else {
      dimLog('IndexedDB cache empty (WebSocket-only mode)');
    }
    
    // Clean up
    client.close();
    await Artery.closePanelStandalone?.();
    
    return true;
    
  } catch (err) {
    errorLog(`Browser test failed: ${err.message}`);
    results.errors.push(err.message);
    return false;
  }
}

/**
 * Print test summary
 */
function printSummary() {
  console.log('\n' + '═'.repeat(50));
  console.log(`${PURPLE_BG}${WHITE} 📦 WebSocket Module Loader Test Summary ${RESET}`);
  console.log('═'.repeat(50) + '\n');
  
  const checks = [
    ['Session server connection', results.sessionServerConnected],
    ['Module loader initialized', results.moduleLoaderInitialized],
    ['Modules loaded via WebSocket', results.modulesLoadedViaWS.length > 0],
    ['No errors', results.errors.length === 0],
  ];
  
  checks.forEach(([name, passed]) => {
    const icon = passed ? `${GREEN}✓${RESET}` : `${RED}✗${RESET}`;
    console.log(`  ${icon} ${name}`);
  });
  
  if (results.modulesLoadedViaWS.length > 0) {
    console.log(`\n  ${CYAN}WebSocket loaded:${RESET}`);
    [...new Set(results.modulesLoadedViaWS)].slice(0, 5).forEach(m => {
      dimLog(`  ${m}`);
    });
  }
  
  if (results.errors.length > 0) {
    console.log(`\n  ${RED}Errors:${RESET}`);
    results.errors.forEach(e => dimLog(`  ${e}`));
  }
  
  console.log();
  
  const allPassed = checks.every(([, passed]) => passed);
  return allPassed;
}

async function main() {
  console.log('\n' + '═'.repeat(50));
  console.log(`${PURPLE_BG}${WHITE} 📦 WebSocket Module Loader Test ${RESET}`);
  console.log('═'.repeat(50) + '\n');
  
  // Run tests
  await testSessionServerModules();
  await testBrowserModuleLoader();
  
  // Print summary
  const passed = printSummary();
  
  process.exit(passed ? 0 : 1);
}

main().catch(err => {
  errorLog(`Test crashed: ${err.message}`);
  console.error(err);
  process.exit(1);
});
