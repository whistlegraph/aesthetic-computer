#!/usr/bin/env node
// show-boot-perf.mjs - Display current boot performance from window._bootTimings

import { withCDP } from './cdp.mjs';

await withCDP(async (cdp) => {
  // Get boot timings from the page
  const timings = await cdp.eval('window._bootTimings || []');
  
  if (!timings || timings.length === 0) {
    console.log('❌ No boot timing data available');
    console.log('   Boot timings are only available during initial load');
    console.log('   Refresh the page to capture new timings');
    process.exit(1);
  }
  
  console.log('\n🚀 Boot Performance Timeline:\n');
  
  let lastTime = 0;
  timings.forEach(({ message, elapsed }) => {
    const delta = lastTime === 0 ? 0 : elapsed - lastTime;
    const deltaStr = lastTime === 0 ? '' : ` (+${delta}ms)`;
    console.log(`  ${String(elapsed).padStart(5)}ms${deltaStr.padEnd(10)} ${message}`);
    lastTime = elapsed;
  });
  
  const totalTime = timings[timings.length - 1]?.elapsed || 0;
  console.log(`\n  Total boot time: ${totalTime}ms`);
  
  // Calculate key metrics
  const auth0Start = timings.find(t => t.message.includes('auth0-start'));
  const auth0Complete = timings.find(t => t.message.includes('auth0-complete'));
  const biosLoad = timings.find(t => t.message.includes('loading parse.mjs'));
  const biosReady = timings.find(t => t.message.includes('canvas-setup-start'));
  
  if (auth0Start && auth0Complete) {
    const auth0Time = auth0Complete.elapsed - auth0Start.elapsed;
    console.log(`\n  📊 Auth0 time: ${auth0Time}ms`);
  }
  
  if (biosLoad && biosReady) {
    const moduleTime = biosReady.elapsed - biosLoad.elapsed;
    console.log(`  📦 Module loading: ${moduleTime}ms`);
  }
  
  console.log('');
}, {
  targetUrl: 'https://localhost:8888',
  verbose: false
});
