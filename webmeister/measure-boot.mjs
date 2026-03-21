#!/usr/bin/env node
// measure-boot.mjs - Refresh page and measure full boot timing via CDP
// Usage: node webmeister/measure-boot.mjs [url] [--repeat N]
// Default URL: https://localhost:8888

import { PageController, listPages, getBrowser } from "./browser.mjs";

const args = process.argv.slice(2);
const repeatIndex = args.indexOf("--repeat");
const repeatCount = repeatIndex !== -1 ? parseInt(args[repeatIndex + 1]) || 1 : 1;
const targetUrl = args.find(a => !a.startsWith("--") && a !== args[repeatIndex + 1]) || "localhost:8888";

async function waitForBoot(ctrl, timeout = 30000) {
  const start = Date.now();
  while (Date.now() - start < timeout) {
    try {
      const ready = await ctrl.eval(() => {
        // Check if boot completed (session:started sent)
        return window.acAuthTiming?.sessionStartedSent !== null;
      });
      if (ready) return true;
    } catch (e) {
      // Page might be reloading
    }
    await ctrl.wait(100);
  }
  return false;
}

async function measureSingleBoot(ctrl, runNumber) {
  console.log(`\n🔄 Run #${runNumber}: Refreshing page...`);
  
  // Reload the page
  await ctrl.eval(() => location.reload());
  await ctrl.wait(500);
  
  // Wait for boot to complete
  const bootCompleted = await waitForBoot(ctrl);
  
  if (!bootCompleted) {
    console.log("⚠️  Boot didn't complete in time");
    return null;
  }
  
  // Wait a tiny bit more for final timing data
  await ctrl.wait(200);
  
  // Get timing data
  const timing = await ctrl.eval(() => {
    const t = window.acAuthTiming;
    if (!t) return null;
    t.computeDurations();
    return {
      durations: { ...t.durations },
      bootStart: t.bootStart,
      sessionStartedSent: t.sessionStartedSent,
    };
  });
  
  const bootTimings = await ctrl.eval(() => window._bootTimings || []);
  
  return { timing, bootTimings };
}

async function runMeasurements() {
  console.log("🔍 Looking for pages...");
  
  try {
    const pages = await listPages();
    const acPage = pages.find(p => p.url.includes(targetUrl) || p.url.includes("aesthetic.computer"));
    
    if (!acPage) {
      console.log(`\n❌ No page found matching "${targetUrl}"`);
      process.exit(1);
    }
    
    console.log(`🎯 Target: ${acPage.title} (${acPage.url})`);
    console.log(`📊 Will run ${repeatCount} measurement(s)\n`);
    
    const ctrl = await PageController.connect(acPage.url);
    const results = [];
    
    for (let i = 1; i <= repeatCount; i++) {
      const result = await measureSingleBoot(ctrl, i);
      if (result) {
        results.push(result);
        
        const d = result.timing.durations;
        const bootLast = result.bootTimings[result.bootTimings.length - 1];
        console.log(`   Total auth time: ${d.totalToSessionStarted?.toFixed(0) || "N/A"}ms`);
        console.log(`   Last boot event: +${bootLast?.elapsed || "?"}ms (${bootLast?.message || "?"})`);
      }
      
      if (i < repeatCount) {
        await ctrl.wait(1000); // Wait between runs
      }
    }
    
    await ctrl.close();
    
    if (results.length === 0) {
      console.log("\n❌ No successful measurements");
      process.exit(1);
    }
    
    // Print summary
    console.log("\n" + "=".repeat(60));
    console.log("📊 SUMMARY");
    console.log("=".repeat(60));
    
    if (results.length > 1) {
      // Calculate averages
      const avgAuth = results.reduce((sum, r) => sum + (r.timing.durations.totalToSessionStarted || 0), 0) / results.length;
      const minAuth = Math.min(...results.map(r => r.timing.durations.totalToSessionStarted || Infinity));
      const maxAuth = Math.max(...results.map(r => r.timing.durations.totalToSessionStarted || 0));
      
      console.log(`\n  Auth Time (${results.length} runs):`);
      console.log(`    Average: ${avgAuth.toFixed(0)}ms`);
      console.log(`    Min: ${minAuth.toFixed(0)}ms`);
      console.log(`    Max: ${maxAuth.toFixed(0)}ms`);
      
      // Breakdown averages
      const keys = ["scriptLoad", "clientCreate", "isAuthenticated", "getToken", "getUser", "userExistsFetch"];
      console.log(`\n  Breakdown (averages):`);
      keys.forEach(key => {
        const vals = results.map(r => r.timing.durations[key]).filter(v => v != null);
        if (vals.length > 0) {
          const avg = vals.reduce((a, b) => a + b, 0) / vals.length;
          console.log(`    ${key}: ${avg.toFixed(0)}ms`);
        }
      });
    } else {
      // Single run - show full detail
      const r = results[0];
      const d = r.timing.durations;
      console.log(`
  Script Load:        ${d.scriptLoad?.toFixed(0) || "N/A"}ms
  Client Create:      ${d.clientCreate?.toFixed(0) || "N/A"}ms
  isAuthenticated:    ${d.isAuthenticated?.toFixed(0) || "N/A"}ms
  getTokenSilently:   ${d.getToken?.toFixed(0) || "N/A"}ms
  getUser:            ${d.getUser?.toFixed(0) || "N/A"}ms
  userExists fetch:   ${d.userExistsFetch?.toFixed(0) || "N/A"}ms
  ─────────────────────────────────
  Total auth time:    ${d.totalToSessionStarted?.toFixed(0) || "N/A"}ms
`);
    }
    
    console.log("\n✅ Done!\n");
    
  } catch (err) {
    console.error("\n❌ Error:", err.message);
    process.exit(1);
  }
}

runMeasurements();
