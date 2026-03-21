#!/usr/bin/env node
// measure-auth.mjs - Measure auth0/session timing via CDP
// Usage: node webmeister/measure-auth.mjs [url]
// Default URL: https://localhost:8888

import { PageController, listPages } from "./browser.mjs";

const targetUrl = process.argv[2] || "localhost:8888";

async function measureAuthTiming() {
  console.log("🔍 Looking for pages...");
  
  try {
    const pages = await listPages();
    console.log("\n📋 Available pages:");
    pages.forEach(p => console.log(`  [${p.index}] ${p.title} - ${p.url}`));
    
    // Find the aesthetic.computer page
    const acPage = pages.find(p => p.url.includes(targetUrl) || p.url.includes("aesthetic.computer"));
    if (!acPage) {
      console.log(`\n❌ No page found matching "${targetUrl}"`);
      console.log("   Try opening https://localhost:8888 in VS Code's browser first.");
      process.exit(1);
    }
    
    console.log(`\n🎯 Connecting to: ${acPage.title} (${acPage.url})`);
    
    const ctrl = await PageController.connect(acPage.url);
    
    // Wait a moment for boot to complete
    await ctrl.wait(500);
    
    // Check if acAuthTiming exists
    const hasAuthTiming = await ctrl.eval(() => typeof window.acAuthTiming !== "undefined");
    
    if (!hasAuthTiming) {
      console.log("\n⚠️  window.acAuthTiming not found - page may not have finished loading");
      console.log("    Try refreshing the page and running this script again.");
      await ctrl.close();
      process.exit(1);
    }
    
    // Get the auth timing data
    const timing = await ctrl.eval(() => {
      const t = window.acAuthTiming;
      t.computeDurations();
      return {
        bootStart: t.bootStart,
        auth0ScriptLoadStart: t.auth0ScriptLoadStart,
        auth0ScriptLoadEnd: t.auth0ScriptLoadEnd,
        auth0ClientCreateStart: t.auth0ClientCreateStart,
        auth0ClientCreateEnd: t.auth0ClientCreateEnd,
        isAuthenticatedStart: t.isAuthenticatedStart,
        isAuthenticatedEnd: t.isAuthenticatedEnd,
        getTokenStart: t.getTokenStart,
        getTokenEnd: t.getTokenEnd,
        getUserStart: t.getUserStart,
        getUserEnd: t.getUserEnd,
        userExistsFetchStart: t.userExistsFetchStart,
        userExistsFetchEnd: t.userExistsFetchEnd,
        sessionStartedSent: t.sessionStartedSent,
        durations: t.durations,
      };
    });
    
    // Get the boot timings too
    const bootTimings = await ctrl.eval(() => window._bootTimings || []);
    
    console.log("\n" + "=".repeat(60));
    console.log("📊 AUTH0 TIMING BREAKDOWN");
    console.log("=".repeat(60));
    
    const d = timing.durations;
    const fmt = (ms) => ms ? `${ms.toFixed(0)}ms` : "N/A";
    
    console.log(`
  Script Load:        ${fmt(d.scriptLoad)}
  Client Create:      ${fmt(d.clientCreate)}
  isAuthenticated:    ${fmt(d.isAuthenticated)}
  getTokenSilently:   ${fmt(d.getToken)}
  getUser:            ${fmt(d.getUser)}
  userExists fetch:   ${fmt(d.userExistsFetch)}
  ─────────────────────────────────
  Total to session:started: ${fmt(d.totalToSessionStarted)}
`);
    
    // Calculate time from boot start
    if (timing.bootStart && timing.sessionStartedSent) {
      const relativeStart = timing.auth0ScriptLoadStart - timing.bootStart;
      const relativeEnd = timing.sessionStartedSent - timing.bootStart;
      console.log(`  Auth0 started at: +${relativeStart.toFixed(0)}ms from boot`);
      console.log(`  session:started sent at: +${relativeEnd.toFixed(0)}ms from boot`);
    }
    
    console.log("\n" + "=".repeat(60));
    console.log("📋 BOOT LOG TIMELINE");
    console.log("=".repeat(60));
    
    bootTimings.forEach(t => {
      console.log(`  +${t.elapsed}ms: ${t.message}`);
    });
    
    // Get overall boot time
    const perfTimings = await ctrl.eval(() => window.acPerfTimings);
    if (perfTimings?.boot) {
      const bootKeys = Object.keys(perfTimings.boot).filter(k => k !== 'start');
      if (bootKeys.length > 0) {
        console.log("\n" + "=".repeat(60));
        console.log("⏱️  PERF MILESTONES");
        console.log("=".repeat(60));
        const bootStart = perfTimings.boot.start;
        bootKeys.forEach(k => {
          const elapsed = perfTimings.boot[k] - bootStart;
          console.log(`  ${k}: +${elapsed.toFixed(0)}ms`);
        });
      }
    }
    
    await ctrl.close();
    console.log("\n✅ Done!\n");
    
  } catch (err) {
    console.error("\n❌ Error:", err.message);
    console.error("   Make sure VS Code's browser is connected to CDP on port 9222");
    process.exit(1);
  }
}

measureAuthTiming();
