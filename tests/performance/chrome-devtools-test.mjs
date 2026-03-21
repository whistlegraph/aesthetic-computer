// Chrome DevTools Performance Testing with Puppeteer
// Automated performance analysis of aesthetic.computer boot sequence

import puppeteer from 'puppeteer';
import lighthouse from 'lighthouse';
import { writeFileSync } from 'fs';
import { join } from 'path';

const TEST_URL = process.env.TEST_URL || 'https://localhost:8888';
const OUTPUT_DIR = './tests/performance/reports';

/**
 * Measure boot performance with Chrome Performance API
 */
async function measureBootPerformance() {
  console.log('🚀 Starting Chrome DevTools Performance Test\n');
  
  const browser = await puppeteer.launch({
    headless: true,
    args: [
      '--no-sandbox',
      '--disable-setuid-sandbox',
      '--ignore-certificate-errors', // For local SSL
      '--disable-web-security', // For local CORS testing
    ]
  });

  try {
    const page = await browser.newPage();
    
    // Enable Performance monitoring
    await page.coverage.startJSCoverage();
    const client = await page.target().createCDPSession();
    await client.send('Performance.enable');
    
    console.log('📊 Loading page and capturing metrics...\n');
    const startTime = Date.now();
    
    // Navigate and wait for network idle
    await page.goto(TEST_URL, { 
      waitUntil: 'networkidle2',
      timeout: 30000 
    });
    
    // Wait for boot complete or timeout
    try {
      await page.waitForFunction(
        () => window.acBOOT_START_TIME && document.readyState === 'complete',
        { timeout: 10000 }
      );
    } catch (e) {
      console.warn('⚠️  Boot completion markers not found, using basic timing');
    }
    
    const loadTime = Date.now() - startTime;
    
    // Collect Performance metrics
    const performanceMetrics = await page.evaluate(() => {
      const perf = performance.getEntriesByType('navigation')[0];
      const paint = performance.getEntriesByType('paint');
      
      return {
        // Navigation timing
        dns: perf.domainLookupEnd - perf.domainLookupStart,
        tcp: perf.connectEnd - perf.connectStart,
        ttfb: perf.responseStart - perf.requestStart,
        download: perf.responseEnd - perf.responseStart,
        domInteractive: perf.domInteractive,
        domComplete: perf.domComplete,
        loadComplete: perf.loadEventEnd - perf.loadEventStart,
        
        // Paint timing
        firstPaint: paint.find(p => p.name === 'first-paint')?.startTime || 0,
        firstContentfulPaint: paint.find(p => p.name === 'first-contentful-paint')?.startTime || 0,
        
        // Custom timing
        bootStartTime: window.acBOOT_START_TIME,
        currentTime: performance.now(),
      };
    });
    
    // Get resource timing
    const resources = await page.evaluate(() => {
      return performance.getEntriesByType('resource').map(r => ({
        name: r.name.split('/').pop() || r.name,
        duration: r.duration,
        size: r.transferSize,
        type: r.initiatorType,
      })).sort((a, b) => b.duration - a.duration).slice(0, 10); // Top 10 slowest
    });
    
    // Get JS coverage
    const jsCoverage = await page.coverage.stopJSCoverage();
    const totalBytes = jsCoverage.reduce((sum, entry) => sum + entry.text.length, 0);
    const usedBytes = jsCoverage.reduce((sum, entry) => {
      const used = entry.ranges.reduce((s, range) => s + (range.end - range.start), 0);
      return sum + used;
    }, 0);
    const coveragePercent = ((usedBytes / totalBytes) * 100).toFixed(1);
    
    // Print results
    console.log('⏱️  Performance Metrics:');
    console.log(`  Total Load Time: ${loadTime}ms`);
    console.log(`  DNS Lookup: ${performanceMetrics.dns.toFixed(2)}ms`);
    console.log(`  TCP Connect: ${performanceMetrics.tcp.toFixed(2)}ms`);
    console.log(`  Time to First Byte: ${performanceMetrics.ttfb.toFixed(2)}ms`);
    console.log(`  Download: ${performanceMetrics.download.toFixed(2)}ms`);
    console.log(`  DOM Interactive: ${performanceMetrics.domInteractive.toFixed(2)}ms`);
    console.log(`  DOM Complete: ${performanceMetrics.domComplete.toFixed(2)}ms`);
    console.log(`  First Paint: ${performanceMetrics.firstPaint.toFixed(2)}ms`);
    console.log(`  First Contentful Paint: ${performanceMetrics.firstContentfulPaint.toFixed(2)}ms\n`);
    
    console.log('📦 Top 10 Slowest Resources:');
    resources.forEach((r, i) => {
      const size = r.size ? `(${(r.size / 1024).toFixed(1)}KB)` : '';
      console.log(`  ${i + 1}. ${r.name} - ${r.duration.toFixed(2)}ms ${size}`);
    });
    console.log();
    
    console.log('📊 JavaScript Coverage:');
    console.log(`  Total JS: ${(totalBytes / 1024).toFixed(1)}KB`);
    console.log(`  Used JS: ${(usedBytes / 1024).toFixed(1)}KB`);
    console.log(`  Coverage: ${coveragePercent}%`);
    console.log(`  Unused: ${((totalBytes - usedBytes) / 1024).toFixed(1)}KB\n`);
    
    // Performance thresholds
    const thresholds = {
      fcp: 1500, // First Contentful Paint
      tti: 3000, // Time to Interactive (approximated by domComplete)
      load: 5000, // Total load time
    };
    
    console.log('✅ Performance Checks:');
    console.log(`  ${performanceMetrics.firstContentfulPaint < thresholds.fcp ? '✅' : '❌'} FCP under ${thresholds.fcp}ms: ${performanceMetrics.firstContentfulPaint.toFixed(0)}ms`);
    console.log(`  ${performanceMetrics.domComplete < thresholds.tti ? '✅' : '❌'} TTI under ${thresholds.tti}ms: ${performanceMetrics.domComplete.toFixed(0)}ms`);
    console.log(`  ${loadTime < thresholds.load ? '✅' : '❌'} Load under ${thresholds.load}ms: ${loadTime}ms\n`);
    
    return {
      loadTime,
      metrics: performanceMetrics,
      resources,
      coverage: { totalBytes, usedBytes, percent: coveragePercent },
    };
    
  } finally {
    await browser.close();
  }
}

/**
 * Run Lighthouse audit
 */
async function runLighthouseAudit() {
  console.log('💡 Running Lighthouse Audit...\n');
  
  const browser = await puppeteer.launch({
    headless: true,
    args: [
      '--no-sandbox',
      '--disable-setuid-sandbox',
      '--ignore-certificate-errors',
    ]
  });

  try {
    const { lhr } = await lighthouse(TEST_URL, {
      port: new URL(browser.wsEndpoint()).port,
      output: 'json',
      onlyCategories: ['performance'],
      formFactor: 'desktop',
      screenEmulation: { disabled: true },
    });

    const scores = {
      performance: lhr.categories.performance.score * 100,
      fcp: lhr.audits['first-contentful-paint'].numericValue,
      lcp: lhr.audits['largest-contentful-paint'].numericValue,
      tbt: lhr.audits['total-blocking-time'].numericValue,
      cls: lhr.audits['cumulative-layout-shift'].numericValue,
      speedIndex: lhr.audits['speed-index'].numericValue,
    };

    console.log('💡 Lighthouse Performance Score:');
    console.log(`  Overall: ${scores.performance.toFixed(0)}/100`);
    console.log(`  First Contentful Paint: ${scores.fcp.toFixed(0)}ms`);
    console.log(`  Largest Contentful Paint: ${scores.lcp.toFixed(0)}ms`);
    console.log(`  Total Blocking Time: ${scores.tbt.toFixed(0)}ms`);
    console.log(`  Cumulative Layout Shift: ${scores.cls.toFixed(3)}`);
    console.log(`  Speed Index: ${scores.speedIndex.toFixed(0)}ms\n`);

    // Save full report
    const reportPath = join(OUTPUT_DIR, `lighthouse-${Date.now()}.json`);
    try {
      writeFileSync(reportPath, JSON.stringify(lhr, null, 2));
      console.log(`📄 Full report saved to: ${reportPath}\n`);
    } catch (e) {
      console.log('⚠️  Could not save report (directory may not exist)\n');
    }

    return scores;

  } finally {
    await browser.close();
  }
}

/**
 * Main test runner
 */
async function runTests() {
  try {
    console.log('🎯 Chrome DevTools Performance Testing\n');
    console.log(`Testing URL: ${TEST_URL}\n`);
    console.log('─'.repeat(60) + '\n');

    // Run performance measurement
    const perfResults = await measureBootPerformance();
    
    console.log('─'.repeat(60) + '\n');
    
    // Run Lighthouse audit (optional, can be slow)
    if (process.env.RUN_LIGHTHOUSE !== 'false') {
      const lighthouseResults = await runLighthouseAudit();
      console.log('─'.repeat(60) + '\n');
    }

    console.log('✅ All tests completed!\n');
    
    // Exit with error if performance is poor
    if (perfResults.loadTime > 5000) {
      console.error('❌ Performance threshold exceeded!');
      process.exit(1);
    }

  } catch (error) {
    console.error('❌ Test failed:', error.message);
    process.exit(1);
  }
}

// Run tests if called directly
if (import.meta.url === `file://${process.argv[1]}`) {
  runTests();
}

export { measureBootPerformance, runLighthouseAudit };
