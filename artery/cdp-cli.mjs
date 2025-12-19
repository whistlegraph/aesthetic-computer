#!/usr/bin/env node
// cdp-cli.mjs - Command-line tools for managing CDP connections

import CDP, { createCDP, withCDP } from './cdp.mjs';

const commands = {
  async list() {
    const cdp = new CDP({ verbose: true });
    const targets = await cdp.listTargets();
    
    console.log('\n📋 Available CDP Targets:\n');
    
    const acPages = targets.filter(t => 
      t.url && (t.url.includes('localhost:8888') || t.url.includes('aesthetic.computer'))
    );
    
    if (acPages.length > 0) {
      console.log('🎨 Aesthetic Computer Pages:');
      acPages.forEach(t => {
        console.log(`  ${t.type.padEnd(8)} ${t.url}`);
        console.log(`           ID: ${t.id}`);
      });
      console.log('');
    }
    
    const vscodePages = targets.filter(t => 
      t.url && t.url.includes('vscode') && !acPages.includes(t)
    );
    
    if (vscodePages.length > 0) {
      console.log(`📝 VS Code Pages (${vscodePages.length} total):`);
      vscodePages.slice(0, 3).forEach(t => {
        const title = t.title?.substring(0, 60) || 'Untitled';
        console.log(`  ${t.type.padEnd(8)} ${title}`);
      });
      if (vscodePages.length > 3) {
        console.log(`  ... and ${vscodePages.length - 3} more`);
      }
      console.log('');
    }
    
    console.log(`Total targets: ${targets.length}`);
  },

  async find(urlPattern) {
    if (!urlPattern) {
      console.error('Usage: cdp-cli find <url-pattern>');
      process.exit(1);
    }
    
    const cdp = new CDP({ verbose: true });
    const target = await cdp.findPage(urlPattern);
    
    if (target) {
      console.log('\n✅ Found target:');
      console.log(`  URL:   ${target.url}`);
      console.log(`  ID:    ${target.id}`);
      console.log(`  Type:  ${target.type}`);
      console.log(`  WS:    ${target.webSocketDebuggerUrl}`);
    } else {
      console.log(`\n❌ No target found matching: ${urlPattern}`);
      const pages = await cdp.findAestheticPages();
      if (pages.length > 0) {
        console.log('\nAvailable aesthetic.computer pages:');
        pages.forEach(p => console.log(`  - ${p.url}`));
      }
      process.exit(1);
    }
  },

  async connect(urlPattern) {
    const targetUrl = urlPattern || 'https://localhost:8888/prompt';
    
    await withCDP(async (cdp) => {
      const info = await cdp.getPageInfo();
      console.log('\n✅ Connected to page:');
      console.log(`  URL:        ${info.url}`);
      console.log(`  Title:      ${info.title}`);
      console.log(`  Ready:      ${info.readyState}`);
      console.log(`  Has Bios:   ${info.hasACBios ? '✓' : '✗'}`);
    }, { targetUrl, verbose: true });
  },

  async eval(expression) {
    if (!expression) {
      console.error('Usage: cdp-cli eval <expression>');
      process.exit(1);
    }
    
    await withCDP(async (cdp) => {
      const result = await cdp.eval(expression);
      console.log(result);
    }, { verbose: false });
  },

  async cache() {
    const cdp = new CDP({ verbose: true });
    const target = await cdp.findPage('https://localhost:8888');
    
    if (target) {
      await cdp.cacheTarget(target);
      console.log('✅ Cached target for fast reconnection');
    } else {
      console.log('❌ No aesthetic.computer page found to cache');
      process.exit(1);
    }
  },

  async test() {
    console.log('🧪 Testing CDP connection...\n');
    
    await withCDP(async (cdp) => {
      // Test basic eval
      const result = await cdp.eval('2 + 2');
      console.log(`✓ Basic eval: 2 + 2 = ${result}`);
      
      // Test page info
      const info = await cdp.getPageInfo();
      console.log(`✓ Page info: ${info.title}`);
      
      // Test performance API access
      const perfTest = await cdp.eval(`
        performance.getEntriesByType('resource').length
      `);
      console.log(`✓ Performance API: ${perfTest} resource entries`);
      
      // Test bios access (if available)
      if (info.hasACBios) {
        const biosTest = await cdp.eval('typeof window.bios');
        console.log(`✓ AC Bios: ${biosTest}`);
      }
      
      console.log('\n✅ All tests passed!');
    }, { verbose: true });
  },

  help() {
    console.log(`
CDP CLI - Chrome DevTools Protocol utilities for Aesthetic Computer

Usage: node cdp-cli.mjs <command> [args]

Commands:
  list                  List all available CDP targets
  find <pattern>        Find a specific target by URL pattern
  connect [url]         Test connection to a target (default: prompt)
  eval <expression>     Evaluate JavaScript in the page
  cache                 Cache current aesthetic.computer page for fast access
  test                  Run connection tests
  help                  Show this help message

Examples:
  node cdp-cli.mjs list
  node cdp-cli.mjs find prompt
  node cdp-cli.mjs connect
  node cdp-cli.mjs eval "document.title"
  node cdp-cli.mjs cache
  node cdp-cli.mjs test
`);
  }
};

// Run command
const [,, command, ...args] = process.argv;

if (!command || !commands[command]) {
  commands.help();
  process.exit(command ? 1 : 0);
}

try {
  await commands[command](...args);
} catch (err) {
  console.error(`\n❌ Error: ${err.message}`);
  if (err.stack && process.env.DEBUG) {
    console.error(err.stack);
  }
  process.exit(1);
}
