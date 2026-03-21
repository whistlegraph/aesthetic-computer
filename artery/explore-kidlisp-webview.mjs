#!/usr/bin/env node
// 🔍 Explore KidLisp Webview Structure
// This script helps discover how to access the KidLisp.com webview through CDP

import WebSocket from 'ws';
import http from 'http';

const RESET = '\x1b[0m';
const BOLD = '\x1b[1m';
const CYAN = '\x1b[36m';
const YELLOW = '\x1b[33m';
const GREEN = '\x1b[32m';
const MAGENTA = '\x1b[35m';

const log = (msg) => console.log(msg);
const heading = (msg) => console.log(`\n${MAGENTA}${BOLD}═══ ${msg} ═══${RESET}\n`);
const info = (msg) => console.log(`${CYAN}→${RESET} ${msg}`);
const success = (msg) => console.log(`${GREEN}✓${RESET} ${msg}`);
const data = (label, value) => console.log(`${YELLOW}${label}:${RESET}`, typeof value === 'object' ? JSON.stringify(value, null, 2) : value);

async function findWorkingCDPHost() {
  const candidates = [];
  
  // Check if we're in a container
  const inContainer = process.env.REMOTE_CONTAINERS === 'true' || process.env.CODESPACES === 'true';
  
  if (!inContainer) {
    // Not in container - try localhost
    for (const port of [9333, 9222]) {
      candidates.push({ host: 'localhost', port });
    }
  } else {
    // In container - try multiple hosts
    candidates.push({ host: 'host.docker.internal', port: 9333 });
    candidates.push({ host: 'host.docker.internal', port: 9222 });
    candidates.push({ host: 'localhost', port: 9333 });
    candidates.push({ host: 'localhost', port: 9222 });
    candidates.push({ host: '172.17.0.1', port: 9224 });
    candidates.push({ host: '172.17.0.1', port: 9223 });
    candidates.push({ host: '172.17.0.1', port: 9222 });
  }
  
  for (const { host, port } of candidates) {
    try {
      const works = await new Promise((resolve) => {
        const req = http.get({
          hostname: host,
          port: port,
          path: '/json',
          timeout: 1000,
          headers: { 'Host': 'localhost' }
        }, (res) => {
          let data = '';
          res.on('data', (chunk) => data += chunk);
          res.on('end', () => resolve(data.length > 0));
        });
        req.on('error', () => resolve(false));
        req.on('timeout', () => { req.destroy(); resolve(false); });
      });
      if (works) return { host, port };
    } catch (e) {}
  }
  return candidates[0] || { host: 'localhost', port: 9333 };
}

async function main() {
  heading('KidLisp Webview Explorer');
  
  const { host, port } = await findWorkingCDPHost();
  info(`Connecting to CDP at ${host}:${port}`);
  
  // Get all targets
  const targetsJson = await new Promise((resolve, reject) => {
    const req = http.get({
      hostname: host,
      port: port,
      path: '/json',
      headers: { 'Host': 'localhost' }
    }, (res) => {
      let data = '';
      res.on('data', chunk => data += chunk);
      res.on('end', () => resolve(JSON.parse(data)));
    });
    req.on('error', reject);
  });
  
  heading('All CDP Targets');
  for (const target of targetsJson) {
    log(`${CYAN}${target.type}${RESET}: ${target.title || target.url?.substring(0, 80)}`);
  }
  
  // Look for the kidlisp.com iframe target specifically
  const kidlispTarget = targetsJson.find(t => 
    t.type === 'iframe' && t.url && t.url.includes('kidlisp.com')
  );
  
  if (kidlispTarget) {
    heading('Found KidLisp.com iframe target!');
    data('URL', kidlispTarget.url);
    data('ID', kidlispTarget.id);
    data('WebSocket URL', kidlispTarget.webSocketDebuggerUrl);
    
    // Connect directly to the kidlisp iframe
    let kidlispWsUrl = kidlispTarget.webSocketDebuggerUrl;
    if (kidlispWsUrl.includes('localhost')) {
      kidlispWsUrl = kidlispWsUrl.replace(/localhost(:\d+)?/, `${host}:${port}`);
    }
    
    info('Connecting directly to KidLisp iframe...');
    const kidlispWs = new WebSocket(kidlispWsUrl);
    
    await new Promise((resolve, reject) => {
      kidlispWs.on('open', resolve);
      kidlispWs.on('error', reject);
      setTimeout(() => reject(new Error('Timeout')), 5000);
    });
    
    success('Connected to KidLisp iframe!');
    
    let kidlispMsgId = 1;
    const kidlispPending = new Map();
    
    kidlispWs.on('message', (data) => {
      const msg = JSON.parse(data);
      if (msg.id && kidlispPending.has(msg.id)) {
        kidlispPending.get(msg.id)(msg);
        kidlispPending.delete(msg.id);
      }
    });
    
    const evalKidlisp = (expr) => new Promise((resolve, reject) => {
      const id = kidlispMsgId++;
      const timer = setTimeout(() => reject(new Error('Timeout')), 10000);
      kidlispPending.set(id, (msg) => {
        clearTimeout(timer);
        resolve(msg.result?.result?.value);
      });
      kidlispWs.send(JSON.stringify({
        id,
        method: 'Runtime.evaluate',
        params: { expression: expr, returnByValue: true }
      }));
    });
    
    // Now we can interact directly with the kidlisp.com page!
    heading('Exploring KidLisp.com page directly');
    
    info('Checking for Monaco editor...');
    const hasEditor = await evalKidlisp(`typeof editor !== 'undefined' && editor !== null`);
    data('Monaco editor available', hasEditor);
    
    if (hasEditor) {
      info('Getting current editor code...');
      const code = await evalKidlisp(`editor.getValue()`);
      data('Current code', code?.substring(0, 200) + (code?.length > 200 ? '...' : ''));
      
      info('Checking playback state...');
      const state = await evalKidlisp(`({ isPlaying, isPaused, pieceReady })`);
      data('Playback state', state);
    }
    
    info('Checking UI elements...');
    const uiCheck = await evalKidlisp(`({
      sendButton: !!document.getElementById('send-button'),
      stopButton: !!document.getElementById('stop-button'),
      clearButton: !!document.getElementById('clear-button'),
      consoleOutput: !!document.getElementById('console-output'),
      previewIframe: !!document.getElementById('preview-iframe'),
      themeToggle: !!document.getElementById('theme-toggle')
    })`);
    data('UI elements', uiCheck);
    
    info('Testing button click...');
    const clickResult = await evalKidlisp(`
      (function() {
        const btn = document.getElementById('send-button');
        if (btn) {
          return { found: true, disabled: btn.disabled, classes: btn.className };
        }
        return { found: false };
      })()
    `);
    data('Send button', clickResult);
    
    // Test setting code
    info('Testing code injection...');
    const setCodeResult = await evalKidlisp(`
      (function() {
        if (typeof editor !== 'undefined') {
          const oldCode = editor.getValue();
          editor.setValue('(ink red)\\n(box 50 50 100)');
          const newCode = editor.getValue();
          return { success: true, oldLength: oldCode.length, newLength: newCode.length };
        }
        return { success: false, error: 'No editor' };
      })()
    `);
    data('Set code result', setCodeResult);
    
    // Restore original code
    await evalKidlisp(`
      (function() {
        if (typeof editor !== 'undefined') {
          editor.setValue('(ink lime)\\n(box)');
        }
      })()
    `);
    
    kidlispWs.close();
  }
  
  // Find workbench
  const workbenchTarget = targetsJson.find(t => 
    t.type === 'page' && t.url && t.url.includes('workbench.html')
  );
  
  if (!workbenchTarget) {
    console.error('Could not find workbench');
    process.exit(1);
  }
  
  let wsUrl = workbenchTarget.webSocketDebuggerUrl;
  if (wsUrl.includes('localhost')) {
    wsUrl = wsUrl.replace(/localhost(:\d+)?/, `${host}:${port}`);
  }
  
  const ws = new WebSocket(wsUrl);
  let msgId = 1;
  const pending = new Map();
  
  ws.on('message', (data) => {
    const msg = JSON.parse(data);
    if (msg.id && pending.has(msg.id)) {
      pending.get(msg.id)(msg);
      pending.delete(msg.id);
    }
  });
  
  await new Promise((resolve, reject) => {
    ws.on('open', resolve);
    ws.on('error', reject);
    setTimeout(() => reject(new Error('Timeout')), 5000);
  });
  
  success('Connected to workbench');
  
  const evaluate = (expr) => new Promise((resolve, reject) => {
    const id = msgId++;
    const timer = setTimeout(() => reject(new Error('Timeout')), 10000);
    pending.set(id, (msg) => {
      clearTimeout(timer);
      resolve(msg.result?.result?.value);
    });
    ws.send(JSON.stringify({
      id,
      method: 'Runtime.evaluate',
      params: { expression: expr, returnByValue: true }
    }));
  });
  
  // Exploration queries
  heading('Looking for KidLisp webview...');
  
  // 1. Check all iframes
  info('Checking iframes...');
  const iframes = await evaluate(`
    (function() {
      const frames = Array.from(document.querySelectorAll('iframe'));
      return frames.map(f => ({
        id: f.id,
        className: f.className,
        src: f.src?.substring(0, 100),
        title: f.title,
        name: f.name
      }));
    })()
  `);
  data('Iframes found', iframes);
  
  // 2. Check webviews
  info('Checking webview elements...');
  const webviews = await evaluate(`
    (function() {
      const wvs = Array.from(document.querySelectorAll('webview'));
      return wvs.map(w => ({
        id: w.id,
        className: w.className,
        src: w.src?.substring(0, 100),
        partition: w.partition
      }));
    })()
  `);
  data('Webviews found', webviews);
  
  // 3. Look for editor tabs / panels with KidLisp
  info('Checking for KidLisp in tabs/panels...');
  const kidlispElements = await evaluate(`
    (function() {
      const results = [];
      
      // Look for tabs
      const tabs = Array.from(document.querySelectorAll('.tab'));
      for (const tab of tabs) {
        if (tab.textContent?.includes('KidLisp')) {
          results.push({ type: 'tab', text: tab.textContent, className: tab.className });
        }
      }
      
      // Look for panel headers
      const headers = Array.from(document.querySelectorAll('.pane-header, .title'));
      for (const h of headers) {
        if (h.textContent?.includes('KidLisp')) {
          results.push({ type: 'header', text: h.textContent, className: h.className });
        }
      }
      
      // Look for webview containers
      const containers = Array.from(document.querySelectorAll('[data-webview-id], .webview-container'));
      for (const c of containers) {
        results.push({ 
          type: 'webview-container', 
          id: c.getAttribute('data-webview-id') || c.id,
          className: c.className 
        });
      }
      
      return results;
    })()
  `);
  data('KidLisp elements', kidlispElements);
  
  // 4. Check editor group structure
  info('Checking editor groups...');
  const editorGroups = await evaluate(`
    (function() {
      const groups = Array.from(document.querySelectorAll('.editor-group-container'));
      return groups.map(g => {
        const title = g.querySelector('.title')?.textContent;
        const tabs = Array.from(g.querySelectorAll('.tab')).map(t => t.textContent?.trim());
        const iframe = g.querySelector('iframe');
        return {
          title,
          tabs,
          hasIframe: !!iframe,
          iframeSrc: iframe?.src?.substring(0, 100)
        };
      });
    })()
  `);
  data('Editor groups', editorGroups);
  
  // 5. Try to find the specific webview iframe for KidLisp
  info('Searching for KidLisp webview iframe specifically...');
  const kidlispWebview = await evaluate(`
    (function() {
      // VS Code uses nested iframes for webviews
      // First, find the webview container
      const containers = Array.from(document.querySelectorAll('.webview'));
      
      for (const container of containers) {
        // Check if this container's tab/title mentions KidLisp
        const parent = container.closest('.editor-instance, .editor-container');
        const tab = parent?.querySelector('.tab.active');
        if (tab?.textContent?.includes('KidLisp')) {
          // Found it! Get iframe details
          const iframe = container.querySelector('iframe');
          if (iframe) {
            return {
              found: true,
              containerId: container.id,
              containerClass: container.className,
              iframeId: iframe.id,
              iframeSrc: iframe.src,
              iframeName: iframe.name
            };
          }
        }
      }
      
      // Alternative: look through all iframes
      const allIframes = Array.from(document.querySelectorAll('iframe'));
      for (const iframe of allIframes) {
        if (iframe.src?.includes('kidlisp') || iframe.name?.includes('kidlisp')) {
          return {
            found: true,
            method: 'direct-iframe-search',
            iframeId: iframe.id,
            iframeSrc: iframe.src,
            iframeName: iframe.name,
            className: iframe.className
          };
        }
      }
      
      return { found: false };
    })()
  `);
  data('KidLisp webview', kidlispWebview);
  
  // 6. Try to access the webview's contentWindow
  info('Trying to access webview content...');
  const contentAccess = await evaluate(`
    (function() {
      const allIframes = Array.from(document.querySelectorAll('iframe'));
      const results = [];
      
      for (const iframe of allIframes) {
        try {
          // Try to access contentWindow
          const hasAccess = !!iframe.contentWindow;
          const canPostMessage = typeof iframe.contentWindow?.postMessage === 'function';
          
          results.push({
            src: iframe.src?.substring(0, 60),
            hasAccess,
            canPostMessage,
            id: iframe.id,
            name: iframe.name
          });
        } catch (e) {
          results.push({
            src: iframe.src?.substring(0, 60),
            error: e.message
          });
        }
      }
      
      return results;
    })()
  `);
  data('Content access results', contentAccess);
  
  // 7. Look for VS Code's internal webview tracking
  info('Checking VS Code webview API...');
  const webviewApi = await evaluate(`
    (function() {
      // Check if we can access webview instances through VS Code internals
      if (typeof vscode !== 'undefined') {
        return { hasVscode: true };
      }
      
      // Check for Monaco internals
      if (typeof monaco !== 'undefined') {
        return { hasMonaco: true };
      }
      
      return { hasVscode: false, hasMonaco: false };
    })()
  `);
  data('API access', webviewApi);
  
  // 8. Try posting a message to find the right iframe
  info('Broadcasting test message to all iframes...');
  const broadcast = await evaluate(`
    (function() {
      const allIframes = Array.from(document.querySelectorAll('iframe'));
      let sent = 0;
      
      for (const iframe of allIframes) {
        try {
          iframe.contentWindow?.postMessage({ type: 'test:ping' }, '*');
          sent++;
        } catch (e) {}
      }
      
      return { sent, total: allIframes.length };
    })()
  `);
  data('Broadcast result', broadcast);
  
  heading('Summary');
  log('Use these findings to build the test harness.');
  log('The key is finding the correct iframe and being able to postMessage to it.');
  
  ws.close();
  process.exit(0);
}

main().catch(err => {
  console.error('Error:', err);
  process.exit(1);
});
