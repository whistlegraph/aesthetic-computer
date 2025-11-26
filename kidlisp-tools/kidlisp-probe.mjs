#!/usr/bin/env node
/**
 * KidLisp.com Probe - Inspect and control kidlisp.com in Simple Browser via CDP
 * 
 * Usage:
 *   node kidlisp-probe.mjs           # Show status
 *   node kidlisp-probe.mjs eval <js> # Evaluate JS in kidlisp.com
 *   node kidlisp-probe.mjs theme     # Toggle theme
 *   node kidlisp-probe.mjs clear     # Clear editor
 *   node kidlisp-probe.mjs set <code> # Set editor content
 */

import http from 'http';
import WebSocket from 'ws';

const CDP_HOST = process.env.REMOTE_CONTAINERS === 'true' ? 'host.docker.internal' : 'localhost';

const PINK = '\x1b[95m';
const CYAN = '\x1b[96m';
const GREEN = '\x1b[92m';
const YELLOW = '\x1b[93m';
const RED = '\x1b[91m';
const RESET = '\x1b[0m';
const BOLD = '\x1b[1m';

async function findKidlispTarget() {
  const targets = await new Promise((resolve, reject) => {
    http.get({
      hostname: CDP_HOST,
      port: 9222,
      path: '/json',
      headers: { 'Host': 'localhost' }
    }, (res) => {
      let data = '';
      res.on('data', (chunk) => data += chunk);
      res.on('end', () => resolve(JSON.parse(data)));
    }).on('error', reject);
  });
  
  return targets.find(t => t.url?.includes('kidlisp.com'));
}

async function connectToKidlisp() {
  const target = await findKidlispTarget();
  if (!target) {
    console.log(`${RED}❌ kidlisp.com not open in Simple Browser${RESET}`);
    console.log(`${YELLOW}Tip: Open it with: open_simple_browser https://localhost:8888/kidlisp.com${RESET}`);
    process.exit(1);
  }
  
  let wsUrl = target.webSocketDebuggerUrl;
  wsUrl = wsUrl.replace('localhost', `${CDP_HOST}:9222`).replace(':9222:9222', ':9222');
  
  const ws = new WebSocket(wsUrl);
  
  await new Promise((resolve, reject) => {
    ws.on('open', resolve);
    ws.on('error', reject);
    setTimeout(() => reject(new Error('Connection timeout')), 5000);
  });
  
  return ws;
}

async function evalInKidlisp(ws, expression) {
  return new Promise((resolve, reject) => {
    const id = Math.floor(Math.random() * 1000000);
    
    const handler = (data) => {
      const msg = JSON.parse(data);
      if (msg.id === id) {
        ws.off('message', handler);
        if (msg.error) {
          reject(new Error(msg.error.message));
        } else if (msg.result?.exceptionDetails) {
          reject(new Error(msg.result.exceptionDetails.text));
        } else {
          resolve(msg.result?.result?.value);
        }
      }
    };
    
    ws.on('message', handler);
    ws.send(JSON.stringify({
      id,
      method: 'Runtime.evaluate',
      params: {
        expression,
        returnByValue: true,
        awaitPromise: true
      }
    }));
    
    setTimeout(() => reject(new Error('Timeout')), 10000);
  });
}

async function showStatus(ws) {
  const info = await evalInKidlisp(ws, `({
    title: document.title,
    logoText: document.querySelector('.baby-colors')?.textContent,
    previewTitle: document.querySelector('#preview-title')?.textContent,
    consoleEntries: document.querySelectorAll('.console-entry').length,
    hasEditor: !!window.monaco,
    currentTheme: document.documentElement.getAttribute('data-theme') || 'auto',
    editorContent: window.monaco?.editor?.getModels()?.[0]?.getValue?.() || '(editor not loaded)',
    isPlaying: document.querySelector('#send-button')?.classList?.contains('playing') || false,
    connectivity: document.querySelector('.connectivity-dot')?.classList?.value || ''
  })`);
  
  console.log(`${CYAN}${BOLD}🌐 KidLisp.com Status${RESET}`);
  console.log('─'.repeat(50));
  console.log(`${PINK}📄 Title:${RESET}`, info.title);
  console.log(`${PINK}🎨 Logo:${RESET}`, info.logoText);
  console.log(`${PINK}📺 Preview:${RESET}`, info.previewTitle);
  console.log(`${PINK}📝 Console entries:${RESET}`, info.consoleEntries);
  console.log(`${PINK}⚡ Monaco loaded:${RESET}`, info.hasEditor ? `${GREEN}Yes${RESET}` : `${RED}No${RESET}`);
  console.log(`${PINK}🌗 Theme:${RESET}`, info.currentTheme);
  console.log(`${PINK}▶️  Playing:${RESET}`, info.isPlaying ? `${GREEN}Yes${RESET}` : 'No');
  console.log(`${PINK}🔗 Connectivity:${RESET}`, info.connectivity);
  console.log('');
  console.log(`${YELLOW}📋 Editor content:${RESET}`);
  console.log('─'.repeat(50));
  console.log(info.editorContent);
  console.log('─'.repeat(50));
}

async function toggleTheme(ws) {
  await evalInKidlisp(ws, `
    const toggle = document.getElementById('theme-toggle');
    toggle?.click();
    document.documentElement.getAttribute('data-theme') || 'toggled'
  `);
  console.log(`${GREEN}✅ Theme toggled${RESET}`);
}

async function clearEditor(ws) {
  await evalInKidlisp(ws, `
    const model = window.monaco?.editor?.getModels()?.[0];
    if (model) model.setValue('');
    'cleared'
  `);
  console.log(`${GREEN}✅ Editor cleared${RESET}`);
}

async function setEditorContent(ws, content) {
  await evalInKidlisp(ws, `
    const model = window.monaco?.editor?.getModels()?.[0];
    if (model) model.setValue(${JSON.stringify(content)});
    'set'
  `);
  console.log(`${GREEN}✅ Editor content set${RESET}`);
}

async function pressPlay(ws) {
  await evalInKidlisp(ws, `
    document.getElementById('send-button')?.click();
    'clicked'
  `);
  console.log(`${GREEN}✅ Play button pressed${RESET}`);
}

async function main() {
  const command = process.argv[2];
  const args = process.argv.slice(3);
  
  try {
    const ws = await connectToKidlisp();
    console.log(`${GREEN}🔌 Connected to kidlisp.com${RESET}`);
    
    switch (command) {
      case 'eval':
        const result = await evalInKidlisp(ws, args.join(' '));
        console.log(`${CYAN}Result:${RESET}`, result);
        break;
        
      case 'theme':
        await toggleTheme(ws);
        break;
        
      case 'clear':
        await clearEditor(ws);
        break;
        
      case 'set':
        await setEditorContent(ws, args.join(' '));
        break;
        
      case 'play':
        await pressPlay(ws);
        break;
        
      default:
        await showStatus(ws);
    }
    
    ws.close();
  } catch (error) {
    console.error(`${RED}❌ Error: ${error.message}${RESET}`);
    process.exit(1);
  }
}

main();
