#!/usr/bin/env node
// KidLisp utility commands - quick actions via CDP
import http from 'http';
import WebSocket from 'ws';

const CDP_HOST = '172.17.0.1';
const CDP_PORT = 9224;

async function getTargets() {
  return new Promise((resolve, reject) => {
    http.get({
      hostname: CDP_HOST,
      port: CDP_PORT,
      path: '/json'
      // Note: Don't set Host header - it causes incorrect WebSocket URLs
    }, (res) => {
      let data = '';
      res.on('data', chunk => data += chunk);
      res.on('end', () => resolve(JSON.parse(data)));
    }).on('error', reject);
  });
}

async function connectToKidLisp() {
  const targets = await getTargets();
  const kidlisp = targets.find(t => t.url && t.url.includes('kidlisp.com'));
  
  if (!kidlisp) {
    throw new Error('KidLisp window not found. Open it first with: artery kidlisp');
  }
  
  let wsUrl = kidlisp.webSocketDebuggerUrl;
  // Fix localhost if needed (some CDP configs return localhost instead of the actual host)
  if (wsUrl.includes('localhost')) {
    wsUrl = wsUrl.replace(/ws:\/\/localhost:(\d+)/, `ws://${CDP_HOST}:$1`);
  }
  
  const ws = new WebSocket(wsUrl);
  
  await new Promise((resolve, reject) => {
    ws.on('open', resolve);
    ws.on('error', reject);
    setTimeout(() => reject(new Error('Connection timeout')), 5000);
  });
  
  return ws;
}

let messageIdCounter = 100000;

async function evaluate(ws, expression) {
  return new Promise((resolve, reject) => {
    const id = messageIdCounter++;
    let timer;
    
    const handler = (data) => {
      const msg = JSON.parse(data);
      if (msg.id === id) {
        clearTimeout(timer);
        ws.off('message', handler);
        if (msg.result?.exceptionDetails) {
          reject(new Error(msg.result.exceptionDetails.exception?.description || 'Error'));
        } else {
          resolve(msg.result?.result?.value);
        }
      }
    };
    
    ws.on('message', handler);
    ws.send(JSON.stringify({
      id,
      method: 'Runtime.evaluate',
      params: { expression, returnByValue: true }
    }));
    
    timer = setTimeout(() => {
      ws.off('message', handler);
      reject(new Error('Timeout waiting for evaluation result'));
    }, 10000);
  });
}

// Commands
const commands = {
  async 'close-tabs'() {
    const ws = await connectToKidLisp();
    const result = await evaluate(ws, `
      (function() {
        const tabs = document.querySelectorAll('.tab-close');
        const count = tabs.length;
        tabs.forEach(btn => btn.click());
        return { closedTabs: count };
      })()
    `);
    console.log(`✓ Closed ${result.closedTabs} tab(s)`);
    ws.close();
  },
  
  async 'list-tabs'() {
    const ws = await connectToKidLisp();
    const tabs = await evaluate(ws, `
      Array.from(document.querySelectorAll('.tab')).map(tab => ({
        name: tab.querySelector('.tab-name')?.textContent || 'unnamed',
        active: tab.classList.contains('active')
      }))
    `);
    if (tabs.length === 0) {
      console.log('No tabs open');
    } else {
      console.log('Open tabs:');
      tabs.forEach(t => console.log(`  ${t.active ? '→' : ' '} ${t.name}`));
    }
    ws.close();
  },
  
  async clear() {
    const ws = await connectToKidLisp();
    await evaluate(ws, `monaco.editor.getEditors()[0].setValue('')`);
    console.log('✓ Editor cleared');
    ws.close();
  },
  
  async stop() {
    const ws = await connectToKidLisp();
    await evaluate(ws, `document.getElementById('stop-button')?.click()`);
    console.log('✓ Stopped');
    ws.close();
  },
  
  async play() {
    const ws = await connectToKidLisp();
    await evaluate(ws, `document.getElementById('send-button')?.click()`);
    console.log('✓ Playing');
    ws.close();
  },
  
  async 'clear-console'() {
    const ws = await connectToKidLisp();
    await evaluate(ws, `document.getElementById('console-output').innerHTML = ''`);
    console.log('✓ Console cleared');
    ws.close();
  },
  
  async theme() {
    const ws = await connectToKidLisp();
    await evaluate(ws, `document.getElementById('theme-toggle')?.click()`);
    const theme = await evaluate(ws, `document.documentElement.getAttribute('data-theme')`);
    console.log(`✓ Theme: ${theme}`);
    ws.close();
  },
  
  async help() {
    console.log('KidLisp Utilities');
    console.log('');
    console.log('Usage: node kidlisp-util.mjs <command>');
    console.log('');
    console.log('Commands:');
    console.log('  close-tabs    Close all open tabs');
    console.log('  list-tabs     List open tabs');
    console.log('  clear         Clear the editor');
    console.log('  stop          Stop playback');
    console.log('  play          Start playback');
    console.log('  clear-console Clear the console');
    console.log('  theme         Toggle theme');
    console.log('  help          Show this help');
  }
};

// Main
const cmd = process.argv[2] || 'help';
if (commands[cmd]) {
  commands[cmd]().catch(e => {
    console.error('Error:', e.message);
    process.exit(1);
  });
} else {
  console.error(`Unknown command: ${cmd}`);
  commands.help();
  process.exit(1);
}
