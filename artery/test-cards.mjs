#!/usr/bin/env node
// Test card controls via CDP
import http from 'http';
import WebSocket from 'ws';

const host = 'host.docker.internal';
const port = 9222;

async function getTargets() {
  return new Promise((resolve, reject) => {
    const req = http.get({
      hostname: host, port, path: '/json',
      headers: { Host: 'localhost' }
    }, (res) => {
      let data = '';
      res.on('data', c => data += c);
      res.on('end', () => resolve(JSON.parse(data)));
    });
    req.on('error', reject);
  });
}

async function main() {
  const targets = await getTargets();
  const kidlispTarget = targets.find(t => t.type === 'iframe' && t.url?.includes('kidlisp.com'));
  
  if (!kidlispTarget) {
    console.log('❌ KidLisp.com iframe not found');
    process.exit(1);
  }
  
  let wsUrl = kidlispTarget.webSocketDebuggerUrl.replace(/localhost(:\d+)?/, host + ':' + port);
  const ws = new WebSocket(wsUrl);
  await new Promise((resolve) => ws.on('open', resolve));
  
  let msgId = 1;
  const send = (method, params = {}) => {
    const id = msgId++;
    return new Promise((resolve) => {
      ws.send(JSON.stringify({ id, method, params }));
      const handler = (data) => {
        const msg = JSON.parse(data);
        if (msg.id === id) { ws.off('message', handler); resolve(msg.result); }
      };
      ws.on('message', handler);
      setTimeout(() => { ws.off('message', handler); resolve({}); }, 3000);
    });
  };
  
  const evalJS = async (expr) => {
    const r = await send('Runtime.evaluate', { expression: expr, returnByValue: true });
    return r.result?.value;
  };
  
  await send('Runtime.enable');
  
  console.log('\n🔍 Card State Diagnostic\n');
  
  // Check current state
  let state = await evalJS(`JSON.stringify({
    spreadView: document.querySelector('.book-stack')?.classList.contains('spread-view'),
    cardCount: document.querySelectorAll('.book-frame').length,
    topCard: document.querySelector('.book-frame')?.dataset?.cardName
  })`);
  console.log('Current state:', state);
  
  // Force close spread view if open
  const isSpread = await evalJS(`document.querySelector('.book-stack')?.classList.contains('spread-view')`);
  if (isSpread) {
    console.log('\n→ Closing spread view...');
    await evalJS(`document.getElementById('card-spread-btn').click()`);
    await new Promise(r => setTimeout(r, 1000)); // Wait longer for animation
  }
  
  // Check state again
  state = await evalJS(`JSON.stringify({
    spreadView: document.querySelector('.book-stack')?.classList.contains('spread-view'),
    topCard: document.querySelector('.book-frame')?.dataset?.cardName
  })`);
  console.log('State after toggle:', state);
  
  // Test navigation
  console.log('\n🧭 Navigation Test (stack view):');
  
  let topCard = await evalJS(`document.querySelector('.book-frame')?.dataset?.cardName`);
  console.log('  Before:', topCard);
  
  // Click Next multiple times with delays
  for (let i = 0; i < 3; i++) {
    await evalJS(`document.getElementById('card-next-btn').click()`);
    await new Promise(r => setTimeout(r, 400));
    topCard = await evalJS(`document.querySelector('.book-frame')?.dataset?.cardName`);
    console.log(`  After Next #${i+1}:`, topCard);
  }
  
  // Click Prev
  await evalJS(`document.getElementById('card-prev-btn').click()`);
  await new Promise(r => setTimeout(r, 300));
  topCard = await evalJS(`document.querySelector('.book-frame')?.dataset?.cardName`);
  console.log('  After Prev:', topCard);
  
  ws.close();
  console.log('\n✅ Done');
}

main().catch(e => console.error('❌', e.message));
