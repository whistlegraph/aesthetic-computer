#!/usr/bin/env node
// Quick script to inspect cards/book elements in KidLisp.com via CDP

import WebSocket from 'ws';
import http from 'http';

async function inspect() {
  console.log('Finding CDP host...');
  
  // Find working CDP host
  const candidates = [
    { host: 'host.docker.internal', port: 9333 },
    { host: '172.17.0.1', port: 9333 },
    { host: 'localhost', port: 9333 }
  ];
  
  let cdpHost, cdpPort;
  for (const { host, port } of candidates) {
    try {
      const works = await new Promise((resolve) => {
        const req = http.get({
          hostname: host, port, path: '/json', timeout: 1000,
          headers: { 'Host': 'localhost' }
        }, (res) => {
          let data = '';
          res.on('data', (chunk) => data += chunk);
          res.on('end', () => resolve(data.length > 0));
        });
        req.on('error', () => resolve(false));
        req.on('timeout', () => { req.destroy(); resolve(false); });
      });
      if (works) { cdpHost = host; cdpPort = port; break; }
    } catch (e) {}
  }
  
  if (!cdpHost) {
    console.error('CDP not available - is the tunnel running?');
    console.log('Try: ac-cdp-tunnel');
    process.exit(1);
  }
  
  console.log('CDP at:', cdpHost + ':' + cdpPort);
  
  const targetsJson = await new Promise((resolve, reject) => {
    const req = http.get({
      hostname: cdpHost, port: cdpPort, path: '/json',
      headers: { 'Host': 'localhost' }
    }, (res) => {
      let data = '';
      res.on('data', chunk => data += chunk);
      res.on('end', () => resolve(JSON.parse(data)));
    });
    req.on('error', reject);
  });
  
  // Find kidlisp.com iframe
  const kidlispTarget = targetsJson.find(t => 
    t.type === 'iframe' && t.url && t.url.includes('kidlisp.com')
  );
  
  if (!kidlispTarget) {
    console.error('KidLisp.com not found in targets');
    console.log('Available targets:', targetsJson.map(t => ({type: t.type, url: t.url?.substring(0, 60)})));
    process.exit(1);
  }
  
  console.log('Found KidLisp target:', kidlispTarget.url.substring(0, 60));
  
  let wsUrl = kidlispTarget.webSocketDebuggerUrl;
  if (wsUrl.includes('localhost')) {
    wsUrl = wsUrl.replace(/localhost(:\d+)?/, `${cdpHost}:${cdpPort}`);
  }
  
  const ws = new WebSocket(wsUrl);
  
  await new Promise((resolve, reject) => {
    ws.on('open', resolve);
    ws.on('error', reject);
    setTimeout(() => reject(new Error('Timeout')), 5000);
  });
  
  let msgId = 1;
  const evaluate = (expression) => new Promise((resolve, reject) => {
    const id = msgId++;
    const handler = (data) => {
      const msg = JSON.parse(data);
      if (msg.id === id) {
        ws.off('message', handler);
        if (msg.result?.exceptionDetails) {
          reject(new Error(msg.result.exceptionDetails.exception?.description || 'Eval error'));
        } else {
          resolve(msg.result?.result?.value);
        }
      }
    };
    ws.on('message', handler);
    ws.send(JSON.stringify({ id, method: 'Runtime.evaluate', params: { expression, returnByValue: true }}));
    setTimeout(() => reject(new Error('Timeout')), 10000);
  });
  
  // Inspect the book/cards elements
  const result = await evaluate(`
    (function() {
      const bookOverlay = document.querySelector('.book-overlay');
      const bookFrame = document.querySelector('.book-frame');
      const bookStack = document.querySelector('.book-stack');
      const bookPage = document.querySelector('.book-page');
      const bookPageInner = document.querySelector('.book-page-inner');
      
      return {
        bookOverlayVisible: bookOverlay ? window.getComputedStyle(bookOverlay).display : 'not found',
        bookOverlayRect: bookOverlay?.getBoundingClientRect(),
        bookFrameRect: bookFrame?.getBoundingClientRect(),
        bookStackRect: bookStack?.getBoundingClientRect(),
        bookPageRect: bookPage?.getBoundingClientRect(),
        bookPageInnerRect: bookPageInner?.getBoundingClientRect(),
        bookPageInnerTransform: bookPageInner ? window.getComputedStyle(bookPageInner).transform : null,
        bookOverlayClasses: bookOverlay?.className,
        bookPageCount: document.querySelectorAll('.book-page').length,
        windowInnerWidth: window.innerWidth,
        windowInnerHeight: window.innerHeight
      };
    })()
  `);
  
  console.log('\nBook/Cards inspection:');
  console.log(JSON.stringify(result, null, 2));
  
  ws.close();
  process.exit(0);
}

inspect().catch(e => { console.error(e); process.exit(1); });
