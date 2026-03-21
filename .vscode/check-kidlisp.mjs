import WebSocket from 'ws';
import http from 'http';

// Get kidlisp target
const targets = await new Promise((resolve, reject) => {
  http.get({
    hostname: 'host.docker.internal',
    port: 9222,
    path: '/json',
    headers: { Host: 'localhost' }
  }, (res) => {
    let data = '';
    res.on('data', chunk => data += chunk);
    res.on('end', () => resolve(JSON.parse(data)));
  }).on('error', reject);
});

const kidlispTarget = targets.find(t => t.url?.includes('kidlisp.com'));
if (!kidlispTarget) {
  console.error('Could not find kidlisp.com target');
  process.exit(1);
}

console.log('Found kidlisp.com target:', kidlispTarget.id);

const wsUrl = kidlispTarget.webSocketDebuggerUrl.replace('localhost', 'host.docker.internal:9222').replace(':9222:9222', ':9222');
console.log('Connecting to:', wsUrl);

const ws = new WebSocket(wsUrl, { headers: { Host: 'localhost' } });

ws.on('open', () => {
  console.log('Connected to kidlisp.com');
  
  // Enable console
  ws.send(JSON.stringify({ id: 1, method: 'Runtime.enable' }));
  ws.send(JSON.stringify({ id: 2, method: 'Console.enable' }));
  
  // Check checkerboard state
  ws.send(JSON.stringify({
    id: 3,
    method: 'Runtime.evaluate',
    params: {
      expression: `
        (function() {
          const checkerboard = document.getElementById('mobile-checkerboard');
          const editorPanel = document.getElementById('editor-panel');
          const mainSplit = document.getElementById('main-split');
          
          return {
            checkerboard: {
              exists: !!checkerboard,
              display: checkerboard?.style.display,
              computedDisplay: checkerboard ? getComputedStyle(checkerboard).display : null,
              height: checkerboard?.offsetHeight
            },
            editorPanel: {
              exists: !!editorPanel,
              collapsed: editorPanel?.dataset?.collapsed,
              height: editorPanel?.offsetHeight
            },
            mainSplit: {
              exists: !!mainSplit,
              childCount: mainSplit?.children?.length
            },
            windowWidth: window.innerWidth,
            windowHeight: window.innerHeight,
            isMobile: window.innerWidth <= 768,
            updateCheckerboardExists: typeof window.updateCheckerboard === 'function',
            bodyHTML: document.body?.innerHTML?.substring(0, 500)
          };
        })()
      `,
      returnByValue: true
    }
  }));
});

ws.on('message', (data) => {
  const msg = JSON.parse(data);
  if (msg.id === 3) {
    console.log('\n=== KIDLISP.COM STATE ===');
    console.log(JSON.stringify(msg.result?.result?.value, null, 2));
    ws.close();
    process.exit(0);
  }
});

ws.on('error', (e) => {
  console.error('Error:', e.message);
  process.exit(1);
});
