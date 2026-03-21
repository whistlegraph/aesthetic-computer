#!/usr/bin/env node
// Quick test to find how to access Monaco editor in kidlisp.com
import WebSocket from 'ws';
import http from 'http';

async function main() {
  const targets = await new Promise((resolve, reject) => {
    const req = http.get({
      hostname: '172.17.0.1', port: 9224, path: '/json',
      headers: { 'Host': 'localhost' }
    }, (res) => {
      let data = '';
      res.on('data', chunk => data += chunk);
      res.on('end', () => resolve(JSON.parse(data)));
    });
    req.on('error', reject);
  });
  
  const kidlispTarget = targets.find(t => t.url?.includes('kidlisp.com'));
  if (!kidlispTarget) { console.log('No kidlisp target'); return; }
  
  console.log('Found:', kidlispTarget.url);
  
  let wsUrl = kidlispTarget.webSocketDebuggerUrl.replace(/localhost(:\d+)?/, '172.17.0.1:9224');
  const ws = new WebSocket(wsUrl);
  
  await new Promise(r => ws.on('open', r));
  console.log('Connected');
  
  let msgId = 1;
  const eval_ = (expr) => new Promise((resolve) => {
    const id = msgId++;
    const handler = (data) => {
      const msg = JSON.parse(data);
      if (msg.id === id) { ws.off('message', handler); resolve(msg.result?.result?.value); }
    };
    ws.on('message', handler);
    ws.send(JSON.stringify({ id, method: 'Runtime.evaluate', params: { expression: expr, returnByValue: true } }));
  });
  
  // Check what we can access
  console.log('\n--- Checking available APIs ---');
  
  const hasMonaco = await eval_(`typeof monaco !== 'undefined'`);
  console.log('Has monaco:', hasMonaco);
  
  const hasRequire = await eval_(`typeof require !== 'undefined'`);
  console.log('Has require:', hasRequire);
  
  const hasEditor = await eval_(`typeof editor !== 'undefined'`);
  console.log('Has editor global:', hasEditor);
  
  // Check Monaco instances
  if (hasMonaco) {
    const monacoEditors = await eval_(`monaco.editor.getEditors().length`);
    console.log('Monaco editor instances:', monacoEditors);
    
    const editorModel = await eval_(`
      (function() {
        const editors = monaco.editor.getEditors();
        if (editors.length > 0) {
          return editors[0].getValue();
        }
        return null;
      })()
    `);
    console.log('First editor content:', editorModel?.substring(0, 100));
  }
  
  // Check for window.editor
  const windowProps = await eval_(`
    Object.getOwnPropertyNames(window).filter(k => 
      k.toLowerCase().includes('editor') || k.toLowerCase().includes('monaco')
    )
  `);
  console.log('Window editor/monaco props:', windowProps);
  
  // Check DOM for clues
  const monacoEls = await eval_(`document.querySelectorAll('.monaco-editor').length`);
  console.log('Monaco editor elements:', monacoEls);
  
  // Try to find editor in the script scope
  const scriptCheck = await eval_(`
    (function() {
      // The editor variable is declared in a script tag, let's see if we can find it
      const scripts = document.querySelectorAll('script');
      for (const s of scripts) {
        if (s.textContent?.includes('editor = monaco.editor.create')) {
          return { found: true, scriptLength: s.textContent.length };
        }
      }
      return { found: false };
    })()
  `);
  console.log('Script with editor:', scriptCheck);
  
  // Since Monaco is loaded, try getting the editor through monaco.editor API
  const tryGetEditor = await eval_(`
    (function() {
      if (typeof monaco === 'undefined') return { error: 'no monaco' };
      
      const models = monaco.editor.getModels();
      const editors = monaco.editor.getEditors();
      
      return {
        modelCount: models.length,
        editorCount: editors.length,
        firstModelContent: models[0]?.getValue()?.substring(0, 100),
        editorIsFunction: typeof editors[0]?.getValue === 'function'
      };
    })()
  `);
  console.log('Monaco API check:', tryGetEditor);
  
  // If we have editors, let's test setting value
  if (tryGetEditor?.editorCount > 0) {
    console.log('\n--- Testing editor manipulation ---');
    
    const setValue = await eval_(`
      (function() {
        const editors = monaco.editor.getEditors();
        if (editors.length === 0) return { error: 'no editors' };
        
        const editor = editors[0];
        const oldValue = editor.getValue();
        editor.setValue('(ink magenta)\\n(circle 100 100 50)');
        const newValue = editor.getValue();
        
        return { 
          success: true, 
          oldLength: oldValue.length, 
          newLength: newValue.length,
          newValue: newValue
        };
      })()
    `);
    console.log('Set value result:', setValue);
  }
  
  ws.close();
  console.log('\nDone');
}

main().catch(console.error);
