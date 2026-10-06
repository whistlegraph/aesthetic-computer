// Exercises the real engine/bridge with deterministic native render events.
// The native runtime itself still needs a device preview test.
const assert = require('node:assert/strict');
const {resolve, extname} = require('node:path');
const {readFile} = require('node:fs/promises');
const http = require('node:http');
const puppeteer = require('puppeteer');
const root = resolve(__dirname, '../Resources/Web');
const base = 'export function paint({wipe}) {wipe("navy");}\n//' + 'complete source '.repeat(600) + '\n// final sentinel';
const edited = 'export function paint({wipe}) {wipe("pink");}';
const server = http.createServer(async (request, response) => {
  try {
    const path = resolve(root, '.' + new URL(request.url, 'http://local').pathname);
    if (!path.startsWith(root + '/')) throw Error('Bad path');
    const data = await readFile(path);
    response.setHeader('Content-Type', ({'.html': 'text/html', '.js': 'text/javascript', '.mjs': 'text/javascript', '.json': 'application/json'})[extname(path)] || 'application/octet-stream');
    response.end(data);
  } catch { response.writeHead(404); response.end(); }
});

(async () => {
  await new Promise(resolve => server.listen(0, '127.0.0.1', resolve));
  const browser = await puppeteer.launch({headless: true, executablePath: process.env.CHROME_PATH || '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome'});
  try {
    const page = await browser.newPage(), errors = [], requests = [];
    page.on('pageerror', error => errors.push(error.message));
    await page.setRequestInterception(true);
    page.on('request', request => {
      requests.push(request.url());
      if (request.url().startsWith('https://')) {
        request.respond({status: 200, contentType: 'text/html', body: '<body>Native preview fixture</body>'});
      } else request.continue();
    });
    await page.evaluateOnNewDocument(source => {
      window.__walkiewareNativeShell = true;
      window.__walkiewareDisableThread = true;
      window.__nativeMessages = [];
      window.__sourceRenderMode = 'paint';
      localStorage.setItem('walkieware-source', source);
      window.webkit = {messageHandlers: {walkie: {postMessage: message => {
        window.__nativeMessages.push(message);
        if (message.action !== 'render') return;
        const fail = window.__sourceRenderMode === 'runtime' && message.source.includes('runtimeFailure');
        setTimeout(async () => {
          const digest = await crypto.subtle.digest('SHA-256', new TextEncoder().encode(message.source));
          const sourceHash = [...new Uint8Array(digest)].map(byte => byte.toString(16).padStart(2, '0')).join('');
          const proof = {sourceHash, requestID: message.renderID};
          window.walkiewareEngineEvent({kind: 'previewEvent', event: {...proof, kind: 'painted'}});
          if (fail) setTimeout(() => window.walkiewareEngineEvent({kind: 'previewEvent', event: {...proof,
            kind: 'console', event: {level: 'error', message: 'Paint failure: runtimeFailure'}}}), 100);
        }, 20);
      }}}};
    }, base);
    await page.goto('http://127.0.0.1:' + server.address().port + '/index.html?walkie=1');
    await page.waitForFunction(() => !!window.walkiewareSourceEditor);
    await page.evaluate(() => window.walkiewareEngineEvent({kind: 'previewReady'}));
    const original = await page.evaluate(() => window.walkiewareSourceEditor.read());
    assert.equal(original.source, base);
    assert.ok(original.source.length > 6000);
    const saved = await page.evaluate(async ({document, source}) => {
      window.__editorDocument = document;
      return window.walkiewareSourceEditor.apply({...document, source});
    }, {document: original, source: edited});
    assert.equal(saved.version, 1);
    let state = await page.evaluate(() => ({ledger: JSON.parse(localStorage.getItem('walkieware-source-versions')),
      source: localStorage.getItem('walkieware-source'), busy: window.walkiewareIsBusy()}));
    assert.equal(state.ledger.head, 1);
    assert.equal(state.ledger.versions[1].request, 'Manual source edit');
    assert.equal(state.source, edited);
    assert.equal(state.busy, false);

    const staleError = await page.evaluate(async source => {
      try { await window.walkiewareSourceEditor.apply({...window.__editorDocument, source}); }
      catch (error) { return error.message; }
    }, edited + '\n// stale');
    assert.match(staleError, /selected version changed/);
    const syntaxError = await page.evaluate(async () => {
      try { await window.walkiewareSourceEditor.apply({...await window.walkiewareSourceEditor.read(), source: 'export function paint('}); }
      catch (error) { return error.message; }
    });
    assert.match(syntaxError, /complete JavaScript module/);
    const runtimeError = await page.evaluate(async () => {
      window.__sourceRenderMode = 'runtime';
      try { await window.walkiewareSourceEditor.apply({...await window.walkiewareSourceEditor.read(),
        source: 'export function paint({wipe}) {wipe("red"); runtimeFailure();}'}); }
      catch (error) { return error.message; }
    });
    assert.match(runtimeError, /runtimeFailure/);
    await page.waitForFunction(() => !window.walkiewareIsBusy());
    state = await page.evaluate(() => ({ledger: JSON.parse(localStorage.getItem('walkieware-source-versions')),
      source: localStorage.getItem('walkieware-source'), renders: window.__nativeMessages.filter(message => message.action === 'render')}));
    assert.equal(state.ledger.head, 1);
    assert.equal(state.ledger.versions.length, 2);
    assert.equal(state.source, edited);
    assert.equal(state.renders.at(-1).source, edited);
    const final = await page.evaluate(() => window.walkiewareSourceEditor.read());
    assert.equal(final.source, edited);
    assert.equal(final.sourceHash, saved.sourceHash);
    assert.equal(requests.filter(url => /easel-inference|easel-musical|openrouter|openai|anthropic/.test(url)).length, 0,
      'manual source edits require no sign-in, AI consent, provider requests or credit debit');
    assert.deepEqual(errors, []);
    console.log('PASS: full source bridge, unsigned local commit, stale/syntax rejection, delayed-runtime rollback and zero AI requests.');
  } finally { await browser.close(); server.close(); }
})().catch(error => { console.error(error); server.close(); process.exitCode = 1; });
