// Isolated offscreen Electron window. No PTY, login, or live Easel process.
const { app, BrowserWindow, ipcMain } = require('electron');
const { join, resolve } = require('node:path');
const fs = require('node:fs');
const assert = require('node:assert/strict');
const { localPreview } = require('../local-preview.cjs');
const root = resolve(__dirname, '..');
const cwd = process.env.EASEL_PREVIEW_SMOKE_DIR || '/tmp/easel-media-preview-smoke';
app.setPath('userData', join(cwd, 'electron-test'));
const delay = ms => new Promise(resolve => setTimeout(resolve, ms));
const inspect = `(()=>{const p=document.getElementById('media-preview'),c=p.querySelector('canvas');let hash=2166136261,colors=new Set();if(c){const bytes=c.getContext('2d').getImageData(0,0,c.width,c.height).data;for(let i=0;i<bytes.length;i+=4){colors.add([bytes[i],bytes[i+1],bytes[i+2]].join(','));for(let n=0;n<4;n++)hash=Math.imul(hash^bytes[i+n],16777619)>>>0;}}return {text:p.textContent,image:p.querySelector('img')?.naturalWidth,canvas:!!c,colors:colors.size,hash,frames:Number(c?.dataset.frames||0),status:c?.dataset.status,viewport:[document.getElementById('preview-viewport').clientWidth,document.getElementById('preview-viewport').clientHeight]}})()`;
app.whenReady().then(async () => {
  const win = new BrowserWindow({ show: false, width: 760, height: 540,
    webPreferences: { preload: join(root, 'preload.cjs'), contextIsolation: true,
      sandbox: true, nodeIntegration: false, webviewTag: true,
      backgroundThrottling: false, plugins: true, offscreen: true } });
  const errors = [];
  win.webContents.on('console-message', event => {
    if (event.level === 'error' || event.message?.includes('violates')) errors.push(event.message);
  });
  await win.loadFile(join(root, 'index.html'));
  await delay(400);
  const results = [];
  let openedPiece='';ipcMain.on('open-piece',(_event,url)=>{openedPiece=url;});
  assert.equal(await win.webContents.executeJavaScript("document.getElementById('artifact-shell').hidden"),true);
  await win.webContents.executeJavaScript("document.getElementById('artifact-shell').hidden=false");
  win.webContents.send('output','\x1b]777;easel-pointer:profile\x07');
  await delay(100);
  assert.equal(await win.webContents.executeJavaScript("getComputedStyle(document.querySelector('.xterm-screen')).cursor"),'pointer');
  win.webContents.send('output','\x1b]777;easel-pointer:\x07');
  await delay(100);
  assert.notEqual(await win.webContents.executeJavaScript("getComputedStyle(document.querySelector('.xterm-screen')).cursor"),'pointer');

  win.webContents.send('display', {width:1600,height:1000});
  win.webContents.send('theme', {background:'#123456',foreground:'#fedcba',cursor:'#abcdef'});
  await delay(100);
  assert.equal(await win.webContents.executeJavaScript("getComputedStyle(document.body).backgroundColor"), 'rgb(18, 52, 86)');
  const paletteTheme=require('../slab-theme.cjs').resolveTheme({version:1,enabled:true,palettes:{blank:{background:[219,255,255],foreground:[27,31,46],bold:[0,0,10],cursor:[67,78,99]}}},'blank');
  win.webContents.send('theme',paletteTheme);
  win.webContents.send('output','\x1b[15;1H'+[[7,'Text'],[1,'Error'],[2,'Status'],[3,'Highlight'],[5,'Handle'],[9,'You'],[10,'Edit'],[11,'Run'],[13,'Prompt']].map(([n,label])=>`\x1b[38;5;${n}m${label}  `).join('')+'\x1b[0m');
  const geometry = () => win.webContents.executeJavaScript(`(()=>{
    const a=document.getElementById('artifact').getBoundingClientRect();
    const v=document.getElementById('preview-viewport').getBoundingClientRect();
    return {width:a.width,height:a.height,canvasWidth:v.width,canvasHeight:v.height};
  })()`);
  await delay(250);
  const compact = await geometry();
  assert(Math.abs((compact.width-2)/(compact.height-2)-1.6)<.005);
  assert.equal(await win.webContents.executeJavaScript("document.getElementById('preview-fullscreen') === null"),true);
  assert.equal(await win.webContents.executeJavaScript("document.getElementById('qr-card').tagName"),'BUTTON');
  win.webContents.sendInputEvent({type:'mouseMove',x:50,y:50});
  await delay(300);
  assert((await geometry()).width>compact.width,'Hover must enlarge the preview');
  win.webContents.sendInputEvent({type:'mouseMove',x:700,y:450});
  await delay(300);
  assert.deepEqual(await geometry(),compact,'Leaving must restore compact preview');

  assert.equal(await win.webContents.executeJavaScript("document.getElementById('artifact').dispatchEvent(new MouseEvent('contextmenu',{button:2,bubbles:true,cancelable:true}))"),true,'Right-click must remain available');
  assert.deepEqual(await geometry(),compact,'Right-click must not zoom');
  win.webContents.send('preview-mode','pinned');
  await delay(250);
  const enlarged = await geometry();
  assert(enlarged.width>compact.width);
  assert(Math.abs(enlarged.canvasWidth-(enlarged.width-2))<.1);
  assert(Math.abs(enlarged.canvasHeight-(enlarged.height-2))<.1);
  win.webContents.send('preview-mode','compact');
  await delay(250);
  assert.deepEqual(await geometry(),compact);
  const logicalSize = () => win.webContents.executeJavaScript("[document.getElementById('preview-viewport').style.width,getComputedStyle(document.getElementById('preview-viewport')).width,getComputedStyle(document.getElementById('preview-viewport')).height]");
  const logicalBefore = await logicalSize();
  win.webContents.send('fullscreen-state',{app:true,preview:true});
  await delay(100);
  assert.deepEqual(await logicalSize(),logicalBefore,'Fullscreen must keep the piece viewport unchanged');
  win.webContents.send('fullscreen-state',{app:false,preview:false});
  await delay(250);
  results.push({compact,enlarged});
  for (const state of JSON.parse(fs.readFileSync(join(cwd, 'cases.json')))) {
    win.webContents.send('state', { ...state, localPreview: localPreview(cwd, state.preview) });
    let result;
    for (let attempt = 0; attempt < 100; attempt++) {
      await delay(60);
      result = await win.webContents.executeJavaScript(inspect);
      const done = state.medium === 'gameboy' ? result.frames >= 30 && result.status === 'ready'
        : state.medium === 'picture' ? result.image > 0
        : state.medium === 'sound' ? result.canvas && result.colors > 1 : result.text.length > 0;
      if (done) break;
    }
    assert(!result.text.startsWith('Preview:'), `${state.medium}: ${result.text}`);
    if (state.medium === 'gameboy') {
      assert.equal(result.status, 'ready');
      assert(result.frames >= 30, 'Game Boy must produce real continuing frames');
      assert(result.colors > 1, 'Game Boy frame must contain visible artwork, not just alpha');
      const before = result.hash;
      await win.webContents.executeJavaScript("document.querySelector('#media-preview canvas').focus()");
      await win.webContents.executeJavaScript("document.querySelector('#media-preview canvas').dispatchEvent(new KeyboardEvent('keydown',{key:'ArrowRight',bubbles:true,cancelable:true}))");
      await delay(220);
      await win.webContents.executeJavaScript("document.querySelector('#media-preview canvas').dispatchEvent(new KeyboardEvent('keyup',{key:'ArrowRight',bubbles:true,cancelable:true}))");
      await delay(100);
      const moved = await win.webContents.executeJavaScript(inspect);
      assert.notEqual(moved.hash, before, 'D-pad must move the starter sprite');
      await win.webContents.executeJavaScript("document.querySelector('#media-preview canvas').dispatchEvent(new KeyboardEvent('keydown',{key:'x',bubbles:true,cancelable:true}))");
      await delay(100);
      await win.webContents.executeJavaScript("document.querySelector('#media-preview canvas').dispatchEvent(new KeyboardEvent('keyup',{key:'x',bubbles:true,cancelable:true}))");
      await delay(100);
      const reset = await win.webContents.executeJavaScript(inspect);
      assert.equal(reset.hash, before, 'A button must recenter the starter sprite');
      result.joypadMoves = true;
      result.aResets = true;
    }
    if (state.medium === 'picture') assert(result.image > 0);
    if (state.medium === 'picture') {
      win.webContents.send('state',{...state,url:'aesthetic.computer/#smoke',qr:[[true,false],[false,true]],localPreview:localPreview(cwd,state.preview)});
      await delay(100);
      assert.equal(await win.webContents.executeJavaScript("document.getElementById('qr-card').hidden"),false);
      await win.webContents.executeJavaScript("document.getElementById('qr-card').click()");
      await delay(50);
      assert.equal(openedPiece,'aesthetic.computer/#smoke');
      assert.equal((await win.webContents.executeJavaScript(inspect)).image,result.image,'Publishing must keep the local Picture preview');
      win.webContents.send('state',{...state,localPreview:localPreview(cwd,state.preview)});
      await delay(100);
      assert.equal(await win.webContents.executeJavaScript("document.getElementById('qr-card').hidden"),true,'Unpublished version must hide the old QR');
    }

    if (state.medium === 'sound') assert(result.canvas && result.colors > 1);
    fs.writeFileSync(join(cwd, state.medium + '.png'), (await win.webContents.capturePage()).toPNG());
    results.push({ medium: state.medium, ...result });
  }
  const textSizes = [];
  const previewSizes = [];
  await win.webContents.executeJavaScript("document.body.classList.remove('preview-fullscreen')");
  for (const action of ['reset','larger','smaller']) {
    win.webContents.send('text-size',action);
    await new Promise(r=>setTimeout(r,100));
    await delay(200);
    previewSizes.push(await geometry());
    textSizes.push(await win.webContents.executeJavaScript("[document.querySelector('.xterm-screen').style.width,document.querySelector('.xterm-screen').style.height]"));
  }
  assert.deepEqual(textSizes[0],textSizes[2]);
  assert.notDeepEqual(textSizes[0],textSizes[1]);
  assert(previewSizes[1].width>previewSizes[0].width);
  assert.deepEqual(previewSizes[0],previewSizes[2]);
  results.push({textSizes,previewSizes});
  assert.equal(errors.length, 0, errors.join('\n'));
  console.log(JSON.stringify(results));
  fs.writeFileSync(join(cwd, 'results.json'), JSON.stringify(results));
  win.destroy();
  app.quit();
}).catch(error => { console.error(error); app.exit(1); });
