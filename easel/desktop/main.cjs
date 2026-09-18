const {chmodSync}=require('node:fs');
const { app, BrowserWindow, ipcMain, shell, Menu, clipboard, screen } = require('electron');
const pty = require('node-pty');
const { join, resolve } = require('node:path');
const { mkdirSync, readdirSync, readFileSync, writeFileSync, watch, existsSync, rmSync, renameSync } = require('node:fs');
const {followSlabTheme,FALLBACK} = require('./slab-theme.cjs');
const {createHash,randomUUID} = require('node:crypto');
const {localPreview} = require('./local-preview.cjs');
const {createUpdater} = require('./updater.cjs');
const {startFrameCapture} = require('./frame-capture.cjs');
const { tmpdir } = require('node:os');

app.setName('aesel');
app.setPath('userData', join(app.getPath('appData'), 'Easel'));
let window, terminal, timer, ledgerWatcher, quitting = false;
let terminalSize = {cols:100,rows:32};
let currentTheme = FALLBACK;
const themeFollower = followSlabTheme(theme => { currentTheme = theme; if (window && !window.isDestroyed()) { window.setBackgroundColor(theme.background); send('theme',theme); } });
const startedAt = Date.now();
const root = app.isPackaged ? join(process.resourcesPath, 'easel') : resolve(__dirname, '..');
const qrEncoder = import(require('node:url').pathToFileURL(join(root, 'src/vendor/qr.mjs')).href);
const slabHome = join(tmpdir(), `easel-desktop-${process.pid}`);
const supplied = process.argv.slice(app.isPackaged ? 1 : 2);
const option = name => { const at = supplied.indexOf(name); return at < 0 ? '' : supplied[at + 1] || ''; };
const launchFile = join(app.getPath('userData'), 'last-workspace.json');
let lastWorkspace = '';
try { const saved = JSON.parse(readFileSync(launchFile,'utf8')); if (typeof saved.cwd === 'string' && existsSync(saved.cwd)) lastWorkspace = saved.cwd; } catch {}
const workspace = resolve(option('--cwd') || lastWorkspace || join(app.getPath('userData'), 'projects', 'first-piece'));
const instance = option('--instance') || 'default';
const independentWindow = /^window-[a-f0-9-]{36}$/.test(instance);
if(independentWindow){const sessionData=join(app.getPath('userData'),'window-sessions',instance);mkdirSync(sessionData,{recursive:true,mode:0o700});app.setPath('sessionData',sessionData);}
const primaryInstance = independentWindow || app.requestSingleInstanceLock();
if (!primaryInstance) app.quit();
else { mkdirSync(app.getPath('userData'),{recursive:true,mode:0o700}); writeFileSync(launchFile,JSON.stringify({cwd:workspace}),{mode:0o600}); }
app.on('second-instance', (_event, _argv, _cwd, data) => {
  if (window) { if (window.isMinimized()) window.restore(); window.show(); window.focus(); }
  if (data?.restart && data.workspace === workspace) requestRestart('restart');
});
mkdirSync(workspace, { recursive: true });
const sessionDir = join(app.getPath('userData'), 'sessions');
mkdirSync(sessionDir, {recursive:true,mode:0o700});
const sessionFile = join(sessionDir, createHash('sha256').update(workspace).digest('hex') + '.json');
const continuationFile = sessionFile + '.continue';
let continueSession = false;
if (primaryInstance) {
  try {
    const marker = JSON.parse(readFileSync(continuationFile,'utf8'));
    continueSession = marker.cwd === workspace && Date.now()-marker.at >= 0 && Date.now()-marker.at < 120000;
    rmSync(continuationFile,{force:true});
  } catch {}
}
const controlFile = join(slabHome, 'desktop-control.json');
const intentFile = join(slabHome, 'desktop-intent.json');
const previewEventsFile=join(slabHome,'preview-events.json');
let previewGuest=null;
let previewContext=null,previewEvents=[],previewSequence=0,previewFlush=null;
const previewRecent=new Map();
function previewDiagnostic(level,text,source='',line=0){
 if(!previewContext||!['error','warn'].includes(level))return;
 const entry={...previewContext,sequence:++previewSequence,level,text:String(text||'').slice(0,2000),source:String(source).slice(0,300),line:Number(line)||0};
 if(!entry.text)return;
 const key=entry.revision+'|'+level+'|'+entry.text,now=Date.now();
 if(now-(previewRecent.get(key)||0)<2000)return;
 previewRecent.set(key,now);if(previewRecent.size>100)previewRecent.delete(previewRecent.keys().next().value);
 previewEvents.push(entry);previewEvents=previewEvents.slice(-50);
 if(!previewFlush)previewFlush=setTimeout(()=>{previewFlush=null;try{const temp=previewEventsFile+'.tmp';writeFileSync(temp,JSON.stringify(previewEvents),{mode:0o600});renameSync(temp,previewEventsFile);}catch{}},100);
}
mkdirSync(slabHome, {recursive:true,mode:0o700});
const stopFrameCapture=startFrameCapture({workspace,root,context:()=>previewContext,guest:()=>previewGuest});
let pendingRestart = false;
function requestRestart(action = 'restart') {
  if (!terminal) return desktopUpdater.afterCheckpoint(action);
  pendingRestart = true;
  writeFileSync(intentFile, JSON.stringify({action}), {mode:0o600});
  terminal.kill('SIGUSR2');
}
// Mac App Store owns updates for its sandboxed build. The Developer ID build
// uses aesel's generic signed feed; never let electron-updater compete with
// Store receipts or present a non-Store update inside the MAS app.
const canUpdateBinary = app.isPackaged && !process.mas && existsSync(join(process.resourcesPath,'app-update.yml'));
const desktopUpdater = createUpdater({app, canUpdateBinary, requestRestart, prepareRelaunch: () => writeFileSync(continuationFile,JSON.stringify({cwd:workspace,at:Date.now()}),{mode:0o600}), notify: message => send('desktop-notice', message)});

function send(channel, data) { if (window && !window.isDestroyed()) window.webContents.send(channel, data); }
function start({home = false} = {}) {
  if (terminal) return;
  const args = [join(root, 'src/tui.mjs'), '--cwd', workspace];
  for (const name of ['--piece', '--resume', '--model', '--backend', '--effort']) if (option(name) && !(home && ['--piece','--resume'].includes(name))) args.push(name, option(name));
  if (continueSession && !home) args.push('--continue-session');
  if (supplied.includes('--no-autopublish')) args.push('--no-autopublish');
  const env = { ...process.env, ELECTRON_RUN_AS_NODE: '1', TERM: 'xterm-256color', COLORTERM: 'truecolor', SLAB_HOME: slabHome, EASEL_DESKTOP: '1', EASEL_KEEP_PREVIEW:keepPreviewOnStart?'1':'0', EASEL_HOST_PID:String(process.pid), EASEL_HOST_WINDOW_ID:window.getMediaSourceId().split(':')[1], EASEL_THEME:'slab', EASEL_MOUSE:'0', EASEL_DESKTOP_SESSION:sessionFile, EASEL_DESKTOP_CONTROL:controlFile, EASEL_DESKTOP_INTENT:intentFile, EASEL_PREVIEW_EVENTS:previewEventsFile };
  delete env.NODE_OPTIONS;
  delete env.NO_COLOR;
  env.FORCE_COLOR = '3';
  env.EASEL_GROUND = 'paint';
  try {
    // Native extraction can discard mode bits; repair only our own launcher.
    if(process.platform!=='win32'&&!process.mas){
      const helper=join(app.isPackaged?join(process.resourcesPath,'app.asar.unpacked'):__dirname,'node_modules/node-pty/build/Release/spawn-helper');
      if(existsSync(helper))chmodSync(helper,0o755);
    }
    terminal = process.mas
      ? require('./mas-terminal.cjs').createMasTerminal(args,{...terminalSize,cwd:workspace,env})
      : pty.spawn(process.execPath, args, { name: 'xterm-256color', ...terminalSize, cwd: workspace, env });
    terminal.onData(data => send('output', data));
    terminal.onExit(({ exitCode }) => {
      terminal = null;
      if (exitCode === 75 && existsSync(controlFile)) {
        const request = JSON.parse(readFileSync(controlFile, 'utf8'));
        rmSync(controlFile, {force:true});
        if (request.action === 'home') {
          timer?.close(); ledgerWatcher?.close();
          pendingRestart = false;
          send('output', '\x1b]777;easel-phase:startup\x07\x1b[2J\x1b[H');
          return start({home:true});
        }
        if (request.action === 'restart') {
          pendingRestart = false; continueSession = true; keepPreviewOnStart = true;
          timer?.close();
          send('desktop-notice','Agent restarted. Your window and preview stay open.');
          start(); return;
        }
        if (request.action === 'update') {
          quitting = true;
          timer?.close();
          return desktopUpdater.afterCheckpoint(request.action);
        }
      }
      if (exitCode === 0) { quitting = true; app.quit(); return; }
      pendingRestart = false; send('output', `\r\naesel closed (${exitCode}). Close this window to finish.\r\n`); if (quitting) app.quit(); });
  } catch (error) { console.error('Agent startup failed:',error); send('desktop-notice','The agent could not start. Use Restart Agent to retry.'); }
  let previous = '', qrUrl = '', qr = null, previewKey = '', previewData = {};
  const dir = join(slabHome, 'state/active-prompts');
  mkdirSync(dir, {recursive:true});
  let updating = false, updateAgain = false;
  const update = async () => {
    if(updating){updateAgain=true;return;}
    updating=true;
    try {
      for (const name of readdirSync(dir)) {
        if (name.includes('.tmp')) continue;
        const state = JSON.parse(readFileSync(join(dir, name), 'utf8'));
        themeFollower.setStatus(state.state);
        previewContext=(state.artifact_kind||'piece')==='piece'&&state.piece_channel?{channel:state.piece_channel,revision:state.piece_revision,version:state.piece_version}:null;
        let proxName=null;
        try {const ledger=JSON.parse(readFileSync(join(app.getPath('home'),'.config/slab/ledger/local.json'),'utf8'));const entry=ledger.entries?.find(e=>e.id===state.session_id&&e.agentType==='easel');if(entry?.proxNamespace==='easel'&&entry.proxName?.length<=100&&/^[a-z0-9_-]+(?:\/[a-z0-9_-]+)?$/.test(entry.proxName))proxName=entry.proxName;}catch{}
        const visible = JSON.stringify({ proxName, handle:state.handle||"",handleColors:state.handle_colors||null, piece: state.piece, status:state.state, url: state.scan_url, version: state.piece_version, publication:state.piece_published_at || "", channel:state.piece_channel || "", flow: state.flow, medium: state.artifact_kind || 'piece', preview: state.artifact_preview || null });
        if (visible !== previous) {
          previous = visible;
          if (state.scan_url !== qrUrl) {
            qrUrl = state.scan_url || '';
            const {qrcode, ErrorCorrectLevel} = await qrEncoder;
            qr = qrUrl ? qrcode(qrUrl, {errorCorrectLevel:ErrorCorrectLevel.L}).modules : null;
          }
          const nextPreviewKey = JSON.stringify(state.artifact_preview || null);
          if (nextPreviewKey !== previewKey) {
            previewKey = nextPreviewKey;
            try { previewData = {localPreview:await localPreview(workspace,state.artifact_preview)}; }
            catch (error) { previewData = {previewError:error.message}; }
          }
          lastVisibleState = {...JSON.parse(visible), qr, ...previewData};
          send('state', lastVisibleState);
        }
      }
    } catch {}
    finally {updating=false;if(updateAgain){updateAgain=false;void update();}}
  };
  timer = watch(dir, () => { update(); });
  try {ledgerWatcher=watch(join(app.getPath('home'),'.config/slab/ledger'),(_,file)=>{if(String(file)==='local.json')update();});}catch{}
  update();
}
function openNewWindow(){
 const id=`window-${randomUUID()}`;
 const cwd=join(app.getPath('userData'),'projects',id);
 mkdirSync(cwd,{recursive:true,mode:0o700});
 const env={...process.env};delete env.ELECTRON_RUN_AS_NODE;delete env.NODE_OPTIONS;
 const args=[...(app.isPackaged?[]:[app.getAppPath()]),'--instance',id,'--cwd',cwd];
 const child=spawn(process.execPath,args,{detached:true,stdio:'ignore',env});
 child.on('error',error=>send('desktop-notice',`Could not open a window: ${error.message}`));child.unref();
}
app.whenReady().then(() => {
  if (!primaryInstance) return;
  Menu.setApplicationMenu(Menu.buildFromTemplate([
    { label: 'aesel', submenu: [
      {role:'about'}, {type:'separator'}, {role:'close',accelerator:'CmdOrCtrl+W'},
      {label:'Check for Updates…', click:()=>desktopUpdater.check()},
      {label:'Restart aesel', click:()=>requestRestart('restart')},
      {type:'separator'}, {role:'hide'}, {role:'hideOthers'}, {role:'unhide'},
      {type:'separator'}, {role:'quit'},
    ] },
    {label:'File',submenu:[{label:'New Window',accelerator:'CmdOrCtrl+N',click:openNewWindow}]},
    { role: 'editMenu' },
    { label: 'View', submenu: [
      {label:'aesel Actions…',click:()=>{const gallery=new BrowserWindow({parent:window,width:1120,height:850,title:'aesel actions',backgroundColor:'#241d35',webPreferences:{contextIsolation:true,nodeIntegration:false,sandbox:true}});gallery.loadFile(join(__dirname,'donkey-gallery.html'));}},
      {label:'Larger Text',accelerator:'CmdOrCtrl+=',click:()=>send('text-size','larger')},
      {label:'Smaller Text',accelerator:'CmdOrCtrl+-',click:()=>send('text-size','smaller')},
      {label:'Reset Text Size',accelerator:'CmdOrCtrl+0',click:()=>send('text-size','reset')},
      {type:'separator'},
      { label: 'Bitmap Font', type: 'radio', checked: true, accelerator: 'CmdOrCtrl+Shift+B', click: () => send('font', true) },
      { label: 'Smooth Font', type: 'radio', accelerator: 'CmdOrCtrl+Shift+M', click: () => send('font', false) },
      {type:'separator'},
      {label:'Preview',submenu:[
        {label:'Compact',type:'radio',click:()=>send('preview-mode','compact')},
        {label:'Zoom on Hover',type:'radio',checked:true,click:()=>send('preview-mode','hover')},
        {label:'Always Open',type:'radio',click:()=>send('preview-mode','pinned')},
      ]},
    ] },
    { role: 'windowMenu' },
  ]));
  window = new BrowserWindow({ width: 760, height: 540, title: 'aesel', backgroundColor: '#463264',
    webPreferences: { preload: join(__dirname, 'preload.cjs'), contextIsolation: true, nodeIntegration: false, sandbox: true, webviewTag: true, plugins:true } });
  const creditLabel = require('./credit-label.cjs').startCreditLabel({app, window, root});
  require('./credit-checkout.cjs').startCreditCheckout({app,window,root,ipcMain,shell,refresh:creditLabel.refresh});
  require('./native-title.cjs').registerNativeTitle({ipcMain, window, app});
  const historySessions=new WeakSet();
  window.webContents.on('will-attach-webview', (event, preferences, params) => {
    if(String(params.partition||'').startsWith('aesel-history-'))historySessions.add(require('electron').session.fromPartition(params.partition));
    delete preferences.preload;
    preferences.nodeIntegration = false;
    preferences.contextIsolation = true;
    preferences.sandbox = true;
    try { const url = new URL(params.src); if (url.protocol !== 'https:' || !['aesthetic.computer', 'prompt.ac'].includes(url.hostname)) event.preventDefault(); }
    catch { event.preventDefault(); }
  });
  window.webContents.on('did-attach-webview',(_event,contents)=>{
    if(historySessions.has(contents.session))return;
    previewGuest=contents;
    contents.on('did-navigate',(_event,address)=>{
      contents.aeselLiveChannel = null;
      try {
        const route=new URL(address);
        const match=/^\/prompt~channel%20([^~]+)~!autorun$/.exec(route.pathname);
        if(route.origin==='https://aesthetic.computer'&&match)contents.aeselLiveChannel=decodeURIComponent(match[1]);
      } catch {}
    });
    contents.on('console-message',(details,level,message,line,sourceId)=>{
      const severity=details.level || ['debug','info','warning','error'][level];
      previewDiagnostic(severity==='warning'?'warn':severity,details.message || message,details.sourceId || sourceId,details.lineNumber || line);
    });
    contents.on('did-fail-load',(_event,code,description,url,isMainFrame)=>{if(code!==-3&&isMainFrame)previewDiagnostic('error',`Preview load failed (${code}): ${description}`,url);});
    contents.on('render-process-gone',(_event,details)=>previewDiagnostic('error',`Preview renderer exited: ${details.reason}`));
  });
  window.webContents.setWindowOpenHandler(({ url }) => { if (/^https:\/\//.test(url)) shell.openExternal(url); return { action: 'deny' }; });
  window.webContents.on('will-navigate', event => event.preventDefault());
  window.webContents.once('did-finish-load', () => console.log(JSON.stringify({event:'window-ready',ms:Date.now()-startedAt,gpu:app.getGPUFeatureStatus()})));
  if (option('--diagnostics')) window.webContents.once('did-finish-load', () => {
    setTimeout(async () => {
      const target = resolve(option('--diagnostics'));
      mkdirSync(target, {recursive:true});
      const renderer = await window.webContents.executeJavaScript(`({font:document.fonts.check('16px "AC aesel Unifont"'), canvases:document.querySelectorAll('.xterm canvas').length, qr:!document.getElementById('qr-card').hidden})`);
      writeFileSync(join(target, 'gpu.json'), JSON.stringify({features:app.getGPUFeatureStatus(),info:await app.getGPUInfo('complete'),renderer,layout:await window.webContents.executeJavaScript('window.aeselLayoutSnapshot?.()')},null,2));
      writeFileSync(join(target, 'window.png'), (await window.webContents.capturePage()).toPNG());
    }, 3000);
  });
  window.on('move', sendDisplay);
  screen.on('display-metrics-changed', sendDisplay);
  window.on('enter-full-screen', fullscreenState);
  window.on('leave-full-screen', () => { previewFullscreen = false; fullscreenState(); });
  const uiRoot = existsSync(join(root,'desktop-ui','index.html')) ? join(root,'desktop-ui') : __dirname;
  window.loadFile(join(uiRoot, 'index.html'));
  if (canUpdateBinary) {
    const firstCheck = setTimeout(() => desktopUpdater.check(), 30000); firstCheck.unref();
  }
  window.on('close', event => {
    if (terminal) { event.preventDefault(); if (!quitting) { quitting = true; terminal.kill('SIGTERM'); setTimeout(() => { if (terminal) { quitting = false; send('desktop-notice','The session is still saving or working. Retry closing when it is ready.'); } }, 6000).unref(); } }
  });
});
let rockLayoutPath='',lastRockLayout='',rockLayoutWatcher;
function checkNativeTitle(){
  try{
    const ack=JSON.parse(readFileSync(rockLayoutPath+'.ack','utf8')),expected=JSON.parse(lastRockLayout);
    send('native-title',ack.title===expected.title&&ack.fontSize===expected.titleFontSize&&['titleX','titleY','titleWidth','titleHeight'].every(k=>Math.abs(ack[k]-expected[k])<1));
  }catch{send('native-title',false);}
}

ipcMain.on('title-geometry',(event,value)=>{
  if(event.sender!==window?.webContents||!value||!['x','y','size'].every(k=>Number.isFinite(value[k]))||typeof value.visible!=='boolean')return;
  const bounds=window.getBounds(),content=window.getContentBounds();
  if(value.x<0||value.y<0||value.x>content.width||value.y>content.height||value.size<16||value.size>96)return;
  const layout={x:value.x+content.x-bounds.x,y:value.y+content.y-bounds.y,size:value.size,visible:value.visible};
  if(typeof value.title==='string'&&value.title.length<=120&&['titleX','titleY','titleWidth','titleHeight','titleFontSize'].every(k=>Number.isFinite(value[k])&&value[k]>=0)){
    Object.assign(layout,{title:value.title,titleX:value.titleX+content.x-bounds.x,titleY:value.titleY+content.y-bounds.y,titleWidth:value.titleWidth,titleHeight:value.titleHeight,titleFontSize:value.titleFontSize,titleColors:Array.isArray(value.titleColors)?value.titleColors.slice(0,120).map(c=>Array.isArray(c)&&c.length===3&&c.every(n=>Number.isFinite(n)&&n>=0&&n<=255)?c:[255,255,255]):[]});
  }
  const serialized=JSON.stringify(layout);if(serialized===lastRockLayout)return;lastRockLayout=serialized;
  try{
    const dir=join(app.getPath('home'),'.local/share/slab/state/easel-layout');mkdirSync(dir,{recursive:true,mode:0o700});
    rockLayoutPath=join(dir,`${process.pid}-${window.getMediaSourceId().split(':')[1]}.json`);
    writeFileSync(rockLayoutPath+'.tmp',serialized,{mode:0o600});renameSync(rockLayoutPath+'.tmp',rockLayoutPath);
    if(!rockLayoutWatcher)rockLayoutWatcher=watch(dir,(_,file)=>{if(rockLayoutPath.endsWith(String(file).replace(/\.ack$/,''))&&String(file).endsWith('.ack'))checkNativeTitle();});
    checkNativeTitle();
  }catch(error){console.warn('aesel rock layout:',error.message);}
});
ipcMain.on('ready', event => { if (event.sender === window?.webContents) { send('theme',currentTheme); sendDisplay(true); if(lastVisibleState)send('state',lastVisibleState); if(terminal)terminal.kill('SIGWINCH'); else start(); } });
ipcMain.on('closing', event => { if(event.sender===window?.webContents) window.hide(); });
ipcMain.on('input', (event, data) => { if (event.sender === window?.webContents && typeof data === 'string' && data.length < 1_048_576) terminal?.write(data); });
ipcMain.on('size', (event, { cols, rows } = {}) => {
  if (event.sender === window?.webContents && Number.isInteger(cols) && Number.isInteger(rows) && cols >= 32 && cols <= 500 && rows >= 10 && rows <= 300) { if(cols!==terminalSize.cols||rows!==terminalSize.rows){terminalSize={cols,rows};terminal?.resize(cols,rows);} }
});
app.on('window-all-closed', () => { rockLayoutWatcher?.close();if(rockLayoutPath){rmSync(rockLayoutPath,{force:true});rmSync(rockLayoutPath+'.ack',{force:true});} timer?.close(); ledgerWatcher?.close(); stopFrameCapture(); themeFollower.close(); terminal?.kill(); app.quit(); });

const isWindow = event => event.sender === window?.webContents;
ipcMain.on('copy-text', (event, text) => { if (isWindow(event) && typeof text === 'string' && text.length <= 1048576) clipboard.writeText(text); });
ipcMain.on('paste-request', event => { if (isWindow(event)) send('paste', clipboard.readText()); });
ipcMain.on('context-menu', (event, value) => {
  if (!isWindow(event)) return;
  const selection = typeof value === 'string' ? value : value?.selection;
  const text = typeof selection === 'string' ? selection.slice(0,1048576) : '';
  Menu.buildFromTemplate([
    {label:'Copy', enabled:!!text, click:()=>clipboard.writeText(text)},
    {label:'Paste', click:()=>send('paste',clipboard.readText())},
    {type:'separator'}, {label:'Select All', click:()=>send('select-all')},
  ]).popup({window});
});
let previewFullscreen = false, beforePreviewFullscreen = false;
let displayKey = '';
function sendDisplay(force = false) {
  if (!window || window.isDestroyed()) return;
  const display = screen.getDisplayMatching(window.getBounds());
  const key = `${display.id}:${display.size.width}:${display.size.height}`;
  if (force === true || key !== displayKey) { displayKey = key; send('display',{width:display.size.width,height:display.size.height}); }
}
function fullscreenState() { send('fullscreen-state', {app:window.isFullScreen() || window.isSimpleFullScreen(),preview:previewFullscreen}); }
function toggleFullscreen(target) {
  if (target === 'preview') {
    previewFullscreen = !previewFullscreen;
    if (previewFullscreen) { beforePreviewFullscreen = window.isFullScreen(); if (!beforePreviewFullscreen) { if (process.platform === 'darwin') window.setSimpleFullScreen(true); else window.setFullScreen(true); } }
    else if (!beforePreviewFullscreen) { if (process.platform === 'darwin') window.setSimpleFullScreen(false); else window.setFullScreen(false); }
  } else if (target === 'app') {
    if (previewFullscreen) { previewFullscreen = false; window.setFullScreen(beforePreviewFullscreen); }
    else window.setFullScreen(!window.isFullScreen());
  }
  fullscreenState();
}
ipcMain.on('open-link',(event,value)=>{if(!isWindow(event)||typeof value!=='string'||value.length>4096)return;try{const url=new URL(value);if(['http:','https:'].includes(url.protocol)&&!url.username&&!url.password)shell.openExternal(url.href);}catch{}});
ipcMain.on('open-piece', (event, value) => {
  if (!isWindow(event) || typeof value !== 'string') return;
  try {
    const url = new URL(value.startsWith('https://') ? value : `https://${value}`);
    if (url.protocol === 'https:' && ['prompt.ac','aesthetic.computer'].includes(url.hostname) && !url.username && !url.password) shell.openExternal(url.href);
  } catch {}
});
ipcMain.on('fullscreen', (event, value) => { if (isWindow(event)) toggleFullscreen(typeof value === 'string' ? value : value?.target); });
app.on('web-contents-created', (_event, contents) => {
  contents.on('before-input-event', (event, input) => {
    if(input.type==='keyDown' && input.control && !input.meta && !input.alt && input.key.toLowerCase()==='c') { event.preventDefault(); terminal?.write('\x03'); return; }
    if (input.type === 'keyDown' && (input.meta || input.control) && !input.alt && ['+','=','-','0'].includes(input.key)) {
      event.preventDefault(); send('text-size', input.key === '0' ? 'reset' : input.key === '-' ? 'smaller' : 'larger'); return;
    }
    if (previewFullscreen && input.type === 'keyDown' && input.key === 'Escape') {
      event.preventDefault(); toggleFullscreen('preview');
    }
  });
});

// Native menus live outside the renderer viewport; return only the chosen index.
ipcMain.handle('notebook-context-menu', (event, items) => {
 if(!isWindow(event)||!Array.isArray(items)||items.length>32)return -1;
 return new Promise(resolve=>{
  let chosen=-1;
  const template=items.map((item,index)=>item?.separator?{type:'separator'}:{label:String(item?.label||'').slice(0,160),enabled:item?.enabled!==false,click:()=>{chosen=index;}});
  Menu.buildFromTemplate(template).popup({window,callback:()=>resolve(chosen)});
 });
});
