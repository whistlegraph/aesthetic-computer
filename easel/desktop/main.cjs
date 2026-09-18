const {chmodSync}=require('node:fs');
const { app, BrowserWindow, ipcMain, shell, Menu, clipboard, screen, nativeImage } = require('electron');
const pty = require('node-pty');
const { spawn } = require('node:child_process');
const { join, resolve } = require('node:path');
const { closeSync, copyFileSync, mkdirSync, openSync, readdirSync, readFileSync, writeFileSync, watch, existsSync, rmSync, renameSync } = require('node:fs');
const {followSlabTheme,FALLBACK} = require('./slab-theme.cjs');
const {createHash,randomUUID} = require('node:crypto');
const {localPreview} = require('./local-preview.cjs');
const {createUpdater} = require('./updater.cjs');
const {startFrameCapture} = require('./frame-capture.cjs');
const { tmpdir, homedir } = require('node:os');

const devHome=process.env.AESEL_DEV_HOME||'';
app.setName(devHome?'Aesel Dev':'aesel');
app.setPath('userData', join(app.getPath('appData'), devHome?'Aesel Dev':'Easel'));
let window, terminal, timer, quitting = false;
let lastVisibleState = null, keepPreviewOnStart = false;
let terminalSize = {cols:100,rows:32};
let currentTheme = FALLBACK;
let currentPaperPath = '';
let currentPreviewPath = '', currentPreviewMime = '', currentPreviewName = '';
const dragDir=join(tmpdir(),`easel-drag-${process.pid}`);
let systemTextSize = null, systemTextWatcher = null;
const systemTextSizePath = join(homedir(), '.local', 'share', 'slab', 'state', 'prompt-text-size.json');
function readSystemTextSize() {
  try {
    const value = JSON.parse(readFileSync(systemTextSizePath, 'utf8'));
    if (Number.isInteger(value.fontSize) && value.fontSize >= 8 && value.fontSize <= 48) {
      systemTextSize = {fontSize:value.fontSize, mode:['far','near','tiny'].includes(value.mode) ? value.mode : ''};
      send('system-text-size', systemTextSize);
    }
  } catch {}
}
try { systemTextWatcher = watch(join(homedir(), '.local', 'share', 'slab', 'state'), (_event,name) => { if (name === 'prompt-text-size.json') readSystemTextSize(); }); } catch {}
readSystemTextSize();
const themeFollower = followSlabTheme(theme => { currentTheme = theme; if (window && !window.isDestroyed()) { window.setBackgroundColor(theme.background); send('theme',theme); } });
const startedAt = Date.now();
let root = process.env.AESEL_DEV_ROOT || (app.isPackaged ? join(process.resourcesPath, 'easel') : resolve(__dirname, '..'));
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
mkdirSync(app.getPath('userData'),{recursive:true,mode:0o700});
writeFileSync(launchFile,JSON.stringify({cwd:workspace}),{mode:0o600});
const addressDir = join(app.getPath('userData'), 'instance-addresses');
mkdirSync(addressDir,{recursive:true,mode:0o700});
function claimAddress() {
  for (let index=0; index<26; index++) {
    const label=String.fromCharCode(65+index), path=join(addressDir,`${label}.json`);
    try {
      const owner=JSON.parse(readFileSync(path,'utf8'));
      try { process.kill(Number(owner.pid),0); continue; } catch { rmSync(path,{force:true}); }
    } catch {}
    try {
      const fd=openSync(path,'wx',0o600);
      writeFileSync(fd,JSON.stringify({pid:process.pid,instance,workspace,startedAt:Date.now()}));
      closeSync(fd);
      return {label,path};
    } catch {}
  }
  return {label:String(process.pid),path:''};
}
const address=primaryInstance?claimAddress():{label:'',path:''};
function releaseAddress(){
  if(!address.path)return;
  try { const owner=JSON.parse(readFileSync(address.path,'utf8')); if(Number(owner.pid)===process.pid)rmSync(address.path,{force:true}); } catch {}
}
app.once('will-quit',()=>{releaseAddress();try{rmSync(dragDir,{recursive:true,force:true});}catch{}});
process.once('exit',releaseAddress);
app.on('second-instance',()=>{if(!window||window.isDestroyed())return;if(window.isMinimized())window.restore();window.show();window.focus();});
// `open -a Easel` activates an existing macOS process without necessarily
// launching a second instance. A checkpointed restart can therefore leave the
// process healthy but its previously hidden window unreachable unless the app
// handles the ordinary Dock/reopen activation itself.
app.on('activate', () => {
  if (!window || window.isDestroyed()) return;
  if (window.isMinimized()) window.restore();
  window.show();
  window.focus();
});
mkdirSync(workspace, { recursive: true });
const sessionDir = join(app.getPath('userData'), 'sessions');
mkdirSync(sessionDir, {recursive:true,mode:0o700});
const sessionFile = join(sessionDir, createHash('sha256').update(`${workspace}\0${instance}`).digest('hex') + '.json');
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
const canUpdateBinary = !devHome && app.isPackaged && !process.mas && existsSync(join(process.resourcesPath,'app-update.yml'));
const desktopUpdater = createUpdater({app, canUpdateBinary, onStatus:(status,info)=>buildStatus.release(status,info), requestRestart, prepareRelaunch: () => writeFileSync(continuationFile,JSON.stringify({cwd:workspace,at:Date.now()}),{mode:0o600}), notify: message => send('desktop-notice', message)});

let pendingDevRoot=null,pendingDevUI=false,pendingDevAction=null,agentReady=false,agentReadyBuffer='';
function applyPendingDev(){if(!pendingDevAction||!agentReady||!lastVisibleState||pendingRestart)return;const action=pendingDevAction;pendingDevAction=null;requestRestart(action);}
const buildStatus=require('./build-status.cjs').createBuildStatus({app,devHome,root,send,channel:canUpdateBinary||process.mas?'release':'local',onDevReady:(next,compatibility)=>{
  pendingDevRoot=next;pendingDevUI=!compatibility.sameUI;
  pendingDevAction=compatibility.sameHost?'restart':'update';applyPendingDev();
}});
function checkBuildUpdates(){
  if(devHome){const child=spawn(process.execPath,[join(devHome,'sync-runner.mjs')],{env:{...process.env,ELECTRON_RUN_AS_NODE:'1'},stdio:'ignore'});child.on('error',()=>buildStatus.poll());child.on('exit',()=>buildStatus.poll());}
  else void desktopUpdater.check();
}
ipcMain.on('check-build-updates',event=>{if(event.sender===window?.webContents)checkBuildUpdates();});

function send(channel, data) { if(channel==='build-status'){const item=Menu.getApplicationMenu()?.getMenuItemById('aesel-build-status');if(item)item.label=[data.channel==='dev'?'Dev':data.channel==='release'?'Release':'Local',data.version,(data.tree||data.revision)?.slice(0,8),({current:'Up to date',ready:'Update ready',modified:'Local changes',unknown:'Unable to verify',checking:'Checking…',syncing:'Syncing…',downloading:'Downloading…'})[data.status]||'Unable to verify'].filter(Boolean).join(' · ');}
if (window && !window.isDestroyed()) window.webContents.send(channel, data); }
function start() {
  if (terminal) return;
  const args = [join(root, 'src/tui.mjs'), '--cwd', workspace];
  for (const name of ['--piece', '--resume', '--model', '--backend', '--effort']) if (option(name)) args.push(name, option(name));
  if (continueSession) args.push('--continue-session');
  if (supplied.includes('--no-autopublish')) args.push('--no-autopublish');
  const env = { ...process.env, ELECTRON_RUN_AS_NODE: '1', TERM: 'xterm-256color', COLORTERM: 'truecolor', SLAB_HOME: slabHome, EASEL_DESKTOP: '1', EASEL_KEEP_PREVIEW:keepPreviewOnStart?'1':'0', EASEL_HOST_PID:String(process.pid), EASEL_HOST_WINDOW_ID:window.getMediaSourceId().split(':')[1], EASEL_THEME:'slab', EASEL_MOUSE:'0', EASEL_DESKTOP_SESSION:sessionFile, EASEL_DESKTOP_CONTROL:controlFile, EASEL_DESKTOP_INTENT:intentFile, EASEL_PREVIEW_EVENTS:previewEventsFile };
  delete env.NODE_OPTIONS;
  delete env.NO_COLOR;
  env.FORCE_COLOR = '3';
  env.EASEL_GROUND = 'paint';
  try {
    // Native extraction can discard mode bits; repair only our own launcher.
    if(process.platform!=='win32'&&!process.mas){
      const helper=join(devHome?join(root,'desktop'):app.isPackaged?join(process.resourcesPath,'app.asar.unpacked'):__dirname,'node_modules/node-pty/build/Release/spawn-helper');
      if(existsSync(helper))chmodSync(helper,0o755);
    }
    terminal = process.mas
      ? require('./mas-terminal.cjs').createMasTerminal(args, { ...terminalSize, cwd: workspace, env })
      : pty.spawn(process.execPath, args, { name: 'xterm-256color', ...terminalSize, cwd: workspace, env });
    agentReady=false;agentReadyBuffer='';
    terminal.onData(data => {send('output', data);if(!agentReady){agentReadyBuffer+=data;if(agentReadyBuffer.includes('\x1b]777;easel-agent-ready\x07')){agentReady=true;applyPendingDev();}agentReadyBuffer=agentReadyBuffer.slice(-64);}});
    terminal.onExit(({ exitCode }) => {
      terminal = null;
      if (exitCode === 75 && existsSync(controlFile)) {
        const request = JSON.parse(readFileSync(controlFile, 'utf8'));
        rmSync(controlFile, {force:true});
        if (request.action === 'restart') {
          pendingRestart = false; continueSession = true; keepPreviewOnStart = true;
          timer?.close();
          send('desktop-notice','Agent restarted. Your window and preview stay open.');
          if(pendingDevRoot){root=pendingDevRoot;pendingDevRoot=null;buildStatus.adopt(root);if(pendingDevUI){pendingDevUI=false;window.loadFile(join(root,'desktop/index.html'));return;}}
          start(); return;
        }
        if (request.action === 'update') {
          quitting = true;
          timer?.close();
          return desktopUpdater.afterCheckpoint(request.action);
        }
      }
      if (exitCode === 0) { quitting = true; app.quit(); return; }
      pendingRestart = false; send('output', `\r\nAesel closed (${exitCode}). Close this window to finish.\r\n`); if (quitting) app.quit(); });
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
        const visible = JSON.stringify({ piece: state.piece, handle:state.handle || "", handleColors:state.handle_colors || null, status:state.state, url: state.scan_url, version: state.piece_version, publication:state.piece_published_at || "", channel:state.piece_channel || "", flow: state.flow, medium: state.artifact_kind || 'piece', preview: state.artifact_preview || null });
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
            try { const loaded=await localPreview(workspace,state.artifact_preview);currentPaperPath=loaded?.mime==='application/pdf'?loaded.internalPath:'';currentPreviewPath=loaded?.internalPath||'';currentPreviewMime=loaded?.mime||'';currentPreviewName=state.piece||'';if(loaded)delete loaded.internalPath;previewData={localPreview:loaded}; }
            catch (error) { currentPaperPath=currentPreviewPath=currentPreviewMime=currentPreviewName='';previewData = {previewError:error.message}; }
          }
          lastVisibleState = {...JSON.parse(visible), qr, ...previewData};
          send('state', lastVisibleState);applyPendingDev();
        }
      }
    } catch {}
    finally {updating=false;if(updateAgain){updateAgain=false;void update();}}
  };
  timer = watch(dir, () => { update(); });
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
    { label: devHome?'Aesel Dev':'Aesel', submenu: [
      {id:'aesel-build-status',label:devHome?'Dev · checking…':'Release · checking…',enabled:false},
      {role:'about'}, {type:'separator'}, {role:'close',accelerator:'CmdOrCtrl+W'},
      {label:'Check for Updates…', click:checkBuildUpdates},
      {label:'Restart Agent', click:()=>requestRestart('restart')},
      {label:'Reload Interface', click:()=>window.webContents.reload()},
      {label:'Restart App', click:()=>requestRestart('update')},
      {type:'separator'}, {role:'hide'}, {role:'hideOthers'}, {role:'unhide'},
      {type:'separator'}, {role:'quit'},
    ] },
    {label:'File',submenu:[{label:'New Window',accelerator:'CmdOrCtrl+N',click:openNewWindow}]},
    { role: 'editMenu' },
    { label: 'View', submenu: [
      {label:'Aesel Actions…',click:()=>{const gallery=new BrowserWindow({parent:window,width:1120,height:850,title:'Aesel actions',backgroundColor:'#241d35',webPreferences:{contextIsolation:true,nodeIntegration:false,sandbox:true}});gallery.loadFile(join(__dirname,'donkey-gallery.html'));}},
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
  window = new BrowserWindow({ width: 760, height: 540, title: devHome?'Aesel Dev':'aesel', backgroundColor: '#463264',
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
  const uiRoot = devHome ? join(root,'desktop') : existsSync(join(root,'desktop-ui','index.html')) ? join(root,'desktop-ui') : __dirname;
  window.loadFile(join(uiRoot, 'index.html'));
  if (canUpdateBinary) {
    const firstCheck = setTimeout(() => desktopUpdater.check(), 30000); firstCheck.unref();
  }
  window.on('close', event => {
    if (terminal) { event.preventDefault(); if (!quitting) { quitting = true; terminal.kill('SIGTERM'); setTimeout(() => { if (terminal) { quitting = false; send('desktop-notice','The session is still saving or working. Retry closing when it is ready.'); } }, 6000).unref(); } }
  });
});
ipcMain.on('ready', event => { if (event.sender === window?.webContents) { send('theme',currentTheme); buildStatus.poll(); send('instance-label',address.label); if(systemTextSize)send('system-text-size',systemTextSize); sendDisplay(true); if(lastVisibleState)send('state',lastVisibleState); if(terminal)terminal.kill('SIGWINCH'); else start(); } });
// Only hide while the user is actually quitting. A stale or replayed closing
// phase must not make a healthy restarted window unreachable.
ipcMain.on('closing', event => { if(event.sender===window?.webContents && quitting) window.hide(); });
ipcMain.on('input', (event, data) => { if (event.sender === window?.webContents && typeof data === 'string' && data.length < 1_048_576) terminal?.write(data); });
ipcMain.on('size', (event, { cols, rows } = {}) => {
  if (event.sender === window?.webContents && Number.isInteger(cols) && Number.isInteger(rows) && cols >= 32 && cols <= 500 && rows >= 10 && rows <= 300) { if(cols!==terminalSize.cols||rows!==terminalSize.rows){terminalSize={cols,rows};terminal?.resize(cols,rows);} }
});
app.on('window-all-closed', () => { buildStatus.close();systemTextWatcher?.close(); timer?.close(); stopFrameCapture(); themeFollower.close(); terminal?.kill(); app.quit(); });

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
ipcMain.on('open-paper', event => { if(isWindow(event)&&currentPaperPath&&process.platform==='darwin')try{spawn('/usr/bin/open',['-a','Preview',currentPaperPath],{detached:true,stdio:'ignore'}).unref();}catch{} });
ipcMain.on('preview-drag', event => {
  if(!isWindow(event)||!currentPreviewPath||!existsSync(currentPreviewPath))return;
  const ext={'image/png':'.png','audio/wav':'.wav','application/pdf':'.pdf','application/x-gameboy-rom':'.gb'}[currentPreviewMime];if(!ext)return;
  const stem=String(currentPreviewName||'easel').replace(/\.(mjs|lisp|lua)$/i,'').replace(/[^a-z0-9._-]+/gi,'-').replace(/^-+|-+$/g,'').slice(0,80)||'easel';
  try{mkdirSync(dragDir,{recursive:true,mode:0o700});const file=join(dragDir,stem+ext);copyFileSync(currentPreviewPath,file);let icon=currentPreviewMime==='image/png'?nativeImage.createFromPath(file):nativeImage.createFromPath(join(__dirname,'assets/aesel-icon.png'));if(icon.isEmpty())return;icon=icon.resize({width:64,height:64});event.sender.startDrag({file,icon});}catch{}
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
