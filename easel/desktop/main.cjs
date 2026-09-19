// The Aesel Studio host: one process, one Dock icon, any number of windows.
// Each window owns a workspace, a PTY running the TUI, a preview, and a letter.
const { app, BrowserWindow, ipcMain, shell, Menu, clipboard, screen, nativeImage, net } = require('electron');
const pty = require('node-pty');
const { spawn } = require('node:child_process');
const { join, resolve } = require('node:path');
const { chmodSync, copyFileSync, mkdirSync, readdirSync, readFileSync, writeFileSync, watch, existsSync, rmSync, renameSync } = require('node:fs');
const {followSlabTheme,FALLBACK} = require('./slab-theme.cjs');
const {randomUUID} = require('node:crypto');
const {localPreview} = require('./local-preview.cjs');
const NetClock = require('./net-clock.js');
const {createUpdater} = require('./updater.cjs');
const {startFrameCapture} = require('./frame-capture.cjs');
const { tmpdir, homedir } = require('node:os');
const {readPieceApp, registerRunning, listProjects} = require('./piece-app.cjs');
const studio = require('./studio.cjs');
const pieceApp = readPieceApp(join(process.resourcesPath, 'piece-app.json'));

const devHome=process.env.AESEL_DEV_HOME||'';
app.setName(pieceApp?.name || (devHome?'Aesel Dev':'aesel'));
const stateRoot = pieceApp?.stateRoot || join(app.getPath('appData'), devHome?'Aesel Dev':'Easel');
// Each named app gets its own OS singleton; saved project sessions remain shared
// with Studio, so switching presentation never forks the conversation.
app.setPath('userData', pieceApp ? join(stateRoot, 'app-profiles', pieceApp.id) : stateRoot);
// A second launch hands its arguments to the running Studio, which opens or
// focuses the window for that workspace instead of becoming a second Dock icon.
// Chromium reorders the argv it relays (switches first, values last), so the
// launch sends its own well-formed argv along with the lock request.
const launch = studio.launchOptions(process.argv.slice(app.isPackaged ? 1 : 2));
if (!app.requestSingleInstanceLock({argv: launch.argv})) { app.quit(); return; }
const host = pieceApp?.id || 'studio';
const releaseProject = pieceApp ? registerRunning(pieceApp) : () => {};
process.once('exit', releaseProject);

// Windows by their renderer's webContents id. A preview webview belongs to the
// window that embeds it.
const sessions = new Map();
let lastFocused = null, sessionCount = 0;
const sessionOf = event => sessions.get(event.sender.id);
const sessionForContents = contents => sessions.get(contents.id) || (contents.hostWebContents ? sessions.get(contents.hostWebContents.id) : undefined);
const findSession = (workspace, instance) => [...sessions.values()].find(s => s.workspace === workspace && s.instance === instance);
function current() {
  const focused = BrowserWindow.getFocusedWindow();
  const session = (focused && sessions.get(focused.webContents.id)) || (lastFocused && sessions.has(lastFocused.window.webContents.id) ? lastFocused : null);
  return session || sessions.values().next().value || null;
}
function broadcast(channel, data) { for (const session of sessions.values()) session.send(channel, data); }

let currentTheme = FALLBACK;
const dragDir=join(tmpdir(),`easel-drag-${process.pid}`);
let systemTextSize = null, systemTextWatcher = null;
const systemTextSizePath = join(homedir(), '.local', 'share', 'slab', 'state', 'prompt-text-size.json');
function readSystemTextSize() {
  try {
    const value = JSON.parse(readFileSync(systemTextSizePath, 'utf8'));
    if (Number.isInteger(value.fontSize) && value.fontSize >= 8 && value.fontSize <= 48) {
      systemTextSize = {fontSize:value.fontSize, mode:['far','near','tiny'].includes(value.mode) ? value.mode : ''};
      broadcast('system-text-size', systemTextSize);
    }
  } catch {}
}
try { systemTextWatcher = watch(join(homedir(), '.local', 'share', 'slab', 'state'), (_event,name) => { if (name === 'prompt-text-size.json') readSystemTextSize(); }); } catch {}
readSystemTextSize();
const themeFollower = followSlabTheme(theme => { currentTheme = theme; for (const session of sessions.values()) if (!session.window.isDestroyed()) session.window.setBackgroundColor(theme.background); broadcast('theme',theme); });
const startedAt = Date.now();
let root = process.env.AESEL_DEV_ROOT || (app.isPackaged ? join(process.resourcesPath, 'easel') : resolve(__dirname, '..'));
const qrEncoder = import(require('node:url').pathToFileURL(join(root, 'src/vendor/qr.mjs')).href);
const launchFile = join(app.getPath('userData'), 'last-workspace.json');
let lastWorkspace = '';
try { const saved = JSON.parse(readFileSync(launchFile,'utf8')); if (typeof saved.cwd === 'string' && existsSync(saved.cwd)) lastWorkspace = saved.cwd; } catch {}
const bootWorkspace = resolve(pieceApp?.workspace || launch.option('--cwd') || lastWorkspace || join(app.getPath('userData'), 'projects', 'first-piece'));
const bootInstance = pieceApp?.instance || launch.option('--instance') || 'default';
mkdirSync(app.getPath('userData'),{recursive:true,mode:0o700});
writeFileSync(launchFile,JSON.stringify({cwd:bootWorkspace}),{mode:0o600});
const addressDir = join(stateRoot, 'instance-addresses');
const sessionDir = join(stateRoot, 'sessions');
mkdirSync(sessionDir, {recursive:true,mode:0o700});

app.once('will-quit',()=>{try{rmSync(dragDir,{recursive:true,force:true});}catch{}});
app.on('second-instance',(_event,argv,_workingDirectory,data)=>{
  const request = studio.launchOptions(Array.isArray(data?.argv) && data.argv.every(a => typeof a === 'string') ? data.argv : argv.slice(app.isPackaged ? 1 : 2));
  console.log(JSON.stringify({event:'second-instance',cwd:request.option('--cwd')}));
  const cwd = request.option('--cwd');
  if (pieceApp || !cwd) { current()?.focus(); return; }
  const workspace = resolve(cwd), instance = studio.INSTANCE.test(request.option('--instance')) ? request.option('--instance') : 'default';
  const existing = findSession(workspace, instance);
  if (existing) existing.focus();
  else openSession({workspace, instance, launch:request, continueSession:false, requestedAt:Number(request.option('--window-requested-at'))||0});
});
// `open -a Easel` activates an existing macOS process without necessarily
// launching a second instance. A checkpointed restart can therefore leave the
// process healthy but its previously hidden window unreachable unless the app
// handles the ordinary Dock/reopen activation itself.
app.on('activate', () => { current()?.focus(); });

// An app restart or binary update checkpoints every window first; the last
// checkpoint to land hands control to the updater.
let appRestart = null; const awaitingCheckpoint = new Set();
function requestAppRestart(action) {
  if (appRestart) return;
  appRestart = action;
  for (const session of sessions.values()) if (session.terminal) { awaitingCheckpoint.add(session); session.requestRestart(action); }
  if (!awaitingCheckpoint.size) void desktopUpdater.afterCheckpoint(action);
}
function checkpointed(session) { awaitingCheckpoint.delete(session); if (appRestart && !awaitingCheckpoint.size) void desktopUpdater.afterCheckpoint(appRestart); }
const canUpdateBinary = !pieceApp && !devHome && app.isPackaged && !process.mas && existsSync(join(process.resourcesPath,'app-update.yml'));
const desktopUpdater = createUpdater({app, canUpdateBinary, onStatus:(status,info)=>buildStatus.release(status,info),
  requestRestart: action => { if (action === 'update') requestAppRestart('update'); else for (const session of sessions.values()) session.requestRestart('restart'); },
  prepareRelaunch: () => { for (const session of sessions.values()) session.writeContinuation(); },
  notify: message => broadcast('desktop-notice', message)});

let applicationMenuTemplate=null;
const pendingDev={root:null,ui:false};
const buildStatus=require('./build-status.cjs').createBuildStatus({app,devHome,root,send:(channel,data)=>{ if(channel==='build-status')refreshBuildMenu(data); broadcast(channel,data); },channel:canUpdateBinary||process.mas?'release':'local',onDevReady:(next,compatibility)=>{
  pendingDev.root=next;pendingDev.ui=!compatibility.sameUI;
  const action=compatibility.sameHost?'restart':'update';
  for (const session of sessions.values()) session.devPending(action);
}});
function refreshBuildMenu(data){
  const item=applicationMenuTemplate?.[0]?.submenu?.[0];
  const label=[data.channel==='dev'?'Dev':data.channel==='release'?'Release':'Local',data.version,(data.tree||data.revision)?.slice(0,8),({current:'Up to date',ready:'Update ready',modified:'Local changes',unknown:'Unable to verify',checking:'Checking…',syncing:'Syncing…',downloading:'Downloading…'})[data.status]||'Unable to verify'].filter(Boolean).join(' · ');
  if(item&&item.label!==label){item.label=label;Menu.setApplicationMenu(Menu.buildFromTemplate(applicationMenuTemplate));}
}
function checkBuildUpdates(){
  if(pieceApp){void shell.openPath(pieceApp.baseApp);broadcast('desktop-notice','Manage runtime updates in Aesel. This project app keeps its own identity.');}
  else if(devHome){const child=spawn(process.execPath,[join(devHome,'sync-runner.mjs')],{env:{...process.env,ELECTRON_RUN_AS_NODE:'1'},stdio:'ignore'});child.on('error',()=>buildStatus.poll());child.on('exit',()=>buildStatus.poll());}
  else void desktopUpdater.check();
}
const nativeTitle = require('./native-title.cjs').createNativeTitle({app});
const uiRoot = () => devHome ? join(root,'desktop') : existsSync(join(root,'desktop-ui','index.html')) ? join(root,'desktop-ui') : __dirname;

function openSession({ workspace, instance, launch, continueSession, requestedAt = 0 }) {
  mkdirSync(workspace, { recursive: true });
  const session = { workspace, instance, terminal: null, status: 'blank', closing: false, painted: false };
  const slabHome = join(tmpdir(), `easel-desktop-${process.pid}-${++sessionCount}`);
  const sessionFile = studio.sessionFile(stateRoot, workspace, instance);
  const continuationFile = sessionFile + '.continue';
  const controlFile = join(slabHome, 'desktop-control.json');
  const intentFile = join(slabHome, 'desktop-intent.json');
  const previewEventsFile=join(slabHome,'preview-events.json');
  mkdirSync(slabHome, {recursive:true,mode:0o700});
  const address = studio.claimAddress(addressDir, {instance, workspace});
  let timer = null, lastVisibleState = null, keepPreviewOnStart = false, terminalSize = {cols:100,rows:32};
  let currentPaperPath = '', currentPreviewPath = '', currentPreviewMime = '', currentPreviewName = '';
  let previewGuest=null, previewContext=null, previewEvents=[], previewSequence=0, previewFlush=null;
  const previewRecent=new Map();
  let pendingRestart = false, agentReady=false, agentReadyBuffer='', devAction=null, devReload=false;
  let previewFullscreen = false, beforePreviewFullscreen = false, displayKey = '';

  const window = new BrowserWindow({ show:false, width: 760, height: 540, title: pieceApp?.name || (devHome?'Aesel Dev':'aesel'), backgroundColor: currentTheme.background,
    webPreferences: { additionalArguments:[`--aesel-initial-theme=${JSON.stringify(currentTheme)}`], preload: join(__dirname, 'preload.cjs'), contextIsolation: true, nodeIntegration: false, sandbox: true, webviewTag: true, plugins:true } });
  session.window = window;
  const contentsId = window.webContents.id;
  sessions.set(contentsId, session);
  const send = (channel, data) => { if (!window.isDestroyed()) window.webContents.send(channel, data); };
  session.send = send;

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
  const stopFrameCapture=startFrameCapture({workspace,root,context:()=>previewContext,guest:()=>previewGuest});

  session.requestRestart = action => {
    if (!session.terminal) { if (action === 'restart') start(); else checkpointed(session); return; }
    pendingRestart = true;
    writeFileSync(intentFile, JSON.stringify({action}), {mode:0o600});
    session.terminal.kill('SIGUSR2');
  };
  session.writeContinuation = () => studio.writeContinuation(continuationFile, {workspace, instance, host});
  session.devPending = action => { devAction = action; applyPendingDev(); };
  function applyPendingDev(){
    if(!studio.readyForDevApply({action:devAction,agentReady,lastVisibleState,pendingRestart}))return;
    const action=devAction;devAction=null;devReload=pendingDev.ui;
    if(action==='update')requestAppRestart('update');else session.requestRestart('restart');
  }

  function start() {
    if (session.terminal) return;
    const args = [join(root, 'src/tui.mjs'), '--cwd', workspace];
    for (const name of ['--piece', '--resume', '--model', '--backend', '--effort']) { const value = launch.option(name) || (name === '--piece' ? pieceApp?.piece || '' : ''); if (value) args.push(name, value); }
    if (continueSession) args.push('--continue-session');
    if (launch.flag('--no-autopublish')) args.push('--no-autopublish');
    const env = { ...process.env, ELECTRON_RUN_AS_NODE: '1', TERM: 'xterm-256color', COLORTERM: 'truecolor', SLAB_HOME: slabHome, EASEL_DESKTOP: '1', EASEL_KEEP_PREVIEW:keepPreviewOnStart?'1':'0', EASEL_HOST_PID:String(process.pid), EASEL_HOST_WINDOW_ID:window.getMediaSourceId().split(':')[1], EASEL_THEME:'slab', EASEL_MOUSE:'0', EASEL_DESKTOP_SESSION:sessionFile, EASEL_DESKTOP_CONTROL:controlFile, EASEL_DESKTOP_INTENT:intentFile, EASEL_PREVIEW_EVENTS:previewEventsFile };
    delete env.NODE_OPTIONS;
    env.EASEL_HOST_BUNDLE_ID = pieceApp?.bundleId || 'computer.aesthetic.easel';
    delete env.NO_COLOR;
    env.FORCE_COLOR = '3';
    env.EASEL_GROUND = 'paint';
    try {
      // Native extraction can discard mode bits; repair only our own launcher.
      if(process.platform!=='win32'&&!process.mas){
        const helper=join(devHome?join(root,'desktop'):app.isPackaged&&app.getAppPath().endsWith('.asar')?join(process.resourcesPath,'app.asar.unpacked'):__dirname,'node_modules/node-pty/build/Release/spawn-helper');
        if(existsSync(helper))chmodSync(helper,0o755);
      }
      const terminal = session.terminal = process.mas
        ? require('./mas-terminal.cjs').createMasTerminal(args, { ...terminalSize, cwd: workspace, env })
        : pty.spawn(process.execPath, args, { name: 'xterm-256color', ...terminalSize, cwd: workspace, env });
      agentReady=false;agentReadyBuffer='';
      terminal.onData(data => {send('output', data);if(!agentReady){agentReadyBuffer+=data;if(agentReadyBuffer.includes('\x1b]777;easel-agent-ready\x07')){agentReady=true;applyPendingDev();}agentReadyBuffer=agentReadyBuffer.slice(-64);}});
      terminal.onExit(({ exitCode }) => {
        session.terminal = null;
        if (exitCode === 75 && existsSync(controlFile)) {
          const request = JSON.parse(readFileSync(controlFile, 'utf8'));
          rmSync(controlFile, {force:true});
          if (request.action === 'restart') {
            pendingRestart = false; continueSession = true; keepPreviewOnStart = true;
            timer?.close();
            send('desktop-notice','Agent restarted. Your window and preview stay open.');
            if(pendingDev.root){root=pendingDev.root;pendingDev.root=null;buildStatus.adopt(root);}
            // A fresh interface asks for the agent again once it is ready.
            if(devReload){devReload=false;window.loadFile(join(root,'desktop/index.html'));return;}
            start(); return;
          }
          if (request.action === 'update') { session.closing = true; timer?.close(); checkpointed(session); return; }
        }
        if (exitCode === 0) { session.closing = true; if (!window.isDestroyed()) window.close(); return; }
        pendingRestart = false; send('output', `\r\nAesel closed (${exitCode}). Close this window to finish.\r\n`); if (session.closing && !window.isDestroyed()) window.close(); });
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
          session.status = state.state;
          if (current() === session) themeFollower.setStatus(state.state);
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

  session.ready = () => { send('theme',currentTheme); buildStatus.poll(); send('instance-label',address.label); if(systemTextSize)send('system-text-size',systemTextSize); sendDisplay(true); if(lastVisibleState)send('state',lastVisibleState); if(session.terminal)session.terminal.kill('SIGWINCH'); else start(); };
  session.closingPhase = () => { if (session.closing) window.hide(); };
  session.write = data => session.terminal?.write(data);
  session.resize = (cols, rows) => { if(cols!==terminalSize.cols||rows!==terminalSize.rows){terminalSize={cols,rows};session.terminal?.resize(cols,rows);} };
  session.focus = () => { if (window.isDestroyed() || !session.painted) return; if (window.isMinimized()) window.restore(); window.show(); window.focus(); };
  function sendDisplay(force = false) {
    if (window.isDestroyed()) return;
    const display = screen.getDisplayMatching(window.getBounds());
    const key = `${display.id}:${display.size.width}:${display.size.height}`;
    if (force === true || key !== displayKey) { displayKey = key; send('display',{width:display.size.width,height:display.size.height}); }
  }
  session.sendDisplay = sendDisplay;
  function fullscreenState() { send('fullscreen-state', {app:window.isFullScreen() || window.isSimpleFullScreen(),preview:previewFullscreen}); }
  session.previewFullscreen = () => previewFullscreen;
  session.toggleFullscreen = target => {
    if (target === 'preview') {
      previewFullscreen = !previewFullscreen;
      if (previewFullscreen) { beforePreviewFullscreen = window.isFullScreen(); if (!beforePreviewFullscreen) { if (process.platform === 'darwin') window.setSimpleFullScreen(true); else window.setFullScreen(true); } }
      else if (!beforePreviewFullscreen) { if (process.platform === 'darwin') window.setSimpleFullScreen(false); else window.setFullScreen(false); }
    } else if (target === 'app') {
      if (previewFullscreen) { previewFullscreen = false; window.setFullScreen(beforePreviewFullscreen); }
      else window.setFullScreen(!window.isFullScreen());
    }
    fullscreenState();
  };
  session.openPaper = () => { if(currentPaperPath&&process.platform==='darwin')try{spawn('/usr/bin/open',['-a','Preview',currentPaperPath],{detached:true,stdio:'ignore'}).unref();}catch{} };
  session.dragPreview = event => {
    if(!currentPreviewPath||!existsSync(currentPreviewPath))return;
    const ext={'image/png':'.png','audio/wav':'.wav','application/pdf':'.pdf','application/x-gameboy-rom':'.gb'}[currentPreviewMime];if(!ext)return;
    const stem=String(currentPreviewName||'easel').replace(/\.(mjs|lisp|lua)$/i,'').replace(/[^a-z0-9._-]+/gi,'-').replace(/^-+|-+$/g,'').slice(0,80)||'easel';
    try{mkdirSync(dragDir,{recursive:true,mode:0o700});const file=join(dragDir,stem+ext);copyFileSync(currentPreviewPath,file);let icon=currentPreviewMime==='image/png'?nativeImage.createFromPath(file):nativeImage.createFromPath(join(__dirname,'assets/aesel-icon.png'));if(icon.isEmpty())return;icon=icon.resize({width:64,height:64});event.sender.startDrag({file,icon});}catch{}
  };

  if (pieceApp) window.on('page-title-updated', event => event.preventDefault());
  window.once('ready-to-show',()=>{session.painted=true;window.show();console.log(JSON.stringify({event:'window-presented',ms:Date.now()-startedAt,requestedMs:requestedAt?Date.now()-requestedAt:undefined}));});
  const creditLabel = require('./credit-label.cjs').startCreditLabel({app, window, root});
  session.credit = require('./credit-checkout.cjs').startCreditCheckout({app,window,root,shell,refresh:creditLabel.refresh});
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
  if (launch.option('--diagnostics')) window.webContents.once('did-finish-load', () => {
    setTimeout(async () => {
      const target = resolve(launch.option('--diagnostics'));
      mkdirSync(target, {recursive:true});
      const renderer = await window.webContents.executeJavaScript(`({font:document.fonts.check('16px "AC aesel Unifont"'), canvases:document.querySelectorAll('.xterm canvas').length, qr:!document.getElementById('qr-card').hidden})`);
      writeFileSync(join(target, 'gpu.json'), JSON.stringify({features:app.getGPUFeatureStatus(),info:await app.getGPUInfo('complete'),renderer,layout:await window.webContents.executeJavaScript('window.aeselLayoutSnapshot?.()')},null,2));
      writeFileSync(join(target, 'window.png'), (await window.webContents.capturePage()).toPNG());
    }, 3000);
  });
  window.on('move', () => sendDisplay());
  window.on('focus', () => { lastFocused = session; themeFollower.setStatus(session.status); });
  window.on('enter-full-screen', fullscreenState);
  window.on('leave-full-screen', () => { previewFullscreen = false; fullscreenState(); });
  window.loadFile(join(uiRoot(), 'index.html'));
  window.on('close', event => {
    if (session.terminal) { event.preventDefault(); if (!session.closing) { session.closing = true; session.terminal.kill('SIGTERM'); setTimeout(() => { if (session.terminal) { session.closing = false; send('desktop-notice','The session is still saving or working. Retry closing when it is ready.'); } }, 6000).unref(); } }
  });
  window.once('closed', () => {
    sessions.delete(contentsId);
    if (lastFocused === session) lastFocused = null;
    awaitingCheckpoint.delete(session);
    timer?.close(); stopFrameCapture(); creditLabel.close();
    session.terminal?.kill(); session.terminal = null;
    studio.releaseAddress(address, {instance});
    try { rmSync(slabHome, {recursive:true,force:true}); } catch {}
    if (appRestart && !awaitingCheckpoint.size) void desktopUpdater.afterCheckpoint(appRestart);
  });
  return session;
}

function openNewWindow(){
 const id=`window-${randomUUID()}`;
 const cwd=join(stateRoot,'projects',id);
 mkdirSync(cwd,{recursive:true,mode:0o700});
 if (!pieceApp) { openSession({workspace:cwd, instance:id, launch:studio.launchOptions([]), continueSession:false, requestedAt:Date.now()}); return; }
 // A project app keeps its own identity; new windows belong to the Studio.
 const env={...process.env};delete env.ELECTRON_RUN_AS_NODE;delete env.NODE_OPTIONS;
 const args=['--instance',id,'--cwd',cwd,'--window-requested-at',String(Date.now())];
 const child=spawn(pieceApp.baseExecutable,args,{detached:true,stdio:'ignore',env});
 child.on('error',error=>broadcast('desktop-notice',`Could not open a window: ${error.message}`));child.unref();
}
app.whenReady().then(() => {
  applicationMenuTemplate=[
    { label: pieceApp?.name || (devHome?'Aesel Dev':'Aesel'), submenu: [
      {id:'aesel-build-status',label:devHome?'Dev · checking…':'Release · checking…',enabled:false},
      {role:'about'}, {type:'separator'}, {role:'close',accelerator:'CmdOrCtrl+W'},
      {label:'Check for Updates…', click:checkBuildUpdates},
      {label:'Restart Agent', click:()=>current()?.requestRestart('restart')},
      {label:'Reload Interface', click:()=>current()?.window.webContents.reload()},
      {label:'Restart App', click:()=>requestAppRestart('update')},
      {type:'separator'}, {role:'hide'}, {role:'hideOthers'}, {role:'unhide'},
      {type:'separator'}, {role:'quit'},
    ] },
    {label:'File',submenu:[{label:'New Window',accelerator:'CmdOrCtrl+N',click:openNewWindow}]},
    {id:'piece-apps',label:'Projects',submenu:[]},
    { role: 'editMenu' },
    { label: 'View', submenu: [
      {label:'Aesel Actions…',click:()=>{const gallery=new BrowserWindow({parent:current()?.window,width:1120,height:850,title:'Aesel actions',backgroundColor:'#241d35',webPreferences:{contextIsolation:true,nodeIntegration:false,sandbox:true}});gallery.loadFile(join(__dirname,'donkey-gallery.html'));}},
      {label:'Larger Text',accelerator:'CmdOrCtrl+=',click:()=>current()?.send('text-size','larger')},
      {label:'Smaller Text',accelerator:'CmdOrCtrl+-',click:()=>current()?.send('text-size','smaller')},
      {label:'Reset Text Size',accelerator:'CmdOrCtrl+0',click:()=>current()?.send('text-size','reset')},
      {type:'separator'},
      { label: 'Bitmap Font', type: 'radio', checked: true, accelerator: 'CmdOrCtrl+Shift+B', click: () => broadcast('font', true) },
      { label: 'Smooth Font', type: 'radio', accelerator: 'CmdOrCtrl+Shift+M', click: () => broadcast('font', false) },
      {type:'separator'},
    ] },
    { role: 'windowMenu' },
  ];
  Menu.setApplicationMenu(Menu.buildFromTemplate(applicationMenuTemplate));
  let projectMenuState = '';
  const refreshProjects = () => {
    const projects = listProjects(stateRoot);
    const key = JSON.stringify(projects.map(p => [p.id,p.name,p.pid,p.installed,p.appPath]));
    if (key === projectMenuState) return;
    projectMenuState = key;
    const items = projects.map(p => ({label:p.name,type:'checkbox',checked:!!p.pid,enabled:p.installed,click:()=>shell.openPath(p.appPath)}));
    if (pieceApp) items.push({type:'separator'},{label:'Open Aesel',click:()=>shell.openPath(pieceApp.baseApp)});
    if (!items.length) items.push({label:'No project apps yet',enabled:false});
    applicationMenuTemplate.find(item=>item.id==='piece-apps').submenu=items;
    Menu.setApplicationMenu(Menu.buildFromTemplate(applicationMenuTemplate));
    if (app.dock) app.dock.setMenu(Menu.buildFromTemplate(items));
  };
  refreshProjects();
  const projectMenuTimer = setInterval(refreshProjects, 3000); projectMenuTimer.unref();
  app.once('will-quit', () => { clearInterval(projectMenuTimer); releaseProject(); });
  if (pieceApp && app.dock) {
    const icon = nativeImage.createFromPath(join(process.resourcesPath, 'piece-app.png'));
    if (!icon.isEmpty()) app.dock.setIcon(icon);
  }
  screen.on('display-metrics-changed', () => { for (const session of sessions.values()) session.sendDisplay(); });
  if (canUpdateBinary) {
    const firstCheck = setTimeout(() => desktopUpdater.check(), 30000); firstCheck.unref();
  }
  // Windows checkpointed by the last restart or update come back first; a
  // project app only ever resumes its own. Then the requested workspace, when
  // it was asked for explicitly or nothing else came back.
  const requestedAt = Number(launch.option('--window-requested-at')) || 0;
  for (const marker of studio.resumableSessions(sessionDir, {host})) {
    if (pieceApp && (marker.workspace !== bootWorkspace || marker.instance !== bootInstance)) continue;
    if (!findSession(marker.workspace, marker.instance)) openSession({workspace:marker.workspace, instance:marker.instance, launch, continueSession:true, requestedAt});
  }
  if (!findSession(bootWorkspace, bootInstance) && (pieceApp || launch.option('--cwd') || !sessions.size)) {
    openSession({workspace:bootWorkspace, instance:bootInstance, launch, continueSession: !!pieceApp && existsSync(studio.sessionFile(stateRoot, bootWorkspace, bootInstance)), requestedAt});
  }
});
ipcMain.on('ready', event => sessionOf(event)?.ready());
// Only hide while the window is actually closing. A stale or replayed closing
// phase must not make a healthy restarted window unreachable.
ipcMain.on('closing', event => sessionOf(event)?.closingPhase());
ipcMain.on('input', (event, data) => { if (typeof data === 'string' && data.length < 1_048_576) sessionOf(event)?.write(data); });
ipcMain.on('size', (event, { cols, rows } = {}) => {
  if (Number.isInteger(cols) && Number.isInteger(rows) && cols >= 32 && cols <= 500 && rows >= 10 && rows <= 300) sessionOf(event)?.resize(cols, rows);
});
ipcMain.on('check-build-updates',event=>{if(sessionOf(event))checkBuildUpdates();});
app.on('window-all-closed', () => { buildStatus.close();systemTextWatcher?.close(); themeFollower.close(); app.quit(); });

ipcMain.on('copy-text', (event, text) => { if (sessionOf(event) && typeof text === 'string' && text.length <= 1048576) clipboard.writeText(text); });
ipcMain.on('paste-request', event => { sessionOf(event)?.send('paste', clipboard.readText()); });
ipcMain.on('context-menu', (event, value) => {
  const session = sessionOf(event); if (!session) return;
  const selection = typeof value === 'string' ? value : value?.selection;
  const text = typeof selection === 'string' ? selection.slice(0,1048576) : '';
  Menu.buildFromTemplate([
    {label:'Copy', enabled:!!text, click:()=>clipboard.writeText(text)},
    {label:'Paste', click:()=>session.send('paste',clipboard.readText())},
    {type:'separator'}, {label:'Select All', click:()=>session.send('select-all')},
  ]).popup({window:session.window});
});
ipcMain.on('open-link',(event,value)=>{if(!sessionOf(event)||typeof value!=='string'||value.length>4096)return;try{const url=new URL(value);if(['http:','https:'].includes(url.protocol)&&!url.username&&!url.password)shell.openExternal(url.href);}catch{}});
ipcMain.on('open-piece', (event, value) => {
  if (!sessionOf(event) || typeof value !== 'string') return;
  try {
    const url = new URL(value.startsWith('https://') ? value : `https://${value}`);
    if (url.protocol === 'https:' && ['prompt.ac','aesthetic.computer'].includes(url.hostname) && !url.username && !url.password) shell.openExternal(url.href);
  } catch {}
});
ipcMain.on('open-paper', event => sessionOf(event)?.openPaper());
ipcMain.on('preview-drag', event => sessionOf(event)?.dragPreview(event));
ipcMain.on('fullscreen', (event, value) => { sessionOf(event)?.toggleFullscreen(typeof value === 'string' ? value : value?.target); });
ipcMain.handle('buy-ac-credits', event => sessionOf(event)?.credit.buy() ?? {error:'No window for this checkout.'});
ipcMain.handle('native-prox-title', (event, value) => sessionOf(event) ? nativeTitle.render(value) : null);
app.on('web-contents-created', (_event, contents) => {
  contents.on('before-input-event', (event, input) => {
    const session = sessionForContents(contents);
    if(input.type==='keyDown' && input.control && !input.meta && !input.alt && input.key.toLowerCase()==='c') { event.preventDefault(); session?.write('\x03'); return; }
    if (input.type === 'keyDown' && (input.meta || input.control) && !input.alt && ['+','=','-','0'].includes(input.key)) {
      event.preventDefault(); session?.send('text-size', input.key === '0' ? 'reset' : input.key === '-' ? 'smaller' : 'larger'); return;
    }
    if (session?.previewFullscreen() && input.type === 'keyDown' && input.key === 'Escape') {
      event.preventDefault(); session.toggleFullscreen('preview');
    }
  });
});

// The shared music clock. Chromium's stack carries a browser user agent, which
// Cloudflare wants; the renderer's CSP cannot reach the site itself. The
// renderer asks whenever it is playing; fetches are throttled here.
const netClock = NetClock.createClock({ sample: () => NetClock.sample({ fetch: net.fetch, site: process.env.EASEL_SITE || 'https://aesthetic.computer' }) });
let netClockAt = 0;
ipcMain.handle('net-clock', async event => {
 if (!sessionOf(event)) return null;
 if (Date.now() - netClockAt > 5000) { netClockAt = Date.now(); try { await netClock.resync(); } catch {} }
 return netClock.toJSON();
});
// Native menus live outside the renderer viewport; return only the chosen index.
ipcMain.handle('notebook-context-menu', (event, items) => {
 const session = sessionOf(event);
 if(!session||!Array.isArray(items)||items.length>32)return -1;
 return new Promise(resolve=>{
  let chosen=-1;
  const template=items.map((item,index)=>item?.separator?{type:'separator'}:{label:String(item?.label||'').slice(0,160),enabled:item?.enabled!==false,click:()=>{chosen=index;}});
  Menu.buildFromTemplate(template).popup({window:session.window,callback:()=>resolve(chosen)});
 });
});
