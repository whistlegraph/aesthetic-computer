async function boot() {
const donkey = window.AeselDonkey.createDonkey({canvas:document.getElementById("aesel-donkey")});
window.addEventListener("beforeunload",()=>donkey.destroy(),{once:true});
await document.fonts.load('16px "AC aesel Unifont"');
let savedTextSize = 16;
try { const size = Number(localStorage.getItem('easel-text-size')); if (size >= 8 && size <= 48) savedTextSize = size; } catch {}
let bitmapFont = true;
const terminal = new Terminal({ fontFamily: '"AC aesel Unifont", Menlo, monospace', fontSize: savedTextSize,
  fontWeight: '400', fontWeightBold: '400', lineHeight: 1, letterSpacing: 0, cursorBlink: false, scrollback: 0,
  allowTransparency:true,
  theme: { background: '#00000000', foreground: '#ffffff', cursor: '#ff64ff' }, allowProposedApi: false });
const fit = new FitAddon.FitAddon();
terminal.loadAddon(fit);
terminal.open(document.getElementById('terminal'));
terminal.options.linkHandler={activate:(_event,url)=>window.aesel.openLink(url)};
const terminalLinks=window.AeselTerminalLinks.attach(terminal,{open:url=>window.aesel.openLink(url),onError:console.error});
window.aesel.onTheme(theme => {
  terminal.options.theme = {...theme,background:"#00000000"};
  window.SpriteLand.theme(theme.background);
  resize();
  document.getElementById('qr-label').style.setProperty('--qr-status-shadow',theme.cursor);
  for (const [name,value] of Object.entries({background:theme.background,foreground:theme.foreground,accent:theme.cursor})) document.documentElement.style.setProperty('--aesel-'+name,value);
});
try {
  const gpu = new WebglAddon.WebglAddon();
  gpu.onContextLoss(() => gpu.dispose());
  terminal.loadAddon(gpu);
} catch (error) { console.info('Terminal using fallback renderer:', error.message); }
let resizeFrame = 0, resizeEndTimer = null;
let sentGrid = "";
const layoutProperties = new Map();
function layoutProperty(key,value){if(layoutProperties.get(key)===value)return;layoutProperties.set(key,value);document.documentElement.style.setProperty(key,value);}
function fitTerminal(){const dimensions=fit.proposeDimensions();if(!dimensions)return;const {cols,rows}=dimensions;if(cols!==terminal.cols||rows!==terminal.rows)terminal.resize(cols,rows);const grid=`${cols}:${rows}`;if(grid!==sentGrid){sentGrid=grid;window.aesel.size(cols,rows);}}
let publication = "";
let fullscreenState = { app: false, preview: false };
let previewAspect = 1.5, previewWidth = 192, previewHeight = 128;
let pieceDimensions = null;
window.setPreviewDimensions = (width, height) => {
  if (![width, height].every(n => Number.isFinite(n) && n > 0 && n <= 32768)) return;
  previewWidth = width; previewHeight = height; previewAspect = width / height;
  sizePreviewBox(); scalePreview();
};
function sizeQr() {
  donkey.setScale(terminal.options.fontSize/8);
  layoutProperty('--aesel-qr-label-size',`${terminal.options.fontSize}px`);
  layoutProperty('--ambient-qr-size',`${(document.getElementById('qr').width || 82) / 2 * terminal.options.fontSize / 8}px`);
}
function sizePreviewBox() {
  sizeQr();
  // Unifont has 16 source pixels per em; companion pixels are twice that scale.
  layoutProperty("--aesel-size", `${terminal.options.fontSize * 8}px`);
  const compact = Math.min(terminal.options.fontSize * 11, innerWidth * .7, innerHeight * .65 * previewAspect);
  const modules=(document.getElementById("qr").width||82)/2;
  const dpr=window.devicePixelRatio||1;
  const stageHeight=112*terminal.options.fontSize/8;
  const largeQr=modules*Math.max(3,Math.round(terminal.options.fontSize/4));
  const hasQr=!document.getElementById("donkey-qr-card").hidden;
  const expanded=Math.min(compact*3,innerWidth*.7,(hasQr?Math.max(compact/previewAspect,innerHeight-stageHeight-largeQr-52):innerHeight*.65)*previewAspect);
  layoutProperty("--preview-qr-size",`${Math.max(1,Math.floor((Math.min(compact,compact/previewAspect)-4)*dpr/modules))*modules/dpr}px`);
  layoutProperty("--donkey-qr-size",`${largeQr}px`);
  layoutProperty("--donkey-qr-bottom",`${stageHeight+20}px`);
  for (const [key,value] of Object.entries({
    'preview-width':compact+2,'preview-height':compact/previewAspect+2,
    'preview-expanded-width':expanded+2,'preview-expanded-height':expanded/previewAspect+2,
    'preview-logical-width':previewWidth,'preview-logical-height':previewHeight,
    'preview-fullscreen-width':Math.min(innerWidth,innerHeight*previewAspect),
    'preview-fullscreen-height':Math.min(innerWidth/previewAspect,innerHeight),
  })) layoutProperty('--'+key,value+'px');
}
window.aesel.onDisplay(display => {
  if (!pieceDimensions) {
    pieceDimensions = [Math.round(128 * display.width / display.height), 128];
    if (!window.currentPreviewMedium || window.currentPreviewMedium === 'piece') window.setPreviewDimensions(...pieceDimensions);
  }
});
let shelfHeight=84;
function paintScene(){window.SpriteLand.layout({scale:terminal.options.fontSize/8,shelf:shelfHeight,startup:document.body.dataset.phase==='startup'});}
function sizeShelf(){const screen=document.querySelector('.xterm-screen')?.getBoundingClientRect();if(screen){shelfHeight=innerHeight-screen.top-screen.height+(screen.height/terminal.rows)*4;layoutProperty('--aesel-shelf-height',`${shelfHeight}px`);}paintScene();}
function reportTitleGeometry(){
  const label=document.getElementById('qr-label'),r=label.getBoundingClientRect();
  const size=Math.max(24,Math.min(96,terminal.options.fontSize*3));
  const visible=!label.hidden&&document.body.dataset.phase!=='startup'&&!fullscreenState.preview;
  window.aesel.titleGeometry?.({x:visible?r.right+6:0,y:visible?Math.max(0,r.top+(r.height-size)/2):0,size,visible,title:label.textContent,titleX:r.x,titleY:r.y,titleWidth:r.width,titleHeight:r.height,titleFontSize:terminal.options.fontSize*2,titleColors:label.titleColors||[]});
}
new ResizeObserver(()=>reportTitleGeometry()).observe(document.getElementById('qr-label'));
function resize() {
  if (resizeFrame) return;
  resizeFrame = requestAnimationFrame(() => {
    resizeFrame = 0;
    sizePreviewBox();
    if (!fullscreenState.preview) { fitTerminal(); sizeShelf(); }
    scalePreview();
    reportTitleGeometry();
  });
}
window.aesel.onNotice(message => {
  const notice = document.getElementById('desktop-notice');
  notice.textContent = message;
  notice.hidden = !message;
});
window.aesel.onTextSize(action => {
  clearSelectionForInput();
  const maximum = Math.min(48, Math.floor((document.getElementById('terminal').clientWidth - 20) / (32 * (bitmapFont ? .5 : .61))));
  const size = action === 'reset' ? (bitmapFont ? 16 : 14) : terminal.options.fontSize + (action === 'larger' ? 2 : -2);
  terminal.options.fontSize = Math.max(8, Math.min(maximum, size));
  try { localStorage.setItem('easel-text-size', String(terminal.options.fontSize)); } catch {}
  resize(); terminal.focus();
});
window.aesel.onFont(bitmap => {
  bitmapFont = bitmap;
  terminal.options.fontFamily = bitmap ? '"AC aesel Unifont", Menlo, monospace' : 'Menlo, monospace';
  terminal.options.fontSize = bitmap ? 16 : 14;
  terminal.options.fontWeightBold = bitmap ? '400' : 'bold';
  resize();
  terminal.focus();
});
// The desktop starts the TUI with EASEL_MOUSE=0, leaving ordinary drags to
// xterm selection. Keep the selected frame stable while the TUI repaints.
const terminalElement = document.getElementById('terminal');
let draggingSelection = false, pendingOutput = '', pastedInput = false;
let clickOrigin = null;
let lastHoverCell = '', hoverFrame = 0, pendingHover = null;
terminal.parser.registerOscHandler(777, data => {
  if(data.startsWith('easel-phase:')) { document.body.dataset.phase=data.slice(12); if(data.slice(12)==='closing') window.aesel.closing(); else resize(); return true; }
  if (!data.startsWith('easel-pointer:')) return false;
  const action=data.slice('easel-pointer:'.length);
  terminalElement.dataset.pointer = (['about','profile'].includes(action)||action.startsWith('settings:')||action.startsWith('choice:')) ? 'link' : '';
  return true;
});
function sendHover(x,y) {
  const cell=`${x};${y}`;
  if(cell===lastHoverCell)return;
  lastHoverCell=cell;
  window.aesel.input(`\x1b[<35;${cell}M`);
}
terminalElement.addEventListener('pointermove', event => {
  if(event.buttons || draggingSelection || terminal.hasSelection())return;
  pendingHover={x:event.clientX,y:event.clientY};
  if(hoverFrame)return;
  hoverFrame=requestAnimationFrame(()=>{
    hoverFrame=0;
    if(!pendingHover || draggingSelection || terminal.hasSelection())return;
    const rect=terminalElement.querySelector('.xterm-screen').getBoundingClientRect();
    const x=Math.floor((pendingHover.x-rect.left)/(rect.width/terminal.cols))+1;
    const y=Math.floor((pendingHover.y-rect.top)/(rect.height/terminal.rows))+1;
    sendHover(x>0&&x<=terminal.cols?x:0,y>0&&y<=terminal.rows?y:0);
  });
});
terminalElement.addEventListener('pointerleave',()=>{
  pendingHover=null;
  terminalElement.dataset.pointer='';
  if(!draggingSelection&&!terminal.hasSelection())sendHover(0,0);
});

function flushOutput() {
  if (!pendingOutput) return;
  const output = pendingOutput; pendingOutput = '';
  terminal.write(output);
}
function clearSelectionForInput() {
  draggingSelection = false;
  terminal.clearSelection();
  flushOutput();
}
window.aesel.onOutput(data => {
  if (typeof data !== 'string') return;
  if (draggingSelection || terminal.hasSelection()) {
    pendingOutput += data;
    // Bound retained output for a selection left open during a long response.
    if (pendingOutput.length > 1024 * 1024) clearSelectionForInput();
  } else terminal.write(data);
});
terminal.onSelectionChange(() => {
  if (!draggingSelection && !terminal.hasSelection()) flushOutput();
});
terminalElement.addEventListener('pointerdown', event => {
  if (event.button === 0) { draggingSelection = true; clickOrigin = {x:event.clientX,y:event.clientY}; }
}, true);
function finishSelection() {
  draggingSelection = false;
  if (!terminal.hasSelection()) flushOutput();
}
window.addEventListener('pointerup', event => {
  if (event.button === 0 && clickOrigin && !terminal.hasSelection() && !terminalLinks.isLinkAtClientPoint(event.clientX,event.clientY) &&
      Math.hypot(event.clientX-clickOrigin.x,event.clientY-clickOrigin.y) < 4) {
    const rect = terminalElement.querySelector('.xterm-screen').getBoundingClientRect();
    const x = Math.floor((event.clientX-rect.left) / (rect.width/terminal.cols))+1;
    const y = Math.floor((event.clientY-rect.top) / (rect.height/terminal.rows))+1;
    if (x>0 && x<=terminal.cols && y>0 && y<=terminal.rows) window.aesel.input(`\x1b[<0;${x};${y}M`);
  }
  clickOrigin = null;
  finishSelection();
});
window.addEventListener('pointercancel', finishSelection);
window.addEventListener('blur', finishSelection);
terminalElement.addEventListener('contextmenu', event => {
  event.preventDefault();
  window.aesel.contextMenu(terminal.getSelection());
});
window.aesel.onSelectAll(() => terminal.selectAll());
window.aesel.onPaste(text => {
  if (!text) return;
  clearSelectionForInput();
  terminal.focus();
  // xterm applies the terminal's bracketed-paste mode and newline handling.
  pastedInput = true;
  try { terminal.paste(text); } finally { pastedInput = false; }
});
terminal.attachCustomKeyEventHandler(event => {
  const command = event.metaKey || (event.ctrlKey && !event.altKey);
  const key = event.key.toLowerCase();
  if (command && key === 'c' && terminal.hasSelection()) {
    event.preventDefault();
    if (event.type === 'keydown') window.aesel.copyText(terminal.getSelection());
    return false;
  }
  if (command && key === 'v') {
    event.preventDefault();
    if (event.type === 'keydown') window.aesel.requestPaste();
    return false;
  }
  if (event.metaKey && key === 'a') {
    event.preventDefault();
    if (event.type === 'keydown') terminal.selectAll();
    return false;
  }
  if (event.type === 'keydown' && !['Meta', 'Control', 'Shift', 'Alt'].includes(event.key)) clearSelectionForInput();
  return true;
});
// No terminal scrollback: the TUI owns its transcript and fixed footer.
let wheelDistance = 0;
terminalElement.addEventListener('wheel', event => {
  if (event.ctrlKey || event.metaKey || !event.deltaY) return;
  event.preventDefault();
  wheelDistance += event.deltaY * (event.deltaMode === 1 ? 16 : event.deltaMode === 2 ? 240 : 1);
  if (Math.abs(wheelDistance) < 80) return;
  clearSelectionForInput();
  window.aesel.input(wheelDistance < 0 ? '\x1b[5~' : '\x1b[6~');
  wheelDistance = 0;
}, { passive: false });
terminal.onData(data => {
  if (!pastedInput) clearSelectionForInput();
  window.aesel.input(data);
});
new ResizeObserver(resize).observe(document.getElementById('terminal'));
const preview = document.getElementById('piece');
const artifact = document.getElementById('artifact');
const previewViewport = document.getElementById('preview-viewport');
// The guest keeps a display-aspect viewport, 128 pixels high. Only its composited surface
// scales; hover/pinned/fullscreen never change the piece's layout or resolution.
function scalePreview() {
  const rect = artifact.getBoundingClientRect();
  const border = fullscreenState.preview ? 0 : 2;
  const width = rect.width - border, height = rect.height - border;
  const scale = Math.max(0, Math.min(width / previewWidth, height / previewHeight));
  const left = (width - previewWidth * scale) / 2;
  const top = (height - previewHeight * scale) / 2;
  previewViewport.style.transform = `translate(${left}px, ${top}px) scale(${scale})`;
}
new ResizeObserver(scalePreview).observe(artifact);
window.addEventListener('resize', () => {
  document.body.classList.add('window-resizing');
  clearTimeout(resizeEndTimer);
  resizeEndTimer=setTimeout(()=>document.body.classList.remove('window-resizing'),150);
  resize();
});
window.aesel.onPreviewMode(mode => {
  document.body.dataset.previewMode = mode;
  scalePreview();
});
sizePreviewBox(); scalePreview();
let focusBeforePreview = null;
window.aesel.onFullscreenState(state => {
  const entering = state.preview && !fullscreenState.preview;
  const leaving = !state.preview && fullscreenState.preview;
  if (entering) focusBeforePreview = document.activeElement;
  fullscreenState = state;
  document.body.classList.toggle('preview-fullscreen', state.preview);
  if (leaving) {
    resize();
    if (focusBeforePreview?.isConnected && typeof focusBeforePreview.focus === 'function') focusBeforePreview.focus();
    else terminal.focus();
    focusBeforePreview = null;
  } else if (!state.preview) resize();
});
// Capture before xterm: leaving the preview must not interrupt an agent turn.
window.addEventListener('keydown', event => {
  if (event.key !== 'Escape' || !fullscreenState.preview) return;
  event.preventDefault();
  event.stopImmediatePropagation();
  window.aesel.fullscreen('preview');
}, true);
const titleLabel=document.getElementById('qr-label');titleLabel.tabIndex=0;
const showQr=show=>{document.body.dataset.qrPreview=String(show&&!document.getElementById('qr-card').hidden);};
titleLabel.addEventListener('pointerenter',()=>showQr(true));titleLabel.addEventListener('pointerleave',()=>showQr(false));
titleLabel.addEventListener('focus',()=>showQr(true));titleLabel.addEventListener('blur',()=>showQr(false));
window.addEventListener('blur',()=>showQr(false));
window.aesel.onNativeTitle?.(ready=>{document.body.dataset.nativeTitle=String(ready);});
for(const id of ['qr-card','donkey-qr-card'])document.getElementById(id).addEventListener('click',()=>{if(shareUrl)window.aesel.openPiece(shareUrl);});
const hoverPreview=document.getElementById('artifact-shell');
hoverPreview.addEventListener('pointerenter',()=>{document.body.dataset.previewQr='true';});
hoverPreview.addEventListener('pointerleave',()=>{document.body.dataset.previewQr='false';});
let shareUrl = '';
let url = '', version = 0, qrFingerprint = '';
window.aesel.onState(state => {
  donkey.update(state);
  resize();
  shareUrl = state.url || '';
  document.getElementById('artifact-shell').hidden = !state.url && !state.preview;
  if (state.medium !== 'piece' && url) { preview.src = 'about:blank'; url = ''; }
  window.currentPreviewMedium = state.medium;
  if (state.medium === 'piece') window.setPreviewDimensions(...(pieceDimensions || [192, 128]));
  window.renderMediaPreview(state).catch(error => console.error('Preview failed:', error));
  const qr = document.getElementById('qr');
  document.getElementById('qr-card').hidden = !state.qr || !shareUrl;
  document.getElementById('donkey-qr-card').hidden = !state.qr || !shareUrl;
  const draftId = state.medium !== 'piece' ? /[?&]id=([a-f0-9]{32})/.exec(state.url || '')?.[1] : null;
  const scanName = state.preview?.publicCode ? '#' + state.preview.publicCode : draftId ? '#~' + draftId.slice(0,12) : (state.piece || 'easel').replace(/\.(mjs|lisp|lua)$/, '');
  const displayName=(state.piece||scanName).split('/').at(-1).replace(/\.(mjs|lisp|lua)$/,'');
  const handle=String(state.handle||'').replace(/^@/,'');
  window.updateQrLabel(handle?`@${handle}/${displayName}`:displayName, state.status || 'ready');
  const label=document.getElementById('qr-label');
  label.titleColors=Array.from(label.textContent).map((_,i)=>handle&&i<=handle.length?state.handleColors?.[i]||[255,255,255]:[255,255,255]);
  label.querySelectorAll('.qr-letter-ink').forEach((ink,i)=>{ink.style.color=`rgb(${label.titleColors[i]||[255,255,255]})`;});
  document.body.dataset.proxRock=String(!!state.proxName);
  document.getElementById('qr-label').hidden=!state.url&&!state.preview;
  if (!document.getElementById('qr-card').hidden && JSON.stringify(state.qr) !== qrFingerprint) {
    qrFingerprint = JSON.stringify(state.qr);
    const scale = 2, quiet = 4, span = state.qr.length + quiet * 2;
    qr.width = qr.height = span * scale;
    const ctx = qr.getContext('2d');
    ctx.fillStyle = '#fff'; ctx.fillRect(0, 0, qr.width, qr.height);
    ctx.fillStyle = '#000';
    state.qr.forEach((row, y) => row.forEach((dark, x) => { if (dark) ctx.fillRect((x + quiet) * scale, (y + quiet) * scale, scale, scale); }));
    const companionQr=document.getElementById('donkey-qr');companionQr.width=qr.width;companionQr.height=qr.height;companionQr.getContext('2d').drawImage(qr,0,0);
    qr.title = state.url;
    sizeQr();
  }
  document.title = state.piece ? `${state.piece} · aesel` : 'aesel';
  if(state.medium==='piece' && state.url===url && state.publication && state.publication!==publication) {
    const target = new URL(preview.src);
    target.searchParams.set('nolabel','true');target.searchParams.set('nogap','true');target.searchParams.set('autoreload','true');
    preview.loadURL(target.href);
  }
  publication=state.publication || '';
  if (state.medium === 'piece' && state.url && state.url !== url) {
    url = state.url;
    preview.src = `https://${url.replace(/^https?:\/\//, '').replace(/^prompt.ac\//, 'aesthetic.computer/')}?nogap=true&nolabel=true&autoreload=true`;
  }
  document.getElementById('version').textContent = state.version ? `v${state.version}${state.preview?.sourceAhead ? ' · build needed' : ''}${state.flow && state.flow !== 'live' ? ` · ${state.flow}` : ''}` : '';
  if (state.medium === 'piece' && state.version && state.version !== version) { version = state.version; artifact.classList.remove('refresh'); requestAnimationFrame(() => artifact.classList.add('refresh')); }
});
preview.addEventListener('did-finish-load', () => { artifact.classList.remove('refresh'); requestAnimationFrame(() => artifact.classList.add('refresh')); });
sizePreviewBox();
fitTerminal();
sizeShelf();
window.aesel.ready();
terminal.focus();
}
boot().catch(error => { document.getElementById('terminal').textContent = `Could not start terminal: ${error.message}`; });
