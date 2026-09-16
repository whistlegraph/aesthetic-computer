async function boot() {
const donkey = window.AeselDonkey.createDonkey({canvas:document.getElementById("aesel-donkey")});
window.addEventListener("beforeunload",()=>donkey.destroy(),{once:true});
await document.fonts.load('16px "AC Easel Unifont"');
let savedTextSize = 16;
try { const size = Number(localStorage.getItem('easel-text-size')); if (size >= 8 && size <= 48) savedTextSize = size; } catch {}
let bitmapFont = true;
const terminal = new Terminal({ fontFamily: '"AC Easel Unifont", Menlo, monospace', fontSize: savedTextSize,
  fontWeight: '400', fontWeightBold: '400', lineHeight: 1, letterSpacing: 0, cursorBlink: false, scrollback: 0,
  theme: { background: '#463264', foreground: '#ffffff', cursor: '#ff64ff' }, allowProposedApi: false });
const fit = new FitAddon.FitAddon();
terminal.loadAddon(fit);
terminal.open(document.getElementById('terminal'));
window.easel.onTheme(theme => {
  terminal.options.theme = theme;
  document.getElementById('qr-label').style.setProperty('--qr-status-shadow',theme.cursor);
  for (const [name,value] of Object.entries({background:theme.background,foreground:theme.foreground,accent:theme.cursor})) document.documentElement.style.setProperty('--easel-'+name,value);
});
try {
  const gpu = new WebglAddon.WebglAddon();
  gpu.onContextLoss(() => gpu.dispose());
  terminal.loadAddon(gpu);
} catch (error) { console.info('Terminal using fallback renderer:', error.message); }
let resizeFrame = 0;
let publication = "";
let fullscreenState = { app: false, preview: false };
let previewAspect = 1.5, previewWidth = 192, previewHeight = 128;
let pieceDimensions = null;
window.setPreviewDimensions = (width, height) => {
  if (![width, height].every(n => Number.isFinite(n) && n > 0 && n <= 32768)) return;
  previewWidth = width; previewHeight = height; previewAspect = width / height;
  sizePreviewBox(); scalePreview();
};
function sizePreviewBox() {
  // Unifont has 16 source pixels per em; companion pixels are twice that scale.
  document.documentElement.style.setProperty("--aesel-size", `${terminal.options.fontSize * 8}px`);
  const compact = Math.min(terminal.options.fontSize * 11, innerWidth * .7, innerHeight * .65 * previewAspect);
  const expanded = Math.min(compact * 3, innerWidth * .7, innerHeight * .65 * previewAspect);
  for (const [key,value] of Object.entries({
    'preview-width':compact+2,'preview-height':compact/previewAspect+2,
    'preview-expanded-width':expanded+2,'preview-expanded-height':expanded/previewAspect+2,
    'preview-logical-width':previewWidth,'preview-logical-height':previewHeight,
    'preview-fullscreen-width':Math.min(innerWidth,innerHeight*previewAspect),
    'preview-fullscreen-height':Math.min(innerWidth/previewAspect,innerHeight),
  })) document.documentElement.style.setProperty('--'+key,value+'px');
}
window.easel.onDisplay(display => {
  if (!pieceDimensions) {
    pieceDimensions = [Math.round(128 * display.width / display.height), 128];
    if (!window.currentPreviewMedium || window.currentPreviewMedium === 'piece') window.setPreviewDimensions(...pieceDimensions);
  }
});
function resize() {
  if (resizeFrame || fullscreenState.preview) return;
  resizeFrame = requestAnimationFrame(() => { resizeFrame = 0; sizePreviewBox(); if (fullscreenState.preview) return; fit.fit(); window.easel.size(terminal.cols, terminal.rows); });
}
window.easel.onNotice(message => {
  const notice = document.getElementById('desktop-notice');
  notice.textContent = message;
  notice.hidden = !message;
});
window.easel.onTextSize(action => {
  clearSelectionForInput();
  const maximum = Math.min(48, Math.floor((document.getElementById('terminal').clientWidth - 20) / (32 * (bitmapFont ? .5 : .61))));
  const size = action === 'reset' ? (bitmapFont ? 16 : 14) : terminal.options.fontSize + (action === 'larger' ? 2 : -2);
  terminal.options.fontSize = Math.max(8, Math.min(maximum, size));
  try { localStorage.setItem('easel-text-size', String(terminal.options.fontSize)); } catch {}
  resize(); terminal.focus();
});
window.easel.onFont(bitmap => {
  bitmapFont = bitmap;
  terminal.options.fontFamily = bitmap ? '"AC Easel Unifont", Menlo, monospace' : 'Menlo, monospace';
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
  if(data.startsWith('easel-phase:')) { document.body.dataset.phase=data.slice(12); resize(); return true; }
  if (!data.startsWith('easel-pointer:')) return false;
  const action=data.slice('easel-pointer:'.length);
  terminalElement.dataset.pointer = (['about','profile'].includes(action)||action.startsWith('settings:')||action.startsWith('choice:')) ? 'link' : '';
  return true;
});
function sendHover(x,y) {
  const cell=`${x};${y}`;
  if(cell===lastHoverCell)return;
  lastHoverCell=cell;
  window.easel.input(`\x1b[<35;${cell}M`);
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
window.easel.onOutput(data => {
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
  if (event.button === 0 && clickOrigin && !terminal.hasSelection() &&
      Math.hypot(event.clientX-clickOrigin.x,event.clientY-clickOrigin.y) < 4) {
    const rect = terminalElement.querySelector('.xterm-screen').getBoundingClientRect();
    const x = Math.floor((event.clientX-rect.left) / (rect.width/terminal.cols))+1;
    const y = Math.floor((event.clientY-rect.top) / (rect.height/terminal.rows))+1;
    if (x>0 && x<=terminal.cols && y>0 && y<=terminal.rows) window.easel.input(`\x1b[<0;${x};${y}M`);
  }
  clickOrigin = null;
  finishSelection();
});
window.addEventListener('pointercancel', finishSelection);
window.addEventListener('blur', finishSelection);
terminalElement.addEventListener('contextmenu', event => {
  event.preventDefault();
  window.easel.contextMenu(terminal.getSelection());
});
window.easel.onSelectAll(() => terminal.selectAll());
window.easel.onPaste(text => {
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
    if (event.type === 'keydown') window.easel.copyText(terminal.getSelection());
    return false;
  }
  if (command && key === 'v') {
    event.preventDefault();
    if (event.type === 'keydown') window.easel.requestPaste();
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
  window.easel.input(wheelDistance < 0 ? '\x1b[5~' : '\x1b[6~');
  wheelDistance = 0;
}, { passive: false });
terminal.onData(data => {
  if (!pastedInput) clearSelectionForInput();
  window.easel.input(data);
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
window.addEventListener('resize', () => { sizePreviewBox(); scalePreview(); });
window.easel.onPreviewMode(mode => {
  document.body.dataset.previewMode = mode;
  scalePreview();
});
sizePreviewBox(); scalePreview();
let focusBeforePreview = null;
window.easel.onFullscreenState(state => {
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
  window.easel.fullscreen('preview');
}, true);
document.getElementById('qr-card').addEventListener('click',()=>{ if(shareUrl)window.easel.openPiece(shareUrl); });
let shareUrl = '';
let url = '', version = 0, qrFingerprint = '';
window.easel.onState(state => {
  donkey.update(state);
  shareUrl = state.url || '';
  document.getElementById('artifact-shell').hidden = !state.url && !state.preview;
  if (state.medium !== 'piece' && url) { preview.src = 'about:blank'; url = ''; }
  window.currentPreviewMedium = state.medium;
  if (state.medium === 'piece') window.setPreviewDimensions(...(pieceDimensions || [192, 128]));
  window.renderMediaPreview(state).catch(error => console.error('Preview failed:', error));
  const qr = document.getElementById('qr');
  document.getElementById('qr-card').hidden = !state.qr || !shareUrl;
  const draftId = state.medium !== 'piece' ? /[?&]id=([a-f0-9]{32})/.exec(state.url || '')?.[1] : null;
  const scanName = state.preview?.publicCode ? '#' + state.preview.publicCode : draftId ? '#~' + draftId.slice(0,12) : (state.piece || 'easel').replace(/\.(mjs|lisp|lua)$/, '');
  window.updateQrLabel(scanName + (state.version ? ` v${state.version}` : ''), state.status || 'ready');
  if (!document.getElementById('qr-card').hidden && JSON.stringify(state.qr) !== qrFingerprint) {
    qrFingerprint = JSON.stringify(state.qr);
    const scale = 2, quiet = 4, span = state.qr.length + quiet * 2;
    qr.width = qr.height = span * scale;
    const ctx = qr.getContext('2d');
    ctx.fillStyle = '#fff'; ctx.fillRect(0, 0, qr.width, qr.height);
    ctx.fillStyle = '#000';
    state.qr.forEach((row, y) => row.forEach((dark, x) => { if (dark) ctx.fillRect((x + quiet) * scale, (y + quiet) * scale, scale, scale); }));
    qr.title = state.url;
  }
  document.title = state.piece ? `${state.piece} · Easel` : 'Easel';
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
fit.fit();
window.easel.size(terminal.cols, terminal.rows);
window.easel.ready();
terminal.focus();
}
boot().catch(error => { document.getElementById('terminal').textContent = `Could not start terminal: ${error.message}`; });
