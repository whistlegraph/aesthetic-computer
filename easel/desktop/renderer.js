async function boot() {
const donkey = {setScale(){},update(){},destroy(){}};
window.addEventListener("beforeunload",()=>donkey.destroy(),{once:true});
await document.fonts.load('16px "AC aesel Unifont"');
let savedTextSize = 16;
try { const size = Number(localStorage.getItem('easel-text-size')); if (size >= 8 && size <= 48) savedTextSize = size; } catch {}
let bitmapFont = false;
const terminal = new Terminal({ fontFamily: 'Menlo, monospace', fontSize: savedTextSize,
  fontWeight: '400', fontWeightBold: '500', lineHeight: 1.15, letterSpacing: 0, cursorBlink: false, cursorInactiveStyle: 'none', scrollback: 0,
  allowTransparency:true,
  theme: { background: '#00000000', foreground: '#ffffff', cursor: '#ff64ff' }, allowProposedApi: false });
const fit = new FitAddon.FitAddon();
terminal.loadAddon(fit);
terminal.open(document.getElementById('terminal'));
terminal.write('\x1b[?25l'); // The proportional input owns the visible caret, including after UI reload.
terminal.options.linkHandler={activate:(_event,url)=>window.aesel.openLink(url)};
const terminalLinks={isLinkAtClientPoint:()=>false,dispose:()=>{}};
window.aesel.onTheme(theme => {
  terminal.options.theme = {...theme,background:"#00000000"};
  window.SpriteLand.theme(theme.background);
  window.setNotebookInk(theme.background);
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
let fullscreenState = { app: false, preview: false };
let previewAspect = 1.5, previewWidth = 192, previewHeight = 128;
let pieceDimensions = null;
let previewDensity=1;
window.setPreviewDimensions = (width, height) => {
  if (![width, height].every(n => Number.isFinite(n) && n > 0 && n <= 32768)) return;
  previewWidth = width; previewHeight = height; previewAspect = width / height;
  sizePreviewBox(); scalePreview();
};
function sizeQr() {
  layoutProperty('--prose-font-size',`${terminal.options.fontSize}px`);
  layoutProperty('--notebook-line-height',`${terminal.options.fontSize*1.5}px`);
  window.alignNotebookRuling?.();
  donkey.setScale(terminal.options.fontSize/8);
  layoutProperty('--aesel-qr-label-size',`${terminal.options.fontSize}px`);
  layoutProperty('--ambient-qr-size',`${(document.getElementById('qr').width || 82) / 2 * terminal.options.fontSize / 8}px`);
}
function sizePreviewBox() {
  sizeQr();
  // Unifont has 16 source pixels per em; companion pixels are twice that scale.
  layoutProperty("--aesel-size", `${terminal.options.fontSize * 8}px`);
  const availablePreviewHeight=Math.max(24,innerHeight-110);
  const ordinaryCompact = Math.min(availablePreviewHeight*previewAspect,176, innerWidth * .28, innerHeight * .32 * previewAspect);
  const compact = window.currentPreviewMedium === 'paper'
    ? Math.min(ordinaryCompact, innerWidth * .28, innerHeight * .34 * previewAspect)
    : ordinaryCompact;
  const modules=(document.getElementById("qr").width||82)/2;
  const dpr=window.devicePixelRatio||1;
  const stageHeight=112*terminal.options.fontSize/8;
  const largeQr=modules*Math.max(3,Math.round(terminal.options.fontSize/4));
  const hasQr=!document.getElementById("donkey-qr-card").hidden;
  // Keep the page above the mascot/shelf region. The preview is absolutely
  // positioned, so this changes only its visual footprint—not terminal or
  // bottom-interface column widths.
  const paperExpanded=Math.min(innerWidth*.88,availablePreviewHeight*previewAspect);
  const expanded=window.currentPreviewMedium==='paper'
    ? Math.max(compact,paperExpanded)
    : Math.min(compact*3,innerWidth*.78,availablePreviewHeight*previewAspect);
  layoutProperty("--preview-qr-size",`${Math.max(1,Math.floor((Math.min(compact,compact/previewAspect)-4)*dpr/modules))*modules/dpr}px`);
  layoutProperty("--donkey-qr-size",`${largeQr}px`);
  layoutProperty("--donkey-qr-bottom",`${stageHeight+20}px`);
  for (const [key,value] of Object.entries({
    'preview-width':compact+12,'preview-height':compact/previewAspect+12,
    'preview-expanded-width':expanded+12,'preview-expanded-height':expanded/previewAspect+12,
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
let shelfHeight=84, trayTop=innerHeight-84, trayRows=4;
function paintScene(){window.SpriteLand.layout({scale:terminal.options.fontSize/8,shelf:shelfHeight,shelfTop:trayTop,startup:document.body.dataset.phase==='startup'});}
function sizeShelf(){
  const grid=document.querySelector('.xterm-screen')?.getBoundingClientRect();
  if(grid && terminal.rows>0){
    const rowHeight=grid.height/terminal.rows;
    const raw=grid.top+Math.max(0,terminal.rows-trayRows)*rowHeight;
    const dpr=window.devicePixelRatio||1;
    trayTop=Math.round(raw*dpr)/dpr;
    shelfHeight=innerHeight-trayTop;
    layoutProperty('--aesel-tray-top',`${trayTop}px`);
    layoutProperty('--aesel-shelf-height',`${shelfHeight}px`);
    layoutProperty('--aesel-shelf-height',`${shelfHeight}px`);
    document.body.dataset.trayRows=String(trayRows);
    const companion=Math.max(16,Math.floor(Math.min(trayRows*rowHeight-8,grid.width/terminal.cols*10-12)/2)*2);
    layoutProperty('--tray-companion-size',`${companion}px`);
    layoutProperty('--tray-companion-bottom',`${innerHeight-grid.bottom+2}px`);

  }
  paintScene();
}
new ResizeObserver(()=>resize()).observe(document.getElementById('terminal'));
window.aeselLayoutSnapshot=()=>{
  const grid=document.querySelector('.xterm-screen').getBoundingClientRect();
  const wood=document.getElementById('scene-foreground').getBoundingClientRect();
  const credits=document.getElementById('credit-label')?.getBoundingClientRect();
  const companion=document.getElementById('aesel-donkey').getBoundingClientRect();
  return {rows:terminal.rows,columns:terminal.cols,trayRows,rowHeight:grid.height/terminal.rows,
    expectedTop:grid.top+(terminal.rows-trayRows)*grid.height/terminal.rows,woodTop:wood.top,
    gridBottom:grid.bottom,creditsTop:credits?.top,devicePixelRatio,companionTop:companion.top,companionBottom:companion.bottom,
    animatedLetters:document.querySelectorAll('.native-prox-letter .qr-letter-ink').length};
};

function reportTitleGeometry(){
  const label=document.getElementById('qr-label'),r=label.getBoundingClientRect();
  const size=Math.max(24,Math.min(96,terminal.options.fontSize*3));
  const visible=!label.hidden&&document.body.dataset.phase!=='startup'&&!fullscreenState.preview;
  window.aesel.titleGeometry?.({x:visible?r.right+6:0,y:visible?Math.max(0,r.top+(r.height-size)/2):0,size,visible,title:label.dataset.title||label.textContent,titleX:r.x,titleY:r.y,titleWidth:r.width,titleHeight:r.height,titleFontSize:16,titleColors:label.titleColors||[]});
}
new ResizeObserver(()=>reportTitleGeometry()).observe(document.getElementById('qr-label'));
function resize() {
  if (resizeFrame) return;
  resizeFrame = requestAnimationFrame(() => {
    resizeFrame = 0;
    if (!fullscreenState.preview) { fitTerminal(); sizeShelf(); }
    sizePreviewBox();
    scalePreview();
    reportTitleGeometry();
  });
}
let noticeTimer = null;
window.aesel.onNotice(message => {
  const notice = document.getElementById('desktop-notice');
  if(/^(?:Agent restarted\.|Desktop thread restored|Reloading this development build|.*up to date)/i.test(message)){notice.hidden=true;notice.textContent='';clearTimeout(noticeTimer);return;}
  notice.textContent = message.replace(/AC credits/g,'braincells');
  notice.hidden = !message;
  clearTimeout(noticeTimer);
  if (/agent restarted|thread restored|up to date/i.test(message)) {
    noticeTimer = setTimeout(() => { notice.hidden = true; notice.textContent = ''; }, 3000);
  }
});
window.aesel.onTextSize(action => {
  if(document.body.dataset.previewZone==='resize'||document.querySelector('#artifact-shell[data-resizing]'))return;
  if(document.body.dataset.previewZone==='picture'||fullscreenState.preview){
    previewDensity=Math.max(.25,Math.min(4,action==='reset'?1:previewDensity*(action==='larger'?1.2:1/1.2)));
    try{preview.setZoomFactor(previewDensity);}catch{}
    return;
  }
  clearSelectionForInput();
  const maximum = Math.min(48, Math.floor((document.getElementById('terminal').clientWidth - 20) / (32 * (bitmapFont ? .5 : .61))));
  const size = action === 'reset' ? (bitmapFont ? 16 : 14) : terminal.options.fontSize + (action === 'larger' ? 2 : -2);
  terminal.options.fontSize = Math.max(8, Math.min(maximum, size));
  textSizeMode = '';
  try { localStorage.setItem('easel-text-size', String(terminal.options.fontSize)); } catch {}
  reportZoom(); resize(); terminal.focus();
});
window.aesel.onFont(bitmap => {
  bitmapFont = bitmap;
  terminal.options.fontFamily = bitmap ? '"AC aesel Unifont", Menlo, monospace' : 'Menlo, monospace';
  terminal.options.fontSize = bitmap ? 16 : 14;
  terminal.options.fontWeightBold = bitmap ? '400' : '500';
  terminal.options.lineHeight = bitmap ? 1 : 1.15;
  textSizeMode = ''; reportZoom(); resize();
  terminal.focus();
});
// The desktop starts the TUI with EASEL_MOUSE=0, leaving ordinary drags to
// xterm selection. Keep the selected frame stable while the TUI repaints.
const terminalElement = document.getElementById('terminal');
// Proportional prompt ink; the existing terminal still owns keyboard editing.
const promptLine=document.createElement('div');promptLine.id='prose-prompt';promptLine.hidden=true;
promptLine.setAttribute('aria-hidden','true');document.getElementById('notebook-page').append(promptLine);
const promptFeedback=document.createElement('span');promptFeedback.id='prompt-feedback';promptFeedback.hidden=true;document.body.append(promptFeedback);
window.installNotebookDonkey(promptFeedback);
window.placeNotebookActivity=()=>{
 const last=Array.from(document.querySelectorAll('#notebook-page article[data-kind="user"]')).at(-1);
 const target=promptState?.text?promptLine:(last?.lastElementChild||last||promptLine);
 if(promptFeedback.parentElement!==target)target.append(promptFeedback);
};
let promptState=null;
function positionPrompt(){
 const rowHeight=terminal.options.fontSize*1.5;
 promptLine.style.fontSize=`${terminal.options.fontSize}px`;
 promptLine.style.lineHeight=`${rowHeight}px`;
 promptLine.style.minHeight=`${rowHeight}px`;
 window.alignNotebookRuling?.();
}

function updatePrompt(value){
 const draftChanged=promptState&&(promptState.text!==value.text||promptState.cursor!==value.cursor);
 promptState=value;promptLine.hidden=!!value.hidden;positionPrompt();
 promptFeedback.textContent='';promptFeedback.setAttribute('role','img');promptFeedback.setAttribute('aria-label',value.feedback||'Idle');promptFeedback.hidden=!!value.hidden||!value.feedback;
 const chars=Array.from(value.text||''),index=Math.max(0,Math.min(chars.length,value.cursor??chars.length));
 const before=document.createTextNode(chars.slice(0,index).join(''));
 const caret=document.createElement('span');caret.className='prose-prompt-caret';
 promptLine.replaceChildren(before,caret,document.createTextNode(chars.slice(index).join('')));
 window.placeNotebookActivity();
 positionPrompt();
 if(draftChanged)caret.scrollIntoView({block:'nearest',inline:'nearest'});
 window.alignNotebookRuling?.();
}
promptLine.addEventListener('pointerdown',event=>{event.preventDefault();terminal.focus();});
new ResizeObserver(positionPrompt).observe(terminalElement);
window.addEventListener('resize',positionPrompt);

let draggingSelection = false, pendingOutput = '', pastedInput = false;
let clickOrigin = null;
let lastHoverCell = '', hoverFrame = 0, pendingHover = null;
terminal.parser.registerOscHandler(777, data => {
  if(data==='easel-camera:request'){window.flashPreviewCapture?.();return true;}
  if(data.startsWith('easel-gamepad:')){try{forwardGamepad(JSON.parse(data.slice('easel-gamepad:'.length)));}catch{}return true;}
  if(data.startsWith('easel-prompt:')){try{updatePrompt(JSON.parse(data.slice('easel-prompt:'.length)));}catch{}return true;}
  if(data.startsWith('easel-conversation:')){try{window.updateConversation(JSON.parse(data.slice('easel-conversation:'.length)));}catch{}return true;}
  if(data.startsWith('easel-version-preview:')){try{window.receiveVersionPreview(JSON.parse(data.slice('easel-version-preview:'.length)));}catch{}return true;}
  if(data.startsWith('easel-binding-result:')){try{window.notebookBindingResult(JSON.parse(data.slice('easel-binding-result:'.length)));}catch{}return true;}
  if(data.startsWith('easel-provider:')){try{window.updateProviderFooter(JSON.parse(data.slice('easel-provider:'.length)));}catch{}return true;}
  if(data.startsWith('easel-layout:')) {
    try { const layout=JSON.parse(data.slice('easel-layout:'.length));
      if(Number.isInteger(layout.trayRows)&&layout.trayRows>=2&&layout.trayRows<=8) {trayRows=layout.trayRows;resize();}
    } catch {}
    return true;
  }
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
// Aesel chrome owns this menu; piece webviews keep their own input handling.
const contextMenu = document.createElement('div');
contextMenu.id = 'aesel-context-menu';
contextMenu.setAttribute('role', 'menu');
contextMenu.setAttribute('aria-label', 'Aesel');
contextMenu.hidden = true;
document.body.append(contextMenu);
let contextFocus = null;
let nativeMenuItems=[];
function closeContext(restore = false) {
  contextMenu.hidden = true;
  if (restore) { contextFocus?.focus?.(); terminal.focus(); }
}
function contextItem(label, action, disabled = false) {
  nativeMenuItems.push({label,enabled:!disabled,action});
  const button = document.createElement('button');
  button.type = 'button'; button.textContent = label;
  button.setAttribute('role', 'menuitem'); button.disabled = disabled;
  button.addEventListener('click', () => { closeContext(); action(); });
  contextMenu.append(button);
}
function contextSeparator() {
  nativeMenuItems.push({separator:true});
  const separator = document.createElement('hr');
  separator.setAttribute('role', 'separator'); contextMenu.append(separator);
}
document.addEventListener('contextmenu', event => {
  if (event.target.closest('input:not(.xterm-helper-textarea),textarea:not(.xterm-helper-textarea),[contenteditable="true"]')) return;
  event.preventDefault(); contextFocus = document.activeElement;
  document.getElementById('provider-menu').close?.();
  document.getElementById('provider-menu').hidden = true;
  document.getElementById('credit-label').setAttribute('aria-expanded', 'false');
  const selection = window.getSelection()?.toString() || terminal.getSelection() || '';
  const word=event.target.closest('.notebook-word');
  const article=event.target.closest('article[data-kind="assistant"]');
  const concept=(article&&(selection.trim()||word?.textContent)||'').trim().slice(0,160);
  const excerpt=article?.raw||article?.textContent||'';
  contextMenu.replaceChildren();nativeMenuItems=[];
  if(concept){
    const ask=action=>{window.getSelection()?.removeAllRanges();window.aesel.input(window.notebookConceptPacket(action,concept,excerpt));terminal.focus();};
    contextItem('Explain simply',()=>ask('explain'));
    contextItem('Give an example',()=>ask('example'));
    contextItem('Open on Wikipedia',()=>window.aesel.openLink('https://en.wikipedia.org/w/index.php?search='+encodeURIComponent(concept)));
    contextSeparator();
  }
  contextItem('Copy', () => window.aesel.copyText(selection||concept), !selection&&!concept);
  contextItem('Paste', () => window.aesel.requestPaste());
  contextItem('Select all', () => { const page=document.getElementById('notebook-page');if(page&&!document.getElementById('conversation').hidden){const range=document.createRange();range.selectNodeContents(page);const selection=window.getSelection();selection.removeAllRanges();selection.addRange(range);}else{terminal.selectAll();terminal.focus();} });
  contextSeparator();
  contextItem('Open piece in browser', () => window.aesel.openPiece(shareUrl), !shareUrl);
  contextItem('Copy piece link', () => window.aesel.copyText(shareUrl), !shareUrl);
  contextSeparator();
  contextItem('Settings…', () => document.getElementById('credit-label').click());
  contextItem(fullscreenState.app ? 'Leave fullscreen' : 'Fullscreen', () => window.aesel.fullscreen('app'));
  if(window.aesel.nativeContextMenu){
    const choices=nativeMenuItems;
    window.aesel.nativeContextMenu(choices.map(({label,enabled,separator})=>({label,enabled,separator}))).then(index=>{if(Number.isInteger(index)&&index>=0)choices[index]?.action?.();});
    return;
  }
  contextMenu.hidden = false;
  const bounds = contextMenu.getBoundingClientRect();
  contextMenu.style.left = `${Math.max(6, Math.min(event.clientX, innerWidth - bounds.width - 6))}px`;
  contextMenu.style.top = `${Math.max(6, Math.min(event.clientY, innerHeight - bounds.height - 6))}px`;
  contextMenu.querySelector('button:not(:disabled)')?.focus();
});
document.addEventListener('pointerdown', event => {
  if (!contextMenu.contains(event.target)) closeContext();
}, true);
window.addEventListener('blur', () => closeContext());
window.addEventListener('resize', () => closeContext());
document.addEventListener('keydown', event => {
  if (contextMenu.hidden) return;
  if (event.key === 'Escape' || event.key === 'Tab') {
    event.preventDefault(); event.stopImmediatePropagation(); closeContext(true); return;
  }
  const buttons = [...contextMenu.querySelectorAll('button:not(:disabled)')];
  const index = buttons.indexOf(document.activeElement);
  if (['ArrowDown', 'ArrowUp', 'Home', 'End'].includes(event.key)) {
    event.preventDefault(); event.stopImmediatePropagation();
    const next = event.key === 'Home' ? 0 : event.key === 'End' ? buttons.length - 1 :
      (index + (event.key === 'ArrowDown' ? 1 : -1) + buttons.length) % buttons.length;
    buttons[next]?.focus();
  }
}, true);
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
window.installNotebookFocus({focusInput:()=>terminal.focus(),sendInput:data=>(window.aesel||window.aesel).input(data),preview});
let gamepadReady=false,gamepadSending=false,gamepadPending=null;
async function forwardGamepad(pads){
 gamepadPending=document.hasFocus()&&!document.getElementById('provider-menu')?.open?pads:[];
 if(!gamepadReady||gamepadSending)return;
 gamepadSending=true;
 try{while(gamepadPending!==null&&gamepadReady){const next=gamepadPending;gamepadPending=null;await preview.executeJavaScript(window.aeselGamepadScript(next));}}catch{}finally{gamepadSending=false;}
}
preview.addEventListener('did-start-loading',()=>{gamepadReady=false;});
preview.addEventListener('dom-ready',()=>{gamepadReady=true;});
window.addEventListener('blur',()=>forwardGamepad([]));

const artifact = document.getElementById('artifact');
window.installPreviewShutter(artifact);
document.getElementById('version').setAttribute('aria-label','Piece version');
const previewViewport = document.getElementById('preview-viewport');
// The guest keeps a display-aspect viewport, 128 pixels high. Only its composited surface
// scales; hover/pinned/fullscreen never change the piece's layout or resolution.
function scalePreview() {
  const rect = artifact.getBoundingClientRect();
  const border = fullscreenState.preview ? 0 : 2;
  const width = rect.width - border, height = rect.height - border;
  if((!window.currentPreviewMedium||window.currentPreviewMedium==='piece')&&width>0&&height>0){
    // Reframe only when the user changes the logical viewport. Hover and
    // notebook redraws composite its existing surface without guest resizes.
    previewViewport.style.transform=`scale(${width/previewWidth},${height/previewHeight})`;return;
  }
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
titleLabel.setAttribute('role','link');titleLabel.title='Open piece in browser';
titleLabel.addEventListener('click',()=>{if(shareUrl)window.aesel.openPiece(shareUrl);});
titleLabel.addEventListener('keydown',event=>{if(event.key==='Enter'||event.key===' '){event.preventDefault();titleLabel.click();}});
window.installProxHover(titleLabel);
window.installPreviewResize?.();
const showQr=()=>{document.body.dataset.qrPreview='false';};
window.aesel.onNativeTitle?.(ready=>{document.body.dataset.nativeTitle=String(ready);});
for(const id of ['qr-card','donkey-qr-card'])document.getElementById(id).addEventListener('click',()=>{if(shareUrl)window.aesel.openPiece(shareUrl);});
const hoverPreview=document.getElementById('artifact-shell');
hoverPreview.addEventListener('pointerenter',()=>{document.body.dataset.previewQr='false';});

hoverPreview.addEventListener('click',()=>{if(window.currentPreviewMedium==='paper')window.aesel.openPaper?.();});
let shareUrl = '';
let url = '', version = 0, qrFingerprint = '';
let instanceLabel = '', titlePiece = '', textSizeMode = '';
function reportZoom() {
  const label=document.getElementById('instance-label');label.textContent=instanceLabel;
  label.title=textSizeMode?`Slab ${textSizeMode[0].toUpperCase()+textSizeMode.slice(1)} text · ${terminal.options.fontSize} pt`:`${terminal.options.fontSize} pt`;
}
function updateWindowTitle() {
  const mark = /^[A-Z]$/.test(instanceLabel) ? String.fromCodePoint(0x1D56C + instanceLabel.charCodeAt(0) - 65) : `[${instanceLabel}]`;
  document.title = titlePiece || 'Aesel';
}
window.aesel.onInstanceLabel(label => {
  instanceLabel = label;
  document.body.dataset.instance = label;
  reportZoom();
  updateWindowTitle();
});
window.aesel.onSystemTextSize?.(value => {
  textSizeMode = value.mode || '';
  // Unifont's bitmap body reads smaller than Terminal's Menlo at the same
  // nominal point size. A three-point optical correction keeps them aligned.
  const easelSize=Math.min(48,value.fontSize+3);
  terminal.options.fontSize = easelSize;
  try { localStorage.setItem('easel-text-size', String(easelSize)); } catch {}
  reportZoom(); resize();
});
window.aesel.onState(state => {
  if(document.body.dataset.phase==='startup' && state.status) document.body.dataset.phase='ready';
  donkey.update(state);
  resize();
  shareUrl = state.url || ''; 
  document.getElementById('artifact-shell').hidden = !state.url && !state.preview;
  if (state.medium !== 'piece' && url) { preview.src = 'about:blank'; url = ''; }
  window.currentPreviewMedium = state.medium;
  document.body.dataset.previewMedium = state.medium || '';
  if (state.medium === 'piece') window.setPreviewDimensions(...(window.previewUserDimensions || pieceDimensions || [192, 128]));
  window.renderMediaPreview(state).catch(error => console.error('Preview failed:', error));
  const qr = document.getElementById('qr');
  const hideQr = state.medium === 'paper' || !state.qr || !shareUrl;
  document.getElementById('qr-card').hidden = hideQr;
  document.getElementById('donkey-qr-card').hidden = hideQr;
  const draftId = state.medium !== 'piece' ? /[?&]id=([a-f0-9]{32})/.exec(state.url || '')?.[1] : null;
  const scanName = state.preview?.publicCode ? '#' + state.preview.publicCode : draftId ? '#~' + draftId.slice(0,12) : (state.piece || 'easel').replace(/\.(mjs|lisp|lua)$/, '');
  const displayName=(state.piece||scanName).split('/').at(-1).replace(/\.(mjs|lisp|lua)$/,'');
  const routeHandle = /\/@([^/]+)\//.exec(state.url || '')?.[1];
  const handle=String(state.handle||routeHandle||'').replace(/^@/,'');
  window.setNotebookHandle(handle,state.handleColors);
  const title=handle?`@${handle}/${displayName}`:displayName;
  const titleColors=Array.from(title).map((_,i)=>handle&&i<=Array.from(handle).length?state.handleColors?.[i]||[255,255,255]:[255,255,255]);
  window.updateQrLabel(title, state.status || 'ready', titleColors);
  const label=document.getElementById('qr-label');
  label.titleColors=titleColors;
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
  titlePiece = `aesel ${title}` + (Number.isInteger(state.version) ? ` v${state.version}` : '');
  updateWindowTitle();
  // Owned piece routes subscribe to their live channel themselves. Wait for
  // the first readable publication instead of booting prompt or racing a 403.
  const ownedRoute = /\/@[^/]+\/[^/?#]+/.test(state.url || '');
  const previewReady = !ownedRoute || !!state.publication;
  if (state.medium === 'piece' && state.url && state.url !== url && previewReady) {
    url = state.url;
    const target = new URL(`https://${url.replace(/^https?:\/\//, '').replace(/^prompt.ac\//, 'aesthetic.computer/')}`);
    target.searchParams.set('nogap','true');
    target.searchParams.set('nolabel','true');
    target.searchParams.set('autoreload','true');
    preview.src = target.href;
  }
  window.updateVersionLabel(Number.isInteger(state.version) ? `v${state.version}` : '');
  document.getElementById('version').dataset.unstable=String(!!state.preview?.sourceAhead||!!(state.flow&&state.flow!=='live'));
  if (state.medium === 'piece' && Number.isInteger(state.version) && state.version !== version) { version = state.version; artifact.classList.remove('refresh'); requestAnimationFrame(() => artifact.classList.add('refresh')); }
});
preview.addEventListener('did-finish-load', () => { artifact.classList.remove('refresh'); requestAnimationFrame(() => artifact.classList.add('refresh')); });
sizePreviewBox();
fitTerminal();
sizeShelf();
window.aesel.ready();
const reportConnection=()=>window.aesel.input(navigator.onLine?'\x1b[99;9;1~':'\x1b[99;9;0~');window.addEventListener('online',reportConnection);window.addEventListener('offline',reportConnection);if(!navigator.onLine)reportConnection();
terminal.focus();
}
boot().catch(error => { document.getElementById('terminal').textContent = `Could not start terminal: ${error.message}`; });

// The footer is the provider selector. Only hosted AC work displays a balance.
(() => {
 const footer=document.createElement('button');footer.id='credit-label';footer.type='button';
 footer.setAttribute('aria-haspopup','dialog');footer.setAttribute('aria-expanded','false');
 const menu=document.createElement('dialog');menu.id='provider-menu';menu.setAttribute('aria-label','Settings');menu.hidden=true;
 document.body.append(footer,menu);
 window.installProxHover(footer,document.getElementById('version'));
 let provider=null,credits=null,buildInfo=null,generation=0,lastText='',menuKey='';
 const balance=()=>{
  if(!credits)return null;
  if(Number.isFinite(credits.remaining)&&Number.isFinite(credits.purchased))return credits.remaining+credits.purchased;
  // Compatibility with the earlier desktop host's text-only balance payload.
  const parts=credits.text.match(/[\d,]+/g);return parts?parts.reduce((sum,n)=>sum+Number(n.replaceAll(',','')),0):null;
 };
 function close(){window.closeVersionPreview?.();menu.close();menu.hidden=true;footer.setAttribute('aria-expanded','false');}
 menu.addEventListener('cancel',event=>{event.preventDefault();close();});
 menu.addEventListener('click',event=>{if(event.target!==menu)return;const r=menu.getBoundingClientRect();if(event.clientX<r.left||event.clientX>r.right||event.clientY<r.top||event.clientY>r.bottom)close();});
 function item(text,action,{selected=false,disabled=false}={}){
  const button=document.createElement('button');button.type='button';button.textContent=text;
  button.disabled=disabled;button.dataset.selected=String(selected);
  button.addEventListener('click',action);menu.append(button);return button;
 }
 function rebuildMenu(){
  const nextKey=JSON.stringify([provider,credits,buildInfo]);if(nextKey===menuKey)return;menuKey=nextKey;
  const focused=document.activeElement?.textContent;
  const scroll=menu.scrollTop;const previewHost=menu.querySelector('.history-preview-host');const historyOpen=menu.querySelector('details')?.open??true;
  menu.replaceChildren();const n=balance();
  const header=document.createElement('header');const title=document.createElement('h2');title.textContent='Settings';
  const dismiss=document.createElement('button');dismiss.type='button';dismiss.textContent='×';dismiss.setAttribute('aria-label','Close settings');dismiss.addEventListener('click',close);header.append(title,dismiss);menu.append(header);
  if(buildInfo){const status=document.createElement('p');status.className='build-status';status.dataset.status=buildInfo.status;status.title=buildInfo.revision?`Source commit ${buildInfo.revision}`:'';const channel=buildInfo.channel==='dev'?'Dev':buildInfo.channel==='release'?'Release':'Local';const state={current:'Up to date',ready:'Update ready',modified:'Local changes · sync paused',checking:'Checking…',syncing:'Downloading dev build…',downloading:'Downloading update…',unknown:'Unable to verify'}[buildInfo.status]||'Unable to verify';status.textContent=[channel,buildInfo.version,(buildInfo.tree||buildInfo.revision)?.slice(0,8),state].filter(Boolean).join(' · ');menu.append(status);item('Check for updates',()=>window.aesel.checkBuildUpdates());}
  const current=document.createElement('p');current.textContent=[provider?.backend==='ac'?'AC':provider?.backend==='codex'?'Codex':'Claude',provider?.model].filter(Boolean).join(' · ');menu.append(current);

  for(const [index,id,name] of [[0,'ac','AC'],[1,'claude','Claude'],[2,'codex','Codex']]){
   const choice=item(name,()=>{close();if(provider?.backend!==id)window.aesel.input(`\x1b[99;${index}~`);},{selected:provider?.backend===id,disabled:!!provider?.busy});
   const icon=new Image();icon.src=`assets/provider-${id}.svg`;icon.alt='';icon.className='provider-mark';choice.prepend(icon);
  }
  const modelLabel=document.createElement('label');modelLabel.textContent='Model';modelLabel.className='settings-field';
  const modelSelect=document.createElement('select');modelSelect.setAttribute('aria-label','Model');modelSelect.disabled=!!provider?.busy||!provider?.models?.length;
  for(const [index,model] of (provider?.models||[]).entries()){
    const option=document.createElement('option');option.value=String(index);option.textContent=model.label;
    option.selected=model.id===(provider.selectedModel??provider.model);modelSelect.append(option);
  }
  if(![...(provider?.models||[])].some(model=>model.id===(provider.selectedModel??provider.model)))modelSelect.selectedIndex=0;
  const backendIndex=['ac','claude','codex'].indexOf(provider?.backend);
  modelSelect.addEventListener('change',()=>{window.aesel.input(`\x1b[99;4;${backendIndex};${modelSelect.value}~`);});
  modelLabel.append(modelSelect);menu.append(modelLabel);

  const stateLine=document.createElement('p');stateLine.className='inference-status';stateLine.textContent=[provider?.mode==='local'?'Local inference':'Remote inference',provider?.status,provider?.activity].filter(Boolean).join(' · ');menu.append(stateLine);
  if(provider?.notice){const notice=document.createElement('p');notice.className='session-notice';notice.textContent=provider.notice;menu.append(notice);}
  if(provider?.backend==='ac'){const count=document.createElement('p');count.className='braincell-balance';const icon=new Image();icon.src='assets/braincell.svg';icon.alt='';icon.className='provider-mark';count.append(icon,`${n===null?'—':n.toLocaleString('en-US')} braincells`);menu.append(count);}
  if(provider?.backend==='ac'&&credits?.offer){
   const buy=item('Buy 1,000,000 braincells · $5',async()=>{
    buy.disabled=true;buy.textContent='Opening checkout…';
    try{const result=await window.aesel.buyCredits();if(result.error){buy.textContent=result.error;buy.disabled=false;}else close();}
    catch{buy.textContent='Try checkout again';buy.disabled=false;}
   });
  }
  const history=document.createElement('details');history.open=historyOpen;
  const summary=document.createElement('summary');summary.textContent='Versions';history.append(summary);
  const versions=document.createElement('ol');versions.className='settings-versions';
  for(const revision of [...(provider?.versions||[])].reverse()){
    const row=document.createElement('li');
    const version=document.createElement('strong');version.textContent=`v${revision.version}`;
    const time=document.createElement('time');time.dateTime=revision.updatedAt;time.textContent=new Date(revision.updatedAt).toLocaleString([],{month:'short',day:'numeric',hour:'2-digit',minute:'2-digit'});
    const choose=document.createElement('button');choose.className='version-choice';choose.setAttribute('aria-label',`Preview version ${revision.version}`);choose.append(version,time);row.append(choose);const note=document.createElement('span');note.className='version-summary';note.textContent=revision.summary||(revision.restoredFrom!==undefined?`Restored version ${revision.restoredFrom}.`:'Saved changes.');choose.append(note);choose.addEventListener('click',()=>{let host=menu.querySelector('.history-preview-host');if(!host){host=document.createElement('section');host.className='history-preview-host';}row.after(host);window.requestVersionPreview(revision.version,host);host.scrollIntoView({block:'nearest'});});if(revision.restoredFrom!==undefined)row.title=`Restored from v${revision.restoredFrom}`;versions.append(row);
  }
  if(!versions.children.length){const empty=document.createElement('li');empty.textContent='No saved versions yet';versions.append(empty);}
  if(previewHost)versions.prepend(previewHost);history.append(versions);menu.append(history);if(provider?.feedPending){const status=document.createElement('p');status.textContent='Feed sync pending';status.className='feed-status';menu.append(status);}
  if(provider?.busy){const note=document.createElement('p');note.textContent='Provider changes are available after this request.';menu.append(note);}
  if(menu.open)[...menu.querySelectorAll('button')].find(button=>button.textContent===focused)?.focus({preventScroll:true});
  menu.scrollTop=scroll;
 }
 async function render(){
  const n=balance();const hosted=provider?.backend==='ac';
  footer.hidden=!provider;
  const model=String(provider?.model||'').split('/').at(-1)
    .replace(/^claude-/, '').replace(/^gpt-5\.6-luna$/, 'Luna')
    .replace(/-20\d{6}$/, '').replace(/(\d)-(\d)/g,'$1.$2').replace(/-/g,' ').replace(/\b(opus|sonnet|haiku)\b/gi,word=>word[0].toUpperCase()+word.slice(1));
  const name=hosted?'AC':provider?.backend==='claude'?'Claude':provider?.backend==='codex'?'Codex':'';
  const text=[name,model,hosted?`${n===null?'—':n.toLocaleString('en-US')} braincells`:''].filter(Boolean).join(' · ');
  footer.title=`Settings · ${text}`;
  footer.setAttribute('aria-label',`Settings. ${text}`);
  rebuildMenu();
  if(footer.childElementCount)return;
  footer.append(document.getElementById('version'));
 }
 window.updateProviderFooter=value=>{if(!['ac','claude','codex'].includes(value?.backend))return;provider=value;window.updateNotebookBindings?.(value);render();};
 window.aesel.onBuildStatus?.(value=>{buildInfo=value;render();});
 window.aesel.onCredits(value=>{credits=value;render();});
 footer.addEventListener('click',()=>{if(menu.hidden){menu.hidden=false;menu.showModal();window.aesel.input('\x1b[99;5~');footer.setAttribute('aria-expanded','true');menu.querySelector('button:not(:disabled)')?.focus();}else close();});
 document.addEventListener('pointerdown',event=>{if(!footer.contains(event.target)&&!menu.contains(event.target))close();});
 document.addEventListener('keydown',event=>{if(event.key==='Escape'&&!menu.hidden){close();footer.focus();event.preventDefault();event.stopPropagation();}},true);
 render();
})();

