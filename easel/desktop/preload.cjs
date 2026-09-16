const { contextBridge, ipcRenderer } = require('electron');
contextBridge.exposeInMainWorld('easel', {
  input: data => { if (typeof data === 'string') ipcRenderer.send('input', data); },
  size: (cols, rows) => ipcRenderer.send('size', { cols, rows }),
  ready: () => ipcRenderer.send('ready'),
  fullscreen: target => {
    if (target === 'app' || target === 'preview') ipcRenderer.send('fullscreen', target);
  },
  openPiece: url => { if (typeof url === 'string') ipcRenderer.send('open-piece',url); },
  onPreviewMode: fn => ipcRenderer.on('preview-mode', (_event, mode) => { if (['compact', 'hover', 'pinned'].includes(mode)) fn(mode); }),
  onNotice: fn => ipcRenderer.on('desktop-notice', (_event, message) => { if (typeof message === 'string') fn(message.slice(0, 2000)); }),
  onFullscreenState: fn => ipcRenderer.on('fullscreen-state', (_event, state) => {
    if (state && typeof state.app === 'boolean' && typeof state.preview === 'boolean') fn({ app: state.app, preview: state.preview });
  }),
  contextMenu: selection => ipcRenderer.send('context-menu', { selection: typeof selection === 'string' ? selection.slice(0, 1024 * 1024) : '', canPaste: true }),
  copyText: text => { if (typeof text === 'string') ipcRenderer.send('copy-text', text.slice(0, 1024 * 1024)); },
  requestPaste: () => ipcRenderer.send('paste-request'),
  onPaste: fn => ipcRenderer.on('paste', (_event, text) => { if (typeof text === 'string') fn(text); }),
  onSelectAll: fn => ipcRenderer.on('select-all', () => fn()),
  onOutput: fn => ipcRenderer.on('output', (_event, data) => fn(data)),
  onState: fn => ipcRenderer.on('state', (_event, data) => fn(data)),
  onTextSize: fn => ipcRenderer.on('text-size', (_event, action) => { if (['larger','smaller','reset'].includes(action)) fn(action); }),
  onDisplay: fn => ipcRenderer.on('display', (_event, value) => { if (Number.isFinite(value?.width) && Number.isFinite(value?.height) && value.width > 0 && value.height > 0) fn(value); }),
  onTheme: fn => ipcRenderer.on('theme', (_event, theme) => fn(theme)),
  onFont: fn => ipcRenderer.on('font', (_event, bitmap) => fn(bitmap)),
});
