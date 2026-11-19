const { contextBridge, ipcRenderer } = require('electron');
contextBridge.exposeInMainWorld(
  'raxRig',
  Object.freeze({
    toggleFullscreen: () => ipcRenderer.send('rax-rig:fullscreen'),
    focusOther: () => ipcRenderer.send('rax-rig:focus-other'),
    claimFocus: () => ipcRenderer.send('rax-rig:claim-focus'),
    restart: () => ipcRenderer.send('rax-rig:restart'),
  }),
);
