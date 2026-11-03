const { contextBridge, ipcRenderer } = require('electron');
contextBridge.exposeInMainWorld(
  'doolRig',
  Object.freeze({
    toggleFullscreen: () => ipcRenderer.send('dool-rig:fullscreen'),
    focusOther: () => ipcRenderer.send('dool-rig:focus-other'),
    claimFocus: () => ipcRenderer.send('dool-rig:claim-focus'),
    restart: () => ipcRenderer.send('dool-rig:restart'),
  }),
);
