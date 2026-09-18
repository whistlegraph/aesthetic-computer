const {execFile} = require('node:child_process');
const {join} = require('node:path');
function registerNativeTitle({ipcMain, window, app}) {
  const cache = new Map();
  let pending = null;
  ipcMain.handle('native-prox-title', async (event, value) => {
    if (event.sender !== window.webContents || typeof value?.text !== 'string' || value.text.length > 160 || /[\x00-\x1f\x7f]/.test(value.text)) return null;
    const size = Math.min(48,Math.max(12,Number(value.size)||28));
    const key = JSON.stringify([value.text,size]);
    if (cache.has(key)) return cache.get(key);
    // Bound helper concurrency when titles change rapidly.
    if (pending) await pending;
    if (cache.has(key)) return cache.get(key);
    const nativeRoot = app.isPackaged ? join(process.resourcesPath,'app.asar.unpacked') : __dirname;
    pending = new Promise(resolve => execFile(join(nativeRoot,'native','credit-label'),[value.text,String(size),'--glyphs'],{encoding:'buffer',timeout:5000,maxBuffer:2*1024*1024},(error,data)=>{try{resolve(error?null:JSON.parse(data.toString('utf8')));}catch{resolve(null);}}));
    const image = await pending;
    pending = null;
    if (image) { cache.set(key,image); if(cache.size>32)cache.delete(cache.keys().next().value); }
    return image;
  });
  window.once('closed',()=>ipcMain.removeHandler('native-prox-title'));
}
module.exports={registerNativeTitle};
