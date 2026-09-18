// Native Prox lettering and the signed-in handle's AC allowance.
const {execFile} = require('node:child_process');
const {join} = require('node:path');
const {pathToFileURL} = require('node:url');
function startCreditLabel({app, window, root, nativeRoot = process.env.AESEL_DEV_ROOT ? join(process.env.AESEL_DEV_ROOT,'desktop') : app.isPackaged ? join(process.resourcesPath,'app.asar.unpacked') : __dirname}) {
  let timer, disposed = false, busy = false, lastText = '', lastPayload, offer = null;
  let sessionPromise;
  const session = () => sessionPromise ||= import(pathToFileURL(join(root,'src/ac-session.mjs')).href).then(({ACSession})=>new ACSession());
  const send = payload => { if (!disposed && !window.isDestroyed()) window.webContents.send('credits', payload); };
  async function refresh() {
    if (busy || disposed) return;
    busy = true;
    offer = null;
    let text = 'AC credits —', description = 'Daily AC allowance unavailable. This is not your Claude, Codex, or OpenRouter account balance.';
    try {
      const account = await session();
      if (!account.signedIn) { text = 'Sign in for credits'; description = 'Sign in to Aesthetic Computer to view your daily allowance.'; }
      else {
        const token = await account.token();
        const response = await fetch('https://aesthetic.computer/api/easel-credits', {headers:{Authorization:`Bearer ${token}`,'User-Agent':'Mozilla/5.0 Aesel'},signal:AbortSignal.timeout(10000)});
        if (!response.ok) throw new Error('allowance unavailable');
        const value = await response.json();
        if (value.unit !== 'weighted_tokens' || !Number.isFinite(value.remaining) || value.remaining < 0 || value.handle !== '@'+account.handle) throw new Error('invalid allowance');
        offer = value.offer;
        const paid = Number(value.purchased)||0;
        text = paid > 0 ? `${Math.floor(value.remaining).toLocaleString('en-US')} free · ${paid.toLocaleString('en-US')} paid` : `${Math.floor(value.remaining).toLocaleString('en-US')} AC credits`; 
        description = `${value.handle}: ${Math.floor(value.remaining).toLocaleString('en-US')} of ${value.limit.toLocaleString('en-US')} daily AC credits remaining. One credit is one metered token (cached tokens are discounted). Resets at midnight UTC. Applies to the AC hosted engine only. ${paid.toLocaleString('en-US')} purchased Luna credits; these do not expire.`;
      }
    } catch { /* Unknown stays unknown; never invent a full balance. */ }
    if (disposed) { busy=false; return; }
    if (text === lastText && lastPayload) { lastPayload={...lastPayload,description,offer}; send(lastPayload); busy=false; return; }
    execFile(join(nativeRoot,'native','credit-label'),[text,'14'],{encoding:'buffer',timeout:5000,maxBuffer:1024*1024},(error,png)=>{
      busy=false;
      if (disposed) return;
      lastText=text;
      lastPayload={text,description,offer,image:error?null:`data:image/png;base64,${png.toString('base64')}`};
      send(lastPayload);
    });
  }
  window.webContents.on('did-finish-load', refresh);
  window.on('focus', refresh);
  timer=setInterval(refresh,30000);timer.unref();
  const close=()=>{disposed=true;clearInterval(timer);};
  window.on('closed',close);app.once('before-quit',close);
  return {refresh,close};
}
module.exports={startCreditLabel};
