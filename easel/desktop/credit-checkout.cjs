const {randomUUID}=require('node:crypto');
const {join}=require('node:path');
const {pathToFileURL}=require('node:url');
const {readFileSync,writeFileSync,unlinkSync}=require('node:fs');
function startCreditCheckout({app,window,root,ipcMain,shell,refresh}) {
 const statePath=join(app.getPath('userData'),'credit-checkout.json');
 let busy=false;
 const account=()=>import(pathToFileURL(join(root,'src/ac-session.mjs')).href).then(({ACSession})=>new ACSession());
 async function request(body){const session=await account();if(!session.signedIn)throw Error('Sign in to AC before buying credits.');const response=await fetch('https://aesthetic.computer/api/easel-checkout',{method:'POST',headers:{'Content-Type':'application/json',Authorization:`Bearer ${await session.token()}`},body:JSON.stringify(body),signal:AbortSignal.timeout(15000)});const data=await response.json();if(!response.ok)throw Error(data.error||'Checkout unavailable');return data;}
 async function reconcile(){
  if(busy)return;
  let saved;try{saved=JSON.parse(readFileSync(statePath,'utf8'));}catch{return;}
  if(!saved.sessionId)return;
  busy=true;
  try{const status=await request({sessionId:saved.sessionId});if(status.paid||status.status==='expired'){unlinkSync(statePath);await refresh();if(status.paid)window.webContents.send('desktop-notice','Payment received. Your AC credits are ready.');}}catch{}finally{busy=false;}
 }
 ipcMain.handle('buy-ac-credits',async event=>{
  if(event.sender!==window.webContents||busy)return {error:'Checkout is already opening.'};busy=true;
  try{
   let saved;try{saved=JSON.parse(readFileSync(statePath,'utf8'));}catch{}
   // Reuse a pending checkout through double-clicks and app restarts.
   if(saved?.sessionId){const status=await request({sessionId:saved.sessionId});if(status.paid||status.status==='expired')saved=null;}
   if(!saved)saved={requestId:randomUUID()};
   writeFileSync(statePath,JSON.stringify(saved),{mode:0o600});
   const checkout=await request({pack:'luna-1m-v1',requestId:saved.requestId});
   const url=new URL(checkout.url);if(url.protocol!=='https:'||!['checkout.stripe.com','pay.aesthetic.computer'].includes(url.hostname))throw Error('Invalid checkout address');
   writeFileSync(statePath,JSON.stringify({...saved,sessionId:checkout.sessionId}),{mode:0o600});
   await shell.openExternal(url.href);return {opened:true};
  }catch(error){return {error:error.message};}finally{busy=false;}
 });
 window.on('focus',reconcile);const timer=setInterval(reconcile,10000);timer.unref();
 window.once('closed',()=>{clearInterval(timer);ipcMain.removeHandler('buy-ac-credits');});
 return {reconcile};
}
module.exports={startCreditCheckout};
