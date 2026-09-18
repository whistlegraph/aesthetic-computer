// Snapshot previews have an isolated, nonpersistent guest and never join the live piece route.
(() => {
 let guest=null,pending=null,sequence=0,ready=false,timer=null;
 const close=()=>{sequence++;pending=null;ready=false;clearTimeout(timer);guest?.remove();guest=null;};
 window.closeVersionPreview=close;
 window.requestVersionPreview=(version,host)=>{
  close();const request=++sequence;
  const label=document.createElement('p');label.textContent=`Loading v${version}…`;label.setAttribute('role','status');host.replaceChildren(label);
  pending={request,version,host,label};
  (window.aesel||window.aesel).input(`\x1b[99;8;${version};${request}~`);
  timer=setTimeout(()=>{if(pending?.request===request)label.textContent='Preview unavailable. Tap the version to retry.';},20000);
 };
 window.receiveVersionPreview=async result=>{
  if(!pending||result.request!==pending.request)return;
  const current=pending;if(result.error){clearTimeout(timer);current.label.textContent=result.error;return;}
  if(typeof result.source!=='string'||result.source.length>500000)return;
  guest=document.createElement('webview');guest.className='history-preview';guest.setAttribute('webpreferences','contextIsolation=yes,sandbox=yes,nodeIntegration=no');guest.setAttribute('partition',`aesel-history-${crypto.randomUUID()}`);
  const source=result.source;
  const content=result.runtime==='lua'?{source,language:'lua',ext:'lua',liveName:'aesel-history'}:result.runtime==='lisp'?{source,createCode:false}:{source,name:'aesel-history'};
  guest.addEventListener('dom-ready',async()=>{
   const target=guest;if(!target||current!==pending)return;
   try{
    const accepted=await target.executeJavaScript(`new Promise(resolve=>{const until=Date.now()+12000;function send(){if(typeof window.acSEND==='function'&&window.preloaded===true){window.acSEND(${JSON.stringify({type:result.runtime==='mjs'?'history-load':'piece-reload',content})});resolve(true);}else if(Date.now()<until)setTimeout(send,100);else resolve(false);}send();})`);
    if(current!==pending)return;clearTimeout(timer);ready=accepted;current.label.textContent=accepted?`Preview · v${result.version}`:'Preview unavailable. Tap the version to retry.';
   }catch{if(current===pending){clearTimeout(timer);current.label.textContent='Preview unavailable. Tap the version to retry.';}}
  },{once:true});
  guest.addEventListener('did-fail-load',event=>{if(current===pending&&event.errorCode!==-3){clearTimeout(timer);current.label.textContent='Could not load preview. Tap the version to retry.';}});
  guest.src='https://aesthetic.computer/starfield?nogap&nolabel&noauth&aesel-history=1';current.host.append(guest);
 };
})();
