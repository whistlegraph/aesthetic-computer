window.installPreviewAudio=preview=>{
 let saved={};try{saved=JSON.parse(localStorage.getItem('aesel-preview-audio')||'{}');}catch{}
 const state={volume:Number.isFinite(saved.volume)?Math.max(0,Math.min(1,saved.volume)):1,muted:saved.muted===true};
 let ready=false,timer=0,generation=0;
 async function apply(attempt=0){
  clearTimeout(timer);const current=generation;
  try{
   preview.setAudioMuted(state.muted||state.volume===0);
   if(!ready)return;
   const applied=await preview.executeJavaScript(`(()=>{if(typeof window.AC?.setMasterVolume!=='function')return false;window.AC.setMasterVolume(${state.volume});return true})()`);
   if(!applied&&current===generation&&attempt<50)timer=setTimeout(()=>apply(attempt+1),200);
  }catch{}
 }
 const controls={get volume(){return state.volume},get muted(){return state.muted},set(value){
  if(Number.isFinite(value.volume))state.volume=Math.max(0,Math.min(1,value.volume));
  if(typeof value.muted==='boolean')state.muted=value.muted;
  try{localStorage.setItem('aesel-preview-audio',JSON.stringify(state));}catch{}
  generation++;void apply();
 }};
 preview.addEventListener('did-start-loading',()=>{ready=false;generation++;clearTimeout(timer);});
 const start=()=>{if(ready)return;ready=true;generation++;void apply();};
 preview.addEventListener('dom-ready',start);preview.addEventListener('did-stop-loading',start);
 queueMicrotask(()=>{try{if(preview.getWebContentsId()&&!preview.isLoading())start();}catch{}});
 window.addEventListener('beforeunload',()=>clearTimeout(timer),{once:true});
 return controls;
};
