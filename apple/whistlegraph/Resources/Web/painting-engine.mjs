import {AccountConnection} from './account-connection.mjs';
import {fetchHandleColors,handleCharacterColors} from '/easel/src/handle-colors.mjs';
import {AcServer} from '/easel/src/ac-server.mjs';
import {verifyAccount} from '/easel/src/account-access.mjs';
import {FLASH_MODEL} from './generation-policy.mjs';
import {PieceVersions} from './piece-versions.mjs';
import {WareProjects} from './ware-projects.mjs';
import {wareControls,WARE_INSTRUCTIONS,requestedWare} from './wares.mjs';
import {savePaintingAsset,loadPaintingAsset,removePaintingAsset} from './painting-assets.mjs';
import {withDrawing} from './drawing-input.mjs';
import {starterPainting,validatePainting,renderPainting,paintingFilename,PAINTING_SCHEMA,requestsNewPainting} from './painting.mjs';
const post=body=>window.webkit.messageHandlers.whistlegraph.postMessage({id:'painting',...body});
const projects=new WareProjects(localStorage,'whistlegraph-painting'),key=projects.key();
const versions=new PieceVersions(localStorage,key+'-versions',JSON.stringify(starterPainting()));
let painting=validatePainting(JSON.parse(versions.head.source)),codes=JSON.parse(localStorage.getItem(key+'-codes')||'{}');
let staged=null,token='',handle='',colors=[],busy=false,error='',output='',attempt=null,server=null,cancelled=false,successful=false,imageController=null;
const wares=wareControls('painting');
const turnAssets=new Set();
const account=new AccountConnection({verify:verifyAccount,changed:state=>post({action:'accountState',...state}),loadColors:fetchHandleColors,initialColors:handleCharacterColors,paletteChanged:state=>{if(handle===state.handle){colors=state.colors;snapshot();}}});
document.body.classList.add('live-mode');document.getElementById('initial').hidden=true;
const box=document.createElement('div');box.id='live-preview-box';box.style.cssText='display:flex;align-items:center;justify-content:center;background:repeating-conic-gradient(#dedede 0% 25%,#fff 0% 50%) 0 / 20px 20px';
const canvas=document.createElement('canvas');canvas.id='painting-preview';canvas.setAttribute('aria-label','Painting');canvas.style.cssText='width:100%;height:100%;object-fit:contain';box.append(canvas);document.getElementById('stage').prepend(box);
function snapshot(){
 post({action:'pieces',pieces:projects.list({ledger:versions.value,name:painting.title,code:codes[versions.head.id]||''})});
 post({action:'snapshot',snapshot:{ware:'painting',caption:painting.title,code:codes[versions.head.id]||'',handle,colors,head:versions.head.id,hasPiece:true,hasPreview:true,busy,error,phase:busy?'Painting…':'Painting',output:output.slice(-6000),attempt,versions:versions.value.versions.map(v=>({id:v.id,parent:v.parent,utterance:v.request||'',createdAt:v.createdAt}))}});
}
async function paint(value=painting){await renderPainting(canvas,value,loadPaintingAsset);}
async function commit(value,request){
 const next=validatePainting(value),source=JSON.stringify(next);if(source===versions.head.source)return false;
 if(versions.value.versions.length>=100)throw Error('This painting has reached its version limit. Start another painting.');
 await paint(next);versions.commit({source,request,layers:next.layers.length});painting=next;error='';snapshot();return true;
}
async function checkout(id){const v=versions.value.versions.find(v=>v.id===id);if(!v)throw Error('Version unavailable');const next=validatePainting(JSON.parse(v.source));await paint(next);versions.checkout(id);painting=next;error='';snapshot();}
const describe=p=>JSON.stringify({...p,layers:p.layers.map(l=>l.kind==='raster'?{...l,data:undefined,imported:true}:l)});
const artifacts={
 context:async()=>`You are making an editable still PNG painting. It is silent and never animated. Use hex colors (#rrggbb or #rrggbbaa), pixel coordinates, and layered shapes. Rect/ellipse x,y is the top-left of its bounding box. Text x,y is top-left; size is pixels. Paths need points and stroke width. Give new artwork a short descriptive title. Preserve existing layers and IDs on edits. Imported raster layers are retained automatically and cannot be recreated from their description. Current painting: ${describe(staged||painting)}`,
 tools:async()=>[{name:'artifact_painting',description:'Stage a complete painting using shapes, paths and text. Retains imported PNG layers unless removeRaster is explicitly requested. Saves one version only after a successful turn.',input_schema:{type:'object',properties:{painting:PAINTING_SCHEMA,removeRaster:{type:'boolean'}},required:['painting'],additionalProperties:false}},{name:'artifact_generate',description:'Generate a still PNG image or edit the current composite through AC’s hosted image provider. Only when the user requests image generation or image editing. One remote image request per turn; never retry automatically. Stages one painting version.',input_schema:{type:'object',properties:{prompt:{type:'string'},title:{type:'string'},edit:{type:'boolean'}},required:['prompt','title','edit'],additionalProperties:false}}],
 run:async(name,input)=>{
  if(cancelled||wares.pending)throw Error('Painting stopped');
  if(name==='generate'){
   if(typeof input.prompt!=='string'||!input.prompt.trim()||input.prompt.length>16000||typeof input.title!=='string'||!input.title.trim()||input.title.length>80||typeof input.edit!=='boolean')throw Error('Provide an image prompt, title and edit flag.');
   const credential=token,jobId=crypto.randomUUID();imageController=new AbortController();let asset;
   try{
    // Durable submission receipt prevents silently repeating an uncertain charge.
    localStorage.setItem(key+'-image-job',JSON.stringify({jobId,status:'submitted',at:new Date().toISOString()}));
    const response=await window.fetch('https://aesthetic.computer/api/easel-image',{method:'POST',headers:{Authorization:'Bearer '+credential,'Content-Type':'application/json'},body:JSON.stringify({jobId,prompt:input.prompt,provider:'openai',model:'gpt-image-2',size:'1024x1024',quality:'medium',...(input.edit?{reference:png()}:{})}),signal:AbortSignal.any([imageController.signal,AbortSignal.timeout(330000)])});
    const result=await response.json();if(!response.ok)throw Error(result.error||'Image generation failed.');
    if(cancelled||token!==credential)throw Error('Image generation stopped.');
    asset=await savePaintingAsset(result.png);turnAssets.add(asset);
    const image=new Image();image.src='data:image/png;base64,'+result.png;await image.decode();
    const next=validatePainting({format:1,title:input.title,width:image.naturalWidth,height:image.naturalHeight,background:'transparent',layers:[{id:'generated-image',kind:'raster',asset,x:0,y:0,width:image.naturalWidth,height:image.naturalHeight}]});
    await paint(next);if(cancelled||token!==credential)throw Error('Image generation stopped.');staged=next;error='';
    localStorage.setItem(key+'-image-job',JSON.stringify({jobId,status:'complete',asset,provenance:result.provenance}));snapshot();return {version:versions.head.id,summary:'PNG staged',title:next.title};
   }catch(failure){if(asset)await removePaintingAsset(asset);error=failure.message;throw failure;}finally{imageController=null;}
  }
  if(name!=='painting')throw Error('Use artifact_painting or artifact_generate.');
  const value=structuredClone(input.painting);
  if(!input.removeRaster)value.layers=[...(staged||painting).layers.filter(l=>l.kind==='raster'),...value.layers];
  try{const next=validatePainting(value);await paint(next);staged=next;error='';snapshot();return {version:versions.head.id,summary:'Painting staged',layers:next.layers.length,title:next.title};}catch(failure){error=failure.message;throw failure;}
 },
};
async function ask(text,drawing){
 if(busy)return;const target=requestedWare(text);if(target){window.whistlegraphWorkFinished?.();window.whistlegraphSelectWare?.(target);return;}
 if(requestsNewPainting(text)){try{projects.new(JSON.stringify(starterPainting()));location.reload();}catch(e){error=e.message;snapshot();}window.whistlegraphWorkFinished?.();return;}
 if(!token){post({action:'signIn'});window.whistlegraphWorkFinished?.();return;}
 busy=true;error='';output='';staged=null;cancelled=false;successful=false;wares.clear();attempt={request:text,status:'working',error:''};snapshot();
 try{
  server=new AcServer({token:()=>token,model:window.__whistlegraphModel||FLASH_MODEL,artifacts,controls:wares,preview:false,frameCapture:false,rounds:8,reasoning:{effort:'none'},thinking:{type:'disabled'},developerInstructions:'Make the requested still image with artifact_painting. Use layered shapes creatively for illustrations, abstract paintings, and pixel art. For a requested generated image or photograph, use artifact_generate. For image edits, use edit:true to send the current composite as a reference. One remote image request per turn, never retry after a failure. No scripts, animation, audio, or preamble. '+WARE_INSTRUCTIONS});
  server.on('notification',({method,params})=>{if(method==='item/agentMessage/delta'){output+=params.delta;snapshot();}if(method==='turn/completed'){successful=params.turn.status==='completed'&&!params.turn.error;if(!successful&&!cancelled)error=params.turn.error?.message||'Could not finish the painting.';}});
  await server.startTurn(withDrawing(text,drawing));
  if(error)successful=false;
  if(!cancelled&&successful&&!wares.pending&&staged)await commit(staged,text);
  if(successful&&!staged&&error)successful=false;
  if(cancelled)error='Painting stopped.';
 }catch(e){error=e.message;successful=false;}
 finally{
  const target=!cancelled&&successful?wares.pending:null;wares.clear();staged=null;busy=false;server?.close();server=null;
  attempt={...attempt,status:successful&&!cancelled?'completed':'failed',error};
  const savedAssets=new Set(versions.value.versions.flatMap(v=>JSON.parse(v.source).layers.filter(l=>l.kind==='raster').map(l=>l.asset)));
  for(const asset of turnAssets)if(!savedAssets.has(asset))await removePaintingAsset(asset).catch(()=>{});turnAssets.clear();
  await paint();snapshot();window.whistlegraphWorkFinished?.();if(target)window.whistlegraphSelectWare?.(target);
 }
}
async function run(operation){busy=true;snapshot();try{await operation();}catch(e){error=e.message;}finally{busy=false;await paint();snapshot();}}
function png(){return canvas.toDataURL('image/png').split(',')[1];}
window.whistlegraphNativeCommand=command=>{
 if(command.action==='stop'){cancelled=true;imageController?.abort();server?.interrupt();return;}if(command.action==='signIn'){post({action:'signIn'});return;}if(busy)return;
 try{
  if(command.action==='setWare')window.whistlegraphSelectWare?.(command.ware);
  if(command.action==='newPiece'){projects.new(JSON.stringify(starterPainting()));location.reload();}
  if(command.action==='openPiece'){projects.open(command.piece);location.reload();}
  if(command.action==='deletePiece'){const ledger=JSON.parse(localStorage.getItem(projects.key(command.piece)+'-versions'));const assets=new Set(ledger.versions.flatMap(v=>JSON.parse(v.source).layers.filter(l=>l.kind==='raster').map(l=>l.asset)).filter(Boolean));projects.remove(command.piece);for(const asset of assets)void removePaintingAsset(asset).catch(()=>{});snapshot();}
  if(command.action==='ask'&&typeof command.text==='string'&&(command.text.trim()||command.drawing)&&Array.from(new Intl.Segmenter(undefined,{granularity:'grapheme'}).segment(command.text)).length<=96)void ask(command.text.trim()||'Paint from this drawing.',command.drawing);
  if(command.action==='retry'&&attempt?.request)void ask(attempt.request);
  if(command.action==='checkout')void run(()=>checkout(command.version));
  if(command.action==='undoPainting'&&versions.head.parent!==null)void run(()=>checkout(versions.head.parent));
  if(command.action==='exportPng')post({action:'exportPng',filename:paintingFilename(painting),data:png()});
  if(command.action==='publishPainting'){
   if(!token){post({action:'signIn'});return;}
   const credential=token,head=versions.head.id;
   void run(async()=>{
    const response=await window.fetch('https://aesthetic.computer/api/mime',{method:'POST',headers:{Authorization:'Bearer '+credential,'Content-Type':'application/json'},body:JSON.stringify({name:painting.title,text:'',file:{name:paintingFilename(painting),type:'image/png',data:png()}}),signal:AbortSignal.timeout(30000)});
    const result=await response.json();if(!response.ok)throw Error(result.error||'Could not publish the painting.');
    if(token!==credential)throw Error('Account changed during publishing.');
    const next={...codes,[head]:result.code};localStorage.setItem(key+'-codes',JSON.stringify(next));codes=next;
   });
  }
 }catch(e){error=e.message;snapshot();}
};
window.whistlegraphEngineEvent=event=>{
 if(event.kind==='account'){
  if(event.token&&event.token===token&&handle&&!event.retry)return;token=event.token||'';handle='';colors=[];window.whistlegraphAccountReady=!!token;
  if(!token&&busy){cancelled=true;imageController?.abort();server?.interrupt();}const expected=token;
  void account.connect(token,event.notice||'').then(result=>{if(result&&token===expected){handle=result.handle||'';colors=result.colors;snapshot();}});snapshot();
 }
 if(event.kind==='importPng'&&!busy)void run(async()=>{
  const image=new Image();image.src='data:image/png;base64,'+event.data;await image.decode();
  const next=validatePainting({format:1,title:(event.name||'Imported painting').replace(/\.png$/i,'').slice(0,80)||'Imported painting',width:image.naturalWidth,height:image.naturalHeight,background:'transparent',layers:[]});
  const asset=await savePaintingAsset(event.data);next.layers=[{id:'imported-png',kind:'raster',asset,x:0,y:0,width:next.width,height:next.height}];
  try{await paint(next);projects.new(JSON.stringify(next));location.reload();}catch(failure){await removePaintingAsset(asset);throw failure;}
 });
 if(event.kind==='error'){error=event.text||'Painting failed.';snapshot();window.whistlegraphWorkFinished?.();}
};
window.whistlegraphAsk=ask;window.whistlegraphAskSound=input=>ask(input.transcript?.trim()||(input.drawing?'Paint from this drawing.':''),input.drawing);window.whistlegraphIsBusy=()=>busy;
window.whistlegraphUndo=()=>{if(!busy&&versions.head.parent!==null)void run(()=>checkout(versions.head.parent));};
document.addEventListener('visibilitychange',()=>{if(document.hidden&&busy&&server){cancelled=true;imageController?.abort();server.interrupt();}});
await paint();snapshot();post({action:'account'});
