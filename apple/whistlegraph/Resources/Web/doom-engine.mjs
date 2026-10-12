import {AccountConnection} from './account-connection.mjs';
import {AcServer} from '/easel/src/ac-server.mjs';
import {verifyAccount} from '/easel/src/account-access.mjs';
import {FLASH_MODEL} from './generation-policy.mjs';
import {PieceVersions} from './piece-versions.mjs';
import {wareControls,WARE_INSTRUCTIONS,requestedWare} from './wares.mjs';
import {starterLevel,validateLevel,emptySeedHistory,WAD_MIME} from './doom-wad.mjs';
import {compileDocument,compileProject,asProject,validateProject,encode64,documentSummary} from './doom-project.mjs';
import {createProjectTools,PROJECT_INSTRUCTIONS} from './doom-project-tools.mjs';
const post=body=>window.webkit.messageHandlers.whistlegraph.postMessage({id:'doom',...body});
const account=new AccountConnection({verify:verifyAccount,changed:state=>post({action:'accountState',...state})});
const versions=new PieceVersions(localStorage,'whistlegraph-doom-versions',JSON.stringify(starterLevel()));
const codes=JSON.parse(localStorage.getItem('whistlegraph-doom-codes')||'{}');
const emptySeed=!codes[0]&&emptySeedHistory(versions.value);if(emptySeed)versions.persist(emptySeed);
const validate=value=>value.format===2?validateProject(value):validateLevel(value);
let level=validate(JSON.parse(versions.head.source)),wad=null,staged=null,token='',handle='',busy=false,server=null,cancelled=false,successful=false,error='',output='',attempt=readAttempt(),code='';
function readAttempt(){try{const value=JSON.parse(localStorage.getItem('whistlegraph-doom-attempt')||'null');if(value?.status==='working')return {...value,status:'interrupted',error:'Editing was interrupted. The saved WAD is unchanged.'};return value;}catch{return null;}}
let savedAttempt='';
code=codes[versions.head.id]||'';
const wares=wareControls('doom');
document.body.classList.add('live-mode','live-preview');document.getElementById('initial').hidden=true;
const box=document.createElement('div');box.id='live-preview-box';document.getElementById('stage').prepend(box);
const frame=document.createElement('iframe');frame.title='Playable Doom WAD';frame.allow='autoplay';frame.style.cssText='width:100%;height:100%;border:0';box.append(frame);
let frameReady=false,compileId=0;const compileCalls=new Map(),frameWaiters=[];
function render(){frameReady=false;frame.src='https://aesthetic.computer/doom-player/?preview=1&bridge=1';}
function sendWad(){if(!wad||!frameReady)return;const copy=wad.slice();frame.contentWindow.postMessage({type:'doom:wad',wad:copy.buffer,title:level.title},'https://aesthetic.computer',[copy.buffer]);}
async function compileRPC(operation,project,game){
 if(!frameReady)await new Promise((resolve,reject)=>{const timer=setTimeout(()=>reject(Error('Doom compiler preview did not load')),30000);frameWaiters.push(()=>{clearTimeout(timer);resolve();});});
 const id=++compileId;return new Promise((resolve,reject)=>{const timer=setTimeout(()=>{compileCalls.delete(id);reject(Error('Doom compiler did not reply'));},30000);compileCalls.set(id,{resolve,reject,timer});frame.contentWindow.postMessage({type:'doom:compile',id,operation,project,game},'https://aesthetic.computer');});
}
async function compileNative(value){if(value.format!==2)return compileDocument(value);validateProject(value);return compileRPC('project',value);}
window.addEventListener('message',event=>{
 if(event.source!==frame.contentWindow||event.origin!=='https://aesthetic.computer')return;
 if(event.data?.type==='doom:error'){error='Doom preview: '+event.data.message;snapshot();}
 if(event.data?.type==='doom:request'){frameReady=true;for(const ready of frameWaiters.splice(0))ready();sendWad();}
 if(event.data?.type==='doom:compiled'){const call=compileCalls.get(event.data.id);if(!call)return;clearTimeout(call.timer);compileCalls.delete(event.data.id);if(event.data.error)call.reject(Error(event.data.error));else if(event.data.catalog)call.resolve(event.data.catalog);else if(event.data.wad instanceof ArrayBuffer)call.resolve(new Uint8Array(event.data.wad));else call.reject(Error('Invalid Doom compiler reply'));}
});
function snapshot(){const pending=JSON.stringify(attempt);if(pending!==savedAttempt){try{localStorage.setItem('whistlegraph-doom-attempt',pending);savedAttempt=pending;}catch{}}post({action:'snapshot',snapshot:{ware:'doom',code,caption:level.title,handle,head:versions.head.id,hasPiece:true,hasPreview:true,busy,phase:busy?'Building WAD…':'Doom arena',error,output:output.slice(-6000),attempt,versions:versions.value.versions.map(v=>({id:v.id,parent:v.parent,utterance:v.request||'',createdAt:v.createdAt}))}});}
async function commit(next,request){
  if(versions.value.versions.length>=100)throw Error('Export this WAD before starting another history.');
  const value=validate(next),source=JSON.stringify(value);if(source===versions.head.source)return false;
  const compiled=await compileNative(value);if(cancelled) return false;versions.commit({source,request,layers:1});level=value;wad=compiled;code='';error='';render();snapshot();return true;
}
async function checkout(id){const version=versions.value.versions.find(v=>v.id===id);if(!version)throw Error('Version unavailable');const next=validate(JSON.parse(version.source)),compiled=await compileNative(next);versions.checkout(id);level=next;wad=compiled;code=codes[versions.head.id]||'';error='';render();snapshot();}
const projectTools=createProjectTools({get:()=>staged||level,compile:compileNative,catalog:game=>compileRPC('catalog',null,game),set:(value)=>{if(cancelled||wares.pending)throw Error('Doom edit stopped');staged=value;}});
const artifacts={
 context:async()=>PROJECT_INSTRUCTIONS+'\nCurrent level: '+documentSummary(staged||level),
 tools:projectTools.tools,
 run:async(name,input)=>{if(cancelled||wares.pending)throw Error('Doom edit stopped');try{return await projectTools.run(name,input);}catch(failure){if(failure.name==='DoomAssetError'){error=failure.message;cancelled=true;server?.interrupt();}throw failure;}}
};
async function ask(text){
  if(busy)return;const target=requestedWare(text);if(target){window.whistlegraphWorkFinished?.();window.whistlegraphSelectWare?.(target);return;}
  error='';output='';staged=null;cancelled=false;successful=false;wares.clear();
  try{
    // A few deterministic edits work offline and make the initial ware useful.
    if(level.format!==2&&/^(?:make it |make the room )?(?:brighter|darker)[.!]?$/i.test(text)){await commit({...level,light:Math.max(32,Math.min(255,level.light+(/darker/i.test(text)?-32:32)))},text);return;}
    if(level.format!==2&&/^(?:remove |no )(?:all )?(?:enemies|monsters)[.!]?$/i.test(text)){await commit({...level,things:level.things.filter(t=>![3004,3001,9].includes(t.type))},text);return;}
    if(!token){error='Sign in to ask the brain to edit this WAD.';post({action:'signIn'});return;}
    busy=true;attempt={request:text,status:'working',error:''};snapshot();
    // Verify native asset access and the node builder before paid inference.
    await compileNative(asProject(level));if(cancelled)return;
    server=new AcServer({token:()=>token,model:window.__whistlegraphModel||FLASH_MODEL,artifacts,controls:wares,preview:false,frameCapture:false,rounds:16,reasoning:{effort:'none'},thinking:{type:'disabled'},developerInstructions:'This is Whistlegraph. Build and iteratively edit the requested Doom world using the project tools. Use custom graphics and actual freeform sector geometry for requests that need them. Preserve existing artwork and maps during edits. Do not claim to have play-tested, exported, or published a staged WAD. No preamble. '+WARE_INSTRUCTIONS});
    server.on('notification',({method,params})=>{if(method==='item/agentMessage/delta'){output+=params.delta;snapshot();}if(method==='turn/completed'){successful=params.turn.status==='completed'&&!params.turn.error;if(!successful&&!cancelled)error=params.turn.error?.message||'Could not finish the WAD';}});
    await server.startTurn(text);if(!cancelled&&successful&&!wares.pending&&staged)await commit(staged,text);
    attempt={...attempt,status:successful?'completed':'failed',error};
  }catch(failure){error=failure.message;if(attempt)attempt={...attempt,status:'failed',error};}
  finally{const target=!cancelled&&successful?wares.pending:null;wares.clear();staged=null;busy=false;server?.close();server=null;snapshot();window.whistlegraphWorkFinished?.();if(target)window.whistlegraphSelectWare?.(target);}
}
function wadFilename(){return (level.title.normalize('NFKD').replace(/[\u0300-\u036f]/g,'').toLowerCase().replace(/[^a-z0-9]+/g,'-').replace(/^-+|-+$/g,'').slice(0,80)||'untitled')+'.wad';}
function base64(){if(!wad)throw Error('WAD is still compiling');return encode64(wad);}
async function publish(){
  if(code){output='Published: '+code;snapshot();return;}
  if(!token){post({action:'signIn'});return;}const credential=token,head=versions.head.id;busy=true;error='';snapshot();
  try{const response=await fetch('https://aesthetic.computer/api/mime',{method:'POST',headers:{Authorization:`Bearer ${credential}`,'Content-Type':'application/json'},body:JSON.stringify({text:level.title,file:{name:wadFilename(),type:WAD_MIME,data:base64()}})});const result=await response.json();if(!response.ok)throw Error(result.error||'Could not publish WAD');if(token===credential&&versions.head.id===head){code=result.code;codes[head]=code;localStorage.setItem('whistlegraph-doom-codes',JSON.stringify(codes));output='Published: '+code;}}
  catch(failure){error=failure.message;}finally{busy=false;snapshot();}
}
async function change(operation){busy=true;cancelled=false;snapshot();try{await operation();}catch(failure){error=failure.message;}finally{busy=false;snapshot();}}
window.whistlegraphNativeCommand=command=>{
  if(command.action==='signIn'){post({action:'signIn'});return;}if(command.action==='stop'){cancelled=true;server?.interrupt();return;}if(busy)return;
  try{
    if(command.action==='setWare')window.whistlegraphSelectWare?.(command.ware);
    if(command.action==='ask'&&typeof command.text==='string'&&command.text.trim()&&Array.from(new Intl.Segmenter(undefined,{granularity:'grapheme'}).segment(command.text)).length<=96)void ask(command.text.trim());
    if(command.action==='checkout')void change(()=>checkout(command.version));
    if(command.action==='undoDoom'&&versions.head.parent!==null)void change(()=>checkout(versions.head.parent));
    if(command.action==='retry'&&attempt?.request)void ask(attempt.request);
    if(command.action==='newDoom')void change(()=>commit(starterLevel(),'New empty WAD'));
    if(command.action==='exportWad')post({action:'exportWad',filename:wadFilename(),data:base64()});
    if(command.action==='publishDoom')void publish();
    if(command.action==='playDoom')render();
  }catch(failure){error=failure.message;snapshot();}
};
window.whistlegraphEngineEvent=event=>{
  if(event.kind==='account'){if(event.token&&event.token===token&&handle&&!event.retry)return;token=event.token||'';handle='';window.whistlegraphAccountReady=!!token;if(!token&&busy){cancelled=true;server?.interrupt();}const expected=token;void account.connect(token,event.notice||'').then(result=>{if(result&&token===expected){handle=result.handle||'';snapshot();}});snapshot();}
  if(event.kind==='error'){error=event.text||'Sign-in failed';snapshot();window.whistlegraphWorkFinished?.();}
};
window.whistlegraphAsk=ask;window.whistlegraphAskSound=input=>ask(input.transcript?.trim()||'');window.whistlegraphIsBusy=()=>busy;
window.whistlegraphUndo=()=>{if(!busy&&versions.head.parent!==null)void change(()=>checkout(versions.head.parent));};
document.addEventListener('visibilitychange',()=>{if(document.hidden&&busy){cancelled=true;server?.interrupt();}});
snapshot();post({action:'account'});
render();void change(async()=>{wad=await compileNative(level);sendWad();});
