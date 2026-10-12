import {AccountConnection} from './account-connection.mjs';
import {AcServer} from '/easel/src/ac-server.mjs';
import {verifyAccount} from '/easel/src/account-access.mjs';
import {FLASH_MODEL} from './generation-policy.mjs';
import {PieceVersions} from './piece-versions.mjs';
import {wareControls,WARE_INSTRUCTIONS,requestedWare} from './wares.mjs';
import {starterLevel,validateLevel,compileWad,emptySeedHistory,LEVEL_SCHEMA,WAD_MIME} from './doom-wad.mjs';
const post=body=>window.webkit.messageHandlers.whistlegraph.postMessage({id:'doom',...body});
const account=new AccountConnection({verify:verifyAccount,changed:state=>post({action:'accountState',...state})});
const versions=new PieceVersions(localStorage,'whistlegraph-doom-versions',JSON.stringify(starterLevel()));
const codes=JSON.parse(localStorage.getItem('whistlegraph-doom-codes')||'{}');
const emptySeed=!codes[0]&&emptySeedHistory(versions.value);if(emptySeed)versions.persist(emptySeed);
let level=validateLevel(JSON.parse(versions.head.source)),staged=null,token='',handle='',busy=false,server=null,cancelled=false,successful=false,error='',output='',attempt=null,code='';
code=codes[versions.head.id]||'';
const wares=wareControls('doom');
document.body.classList.add('live-mode','live-preview');document.getElementById('initial').hidden=true;
const box=document.createElement('div');box.id='live-preview-box';document.getElementById('stage').prepend(box);
const frame=document.createElement('iframe');frame.title='Playable Doom WAD';frame.allow='autoplay';frame.style.cssText='width:100%;height:100%;border:0';box.append(frame);
function render(){frame.src='https://aesthetic.computer/doom-player/?preview=1&level='+encodeURIComponent(JSON.stringify(level));}
function snapshot(){post({action:'snapshot',snapshot:{ware:'doom',code,handle,head:versions.head.id,hasPiece:true,hasPreview:true,busy,phase:busy?'Building WAD…':'Doom arena',error,output:output.slice(-6000),attempt,versions:versions.value.versions.map(v=>({id:v.id,parent:v.parent,utterance:v.request||'',createdAt:v.createdAt}))}});}
function commit(next,request){
  if(versions.value.versions.length>=100)throw Error('Export this WAD before starting another history.');
  const value=validateLevel(next),source=JSON.stringify(value);if(source===versions.head.source)return false;
  compileWad(value);versions.commit({source,request,layers:1});level=value;code='';error='';render();snapshot();return true;
}
function checkout(id){const version=versions.value.versions.find(v=>v.id===id);if(!version)throw Error('Version unavailable');const next=validateLevel(JSON.parse(version.source));versions.checkout(id);level=next;code=codes[versions.head.id]||'';error='';render();snapshot();}
const artifacts={context:async()=>`Edit a classic Doom 1 arena, exported as a real PWAD and played in the Doom engine with Freedoom assets. Supported geometry is ONE rectangular room, no internal walls/doors/stairs. Width/depth are even Doom units; player starts at (0, -depth/2+96), facing north. Keep all objects at least 48 units from walls and 64 units from spawn. Types: 3004 trooper, 3001 imp, 9 shotgun enemy, 2001 shotgun, 2002 chaingun, 2003 rocket launcher, 2007 ammo clip, 2008 shells, 2011 small health, 2012 medikit, 2018 green armor, 2019 blue armor, 2022 invulnerability. North is positive Y. South wall is a use-triggered exit. MIDI music is unavailable in this runtime; game sound effects work. Current level: ${JSON.stringify(staged||level)}`,
  tools:async()=>[{name:'artifact_level',description:'Stage a complete valid Doom arena. Preserve existing objects unless asked otherwise. One completed turn saves one version.',input_schema:{type:'object',properties:{level:LEVEL_SCHEMA},required:['level'],additionalProperties:false}}],
  run:async(name,input)=>{if(cancelled||wares.pending)throw Error('Doom edit stopped');if(name!=='level'||Object.keys(input).some(k=>k!=='level'))throw Error('Use the level tool');staged=validateLevel(input.level);return {bytes:compileWad(staged).length,summary:'Valid PWAD compiled; staged level has not been play-tested'};}};
async function ask(text){
  if(busy)return;const target=requestedWare(text);if(target){window.whistlegraphWorkFinished?.();window.whistlegraphSelectWare?.(target);return;}
  error='';output='';staged=null;cancelled=false;successful=false;wares.clear();
  try{
    // A few deterministic edits work offline and make the initial ware useful.
    if(/^(?:make it |make the room )?(?:brighter|darker)[.!]?$/i.test(text)){commit({...level,light:Math.max(32,Math.min(255,level.light+(/darker/i.test(text)?-32:32)))},text);return;}
    if(/^(?:remove |no )(?:all )?(?:enemies|monsters)[.!]?$/i.test(text)){commit({...level,things:level.things.filter(t=>![3004,3001,9].includes(t.type))},text);return;}
    if(!token){error='Sign in to ask the brain to edit this WAD.';post({action:'signIn'});return;}
    busy=true;attempt={request:text,status:'working',error:''};snapshot();
    server=new AcServer({token:()=>token,model:window.__whistlegraphModel||FLASH_MODEL,artifacts,controls:wares,preview:false,frameCapture:false,rounds:6,reasoning:{effort:'none'},thinking:{type:'disabled'},developerInstructions:'This is Whistlegraph. Make the requested Doom arena edit using artifact_level. Emit bounded level data, never scripts or raw binary. Explain unsupported geometry briefly. Do not claim to have play-tested, exported, or published a staged WAD. No preamble. '+WARE_INSTRUCTIONS});
    server.on('notification',({method,params})=>{if(method==='item/agentMessage/delta'){output+=params.delta;snapshot();}if(method==='turn/completed'){successful=params.turn.status==='completed'&&!params.turn.error;if(!successful&&!cancelled)error=params.turn.error?.message||'Could not finish the WAD';}});
    await server.startTurn(text);if(!cancelled&&successful&&!wares.pending&&staged)commit(staged,text);
    attempt={...attempt,status:successful?'completed':'failed',error};
  }catch(failure){error=failure.message;if(attempt)attempt={...attempt,status:'failed',error};}
  finally{const target=!cancelled&&successful?wares.pending:null;wares.clear();staged=null;busy=false;server?.close();server=null;snapshot();window.whistlegraphWorkFinished?.();if(target)window.whistlegraphSelectWare?.(target);}
}
function base64(){return btoa(String.fromCharCode(...compileWad(level)));}
async function publish(){
  if(code){output='Published: '+code;snapshot();return;}
  if(!token){post({action:'signIn'});return;}const credential=token,head=versions.head.id;busy=true;error='';snapshot();
  try{const response=await fetch('https://aesthetic.computer/api/mime',{method:'POST',headers:{Authorization:`Bearer ${credential}`,'Content-Type':'application/json'},body:JSON.stringify({text:level.title,file:{name:'Whistlegraph.wad',type:WAD_MIME,data:base64()}})});const result=await response.json();if(!response.ok)throw Error(result.error||'Could not publish WAD');if(token===credential&&versions.head.id===head){code=result.code;codes[head]=code;localStorage.setItem('whistlegraph-doom-codes',JSON.stringify(codes));output='Published: '+code;}}
  catch(failure){error=failure.message;}finally{busy=false;snapshot();}
}
window.whistlegraphNativeCommand=command=>{
  if(command.action==='signIn'){post({action:'signIn'});return;}if(command.action==='stop'){cancelled=true;server?.interrupt();return;}if(busy)return;
  try{
    if(command.action==='setWare')window.whistlegraphSelectWare?.(command.ware);
    if(command.action==='ask'&&typeof command.text==='string'&&command.text.trim()&&Array.from(new Intl.Segmenter(undefined,{granularity:'grapheme'}).segment(command.text)).length<=96)void ask(command.text.trim());
    if(command.action==='checkout')checkout(command.version);
    if(command.action==='undoDoom'&&versions.head.parent!==null)checkout(versions.head.parent);
    if(command.action==='retry'&&attempt?.request)void ask(attempt.request);
    if(command.action==='newDoom')commit(starterLevel(),'New empty WAD');
    if(command.action==='exportWad')post({action:'exportWad',data:base64()});
    if(command.action==='publishDoom')void publish();
    if(command.action==='playDoom')render();
  }catch(failure){error=failure.message;snapshot();}
};
window.whistlegraphEngineEvent=event=>{
  if(event.kind==='account'){if(event.token&&event.token===token&&handle&&!event.retry)return;token=event.token||'';handle='';window.whistlegraphAccountReady=!!token;if(!token&&busy){cancelled=true;server?.interrupt();}const expected=token;void account.connect(token,event.notice||'').then(result=>{if(result&&token===expected){handle=result.handle||'';snapshot();}});snapshot();}
  if(event.kind==='error'){error=event.text||'Sign-in failed';snapshot();window.whistlegraphWorkFinished?.();}
};
window.whistlegraphAsk=ask;window.whistlegraphAskSound=input=>ask(input.transcript?.trim()||'');window.whistlegraphIsBusy=()=>busy;
window.whistlegraphUndo=()=>{if(!busy&&versions.head.parent!==null)checkout(versions.head.parent);};
document.addEventListener('visibilitychange',()=>{if(document.hidden&&busy){cancelled=true;server?.interrupt();}});
render();snapshot();post({action:'account'});
