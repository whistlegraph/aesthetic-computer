import {AccountConnection} from './account-connection.mjs';
import {AcServer} from '/easel/src/ac-server.mjs';
import {verifyAccount} from '/easel/src/account-access.mjs';
import {DEFAULT_MODEL} from './generation-policy.mjs';
import {PieceVersions} from './piece-versions.mjs';
import {wareControls,WARE_INSTRUCTIONS,requestedWare} from './wares.mjs';
import {starterRoom,validateRoom,drawPath,localRoomEdit,ROOM_SCHEMA,ROOM_LIMITS} from './room-schema.mjs';
import {roomPreview} from './roblox-preview.mjs';
import {roomPlace} from './roblox-export.mjs';
import {saveForLaunch} from './roblox-connection.mjs';

const post=body=>window.webkit.messageHandlers.whistlegraph.postMessage({id:'roblox',...body});
const accountConnection=new AccountConnection({verify:verifyAccount,changed:state=>post({action:'accountState',...state})});
const key='whistlegraph-roblox-room',endpoint='https://aesthetic.computer/api/whistlegraph-roblox';
const versions=new PieceVersions(localStorage,key+'-versions',JSON.stringify(starterRoom()));
let room=validateRoom(JSON.parse(versions.head.source)),selected='bridge',token='',handle='',owner='',busy=false,server=null;
let notice='Saved on this phone.',error='',output='',attempt=null,staged=null,cancelled=false,successful=false;
const wares=wareControls('roblox');
document.body.classList.add('live-mode');
document.getElementById('initial').hidden=true;
const box=document.createElement('div');box.id='live-preview-box';document.getElementById('stage').prepend(box);
const preview=roomPreview(box,{getRoom:()=>staged||room,canEdit:()=>!busy&&!window.whistlegraphRecording?.(),
  onPath:points=>commit(drawPath(room,points),'Draw a path'),onSelect:id=>{selected=id;snapshot();},onError:message=>{error=message;snapshot();}});
function snapshot(){
  post({action:'snapshot',snapshot:{ware:'roblox',roblox:{selected,notice:error||notice},code:'',handle,colors:[],head:versions.head.id,
    hasPiece:true,hasPreview:true,busy,phase:busy?'Building room…':'Room plan',error,output:output.slice(-6000),attempt,
    versions:versions.value.versions.map(v=>({id:v.id,parent:v.parent,utterance:v.request||'',createdAt:v.createdAt})),
  }});
}
function commit(next,request){
  if(versions.value.versions.length>=ROOM_LIMITS.history)throw Error('This room has reached its version limit. Export it before starting another.');
  const value=validateRoom(next),source=JSON.stringify(value);
  if(source===versions.head.source)return false;
  versions.commit({source,request,layers:1});room=value;staged=null;error='';notice='Saved on this phone.';
  preview.paint();snapshot();return true;
}
function checkout(id){
  const version=versions.value.versions.find(v=>v.id===id);
  if(!version)throw Error('Version unavailable');
  const next=validateRoom(JSON.parse(version.source));
  versions.checkout(id);room=next;staged=null;error='';notice='Saved on this phone.';preview.paint();snapshot();
}
const artifacts={
  context:async()=>`You are editing a Whistlegraph Roblox Room. The app shows a top-down room plan, not Roblox physics. All content is bounded data; never emit scripts. Existing object IDs must remain stable. Coordinates are Roblox studs: Y is height, X/Z are the plan. Use platform or goal; bounce is upward speed (0 disables it). Preserve a reachable spawn and the rest of the room. Selected object: ${selected}. Current room: ${JSON.stringify(staged||room)}`,
  tools:async()=>[{name:'artifact_room',description:'Stage a complete valid room description. Preserve existing objects unless asked to remove them. One successful request saves one version after the turn finishes.',input_schema:{type:'object',properties:{room:ROOM_SCHEMA},required:['room'],additionalProperties:false}}],
  run:async(name,input)=>{
    if(cancelled||wares.pending)throw Error('Room editing stopped');
    if(name!=='room'||Object.keys(input).some(k=>k!=='room'))throw Error('Use the room tool');
    staged=validateRoom(input.room);preview.paint();snapshot();
    return {version:versions.head.id,summary:'Room plan staged; Roblox execution not tested',objects:staged.objects.length};
  },
};
async function ask(text){
  if(busy)return;
  if(!String(text).trim()){error='Say what should change in the room.';snapshot();window.whistlegraphWorkFinished?.();return;}
  const target=requestedWare(text);
  if(target){window.whistlegraphWorkFinished?.();window.whistlegraphSelectWare?.(target);return;}
  error='';output='';wares.clear();cancelled=false;successful=false;staged=null;
  try {
    const local=localRoomEdit(room,text,selected);
    if(local){commit(local,text);return;}
    if(!token){error='Sign in to ask the brain to edit this room.';post({action:'signIn'});return;}
    busy=true;attempt={request:text,status:'working',error:''};preview.cancel();snapshot();
    server=new AcServer({token:()=>token,model:window.__whistlegraphModel||DEFAULT_MODEL,artifacts,controls:wares,preview:false,frameCapture:false,rounds:6,reasoning:{effort:'none'},thinking:{type:'disabled'},
      developerInstructions:'This is Whistlegraph. Make the requested room edit using artifact_room. Keep changes small and use only the supported grammar. Do not claim to have run Roblox, published a game, or linked an account. No preamble. If the requested behavior is unsupported, explain that briefly instead of inventing a component. '+WARE_INSTRUCTIONS});
    server.on('notification',({method,params})=>{
      if(method==='item/agentMessage/delta'){output+=params.delta;snapshot();}
      if(method==='turn/completed'){successful=params.turn.status==='completed'&&!params.turn.error;if(!successful&&!cancelled)error=params.turn.error?.message||'Could not finish the room';}
    });
    await server.startTurn(text);
    if(!cancelled&&successful&&!wares.pending){
      if(staged)commit(staged,text);
      else notice=output.trim().slice(0,500)||'No change to the room.';
    }
    attempt={...attempt,status:successful?'completed':'failed',error};
  }catch(failure){error=failure.message;if(attempt)attempt={...attempt,status:'failed',error};}
  finally{
    const target=!cancelled&&successful?wares.pending:null;wares.clear();
    staged=null;busy=false;server?.close();server=null;preview.paint();snapshot();
    window.whistlegraphWorkFinished?.();
    if(target)window.whistlegraphSelectWare?.(target);
  }
}
async function requestRoom(method,body){
  const response=await fetch(endpoint,{method,headers:{Authorization:`Bearer ${token}`,'Content-Type':'application/json'},...(body?{body:JSON.stringify(body)}:{}),signal:AbortSignal.timeout(15000)});
  const data=await response.json().catch(()=>null);
  if(!response.ok)throw Error(data?.error||(response.status===404?'Roblox connection is not set up yet.':`Room service unavailable (${response.status})`));
  return data;
}
async function play(){
  if(busy)return;
  if(!token){error='Sign in to connect this room to Roblox.';post({action:'signIn'});snapshot();return;}
  if(!owner){error='Wait for your account to connect.';notice=error;snapshot();return;}
  const identity=owner,credential=token;
  const request=async(...args)=>{
    if(owner!==identity||token!==credential)throw Error('Account changed. Tap Play again.');
    const result=await requestRoom(...args);
    if(owner!==identity||token!==credential)throw Error('Account changed. Tap Play again.');
    return result;
  };
  busy=true;error='';notice='Saving room for Roblox…';snapshot();
  try {
    const config=await request('GET');
    if(!config.available)throw Error('Roblox connection is not set up yet. You can export this room for Studio.');
    // The backend binds this signed-in maker to a configured Roblox pilot
    // identity. The client never supplies or trusts a Roblox user ID.
    const data=await saveForLaunch({storage:localStorage,owner:identity,room,request});
    notice='Saved for Roblox. Opening…';post({action:'openRoblox',url:data.launchURL});
  }catch(failure){error=failure.message;notice=error;}
  finally{busy=false;snapshot();}
}
window.whistlegraphNativeCommand=command=>{
  if(command.action==='signIn'){post({action:'signIn'});return;}
  if(command.action==='stop'){cancelled=true;server?.interrupt();return;}
  if(busy)return;
  try {
    if(command.action==='setWare')window.whistlegraphSelectWare?.(command.ware);
    if(command.action==='ask'&&typeof command.text==='string'&&command.text.trim()&&Array.from(new Intl.Segmenter(undefined,{granularity:'grapheme'}).segment(command.text)).length<=96)void ask(command.text.trim());
    if(command.action==='checkout')checkout(command.version);
    if(command.action==='undoRoom'&&versions.head.parent!==null)checkout(versions.head.parent);
    if(command.action==='retry'&&attempt?.request)void ask(attempt.request);
    if(command.action==='playRoblox')void play();
    if(command.action==='exportRoom')post({action:'exportRoom',source:roomPlace(room)});
  }catch(failure){error=failure.message;snapshot();}
};
window.whistlegraphEngineEvent=event=>{
  if(event.kind==='account'){
    if(event.token&&event.token===token&&handle&&!event.retry)return;
    token=event.token||'';handle='';owner='';window.whistlegraphAccountReady=!!token;
    if(!token&&busy){cancelled=true;server?.interrupt();}
    const expected=token;
    void accountConnection.connect(token,event.notice||'').then(account=>{if(account&&token===expected){handle=account.handle||'';owner=account.sub;snapshot();}});
    snapshot();
  }
  if(event.kind==='error'){error=event.text||'Sign-in failed';snapshot();window.whistlegraphWorkFinished?.();}
  if(event.kind==='robloxLaunch'){notice=event.opened?'Room saved. Switch back here to keep editing.':'Could not open the Roblox link.';snapshot();}
};
window.whistlegraphAsk=ask;
window.whistlegraphAskSound=input=>ask(input.transcript?.trim()||'');
window.whistlegraphIsBusy=()=>busy;
window.whistlegraphUndo=()=>{if(!busy&&versions.head.parent!==null)checkout(versions.head.parent);};
document.addEventListener('visibilitychange',()=>{
  if(document.hidden){preview.cancel();if(busy){cancelled=true;server?.interrupt();}}
  else if(owner&&!busy){
    const identity=owner,saved=JSON.parse(localStorage.getItem('whistlegraph-roblox-cloud-'+identity)||'null');
    if(saved&&saved.source===JSON.stringify(room))void requestRoom('GET').then(status=>{
      if(identity===owner&&saved.source===JSON.stringify(room)&&status.appliedRevision===saved.revision){notice='Applied in Roblox.';snapshot();}
    }).catch(()=>{});
  }
});
snapshot();post({action:'account'});
