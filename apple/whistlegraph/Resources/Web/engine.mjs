import {withDrawing,inputData,drawingImage,drawingContent} from './drawing-input.mjs';
import {reviewVisualResult,reviewWithRepair} from './visual-review.mjs';
import {inferenceRequest,wantsSoundEvidence} from './inference-input.mjs';
import {contextualRequest,selectedBranch} from './branch-context.mjs';
import {compileEditContract,sourceChecks,validateCandidate,runEditExperiment} from '/easel/src/edit-contract.mjs';
import {ReceiptJournal,AttemptReceipt,hashSource} from '/easel/src/attempt-receipt.mjs';
import {pieceCaption} from './piece-caption.mjs';
import {readAttempt,saveAttempt,claimAttempt} from './attempt-recovery.mjs';
import {initializeBasePiece,isBasePiece} from './base-piece.mjs';
import {handleCharacterColors,fetchHandleColors} from '/easel/src/handle-colors.mjs';
import {verifyAccount} from '/easel/src/account-access.mjs';
import {localEdit} from './local-scene.mjs';
import {WalkiewareThread,verifyThreadRevision} from '/easel/src/walkieware-thread.mjs';
import {instantPiece} from './instant-piece.mjs';
import {MusicalInputSocket} from '/easel/src/musical-input-socket.mjs';
import {MusicalInputAdvisor} from '/easel/src/musical-input-advisor.mjs';
import {musicalPrompt} from './musical-input.mjs';
import {PieceVersions} from './piece-versions.mjs';
// The same inference/tool loop as Aesel, with a piece-first streaming renderer.
import {AcServer} from '/easel/src/ac-server.mjs';
import {DEFAULT_MODEL,GENERATION_INSTRUCTIONS} from './generation-policy.mjs';
import {runnablePrefix} from './stream-preview.mjs';
import * as vfs from '/easel/phone/shim/fs.mjs';

const post = body => window.webkit.messageHandlers.walkie.postMessage({id:'engine',...body});
const benchmark=(event,fields={})=>{post({action:'benchmark',event,fields});window.__walkiewareSequenceEvent?.(event,fields);};
const file = '/piece/walkieware.mjs';
const storageKey=window.__whistlegraphFixture?'whistlegraph-fixture-source':window.__walkiewareSpace?'walkieware-space-source':window.__walkiewareLocalSequence?'walkieware-local-source':window.__walkiewareSequence?'walkieware-sequence-source':window.__walkiewareBenchmark?'walkieware-benchmark-source':'walkieware-source';
const receipts=new ReceiptJournal(localStorage,storageKey);
let activeReceipt=null,renderID=0,previewHash=null,validationChecks=[];
let visualController=null,pendingCapture=null;
let turnStarter='',starterPainted=false,turnCancelled=false;
let thread=null,threadTimer=null;
const runtimeErrors=[];
let lastAttempt=null,activeAttempt=null,recoveryPending=true,presentedVersion=null,narrationPending=null;
try{lastAttempt=JSON.parse(localStorage.getItem(storageKey+'-attempt')||'null');}catch{}
let versions=null,turnSucceeded=false,turnRequest='',turnParent=null,turnRuntimeFailed=false,turnError='';
let token = '', busy = false, server, source = '', previous = '', pending = '', checkpoints = 0;
let feedback = null, lastPaintedSource = '', previewSource = '', provisional = '', compileTimer = null;
let outputStream='',reasoningStream='',code = '', codeItem = '', firstDelta = false, started = 0, timer, ready = false, painted = false;
const events = [];
const signIn=document.createElement('button');signIn.id='connect-ac';signIn.textContent='Account';signIn.onclick=()=>post({action:'signIn'});const identity=document.createElement('div');identity.id='walkieware-identity';const codeLabel=document.createElement('span');codeLabel.id='walkieware-thread';identity.append(signIn,codeLabel);document.body.append(identity);
let accountToken='',accountHandle='',accountPalette=[];
function paintHandle(handle,colors=handleCharacterColors('@'+handle)){
  accountPalette=colors;
  signIn.replaceChildren(...Array.from('@'+handle,(character,index)=>{const span=document.createElement('span');span.textContent=character;span.style.color='rgb('+colors[index].join(',')+')';return span;}));nativeSnapshot();
}
function accountIdentity(value){
  if(value===accountToken)return;accountToken=value;accountHandle='';signIn.textContent=value?'…':'Sign in';
  if(value)void verifyAccount(value).then(account=>{if(accountToken!==value)return;accountHandle=account.handle;if(!accountHandle){signIn.textContent='Set handle';return;}paintHandle(accountHandle);const handle=accountHandle;void fetchHandleColors('@'+handle).then(colors=>{if(accountToken===value&&accountHandle===handle)paintHandle(handle,colors);}).catch(()=>{});}).catch(()=>{if(accountToken===value)signIn.textContent='Retry sign-in';});
}
const $ = id => document.getElementById(id);
const ui = document.createElement('section'); ui.id = 'live-work'; ui.hidden = true;
ui.innerHTML = '<div class="live-line"><strong id="live-phase"></strong><span id="live-time"></span><button id="live-stop">Stop</button></div><p id="live-request"></p><ol id="version-feed" aria-label="Versions"></ol><details id="live-details" hidden><pre id="live-code"></pre><ol id="live-events"></ol></details>';
document.body.append(ui);
$('live-stop').hidden=true;
const frame = document.createElement('iframe'); frame.id = 'live-piece'; frame.title = 'Your piece, live';
frame.allow = 'autoplay'; frame.src = 'https://aesthetic.computer/wipe?noauth=true&noplot=true&nogap=true&nolabel=true&preview=walkieware';
const previewBox=document.createElement('div');previewBox.id='live-preview-box';previewBox.append(frame,identity);$('stage').prepend(previewBox);
function positionHistory(){ui.style.top=(previewBox.getBoundingClientRect().bottom+identity.getBoundingClientRect().height+16)+'px';}
new ResizeObserver(positionHistory).observe(previewBox);
new ResizeObserver(positionHistory).observe(identity);
window.addEventListener('resize',positionHistory);
positionHistory();
function log(text) {
  events.push(`${((performance.now()-started)/1000).toFixed(2)}s · ${text}`);
  if(events.length>60) events.shift();
  $('live-events').replaceChildren(...events.map(text=>{const li=document.createElement('li');li.textContent=text;return li;}));
}
function threadUpdate(){nativeSnapshot();clearTimeout(threadTimer);threadTimer=setTimeout(()=>{try{if(lastAttempt)localStorage.setItem(storageKey+'-attempt',JSON.stringify(lastAttempt));}catch{}thread?.sync();thread?.update();},100);}
function phase(text) { $('live-phase').textContent = text; updateFeed(); threadUpdate(); }
function musicalData(request) {
  try{return JSON.parse(request.split('\nINPUT DATA:\n')[1]);}catch{return null;}
}
function utterance(request) {return musicalData(request)?.transcript|| (musicalData(request)?.drawing?'Drawing':musicalData(request)?'Sound':request)||'Starting piece';}
function relativeTime(date) {
  const seconds=Math.max(0,Math.floor((Date.now()-Date.parse(date))/1000));
  if(!Number.isFinite(seconds))return '';
  if(seconds<60)return 'just now';
  if(seconds<3600)return Math.floor(seconds/60)+'m ago';
  if(seconds<86400)return Math.floor(seconds/3600)+'h ago';
  return Math.floor(seconds/86400)+'d ago';
}
function soundContour(input) {
  if(!input?.sound?.frames?.length)return null;
  const svg=document.createElementNS('http://www.w3.org/2000/svg','svg');
  svg.setAttribute('viewBox','0 0 260 54');svg.classList.add('utterance-sound');
  svg.setAttribute('role','img');svg.setAttribute('aria-label','Recorded sound: pitch contour and loudness over '+(input.sound.durationMs/1000).toFixed(1)+' seconds');
  const duration=Math.max(1,input.sound.durationMs);const pitched=input.sound.frames.filter(f=>Number.isFinite(f.pitchHz)&&f.pitchHz>0);
  const pitches=pitched.map(f=>Math.log2(f.pitchHz));const low=Math.min(...pitches),span=Math.max(.5,Math.max(...pitches)-low);
  const peak=Math.max(.001,...input.sound.frames.map(f=>f.rms||0));
  for(const f of input.sound.frames){
    const x=4+f.atMs/duration*252;
    const line=document.createElementNS(svg.namespaceURI,'path');
    line.setAttribute('d',`M${x} 52v-${Math.max(1,(f.rms||0)/peak*12)}`);line.setAttribute('stroke','#897697');svg.append(line);
  }
  let path='',last=-Infinity;
  for(const f of pitched){const x=4+f.atMs/duration*252,y=32-(Math.log2(f.pitchHz)-low)/span*28;path+=(f.atMs-last>150?'M':'L')+x+' '+y+' ';last=f.atMs;const dot=document.createElementNS(svg.namespaceURI,'circle');dot.setAttribute('cx',x);dot.setAttribute('cy',y);dot.setAttribute('r','1.5');dot.setAttribute('fill','currentColor');svg.append(dot);}
  const trace=document.createElementNS(svg.namespaceURI,'path');trace.setAttribute('d',path);trace.setAttribute('fill','none');trace.setAttribute('stroke','currentColor');trace.setAttribute('stroke-width','3');trace.setAttribute('stroke-linecap','round');svg.append(trace);
  return svg;
}
function jumpVersion(id) {
  if(busy||window.walkiewareRecording?.())return;
  source=versions.checkout(id).source;previous=source;lastAttempt=null;
  server?.close();server=null;vfs.mount(file,source);saved();
  render(source||'export function paint({wipe}) {wipe("black");}');review(false);phase('');
}
setInterval(()=>document.querySelectorAll('#version-feed time').forEach(t=>t.textContent=relativeTime(t.dateTime)),15000);
let postPieces=()=>{};
let nativeTimer=null,nativeLedger=null,nativeRevisions=[],nativeLast='',captionSource=null,caption='';
function nativeSnapshot(){
  if(!window.__walkiewareNativeShell||nativeTimer)return;
  nativeTimer=setTimeout(()=>{
    nativeTimer=null;
    const historyChanged=nativeLedger!==versions?.value;
    if(historyChanged){nativeLedger=versions?.value;nativeRevisions=(versions?.value.versions||[]).map(v=>{const sound=musicalData(v.request)?.sound;return {id:v.id,parent:v.parent,hasDrawing:!!musicalData(v.request)?.drawing,utterance:(v.id===0&&!v.request?'':utterance(v.request)).slice(0,1000),createdAt:v.createdAt,recordingID:sound?.recordingID||null,words:(musicalData(v.request)?.words||[]).slice(0,256),sound:sound?{durationMs:sound.durationMs,frames:sound.frames.filter((_,i)=>i%Math.max(1,Math.ceil(sound.frames.length/64))===0)}:null};});}
    const displayVersion=presentedVersion===null?versions?.head:versions?.value.versions.find(v=>v.id===presentedVersion);
    if(captionSource!==displayVersion?.source){captionSource=displayVersion?.source;caption=pieceCaption(captionSource||'');}
    const phaseText=$('live-phase').textContent;
    const snapshot={output:outputStream,caption,code:thread?.identity.code||'',handle:accountHandle,colors:accountPalette,head:displayVersion?.id||0,hasPiece:!!source.trim(),hasPreview:!!source.trim()||!!provisional.trim(),busy,phase:phaseText,error:!busy&&/error|unavailable|could not|sign.in|loading|no piece/i.test(phaseText)?phaseText:'',attempt:lastAttempt?{request:lastAttempt.request.slice(0,1000),status:lastAttempt.status,error:lastAttempt.error||''}:null};
    const serialized=JSON.stringify(snapshot);if(serialized===nativeLast&&!historyChanged)return;nativeLast=serialized;
    if(historyChanged)snapshot.versions=nativeRevisions;
    post({action:'snapshot',snapshot});
    if(historyChanged)postPieces();
  },80);
}
window.walkiewareNativeCommand=command=>{
  if(command.action==='ask'&&typeof command.text==='string'&&(command.text.trim()||command.drawing)&&Array.from(new Intl.Segmenter(undefined,{granularity:'grapheme'}).segment(command.text)).length<=96&&!busy)void window.walkiewareAskDrawing(command.text.trim(),command.drawing);
  if(command.action==='checkout'&&!busy){presentedVersion=null;narrationPending=command.version;jumpVersion(command.version);}
  if(command.action==='presentVersion'&&!busy){
    const version=versions?.value.versions.find(v=>v.id===command.version);
    if(version){presentedVersion=version.id;narrationPending=version.id;render(version.source||'export function paint({wipe}){wipe("black");}');nativeSnapshot();}
  }
  if(command.action==='endPresentation'){presentedVersion=null;narrationPending=null;render(source);nativeSnapshot();}
  if(command.action==='newPiece'&&!busy)window.walkiewareNewPiece?.();
  if(command.action==='openPiece'&&!busy&&typeof command.piece==='string')window.walkiewareOpenPiece?.(command.piece);
  if(command.action==='retry'&&!busy)void resumeAttempt(true);
  if(command.action==='stop')$('live-stop').click();
  if(command.action==='signIn')post({action:'signIn'});
};
function updateFeed() {
  if(!versions)return;
  const rows=versions.value.versions.slice().reverse().map(v=>{
    const li=document.createElement('li');
    const number=document.createElement('span');number.textContent='v'+v.id;
    const words=document.createElement('span');words.textContent=(v.id===0&&!v.request?'':utterance(v.request));
    li.append(number,words);
    const time=document.createElement('time');time.dateTime=v.createdAt;time.title=new Date(v.createdAt).toLocaleString();time.textContent=relativeTime(v.createdAt);li.append(time);
    if(v.parent!==null&&v.parent!==v.id-1){const branch=document.createElement('span');branch.className='version-parent';branch.textContent='↳ from v'+v.parent;li.append(branch);}
    const contour=soundContour(musicalData(v.request));if(contour)li.append(contour);
    li.tabIndex=0;li.setAttribute('role','button');li.setAttribute('aria-label','Version '+v.id+': '+(v.id===0&&!v.request?'':utterance(v.request)));
    li.onclick=()=>jumpVersion(v.id);li.onkeydown=e=>{if(e.key==='Enter'||e.key===' '){e.preventDefault();jumpVersion(v.id);}};
    if(v.id===versions.head.id)li.setAttribute('aria-current','true');
    return li;
  });
  if(lastAttempt&&['working','failed','unchanged','interrupted'].includes(lastAttempt.status)){
    const li=document.createElement('li');const status=document.createElement('span');
    status.textContent=lastAttempt.status==='working'?'…':lastAttempt.status==='failed'?'Failed':'Same';
    const words=document.createElement('span');words.textContent=lastAttempt.request;
    li.append(status,words);rows.unshift(li);
  }
  $('version-feed').replaceChildren(...rows);
  $('live-request').hidden=true;
  $('live-phase').hidden=!busy&&!/error|unavailable|could not|sign.in|loading|no piece/i.test($('live-phase').textContent);
  $('live-time').hidden=!busy;nativeSnapshot();
}

function render(value) {
  previewSource=value;painted=false;feedback=null;previewHash=null;
  const id=++renderID;
  void hashSource(value).then(hash=>{
    if(id!==renderID)return;
    previewHash=hash;
    feedback={rendered:false,sourceHash:hash,revision:hash,requestID:id,logs:sourceChecks(value).map(f=>({level:'error',text:f.message,code:f.code})),updatedAt:new Date().toISOString()};
    post({action:'render',source:value,threadID:thread?.identity.id,renderID:id});
  }).catch(()=>{if(id===renderID){turnRuntimeFailed=true;turnError='Could not identify preview source';phase(turnError);}});
}
async function finishReceipt(status) {
  if(!activeReceipt)return;
  activeReceipt.value.checkpoints=checkpoints;
  activeReceipt.finish(status,await hashSource(source).catch(()=>null),validationChecks);
  activeReceipt=null;thread?.flushReceipts();
}
function captureVisual(sourceHash, expectedRenderID, signal) {
  return new Promise((resolve,reject)=>{
    const captureID=crypto.randomUUID();
    const finish=(error,value)=>{clearTimeout(timer);signal.removeEventListener('abort',abort);if(pendingCapture?.id===captureID)pendingCapture=null;error?reject(error):resolve(value);};
    const abort=()=>{post({action:'cancelVisualCapture'});finish(Error('Visual capture stopped'));};
    const timer=setTimeout(()=>{post({action:'cancelVisualCapture'});finish(Error('Visual capture unavailable'));},12000);
    pendingCapture={id:captureID,finish};
    signal.addEventListener('abort',abort,{once:true});
    if(signal.aborted){abort();return;}
    const r=frame.getBoundingClientRect();
    post({action:'visualCapture',captureID,sourceHash,renderID:expectedRenderID,viewport:{width:window.innerWidth,height:window.innerHeight},rect:{x:r.x,y:r.y,width:r.width,height:r.height}});
  });
}
async function checkVisualResult() {
  clearTimeout(compileTimer);compileTimer=null;provisional='';
  visualController=new AbortController();
  const signal=visualController.signal;
  const deadline=setTimeout(()=>{visualController?.abort();server?.interrupt();},90000);
  const task=contextualRequest(versions.value,inferenceRequest(turnRequest));
  const drawing=inputData(turnRequest)?.drawing;
  const chalk=drawing?drawingImage(drawing):null;
  try {
    return await reviewWithRepair({cancelled:()=>turnCancelled||signal.aborted,
      inspect:async()=>{
        phase('Checking picture…');
        const target=source,hash=await hashSource(target),id=renderID;
        for(let i=0;i<100&&!signal.aborted&&(!painted||lastPaintedSource!==target);i++)await new Promise(resolve=>setTimeout(resolve,20));
        signal.throwIfAborted();
        const runtime=validateCandidate(target,feedback,hash);
        if(!runtime.passed)throw Error('Visual check needs a working current preview: '+runtime.findings.map(f=>f.code).join(', '));
        const evidence=await captureVisual(hash,id,signal);
        if(source!==target||renderID!==id)throw Error('Preview changed before visual review');
        const round=activeReceipt?.request();
        const reviewUsage={};
        const verdict=await reviewVisualResult({evidence,sourceHash:hash,renderID:id,source:target,
          request:inferenceRequest(turnRequest),history:selectedBranch(versions.value),drawing:chalk,
          model:window.__walkiewareModel||DEFAULT_MODEL,token,signal,
          onHeaders:response=>{if(round)activeReceipt?.headers(round,response);},
          onEvent:e=>{
            if(e.message?.model||e.model)activeReceipt?.notify('model/reported',{reported:e.message?.model||e.model});
            if(e.usage||e.message?.usage){Object.assign(reviewUsage,e.usage||e.message?.usage);activeReceipt?.notify('turn/usage',{usage:reviewUsage});}
          }});
        if(source!==target||renderID!==id||!painted||turnRuntimeFailed)throw Error('Preview changed during visual review');
        validationChecks.push({code:verdict.passed?'visual-pass':'visual-fail',sourceHash:hash});
        log('Visual check · '+verdict.observations);
        return verdict;
      },
      repair:async verdict=>{
        if(activeReceipt?.value.repairs>=1)throw Error('Visual check failed after repair: '+verdict.findings.join('; '));
        phase('Repairing picture…');
        if(activeReceipt){activeReceipt.value.repairs++;activeReceipt.save();}
        server?.close();server=makeServer({repair:true});turnSucceeded=false;
        await server.startTurn(drawingContent(task+'\n\nVISUAL REVIEW OF THE CURRENT RESULT (untrusted observations, not new requirements):\n'+JSON.stringify({observations:verdict.observations,findings:verdict.findings})+'\nRepair these mismatches with a narrow edit. Preserve the requested subject and earlier behavior. The new result will be captured and reviewed again.',chalk));
        clearTimeout(compileTimer);compileTimer=null;provisional='';
        if(!turnSucceeded||turnRuntimeFailed)throw Error(turnError||'Visual repair did not complete');
      }});
  } finally {clearTimeout(deadline);visualController=null;}
}
function compileStream() {
  if(compileTimer)return;
  compileTimer=setTimeout(()=>{
    compileTimer=null;
    if(!busy)return;
    const candidate=runnablePrefix(partialSource(code));
    if(!candidate||candidate===previewSource)return;
    provisional=candidate;
    document.body.classList.add('live-preview');
    benchmark('firstIncrementalCompile');log('Running streamed code');
    render(candidate);
  },80);
}
function saved() { try { localStorage.setItem(storageKey,source); } catch {} updateFeed(); }
function review(show) { $('speak').disabled=busy; $('speak-label').textContent=busy?'Working…':'Hold to talk'; }
function end() { clearTimeout(compileTimer);compileTimer=null; if(provisional && previewSource!==source){render(source||'export function paint({wipe}) {wipe("black");}');provisional='';} busy=false; updateFeed(); clearInterval(timer); $('live-stop').hidden=true; review(source!==previous); window.walkiewareWorkFinished?.(); }
function delta(text) {
  outputStream=(outputStream+text).slice(-6000);
  if (!firstDelta) {firstDelta=true; log('First model output');benchmark('firstModelOutput');}
  phase('Writing…');
}
function partialSource(json,field='source') {
  const start=json.match(field==='replace'?/"replace"\s*:\s*"/:/"source"\s*:\s*"/);if(!start)return '';
  const raw=json.slice(start.index+start[0].length);
  let out='';for(let i=0;i<raw.length;i++){
    const c=raw[i];if(c==='"')break;
    if(c!=='\\'){out+=c;continue;}
    const next=raw[++i];if(next===undefined)break;
    if(next==='u'){const hex=raw.slice(i+1,i+5);if(!/^[0-9a-f]{4}$/i.test(hex))break;out+=String.fromCharCode(parseInt(hex,16));i+=4;}
    else out+=({n:'\n',r:'\r',t:'\t',b:'\b',f:'\f','"':'"','\\':'\\','/':'/'})[next]??'';
  }return out;
}
vfs.setWriteHandler((path,value)=>{
  if(path!==file)return;
  clearTimeout(compileTimer);compileTimer=null;provisional='';
  source=value;checkpoints++;benchmark('firstCheckpoint');
  turnRuntimeFailed=false;
  document.body.classList.add('live-preview'); $('initial').hidden=true;
  $('play-deck').hidden=true;
  $('live-code').textContent=value;
  phase(ready?'Evaluating…':'Loading preview…'); log(`Checkpoint ${checkpoints} · valid JavaScript`);
  if(value!==previewSource)render(value);
});
const guides = vfs.preload(['pieces.md','screen.md','hand.md','kidlisp.md','api.json'].map(name=>'/easel/context/'+name));
function makeServer({repair=false}={}){
  const value=new AcServer({cwd:'/piece',piece:{file,checkpoint:async()=>{const target=source;for(let i=0;i<100;i++){if(painted&&lastPaintedSource===target)return;await new Promise(resolve=>setTimeout(resolve,20));}}},frameCapture:false,layeredEdits:true,token:()=>token,model:window.__walkiewareModel||DEFAULT_MODEL,
    fetch:async(url,options)=>{
      benchmark('requestDispatched');const recorder=activeReceipt,round=recorder?.request();
      const body=JSON.parse(options.body);body.max_tokens=4096;options={...options,body:JSON.stringify(body)};
      const response=await globalThis.fetch(url,options);if(round)recorder.headers(round,response);
      benchmark('inferenceHeaders',{status:response.status});return response;
    },preview:true,rounds:repair?2:4,outputContinuations:repair?0:1,reasoning:{effort:'none'},thinking:{type:'disabled'},
    developerInstructions:GENERATION_INSTRUCTIONS});
  value.runtimeFeedback=()=>feedback;
  value.on('notification',({method,params})=>{
    activeReceipt?.notify(method,params);
    if(method==='turn/progress' && !firstDelta) phase(params.phase==='connecting'?'Connecting…':'Waiting for model…');
    if(method==='item/modelCode/delta'){
      delta('');if(codeItem!==params.itemId){benchmark('layerStarted',{tool:params.tool||'write_piece'});code='';codeItem=params.itemId;}
      code+=params.delta;const visibleCode=params.tool==='edit_piece'?partialSource(code,'replace'):partialSource(code);$('live-code').textContent=visibleCode;outputStream=visibleCode.slice(-6000);nativeSnapshot();if(params.tool!=='edit_piece')compileStream();else phase('Editing…');
      $('live-details').open=true;
    }
    if(method==='item/agentMessage/delta'){delta(params.delta);$('live-request').textContent+=params.delta;}
    // Reasoning streams before any code. Let it fly by in the ticker, dimmed by
    // the native side, so the wait reads as work rather than silence.
    if(method==='item/reasoning/delta'){reasoningStream=(reasoningStream+params.delta).slice(-6000);if(!firstDelta){benchmark('firstReasoning');log('Model thinking');}if(!code)outputStream=reasoningStream;phase('Thinking…');nativeSnapshot();}
    if(method==='item/started')benchmark('toolStarted',{tool:params.item?.tool||params.item?.type});
    if(method==='item/completed' && params.item?.status?.startsWith('failed')) {log(params.item.status);benchmark('toolFailed',{message:params.item.status});}
    if(method==='turn/usage') log('Usage · '+(params.usage.output_tokens??0)+' output tokens');
    if(method==='turn/completed'){turnSucceeded=!params.turn.error&&params.turn.status==='completed';turnError=params.turn.error?.message||(params.turn.status==='interrupted'?'Stopped':'');
      if(params.turn.error){phase('Could not finish');log(params.turn.error.message);}
      else if(params.turn.status==='interrupted'){phase('Stopped');log('Stopped by you');}
      else {phase(painted?'Checking picture…':source?'Waiting for preview…':'No piece written');log('Model finished');}
    }
  });return value;
}
async function ask(text,displayText=text,advice=null,starter=null,localText=text,recovered=null){
  if(busy)return;
  if(!versions){phase('Version history unavailable');return;}
  benchmark('generationEntered');
  document.body.classList.add('live-mode');
  pending=text;ui.hidden=false;$('live-request').textContent=displayText;started=performance.now();events.length=0;
  $('live-stop').hidden=false;$('live-details').open=false;
  if(!token){phase('Sign in to make software');log('Uses your AC braincells. Speech stays on device; submitted words go to AC.');post({action:'signIn'});return;}
  try {
    activeAttempt=saveAttempt(localStorage,storageKey,recovered||{id:crypto.randomUUID(),text,displayText,localText,parent:versions.head.id,baseSource:versions.head.source,retries:0,status:'working'});
  } catch { phase('Could not save request for recovery');return; }
  recoveryPending=false;
  pending='';busy=true;outputStream='';reasoningStream='';previous=source;turnStarter='';starterPainted=false;turnCancelled=false;turnSucceeded=false;turnRuntimeFailed=false;turnError='';turnRequest=text;turnParent=versions?.head.id;review(false);firstDelta=false;checkpoints=0;code='';codeItem='';
  lastAttempt={request:displayText.slice(0,20000),parent:turnParent,status:'working',startedAt:new Date().toISOString()};
  document.body.classList.add('live-mode');phase('Sending…');log('Submitted');
  timer=setInterval(()=>$('live-time').textContent=((performance.now()-started)/1000).toFixed(1)+'s',100);
  let noChange=false;
  validationChecks=[];runtimeErrors.length=0;
  try{
    activeReceipt=new AttemptReceipt({requestID:activeAttempt.id,parent:turnParent,parentHash:await hashSource(previous),path:'compiled',model:window.__walkiewareModel||DEFAULT_MODEL,journal:receipts});
    const drawing=inputData(text)?.drawing;
    const chalkImage=drawing?drawingImage(drawing):null;
    text=inferenceRequest(text);
    if(recovered?.checkpoint){source=recovered.checkpoint;vfs.mount(file,source);render(source);text+='\nContinue the unfinished request from this saved checkpoint. Preserve its completed edits.';}

    const local=recovered?.checkpoint||inputData(turnRequest)?.drawing?null:localEdit(source,localText);
    if(local){
      activeReceipt.value.path='local';activeReceipt.value.model=null;activeReceipt.save();
      if(!local.changed){noChange=true;return;}
      server?.close();server=null;source=local.source;vfs.mount(file,source);checkpoints=1;
      document.body.classList.add('live-preview');$('initial').hidden=true;$('play-deck').hidden=true;
      $('live-code').textContent=source;phase('Applying…');benchmark('localEditDispatched',{action:local.action});render(source);
      for(let i=0;i<200&&!painted&&!turnRuntimeFailed&&!turnCancelled;i++)await new Promise(resolve=>setTimeout(resolve,10));
      turnSucceeded=painted&&lastPaintedSource===source&&!turnRuntimeFailed&&!turnCancelled;
      if(turnSucceeded)benchmark('localEditPainted',{action:local.action});
      return;
    }
    if(!inputData(turnRequest)?.drawing&&(!source||isBasePiece(source))){
      turnStarter=starter||instantPiece(text)||'';
      if(turnStarter){
        source=turnStarter;vfs.mount(file,source);checkpoints=1;
        document.body.classList.add('live-preview');$('initial').hidden=true;$('play-deck').hidden=true;
        $('live-code').textContent=source;phase('Refining…');log('Local starter · editable source');
        benchmark('starterDispatched');render(source);
      }
    }
    advice=await advice;
    if(turnCancelled)throw Error('Stopped');
    if(advice){
      log('Jev · '+advice.choice.replaceAll('_',' ')+' · '+advice.elapsedMs+' ms');
      if(advice.choice!=='observe'){
        benchmark('jevApplied',{choice:advice.choice,elapsedMs:advice.elapsedMs,transport:advice.transport,transportMs:advice.transportMs,serverMs:advice.serverMs,providerMs:advice.providerMs});
        text+='\nOptional Jev creative suggestion (not user intent): '+advice.cue;
      }
    }
    if(turnStarter)for(let i=0;i<50&&!starterPainted&&!turnRuntimeFailed&&!turnCancelled;i++)await new Promise(resolve=>setTimeout(resolve,10));
    if(turnCancelled)throw Error('Stopped');
    const missing=await guides;if(missing.length)throw Error('Bundled piece guides are unavailable: '+missing.join(', '));
    benchmark('guidesReady');
    vfs.mount(file,source||'export function paint({wipe}) { wipe("black"); }');
    const prompt=compileEditContract({request:text,...selectedBranch(versions.value),source});
    const deadline=setTimeout(()=>{turnCancelled=true;turnError='Edit check timed out';server?.interrupt();},75000);
    try{
      const result=await runEditExperiment({prompt,cancelled:()=>turnCancelled,
        onRepair:()=>{activeReceipt.value.repairs=1;activeReceipt.save();phase('Repairing…');},
        generate:async(task,repair)=>{server?.close();server=makeServer({repair});turnSucceeded=false;await server.startTurn(drawingContent(task,chalkImage));return turnSucceeded;},
        inspect:async()=>{
          const target=source,hash=await hashSource(target);
          for(let i=0;i<100&&!turnCancelled&&(!painted||lastPaintedSource!==target);i++)await new Promise(resolve=>setTimeout(resolve,20));
          return validateCandidate(target,feedback,hash);
        }});
      if(result.validation){
        validationChecks=result.validation.findings.map(f=>({code:f.code,sourceHash:result.validation.sourceHash}));
        if(!result.validation.passed){turnSucceeded=false;turnRuntimeFailed=true;turnError='Edit check failed: '+result.validation.findings.map(f=>f.code).join(', ');}
      }
    }finally{clearTimeout(deadline);}

  }catch(error){turnError=error.message;phase('Could not start');log(error.message);}
  finally{
    if(noChange){await finishReceipt('unchanged');localStorage.removeItem(storageKey+'-inflight');activeAttempt=null;lastAttempt={...lastAttempt,status:'unchanged'};end();phase('Already there');benchmark('localEditUnchanged');return;}
    if(!turnSucceeded&&!turnRuntimeFailed&&!turnCancelled&&turnStarter&&starterPainted){
      // A failed refinement must not erase the useful, verified first drawing.
      source=turnStarter;vfs.mount(file,source);
      if(previewSource!==source)render(source);
      for(let i=0;i<100&&!painted;i++)await new Promise(resolve=>setTimeout(resolve,20));
      turnSucceeded=painted&&lastPaintedSource===source;turnRuntimeFailed=false;
      if(turnSucceeded){benchmark('refinementFailed',{message:turnError||'Refinement unavailable'});log('Refinement failed; kept the starter');server?.close();server=null;}
    }
    // All generation paths, including local edits and starter recovery, inspect
    // the exact candidate's pixels before adding a saved version.
    if(turnSucceeded&&!turnCancelled&&!turnRuntimeFailed){
      try {
        const verdict=await checkVisualResult();
        if(!verdict.passed)throw Error('Visual check failed: '+verdict.findings.join('; '));
      } catch(error) {turnSucceeded=false;turnError=error.message;phase('Could not verify picture');log(turnError);}
    }
    if(turnSucceeded&&!turnCancelled&&!turnRuntimeFailed&&painted&&lastPaintedSource===source){
      try {const version=versions.commit({source,request:turnRequest,layers:checkpoints,parent:turnParent,requestID:activeAttempt?.id});saved();phase(`v${version.id} · Ready to play`);log(`Saved v${version.id} · ${checkpoints} layers`);const drawing=inputData(turnRequest)?.drawing;if(drawing)post({action:'drawingCommitted',drawingID:drawing.id,revision:drawing.revision});benchmark('versionCommitted',{version:version.id,layers:checkpoints});}
      catch(error){turnSucceeded=false;turnError=error.message;phase('Could not save version');log(error.message);benchmark('generationFailed',{message:error.message});}
    }else turnSucceeded=false;
    if(!turnSucceeded){source=previous;vfs.mount(file,source);saved();const restored=source||'export function paint({wipe}) {wipe("black");}';if(previewSource!==restored)render(restored);server?.close();server=null;log('Restored previous version');}
    benchmark(turnSucceeded?'generationFinished':'generationFailed',{message:turnSucceeded?'':turnError||'No verified version was committed'});
    lastAttempt={...lastAttempt,status:turnSucceeded?'completed':'failed',error:turnError||(!turnSucceeded?'No verified version was committed':''),runtimeErrors:[...runtimeErrors],finishedAt:new Date().toISOString()};threadUpdate();
    await finishReceipt(turnCancelled?'interrupted':turnSucceeded?'completed':'failed');
    if(activeAttempt){
      if(turnSucceeded||turnCancelled)localStorage.removeItem(storageKey+'-inflight');
      else saveAttempt(localStorage,storageKey,{...activeAttempt,status:'failed'});
      activeAttempt=null;
    }
    end();
  }
}
async function resumeAttempt(manual=false){
  if(busy||!token||!ready||!painted||!versions||(!manual&&!recoveryPending))return;
  recoveryPending=false;
  const attempt=claimAttempt(localStorage,storageKey,versions.value,manual);
  if(!attempt){if(manual)phase('Could not resume: version changed or request unavailable');return;}
  phase('Resuming interrupted edit…');
  await ask(attempt.text,attempt.displayText,null,null,attempt.localText,attempt);
}
window.walkiewareEngineEvent=event=>{
  if(event.kind==='visualCapture'){
    if(pendingCapture?.id===event.captureID)pendingCapture.finish(event.error?Error(event.error):null,event);
    return;
  }
  if(event.kind==='account') {token=event.token;if(token){musicalSocket.resume();thread?.resume();}else{musicalSocket.suspend();thread?.suspend();}window.walkiewareAccountReady=!!token;accountIdentity(token);if(token&&pending)void ask(pending);else void resumeAttempt();}
  if(event.kind==='error'){phase('Sign-in needed');log(event.text);window.walkiewareWorkFinished?.();}
  if(event.kind==='previewReady'){ready=true;log('AC runtime ready');}
  if(event.kind==='previewEvent'){
    if(event.event?.sourceHash!==previewHash||event.event?.requestID!==renderID)return;
    activeReceipt?.observe(event.event);
    if(event.event.kind==='painted'){if(turnStarter&&previewSource===turnStarter&&!starterPainted){starterPainted=true;benchmark('starterPainted');}if(previewSource.trimEnd()!==previous.trimEnd())window.__walkiewareSequenceEvent?.('painted');painted=true;lastPaintedSource=previewSource;if(busy&&activeAttempt&&source===previewSource&&!turnRuntimeFailed){activeAttempt={...activeAttempt,checkpoint:source};try{saveAttempt(localStorage,storageKey,activeAttempt);}catch(error){log('Could not persist checkpoint: '+error.message);}}feedback={...feedback,rendered:true,updatedAt:new Date().toISOString()};if(activeReceipt&&previewSource.trimEnd()!==previous.trimEnd())activeReceipt.painted();log('Checkpoint painted');phase(busy?'Building…':lastAttempt?.status==='failed'?'Could not finish · previous version restored':'Ready to play');if(narrationPending!==null){post({action:'narrationReady',version:narrationPending});narrationPending=null;}void resumeAttempt();}
    if(event.event.kind==='invalidated'){turnRuntimeFailed=true;window.__walkiewareSequenceEvent?.('runtimeError',{message:'Preview invalidated'});painted=false;feedback={...feedback,rendered:false,logs:[...(feedback?.logs||[]),{level:'error',text:'Preview invalidated'}],updatedAt:new Date().toISOString()};log('Preview failed; inspect activity');phase('Preview error');if(lastPaintedSource && lastPaintedSource!==previewSource){render(lastPaintedSource);log('Restored last painted checkpoint');}}
    if(event.event.kind==='console'&&['error','warn'].includes(event.event.event?.level)){
      const entry={level:event.event.event.level,text:event.event.event.message||'Runtime error'};
      if(/\b(?:Paint|Sim|Boot) failure\b/i.test(entry.text))entry.level='error';
      log(entry.text);runtimeErrors.push(entry.text);if(runtimeErrors.length>20)runtimeErrors.shift();
      feedback={...feedback,rendered:painted,logs:[...(feedback?.logs||[]),entry].slice(-20),updatedAt:new Date().toISOString()};
      if(entry.level==='error'){turnRuntimeFailed=true;turnError=entry.text;window.__walkiewareSequenceEvent?.('runtimeError',{message:entry.text});}
      threadUpdate();
    }
  }
};
$('live-stop').onclick=()=>{turnCancelled=true;pending='';visualController?.abort();server?.interrupt();if(!busy){end();phase('Stopped');}};
window.walkiewareUndo=()=>{try{source=versions.undo().source;}catch(error){log(error.message);return;}previous=source;vfs.mount(file,source);saved();server?.close();server=null;render(source||'export function paint({wipe}) {wipe("black");}');review(false);phase('Undone');};
window.walkiewareAsk=ask;
window.walkiewareAskDrawing=(text,drawing)=>{
  try{return ask(withDrawing(text,drawing),text||(drawing?'Drawing':''),null,null,drawing?'':text);}
  catch(error){phase('Could not read drawing');log(error.message);return Promise.resolve();}
};
let musicalTurn=0;
const musicalSocket=new MusicalInputSocket({token:()=>token,onEvent:benchmark});
document.addEventListener('visibilitychange',()=>{if(document.hidden){musicalSocket.suspend();thread?.suspend();}else{musicalSocket.resume();thread?.resume();}});
const musicalAdvisor=new MusicalInputAdvisor({fetchImpl:musicalSocket.fetch,token:()=>token,onEvent:(event,fields)=>{benchmark(event,fields);if(event==='jevDecision')log('Jev · '+fields.choice);}});
window.walkiewareInputStart=()=>{musicalTurn++;musicalAdvisor.reset();};
window.walkiewareInputCancel=()=>{musicalTurn++;musicalAdvisor.cancel();};
window.walkiewareObserveSound=input=>{musicalPrompt(input);if(wantsSoundEvidence(input.transcript))musicalAdvisor.observe(input);};
window.walkiewareAskSound=async input=>{
 const prompt=withDrawing(musicalPrompt(input),input.drawing);const useSound=!!input.drawing||wantsSoundEvidence(input.transcript);if(!useSound)musicalAdvisor.cancel();phase(useSound?'Interpreting sound…':'Sending…');
 return ask(prompt,`${input.transcript||'Sound'} · ${(input.sound.durationMs/1000).toFixed(1)} seconds`,input.drawing||!useSound||localEdit(source,input.transcript)?null:musicalAdvisor.finish(input),input.drawing?null:instantPiece(input.transcript),input.drawing?'':input.transcript);
};
window.walkiewareIsBusy=()=>busy;
window.walkiewareHasReview=()=>false;
if(!window.__walkiewareSequence&&!window.__walkiewareBenchmark&&!window.__walkiewareLocalSequence&&!window.__walkiewareDisableThread)try{if(initializeBasePiece(localStorage,storageKey))lastAttempt=null;}catch(error){phase('Could not initialize piece');log(error.message);}
try{source=window.__walkiewareSpace&&window.__walkiewareSequenceStart===1?'':window.__walkiewareLocalSequence?'':window.__walkiewareSequence?((window.__walkiewareSequenceStart>1?localStorage.getItem(storageKey):'')||localStorage.getItem('walkieware-benchmark-source')||''):window.__walkiewareBenchmark?'':localStorage.getItem(storageKey)||'';if(source){previous=source;ui.hidden=false;document.body.classList.add('live-mode','live-preview');$('initial').hidden=true;render(source);}}catch{}
try {
  const versionKey=storageKey+'-versions';
  if(window.__walkiewareLocalSequence||window.__walkiewareBenchmark||(window.__walkiewareSequence&&window.__walkiewareSequenceStart===1))localStorage.removeItem(versionKey);
  versions=new PieceVersions(localStorage,versionKey,source);
  if(versions.head.source!==source){source=previous=versions.head.source;saved();if(source)render(source);}
}catch(error){phase('Could not load versions');log(error.message);}
// Older installs retained the words but not the full mixed-audio prompt.
// Recover that spoken request once; new journals retain the complete input.
if(versions&&['working','failed','interrupted'].includes(lastAttempt?.status)){
  if(!readAttempt(localStorage,storageKey)&&lastAttempt.parent===versions.head.id){
    const text=lastAttempt.request.replace(/ · [\d.]+ seconds$/,'');
    if(text&&text!=='Sound')saveAttempt(localStorage,storageKey,{id:crypto.randomUUID(),text,displayText:lastAttempt.request,localText:text,parent:versions.head.id,baseSource:versions.head.source,retries:0,status:lastAttempt.status==='failed'?'failed':'working'});
  }
  const journal=readAttempt(localStorage,storageKey);
  if(journal&&versions.value.versions.some(v=>v.requestID===journal.id)){
    localStorage.removeItem(storageKey+'-inflight');lastAttempt={...lastAttempt,status:'completed',error:''};
  }else if(lastAttempt.status==='working'){lastAttempt={...lastAttempt,status:'interrupted',error:'Interrupted before finishing'};}
}
// Simulator fixture: hold the pending row open with streamed code so the busy
// state can be screenshot without a model or an account. Never leaves DEBUG.
if(typeof window.__walkiewareFixtureBusy==='string'){
  busy=true;ui.hidden=false;document.body.classList.add('live-mode','live-preview');
  lastAttempt={request:window.__walkiewareFixtureBusy,parent:versions?.head.id??0,status:'working',startedAt:new Date().toISOString()};
  outputStream='export function paint({ wipe, ink, circle, screen }) {\n  wipe("#151838");\n  ink("#4653c6");\n  circle(screen.width / 2, screen.height / 2, 40, true);';
  $('live-code').textContent=outputStream;phase('Writing…');$('live-stop').hidden=false;review(false);
}
updateFeed();
if(versions&&!window.__walkiewareSequence&&!window.__walkiewareBenchmark&&!window.__walkiewareDisableThread) {
  const label=codeLabel;label.setAttribute('aria-live','polite');
  thread=new WalkiewareThread({storage:localStorage,key:storageKey,receipts,token:()=>token,ledger:()=>versions.value,
    state:()=>({busy,phase:$('live-phase').textContent,head:versions.head.id,source:versions.head.source,errors:runtimeErrors,attempt:lastAttempt}),
    onStatus:(code,status)=>{label.textContent=code?'/'+code:'';label.title=status;label.dataset.status=status;post({action:'threadStatus',code:code||'',threadID:thread.identity.id,status});nativeSnapshot();},
    onCommand:async command=>{
      if(command.action==='layout'){
        if(typeof command.css!=='string'||new TextEncoder().encode(command.css).length>100000)throw Error('Invalid layout');
        window.walkiewareApplyLayout(command.css);
        return {ok:true,head:versions.head.id};
      }
      await verifyThreadRevision(command,()=>({busy,head:versions.head.id,source:versions.head.source}));
      const before=versions.head.id;
      if(command.action==='ask')await ask(command.text);
      else if(command.action==='undo')window.walkiewareUndo();
      else if(command.action==='edit') {
        busy=true;previous=source;turnRuntimeFailed=false;turnCancelled=false;checkpoints=1;server?.close();server=null;review(false);
        source=command.source;vfs.mount(file,source);phase('Applying remote edit…');render(source);
        try {
          for(let i=0;i<500&&!painted&&!turnRuntimeFailed&&!turnCancelled;i++)await new Promise(r=>setTimeout(r,10));
          await new Promise(r=>setTimeout(r,250));
          if(!painted||lastPaintedSource!==source||turnRuntimeFailed||turnCancelled)throw Error('Remote edit did not paint cleanly');
          versions.commit({source,request:'Remote source edit',layers:1,parent:before});saved();phase('Ready to play');
        }catch(error){source=previous;vfs.mount(file,source);render(source);throw error;}
        finally{end();}
      }
      return {ok:versions.head.id!==before,head:versions.head.id,error:versions.head.id===before?(turnError||'No new version'):''};
    }});
  label.textContent=thread.identity.code?'/'+thread.identity.code:'';
  const newPiece=document.createElement('button');newPiece.textContent='New piece';newPiece.style.cssText='font:24px Comic,Arial;padding:14px';
  newPiece.onclick=window.walkiewareNewPiece=()=>{
    if(busy)return;
    archiveCurrentPiece();
    location.reload();
  };
  // Every piece on this phone: the open one plus the archives "New piece" left.
  // Opening another swaps archives, so the current one is never lost.
  const ARCHIVE='walkieware-archive-',ARCHIVE_SUFFIXES=['-cloud-revision','-cloud-ledger','-receipts'];
  function archiveCurrentPiece(){
    const extras={};for(const suffix of ARCHIVE_SUFFIXES){const v=localStorage.getItem(storageKey+suffix);if(v!==null)extras[suffix]=v;}
    localStorage.setItem(ARCHIVE+thread.identity.id,JSON.stringify({identity:thread.identity,ledger:versions.value,source,extras,archivedAt:new Date().toISOString()}));
    thread.suspend();
    for(const suffix of ['', '-versions','-thread','-cloud-revision','-cloud-ledger','-attempt','-inflight','-receipts'])localStorage.removeItem(storageKey+suffix);
  }
  function pieceSummary(id,identity,ledger,current){
    const made=(ledger?.versions||[]).filter(v=>v.id>0),last=made.at(-1);
    return {id,code:identity?.code||'',utterance:last?utterance(last.request).slice(0,160):'',versions:made.length,updatedAt:last?.createdAt||made[0]?.createdAt||'',current};
  }
  function pieceList(){
    const list=[pieceSummary(thread.identity.id,thread.identity,versions.value,true)];
    for(let i=0;i<localStorage.length;i++){
      const key=localStorage.key(i);if(!key?.startsWith(ARCHIVE))continue;
      try{const saved=JSON.parse(localStorage.getItem(key));if(saved?.identity?.id&&saved.identity.id!==thread.identity.id)list.push(pieceSummary(saved.identity.id,saved.identity,saved.ledger,false));}catch{}
    }
    return list.sort((a,b)=>(b.current-a.current)||(Date.parse(b.updatedAt)||0)-(Date.parse(a.updatedAt)||0)).slice(0,256);
  }
  postPieces=()=>post({action:'pieces',pieces:pieceList()});
  window.walkiewareOpenPiece=id=>{
    if(busy||id===thread.identity.id)return;
    let saved;try{saved=JSON.parse(localStorage.getItem(ARCHIVE+id));}catch{}
    if(!saved?.identity?.id||!saved.ledger){phase('That piece is no longer on this phone');postPieces();return;}
    archiveCurrentPiece();
    localStorage.setItem(storageKey,saved.source||'');
    localStorage.setItem(storageKey+'-versions',JSON.stringify(saved.ledger));
    localStorage.setItem(storageKey+'-thread',JSON.stringify(saved.identity));
    for(const [suffix,value] of Object.entries(saved.extras||{}))localStorage.setItem(storageKey+suffix,value);
    localStorage.removeItem(ARCHIVE+id);
    location.reload();
  };
  postPieces();
  $('info').append(newPiece);
}
post({action:'account'});
// Opt-in capture smoke test: replay the current piece without generating,
// reviewing, checking out or committing anything. Native debug code saves proof.
if(window.__whistlegraphVisualCaptureTest)void(async()=>{
  for(let i=0;i<300&&(!ready||!painted||busy);i++)await new Promise(r=>setTimeout(r,100));
  if(!ready||!painted||busy)return;
  try {await captureVisual(previewHash,renderID,new AbortController().signal);}
  catch(error){log(error.message);}
})();
setTimeout(()=>{if(!ready){ui.hidden=false;phase('Preview still loading');log('AC runtime has not reported ready. Check your connection.');}},20000);

if(window.__walkiewareSequence && !window.__walkiewareLocalSequence && window.__walkiewareReviewVersion===undefined) import("./sequence-benchmark.mjs").then(({runSequence})=>runSequence({ask,ready:()=>ready,source:()=>source,painted:()=>painted&&lastPaintedSource===source,interrupt:()=>server?.interrupt(),model:window.__walkiewareModel||DEFAULT_MODEL}));

if(window.__walkiewareSequence && Number.isInteger(window.__walkiewareReviewVersion)) void (async()=>{
  const revision=versions.value.versions.find(v=>v.id===window.__walkiewareReviewVersion);
  if(!revision){phase('Review version unavailable');return;}
  source=revision.source;render(source);
  for(let i=0;i<300&&!(ready&&painted&&lastPaintedSource===source);i++)await new Promise(r=>setTimeout(r,100));
  if(!painted){phase('Review preview unavailable');return;}
  phase(`Reviewing v${revision.id}`);
  const r=frame.getBoundingClientRect();
  post({action:'sequenceCapture',report:{index:revision.id,total:1,source,evidenceOnly:true,requiresMotion:true,checks:{committedSourcePainted:true},scope:'Exact saved version replayed on physical phone. Sequential native snapshots; no model edit, version commit or visual verdict.'},rect:{x:r.x,y:r.y,width:r.width,height:r.height}});
})();

if(window.__walkiewareLocalSequence)import('./local-sequence.mjs').then(({runLocalSequence})=>runLocalSequence({ask,source:()=>source,head:()=>versions.value.head,count:()=>versions.value.versions.length,ready:()=>ready,painted:()=>painted&&lastPaintedSource.trimEnd()===(source||'export function paint({wipe}) {wipe("black");}').trimEnd(),undo:()=>window.walkiewareUndo()}));
