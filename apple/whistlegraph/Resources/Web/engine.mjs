import {AccountConnection} from './account-connection.mjs';
import {fetchPersonalAccess,hasPersonalAccess} from './personal-access.mjs';
import {createAIConsentGate} from './ai-consent.mjs';
import {SourceEditor} from './source-editor.mjs';
import {inferenceError} from './inference-error.mjs';
import {withDrawing,inputData,drawingImage,drawingContent} from './drawing-input.mjs';
import {checkedPrompt} from './prompt-limit.mjs';
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
import {WhistlegraphThread,verifyThreadRevision} from '/easel/src/whistlegraph-thread.mjs';
import {instantPiece} from './instant-piece.mjs';
import {MusicalInputSocket} from '/easel/src/musical-input-socket.mjs';
import {MusicalInputAdvisor} from '/easel/src/musical-input-advisor.mjs';
import {musicalPrompt} from './musical-input.mjs';
import {PieceVersions} from './piece-versions.mjs';
import {wareControls,WARE_INSTRUCTIONS,requestedWare} from './wares.mjs';
const wares=wareControls('piece');
// The same inference/tool loop as Aesel, with a piece-first streaming renderer.
import {AcServer} from '/easel/src/ac-server.mjs';
import {RelayPieceServer} from '/easel/src/relay-piece-server.mjs';
import {DEFAULT_MODEL,generationProfile,modelChoices,MODEL_LABELS,GENERATION_INSTRUCTIONS} from './generation-policy.mjs';
import {runnablePrefix,partialString,streamedEdits,streamedCode} from './stream-preview.mjs';
import * as vfs from '/easel/phone/shim/fs.mjs';

const post = body => window.webkit.messageHandlers.whistlegraph.postMessage({id:'engine',...body});
let consentState = window.__whistlegraphAIConsent || {};
const aiConsent = createAIConsentGate({fetch: globalThis.fetch.bind(globalThis), onRequired: () => post({action:'aiConsent'})});
globalThis.fetch = aiConsent.fetch;
function syncAIConsent() {
  const allowed = consentState.creation === true && !!accountHandle && consentState.handle === accountHandle && accountToken === token;
  aiConsent.setAllowed(allowed);
  if (!allowed) {
    turnCancelled = true; visualController?.abort(); server?.interrupt();
    musicalAdvisor.cancel(); musicalSocket.suspend();
  } else { musicalSocket.resume(); }
}

const benchmark=(event,fields={})=>{post({action:'benchmark',event,fields});window.__whistlegraphSequenceEvent?.(event,fields);};
const file = '/piece/whistlegraph.mjs';
const storageKey=window.__whistlegraphFixture?'whistlegraph-fixture-source':window.__whistlegraphSpace?'whistlegraph-space-source':window.__whistlegraphLocalSequence?'whistlegraph-local-source':window.__whistlegraphSequence?'whistlegraph-sequence-source':window.__whistlegraphBenchmark?'whistlegraph-benchmark-source':'whistlegraph-source';
const receipts=new ReceiptJournal(localStorage,storageKey);
let activeReceipt=null,renderID=0,previewHash=null,validationChecks=[];
let visualController=null,pendingCapture=null;
let turnStarter='',starterPainted=false,turnCancelled=false;
let thread=null,threadTimer=null;
const runtimeErrors=[];
let lastAttempt=null,activeAttempt=null,recoveryPending=true,presentedVersion=null,narrationPending=null;
try{lastAttempt=JSON.parse(localStorage.getItem(storageKey+'-attempt')||'null');}catch{}
let versions=null,turnSucceeded=false,turnRequest='',turnParent=null,turnRuntimeFailed=false,turnError='';
let turnNotes=[]; // The reviewer's findings on a saved version: advice for the next request, never a veto.
let token = '', busy = false, server, source = '', previous = '', pending = '', checkpoints = 0;
let feedback = null, lastPaintedSource = '', previewSource = '', provisional = '', compileTimer = null;
let streamTool='',streamBase='',streamRevision='',rejectedPreview='';
let outputStream='',reasoningStream='',code = '', codeItem = '', firstDelta = false, started = 0, timer, ready = false, painted = false;
const events = [];
const signIn=document.createElement('button');signIn.id='connect-ac';signIn.textContent='Account';signIn.onclick=()=>post({action:'signIn'});const identity=document.createElement('div');identity.id='whistlegraph-identity';const codeLabel=document.createElement('span');codeLabel.id='whistlegraph-thread';identity.append(signIn,codeLabel);document.body.append(identity);
let accountToken='',accountHandle='',accountPalette=[],accountVerification=Promise.resolve();
let personalAccess=null,turnPersonalAccess=false;
let turnHandle='',turnModel='',activeModel='',braincells=null,braincellsError='',creditsRequest=0;
function selectedModel(handle=accountHandle){try{return localStorage.getItem('whistlegraph-model-'+handle)||'';}catch{return '';}}
function profile(repair=false){return generationProfile(busy?turnHandle:accountHandle,{repair,image:busy&&!!inputData(turnRequest)?.drawing,personalAccess:busy?turnPersonalAccess:hasPersonalAccess(personalAccess),model:busy?turnModel:selectedModel()});}
async function refreshBraincells(){
  benchmark('braincellsRequest');
  const currentToken=token,request=++creditsRequest;
  if(!currentToken){braincells=null;braincellsError='Sign in to view braincells';nativeSnapshot();return;}
  braincellsError='';nativeSnapshot();
  try{
    const response=await fetch('https://aesthetic.computer/api/easel-credits',{headers:{Authorization:'Bearer '+currentToken},signal:AbortSignal.timeout(8000)});
    benchmark('braincellsHeaders',{status:response.status});
    if(!response.ok)throw Error('Braincells unavailable');
    const value=await response.json();
    if(![value.remaining,value.used,value.limit,value.purchased].every(v=>Number.isFinite(v)&&v>=0))throw Error('Braincells unavailable');
    if(token!==currentToken||request!==creditsRequest)return;
    braincells=value;braincellsError='';benchmark('braincellsLoaded');
  }catch{if(token!==currentToken||request!==creditsRequest)return;braincells=null;braincellsError='Could not load braincells. Check your connection and tap Refresh.';benchmark('braincellsFailed');}
  nativeSnapshot();
}
function inferenceSnapshot(){
  const model=activeModel||profile().model,receipt=activeReceipt?.value||receipts.rows.at(-1)?.receipt;
  return {model,label:MODEL_LABELS[model]||model,provider:profile().personalRelay?(model.startsWith('openai/')?'Personal Codex':'Personal Claude'):'OpenRouter',selection:profile().model,models:modelChoices(accountHandle,{personalAccess:hasPersonalAccess(personalAccess)}),braincells,braincellsError,threadCost:receipts.cost.snapshot(),
    usage:receipt?{inputTokens:receipt.rounds.reduce((n,r)=>n+(r.usage?.inputTokens||0),0),outputTokens:receipt.rounds.reduce((n,r)=>n+(r.usage?.outputTokens||0),0),rounds:receipt.rounds.length,repairs:receipt.repairs,status:receipt.status,cost:{usd:receipt.rounds.reduce((n,r)=>n+(r.usage?.costUSD||0),0),partial:receipt.rounds.some(r=>r.usage?.costUSD==null && (r.httpStatus==null || r.httpStatus<400)),estimated:receipt.rounds.some(r=>r.usage?.estimated)}}:null};
}
function paintHandle(handle,colors=handleCharacterColors('@'+handle)){
  accountPalette=colors;
  signIn.replaceChildren(...Array.from('@'+handle,(character,index)=>{const span=document.createElement('span');span.textContent=character;span.style.color='rgb('+colors[index].join(',')+')';return span;}));nativeSnapshot();
}
const accountConnection = new AccountConnection({verify: verifyAccount, changed: state => post({action:'accountState', ...state})});
function accountIdentity(value, notice='', force=false){
  if(value&&value===accountToken&&accountHandle&&!force)return;
  accountToken=value;accountHandle='';accountPalette=[];personalAccess=null;syncAIConsent();braincells=null;braincellsError='';signIn.textContent=value?'…':'Sign in';
  nativeSnapshot();
  accountVerification=accountConnection.connect(value, notice).then(account=>{
    if(!account||accountToken!==value)return;
    accountHandle=account.handle;syncAIConsent();
    if(!accountHandle){signIn.textContent='Set handle';nativeSnapshot();return;}
    paintHandle(accountHandle);void refreshBraincells();
    const handle=accountHandle,revision=accountConnection.revision;
    void fetchPersonalAccess(value).then(access=>{if(accountToken===value&&accountConnection.revision===revision){personalAccess=access;nativeSnapshot();}});
    void fetchHandleColors('@'+handle).then(colors=>{if(accountToken===value&&accountConnection.revision===revision)paintHandle(handle,colors);}).catch(()=>{});
  });
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
  if(busy||window.whistlegraphRecording?.())return;
  source=versions.checkout(id).source;previous=source;lastAttempt=null;
  server?.close();server=null;vfs.mount(file,source);saved();
  render(source||'export function paint({wipe}) {wipe("black");}');review(false);phase('');
}
setInterval(()=>document.querySelectorAll('#version-feed time').forEach(t=>t.textContent=relativeTime(t.dateTime)),15000);
let postPieces=()=>{};
let nativeTimer=null,nativeLedger=null,nativeRevisions=[],nativeLast='',captionSource=null,caption='';
function nativeSnapshot(){
  if(!window.__whistlegraphNativeShell||nativeTimer)return;
  nativeTimer=setTimeout(()=>{
    nativeTimer=null;
    const historyChanged=nativeLedger!==versions?.value;
    if(historyChanged){nativeLedger=versions?.value;nativeRevisions=(versions?.value.versions||[]).map(v=>{const sound=musicalData(v.request)?.sound;return {id:v.id,parent:v.parent,hasDrawing:!!musicalData(v.request)?.drawing,utterance:(v.id===0&&!v.request?'':utterance(v.request)).slice(0,1000),createdAt:v.createdAt,recordingID:sound?.recordingID||null,words:(musicalData(v.request)?.words||[]).slice(0,256),sound:sound?{durationMs:sound.durationMs,frames:sound.frames.filter((_,i)=>i%Math.max(1,Math.ceil(sound.frames.length/64))===0)}:null};});}
    const displayVersion=presentedVersion===null?versions?.head:versions?.value.versions.find(v=>v.id===presentedVersion);
    if(captionSource!==displayVersion?.source){captionSource=displayVersion?.source;caption=pieceCaption(captionSource||'');}
    const phaseText=$('live-phase').textContent;
    const snapshot={ware:'piece',inference:inferenceSnapshot(),output:outputStream,caption,code:thread?.identity.code||'',handle:accountHandle,colors:accountPalette,head:displayVersion?.id||0,hasPiece:!!source.trim(),hasPreview:!!source.trim()||!!provisional.trim(),busy:busy||!!remoteJob,phase:phaseText,error:!busy&&/error|unavailable|could not|sign.in|loading|no piece/i.test(phaseText)?phaseText:'',attempt:lastAttempt?{request:lastAttempt.request.slice(0,1000),status:lastAttempt.status,error:lastAttempt.error||''}:null,draft:(d=>d?{request:d.request.slice(0,1000),error:d.error||'',createdAt:d.createdAt||'',characters:d.source.length}:null)(readDraft())};
    const serialized=JSON.stringify(snapshot);if(serialized===nativeLast&&!historyChanged)return;nativeLast=serialized;
    if(historyChanged)snapshot.versions=nativeRevisions;
    post({action:'snapshot',snapshot});
    if(historyChanged)postPieces();
  },80);
}
window.whistlegraphNativeCommand=command=>{
  if(command.action==='setWare'&&!busy)window.whistlegraphSelectWare?.(command.ware);
  if(['ask','retry'].includes(command.action)) {
    if(busy)return {accepted:false,reason:'busy'};
    if(!versions||(command.action==='retry'&&!ready))return {accepted:false,reason:'notReady'};
    if(!accountHandle||accountToken!==token)return {accepted:false,reason:'authentication'};
    if(!aiConsent.allowed)return {accepted:false,reason:'permission'};
    if(command.action==='ask'&&(typeof command.text!=='string'||(!command.text.trim()&&!command.drawing)))return {accepted:false,reason:'emptyInput'};
    if(command.action==='ask'&&Array.from(new Intl.Segmenter(undefined,{granularity:'grapheme'}).segment(command.text)).length>96)return {accepted:false,reason:'inputTooLong'};
  }
  if(command.action==='refreshBraincells')void refreshBraincells();
  if(command.action==='setModel'&&!busy&&accountHandle&&modelChoices(accountHandle,{personalAccess:hasPersonalAccess(personalAccess)}).some(m=>m.id===command.text)){
    try{localStorage.setItem('whistlegraph-model-'+accountHandle,command.text);}catch{return;}
    server?.close();server=null;activeModel='';nativeSnapshot();
  }
  if(command.action==='ask'&&typeof command.text==='string'&&(command.text.trim()||command.drawing)&&Array.from(new Intl.Segmenter(undefined,{granularity:'grapheme'}).segment(command.text)).length<=96&&!busy)void window.whistlegraphAskDrawing(command.text.trim(),command.drawing);
  if(command.action==='checkout'&&!busy){presentedVersion=null;narrationPending=command.version;jumpVersion(command.version);}
  if(command.action==='presentVersion'){
    const version=versions?.value.versions.find(v=>v.id===command.version);
    if(version)post({action:'presentation',version:version.id,source:version.source||'export function paint({wipe}){wipe("black");}'});
  }
  if(command.action==='endPresentation'){presentedVersion=null;narrationPending=null;}
  if(command.action==='keepDraft'&&!busy)void keepDraft();
  if(command.action==='discardDraft'){localStorage.removeItem(storageKey+'-draft');log('Draft discarded');nativeSnapshot();}
  if(command.action==='newPiece'&&!busy){const result=window.whistlegraphNewPiece?.();if(result?.accepted===false)return result;}
  if(command.action==='openPiece'&&!busy&&typeof command.piece==='string'){const result=window.whistlegraphOpenPiece?.(command.piece);if(result?.accepted===false)return result;}
  if(command.action==='deletePiece'&&typeof command.piece==='string'){const result=window.whistlegraphDeletePiece?.(command.piece);if(result?.accepted===false)return result;}
  if(command.action==='retry'&&!busy)void resumeAttempt(true);
  if(command.action==='stop')$('live-stop').click();
  if(command.action==='signIn')post({action:'signIn'});
  return {accepted:true};
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

// A phone loading the piece in the production frame over cellular can take
// seconds to paint. Two seconds turned late paints into unverified-render
// rollbacks (wgZuhus, 2026-10-08), so wait longer but stop on a runtime error.
async function waitForPaint(target, stopped, ms=20000) {
  const until=Date.now()+ms;
  while(Date.now()<until&&!stopped()&&!turnRuntimeFailed&&(!painted||lastPaintedSource!==target))await new Promise(resolve=>setTimeout(resolve,50));
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
  // Allow two personal reviews plus a repair; each review has its own deadline.
  let timedOut=false;
  const deadline=setTimeout(()=>{timedOut=true;visualController?.abort();server?.interrupt();},profile().personalRelay?900000:90000);
  const task=contextualRequest(versions.value,inferenceRequest(turnRequest));
  const drawing=inputData(turnRequest)?.drawing;
  const chalk=drawing?drawingImage(drawing):null;
  try {
    return await reviewWithRepair({cancelled:()=>turnCancelled||signal.aborted,
      inspect:async()=>{
        phase('Checking picture…');
        const target=source,hash=await hashSource(target),id=renderID;
        await waitForPaint(target,()=>signal.aborted);
        signal.throwIfAborted();
        const runtime=validateCandidate(target,feedback,hash);
        if(!runtime.passed)throw Error('Visual check needs a working current preview: '+runtime.findings.map(f=>f.code).join(', '));
        const evidence=await captureVisual(hash,id,signal);
        if(source!==target||renderID!==id)throw Error('Preview changed before visual review');
        const round=activeReceipt?.request();
        const reviewUsage={};
        let verdict;
        try{
          verdict=await reviewVisualResult({evidence,sourceHash:hash,renderID:id,source:target,
            request:inferenceRequest(turnRequest),history:selectedBranch(versions.value),drawing:chalk,
            model:window.__whistlegraphModel||profile().model,token,signal,personalRelay:profile().personalRelay,
            onHeaders:response=>{if(round)activeReceipt?.headers(round,response);},
            onEvent:e=>{
              if(e.message?.model||e.model)activeReceipt?.notify('model/reported',{reported:e.message?.model||e.model});
              if(e.usage||e.message?.usage){Object.assign(reviewUsage,e.usage||e.message?.usage);activeReceipt?.notify('turn/usage',{usage:reviewUsage});}
            }});
        }catch(error){
          // The reviewer being busy (429), down (5xx) or unreachable is not a
          // judgment on the picture. The candidate painted; keep it, marked
          // unreviewed in the receipt, rather than throw the work away
          // (jeffrey lost a valid "make fia larger" to a 429, 2026-10-09).
          if(signal.aborted||turnCancelled||!/unavailable \(HTTP (429|5\d\d)\)|fetch failed|network|timed out|aborted/i.test(String(error?.message||error)))throw error;
          if(source!==target||renderID!==id||!painted||turnRuntimeFailed)throw Error('Preview changed during visual review');
          validationChecks.push({code:'visual-unreviewed',sourceHash:hash});
          log('Visual review unavailable; keeping the painted result unreviewed · '+(error?.message||error));
          return {passed:true,unreviewed:true,observations:'Review unavailable: '+(error?.message||error),findings:[]};
        }
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
  } catch(error) {
    if(timedOut)throw Error('Visual check timed out. Your request and generated checkpoint remain saved for retry.');
    throw error;
  } finally {clearTimeout(deadline);visualController=null;}
}
function compileStream() {
  if(compileTimer)return;
  compileTimer=setTimeout(()=>{
    compileTimer=null;
    if(!busy)return;
    const candidate=streamTool==='edit_piece'?streamedEdits(code,streamBase,streamRevision):runnablePrefix(partialString(code).value);
    if(!candidate||candidate===previewSource||candidate===rejectedPreview)return;
    provisional=candidate;
    document.body.classList.add('live-preview');
    benchmark('firstIncrementalCompile');log('Running streamed code');
    render(candidate);
  },160);
}
function saved() { try { localStorage.setItem(storageKey,source); } catch {} updateFeed(); }
function review(show) { $('speak').disabled=busy; $('speak-label').textContent=busy?'Working…':'Hold to talk'; }
function end() { clearTimeout(compileTimer);compileTimer=null; if(provisional && previewSource!==source){render(source||'export function paint({wipe}) {wipe("black");}');provisional='';} busy=false;activeModel='';void refreshBraincells(); updateFeed(); clearInterval(timer); $('live-stop').hidden=true; review(source!==previous); window.whistlegraphWorkFinished?.(); if(pendingAdopt){const next=pendingAdopt;pendingAdopt=null;adoptLedger(next);} }
let pendingAdopt=null;
function adoptLedger(ledger){
  try{
    if(!versions||ledger?.format!==1||!Array.isArray(ledger.versions))return;
    versions.persist(ledger);
    source=previous=versions.head.source;vfs.mount(file,source);saved();
    render(source||'export function paint({wipe}) {wipe("black");}');
    lastAttempt={request:versions.head.request||'',status:'completed',error:'',startedAt:versions.head.createdAt||''};
    localStorage.removeItem(storageKey+'-inflight');localStorage.removeItem(storageKey+'-attempt');localStorage.removeItem(storageKey+'-draft');
    phase(`v${versions.head.id} · Made while you were away`);log('Adopted v'+versions.head.id+' from the knot');
    updateFeed();threadUpdate();nativeSnapshot();
  }catch(error){log('Could not adopt the server version: '+error.message);}
}
function delta(text) {
  outputStream=(outputStream+text).slice(-6000);
  if (!firstDelta) {firstDelta=true; log('First model output');benchmark('firstModelOutput');}
  phase('Writing…');
}
vfs.setWriteHandler((path,value)=>{
  if(path!==file)return;
  clearTimeout(compileTimer);compileTimer=null;const wasProvisional=!!provisional;provisional='';rejectedPreview='';
  source=value;checkpoints++;benchmark('firstCheckpoint');
  turnRuntimeFailed=false;
  document.body.classList.add('live-preview'); $('initial').hidden=true;
  $('play-deck').hidden=true;
  $('live-code').textContent=value;
  phase(ready?'Evaluating…':'Loading preview…'); log(`Checkpoint ${checkpoints} · valid JavaScript`);
  if(wasProvisional||value!==previewSource)render(value);
});
const guides = vfs.preload(['pieces.md','screen.md','hand.md','kidlisp.md','api.json'].map(name=>'/easel/context/'+name));
function makeServer({repair=false}={}){
  const settings=profile(repair);activeModel=(repair?window.__whistlegraphRepairModel:window.__whistlegraphModel)||settings.model;nativeSnapshot();
  let relayRound;
  const Engine=settings.personalRelay?RelayPieceServer:AcServer;
  const value=new Engine({relayStorage:localStorage,relayKey:storageKey+'-personal-'+activeAttempt?.id,
    onRelayRequest:()=>{relayRound=activeReceipt?.request();benchmark('requestDispatched');},
    onRelayHeaders:response=>{if(relayRound)activeReceipt?.headers(relayRound,response);},cwd:'/piece',piece:{file,checkpoint:async()=>{const target=source.trimEnd();for(let i=0;i<100;i++){if(painted&&lastPaintedSource.trimEnd()===target)return;await new Promise(resolve=>setTimeout(resolve,20));}}},frameCapture:false,layeredEdits:true,token:()=>token,model:activeModel,
    fetch:settings.personalRelay?globalThis.fetch.bind(globalThis):async(url,options)=>{
      benchmark('requestDispatched');const recorder=activeReceipt,round=recorder?.request();
      const body=JSON.parse(options.body);body.max_tokens=settings.maxTokens;options={...options,body:JSON.stringify(body)};
      const response=await globalThis.fetch(url,options);if(round)recorder.headers(round,response);
      benchmark('inferenceHeaders',{status:response.status});return response;
    },preview:true,rounds:settings.rounds,outputContinuations:settings.outputContinuations,reasoning:settings.reasoning,thinking:settings.thinking,
    controls:wares,developerInstructions:GENERATION_INSTRUCTIONS+'\n'+WARE_INSTRUCTIONS});
  value.runtimeFeedback=()=>feedback;
  value.on('notification',({method,params})=>{
    activeReceipt?.notify(method,params);
    if(method==='turn/progress' && !firstDelta) phase(params.phase==='connecting'?'Connecting…':'Waiting for model…');
    if(method==='item/modelCode/delta'){
      delta('');if(codeItem!==params.itemId){benchmark('layerStarted',{tool:params.tool||'write_piece'});code='';codeItem=params.itemId;streamTool=params.tool||'write_piece';streamBase=source;streamRevision=value.revisionForSource(source);rejectedPreview='';}
      code+=params.delta;const visibleCode=streamedCode(code,params.tool);$('live-code').textContent=visibleCode;outputStream=visibleCode.slice(-6000);nativeSnapshot();compileStream();if(params.tool==='edit_piece')phase('Editing…');
      $('live-details').open=true;
    }
    if(method==='item/agentMessage/delta'){delta(params.delta);$('live-request').textContent+=params.delta;}
    // Reasoning streams before any code. Let it fly by in the ticker, dimmed by
    // the native side, so the wait reads as work rather than silence.
    if(method==='item/reasoning/delta'){reasoningStream=(reasoningStream+params.delta).slice(-6000);if(!firstDelta){benchmark('firstReasoning');log('Model thinking');}if(!code)outputStream=reasoningStream;phase('Thinking…');nativeSnapshot();}
    if(method==='item/started')benchmark('toolStarted',{tool:params.item?.tool||params.item?.type});
    if(method==='item/completed' && params.item?.status?.startsWith('failed')) {log(params.item.status);benchmark('toolFailed',{message:params.item.status});}
    if(method==='turn/usage'){log('Usage · '+(params.usage.output_tokens??0)+' output tokens');nativeSnapshot();}
    if(method==='turn/completed'){turnSucceeded=!params.turn.error&&params.turn.status==='completed';turnError=inferenceError(params.turn.error)||(params.turn.status==='interrupted'?'Stopped':'');
      if(params.turn.error){phase('Could not finish');log(turnError);}
      else if(params.turn.status==='interrupted'){phase('Stopped');log('Stopped by you');}
      else {phase(painted?'Checking picture…':source?'Waiting for preview…':'No piece written');log('Model finished');}
    }
  });return value;
}
async function ask(text,displayText=text,advice=null,starter=null,localText=text,recovered=null){
  if(busy)return;
  const target=requestedWare(localText);
  if(target){window.whistlegraphWorkFinished?.();window.whistlegraphSelectWare?.(target);return;}
  wares.clear();
  if(!versions){phase('Version history unavailable');return;}
  benchmark('generationEntered');
  document.body.classList.add('live-mode');
  pending=text;ui.hidden=false;$('live-request').textContent=displayText;started=performance.now();events.length=0;
  $('live-stop').hidden=false;$('live-details').open=false;
  if(!token){phase('Sign in to make software');log('AI permissions are managed in AI & privacy.');post({action:'signIn'});return;}
  if(!aiConsent.allowed){pending='';phase('Allow AI creation in AI & privacy');post({action:'aiConsent'});return;}
  try {
    activeAttempt=saveAttempt(localStorage,storageKey,recovered||{id:crypto.randomUUID(),text,displayText,localText,parent:versions.head.id,baseSource:versions.head.source,retries:0,status:'working'});
  } catch { phase('Could not save request for recovery');return; }
  recoveryPending=false;
  pending='';busy=true;outputStream='';reasoningStream='';previous=source;turnStarter='';starterPainted=false;turnCancelled=false;turnSucceeded=false;turnRuntimeFailed=false;turnError='';turnRequest=text;turnParent=versions?.head.id;review(false);firstDelta=false;checkpoints=0;code='';codeItem='';
  lastAttempt={request:displayText.slice(0,20000),parent:turnParent,status:'working',startedAt:new Date().toISOString()};
  document.body.classList.add('live-mode');phase('Sending…');log('Submitted');
  timer=setInterval(()=>$('live-time').textContent=((performance.now()-started)/1000).toFixed(1)+'s',100);
  // Turns run off the phone (TURNS.md slice 5): the request goes to the knot
  // and the piece follows the ledger. A recovered checkpoint still finishes here.
  if(remoteTurns()&&!recovered?.checkpoint&&thread?.identity.code){await submitRemoteTurn({text,displayText,drawing:inputData(text)?.drawing||null});return;}
  let noChange=false;
  validationChecks=[];runtimeErrors.length=0;turnNotes=[];
  try{
    await accountVerification;if(turnCancelled)throw Error('Stopped');turnHandle=accountToken===token?accountHandle:'';turnPersonalAccess=!!turnHandle&&hasPersonalAccess(personalAccess);turnModel=selectedModel(turnHandle);
    activeReceipt=new AttemptReceipt({requestID:activeAttempt.id,parent:turnParent,parentHash:await hashSource(previous),path:'compiled',model:window.__whistlegraphModel||profile().model,journal:receipts});
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
    const viewport=frame.getBoundingClientRect();
    const prompt=compileEditContract({request:text,...selectedBranch(versions.value),source})+
      `\nCurrent preview: ${Math.round(viewport.width)} × ${Math.round(viewport.height)} CSS points. Compose for this shape using screen.width and screen.height; keep subjects within the canvas and remain responsive when it resizes.`;
    // No wall-clock deadline: every turn is bounded by its profile's rounds,
    // continuations and output tokens, and by Stop. A fixed 75 s cut off slow
    // phones and long Opus turns that were still making progress.
    const deadline=null;
    try{
      const result=await runEditExperiment({prompt,cancelled:()=>turnCancelled,
        onRepair:()=>{activeReceipt.value.repairs=1;activeReceipt.save();phase('Repairing…');},
        generate:async(task,repair)=>{server?.close();server=makeServer({repair});turnSucceeded=false;await server.startTurn(drawingContent(task,chalkImage));return turnSucceeded;},
        inspect:async()=>{
          const target=source,hash=await hashSource(target);
          await waitForPaint(target,()=>turnCancelled);
          return validateCandidate(target,feedback,hash);
        }});
      if(result.validation){
        validationChecks=result.validation.findings.map(f=>({code:f.code,sourceHash:result.validation.sourceHash}));
        if(!result.validation.passed){turnSucceeded=false;turnRuntimeFailed=true;turnError='Edit check failed: '+result.validation.findings.map(f=>f.code).join(', ');}
      }
    }finally{clearTimeout(deadline);}

  }catch(error){turnError=inferenceError(error);phase('Could not start');log(turnError);}
  finally{
    if(wares.pending){
      await finishReceipt(turnSucceeded?'success':'failed');
      const target=wares.pending;wares.clear();
      source=previous;vfs.mount(file,source);saved();
      localStorage.removeItem(storageKey+'-inflight');activeAttempt=null;lastAttempt=null;
      localStorage.removeItem(storageKey+'-attempt');
      render(source||'export function paint({wipe}) {wipe("black");}');end();
      if(!turnCancelled&&turnSucceeded)window.whistlegraphSelectWare?.(target);
      return;
    }
    if(noChange){await finishReceipt('unchanged');localStorage.removeItem(storageKey+'-inflight');activeAttempt=null;lastAttempt={...lastAttempt,status:'unchanged'};end();phase('Already there');benchmark('localEditUnchanged');return;}
    if(!turnSucceeded&&!turnRuntimeFailed&&!turnCancelled&&turnStarter&&starterPainted){
      // A failed refinement must not erase the useful, verified first drawing.
      source=turnStarter;vfs.mount(file,source);
      if(previewSource!==source)render(source);
      for(let i=0;i<100&&!painted;i++)await new Promise(resolve=>setTimeout(resolve,20));
      turnSucceeded=painted&&lastPaintedSource===source;turnRuntimeFailed=false;
      if(turnSucceeded){benchmark('refinementFailed',{message:turnError||'Refinement unavailable'});log('Refinement failed; kept the starter');server?.close();server=null;}
    }
    if(!turnSucceeded&&!turnCancelled&&!turnRuntimeFailed&&source!==previous&&painted&&lastPaintedSource===source&&!sourceChecks(source).length){
      // Out of rounds, or the model stopped with a working picture on screen: that is the work.
      turnSucceeded=true;turnNotes=[...turnNotes,'The model stopped early ('+(turnError||'did not finish')+'); kept its last working picture.'];log('Kept the painted candidate after an early stop');
    }
    if(!turnSucceeded&&!turnCancelled&&activeAttempt?.checkpoint&&activeAttempt.checkpoint!==previous&&activeAttempt.checkpoint!==source){
      // A repair broke the picture (the last candidate never painted). The
      // last candidate that did paint is the work worth keeping, with a note;
      // throwing it away cost a whole try on 2026-10-10 ('unverified-render').
      source=activeAttempt.checkpoint;vfs.mount(file,source);turnRuntimeFailed=false;
      if(previewSource!==source)render(source);
      for(let i=0;i<200&&!(painted&&lastPaintedSource===source);i++)await new Promise(resolve=>setTimeout(resolve,20));
      if(painted&&lastPaintedSource===source){
        turnSucceeded=true;turnNotes=[...turnNotes,'The last repair did not paint; kept the last picture that did.'];
        log('Repair did not paint; kept the last painted checkpoint');server?.close();server=null;
      }
    }
    // All generation paths, including local edits and starter recovery, inspect
    // the exact candidate's pixels before adding a saved version.
    if(turnSucceeded&&!turnCancelled&&!turnRuntimeFailed){
      try {
        const verdict=await checkVisualResult();
        if(!verdict.passed){
          // The picture painted and ran. After one repair, the reviewer's
          // remaining findings are notes on the saved version, not a reason to
          // throw the work away (a lettering nitpick discarded a valid chalk
          // piece twice on 2026-10-09).
          turnNotes=verdict.findings.slice(0,4);
          log('Saved with notes · '+turnNotes.join('; '));
        }
      } catch(error) {
        // A review that could not happen — timed out, capture unavailable,
        // preview moved — is not a verdict on a picture that painted. The
        // version saves unchecked and says so ('Visual check timed out'
        // regressed a whole 3D version on 2026-10-10). Only Stop is a stop.
        if(turnCancelled){turnSucceeded=false;turnError=error.message;phase('Stopped');}
        else {
          validationChecks.push({code:'visual-unreviewed',sourceHash:await hashSource(source).catch(()=>null)});
          turnNotes=[...turnNotes,'Picture not checked: '+String(error.message||error).slice(0,120)];
          log('Picture not checked; keeping the painted result · '+(error.message||error));
        }
      }
    }
    if(turnSucceeded&&!turnCancelled&&!turnRuntimeFailed&&painted&&lastPaintedSource===source){
      try {const version=versions.commit({source,request:turnRequest,layers:checkpoints,parent:turnParent,requestID:activeAttempt?.id});saved();phase(`v${version.id} · ${turnNotes.length?'Saved with notes':'Ready to play'}`);log(`Saved v${version.id} · ${checkpoints} layers`);const drawing=inputData(turnRequest)?.drawing;if(drawing)post({action:'drawingCommitted',drawingID:drawing.id,revision:drawing.revision});benchmark('versionCommitted',{version:version.id,layers:checkpoints});}
      catch(error){turnSucceeded=false;turnError=error.message;phase('Could not save version');log(error.message);benchmark('generationFailed',{message:error.message});}
    }else turnSucceeded=false;
    // A try that painted but failed its checks is not thrown away: it waits as
    // a draft the phone can keep as a version or discard. Partial progress is
    // the user's; the checks only decide what gets saved automatically.
    if(!turnSucceeded&&!turnCancelled&&source&&source!==previous&&painted&&lastPaintedSource===source){
      try{localStorage.setItem(storageKey+'-draft',JSON.stringify({source,request:turnRequest,error:turnError||'No verified version was committed',parent:turnParent,createdAt:new Date().toISOString()}));}catch{}
    }
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
// Debug installs can be launched with WHISTLEGRAPH_RETRY_ON_LAUNCH=1 so a
// failed request is tried again from a Mac without touching the phone. The
// flag is spent only when a resume actually proceeds: the first paint comes
// before the account token, and spending it there would retry nothing.
let launchRetryUsed=false;
const launchRetryPending=()=>window.__whistlegraphRetryOnLaunch===true&&!launchRetryUsed;
// Debug installs can also fire one request on launch (WHISTLEGRAPH_AUTO_ASK),
// once the account and the first paint are in: a hands-free make, for tests.
let autoAsked=false;
function autoAskIfDue(){
  if(autoAsked||typeof window.__whistlegraphAutoAsk!=='string'||!token||!ready||!painted||busy||!versions)return;
  autoAsked=true;log('Auto ask on launch');void window.whistlegraphAskDrawing(window.__whistlegraphAutoAsk,null);
}
function readDraft(){
  try{const draft=JSON.parse(localStorage.getItem(storageKey+'-draft'));if(draft?.source&&typeof draft.request==='string')return draft;}catch{}
  // A failed try from before drafts existed still has its last painted checkpoint in the journal.
  const attempt=readAttempt(localStorage,storageKey);
  if(attempt?.status==='failed'&&typeof attempt.checkpoint==='string'&&attempt.checkpoint.trim()&&attempt.checkpoint!==attempt.baseSource)
    return {source:attempt.checkpoint,request:attempt.displayText||attempt.localText||'Request',error:lastAttempt?.error||'Checks did not pass',parent:attempt.parent,createdAt:lastAttempt?.startedAt||''};
  return null;
}
// Keep the draft as the next version: it has to paint again on this phone,
// and the piece must still be where the draft left it.
async function keepDraft(){
  const draft=readDraft();
  if(!draft||busy||!versions)return;
  if(draft.parent!==versions.head.id){phase('The piece moved on; that draft belonged to v'+draft.parent);localStorage.removeItem(storageKey+'-draft');nativeSnapshot();return;}
  busy=true;previous=source;nativeSnapshot();
  source=draft.source;vfs.mount(file,source);render(source);phase('Keeping the draft…');
  await waitForPaint(source,()=>false,20000);
  if(painted&&lastPaintedSource===source){
    try{
      const version=versions.commit({source,request:draft.request,layers:0,parent:draft.parent});saved();
      localStorage.removeItem(storageKey+'-draft');lastAttempt={...(lastAttempt||{request:draft.request,startedAt:draft.createdAt}),status:'completed',error:''};
      phase(`v${version.id} · Kept as is`);log(`Kept the draft as v${version.id}`);
    }catch(error){source=previous;vfs.mount(file,source);render(source);phase('Could not keep the draft');log(error.message);}
  }else{source=previous;vfs.mount(file,source);render(source);phase('The draft did not paint this time');}
  busy=false;end();threadUpdate();
}
// ---- Turns on the knot (TURNS.md slice 5) ----
let remoteJob=null;
function remoteTurns(){
  try{const pref=localStorage.getItem('whistlegraph-remote-turns');if(pref==='on')return true;if(pref==='off')return false;}catch{}
  return accountHandle==='jeffrey'; // jeffrey first; everyone with the switch after.
}
async function submitRemoteTurn({text,displayText,drawing}){
  busy=false; // Nothing runs here; the phone is free to follow the ledger.
  try{
    const body={code:thread.identity.code,text,displayText:String(displayText||'').slice(0,200),baseVersion:versions.head.id,baseHash:await hashSource(versions.head.source),requestID:crypto.randomUUID()};
    if(drawing)body.drawing=drawing;
    const r=await fetch('https://aesthetic.computer/api/whistlegraph-turn',{method:'POST',headers:{'Content-Type':'application/json',Authorization:'Bearer '+token},body:JSON.stringify(body),signal:AbortSignal.timeout(20000)});
    const job=await r.json().catch(()=>({}));
    if(r.status!==202)throw Error(job.error||('Could not send the request (HTTP '+r.status+')'));
    remoteJob={id:job.id,parent:versions.head.id};
    if(activeAttempt)activeAttempt=saveAttempt(localStorage,storageKey,{...activeAttempt,remote:job.id,status:'working'});
    phase('Working on the knot…');log('Sent to the knot as turn '+String(job.id).slice(0,8));benchmark('remoteDispatched');
    nativeSnapshot();threadUpdate();
    void watchRemoteTurn(remoteJob);
  }catch(error){remoteFailed(error.message||String(error),null);}
}
function remoteFailed(message,draft){
  lastAttempt={...(lastAttempt||{request:''}),status:message==='Stopped'?'interrupted':'failed',error:message,finishedAt:new Date().toISOString()};
  if(activeAttempt)saveAttempt(localStorage,storageKey,{...activeAttempt,status:'failed'});
  if(draft)try{localStorage.setItem(storageKey+'-draft',JSON.stringify({source:draft,request:lastAttempt.request,error:message,parent:versions.head.id,createdAt:new Date().toISOString()}));}catch{}
  remoteJob=null;activeAttempt=null;busy=false;phase(message==='Stopped'?'Stopped':'Could not finish');log(message);
  $('live-stop').hidden=true;clearInterval(timer);updateFeed();threadUpdate();nativeSnapshot();
}
async function watchRemoteTurn(job){
  const startedAt=Date.now();
  while(remoteJob&&remoteJob.id===job.id){
    await new Promise(r=>setTimeout(r,5000));
    if(!remoteJob||remoteJob.id!==job.id)return;
    let state;
    try{const r=await fetch('https://aesthetic.computer/api/whistlegraph-turn?id='+encodeURIComponent(job.id),{headers:{Authorization:'Bearer '+token},signal:AbortSignal.timeout(15000)});state=await r.json();}catch{continue;}
    if(state.status==='queued'||state.status==='running'){
      if(Date.now()-startedAt>15*60000){remoteFailed('The knot did not finish in fifteen minutes. Try again.',state.checkpoint||null);return;}
      continue;
    }
    if(state.status==='done'){
      remoteJob=null;localStorage.removeItem(storageKey+'-inflight');activeAttempt=null;
      // The socket usually delivered the version already; if not, fetch the thread and follow it.
      if(versions.head.id<(state.result?.versionID||0)){
        try{const r=await fetch('https://aesthetic.computer/api/whistlegraph?code='+encodeURIComponent(thread.identity.code),{headers:{Authorization:'Bearer '+token},signal:AbortSignal.timeout(15000)});const t=await r.json();if(t?.ledger&&Number(t.revision)>Number(thread.revision))await thread.follow(t);}
        catch(error){log('Could not fetch the finished turn: '+error.message);}
      }
      const notes=state.result?.notes||[];
      phase(`v${versions.head.id} · ${state.result?.acceptance==='reviewed'?'Ready to play':'Saved with notes'}`);if(notes.length)log('Notes · '+notes.join('; '));
      $('live-stop').hidden=true;clearInterval(timer);updateFeed();threadUpdate();nativeSnapshot();return;
    }
    if(state.status==='failed'){remoteFailed(state.result?.error||'The knot could not finish',state.result?.draft||null);return;}
  }
}
async function resumeAttempt(manual=false){
  // A turn already on the knot is watched, never rerun here.
  if(!manual){const journal=readAttempt(localStorage,storageKey);if(journal?.remote&&journal.status==='working'&&!remoteJob&&token){remoteJob={id:journal.remote,parent:journal.parent};activeAttempt=journal;lastAttempt={request:journal.displayText||journal.localText||'',parent:journal.parent,status:'working',startedAt:new Date().toISOString()};phase('Working on the knot…');nativeSnapshot();void watchRemoteTurn(remoteJob);return;}}
  const launchRetry=!manual&&launchRetryPending();
  if(launchRetry)manual=true;
  if(busy||!token||!ready||!versions||(!manual&&!recoveryPending))return;
  if(launchRetry&&!painted)return; // Wait for the paint; the flag stays pending.
  if(launchRetry){launchRetryUsed=true;log('Retrying the last request on launch');}
  if(!painted){
    if(!manual)return;
    // Nothing has painted since the last edit: a half-written checkpoint can
    // wedge the runtime, and every retry after that is refused. Try again means
    // start over, so queue the request fresh (no checkpoint) and reload the
    // workspace; the account arriving on load resumes it on a clean preview.
    const attempt=readAttempt(localStorage,storageKey);
    if(!attempt){phase('Could not resume: request unavailable');return;}
    saveAttempt(localStorage,storageKey,{...attempt,checkpoint:undefined,status:'working',retries:0});
    phase('Restarting the preview…');setTimeout(()=>location.reload(),50);return;
  }
  const expectedToken=token;
  await accountVerification;
  if(busy||token!==expectedToken||!aiConsent.allowed||(!manual&&!recoveryPending))return;
  recoveryPending=false;
  const attempt=claimAttempt(localStorage,storageKey,versions.value,manual);
  if(!attempt){if(manual)phase('Could not resume: version changed or request unavailable');return;}
  phase(manual?'Trying again from the start…':'Resuming interrupted edit…');
  // A manual Try again starts over; the checkpoint is what failed the last time.
  await ask(attempt.text,attempt.displayText,null,null,attempt.localText,manual?{...attempt,checkpoint:undefined}:attempt);
}
window.whistlegraphEngineEvent=event=>{
  if(event.kind==='visualCapture'){
    if(pendingCapture?.id===event.captureID)pendingCapture.finish(event.error?Error(event.error):null,event);
    return;
  }
  if(event.kind==='account') {token=event.token;if(token){if(aiConsent.allowed)musicalSocket.resume();thread?.resume();}else{musicalSocket.suspend();thread?.suspend();}window.whistlegraphAccountReady=!!token;accountIdentity(token,event.notice||'',event.retry===true);if(token&&pending)void ask(pending);else void resumeAttempt();autoAskIfDue();}
  if(event.kind==='error'){phase('Sign-in needed');log(event.text);window.whistlegraphWorkFinished?.();}
  if(event.kind==='previewReady'){ready=true;log('AC runtime ready');}
  if(event.kind==='previewEvent'){
    if(event.event?.sourceHash!==previewHash||event.event?.requestID!==renderID)return;
    const streamingPreview=!!provisional&&previewSource===provisional;
    if(streamingPreview&&(event.event.kind==='invalidated'||event.event.kind==='console'&&(event.event.event?.level==='error'||/\b(?:Paint|Sim|Boot) failure\b/i.test(event.event.event?.message||'')))){
      rejectedPreview=provisional;log('Waiting for more source');
      const fallback=lastPaintedSource!==previewSource?lastPaintedSource:source;
      if(fallback&&fallback!==previewSource)render(fallback);
      return;
    }
    if(!streamingPreview)activeReceipt?.observe(event.event);
    if(event.event.kind==='painted'){if(turnStarter&&previewSource===turnStarter&&!starterPainted){starterPainted=true;benchmark('starterPainted');}if(previewSource.trimEnd()!==previous.trimEnd())window.__whistlegraphSequenceEvent?.('painted');painted=true;lastPaintedSource=previewSource;if(busy&&activeAttempt&&source===previewSource&&!turnRuntimeFailed){activeAttempt={...activeAttempt,checkpoint:source};try{saveAttempt(localStorage,storageKey,activeAttempt);}catch(error){log('Could not persist checkpoint: '+error.message);}}feedback={...feedback,rendered:true,updatedAt:new Date().toISOString()};if(!streamingPreview&&activeReceipt&&previewSource.trimEnd()!==previous.trimEnd())activeReceipt.painted();log('Checkpoint painted');phase(busy?'Building…':lastAttempt?.status==='failed'?'Could not finish · previous version restored':'Ready to play');if(narrationPending!==null){post({action:'narrationReady',version:narrationPending});narrationPending=null;}void resumeAttempt();autoAskIfDue();}
    if(event.event.kind==='invalidated'){turnRuntimeFailed=true;window.__whistlegraphSequenceEvent?.('runtimeError',{message:'Preview invalidated'});painted=false;feedback={...feedback,rendered:false,logs:[...(feedback?.logs||[]),{level:'error',text:'Preview invalidated'}],updatedAt:new Date().toISOString()};log('Preview failed; inspect activity');phase('Preview error');if(lastPaintedSource && lastPaintedSource!==previewSource){render(lastPaintedSource);log('Restored last painted checkpoint');}}
    if(event.event.kind==='console'&&['error','warn'].includes(event.event.event?.level)){
      const entry={level:event.event.event.level,text:event.event.event.message||'Runtime error'};
      if(/\b(?:Paint|Sim|Boot) failure\b/i.test(entry.text))entry.level='error';
      log(entry.text);runtimeErrors.push(entry.text);if(runtimeErrors.length>20)runtimeErrors.shift();
      feedback={...feedback,rendered:painted,logs:[...(feedback?.logs||[]),entry].slice(-20),updatedAt:new Date().toISOString()};
      if(entry.level==='error'){turnRuntimeFailed=true;turnError=entry.text;window.__whistlegraphSequenceEvent?.('runtimeError',{message:entry.text});}
      threadUpdate();
    }
  }
};
$('live-stop').onclick=()=>{
  if(remoteJob){const id=remoteJob.id;remoteJob=null;fetch('https://aesthetic.computer/api/whistlegraph-turn?id='+encodeURIComponent(id),{method:'DELETE',headers:{Authorization:'Bearer '+token}}).catch(()=>{});remoteFailed('Stopped',null);return;}
  turnCancelled=true;pending='';visualController?.abort();server?.interrupt();if(!busy){end();phase('Stopped');}};
window.whistlegraphUndo=()=>{try{source=versions.undo().source;}catch(error){log(error.message);return;}previous=source;vfs.mount(file,source);saved();server?.close();server=null;render(source||'export function paint({wipe}) {wipe("black");}');review(false);phase('Undone');};
window.whistlegraphAsk=ask;
window.whistlegraphAskDrawing=(text,drawing)=>{
  try{return ask(withDrawing(text,drawing),text||(drawing?'Drawing':''),null,null,drawing?'':text);}
  catch(error){phase('Could not read drawing');log(error.message);return Promise.resolve();}
};
let musicalTurn=0;
const musicalSocket=new MusicalInputSocket({token:()=>aiConsent.allowed?token:null,onEvent:benchmark});
document.addEventListener('visibilitychange',()=>{if(document.hidden){musicalSocket.suspend();thread?.suspend();}else{if(aiConsent.allowed)musicalSocket.resume();thread?.resume();}});
const musicalAdvisor=new MusicalInputAdvisor({fetchImpl:(...args)=>{aiConsent.require();return musicalSocket.fetch(...args);},token:()=>aiConsent.allowed?token:null,onEvent:(event,fields)=>{benchmark(event,fields);if(event==='jevDecision')log('Jev · '+fields.choice);}});
window.whistlegraphInputStart=()=>{musicalTurn++;musicalAdvisor.reset();};
window.whistlegraphInputCancel=()=>{musicalTurn++;musicalAdvisor.cancel();};
window.whistlegraphObserveSound=input=>{musicalPrompt(input);if(aiConsent.allowed&&wantsSoundEvidence(input.transcript))musicalAdvisor.observe(input);};
window.whistlegraphAskSound=async input=>{
 if(!aiConsent.allowed){post({action:'aiConsent'});return;}
 const prompt=withDrawing(musicalPrompt(input),input.drawing);const useSound=!!input.drawing||wantsSoundEvidence(input.transcript);if(!useSound)musicalAdvisor.cancel();phase(useSound?'Interpreting sound…':'Sending…');
 return ask(prompt,`${input.transcript||'Sound'} · ${(input.sound.durationMs/1000).toFixed(1)} seconds`,input.drawing||!useSound||localEdit(source,input.transcript)?null:musicalAdvisor.finish(input),input.drawing?null:instantPiece(input.transcript),input.drawing?'':input.transcript);
};
window.whistlegraphSetAIConsent = value => { consentState = value || {}; syncAIConsent(); };
window.whistlegraphForgetLocalData = () => {
  consentState={}; aiConsent.setAllowed(false); turnCancelled=true;
  visualController?.abort(); server?.interrupt(); musicalAdvisor.cancel(); musicalSocket.suspend(); thread?.suspend();
  token=accountToken=accountHandle=''; localStorage.clear();
};
window.whistlegraphIsBusy=()=>busy;
window.whistlegraphHasReview=()=>false;
if(!window.__whistlegraphSequence&&!window.__whistlegraphBenchmark&&!window.__whistlegraphLocalSequence&&!window.__whistlegraphDisableThread)try{if(initializeBasePiece(localStorage,storageKey))lastAttempt=null;}catch(error){phase('Could not initialize piece');log(error.message);}
try{source=window.__whistlegraphSpace&&window.__whistlegraphSequenceStart===1?'':window.__whistlegraphLocalSequence?'':window.__whistlegraphSequence?((window.__whistlegraphSequenceStart>1?localStorage.getItem(storageKey):'')||localStorage.getItem('whistlegraph-benchmark-source')||''):window.__whistlegraphBenchmark?'':localStorage.getItem(storageKey)||'';if(source){previous=source;ui.hidden=false;document.body.classList.add('live-mode','live-preview');$('initial').hidden=true;render(source);}}catch{}
try {
  const versionKey=storageKey+'-versions';
  if(window.__whistlegraphLocalSequence||window.__whistlegraphBenchmark||(window.__whistlegraphSequence&&window.__whistlegraphSequenceStart===1))localStorage.removeItem(versionKey);
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
if(typeof window.__whistlegraphFixtureBusy==='string'){
  busy=true;ui.hidden=false;document.body.classList.add('live-mode','live-preview');
  lastAttempt={request:window.__whistlegraphFixtureBusy,parent:versions?.head.id??0,status:'working',startedAt:new Date().toISOString()};
  outputStream='export function paint({ wipe, ink, circle, screen }) {\n  wipe("#151838");\n  ink("#4653c6");\n  circle(screen.width / 2, screen.height / 2, 40, true);';
  $('live-code').textContent=outputStream;phase('Writing…');$('live-stop').hidden=false;review(false);
}
window.whistlegraphSourceEditor = new SourceEditor({
  state: () => {
    if (!versions) throw Error('The piece is still loading.');
    return {piece: thread?.identity.id || storageKey, code: thread?.identity.code || '',
      version: versions.head.id, source: versions.head.source, busy,
      recording: !!window.whistlegraphRecording?.()};
  },
  checks: sourceChecks,
  hash: hashSource,
  begin: () => {
    busy = true; previous = source; recoveryPending = false;
    turnRuntimeFailed = false; turnCancelled = false; turnError = '';
    clearTimeout(compileTimer); compileTimer = null; provisional = '';
    server?.close(); server = null; review(false); phase('Checking source edit…');
  },
  render: value => { render(value); return renderID; },
  inspect: () => ({requestID: renderID, sourceHash: previewHash,
    rendered: painted && lastPaintedSource === previewSource,
    logs: feedback?.logs || [], runtimeFailed: turnRuntimeFailed, cancelled: turnCancelled}),
  commit: value => {
    const version = versions.commit(value);
    source = previous = version.source; lastAttempt = null;
    vfs.mount(file, source); saved();
    return version;
  },
  restore: () => {
    // Read the current durable head, so even a concurrent checkout is preserved.
    source = previous = versions.head.source;
    turnRuntimeFailed = false; turnCancelled = false;
    vfs.mount(file, source);
    render(source || 'export function paint({wipe}) {wipe("black");}');
  },
  finish: committed => {
    busy = false; review(false);
    phase(committed ? `v${versions.head.id} · Ready to play` : 'Could not apply source edit');
    updateFeed(); window.whistlegraphWorkFinished?.();
  },
});

updateFeed();
if(versions&&!window.__whistlegraphSequence&&!window.__whistlegraphBenchmark&&!window.__whistlegraphDisableThread) {
  const label=codeLabel;label.setAttribute('aria-live','polite');
  thread=new WhistlegraphThread({storage:localStorage,key:storageKey,receipts,token:()=>token,ledger:()=>versions.value,
    state:()=>({busy,phase:$('live-phase').textContent,head:versions.head.id,source:versions.head.source,errors:runtimeErrors,attempt:lastAttempt}),
    onStatus:(code,status)=>{label.textContent=code?'/'+code:'';label.title=status;label.dataset.status=status;post({action:'threadStatus',code:code||'',threadID:thread.identity.id,status});nativeSnapshot();},
    // A turn that ran off the phone: take the server's ledger as our own.
    // While a turn runs here, it waits until that turn ends.
    onAdopt:async ledger=>{if(busy){pendingAdopt=ledger;return;}adoptLedger(ledger);},
    onCommand:async command=>{
      if(command.action==='layout'){
        if(typeof command.css!=='string'||new TextEncoder().encode(command.css).length>100000)throw Error('Invalid layout');
        window.whistlegraphApplyLayout(command.css);
        return {ok:true,head:versions.head.id};
      }
      await verifyThreadRevision(command,()=>({busy,head:versions.head.id,source:versions.head.source}));
      const before=versions.head.id;
      if(command.action==='ask')await ask(checkedPrompt(command.text));
      else if(command.action==='undo')window.whistlegraphUndo();
      else if(command.action==='edit') {
        const request=checkedPrompt(command.text||'Remote source edit');
        const findings=sourceChecks(command.source);
        if(findings.length)throw Error('Source check failed: '+findings.map(f=>f.message).join('; '));
        busy=true;previous=source;turnRuntimeFailed=false;turnCancelled=false;checkpoints=1;server?.close();server=null;review(false);
        source=command.source;vfs.mount(file,source);phase('Applying remote edit…');render(source);
        try {
          for(let i=0;i<500&&!painted&&!turnRuntimeFailed&&!turnCancelled;i++)await new Promise(r=>setTimeout(r,10));
          await new Promise(r=>setTimeout(r,250));
          if(!painted||lastPaintedSource!==source||turnRuntimeFailed||turnCancelled)throw Error('Remote edit did not paint cleanly');
          versions.commit({source,request,layers:1,parent:before});saved();phase('Ready to play');
        }catch(error){source=previous;vfs.mount(file,source);render(source);throw error;}
        finally{end();}
      }
      return {ok:versions.head.id!==before,head:versions.head.id,error:versions.head.id===before?(turnError||'No new version'):''};
    }});
  label.textContent=thread.identity.code?'/'+thread.identity.code:'';
  const newPiece=document.createElement('button');newPiece.textContent='New piece';newPiece.style.cssText='font:24px Comic,Arial;padding:14px';
  newPiece.onclick=window.whistlegraphNewPiece=()=>{
    if(busy)return {accepted:false,reason:'busy'};
    const parked=archiveCurrentPiece();if(parked.accepted===false)return parked;
    location.reload();return {accepted:true};
  };
  // Every piece on this phone: the open one plus the archives "New piece" left.
  // Opening another swaps archives, so the current one is never lost.
  const ARCHIVE='whistlegraph-archive-',ARCHIVE_SUFFIXES=['-cloud-revision','-cloud-ledger','-receipts','-receipt-cost','-attempt','-inflight'];
  // The archive write comes first: when the phone's 5 MB store is full it
  // throws, nothing is removed, and the open piece stays exactly as it was.
  function archiveCurrentPiece(){
    const extras={};for(const suffix of ARCHIVE_SUFFIXES){const v=localStorage.getItem(storageKey+suffix);if(v!==null)extras[suffix]=v;}
    try{localStorage.setItem(ARCHIVE+thread.identity.id,JSON.stringify({identity:thread.identity,ledger:versions.value,source,extras,archivedAt:new Date().toISOString()}));}
    catch{phase('This phone has no room to put the open piece away');return {accepted:false,reason:'storageFull'};}
    thread.suspend();
    for(const suffix of ['', '-versions','-thread','-cloud-revision','-cloud-ledger','-attempt','-inflight','-receipts','-receipt-cost'])localStorage.removeItem(storageKey+suffix);
    return {accepted:true};
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
  window.whistlegraphVersionSource=id=>versions?.value.versions.find(v=>v.id===id)?.source??null;
  window.whistlegraphOpenPiece=id=>{
    if(busy||id===thread.identity.id)return {accepted:false,reason:'busy'};
    let saved;try{saved=JSON.parse(localStorage.getItem(ARCHIVE+id));}catch{}
    if(!saved?.identity?.id||!saved.ledger){phase('That piece is no longer on this phone');postPieces();return {accepted:false,reason:'notReady'};}
    const parked=archiveCurrentPiece();if(parked.accepted===false)return parked;
    try{
      localStorage.setItem(storageKey,saved.source||'');
      localStorage.setItem(storageKey+'-versions',JSON.stringify(saved.ledger));
      localStorage.setItem(storageKey+'-thread',JSON.stringify(saved.identity));
      for(const [suffix,value] of Object.entries(saved.extras||{}))localStorage.setItem(storageKey+suffix,value);
    }catch{phase('This phone has no room to open that piece');location.reload();return {accepted:false,reason:'storageFull'};}
    localStorage.removeItem(ARCHIVE+id);
    location.reload();return {accepted:true};
  };
  // Deleting frees this phone's store; the open piece is never deletable here.
  window.whistlegraphDeletePiece=id=>{
    if(busy||id===thread.identity.id)return {accepted:false,reason:'busy'};
    localStorage.removeItem(ARCHIVE+id);postPieces();return {accepted:true};
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
// A slow connection is not a status line; the console keeps the note and the engine keeps waiting.
setTimeout(()=>{if(!ready){ui.hidden=false;log('AC runtime has not reported ready after 20 s. Check the connection.');}},20000);

if(window.__whistlegraphSequence && !window.__whistlegraphLocalSequence && window.__whistlegraphReviewVersion===undefined) import("./sequence-benchmark.mjs").then(({runSequence})=>runSequence({ask,ready:()=>ready,source:()=>source,painted:()=>painted&&lastPaintedSource===source,interrupt:()=>server?.interrupt(),model:window.__whistlegraphModel||DEFAULT_MODEL}));

if(window.__whistlegraphSequence && Number.isInteger(window.__whistlegraphReviewVersion)) void (async()=>{
  const revision=versions.value.versions.find(v=>v.id===window.__whistlegraphReviewVersion);
  if(!revision){phase('Review version unavailable');return;}
  source=revision.source;render(source);
  for(let i=0;i<300&&!(ready&&painted&&lastPaintedSource===source);i++)await new Promise(r=>setTimeout(r,100));
  if(!painted){phase('Review preview unavailable');return;}
  phase(`Reviewing v${revision.id}`);
  const r=frame.getBoundingClientRect();
  post({action:'sequenceCapture',report:{index:revision.id,total:1,source,evidenceOnly:true,requiresMotion:true,checks:{committedSourcePainted:true},scope:'Exact saved version replayed on physical phone. Sequential native snapshots; no model edit, version commit or visual verdict.'},rect:{x:r.x,y:r.y,width:r.width,height:r.height}});
})();

if(window.__whistlegraphLocalSequence)import('./local-sequence.mjs').then(({runLocalSequence})=>runLocalSequence({ask,source:()=>source,head:()=>versions.value.head,count:()=>versions.value.versions.length,ready:()=>ready,painted:()=>painted&&lastPaintedSource.trimEnd()===(source||'export function paint({wipe}) {wipe("black");}').trimEnd(),undo:()=>window.whistlegraphUndo()}));
