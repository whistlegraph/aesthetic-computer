import {localMoves} from './local-moves.mjs';
import {readScene} from './local-scene.mjs';
const delay=ms=>new Promise(r=>setTimeout(r,ms));
export async function runLocalSequence(api){
 while(!window.walkiewareAccountReady||!api.ready())await delay(100);
 let expected={};
 for(let i=0;i<localMoves.length;i++){
  const [text,change]=localMoves[i];expected={...expected,...change};
  const before=api.source(),head=api.head(),count=api.count(),events=[];const start=performance.now();
  window.__walkiewareSequenceEvent=(event,fields={})=>events.push({event,ms:performance.now()-start,...fields});
  await api.ask(text);
  const source=api.source(),firstHead=api.head();
  const checks={properties:JSON.stringify(readScene(source))===JSON.stringify(expected),painted:api.painted(),oneVersion:api.count()===count+1&&firstHead!==head,noInference:!events.some(e=>e.event==='requestDispatched')};
  api.undo();for(let n=0;n<200&&!api.painted();n++)await delay(10);
  checks.undo=api.source()===before&&api.head()===head&&api.painted();
  await api.ask(text);checks.replay=JSON.stringify(readScene(api.source()))===JSON.stringify(readScene(source))&&api.painted()&&api.count()===count+2;
  const report={index:i+1,total:32,prompt:text,source,expected,events,checks,requiresMotion:expected.bounce,scope:'Local text edits on physical phone. Each move checks properties, painted source, one version, undo and replay; no speech or inference.'};
  const rect=document.getElementById('live-piece').getBoundingClientRect();
  const result=await new Promise(resolve=>{window.__walkiewareSequenceCaptureDone=resolve;window.webkit.messageHandlers.walkie.postMessage({action:'sequenceCapture',id:'sequence',report,rect:{x:rect.x,y:rect.y,width:rect.width,height:rect.height}});});
  window.__walkiewareSequenceEvent=null;
  document.getElementById('live-phase').textContent=`Local edit ${i+1}/32 ${result.passed?'passed':'failed'}`;
  if(!result.passed)return;
 }
 document.getElementById('live-keep').click();
}
