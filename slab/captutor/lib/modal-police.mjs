import { execFile } from 'node:child_process';
import { promisify } from 'node:util';
import { createHash } from 'node:crypto';
import { mkdirSync, readFileSync, writeFileSync, renameSync, appendFileSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { homedir } from 'node:os';
const run = promisify(execFile);
export const CHROME_MODAL_SCRIPT = `
const chrome=Application('System Events').processes.byName('Google Chrome');
const hits=[];
function text(e){const a=[];for(const k of ['name','description','value'])try{const v=String(e[k]());if(v&&v!=='undefined')a.push(v);}catch{}return [...new Set(a)].join(' ').slice(0,1000);}
function walk(e,depth){
 if(depth>14)return {texts:[],buttons:[],modal:false};
 let role='',subrole='';try{subrole=e.subrole();}catch{}try{role=e.role();}catch{return {texts:[],buttons:[],modal:false};}
 if(role==='AXWebArea')return {texts:[],buttons:[],modal:false};
 const texts=[text(e)],buttons=[];
 if(role==='AXButton'){
  const labels=[];for(const k of ['name','description'])try{labels.push(String(e[k]()));}catch{}
  const name=labels.find(v=>/^(allow|close|dismiss)( (notification|banner|infobar|info bar))?$/i.test(v))||text(e);
  buttons.push({name,element:e});
 }
 let modal=false,children=[];try{children=e.uiElements();}catch{}
 for(const c of children){const r=walk(c,depth+1);texts.push(...r.texts);buttons.push(...r.buttons);modal=modal||r.modal;}
 const all=texts.join(' ');
 const kind=/Allow remote debugging\\?/i.test(all)?'remote-debugging':/Chrome is being controlled by automated test software/i.test(all)?'automation-banner':null;
 const close=buttons.filter(b=>/^(close|dismiss)( (notification|banner|infobar|info bar))?$/i.test(b.name));
 const allow=buttons.filter(b=>/^allow$/i.test(b.name));
 const actionable=kind==='remote-debugging'?allow:close;
 if(!modal&&((kind&&actionable.length===1&&['AXGroup','AXUnknown','AXDialog','AXSheet','AXToolbar'].includes(role))||['AXDialog','AXSheet'].includes(role)||['AXDialog','AXSystemDialog'].includes(subrole))){
  hits.push({kind:kind||'unknown',title:kind||texts.filter(Boolean).join(' ').slice(0,400),buttons:buttons.map(b=>b.name),element:actionable.length===1?actionable[0].element:null});modal=true;
 }
 return {texts,buttons,modal};
}
let windowIndex=0;
for(const w of chrome.windows()){const start=hits.length;let sheets=[];try{sheets=w.sheets();}catch{}
 // Chrome can retain off-screen AX group copies of expired consent dialogs.
 // A real attached sheet is the active blocking surface; inspect it first.
 for(const root of sheets.length?sheets:[w])walk(root,0);for(const h of hits.slice(start))h.windowIndex=windowIndex;windowIndex++;}
// Chrome may expose the same sheet both below its window and as a window.
const uniqueHits=[];const seen=new Set(),physical=new Set();
for(const h of hits){let position=null;try{position=h.element.position();}catch{}
 // Chrome can report duplicate sheet positions during a native relayout.
 const key=JSON.stringify([h.windowIndex,h.kind,h.title,h.buttons,h.kind==='remote-debugging'?null:position]);
 const physicalKey=JSON.stringify([h.kind,h.title,h.buttons,position]);
 if(!seen.has(key)&&!physical.has(physicalKey))uniqueHits.push(h);
 seen.add(key);physical.add(physicalKey);
}
`;
export async function readNativeModalScan(execute){
 for(let attempt=0;attempt<3;attempt++){
  const {stdout}=await execute();
  if(!stdout.trim()){
   if(attempt<2){await new Promise(resolve=>setTimeout(resolve,150));continue;}
   throw Error('Native modal inspection returned no result after 3 scans');
  }
  const hits=JSON.parse(stdout);
  if(!Array.isArray(hits)||hits.some(h=>!h||typeof h.kind!=='string'||typeof h.title!=='string'||!Array.isArray(h.buttons)||h.buttons.some(b=>typeof b!=='string')))throw Error('Invalid native modal inspection result');
  return hits;
 }
}
async function native(action,expected) {
 const tail=action
  ? `const h=uniqueHits.filter(h=>h.kind===${JSON.stringify(action)});if(h.length!==1||!h[0].element)throw Error('Modal changed or action ambiguous');if(JSON.stringify([h[0].kind,h[0].title,[...h[0].buttons].sort()])!==${JSON.stringify(JSON.stringify(expected?[expected.kind,expected.title,[...expected.buttons].sort()]:null))})throw Error('Modal fingerprint changed');h[0].element.click();JSON.stringify(true);`
  : `JSON.stringify(uniqueHits.map(({kind,title,buttons})=>({kind,title,buttons})));`;
 const execute=()=>run('/usr/bin/osascript',['-l','JavaScript','-e',CHROME_MODAL_SCRIPT+tail],{timeout:30000,maxBuffer:128*1024});
 // Never replay a click. Only retry a read-only scan with an empty response.
 if(!action)return readNativeModalScan(execute);
 return JSON.parse((await execute()).stdout);
}
export function fingerprintModal(hit) {
 return createHash('sha256').update(JSON.stringify([hit.kind,hit.title,[...hit.buttons].sort()])).digest('hex').slice(0,20);
}
export function createModalPolice({
 directory=join(homedir(),'.local/share/captutor/modal-police'),
 scan=()=>native(), act=(kind,hit)=>native(kind,hit), onEvent=event=>process.stderr.write(JSON.stringify({source:"modal-police",...event})+"\n"),
 allowRemoteDebugging=false,
}={}) {
 mkdirSync(directory,{recursive:true});
 const memoPath=join(directory,'memo.json');
 let memo={};try{memo=JSON.parse(readFileSync(memoPath,'utf8'));}catch(error){if(error.code!=='ENOENT')throw error;}
 let active=new Set();
 const emit=event=>{appendFileSync(join(directory,'events.jsonl'),JSON.stringify(event)+'\n');onEvent(event);};
 return {
  async check(phase='recording',{mayHandle=()=>true}={}) {
   if(!['connecting','preparing','recording'].includes(phase))throw Error('Unknown modal-police phase');
   let hits;
   try{hits=await scan();}catch(error){emit({type:'blocked',kind:'inspection-failed',at:new Date().toISOString()});throw error;}
   if(!mayHandle())return {checkedAt:new Date().toISOString(),modals:hits.length,expired:true};
   const seen=new Set();let blocked;
   for(const hit of hits){
    const id=fingerprintModal(hit);seen.add(id);
    const remembered=Boolean(memo[id]);
    const allowed=phase!=='recording'&&hit.kind==='automation-banner'||phase==='connecting'&&allowRemoteDebugging&&hit.kind==='remote-debugging';
    const at=new Date().toISOString();
    const entry=memo[id]||{kind:hit.kind,firstSeen:at,count:0};
    entry.lastSeen=at;entry.count++;entry.response=allowed?(hit.kind==='remote-debugging'?'allow':'dismiss'):'flag';memo[id]=entry;
    // The memo is recognition evidence, never authority to click a future dialog.
    if(allowed&&mayHandle()){await act(hit.kind,hit);emit({type:'handled',id,kind:hit.kind,response:entry.response,remembered,at});}
    else{if(!active.has(id))emit({type:'blocked',id,kind:hit.kind,remembered,at});blocked=hit.kind;}
   }
   const tmp=memoPath+'.'+process.pid+'.tmp';writeFileSync(tmp,JSON.stringify(memo,null,2));renameSync(tmp,memoPath);active=seen;
   if(blocked){const error=Error('Modal police blocked capture: '+blocked);error.code='CAPTUTOR_MODAL_BLOCKED';throw error;}
   return {checkedAt:new Date().toISOString(),modals:hits.length};
  },
 };
}
export function approvedDebuggingPolicy() {
 if(process.env.CAPTUTOR_ALLOW_REMOTE_DEBUGGING==='1')return true;
 try{return JSON.parse(readFileSync(join(homedir(),'.config/captutor/modal-police.json'),'utf8')).allowRemoteDebugging===true;}catch{return false;}
}
// Watch only this connection attempt; never leave an unattended consent clicker.
export async function connectWithModalPolice(connect,{police=createModalPolice({allowRemoteDebugging:approvedDebuggingPolicy()}),intervalMs=500}={}) {
 let pending=true;let failure;
 const watch=(async()=>{while(pending){await new Promise(r=>{setTimeout(r,intervalMs);});if(!pending)break;try{await police.check('connecting',{mayHandle:()=>pending});}catch(error){if(/Modal changed or action ambiguous|Modal fingerprint changed/.test(String(error.stderr||error.message)))continue;failure=error;break;}}})();
 let result;
 try{result=await connect();}finally{pending=false;await watch;}
 if(failure){await result?.close?.();throw failure;}
 return result;
}
