import {spawnSync} from 'node:child_process';
import {macPalStage} from './stage-overlays.mjs';
const inspectChrome=`
const se=Application('System Events');
const chrome=se.processes.byName('Google Chrome');
const matches=[];
function walk(e,depth){
 if(depth>14)return;
 let role='';try{role=e.role();}catch{return;}
 if(role==='AXWebArea')return;
 for(const key of ['name','description','value']){try{const v=String(e[key]());if(/Allow remote debugging\\?|Chrome is being controlled by automated test software/i.test(v))matches.push(v);}catch{}}
 let children=[];try{children=e.uiElements();}catch{}
 for(const child of children)walk(child,depth+1);
}
for(const w of chrome.windows())walk(w,0);
JSON.stringify([...new Set(matches)]);
`;
export function assertPresentationClean({exec=spawnSync,checkMacPal=true}={}) {
 if(checkMacPal){const state=macPalStage({exec}).capture();if(state.loaded||state.running)throw Error('MacPal is active; stop capture and restore clean Stage Mode');}
 const r=exec('/usr/bin/osascript',['-l','JavaScript','-e',inspectChrome],{encoding:'utf8',timeout:10000});
 if(r.status!==0)throw Error('Cannot verify native Chrome presentation state');
 const matches=JSON.parse(r.stdout);
 if(matches.length)throw Error('Chrome debugging dialog/banner is visible; resolve it before filming');
 return {macpalHidden:checkMacPal,chromeDebuggingUIAbsent:true};
}

// Close only the named automation infobar. Never press Chrome's consent,
// "Turn off in settings", tab-close, or window-close controls.
const dismissBanner=`
const se=Application('System Events');
const chrome=se.processes.byName('Google Chrome');
let candidate=null;
function label(e){const values=[];for(const k of ['name','description','value']){try{values.push(String(e[k]()));}catch{}}return values.join(' ');}
function walk(e,depth){
 if(depth>14)return {banner:false,buttons:[]};
 let role='';try{role=e.role();}catch{return {banner:false,buttons:[]};}
 if(role==='AXWebArea')return {banner:false,buttons:[]};
 const text=label(e);
 let banner=/Chrome is being controlled by automated test software/i.test(text);
 const buttons=[];
 if(role==='AXButton'){
  let name='',desc='';try{name=e.name();}catch{}try{desc=e.description();}catch{}
  if([name,desc].some(v=>/^(close|dismiss)( (notification|banner|infobar|info bar))?$/i.test(String(v))))buttons.push(e);
 }
 let children=[];try{children=e.uiElements();}catch{}
 for(const child of children){const hit=walk(child,depth+1);banner=banner||hit.banner;buttons.push(...hit.buttons);}
 // A small deepest group must own both the exact banner and its sole close
 // button; never fall back to an enclosing browser window or toolbar.
 if(!candidate&&banner&&buttons.length===1&&['AXGroup','AXUnknown'].includes(role))candidate=buttons[0];
 return {banner,buttons};
}
let found=false;for(const w of chrome.windows())found=walk(w,0).banner||found;
if(found&&!candidate)throw new Error('Automation banner found but its own close control is not uniquely identifiable');
if(candidate)candidate.click();
JSON.stringify({dismissed:Boolean(candidate),bannerFound:found});
`;
export function dismissAutomationBanner({exec=spawnSync}={}) {
 const r=exec('/usr/bin/osascript',['-l','JavaScript','-e',dismissBanner],{encoding:'utf8',timeout:10000});
 if(r.status!==0)throw Error('Cannot safely dismiss Chrome automation banner: '+String(r.stderr||'native UI unavailable').slice(0,250));
 const result=JSON.parse(r.stdout);
 return {schema:'captutor-chrome-presentation/v1',checkedAt:new Date().toISOString(),...result};
}
