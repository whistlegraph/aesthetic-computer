import {installTouchControls} from './touch-controls.mjs?v=2026-10-12.8';
import {observeInteraction} from '../aesthetic.computer/lib/frame-interaction.mjs';
if (new URLSearchParams(location.search).get('mime') === '1') observeInteraction();
import {compileWad,starterLevel,validateLevel,inspectWad,WAD_MIME} from './wad.mjs?v=2026-10-12.3';
const canvas=document.querySelector('#canvas'),status=document.querySelector('#status'),start=document.querySelector('#start'),boot=document.querySelector('#boot');
const query=new URLSearchParams(location.search),preview=query.get('preview')==='1';
if(preview)document.body.classList.add('preview');
// Compile in a shared HTTPS worker; the app keeps native controls/history.
let compilerWorker;
window.addEventListener('message',event=>{
 if(!preview||query.get('bridge')!=='1'||event.source!==parent||event.data?.type!=='doom:compile')return;
 if(!compilerWorker){compilerWorker=new Worker(new URL('./compile-worker.mjs?v=2026-10-12.13',import.meta.url),{type:'module'});compilerWorker.onmessage=reply=>{if(reply.data.wad)parent.postMessage(reply.data,'*',[reply.data.wad]);else parent.postMessage(reply.data,'*');};compilerWorker.onerror=()=>parent.postMessage({type:'doom:compiled',id:event.data.id,error:'Doom compiler worker failed to load'},'*');}
 compilerWorker.postMessage(event.data);
});
let level=starterLevel(),wad,started=false,downloadURL,playing=true,game='doom';
function loadWad(next){const info=inspectWad(next);if(info.kind!=='PWAD')throw Error('Open a patch WAD for Freedoom; the base game is already included.');const marker=info.lumps.find(l=>/^(?:E[1-4]M[1-9]|MAP(?:0[1-9]|[12][0-9]|3[0-2]))$/.test(l.name));if(!marker)throw Error('WAD needs a classic Doom or Doom II map.');wad=next;map=marker.name;game=map.startsWith('MAP')?'doom2':'doom';const things=info.lumps.slice(info.lumps.indexOf(marker)+1).find(l=>l.name==='THINGS');level.things=things?.size>10?[{}]:[];}

try{if(query.has('level'))level=validateLevel(JSON.parse(query.get('level')));wad=compileWad(level);}catch(error){status.textContent=error.message;throw error;}
function download(){if(downloadURL)URL.revokeObjectURL(downloadURL);downloadURL=URL.createObjectURL(new Blob([wad],{type:WAD_MIME}));document.querySelector('#download').href=downloadURL;}
download();
const cfg='fullscreen 0\ngrabmouse 0\nuse_mouse 0\nuse_joystick 0\nscreenblocks '+(preview?11:10)+'\nsfx_volume 8\nmusic_volume 6\nsnd_musicdevice 3\nsnd_sfxdevice 3\nkey_up 72\nkey_down 80\nkey_left 75\nkey_right 77\nkey_fire 29\nkey_use 57\nkey_strafeleft 51\nkey_straferight 52\n';
let map='E1M1';
document.querySelector('#file').onchange=async event=>{
  try{
    const file=event.target.files[0];if(!file)return;if(file.size>32*1024*1024)throw Error('WAD must be smaller than 32 MB');
    if(started)throw Error('Restart the player before opening another WAD');loadWad(new Uint8Array(await file.arrayBuffer()));const response=await fetch('./vendor/freedoom'+(game==='doom2'?'2':'1')+'.wad');if(!response.ok)throw Error('Could not load Freedoom');base=new Uint8Array(await response.arrayBuffer());Module.FS.writeFile('/base.wad',base);Module.FS.writeFile('/level.wad',wad);download();status.textContent=file.name;
  }catch(error){status.textContent=error.message;}
};
const engineKeys=new WeakSet();
const held=new Set(),keyCodes={ArrowLeft:37,ArrowUp:38,ArrowRight:39,ArrowDown:40,Comma:188,Period:190,ControlLeft:17,Space:32,Enter:13,Escape:27};
function key(code,down){if(down)held.add(code);else held.delete(code);const key=code==='Space'?' ':code==='ControlLeft'?'Control':code==='Comma'?',':code==='Period'?'.':code;const event=new KeyboardEvent(down?'keydown':'keyup',{key,code,keyCode:keyCodes[code],which:keyCodes[code],bubbles:true});engineKeys.add(event);window.dispatchEvent(event);}
const movementKeys={KeyW:'ArrowUp',KeyS:'ArrowDown',KeyA:'Comma',KeyD:'Period',...(query.get('mime')==='1'?{Space:'ControlLeft',Enter:'ControlLeft',KeyE:'Space'}:{})},keyboardHeld=new Set();
if(query.get('mime')==='1')canvas.setAttribute('aria-description','WASD move and strafe; left/right arrows turn; Space or Enter shoot; E opens doors; Escape goes to the next post.');
for(const type of ['keydown','keyup'])window.addEventListener(type,event=>{
  if(query.get('mime')==='1'&&event.code==='Escape'){
    event.preventDefault();event.stopImmediatePropagation();
    if(type==='keydown'&&!event.repeat){release();parent.postMessage({type:'mime:next'},'*');}
    return;
  }
  if(engineKeys.has(event))return;
  const code=movementKeys[event.code];if(!code)return;
  const down=type==='keydown';
  if(down&&(!started||!playing||document.hidden||event.metaKey||event.ctrlKey||event.altKey||event.target.closest?.('input,textarea,select,[contenteditable]')))return;
  if(!down&&!keyboardHeld.has(event.code))return;
  event.preventDefault();event.stopImmediatePropagation();
  if(down){if(keyboardHeld.has(event.code))return;keyboardHeld.add(event.code);}
  else {keyboardHeld.delete(event.code);if([...keyboardHeld].some(physical=>movementKeys[physical]===code))return;}
  key(code,down);
},true);
window.addEventListener('wheel',event=>{
  if(query.get('mime')!=='1'||event.ctrlKey)return;
  event.preventDefault();release();
  parent.postMessage({type:'mime:scroll',deltaY:event.deltaY,deltaMode:event.deltaMode},'*');
},{passive:false});
const touchControls=installTouchControls(canvas,key,()=>started&&playing&&!document.hidden);
function release(){keyboardHeld.clear();touchControls.reset();for(const code of [...held])key(code,false);}
for(const button of document.querySelectorAll('[data-code]')){
  button.onpointerdown=e=>{e.preventDefault();button.setPointerCapture(e.pointerId);key(button.dataset.code,true);};
  button.onpointerup=button.onpointercancel=()=>key(button.dataset.code,false);
  button.onlostpointercapture=()=>key(button.dataset.code,false);
}
window.addEventListener('blur',release);document.addEventListener('visibilitychange',()=>{if(document.hidden)release();});
function fail(message){parent.postMessage({type:'doom:error',message:String(message)},'*');status.textContent=String(message);boot.hidden=false;start.disabled=true;document.body.dataset.state='error';}
window.Module={canvas,noInitialRun:true,noExitRuntime:true,locateFile:path=>new URL('vendor/'+path,location.href).href,
  // Asyncify wakes the engine after I_Sleep and resumes its loop. Gate that
  // next iteration too, so an offscreen card remains stopped.
  preMainLoop:()=>{if(!playing||document.hidden){Module.pauseMainLoop();return false;}},
  print:text=>console.log('[doom]',text),printErr:text=>console.warn('[doom]',text),onAbort:fail,
  preRun:()=>{Module.FS.mkdir('/savefiles');Module.FS.writeFile('/base.wad',base);Module.FS.writeFile('/level.wad',wad);Module.FS.writeFile('/default.cfg',cfg);},
  onRuntimeInitialized:()=>{status.textContent=level.title;start.disabled=false;document.body.dataset.state='ready';if(!preview)window.__mimeReady=true;document.querySelector('#file').disabled=false;if(preview)queueMicrotask(startGame);},onExit:code=>fail('Doom stopped ('+code+')')};
let base;
try{
  if(preview&&query.get('bridge')==='1'){
    await new Promise((resolve,reject)=>{
      const timer=setTimeout(()=>{window.removeEventListener('message',receive);reject(Error('WAD did not arrive from the editor'));},30000);
      function receive(event){if(event.source!==parent||event.data?.type!=='doom:wad')return;
        try{const data=event.data.wad;if(!(data instanceof ArrayBuffer)||data.byteLength>32*1024*1024)throw Error('Invalid editor WAD');loadWad(new Uint8Array(data));level.title=typeof event.data.title==='string'?event.data.title.slice(0,80):'Doom world';download();clearTimeout(timer);window.removeEventListener('message',receive);resolve();}catch(error){clearTimeout(timer);window.removeEventListener('message',receive);reject(error);}
      }
      window.addEventListener('message',receive);parent.postMessage({type:'doom:request'},'*');
    });
  }
  if(query.has('file')){const url=new URL(query.get('file'),location.href);if(!['http:','https:'].includes(url.protocol))throw Error('Invalid WAD URL');const file=await fetch(url);if(!file.ok)throw Error('WAD unavailable');loadWad(new Uint8Array(await file.arrayBuffer()));download();}
  const response=await fetch('./vendor/freedoom'+(game==='doom2'?'2':'1')+'.wad');if(!response.ok)throw Error('Could not load Freedoom');base=new Uint8Array(await response.arrayBuffer());
  const script=document.createElement('script');script.src='./vendor/websockets-doom.js';script.onerror=()=>fail('Could not load the Doom engine');document.body.append(script);
}catch(error){fail(error.message);}
function startGame(){
  if(started)return;started=true;playing=true;start.disabled=true;canvas.focus();
  try{
    const warp=map.startsWith('E')?[map[1],map[3]]:[String(Number(map.slice(3)))];
    window.callMain(['-iwad','base.wad','-file','level.wad','-warp',...warp,'-skill','3','-window','-nogui','-nomusic','-config','default.cfg','-savedir','/savefiles']);
    boot.hidden=true;document.querySelector('#controls').hidden=false;document.body.dataset.state=preview?'booting':'playing';
    requestAnimationFrame(()=>requestAnimationFrame(()=>setTimeout(()=>{
      document.body.dataset.state='playing';window.__mimeReady=true;canvas.setAttribute('aria-label',level.things.length?'Doom room':'Empty Doom room');
      parent.postMessage({type:'doom:ready'},'*');
    },preview?2500:250)));
  }catch(error){started=false;fail(error.message);}
}
start.onclick=startGame;

window.__mimePlayerSettle=play=>{
  const wasPlaying=playing;playing=play;
  if(!started)return;
  if(playing&&!document.hidden){if(!wasPlaying&&!window.Asyncify?.currData)Module.resumeMainLoop();window.SDL2?.audioContext?.resume();}
  else{release();Module.pauseMainLoop();window.SDL2?.audioContext?.suspend();}
};
window.__mimeSettle=window.__mimePlayerSettle;
window.addEventListener('message',event=>{if(event.source===parent&&event.data?.type==='mime:playing')window.__mimeSettle(event.data.playing===true);});
window.addEventListener('message',event=>{
  if(query.get('mime')!=='1'||event.source!==parent||event.data?.type!=='mime:key'||![...Object.keys(movementKeys),'Escape'].includes(event.data.code))return;
  window.dispatchEvent(new KeyboardEvent(event.data.down===false?'keyup':'keydown',{code:event.data.code,repeat:event.data.repeat===true,bubbles:true,cancelable:true}));
});
document.addEventListener('visibilitychange',()=>{if(document.hidden){release();if(started){Module.pauseMainLoop();window.SDL2?.audioContext?.suspend();}}else if(playing&&started){if(!window.Asyncify?.currData)Module.resumeMainLoop();window.SDL2?.audioContext?.resume();}});
