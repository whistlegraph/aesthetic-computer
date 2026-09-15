(() => {
  const canvas=document.getElementById('screen'),status=document.getElementById('game-status'),pause=document.getElementById('pause');
  const emulator=window.WasmBoy?.WasmBoy,held={};let loaded=false,playing=false;
  const update=()=>{if(loaded)emulator.setJoypadState({...held});};
  const release=()=>{for(const key of Object.keys(held))held[key]=false;update();};
  const keys={ArrowUp:'UP',w:'UP',W:'UP',ArrowDown:'DOWN',s:'DOWN',S:'DOWN',ArrowLeft:'LEFT',a:'LEFT',A:'LEFT',ArrowRight:'RIGHT',d:'RIGHT',D:'RIGHT',' ':'A',x:'A',X:'A',Enter:'B',z:'B',Z:'B',p:'START',P:'START',Shift:'SELECT'};
  for(const name of ['keydown','keyup'])canvas.addEventListener(name,event=>{const key=keys[event.key];if(!key)return;event.preventDefault();held[key]=name==='keydown';update();});
  canvas.addEventListener('blur',release);
  for(const button of document.querySelectorAll('[data-key]')){
    button.addEventListener('pointerdown',event=>{event.preventDefault();button.setPointerCapture(event.pointerId);held[button.dataset.key]=true;update();});
    for(const name of ['pointerup','pointercancel','lostpointercapture'])button.addEventListener(name,()=>{held[button.dataset.key]=false;update();});
    button.addEventListener('keydown',event=>{if(event.key===' '||event.key==='Enter'){event.preventDefault();held[button.dataset.key]=true;update();}});
    button.addEventListener('keyup',event=>{if(event.key===' '||event.key==='Enter'){event.preventDefault();held[button.dataset.key]=false;update();}});
    button.addEventListener('blur',release);
  }
  pause.addEventListener('click',async()=>{try{if(playing){await emulator.pause();playing=false;pause.textContent='Play';}else{await emulator.play();playing=true;pause.textContent='Pause';canvas.focus();}}catch{status.textContent='Could not resume Game Boy';}});
  const receive=async event=>{
    if(event.source!==parent||event.data?.type!=='easel-gameboy-rom')return;
    window.removeEventListener('message',receive);
    const bytes=event.data.bytes;
    if(!(bytes instanceof ArrayBuffer)||bytes.byteLength<32768||bytes.byteLength>8*1024*1024){status.textContent='This ROM cannot be previewed.';return;}
    if(!emulator){status.textContent='Game Boy emulator could not load.';return;}
    let timer,failed=false;
    try{
      let firstFrame;const first=new Promise((resolve,reject)=>{firstFrame=resolve;timer=setTimeout(()=>reject(new Error('The ROM has not produced a frame.')),10000);});first.catch(()=>{});
      const initialize=async()=>{
        await emulator.config({headless:false,isAudioEnabled:false,enableBootROMIfAvailable:false,updateGraphicsCallback:pixels=>{if(pixels?.length===160*144*4)firstFrame();}},canvas);
        if(failed)return;await emulator.loadROM(new Uint8Array(bytes));if(failed)return;emulator.disableDefaultJoypad();loaded=true;await emulator.play();playing=true;if(failed){await emulator.pause();return;}await first;
      };
      await Promise.race([initialize(),first]);
      status.textContent='WASD / arrows · Space: A · Enter: B · P: Start · Shift: Select — sound off';pause.hidden=false;
    }catch(error){failed=true;status.textContent=error.message||'Game Boy could not start.';await emulator.pause().catch(()=>{});}
    finally{clearTimeout(timer);}
  };
  window.addEventListener('message',receive);
  document.addEventListener('visibilitychange',()=>{if(document.hidden&&loaded){release();emulator.pause();playing=false;pause.textContent='Play';}});
  window.addEventListener('pagehide',()=>{if(loaded)emulator.pause();});
  parent.postMessage({type:'easel-gameboy-ready'},'*');
})();
