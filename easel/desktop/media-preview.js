(() => {
  const viewport = document.getElementById('preview-viewport');
  const host = document.createElement('div'); host.id = 'media-preview'; host.hidden = true; viewport.append(host);
  let key='', player=null, emulator=null, generation=0;
  const ready = () => { const box=document.getElementById('artifact'); box.classList.remove('refresh'); requestAnimationFrame(()=>box.classList.add('refresh')); };
  window.renderMediaPreview = async state => {
    if (state.medium === 'piece') {
      ++generation;
      host.hidden=true; document.getElementById('piece').hidden=false;
      if(player)player.pause(); if(emulator)await emulator.pause(); key=''; return;
    }
    const next=JSON.stringify([state.medium,state.preview?.artifactId,state.preview?.version]);
    if(next===key)return; key=next; const current=++generation;
    host.hidden=false; document.getElementById('piece').hidden=true;
    window.setPreviewDimensions(192,128);
    if(player)player.pause(); if(emulator)await emulator.pause(); host.replaceChildren();
    const preview=state.localPreview;
    if(!preview){host.textContent=state.previewError||'No preview yet';return;}
    if(preview.width && preview.height) window.setPreviewDimensions(preview.width,preview.height);
    try {
      if(preview.mime==='image/png') {
        const img=new Image();img.alt=state.piece||'Picture';img.onload=()=>{if(current!==generation)return;window.setPreviewDimensions(img.naturalWidth,img.naturalHeight);ready();};img.src=preview.data;host.append(img);
      } else if(preview.mime==='audio/wav') {
        player=new Audio(preview.data);const audio=player;
        const canvas=document.createElement('canvas');canvas.width=192;canvas.height=90;
        const play=document.createElement('button');play.textContent='Play';play.onclick=async()=>{if(audio.paused){await audio.play();play.textContent='Pause';}else{audio.pause();play.textContent='Play';}};
        audio.onended=()=>play.textContent='Play';host.append(canvas,play);
        const context=new OfflineAudioContext(1,1,24000);
        const buffer=await context.decodeAudioData(await(await fetch(preview.data)).arrayBuffer());
        if(current!==generation)return;
        const samples=buffer.getChannelData(0),ctx=canvas.getContext('2d');ctx.fillStyle='#ff64ff';
        for(let x=0;x<192;x++){let peak=0;for(let i=Math.floor(x*samples.length/192);i<Math.floor((x+1)*samples.length/192);i++)peak=Math.max(peak,Math.abs(samples[i]));ctx.fillRect(x,45-peak*42,1,Math.max(1,peak*84));}ready();
      } else if(preview.mime==='application/pdf') {
        const frame=document.createElement('iframe');frame.title='Paper preview';frame.src=preview.data+'#view=Fit&toolbar=0&navpanes=0';frame.onload=ready;host.append(frame);
      } else if(state.medium==='gameboy' && preview.mime!=='text/plain') {
        // Same bundled WasmBoy TS core and graphics callback used by AC bios.
        if(!window.WasmBoy){await new Promise((resolve,reject)=>{const script=document.createElement('script');script.src='vendor/wasmboy/wasmboy.js';script.onload=resolve;script.onerror=()=>reject(new Error('Could not load the bundled Game Boy emulator.'));document.head.append(script);});}
        if(current!==generation||host.hidden)return;
        emulator=window.WasmBoy.WasmBoy;
        const activeEmulator=emulator;
        window.setPreviewDimensions(160,144);
        const canvas=document.createElement('canvas');canvas.width=160;canvas.height=144;canvas.tabIndex=0;canvas.title='WASD / arrows: D-pad; Space / X: A; Enter / Z: B; P: Start; Shift: Select';canvas.dataset.status='loading';canvas.dataset.frames='0';host.append(canvas);
        const isCurrent=()=>current===generation&&!host.hidden&&host.contains(canvas);
        let frames=0,timer;
        const firstFrame=new Promise((resolve,reject)=>{timer=setTimeout(()=>reject(new Error('Game Boy did not produce a frame within 10 seconds.')),10000);canvas._resolveFirstFrame=resolve;});
        // Attach immediately so an init failure cannot leave a rejected promise unhandled.
        firstFrame.catch(()=>{});
        try {
          const initialize=async()=>{
            await activeEmulator.config({headless:false,isAudioEnabled:false,disablePauseOnHidden:true,enableBootROMIfAvailable:false,
              updateGraphicsCallback:pixels=>{
                if(!isCurrent()||pixels?.length!==160*144*4)return;
                canvas.dataset.frames=String(++frames);
                if(frames===1){canvas.dataset.status='ready';canvas._resolveFirstFrame();ready();}
              }},canvas);
            if(!isCurrent())return;
            const bytes=new Uint8Array(await(await fetch(preview.data)).arrayBuffer());
            if(!isCurrent())return;
            await activeEmulator.loadROM(bytes);
            // loadROM initializes (and re-enables) WasmBoy default controls.
            // Disable them after loading so they cannot overwrite our held keys.
            activeEmulator.disableDefaultJoypad();
            if(!isCurrent())return;
            await activeEmulator.play();
          };
          await Promise.race([initialize().then(()=>firstFrame),firstFrame]);
        }catch(error){if(!isCurrent())return;throw new Error(error?.message||'Game Boy emulator failed to initialize.');}
        finally{clearTimeout(timer);delete canvas._resolveFirstFrame;}
        if(!isCurrent())return;
        const keys={ArrowUp:'UP',w:'UP',W:'UP',ArrowDown:'DOWN',s:'DOWN',S:'DOWN',ArrowLeft:'LEFT',a:'LEFT',A:'LEFT',ArrowRight:'RIGHT',d:'RIGHT',D:'RIGHT',' ':'A',x:'A',X:'A',Enter:'B',z:'B',Z:'B',p:'START',P:'START',Shift:'SELECT'},held={};
        const update=e=>{const k=keys[e.key];if(k){e.preventDefault();e.stopPropagation();held[k]=e.type==='keydown';activeEmulator.setJoypadState(held);}};
        canvas.addEventListener('keydown',update);canvas.addEventListener('keyup',update);canvas.onblur=()=>{for(const k of Object.keys(held))held[k]=false;activeEmulator.setJoypadState(held);};
      } else {const text=document.createElement('pre');text.textContent=preview.text||'Build the artifact to preview it.';host.append(text);ready();}
    }catch(error){host.textContent=`Preview: ${error.message||'could not load'}`;}
  };
})();
