(() => {
  const viewport = document.getElementById('preview-viewport');
  const host = document.createElement('div'); host.id = 'media-preview'; host.hidden = true; viewport.append(host);
  let draggingPreview=false,dragSent=false;
  host.addEventListener('pointerdown',()=>{draggingPreview=true;dragSent=false;});
  window.addEventListener('pointermove',event=>{if(draggingPreview&&!dragSent&&(event.clientX<=0||event.clientY<=0||event.clientX>=innerWidth-1||event.clientY>=innerHeight-1)){dragSent=true;window.aesel.dragPreview?.();}});
  window.addEventListener('pointerup',()=>{draggingPreview=false;dragSent=false;});
  let key='', player=null, playerFrame=0, emulator=null, generation=0;
  const ready = () => { const box=document.getElementById('artifact'); box.classList.remove('refresh'); requestAnimationFrame(()=>box.classList.add('refresh')); };
  window.renderMediaPreview = async state => {
    if (state.medium === 'piece') {
      ++generation;
      host.hidden=true; document.getElementById('piece').hidden=false;
      if(player)player.pause();cancelAnimationFrame(playerFrame);playerFrame=0;if(emulator)await emulator.pause(); key=''; return;
    }
    const next=JSON.stringify([state.medium,state.preview?.artifactId,state.preview?.version]);
    if(next===key)return; key=next; const current=++generation;
    host.hidden=false; document.getElementById('piece').hidden=true;
    // Paper previews are pages, not generic media cards. US Letter points keep
    // the compact, expanded and fullscreen shells at the document's real
    // portrait ratio even before PDF metadata arrives (or when TeX is absent).
    window.setPreviewDimensions(state.medium==='paper'?612:192,state.medium==='paper'?792:128);
    if(player)player.pause();cancelAnimationFrame(playerFrame);playerFrame=0;if(emulator)await emulator.pause(); host.replaceChildren();
    const preview=state.localPreview;
    if(!preview){host.textContent=state.previewError||'No preview yet';return;}
    if(preview.width && preview.height) window.setPreviewDimensions(preview.width,preview.height);
    try {
      if(preview.mime==='image/png') {
        const img=new Image();img.alt=state.piece||'Picture';img.draggable=false;img.onload=()=>{if(current!==generation)return;window.setPreviewDimensions(img.naturalWidth,img.naturalHeight);ready();};img.src=preview.data;host.append(img);
      } else if(preview.mime==='audio/wav') {
        player=new Audio(preview.data);const audio=player;
        const canvas=document.createElement('canvas');canvas.className='tape-player';canvas.width=192;canvas.height=128;canvas.tabIndex=0;canvas.setAttribute('role','button');canvas.setAttribute('aria-label','Play sound; drag to scrub; drag beyond the Easel window to export');host.append(canvas);
        const context=new OfflineAudioContext(1,1,24000);
        const buffer=await context.decodeAudioData(await(await fetch(preview.data)).arrayBuffer());
        if(current!==generation)return;
        const samples=buffer.getChannelData(0),ctx=canvas.getContext('2d'),peaks=[];
        for(let x=0;x<192;x++){let peak=0;for(let i=Math.floor(x*samples.length/192);i<Math.floor((x+1)*samples.length/192);i++)peak=Math.max(peak,Math.abs(samples[i]));peaks.push(peak);}
        const draw=()=>{cancelAnimationFrame(playerFrame);playerFrame=0;if(current!==generation)return;const progress=audio.duration?audio.currentTime/audio.duration:0;
          ctx.fillStyle='#07182c';ctx.fillRect(0,0,192,128);
          ctx.fillStyle='#ffc80033';for(let x=0;x<192;x++)ctx.fillRect(x,64-peaks[x]*52,1,Math.max(1,peaks[x]*104));
          ctx.fillStyle='#ffc800aa';for(let x=0;x<Math.floor(progress*192);x++)ctx.fillRect(x,64-peaks[x]*52,1,Math.max(1,peaks[x]*104));
          ctx.fillStyle='#3c4b5fdd';ctx.fillRect(151,5,36,13);ctx.strokeStyle='#6e829f';ctx.strokeRect(151.5,5.5,35,12);ctx.fillStyle='#ffff00';ctx.font='8px monospace';ctx.textAlign='center';ctx.fillText(audio.paused?'0.00x':'1.00x',169,14);
          if(audio.paused){ctx.fillStyle='#ffffffdd';ctx.beginPath();ctx.moveTo(88,48);ctx.lineTo(88,80);ctx.lineTo(113,64);ctx.closePath();ctx.fill();}
          ctx.fillStyle='#ffffff2d';ctx.fillRect(0,125,192,3);ctx.fillStyle='#ff3344';ctx.fillRect(0,125,Math.max(1,Math.floor(progress*192)),3);ctx.fillStyle='#fff';ctx.fillRect(Math.min(188,Math.floor(progress*188)),123,4,5);
          if(!audio.paused)playerFrame=requestAnimationFrame(draw);
        };
        const toggle=async()=>{if(audio.paused)await audio.play();else audio.pause();};
        let down=0,moved=false;
        canvas.onpointerdown=e=>{down=e.clientX;moved=false;canvas.setPointerCapture(e.pointerId);};
        canvas.onpointermove=e=>{if(!canvas.hasPointerCapture(e.pointerId))return;if(Math.abs(e.clientX-down)>3)moved=true;if(moved&&audio.duration){const box=canvas.getBoundingClientRect();audio.currentTime=Math.max(0,Math.min(1,(e.clientX-box.left)/box.width))*audio.duration;draw();}};
        canvas.onpointerup=e=>{canvas.releasePointerCapture(e.pointerId);if(!moved)toggle();};
        canvas.onkeydown=e=>{if(e.key===' '||e.key==='Enter'){e.preventDefault();toggle();}else if((e.key==='ArrowLeft'||e.key==='ArrowRight')&&audio.duration){e.preventDefault();audio.currentTime=Math.max(0,Math.min(audio.duration,audio.currentTime+(e.key==='ArrowLeft'?-1:1)*audio.duration*.05));draw();}};
        audio.onplay=draw;audio.onpause=draw;audio.onended=draw;draw();ready();
      } else if(preview.mime==='application/pdf') {
        window.setPreviewDimensions(612,792);
        const frame=document.createElement('iframe');frame.title='Paper preview';frame.src=preview.data+'#page=1&zoom=page-fit&toolbar=0&navpanes=0';frame.onload=ready;host.append(frame);
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
