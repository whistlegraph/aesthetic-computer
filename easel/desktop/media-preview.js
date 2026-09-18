(() => {
  const viewport = document.getElementById('preview-viewport');
  const host = document.createElement('div'); host.id = 'media-preview'; host.hidden = true; viewport.append(host);
  let draggingPreview=false,dragSent=false;
  host.addEventListener('pointerdown',()=>{draggingPreview=true;dragSent=false;});
  window.addEventListener('pointermove',event=>{if(draggingPreview&&!dragSent&&(event.clientX<=0||event.clientY<=0||event.clientX>=innerWidth-1||event.clientY>=innerHeight-1)){dragSent=true;window.aesel.dragPreview?.();}});
  window.addEventListener('pointerup',()=>{draggingPreview=false;dragSent=false;});
  let key='', player=null, playerFrame=0, emulator=null, generation=0;
  const ready = () => { const box=document.getElementById('artifact'); box.classList.remove('refresh'); requestAnimationFrame(()=>box.classList.add('refresh')); };
  // The shared music clock (net-clock.js). Main measures the offset against
  // /api/clock; here it is adopted and beats are counted from it, never from
  // Date.now or the AudioContext alone. Loaded like the emulator: this file is
  // wired in index.html, the clock rides along with it.
  const netClock=new Promise((resolve,reject)=>{const script=document.createElement('script');script.src='net-clock.js';script.onload=()=>resolve(window.NetClock);script.onerror=()=>reject(new Error('Could not load the net clock.'));document.head.append(script);});
  let clock=null,syncedAt=0;
  const syncClock=async(force)=>{if(!clock||(!force&&performance.now()-syncedAt<5000))return;syncedAt=performance.now();try{const s=await window.aesel.clock?.();if(s)clock.adopt(s.offset);}catch{}};
  netClock.then(NetClock=>{clock=NetClock.createClock();window.aeselClock=clock;}).catch(()=>{});
  // One AudioContext for every tape; 24 kHz matches the rendered WAV so the
  // decoded samples are the file's samples.
  let deckContext=null;const audioContext=()=>deckContext||(deckContext=new AudioContext({sampleRate:24000}));
  // A tape deck over Web Audio. A loop starts at the phase the shared clock
  // says the loop is at right now: clock.time() and context.currentTime are
  // read as a pair, then everything is scheduled on the context's timeline. Two
  // machines with the same score wrap on the same beat. A one-shot phrase waits
  // for the next shared beat, like a sync button.
  const tapeDeck=(NetClock,buffer,grid)=>{
    const context=audioContext(),loopSec=grid?.loop?buffer.duration:0;
    let source=null,anchor=0,head=0; // anchor: the context time where tape position 0 sits
    const stop=()=>{if(source){source.onended=null;try{source.stop();}catch{}source=null;}};
    const position=()=>{const t=context.currentTime-anchor;return loopSec?((t%loopSec)+loopSec)%loopSec:Math.min(Math.max(0,t),buffer.duration);};
    const start=offset=>{
      const lead=.05,at=clock.time()+lead*1000;let when=context.currentTime+lead;
      if(grid&&loopSec)offset=NetClock.loop(at,grid).phase*loopSec;
      else if(grid)when+=(NetClock.nextBeat(at,grid.bpm)-at)/1000;
      source=context.createBufferSource();source.buffer=buffer;source.loop=!!loopSec;
      source.onended=()=>{source=null;head=0;deck.paused=true;deck.onended?.();};
      source.connect(context.destination);source.start(when,offset);anchor=when-offset;
      if(grid)console.log(`🕰️ deck ${loopSec?'loop':'phrase'} on shared beat ${NetClock.beat(at,grid.bpm).index} (offset ${Math.round(clock.offset)}ms, rtt ${Math.round(clock.rtt)}ms)`);
    };
    const deck={
      paused:true,loop:!!loopSec,duration:buffer.duration,onplay:null,onpause:null,onended:null,
      get currentTime(){return deck.paused?head:position();},
      set currentTime(v){head=Math.max(0,Math.min(buffer.duration,v));if(!deck.paused&&!loopSec){stop();start(head);}}, // a loop stays locked
      async play(){if(!deck.paused)return;await context.resume();await syncClock(true);start(head);deck.paused=false;deck.onplay?.();},
      pause(){if(deck.paused)return;head=position();stop();deck.paused=true;deck.onpause?.();},
      // A resync or the buffer's sample rounding can walk a running loop off the grid; snap it back.
      relock(){if(deck.paused||!loopSec)return;const drift=position()-NetClock.loop(clock.time(),grid).phase*loopSec;if(Math.abs(drift)>.02&&Math.abs(drift)<loopSec-.02){stop();start();}},
      // The beat under the head, for the readout.
      beat(){return loopSec?NetClock.loop(clock.time(),grid).beat:Math.floor(deck.currentTime*grid.bpm/60);},
    };
    return deck;
  };
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
        const NetClock=await netClock;
        const buffer=await audioContext().decodeAudioData(await(await fetch(preview.data)).arrayBuffer());
        if(current!==generation)return;
        const grid=preview.grid||null,audio=tapeDeck(NetClock,buffer,grid);player=audio;void syncClock();
        const canvas=document.createElement('canvas');canvas.className='tape-player';canvas.width=192;canvas.height=128;canvas.tabIndex=0;canvas.setAttribute('role','button');canvas.setAttribute('aria-label',`Play sound; drag to scrub${audio.loop?' (a loop stays locked to the shared beat)':''}; drag beyond the Easel window to export`);host.append(canvas);
        const samples=buffer.getChannelData(0),ctx=canvas.getContext('2d'),peaks=[];
        for(let x=0;x<192;x++){let peak=0;for(let i=Math.floor(x*samples.length/192);i<Math.floor((x+1)*samples.length/192);i++)peak=Math.max(peak,Math.abs(samples[i]));peaks.push(peak);}
        const draw=()=>{cancelAnimationFrame(playerFrame);playerFrame=0;if(current!==generation)return;audio.relock();void syncClock();const progress=audio.duration?audio.currentTime/audio.duration:0;
          ctx.fillStyle='#07182c';ctx.fillRect(0,0,192,128);
          if(grid){ctx.fillStyle='#ffffff14';for(let b=1;b<grid.beats;b++)ctx.fillRect(Math.round(b/grid.beats*192),0,1,123);}
          ctx.fillStyle='#ffc80033';for(let x=0;x<192;x++)ctx.fillRect(x,64-peaks[x]*52,1,Math.max(1,peaks[x]*104));
          ctx.fillStyle='#ffc800aa';for(let x=0;x<Math.floor(progress*192);x++)ctx.fillRect(x,64-peaks[x]*52,1,Math.max(1,peaks[x]*104));
          ctx.fillStyle='#3c4b5fdd';ctx.fillRect(151,5,36,13);ctx.strokeStyle='#6e829f';ctx.strokeRect(151.5,5.5,35,12);ctx.fillStyle='#ffff00';ctx.font='8px monospace';ctx.textAlign='center';
          if(grid){const beat=audio.beat();canvas.dataset.beat=String(beat);canvas.dataset.head=String(Math.round(audio.currentTime*1000));ctx.fillText(audio.paused?`${grid.bpm}bpm`:`${beat+1}/${grid.beats}`,169,14);}else ctx.fillText(audio.paused?'0.00x':'1.00x',169,14);
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
