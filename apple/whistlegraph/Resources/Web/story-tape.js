// Injected into the piece frame with the shared AC canvas-tape encoder.
(() => {
  if (window === window.top || location.origin !== 'https://aesthetic.computer') return;
  let capture=null, state={};
  const post = (session,body) => window.webkit.messageHandlers.walkie.postMessage({action:'storyTape',session,...body});
  const canvas=document.createElement('canvas'); canvas.width=1080;canvas.height=1920;
  const ctx=canvas.getContext('2d');
  function draw() {
    ctx.fillStyle=state.background||'rgb(58,38,89)';ctx.fillRect(0,0,1080,1920);ctx.imageSmoothingEnabled=false;
    const layers=[document.querySelector('canvas[data-ac-visit-canvas]'), ...document.querySelectorAll('canvas[data-type="webgl-composite"],canvas[data-type="webgpu"],canvas[data-type="hd"]')];
    let drew=false;
    for(const layer of layers) if(layer?.width && getComputedStyle(layer).display!=='none') { ctx.drawImage(layer,76,270,928,696);drew=true; }
    if(!drew)throw Error('The piece canvas is unavailable.');
    const version='v'+(state.version??'');
    ctx.font='700 58px WhistlegraphComicBold, sans-serif';
    ctx.fillStyle='#00ffff';ctx.fillText(version,82,1036);
    ctx.fillStyle='#ff2d55';ctx.fillText(version,79,1033);
    ctx.fillStyle='#141414';
    for(let i=0;i<8;i++)ctx.fillText(version,76+Math.cos(i*Math.PI/4)*2.7,1030+Math.sin(i*Math.PI/4)*2.7);
    ctx.fillStyle='#fff5e8';ctx.fillText(version,76,1030);
    ctx.fillStyle='white';ctx.font='58px WhistlegraphComic, sans-serif';
    const words=String(state.caption||'').split(/\s+/);let line='',y=1120;
    for(const word of words){const next=line?line+' '+word:word;if(ctx.measureText(next).width>928&&line){ctx.fillText(line,76,y);y+=72;line=word;if(y>1320)break;}else line=next;}
    if(y<=1320)ctx.fillText(line,76,y);
  }
  window.whistlegraphStoryTape={
    update(value){state=value;},
    async start(id){
      if(capture)throw Error('A tape is already exporting.');
      const current={id,recorder:null,timer:null,delivery:Promise.resolve(),failure:null};capture=current;
      try {
        await window.whistlegraphStoryFont;
        if(capture!==current)return;
        draw();const recorder=current.recorder=createCanvasTapeRecorder(canvas,{fps:30,mp4Only:true});
        recorder.ondataavailable=e=>{if(!e.data.size)return;current.delivery=current.delivery.then(async()=>{
          if(capture!==current)return;
          const bytes=new Uint8Array(await e.data.arrayBuffer());
          for(let i=0;i<bytes.length&&capture===current;i+=192000){let s='';for(const b of bytes.subarray(i,i+192000))s+=String.fromCharCode(b);post(id,{kind:'chunk',data:btoa(s)});}
        }).catch(error=>{current.failure=error;});};
        recorder.onerror=e=>{current.failure=e.error||Error('Canvas tape encoding failed');};
        recorder.onstop=()=>{clearInterval(current.timer);current.delivery.then(()=>{
          if(capture!==current)return;capture=null;
          post(id,current.failure?{kind:'error',error:current.failure.message}:{kind:'done'});
        });};
        current.timer=setInterval(()=>{try{draw();}catch(error){current.failure=error;clearInterval(current.timer);if(recorder.state!=='inactive')recorder.stop();}},1000/30);
        recorder.start(1000);
      } catch(error) { if(capture===current){window.whistlegraphStoryTape.cancel();throw error;} }
    },
    pause(){if(capture?.recorder?.state==='recording')capture.recorder.pause();},
    resume(){if(capture?.recorder?.state==='paused')capture.recorder.resume();},
    stop(){if(capture?.recorder?.state!=='inactive')capture?.recorder?.stop();},
    cancel(){const current=capture;capture=null;if(!current)return;clearInterval(current.timer);const recorder=current.recorder;if(recorder){recorder.ondataavailable=null;recorder.onstop=null;if(recorder.state!=='inactive')recorder.stop();}}
  };
})();
