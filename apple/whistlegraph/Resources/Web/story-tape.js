// Injected into the piece frame with the shared AC canvas-tape encoder.
(() => {
  if (window === window.top || location.origin !== 'https://aesthetic.computer') return;
  let recorder, timer, session, state={}, delivery=Promise.resolve(), failure=null;
  const post = body => window.webkit.messageHandlers.walkie.postMessage({action:'storyTape',session,...body});
  const canvas=document.createElement('canvas'); canvas.width=1080;canvas.height=1920;
  const ctx=canvas.getContext('2d');
  function draw() {
    ctx.fillStyle='black';ctx.fillRect(0,0,1080,1920);ctx.imageSmoothingEnabled=false;
    const layers=[document.querySelector('canvas[data-ac-visit-canvas]'), ...document.querySelectorAll('canvas[data-type="webgl-composite"],canvas[data-type="webgpu"],canvas[data-type="hd"]')];
    let drew=false;
    for(const layer of layers) if(layer?.width && getComputedStyle(layer).display!=='none') { ctx.drawImage(layer,76,270,928,696);drew=true; }
    if(!drew)throw Error('The piece canvas is unavailable.');
    ctx.fillStyle='#aaa';ctx.font='40px monospace';ctx.fillText('v'+(state.version??''),76,1030);
    ctx.fillStyle='white';ctx.font='58px WhistlegraphComic, sans-serif';
    const words=String(state.caption||'').split(/\s+/);let line='',y=1120;
    for(const word of words){const next=line?line+' '+word:word;if(ctx.measureText(next).width>928&&line){ctx.fillText(line,76,y);y+=72;line=word;if(y>1320)break;}else line=next;}
    if(y<=1320)ctx.fillText(line,76,y);
  }
  window.whistlegraphStoryTape={
    update(value){state=value;},
    async start(id){
      await window.whistlegraphStoryFont;
      if(recorder)throw Error('A tape is already exporting.');session=id;failure=null;delivery=Promise.resolve();
      draw();recorder=createCanvasTapeRecorder(canvas,{fps:30,mp4Only:true});
      recorder.ondataavailable=e=>{if(!e.data.size)return;delivery=delivery.then(async()=>{
        const bytes=new Uint8Array(await e.data.arrayBuffer());
        for(let i=0;i<bytes.length;i+=192000){let s='';for(const b of bytes.subarray(i,i+192000))s+=String.fromCharCode(b);post({kind:'chunk',data:btoa(s)});}
      }).catch(error=>{failure=error;});};
      recorder.onerror=e=>{failure=e.error||Error('Canvas tape encoding failed');};
      recorder.onstop=()=>{clearInterval(timer);delivery.then(()=>post(failure?{kind:'error',error:failure.message}:{kind:'done'}));recorder=null;};
      timer=setInterval(()=>{try{draw();}catch(error){failure=error;clearInterval(timer);if(recorder?.state!=='inactive')recorder.stop();}},1000/30);
      recorder.start(1000);
    },
    stop(){if(recorder?.state!=='inactive')recorder?.stop();},
    cancel(){clearInterval(timer);if(recorder){recorder.ondataavailable=null;recorder.onstop=null;if(recorder.state!=='inactive')recorder.stop();recorder=null;}session=null;}
  };
})();
