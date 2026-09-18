// Incoming public text/code only. No invented glyphs, reasoning, transcript, or network work.
window.installTokenGrass=anchor=>{
 const canvas=document.createElement('canvas');canvas.id='token-grass';canvas.hidden=true;canvas.setAttribute('aria-hidden','true');document.body.append(canvas);
 const ctx=canvas.getContext('2d'),reduced=matchMedia('(prefers-reduced-motion: reduce)');
 let blades=[],frame=0,lastPaint=0,serial=0;
 function clear(){blades=[];cancelAnimationFrame(frame);frame=0;canvas.hidden=true;}
 function paint(now){
  frame=0;
  if(document.hidden||anchor.hidden||!anchor.isConnected){clear();return;}
  if(now-lastPaint<32){frame=requestAnimationFrame(paint);return;}lastPaint=now;
  blades=blades.filter(b=>now-b.at<1300);
  if(!blades.length){clear();return;}
  const rect=anchor.getBoundingClientRect(),sheet=document.getElementById('conversation').getBoundingClientRect();
  if(rect.bottom<sheet.top||rect.top>sheet.bottom){clear();return;}
  const width=Math.min(160,rect.width+36,innerWidth-20),height=30,dpr=Math.min(devicePixelRatio||1,2);
  canvas.style.left=`${Math.max(10,Math.min(innerWidth-width-10,rect.left-10))}px`;
  canvas.style.top=`${Math.min(sheet.bottom-height,rect.bottom-10)}px`;
  canvas.style.width=`${width}px`;canvas.style.height=`${height}px`;
  if(canvas.width!==Math.round(width*dpr)||canvas.height!==Math.round(height*dpr)){canvas.width=Math.round(width*dpr);canvas.height=Math.round(height*dpr);}
  ctx.setTransform(dpr,0,0,dpr,0,0);ctx.clearRect(0,0,width,height);ctx.font='10px monospace';ctx.textBaseline='bottom';
  for(const b of blades){
   const age=(now-b.at)/1300,x=5+b.lane*(width-12),sway=reduced.matches?0:Math.sin(age*4+b.lane*6)*2;
   const rise=reduced.matches?0:Math.min(1,age*5)*b.height;
   ctx.globalAlpha=Math.max(0,(1-age)*.8);ctx.fillStyle=b.lane>.65?'#baffbf':'#60e58a';
   ctx.fillText(b.glyph,x+sway,height-2-rise);
  }
  ctx.globalAlpha=1;canvas.hidden=false;frame=requestAnimationFrame(paint);
 }
 function receive({delta='',reset=false}={}){
  if(reset)clear();
  if(typeof delta!=='string'||document.hidden||anchor.hidden)return;
  const glyphs=Array.from(delta).filter(c=>!/[\s\x00-\x1f\x7f]/.test(c)).slice(-48),at=performance.now();
  for(const glyph of glyphs){const n=serial++;blades.push({glyph,at,lane:((n*17)%53)/53,height:5+(n%12)});}
  blades=blades.slice(-72);if(blades.length&&!frame)frame=requestAnimationFrame(paint);
 }
 document.addEventListener('visibilitychange',()=>{if(document.hidden)clear();});
 new MutationObserver(()=>{if(anchor.hidden)clear();}).observe(anchor,{attributes:true,attributeFilter:['hidden']});
 reduced.addEventListener('change',()=>{if(blades.length&&!frame)frame=requestAnimationFrame(paint);});
 return {receive,clear};
};
