// A small pencil companion behind the ink. The anchor reserves space beside the latest text.
window.installNotebookDonkey=anchor=>{
 const canvas=document.createElement('canvas');canvas.id='notebook-thinking-donkey';canvas.hidden=true;canvas.setAttribute('aria-hidden','true');document.body.append(canvas);
 const context=canvas.getContext('2d');const source=new Image();const reduced=matchMedia('(prefers-reduced-motion: reduce)');
 let boxes=[],frame=0,timer=null,loaded=false;
 const active=()=>loaded&&!anchor.hidden&&anchor.isConnected&&!document.hidden;
 function paint(){
  clearTimeout(timer);timer=null;if(!active()){canvas.hidden=true;return;}
  const rect=anchor.getBoundingClientRect(),sheet=document.getElementById('conversation').getBoundingClientRect();
  if(rect.bottom<sheet.top||rect.top>sheet.bottom){canvas.hidden=true;return;}
  const size=Math.max(1,rect.height),width=Math.min(rect.width-16,innerWidth-24);
  canvas.style.width=`${width}px`;canvas.style.height=`${size}px`;
  canvas.style.left=`${Math.max(10,Math.min(innerWidth-width-8,rect.left+8))}px`;
  canvas.style.top=`${Math.max(sheet.top,rect.top+(rect.height-size)/2)}px`;
  const dpr=Math.min(devicePixelRatio||1,2),pixels=Math.round(size*dpr),pixelWidth=Math.round(width*dpr);if(canvas.width!==pixelWidth||canvas.height!==pixels){canvas.width=pixelWidth;canvas.height=pixels;}
  const box=boxes[reduced.matches?0:Math.floor(performance.now()/90)%8],scale=Math.min(pixelWidth/box.width,pixels/box.height);
  context.clearRect(0,0,pixelWidth,pixels);context.drawImage(source,box.x,box.y,box.width,box.height,(pixelWidth-box.width*scale)/2,(pixels-box.height*scale)/2,box.width*scale,box.height*scale);
  canvas.hidden=false;if(!reduced.matches){timer=setTimeout(paint,90);}
 }
 source.onload=()=>{
  // Shared alpha bounds measured across the eight-frame 4×2 sheet.
  // Keep image reads out of the renderer: file:// canvases can be tainted.
  const cell=source.naturalWidth/4;
  boxes=Array.from({length:8},(_,i)=>({x:(i%4)*cell,y:Math.floor(i/4)*source.naturalHeight/2+64,width:cell,height:416}));loaded=true;paint();
 };
 source.src='assets/donkey-pencil-run-v2.png';
 new MutationObserver(paint).observe(anchor,{attributes:true,attributeFilter:['hidden','aria-label']});
 new MutationObserver(paint).observe(document.getElementById('notebook-page'),{childList:true,subtree:true});
 document.addEventListener('scroll',paint,true);document.addEventListener('visibilitychange',paint);window.addEventListener('resize',paint);reduced.addEventListener('change',paint);
};
