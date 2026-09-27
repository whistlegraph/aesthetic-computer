// The companion stands above the output horizon; poses follow observable stages.
window.installNotebookDonkey=anchor=>{
 const canvas=document.createElement('canvas');canvas.id='notebook-thinking-donkey';canvas.hidden=true;canvas.setAttribute('aria-hidden','true');document.body.append(canvas);
 const context=canvas.getContext('2d'),reduced=matchMedia('(prefers-reduced-motion: reduce)');
 const sheets={};let timer=null,phase='',started=performance.now();
 const sequences={
  listening:{sheet:'thinking',frames:[0,1,0,7],times:[1100,900,1200,900]},
  thinking:{sheet:'thinking',frames:[0,1,2,3,2,4,5,6,7],times:[900,800,1100,1300,800,950,1000,900,1000]},
  working:{sheet:'running',frames:[0,1,2,3,4,5,6,7],times:[110,110,110,110,110,110,110,110]},
 };
 function stage(){const status=anchor.dataset.status||'';return /connecting|waiting|preparing|Starting|Connecting|Waiting/.test(status)?'listening':/writing|tool|Saving|Running tool|Writing/.test(status)?'working':'thinking';}
 function paint(){
  clearTimeout(timer);timer=null;
  if(anchor.hidden||!anchor.isConnected||document.hidden){canvas.hidden=true;return;}
  if(document.body.classList.contains('window-resizing')){timer=setTimeout(paint,160);return;}
  const next=stage(),now=performance.now();if(next!==phase){phase=next;started=now;}
  const animation=sequences[phase],image=sheets[animation.sheet]||sheets.running;
  if(!image){canvas.hidden=true;return;}
  let index=0,offset=(now-started)%animation.times.reduce((a,b)=>a+b,0);
  if(!reduced.matches)for(let i=0;i<animation.frames.length;i++){index=i;if(offset<animation.times[i])break;offset-=animation.times[i];}
  const frame=animation.frames[index],rect=anchor.getBoundingClientRect();
  const width=Math.max(1,Math.min(rect.width,innerWidth-20)),height=Math.max(1,rect.height),dpr=Math.min(devicePixelRatio||1,2);
  canvas.style.width=`${width}px`;canvas.style.height=`${height}px`;canvas.style.left=`${rect.left}px`;canvas.style.top=`${rect.top}px`;
  const w=Math.round(width*dpr),h=Math.round(height*dpr);if(canvas.width!==w||canvas.height!==h){canvas.width=w;canvas.height=h;}
  const cellW=image.naturalWidth/4,cellH=image.naturalHeight/2;
  const running=image===sheets.running,top=running?cellH*.125:0,cropH=running?cellH*.8125:cellH;
  const scale=Math.min(w/cellW,h/cropH),drawW=cellW*scale,drawH=cropH*scale;
  context.clearRect(0,0,w,h);context.drawImage(image,(frame%4)*cellW,Math.floor(frame/4)*cellH+top,cellW,cropH,(w-drawW)/2,h-drawH,drawW,drawH);
  canvas.dataset.pose=String(frame);canvas.dataset.state=phase;canvas.hidden=false;
  if(!reduced.matches)timer=setTimeout(paint,Math.max(16,animation.times[index]-offset));
 }
 for(const [name,file] of [['running','donkey-pencil-run-v2.png'],['thinking','donkey-pencil-thinking-v1.png']]){
  const image=new Image();image.onload=()=>{sheets[name]=image;paint();};image.src=`assets/${file}`;
 }
 new MutationObserver(paint).observe(anchor,{attributes:true,attributeFilter:['hidden','aria-label','data-status']});
 document.addEventListener('visibilitychange',paint);window.addEventListener('aesel-resize-settled',paint);reduced.addEventListener('change',paint);
 return {paint};
};
