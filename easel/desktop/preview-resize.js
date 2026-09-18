// The picture owns hover enlargement; only its frame owns dragging.
window.installPreviewResize=()=>{
 const shell=document.getElementById('artifact-shell'),artifact=document.getElementById('artifact');
 let drag=null,custom=null,frame=0,leaveTimer=null,compactWidth=null;
 const apply=()=>{
  if(!custom)return;
  const right=Math.max(8,Math.min(innerWidth-104,custom.right)),top=Math.max(8,Math.min(innerHeight-80,custom.top));
  const width=Math.max(96,Math.min(innerWidth-right-8,custom.width)),height=Math.max(72,Math.min(innerHeight-top-40,custom.height));
  const expanded=drag?.expanded??custom.expanded??false;
  custom={width,height,right,top,expanded};
  if(!expanded)compactWidth=width;
  const aspect=(width-12)/(height-12);
  const smallWidth=Math.min(compactWidth||188,innerWidth-right-8,(innerHeight-top-40-12)*aspect+12);
  const largeWidth=expanded?width:Math.min((smallWidth-12)*3+12,innerWidth-right-8,(innerHeight-top-40-12)*aspect+12);
  shell.style.setProperty('--resized-preview-compact-width',`${smallWidth}px`);
  shell.style.setProperty('--resized-preview-compact-height',`${(smallWidth-12)/aspect+12}px`);
  shell.style.setProperty('--resized-preview-drag-width',`${width}px`);
  shell.style.setProperty('--resized-preview-drag-height',`${height}px`);
  window.previewUserDimensions=[128*(width-12)/(height-12),128];window.setPreviewDimensions(...window.previewUserDimensions);
  for(const [key,value]of Object.entries({width:largeWidth,height:(largeWidth-12)/aspect+12,right,top}))shell.style.setProperty('--resized-preview-'+key,`${value}px`);
  shell.dataset.resized='true';
 };
 artifact.addEventListener('pointerenter',()=>{if(drag)return;clearTimeout(leaveTimer);document.body.dataset.previewZone='picture';document.body.dataset.previewEngaged='true';});
 shell.addEventListener('pointerenter',()=>clearTimeout(leaveTimer));
 shell.addEventListener('pointerleave',()=>{if(drag)return;leaveTimer=setTimeout(()=>{if(!drag&&!shell.matches(':hover')){document.body.dataset.previewEngaged='false';document.body.dataset.previewZone='';}},160);});
 for(const direction of ['n','e','s','w','nw','ne','se','sw']){
  const grip=document.createElement('button');grip.type='button';grip.className='preview-resize-edge';grip.dataset.edge=direction;if(direction==='sw')grip.id='preview-resize-grip';grip.setAttribute('aria-label',`Resize preview ${direction}`);grip.title='Drag to reshape preview';shell.append(grip);
  grip.addEventListener('pointerenter',()=>{document.body.dataset.previewZone='resize';});
  grip.addEventListener('pointerdown',event=>{
   if(event.button!==0||document.body.classList.contains('preview-fullscreen')||window.currentPreviewMedium&&window.currentPreviewMedium!=='piece')return;
   event.preventDefault();event.stopPropagation();clearTimeout(leaveTimer);
   const rect=shell.getBoundingClientRect();drag={x:event.clientX,y:event.clientY,width:rect.width,height:rect.height,right:innerWidth-rect.right,top:rect.top,direction,expanded:document.body.dataset.previewEngaged==='true'||document.body.dataset.previewMode==='pinned'};
   custom={width:rect.width,height:rect.height,right:innerWidth-rect.right,top:rect.top,expanded:drag.expanded};apply();
   shell.dataset.resizing='true';grip.setPointerCapture(event.pointerId);
  });
  grip.addEventListener('pointermove',event=>{
   if(!drag)return;const dx=event.clientX-drag.x,dy=event.clientY-drag.y,d=drag.direction;
   custom={width:drag.width+(d.includes('w')?-dx:d.includes('e')?dx:0),height:drag.height+(d.includes('n')?-dy:d.includes('s')?dy:0),right:drag.right-(d.includes('e')?dx:0),top:drag.top+(d.includes('n')?dy:0),expanded:drag.expanded};
   if(!frame)frame=requestAnimationFrame(()=>{frame=0;apply();});
  });
  const end=()=>{if(!drag)return;drag=null;delete shell.dataset.resizing;apply();if(!shell.matches(':hover'))shell.dispatchEvent(new PointerEvent('pointerleave'));};
  for(const name of ['pointerup','pointercancel','lostpointercapture'])grip.addEventListener(name,end);
  grip.addEventListener('keydown',event=>{if(!['ArrowLeft','ArrowRight','ArrowUp','ArrowDown'].includes(event.key))return;event.preventDefault();event.stopPropagation();const rect=shell.getBoundingClientRect();custom={width:rect.width+(event.key==='ArrowLeft'?16:event.key==='ArrowRight'?-16:0),height:rect.height+(event.key==='ArrowDown'?16:event.key==='ArrowUp'?-16:0),right:innerWidth-rect.right,top:rect.top};apply();});
 }
 window.addEventListener('resize',()=>{if(custom)apply();});
};
