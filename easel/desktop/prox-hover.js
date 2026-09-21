// Match Prox: spring to 1.5x and retime letter sway without resetting its phase.
window.installProxHover=(host,label=host)=>{
 if(!host||!label)return;
 let hovered=false,focused=false,animation=null;
 const reduced=matchMedia('(prefers-reduced-motion: reduce)');
 const retime=()=>{const speed=(hovered||focused)&&!reduced.matches?2.2:1;for(const ink of label.querySelectorAll('.qr-letter-ink'))for(const motion of ink.getAnimations())motion.updatePlaybackRate(speed);};
 const update=(animate=true)=>{
  const limit=Number(host.dataset.proxMaxScale);
  const maximum=Number.isFinite(limit)&&limit>=0&&host.hasAttribute('data-prox-max-scale')?limit:Infinity;
  const to=Math.min(maximum,(hovered||focused)&&!reduced.matches?1.5:1);
  const transform=getComputedStyle(host).transform;
  const from=transform==='none'?1:new DOMMatrixReadOnly(transform).a;
  animation?.cancel();host.style.transform=`scale(${to})`;
  if(animate&&!reduced.matches){
   const frames=Array.from({length:49},(_,i)=>{const t=i/48*.8;const value=to+(from-to)*Math.exp(-7.5*t)*(Math.cos(16*t)+7.5/16*Math.sin(16*t));return{transform:`scale(${Math.min(maximum,i===48?to:value)})`,offset:i/48};});
   animation=host.animate(frames,{duration:800,easing:'linear'});
  }
  retime();
 };
 host.addEventListener('pointerenter',()=>{hovered=true;update();});host.addEventListener('pointerleave',()=>{hovered=false;update();});
 host.addEventListener('focus',()=>{focused=host.matches(':focus-visible');update();});host.addEventListener('blur',()=>{focused=false;update();});
 // Resize/title changes must fit immediately, including during a hover spring.
 host.addEventListener('prox-bounds-change',()=>update(false));
 reduced.addEventListener('change',()=>update(false));new MutationObserver(retime).observe(label,{childList:true,subtree:true});
 update(false);
};
