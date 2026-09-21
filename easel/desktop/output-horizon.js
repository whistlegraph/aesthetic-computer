// Public output only: one bounded, selectable line that follows arriving text.
window.installOutputHorizon=({caption,text,node})=>{
 let active=false,output='',fallback='',timer=null,follow=true,writing=false;
 function paint(){
  timer=null;
  const value=active?(output||fallback):'';
  caption.hidden=!active;node.data=value;
  if(follow){writing=true;text.scrollLeft=text.scrollWidth;requestAnimationFrame(()=>{writing=false;});}
 }
 function schedule(){if(timer===null)timer=setTimeout(paint,90);}
 text.tabIndex=0;text.setAttribute('aria-label','Intermediate output');
 text.addEventListener('scroll',()=>{if(!writing)follow=text.scrollWidth-text.clientWidth-text.scrollLeft<8;},{passive:true});
 text.addEventListener('keydown',event=>{if(event.key==='End'){follow=true;paint();}else if(event.key==='Home'||event.key==='ArrowLeft')follow=false;});
 const flat=value=>String(value).replace(/[\x00-\x08\x0b-\x1f\x7f]/g,'').replace(/\s+/g,' ');
 return {
  update({visible,activity=''}){
   const next=!!visible;
   if(next!==active){output='';follow=true;}
   active=next;fallback=flat(activity);schedule();
  },
  receive({delta='',reset=false}={}){
   if(reset){output='';follow=true;}
   if(typeof delta==='string'&&active){output=(output+flat(delta)).slice(-16000);schedule();}
  },
  clear(){active=false;output='';fallback='';clearTimeout(timer);timer=null;paint();},
 };
};
