// Runs inside the trusted AC preview. Exposes native controller snapshots through
// the same browser API that AC already polls, without synthesizing keyboard input.
window.aeselGamepadScript=function(pads){
 function receive(pads){
  if(!window.__aeselGamepad){
   const browser=navigator.getGamepads.bind(navigator);
   const state={pads:[],updated:0};
   Object.defineProperty(navigator,'getGamepads',{configurable:true,value:()=>{
    if(state.pads.length){
     if(performance.now()-state.updated>1500)return state.pads.map(p=>({...p,axes:p.axes.map(()=>0),buttons:p.buttons.map(()=>({pressed:false,touched:false,value:0}))}));
     return state.pads;
    }
    return browser();
   }});
   window.__aeselGamepad=state;
  }
  const state=window.__aeselGamepad;
  if(!Array.isArray(pads))return;
  // Release held controls before dropping a disconnected device.
  if(!pads.length&&state.pads.length){state.updated=0;setTimeout(()=>{if(state.updated===0)state.pads=[];},100);return;}
  state.pads=pads.slice(0,8).map((p,index)=>({index,id:String(p.id).slice(0,160),mapping:'standard',connected:true,timestamp:performance.now(),
   axes:(p.axes||[]).slice(0,8).map(v=>Number.isFinite(v)?Math.max(-1,Math.min(1,v)):0),
   buttons:(p.buttons||[]).slice(0,32).map(v=>({pressed:v>.5,touched:v>0,value:Number.isFinite(v)?Math.max(0,Math.min(1,v)):0}))}));
  state.updated=performance.now();
 }
 return `(${receive.toString()})(${JSON.stringify(pads)})`;
};
