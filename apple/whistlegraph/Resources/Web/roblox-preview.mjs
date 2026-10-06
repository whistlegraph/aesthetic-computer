// A room plan, not a Roblox physics simulation. Drawn strokes become platforms.
export function roomPreview(container,{getRoom,onPath,onSelect,onError,canEdit}){
  container.innerHTML='<div class="room-toolbar"><span>Room plan</span><button id="room-select" aria-pressed="true">Select</button><button id="room-draw" aria-pressed="false">Draw path</button></div><canvas id="room-plan" aria-label="Roblox room plan. Select an object or draw a path."></canvas>';
  const style=document.createElement('style');style.textContent=`.room-toolbar{height:44px;display:flex;align-items:center;gap:8px;padding:0 8px;background:#f1eee7;color:#24202b;font:15px Comic,Arial}.room-toolbar span{flex:1}.room-toolbar button{min-height:40px;padding:0 10px;background:transparent;color:inherit;border-radius:7px}.room-toolbar button[aria-pressed=true]{background:#d9c5da}#room-plan{display:block;width:100%;height:calc(100% - 44px);touch-action:none;background:#f8f6f1}`;document.head.append(style);
  const canvas=container.querySelector('canvas'),context=canvas.getContext('2d');
  let drawing=false,selected='bridge',points=null,pointer=null;
  const select=container.querySelector('#room-select'),draw=container.querySelector('#room-draw');
  function mode(value){drawing=value;select.setAttribute('aria-pressed',String(!value));draw.setAttribute('aria-pressed',String(value));}
  select.onclick=()=>mode(false);draw.onclick=()=>mode(true);
  function layout(){const rect=canvas.getBoundingClientRect();return {w:rect.width,h:rect.height,scale:Math.min(rect.width,rect.height)/100};}
  function local(event){const rect=canvas.getBoundingClientRect(),{w,h,scale}=layout();return [Math.max(-48,Math.min(48,(event.clientX-rect.left-w/2)/scale)),Math.max(-48,Math.min(48,(event.clientY-rect.top-h/2)/scale))];}
  function paint(){
    const {w,h,scale}=layout(),dpr=window.devicePixelRatio||1;
    canvas.width=Math.round(w*dpr);canvas.height=Math.round(h*dpr);
    context.setTransform(dpr,0,0,dpr,0,0);context.fillStyle='#f8f6f1';context.fillRect(0,0,w,h);
    context.translate(w/2,h/2);context.scale(scale,scale);
    context.strokeStyle='#e8e3dc';context.lineWidth=.5;
    for(let i=-48;i<=48;i+=8){context.beginPath();context.moveTo(i,-48);context.lineTo(i,48);context.moveTo(-48,i);context.lineTo(48,i);context.stroke();}
    for(const o of getRoom().objects){
      context.save();context.translate(o.position[0],o.position[2]);context.rotate(-o.yaw*Math.PI/180);
      context.fillStyle=`rgb(${o.color.join(',')})`;context.fillRect(-o.size[0]/2,-o.size[2]/2,o.size[0],o.size[2]);
      context.lineWidth=o.id===selected?1.1:.3;context.strokeStyle=o.id===selected?'#292032':'#ffffff';context.strokeRect(-o.size[0]/2,-o.size[2]/2,o.size[0],o.size[2]);
      if(o.bounce>0){context.fillStyle='#ffffff';context.font='bold 5px Arial';context.textAlign='center';context.fillText('↑',0,2);}
      if(o.kind==='goal'){context.fillStyle='#292032';context.beginPath();context.arc(0,0,1.2,0,2*Math.PI);context.fill();}
      context.restore();
    }
    const spawn=getRoom().spawn;context.beginPath();context.arc(spawn[0],spawn[2],1.8,0,2*Math.PI);context.fillStyle='#fff';context.fill();context.strokeStyle='#24202b';context.lineWidth=.7;context.stroke();
    if(points){context.beginPath();points.forEach((p,i)=>i?context.lineTo(...p):context.moveTo(...p));context.strokeStyle='#b44887';context.lineWidth=4;context.lineCap='round';context.stroke();}
  }
  canvas.onpointerdown=event=>{
    if(!canEdit())return;const point=local(event);
    if(drawing){pointer=event.pointerId;points=[point];canvas.setPointerCapture(pointer);}
    else {for(const o of [...getRoom().objects].reverse()){
      const dx=point[0]-o.position[0],dz=point[1]-o.position[2],a=o.yaw*Math.PI/180;
      if(Math.abs(dx*Math.cos(a)-dz*Math.sin(a))<=o.size[0]/2&&Math.abs(dx*Math.sin(a)+dz*Math.cos(a))<=o.size[2]/2){selected=o.id;onSelect(o.id);break;}
    }}paint();
  };
  canvas.onpointermove=event=>{if(event.pointerId!==pointer||!points||points.length>=128)return;const p=local(event);if(Math.hypot(p[0]-points.at(-1)[0],p[1]-points.at(-1)[1])>=3){points.push(p);paint();}};
  canvas.onpointerup=event=>{if(event.pointerId!==pointer||!points)return;const path=points;points=null;pointer=null;try{if(canEdit())onPath(path);}catch(error){onError(error.message);}paint();};
  canvas.onpointercancel=()=>{points=null;pointer=null;paint();};
  new ResizeObserver(paint).observe(canvas);
  return {paint,select(id){selected=id;paint();},cancel(){points=null;pointer=null;paint();}};
}
