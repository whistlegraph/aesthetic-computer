// First finger aims or taps to fire; second finger holds forward and drags to steer movement.
export function installTouchControls(canvas,key,active=()=>true){
  const fingers=new Map(),timers=new Map(),held=new Set();
  let look=null,move=null;
  function set(code,down){if(held.has(code)===down)return;if(down)held.add(code);else held.delete(code);key(code,down);}
  function pulse(code,ms){set(code,true);clearTimeout(timers.get(code));timers.set(code,setTimeout(()=>{timers.delete(code);set(code,false);},ms));}
  function movement(finger){
    const dx=finger.x-finger.startX,dy=finger.y-finger.startY;
    set('ArrowUp',dy<24);set('ArrowDown',dy>=24);
    set('Comma',dx < -24);set('Period',dx>24);
  }
  function reset(){for(const timer of timers.values())clearTimeout(timer);timers.clear();for(const code of [...held])set(code,false);fingers.clear();look=move=null;}
  canvas.addEventListener('pointerdown',event=>{
    if(event.pointerType!=='touch'||!active())return;
    event.preventDefault();canvas.focus();canvas.setPointerCapture(event.pointerId);
    const finger={x:event.clientX,y:event.clientY,startX:event.clientX,startY:event.clientY,at:performance.now(),dragged:false,multi:fingers.size>0};
    if(fingers.size)for(const other of fingers.values())other.multi=true;
    fingers.set(event.pointerId,finger);
    if(look===null)look=event.pointerId;
    else if(move===null){move=event.pointerId;movement(finger);}
  });
  canvas.addEventListener('pointermove',event=>{
    const finger=fingers.get(event.pointerId);if(!finger)return;
    event.preventDefault();if(!active()){reset();return;}
    const dx=event.clientX-finger.x;finger.x=event.clientX;finger.y=event.clientY;
    if(Math.hypot(finger.x-finger.startX,finger.y-finger.startY)>8)finger.dragged=true;
    if(event.pointerId===look&&finger.dragged&&dx){
      const code=dx>0?'ArrowRight':'ArrowLeft',opposite=dx>0?'ArrowLeft':'ArrowRight';
      clearTimeout(timers.get(opposite));timers.delete(opposite);set(opposite,false);
      pulse(code,Math.min(120,Math.max(25,Math.abs(dx)*5)));
    }
    if(event.pointerId===move)movement(finger);
  });
  function end(event){
    const finger=fingers.get(event.pointerId);if(!finger)return;
    event.preventDefault();fingers.delete(event.pointerId);
    if(event.pointerId===look){look=null;for(const code of ['ArrowLeft','ArrowRight']){clearTimeout(timers.get(code));timers.delete(code);set(code,false);}
      if(event.type==='pointerup'&&active()&&!finger.dragged&&(!finger.multi||(move!==null&&performance.now()-fingers.get(move).at>=350))&&performance.now()-finger.at<350)pulse('ControlLeft',120);
    }
    if(event.pointerId===move){move=null;for(const code of ['ArrowUp','ArrowDown','Comma','Period'])set(code,false);if(event.type==='pointerup'&&active()&&!finger.dragged&&performance.now()-finger.at<350)pulse('Space',120);}
  }
  for(const name of ['pointerup','pointercancel','lostpointercapture'])canvas.addEventListener(name,end);
  return {reset};
}
