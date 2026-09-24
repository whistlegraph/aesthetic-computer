// NOTEPAT_SCORE_VISUAL_V1_BEGIN
function drawNotepatScore(music, elapsed) {
  if (!music.look || !music.movement) return;
  const timeline={look:music.look,movements:[]};
  timeline.movements[music.look.section]=music.movement;
  let color=[240,240,240];
  const rect=(x,y,w,h)=>screenRect(x,y,w,h,color);
  const api={screen:{width:viewWidth(),height:Math.max(100,viewHeight-160)},
    wipe:(r,g,b)=>wipe(r,g,b),ink:(r,g,b)=>{color=[r,g,b];},
    line:(x,y,a,b)=>filledCapsule(x,y,a,b,1.6,color),
    box:(x,y,w,h,kind)=>{if(kind==='fill')rect(x,y,w,h);else{rect(x,y,w,2);rect(x,y+h-2,w,2);rect(x,y,2,h);rect(x+w-2,y,2,h);}},
    write:(text,{x,y,size=1})=>typeWrite(text,x,y,size*10,...color)};
  triangleDepth=-.8;
  paintOskiewarNotepat(api,timeline,elapsed,5,{noteLabels:true});
}
function oskiewarScoreNoteName(pitch) {
  return pitch>0?['C','C#','D','D#','E','F','F#','G','G#','A','A#','B'][pitch%12]+(Math.floor(pitch/12)-1):'';
}
function paintOskiewarNotepat(api,timeline,t,seat=0,{noteLabels=false}={}) {
  const look=timeline.look,{wipe,ink,line,box,screen}=api;
  const {width:w,height:h}=screen;
  wipe(4,6,11);
  if(!look.active)return look;
  const {energy:e,rgb}=look;
  // Raster contrast has its own floor; DMX channel caps must not make screens illegible.
  const peak=Math.max(1,...rgb);
  ink(...rgb.map(v=>Math.round(v/peak*(110+145*e))));
  const phase=t*.4+seat*.8,cx=w/2,cy=h/2;
  if(look.mode==='tide') {
    for(let i=0;i<16;i++) {
      const y=h*(i+1)/17;
      const shift=Math.sin(phase+i*.32)*w*(.04+.1*e);
      line(w*.1+shift,y,w*.9+shift,y);
    }
  } else if(look.mode==='steps') {
    for(let i=0;i<12;i++) {
      const width=w/15,x=(i+1)*w/14;
      const height=h*(.08+(.15+.4*e)*(.5+.5*Math.sin(phase+i*.72)));
      box(Math.round(x-width/2),Math.round(cy-height/2),Math.round(width),Math.round(height),'outline');
    }
  } else if(look.mode==='orbit') {
    for(let i=0;i<18;i++) {
      const angle=phase*.6+i*Math.PI*2/18;
      const radius=Math.min(w,h)*(.14+.22*e);
      const x=cx+Math.cos(angle)*radius,y=cy+Math.sin(angle)*radius;
      const side=4+Math.min(w,h)*(.01+.018*e);
      box(Math.round(x-side/2),Math.round(y-side/2),Math.round(side),Math.round(side),'fill');
    }
  } else if(look.mode==='weave') {
    for(let i=0;i<12;i++) {
      const x=w*(i+1)/13,shift=Math.sin(phase+i*.6)*w*.12*e;
      line(x+shift,h*.12,w-x-shift,h*.88);
      line(w*.12,h*(i+1)/13,w*.88,h*(i+1)/13);
    }
  } else if(look.mode==='slits') {
    for(let i=0;i<9;i++) {
      const x=w*(i+1)/10,y=cy+Math.sin(phase+i*.9)*h*.17;
      box(Math.round(x),Math.round(y),Math.max(2,Math.round(w*.012)),Math.round(h*(.03+.13*e)),'fill');
    }
  } else if(look.mode==='cradle') {
    const sway=Math.sin(phase*.6)*w*.16;
    for(let i=0;i<12;i++) {
      const size=(i+1)/13,ww=w*.65*size,hh=h*.58*size;
      box(Math.round(cx+sway*size-ww/2),Math.round(cy-hh/2),Math.round(ww),Math.round(hh),'outline');
    }
  } else if(look.mode==='stairs') {
    for(let i=0;i<12;i++) {
      const x=w*(i+1)/14,y=h*(.82-i*.052),width=w*(.035+.12*e);
      box(Math.round(x),Math.round(y),Math.round(width),Math.max(2,Math.round(h*.012)),'fill');
    }
  } else if(look.mode==='braid') {
    for(let i=0;i<12;i++) {
      const y=h*(i+1)/13,spread=Math.sin(phase+i*.65)*w*(.15+.2*e);
      const size=Math.max(3,Math.round(h*.025));
      box(Math.round(cx+spread),Math.round(y),size,size,'fill');
      box(Math.round(cx-spread),Math.round(y),size,size,'fill');
    }
  } else if(look.mode==='rays') {
    for(let i=0;i<20;i++) {
      const angle=i*Math.PI*2/20+Math.sin(phase*.3)*.12;
      const inner=Math.min(w,h)*.11,outer=Math.min(w,h)*(.25+.2*e);
      line(cx+Math.cos(angle)*inner,cy+Math.sin(angle)*inner,cx+Math.cos(angle)*outer,cy+Math.sin(angle)*outer);
    }
  } else if(look.mode==='ripples') {
    for(let i=0;i<10;i++) {
      const scale=(i+1)/11,ww=w*.78*scale,hh=h*.7*scale;
      const offset=Math.sin(phase-i*.35)*w*.025;
      box(Math.round(cx+offset-ww/2),Math.round(cy-hh/2),Math.round(ww),Math.round(hh),'outline');
      line(cx+offset-ww/2,cy,cx+offset+ww/2,cy);
    }
  } else {
    const movement=timeline.movements[look.section];
    const remaining=clamp((movement.t1-t)/(movement.t1-movement.t0),0,1);
    for(let i=0;i<8;i++) {
      const width=w*(.02+.65*remaining)*(1-i/10),y=cy+(i-3.5)*h*.045;
      line(cx-width/2,y,cx+width/2,y);
    }
  }
  if(noteLabels&&look.pitch) {
    const note=oskiewarScoreNoteName(look.pitch),size=Math.max(1,Math.floor(Math.min(w/(note.length*6+4),h/18)));
    ink(250,248,238);
    (api.overlayWrite||api.write)(note,{x:Math.round((w-note.length*6*size)/2),y:Math.round((h-10*size)/2),font:'6x10',size});
  }
  return look;
}

// NOTEPAT_SCORE_VISUAL_V1_END
