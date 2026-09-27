import {audioLook} from './audio-look.mjs';
// Shared score-clock lighting/raster reader. No network, DMX, or audio writes.
export const LOOKS = [
  {name:'Overture', mode:'tide', color:[255,156,80]},
  {name:'The Walk', mode:'steps', color:[82,230,169]},
  {name:'Waltz', mode:'orbit', color:[139,135,255]},
  {name:'Chase', mode:'weave', color:[255,91,116]},
  {name:'Sneak', mode:'slits', color:[88,150,233]},
  {name:'Lullaby', mode:'cradle', color:[143,180,249]},
  {name:'The Climb', mode:'stairs', color:[199,114,250]},
  {name:'The Lift', mode:'braid', color:[111,242,221]},
  {name:'Fanfare', mode:'rays', color:[255,191,90]},
  {name:'Return', mode:'ripples', color:[213,155,244]},
  {name:'Vanish', mode:'vanish', color:[95,149,224]},
];
export const CHANNELS = 5; // R G B energy pitch (MIDI, zero means no note)
const clamp = (v,a,b) => Math.max(a,Math.min(b,v));
export function movementAt(timeline,t) {
  let i=0;
  while(i+1<timeline.movements.length && timeline.movements[i+1].t0<=t)i++;
  return i;
}
export function lookAt(timeline,t,seat=0) {
  const section=movementAt(timeline,Math.max(0,t));
  const look=LOOKS[section%LOOKS.length];
  if(!Number.isFinite(t)||t<0||t>=timeline.duration)return {...look,section,rgb:[0,0,0],energy:0,pitch:0,active:false};
  const frame=t*timeline.rate,a=Math.floor(frame),b=Math.min(a+1,timeline.frames-1),u=frame-a;
  seat=clamp(Math.trunc(seat),0,timeline.seats-1);
  const stride=timeline.seats*CHANNELS,offset=seat*CHANNELS;
  const read=c=>timeline.data[a*stride+offset+c]*(1-u)+timeline.data[b*stride+offset+c]*u;
  return {...look,section,rgb:[0,1,2].map(c=>Math.round(read(c))),energy:read(3)/255,
    pitch:timeline.data[(u<.5?a:b)*stride+offset+4],active:true};
}
export function loadTimeline(system,path='/pieces/notespatial-look.nstimeline') {
  const bytes=system.readFileBytes(path);
  if(!bytes)throw Error('Missing score look timeline');
  // Native readFile can truncate larger JSON; match the score's full-byte loader.
  const chunks=[],view=new Uint8Array(bytes);
  for(let i=0;i<view.length;i+=4096)chunks.push(String.fromCharCode.apply(null,view.subarray(i,i+4096)));
  const timeline=JSON.parse(chunks.join(''));
  if(timeline.version!==1||timeline.channels!==CHANNELS||timeline.data.length!==timeline.frames*timeline.seats*CHANNELS)
    throw Error('Invalid score look timeline');
  return timeline;
}
export function noteName(pitch) {
  return pitch>0?['C','C#','D','D#','E','F','F#','G','G#','A','A#','B'][pitch%12]+(Math.floor(pitch/12)-1):'';
}
// One dark wipe and at most 24 primitives, regardless of polyphony or score length.
// Call before battery overlay. Notes are an optional single label, never a score scan.
export function paintLook(api,timeline,t,seat=0,{noteLabels=false}={}) {
  const look=lookAt(timeline,t,seat),{wipe,ink,line,box,screen}=api;
  const {width:w,height:h}=screen;
  wipe(4,6,11);
  if(!look.active)return look;
  const audio=audioLook(api.sound);
  const e=audio.env,rgb=look.rgb;
  wipe(...rgb.map(v=>Math.round(v/Math.max(1,...rgb)*(22+65*e))));
  for(let i=0;i<12;i++){const height=h*(.05+.65*audio.bands[i]);ink(...rgb.map(v=>Math.round(v/Math.max(1,...rgb)*(100+155*audio.bands[i]))));box(Math.round(i*w/12),Math.round(h-height),Math.ceil(w/12)-3,Math.ceil(height),'fill');}
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
    const note=noteName(look.pitch),size=Math.max(1,Math.floor(Math.min(w/(note.length*6+4),h/18)));
    ink(250,248,238);
    (api.overlayWrite||api.write)(note,{x:Math.round((w-note.length*6*size)/2),y:Math.round((h-10*size)/2),font:'6x10',size});
  }
  return look;
}
