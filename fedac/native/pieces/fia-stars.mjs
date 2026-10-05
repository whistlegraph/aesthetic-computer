// Fia Stars, 26.10.04
// A pink and blue starfield for Fia.
import { savedWifi } from '../lib/saved-wifi.mjs';
const connectSaved=savedWifi();
export function sim(api){connectSaved(api);}
let stars = [], t = 0;
export function boot({system}) {
  system.startSSH?.();
  stars = Array.from({length: 230}, () => ({x: (Math.random()-.5)*2, y: (Math.random()-.5)*2, z: Math.random()+.01, hue: Math.random()}));
}
export function paint({wipe, ink, box, line, write, screen}) {
  t += 1/60;
  const w=screen.width, h=screen.height, cx=w/2, cy=h/2;
  wipe(3, 3, 15);
  const scale=Math.min(w,h)*.68;
  for (const s of stars) {
    const prev=s.z; s.z-=.0024;
    if(s.z<.025) {s.z=1; s.x=(Math.random()-.5)*2; s.y=(Math.random()-.5)*2; continue;}
    const drift=Math.sin(t*.14)*.035;
    const x=cx+(s.x+drift)*scale/s.z, y=cy+s.y*scale/s.z;
    if(x<0||x>w||y<0||y>h) {s.z=1; s.x=(Math.random()-.5)*2; s.y=(Math.random()-.5)*2; continue;}
    const glow=Math.round(80+175*(1-s.z));
    ink(s.hue>.55 ? glow : Math.round(glow*.6), Math.round(glow*.65), glow);
    line(x,y,cx+(s.x+drift)*scale/prev,cy+s.y*scale/prev);
    const size=s.z<.24?2:1; box(Math.round(x),Math.round(y),size,size);
  }
  const text='love u fia';
  const size=Math.max(2,Math.floor(Math.min(w/(text.length*6+12),h/28)));
  const x=Math.round((w-text.length*6*size)/2), y=Math.round(cy-5*size+Math.sin(t*.8)*3);
  ink(3,3,15,215); box(x-14,y-10,text.length*6*size+28,10*size+20);
  ink(93,34,110); write(text,{x:x+2,y:y+2,size,font:'font_1'});
  ink(255,177+Math.round(Math.sin(t)*14),217); write(text,{x,y,size,font:'font_1'});
}
export function act({event,system}) { if(event.is('keyboard:down:escape')) system.jump('prompt'); }
