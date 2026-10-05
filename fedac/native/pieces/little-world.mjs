// Little World, 26.10.04
// Walk an 8BitDo-controlled little guy through a scrolling pixel meadow.
import { savedWifi } from '../lib/saved-wifi.mjs';
const connectSaved=savedWifi();
const TILE=32, COLS=40, ROWS=32, WIDTH=COLS*TILE, HEIGHT=ROWS*TILE;
const bindings=[[0x100,'UP'],[0x200,'DOWN'],[0x400,'LEFT'],[0x800,'RIGHT'],[0x10,'A'],[0x20,'B'],[0x40,'X'],[0x80,'Y'],[0x1000,'LB'],[0x2000,'RB'],[0x4000,'LS'],[0x8000,'RS'],[4,'MENU'],[8,'VIEW']];
const keys=new Set();
let map,guys=[],pad={},held=[],x=336,y=336,cameraX=0,cameraY=0,walk=0,facing=1;
let last=0,now=0,previousButtons=0,jump=0,flowers=[],stars=[],collected=0,music=true,nextNote=0,note=0;
let fps=0,fpsAt=0,frames=0,helperAt=0;
const clamp=(v,a,b)=>Math.max(a,Math.min(b,v));
const hash=(a,b)=>{let n=Math.imul(a+71,374761393)^Math.imul(b+137,668265263);n=Math.imul(n^(n>>>13),1274126177);return (n^(n>>>16))>>>0;};
function terrain(tx,ty){
  if(tx<1||ty<1||tx>=COLS-1||ty>=ROWS-1)return 'tree';
  if(((tx-25)/5)**2+((ty-12)/4)**2<1)return 'water';
  if(tx===10||ty===10||ty===22&&tx>=10&&tx<=31)return 'path';
  if(tx>=5&&tx<=8&&ty>=5&&ty<=7)return 'house';
  return hash(tx,ty)%17===0?'tree':'grass';
}
function open(px,py){const t=terrain(Math.floor(px/TILE),Math.floor(py/TILE));return t!=='water'&&t!=='tree'&&t!=='house';}
function startReader(system){
  // One detached reader per machine; its flock makes re-entry harmless.
  system.pty2?.spawn('/bin/sh',['-c','exec /mnt/tools/ac-usb-controller'],40,10);
  helperAt=Date.now();
}
export function boot({system}){
  system.startSSH?.();last=fpsAt=Date.now();
  try{pad=JSON.parse(system.readFile('/tmp/ac-controller.json')||'{}');}catch{}
  if(!pad.at||Date.now()-pad.at>1500)startReader(system);
  for(let ty=3;ty<ROWS-2;ty+=3)for(let tx=3;tx<COLS-2;tx+=4)
    if(open(tx*TILE+16,ty*TILE+16))stars.push({x:tx*TILE+16,y:ty*TILE+16,taken:false});
}
function tone(api,tone,volume=.05,duration=.13){api.sound?.synth?.({type:'sine',tone,volume,duration,attack:.008,decay:duration-.01});}
export function sim(api){
  connectSaved(api);now=Date.now();const dt=clamp((now-last)/1000,0,.05);last=now;
  try{pad=JSON.parse(api.system.readFile('/tmp/ac-controller.json')||'{}');}catch{pad={};}
  const fresh=pad.at&&now-pad.at<1500;
  if(!fresh){pad={};if(now-helperAt>5000)startReader(api.system);}
  const active=!!(fresh&&pad.connected&&pad.ready),b=active?pad.buttons:0;
  held=active?bindings.filter(([bit])=>b&bit).map(([,name])=>name):[];
  if(active&&pad.lt>100)held.push('LT');if(active&&pad.rt>100)held.push('RT');if(active&&pad.guide)held.push('XBOX');
  const axis=v=>Math.abs(v||0)<6500?0:clamp(v/32767,-1,1);
  let dx=(!!(b&0x800)||keys.has('arrowright')?1:0)-(!!(b&0x400)||keys.has('arrowleft')?1:0);
  let dy=(!!(b&0x200)||keys.has('arrowdown')?1:0)-(!!(b&0x100)||keys.has('arrowup')?1:0);
  if(!dx&&active)dx=axis(pad.lx);if(!dy&&active)dy=axis(pad.ly);
  const length=Math.hypot(dx,dy);if(length>1){dx/=length;dy/=length;}
  const speed=active&&(pad.rt>100||b&0x2000)?155:90;
  const nx=clamp(x+dx*speed*dt,16,WIDTH-16),ny=clamp(y+dy*speed*dt,16,HEIGHT-16);
  if(open(nx-6,y)&&open(nx+6,y))x=nx;
  if(open(x,ny-4)&&open(x,ny+4))y=ny;
  if(length){walk+=dt*10;if(dx)facing=dx<0?-1:1;}
  const pressed=b&~previousButtons;previousButtons=b;
  if(pressed&0x10){jump=.45;if(music)tone(api,164,.07);}
  if(pressed&0x20){flowers.push({x:Math.round(x),y:Math.round(y)});if(flowers.length>256)flowers.shift();if(music)tone(api,130,.06);}
  jump=Math.max(0,jump-dt);
  for(const s of stars)if(!s.taken&&Math.hypot(x-s.x,y-s.y)<16){s.taken=true;collected++;if(music)tone(api,220,.06,.2);}
  cameraX=clamp(Math.round(x-api.screen.width/2),0,WIDTH-api.screen.width);
  cameraY=clamp(Math.round(y-(api.screen.height-72)/2),0,HEIGHT-(api.screen.height-72));
  if(music&&now>=nextNote){nextNote=now+500;tone(api,[65.41,82.41,98,73.42,65.41,98,82.41,73.42][note++%8],.045,.38);}
}
function prepare(api){
  const {painting,page,wipe,ink,box,line,write}=api;
  map=painting(WIDTH,HEIGHT);page(map);wipe(67,117,79);
  for(let ty=0;ty<ROWS;ty++)for(let tx=0;tx<COLS;tx++){
    const px=tx*TILE,py=ty*TILE,t=terrain(tx,ty),n=hash(tx,ty);
    ink(...((tx+ty)%2?[69,120,80]:[72,124,83]));box(px,py,32,32);
    if(t==='path'){
      ink(198,171,116);box(px,py,32,32);ink(176,149,100);box(px+(n%23),py+7,3,2);box(px+20,py+23,2,2);
    }else if(t==='water'){
      ink(45,100,131);box(px,py,32,32);ink(88,158,177);box(px+5,py+9,12,2);box(px+18,py+24,9,2);
    }else if(t==='tree'){
      ink(45,89,64);box(px+4,py+24,26,6);ink(108,73,56);box(px+13,py+18,6,11);
      ink(29,73,62);box(px+3,py+8,26,16);ink(40,91,67);box(px+6,py+2,20,20);ink(78,143,87);box(px+8,py+4,8,5);
    }else if(t==='house'){
      ink(214,180,133);box(px,py,32,32);ink(99,62,62);box(px+3,py+3,26,5);
    }else{
      ink(96,151,93);box(px+n%25,py+(n>>>8)%25,2,4);box(px+(n>>>4)%25,py+(n>>>12)%25,4,2);
      if(n%7===0){ink(243,200,129);box(px+18,py+15,3,3);}
    }
  }
  // One little cottage and a sign are landmarks while the camera scrolls.
  ink(109,58,67);box(154,145,140,20);box(163,133,122,12);box(175,123,98,10);
  ink(70,57,55);box(210,217,26,39);ink(137,201,201);box(169,190,25,23);box(253,190,25,23);
  ink(97,70,55);box(355,302,4,25);ink(231,208,156);box(347,291,102,20);
  ink(74,62,51);write('LOVE U FIA',{x:351,y:297,size:1,font:'font_1'});
  // The cached RGBA frames leave untouched pixels transparent.
  for(let f=0;f<4;f++){
    const p=painting(24,32);page(p);
    ink(51,40,64);box(5,5,14,12);box(3,9,18,7);box(7,17,10,9);
    ink(247,176,180);box(6,7,12,10);box(4,10,16,5);
    ink(241,89,140);box(7,17,10,9);box(4,18,3,6);box(17,18,3,6);
    ink(42,43,66);box(8,11,2,3);box(15,11,2,3);
    ink(255,226,197);box(10,15,4,1);
    ink(48,42,65);box(7,25,4,f%2?3:5);box(13,25,4,f%2?5:3);
    ink(255,213,132);box(5,4,14,3);box(8,1,9,4);
    guys.push(p);
  }
  page();
}
export function paint(api){
  if(!map)prepare(api);
  const {paste,ink,box,write,screen}=api,w=screen.width,h=screen.height;
  paste(map,0,0,cameraX,cameraY,w,h-72);
  for(const s of stars)if(!s.taken){
    const sx=Math.round(s.x-cameraX),sy=Math.round(s.y-cameraY);
    if(sx<0||sy<0||sx>w||sy>h-72)continue;
    ink(255,224,125);box(sx-2,sy-6,4,12);box(sx-6,sy-2,12,4);ink(255,248,208);box(sx-1,sy-2,2,3);
  }
  for(const f of flowers){const fx=f.x-cameraX,fy=f.y-cameraY;if(fx<0||fy<0||fx>w||fy>h-72)continue;ink(32,78,57);box(fx,fy-5,2,8);ink(248,132,167);box(fx-3,fy-9,8,6);ink(255,220,118);box(fx,fy-7,2,2);}
  const px=Math.round(x-cameraX),py=Math.round(y-cameraY),lift=Math.round(Math.sin(jump/.45*Math.PI)*18);
  ink(42,86,65);box(px-10,py-2,20,5);
  paste(guys[Math.floor(walk)%4],px-12,py-29-lift);
  ink(247,176,180);box(px+(facing<0?-10:8),py-19-lift,2,3);
  const t=Date.now();frames++;if(t-fpsAt>=500){fps=frames*1000/(t-fpsAt);frames=0;fpsAt=t;}
  ink(22,35,43);box(0,h-72,w,72);
  const active=pad.connected&&pad.ready&&t-pad.at<1500;
  ink(...(active?[140,237,176]:[255,192,128]));
  write(active?'8BITDO USB':pad.connected?'WAITING FOR INPUT':'CONTROLLER DISCONNECTED',{x:12,y:h-64,size:1,font:'font_1'});
  ink(212,228,223);write(`${fps.toFixed(0)} FPS   ${collected} STARS   ${Math.round(x)},${Math.round(y)}`,{x:w-240,y:h-64,size:1,font:'font_1'});
  ink(255,222,137);write(`HELD: ${held.length?held.join(' '):'-'}`,{x:12,y:h-47,size:held.join(' ').length>39?1:2,font:'font_1'});
  ink(183,205,198);write('Move: pad/stick   A: hop   B: plant   RB/RT: run   M: music',{x:12,y:h-16,size:1,font:'font_1'});
}
export function act({event:e,system}){
  if(e.is('keyboard:down'))keys.add(e.key);
  if(e.is('keyboard:up'))keys.delete(e.key);
  if(e.is('keyboard:down:m'))music=!music;
  if(e.is('keyboard:down:escape'))system.jump('prompt');
}
