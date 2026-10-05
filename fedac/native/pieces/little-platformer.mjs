// Little Platformer, 26.10.04
// Controller playground: momentum, variable jumps, springs, balls and crates.
import { savedWifi } from '../lib/saved-wifi.mjs';
import { createWorld, step, STEP, clamp } from '../lib/platform-physics.mjs';
const connectSaved=savedWifi(),keys=new Set();
const names=[[0x100,'UP'],[0x200,'DOWN'],[0x400,'LEFT'],[0x800,'RIGHT'],[0x10,'A'],[0x20,'B'],[0x40,'X'],[0x80,'Y'],[0x1000,'LB'],[0x2000,'RB'],[0x4000,'LS'],[0x8000,'RS'],[4,'MENU'],[8,'VIEW']];
let world=createWorld(),pad={},held=[],previous=0,facing=1,last=0,accum=0,cx=0,cy=240;
let fps=0,frames=0,fpsAt=0,helperAt=0,pending={},keyboardPressed=new Set(),soundOn=true,lastTone=0;
let guys=[];
function startReader(system){system.pty2?.spawn('/bin/sh',['-c','exec /mnt/tools/ac-usb-controller'],40,10);helperAt=Date.now();}
export function boot({system}){system.startSSH?.();last=fpsAt=Date.now();try{pad=JSON.parse(system.readFile('/tmp/ac-controller.json')||'{}');}catch{}if(!pad.at||Date.now()-pad.at>1500)startReader(system);}
function tone(api,hz){const t=Date.now();if(soundOn&&t-lastTone>75){lastTone=t;api.sound?.synth?.({type:'sine',tone:hz,volume:.08,duration:.16,attack:.006,decay:.15});}}
export function sim(api){
 connectSaved(api);const now=Date.now(),dt=clamp((now-last)/1000,0,.1);last=now;accum=Math.min(.1,accum+dt);
 try{pad=JSON.parse(api.system.readFile('/tmp/ac-controller.json')||'{}');}catch{pad={};}
 if(!pad.at||now-pad.at>1500){pad={};if(now-helperAt>5000)startReader(api.system);}
 const b=pad.connected&&pad.ready?pad.buttons:0,pressed=b&~previous;previous=b;
 held=names.filter(([bit])=>b&bit).map(([,name])=>name);
 if(pad.lt>100)held.push('LT');if(pad.rt>100)held.push('RT');if(pad.guide)held.push('XBOX');
 let axis=(b&0x800||keys.has('arrowright')?1:0)-(b&0x400||keys.has('arrowleft')?1:0);
 if(!axis&&pad.ready&&Math.abs(pad.lx)>6500)axis=clamp(pad.lx/32767,-1,1);
 if(axis)facing=axis<0?-1:1;
 if(pressed&0x80||keyboardPressed.has('r')){world=createWorld();accum=0;cx=0;cy=240;}
 pending.jumpPressed ||= !!(pressed&0x10)||keyboardPressed.has('space')||keyboardPressed.has('arrowup');
 pending.shove ||= !!(pressed&0x20)||keyboardPressed.has('b');
 pending.spawn ||= !!(pressed&0x40)||keyboardPressed.has('x');
 keyboardPressed.clear();
 const input={axis,jumpHeld:!!(b&0x10)||keys.has('space')||keys.has('arrowup'),run:!!(b&0x2000)||pad.rt>100||keys.has('shift'),facing};
 const landed=world.landings,bumps=world.bumps;
 while(accum>=STEP){
  if(pending.jumpPressed)tone(api,147);
  step(world,{...input,...pending},STEP);pending={};accum-=STEP;
 }
 if(world.bumps>bumps)tone(api,110);else if(world.landings>landed)tone(api,65);
 const h=api.screen.height-54,w=api.screen.width;
 const tx=clamp(world.player.x-w*.38,0,Math.max(0,2700-w));
 const ty=clamp(world.player.y-h*.65,0,Math.max(0,600-h));
 cx+=(tx-cx)*(1-Math.exp(-9*dt));cy+=(ty-cy)*(1-Math.exp(-6*dt));
}
function prepare({painting,page,ink,box}){
 for(let f=0;f<4;f++){
  const p=painting(26,34);page(p);
  ink(47,34,64);box(6,5,14,13);box(4,9,18,8);box(7,18,12,11);
  ink(249,179,189);box(7,7,12,10);box(5,10,16,5);
  ink(242,83,140);box(7,18,12,10);box(4,19,3,7);box(19,19,3,7);
  ink(255,217,124);box(5,4,16,3);box(9,1,10,4);
  ink(49,38,62);box(8,27,4,f%2?4:6);box(15,27,4,f%2?6:4);
  guys.push(p);
 }page();
}
export function paint(api){
 const {wipe,ink,box,line,circle,write,paste,screen}=api;if(!guys.length)prepare(api);
 const w=screen.width,h=screen.height,vh=h-54,X=Math.round(cx),Y=Math.round(cy),p=world.player;
 wipe(26,27,52);
 // Distant hills and stars move more slowly than the playable foreground.
 for(let i=0;i<65;i++){const sx=((i*137-X*.18)%(w+60)+w+60)%(w+60)-30,sy=((i*i*19)%240)-Y*.08;ink(i%3===0?185:98,130,175);box(sx,sy,i%4===0?2:1,2);}
 ink(239,207,153);circle(w-105-X*.05,45-Y*.04,18,true);ink(26,27,52);circle(w-97-X*.05,39-Y*.04,17,true);
 for(let i=-1;i<14;i++){const bx=i*90-(X*.32%90),by=vh-58-((i+40)%3)*24;ink(40,45,72);box(bx,by,80,vh-by);ink(44,53,79);box(bx+12,by-16,56,22);}
 for(const r of world.solids){
  const x=r.x-X,y=r.y-Y;if(x+r.w<0||x>w||y>vh||y+r.h<0)continue;
  ink(91,66,89);box(x,y,r.w,r.h);ink(113,77,103);box(x,y+7,r.w,8);ink(167,137,126);box(x,y,r.w,4);
  ink(68,52,76);if(r.h>23)for(let bx=Math.max(r.x,Math.floor(X/32)*32);bx<Math.min(r.x+r.w,X+w);bx+=32){box(bx-X,y+23,2,r.h-23);box(bx-X+8,y+17,4,3);}
  if(r.h<30){ink(225,190,137);box(x+3,y+4,r.w-6,2);}
 }
 for(const spring of world.springs){const x=spring.x-X,y=spring.y-Y;ink(130,240,177);box(x,y-6,spring.w,5);ink(60,126,111);for(let i=3;i<spring.w-3;i+=8)line(x+i,y-5,x+i+4,y);}
 for(const o of world.objects){const x=Math.round(o.x-X),y=Math.round(o.y-Y);if(x+o.w<0||x-o.w>w||y-o.h>vh)continue;
  if(o.kind==='ball'){ink(219,117,145);circle(x,y,o.w/2,true);ink(255,193,161);circle(x-3,y-4,o.w/2-4,true);ink(255,232,180);box(x-5,y-7,4,3);}
  else{const left=x-o.w/2,top=y-o.h/2;ink(83,54,66);box(left,top,o.w,o.h);ink(190,128,84);box(left+2,top+2,o.w-4,o.h-4);ink(237,184,121);box(left+3,top+3,o.w-6,3);line(left+4,top+6,left+o.w-5,top+o.h-5);line(left+o.w-5,top+6,left+4,top+o.h-5);}
 }
 const px=Math.round(p.x-X),py=Math.round(p.y-Y),f=p.grounded&&Math.abs(p.vx)>15?Math.floor(world.time*12)%4:0;
 paste(guys[f],px-13,py-19);
 ink(43,35,58);box(px+(facing>0?2:-7),py-8,2,3);box(px+(facing>0?7:-2),py-8,2,3);
 if(!p.grounded){ink(255,191,151);box(px-12,py+1,3,3);box(px+10,py+1,3,3);}
 ink(229,196,151);write('LOVE U FIA',{x:45-X,y:420-Y,size:1,font:'font_1'});
 if(2600-X<w){ink(223,180,131);box(2600-X,340-Y,3,140);ink(245,97,151);box(2603-X,340-Y,56,31);}
 if(world.finish){ink(255,220,155);write('NICE JUMPING!',{x:w/2-78,y:24,size:2,font:'font_1'});}
 const now=Date.now();frames++;if(now-fpsAt>=500){fps=frames*1000/(now-fpsAt);frames=0;fpsAt=now;}
 ink(18,23,39);box(0,vh,w,54);
 ink(250,212,150);write(`HELD: ${held.length?held.join(' '):'-'}`,{x:10,y:vh+8,size:1,font:'font_1'});
 ink(137,233,183);write(`${fps.toFixed(0)} FPS  ${Math.abs(p.vx).toFixed(0)} PX/S`,{x:w-138,y:vh+8,size:1,font:'font_1'});
 ink(203,208,221);write('A: jump (hold higher)   B: shove   X: ball   Y: reset',{x:10,y:vh+25,size:1,font:'font_1'});
 ink(147,166,187);write('Pad/stick: move   RB/RT: run   M: sound   Green pads: spring',{x:10,y:vh+40,size:1,font:'font_1'});
 if(!pad.ready||!pad.connected||now-pad.at>1500){ink(255,190,140);write('CONTROLLER DISCONNECTED - ARROWS + SPACE WORK',{x:10,y:10,size:1,font:'font_1'});}
}
export function act({event:e,system}){
 if(e.is('keyboard:down')){if(!keys.has(e.key))keyboardPressed.add(e.key);keys.add(e.key);}
 if(e.is('keyboard:up'))keys.delete(e.key);
 if(e.is('keyboard:down:m'))soundOn=!soundOn;
 if(e.is('keyboard:down:escape'))system.jump('prompt');
}
