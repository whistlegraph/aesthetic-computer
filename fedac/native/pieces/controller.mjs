// Controller, 26.10.13
// Test USB gamepad buttons, stick travel, and analog triggers.
// Control guide: https://download.8bitdo.com/Manual/Controller/Ultimate/Ultimate-2-Wireless-Controller.pdf
import { savedWifi } from '../lib/saved-wifi.mjs';
const connectSaved=savedWifi();
const controls = [
  ['U',0x100,84,162],['D',0x200,84,218],
  ['L',0x400,56,190],['R',0x800,112,190],
  ['A',0x10,516,218],['B',0x20,544,190],
  ['X',0x40,488,190],['Y',0x80,516,162],
  ['LB',0x1000,92,116],['RB',0x2000,508,116],
  ['VIEW',8,248,116],['MENU',4,352,116],
  ['LS',0x4000,228,270],['RS',0x8000,372,270],
  ['HOME',0,300,116],
];
const localControls=[['L4*',164,116],['R4*',436,116],['PL*',84,270],
  ['PR*',516,270],['SQ*',274,150],['STAR*',326,150],['PROF*',300,84]];
const colors = {bg:[18,23,32],fg:[232,239,246],dim:[145,160,180],
  idle:[50,63,81],seen:[112,223,178],held:[255,130,177],warn:[255,201,114]};
let pad={},fresh=false,active=false,seen=0,seenHome=false,peaks=[0,0];
let helperAt=0,rateAt=0,rateReports=0,rate=0;
let showGuide=false,showOverlay=false;
let paintAt=0,paintFrames=0,fps=0;
const world={x:0,z:0,y:0,vy:0,yaw:0,pitch:.32,heading:Math.PI,walk:0,time:0,last:0,a:false};
const clamp=(n,a,b)=>Math.max(a,Math.min(b,n));

function read(system) {
  try { pad=JSON.parse(system.readFile('/tmp/ac-controller.json')||'{}')||{}; }
  catch { pad={}; }
  fresh=Number.isFinite(pad.at)&&Math.abs(Date.now()-pad.at)<1500;
  active=!!(fresh&&pad.connected&&pad.ready);
}
function startReader(system) {
  helperAt=Date.now();
  // The reader's flock prevents duplicate processes across piece visits.
  system.pty2?.spawn('/bin/sh',['-c',
    'if [ -x /tmp/ac-usb-controller ]; then exec /tmp/ac-usb-controller; '+
    'elif [ -x /usr/bin/ac-usb-controller ]; then exec /usr/bin/ac-usb-controller; '+
    'elif [ -x /mnt/tools/ac-usb-controller ]; then exec /mnt/tools/ac-usb-controller; fi'],40,10);
}
export function boot({system}) {
  if(!system.readFile('/tmp/dropbear.pid'))system.startSSH?.();
  read(system);rateAt=Date.now();rateReports=pad.reports||0;
  if(!fresh)startReader(system);
}
export function sim(api) {
  connectSaved(api);
  const {system}=api;
  read(system);const now=Date.now();
  const dt=world.last?clamp((now-world.last)/1000,0,.05):0;
  world.last=now;world.time+=dt;
  const axis=v=>Math.abs(v||0)<4000?0:clamp(v/32767,-1,1);
  const ax=active?axis(pad.lx):0,az=active?axis(pad.ly):0;
  world.yaw+=(active?axis(pad.rx):0)*dt*1.8;
  world.pitch=clamp(world.pitch+(active?axis(pad.ry):0)*dt*.65,.2,.9);
  const length=Math.max(1,Math.hypot(ax,az));
  const dx=(ax*Math.cos(world.yaw)+az*Math.sin(world.yaw))/length;
  const dz=(-ax*Math.sin(world.yaw)+az*Math.cos(world.yaw))/length;
  world.x+=dx*dt*3.5;world.z+=dz*dt*3.5;
  if(dx||dz){world.heading=Math.atan2(dx,dz);world.walk+=Math.hypot(dx,dz)*dt*10;}
  const a=active&&!!(pad.buttons&0x10);
  if(a&&!world.a&&world.y===0)world.vy=5;
  world.a=a;world.vy-=dt*12;world.y=Math.max(0,world.y+world.vy*dt);
  if(!world.y)world.vy=0;
  if(!fresh&&now-helperAt>5000)startReader(system);
  if(active) {
    seen|=pad.buttons;seenHome=seenHome||!!pad.guide;
    peaks[0]=Math.max(peaks[0],pad.lt||0);peaks[1]=Math.max(peaks[1],pad.rt||0);
  }
  if(now-rateAt>=1000) {
    rate=active?Math.max(0,Math.round(((pad.reports||0)-rateReports)*1000/(now-rateAt))):0;
    rateReports=pad.reports||0;rateAt=now;
  }
}
function backdrop({screen,wipe,ink,box,line,circle}) {
  const w=screen.width,h=screen.height,focal=h*.95;
  wipe(117,181,213);
  const horizon=clamp(Math.round(h*.55-focal*Math.tan(world.pitch)),0,h);
  for(let y=0;y<horizon;y+=8){const t=y/Math.max(1,horizon);ink(117+Math.round(t*45),181+Math.round(t*28),213+Math.round(t*13));box(0,y,w,Math.min(8,horizon-y));}
  if(horizon>20){
    ink(255,229,160);circle(Math.round(w*.79),Math.round(horizon*.34),Math.max(5,Math.round(h*.045)),true);
    ink(237,245,237);
    for(let i=0;i<4;i++){
      const x=((i*w*.3+world.time*3-world.yaw*90)%(w+100)+w+100)%(w+100)-50,y=horizon*(.25+(i%2)*.35);
      circle(Math.round(x),Math.round(y),Math.round(h*.024),true);
      circle(Math.round(x+9),Math.round(y-3),Math.round(h*.033),true);
      circle(Math.round(x+20),Math.round(y),Math.round(h*.024),true);
    }
  }
  for(let y=horizon;y<h;y+=8){const t=(y-horizon)/Math.max(1,h-horizon);ink(107-Math.round(t*28),156-Math.round(t*25),98-Math.round(t*26));box(0,y,w,Math.min(8,h-y));}
  const cy=Math.cos(world.yaw),sy=Math.sin(world.yaw),cp=Math.cos(world.pitch),sp=Math.sin(world.pitch);
  const camera=[world.x+sy*cp*8,2+sp*8,world.z+cy*cp*8];
  function view(p){
    const x=p[0]-camera[0],y=p[1]-camera[1],z=p[2]-camera[2];
    return [x*cy-z*sy,-x*sy*sp+y*cp-z*cy*sp,-x*sy*cp-y*sp-z*cy*cp];
  }
  const project=p=>[w/2+p[0]*focal/p[2],h*.55-p[1]*focal/p[2]];
  function edge(a,b,thickness=1){
    let av=view(a),bv=view(b);
    if(av[2]<.3&&bv[2]<.3)return;
    if(av[2]<.3||bv[2]<.3){
      const t=(.3-av[2])/(bv[2]-av[2]);
      const p=av.map((v,i)=>v+(bv[i]-v)*t);
      if(av[2]<.3)av=p;else bv=p;
    }
    const [x,y]=project(av),[xx,yy]=project(bv),dx=xx-x,dy=yy-y;
    // Clip before Bresenham: near-camera ground lines can project far offscreen.
    let lo=0,hi=1;
    for(const [p,q] of [[-dx,x],[dx,w-1-x],[-dy,y],[dy,h-1-y]]){
      if(!p){if(q<0)return;continue;}
      const t=q/p;if(p<0)lo=Math.max(lo,t);else hi=Math.min(hi,t);
      if(lo>hi)return;
    }
    line(Math.round(x+dx*lo),Math.round(y+dy*lo),Math.round(x+dx*hi),Math.round(y+dy*hi),thickness);
  }
  const gx=Math.floor(world.x/2)*2,gz=Math.floor(world.z/2)*2;
  ink(99,145,91);
  for(let i=-12;i<=12;i++){
    edge([gx+i*2,0,gz-24],[gx+i*2,0,gz+24]);
    edge([gx-24,0,gz+i*2],[gx+24,0,gz+i*2]);
  }
  function cube(x,y,z,r,angle){
    const c=Math.cos(angle),s=Math.sin(angle),points=[];
    for(let i=0;i<8;i++){
      const a=(i&1?r:-r),b=(i&4?r:-r);
      points.push([x+a*c+b*s,y+(i&2?r:-r),z-a*s+b*c]);
    }
    for(let i=0;i<8;i++)for(const bit of [1,2,4])if(!(i&bit))edge(points[i],points[i|bit]);
  }
  const tileX=Math.floor(world.x/24)*24,tileZ=Math.floor(world.z/24)*24;
  for(const [i,x,z] of [[0,-7,-5],[1,7,-8],[2,-11,7],[3,10,8],[4,-4,-17],[5,15,-19]]){
    const px=tileX+x,pz=tileZ+z,t=world.time*.6+i;
    ink(83,131,107);cube(px,1.4,pz,.7,0);
    ink(244,214,147);cube(px,3+Math.sin(t)*.35,pz,.45,t*.5);
    edge([px,0,pz],[px,1.4,pz]);
  }
  // Trees share the same ground and camera as the walkers.
  const trees=[[-4,-4],[6,-8],[-10,-12],[11,4],[-7,10]]
    .map(([x,z],i)=>({x:tileX+x,z:tileZ+z,i,depth:view([tileX+x,2,tileZ+z])[2]}))
    .sort((a,b)=>b.depth-a.depth);
  for(const tree of trees){
    if(tree.depth<.5)continue;
    const {x,z,i}=tree,thick=Math.max(2,Math.round(focal*.18/tree.depth));
    const shadow=view([x,.02,z]);
    if(shadow[2]>.3){const q=project(shadow);ink(66,119,73);circle(Math.round(q[0]),Math.round(q[1]),Math.max(1,Math.round(focal*.65/shadow[2])),true);}
    ink(114,83,62);edge([x,0,z],[x,2.8,z],thick);
    edge([x,1.6,z],[x-.5,2.7,z],thick);edge([x,1.9,z],[x+.5,3,z],thick);
    for(const [dx,dy,dz,r,tint] of [[-.55,2.85,0,.9,0],[.6,3.1,.1,1,1],[0,3.75,0,.9,2]]){
      const sway=Math.sin(world.time*.9+i)*.06,p=view([x+dx+sway,dy,z+dz]);
      if(p[2]<.3)continue;
      const q=project(p);ink(42+tint*13,109+tint*18,70+tint*8);
      circle(Math.round(q[0]),Math.round(q[1]),Math.max(1,Math.round(focal*r/p[2])),true);
    }
  }
  // Pedestrians wander around the neighboring blocks.
  const walkers=Array.from({length:8},(_,i)=>{
    const t=world.time*(.12+(i%3)*.025)+i*2.4;
    const radius=4+(i%4)*2;
    const x=tileX+Math.sin(t)*radius+(i%2?5:-5);
    const z=tileZ+Math.cos(t*.8)*radius-3;
    return {i,x,z,heading:Math.atan2(Math.cos(t),-.8*Math.sin(t*.8)),depth:view([x,1,z])[2]};
  }).sort((a,b)=>b.depth-a.depth);
  const coats=[[104,198,192],[245,184,100],[147,156,233],[214,132,181]];
  for(const p of walkers){
    if(p.depth<.5)continue;
    const c=Math.cos(p.heading),s=Math.sin(p.heading);
    const part=(x,y,z)=>[p.x+x*c+z*s,y,p.z-x*s+z*c];
    const thick=Math.max(1,Math.round(focal*.10/p.depth));
    const swing=Math.sin(world.time*(4+p.i%3)+p.i)*.26;
    ink(52+p.i*4,65+p.i*3,89+p.i*2);
    edge(part(-.15,.7,0),part(-.15,.05,swing),thick);
    edge(part(.15,.7,0),part(.15,.05,-swing),thick);
    ink(238,229,209);
    edge(part(-.15,.05,swing),part(-.15,.05,swing+.16),thick+1);
    edge(part(.15,.05,-swing),part(.15,.05,-swing+.16),thick+1);
    ink(...coats[p.i%coats.length]);
    edge(part(0,.7,0),part(0,1.18,0),thick*3);
    edge(part(-.22,1.1,0),part(-.34,.7,-swing),thick);
    edge(part(.22,1.1,0),part(.34,.7,swing),thick);
    ink(...coats[(p.i+2)%coats.length]);
    if(p.i%2)edge(part(.2,.8,-.18),part(.2,1.12,-.18),thick*3);
    else edge(part(-.15,1.2,.1),part(.15,1.2,.1),thick*2);
    const head=view(part(0,1.48,0));
    if(head[2]>.3){
      const q=project(head),r=Math.max(1,Math.round(focal*.2/head[2]));
      ink(224-p.i*7,183-p.i*6,145-p.i*4);circle(Math.round(q[0]),Math.round(q[1]),r,true);
      if(p.i%3===0){ink(...coats[(p.i+1)%coats.length]);box(Math.round(q[0]-r),Math.round(q[1]-r),r*2+2,Math.max(2,r));}
      else {ink(62+p.i*8,47+p.i*5,47);line(Math.round(q[0]-r*.7),Math.round(q[1]-r*.6),Math.round(q[0]+r*.7),Math.round(q[1]-r*.6),Math.max(1,thick));}
    }
  }
  // The player stays in front of the camera.
  const c=Math.cos(world.heading),s=Math.sin(world.heading);
  const part=(x,y,z)=>[world.x+x*c+z*s,world.y+y,world.z-x*s+z*c];
  const foot=view([world.x,0,world.z]);
  if(foot[2]>.3){const p=project(foot);ink(6,13,23);circle(Math.round(p[0]),Math.round(p[1]),Math.round(focal*.22/foot[2]),true);}
  const depth=view(part(0,1,0))[2],thick=Math.max(2,Math.round(focal*.12/Math.max(1,depth)));
  const swing=Math.sin(world.walk)*.28;
  ink(236,160,117);
  edge(part(-.18,.7,0),part(-.18,.08,swing),thick);
  edge(part(.18,.7,0),part(.18,.08,-swing),thick);
  ink(245,126,164);edge(part(0,.72,0),part(0,1.22,0),thick*3);
  ink(255,193,146);
  edge(part(-.25,1.15,0),part(-.4,.72,-swing),thick);
  edge(part(.25,1.15,0),part(.4,.72,swing),thick);
  const head=view(part(0,1.55,0));
  if(head[2]>.3){const p=project(head);ink(255,209,153);circle(Math.round(p[0]),Math.round(p[1]),Math.round(focal*.23/head[2]),true);}
  ink(181,110,102);cube(world.x,world.y+1.55,world.z,.24,world.heading);
}
export function paint({screen,wipe,ink,box,line,circle,write}) {
  const now=Date.now();paintFrames++;
  if(!paintAt)paintAt=now;
  if(now-paintAt>=750){fps=Math.round(paintFrames*1000/(now-paintAt));paintFrames=0;paintAt=now;}
  if(showGuide)wipe(...colors.bg);
  else {
    backdrop({screen,wipe,ink,box,line,circle});
    if(!showOverlay)return;
  }
  const scale=Math.min(screen.width/600,screen.height/340);
  const ox=Math.floor((screen.width-600*scale)/2),oy=Math.floor((screen.height-340*scale)/2);
  const px=x=>Math.round(ox+x*scale),py=y=>Math.round(oy+y*scale);
  const color=name=>ink(...colors[name]);
  const rect=(x,y,w,h,outline=false)=>box(px(x),py(y),Math.max(1,Math.round(w*scale)),Math.max(1,Math.round(h*scale)),outline?'outline':'fill');
  const stroke=(x,y,x2,y2)=>line(px(x),py(y),px(x2),py(y2));
  const ring=(x,y,r,fill=false)=>circle(px(x),py(y),Math.round(r*scale),fill);
  function text(str,x,y,size=1,center=false) {
    const fontSize=Math.max(1,Math.floor(size*scale));
    const width=str.length*6*fontSize;
    write(str,{x:px(x)-(center==='right'?width:center?Math.round(width/2):0),y:py(y),size:fontSize,font:'font_1'});
  }
  if(!showGuide){ink(...colors.bg,210);rect(12,8,576,49);}
  color('fg');text(pad.device||'USB controller',24,14,2);
  color('dim');text(`${fps} FPS`,576,18,1,'right');
  color(active?'seen':'warn');
  const status=!fresh?'USB reader unavailable':!pad.connected?'Connect an 8BitDo by USB':!pad.ready?'Wake controller / press a button':`USB   ${rate} reports/s`;
  text(active&&!showGuide?`${status}   LS move / RS look / A jump`:status,24,40);
  if(showGuide) {
    const entries=[
      ['VIEW (-), left of HOME: View / Back'],
      ['SQUARE, below-left of HOME','Maps the extra buttons. Hold an extra + target',
        'button(s), then press SQUARE. Extra + SQUARE clears.'],
      ['L4 / R4 + PL / PR: no separate USB indicator',
        'Hold L4 + A, press SQUARE. L4 then lights A.',
        'Hold R4 + B, press SQUARE. R4 then lights B.',
        'PL/PR work the same. LS/RS cannot be targets.'],
      ['STAR','Press: lights. Hold a button + press STAR: turbo.',
        'Turbo excludes D-pad, HOME, LS and RS.'],
      ['PROFILE: switches saved controller profiles.'],
    ];
    let y=59;
    for(const [title,...lines] of entries) {
      color('warn');text(title,24,y);y+=18;
      color('fg');for(const str of lines) { text(str,24,y);y+=18; }
      y+=4;
    }
    color('dim');text('Source: 8BitDo Ultimate 2 Wireless manual',24,316);
    text('H close',576,316,1,'right');
    return;
  }
  if(!fresh||!pad.connected) { color('dim');text('Ultimate 2 Wireless / Arcade supported',24,58); }

  function trigger(label,value,peak,x) {
    const max=pad.triggerMax||1023,v=active?clamp(value||0,0,max):0;
    color(v?'held':'fg');text(`${label} ${Math.round(v/max*100)}%`,x,76);
    color('idle');rect(x,92,180,10);
    if(peak) { color('seen');rect(x,92,180*clamp(peak/max,0,1),10,true); }
    if(v) { color('held');rect(x,92,180*v/max,10); }
  }
  trigger('LT',pad.lt,peaks[0],48);trigger('RT',pad.rt,peaks[1],372);

  for(const [label,bit,x,y] of controls) {
    const held=active&&(bit?pad.buttons&bit:pad.guide);
    const tested=bit?seen&bit:seenHome;
    const width=label.length>2?48:38;
    color(held?'held':'idle');rect(x-width/2,y-11,width,25);
    if(tested&&!held) { color('seen');rect(x-width/2,y-11,width,25,true); }
    color(held?'bg':tested?'seen':'fg');text(label,x,y-4,1,true);
  }
  if(pad.product===0x310b)for(const [label,x,y] of localControls) {
    const width=label.length>4?54:46;
    color('idle');rect(x-width/2,y-11,width,25,true);
    color('warn');text(label,x,y-4,1,true);
  }
  function stick(x,y,vx,vy) {
    const ax=active?(vx||0):0,ay=active?(vy||0):0;
    const nx=clamp(ax/32767,-1,1),ny=clamp(ay/32767,-1,1);
    ink(...colors.bg,175);ring(x,y,38,true);
    color('idle');ring(x,y,38);stroke(x-38,y,x+38,y);stroke(x,y-38,x,y+38);
    color('dim');ring(x,y,4);
    color(active?'held':'dim');ring(x+nx*35,y+ny*35,5,true);
    color('fg');text(`${ax},${ay}`,x,y+43,1,true);
  }
  stick(228,198,pad.lx,pad.ly);stick(372,198,pad.rx,pad.ry);
  const tested=controls.reduce((n,[,bit])=>n+(bit?!!(seen&bit):seenHome),0);
  ink(...colors.bg,220);rect(12,287,576,49);
  const compact=scale<.75;
  color('dim');text(compact?`${tested}/15 tested | extra buttons: H`:`${tested}/15 buttons tested`,300,compact?294:289,1,true);
  if(pad.product===0x310b&&!compact) {
    color('warn');text('L4/R4/PL/PR send mapped buttons. H: setup',300,302,1,true);
  }
  color('held');text('held',24,316);
  color('seen');text('tested',84,316);
  color('warn');text('* on pad',164,316);
  color('dim');text('H guide  R reset  Esc back',576,316,1,'right');
}
export function act({event,system}) {
  if(event.is('keyboard:down:o')) { showOverlay=!showOverlay;showGuide=false; }
  if(event.is('keyboard:down:h'))showGuide=!showGuide;
  if(event.is('keyboard:down:r')) { seen=0;seenHome=false;peaks=[0,0]; }
  if(event.is('keyboard:down:escape')) { if(showGuide)showGuide=false;else system.jump('prompt'); }
}
