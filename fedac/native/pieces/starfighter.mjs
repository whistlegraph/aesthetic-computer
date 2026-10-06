// Starfighter, 26.10.04
// A cockpit space fighter: steer, fire, boost, shields and homing missiles.
import { savedWifi } from '../lib/saved-wifi.mjs';
const connectSaved=savedWifi();
const clamp=(v,a,b)=>Math.max(a,Math.min(b,v));
const add=(a,b)=>({x:a.x+b.x,y:a.y+b.y,z:a.z+b.z});
const sub=(a,b)=>({x:a.x-b.x,y:a.y-b.y,z:a.z-b.z});
const mul=(v,s)=>({x:v.x*s,y:v.y*s,z:v.z*s});
const dot=(a,b)=>a.x*b.x+a.y*b.y+a.z*b.z;
const length=v=>Math.hypot(v.x,v.y,v.z);
const unit=v=>mul(v,1/(length(v)||1));
export function basis(yaw,pitch){
 const forward={x:Math.sin(yaw)*Math.cos(pitch),y:Math.sin(pitch),z:Math.cos(yaw)*Math.cos(pitch)};
 const right={x:Math.cos(yaw),y:0,z:-Math.sin(yaw)};
 const up={x:-Math.sin(yaw)*Math.sin(pitch),y:Math.cos(pitch),z:-Math.cos(yaw)*Math.sin(pitch)};
 return {forward,right,up};
}
export function segmentSphere(a,b,c,r){const v=sub(b,a),n=dot(v,v),t=n?clamp(dot(sub(c,a),v)/n,0,1):0;return length(sub(add(a,mul(v,t)),c))<=r;}
export function createFlight(seed=417){
 const s={pos:{x:0,y:0,z:0},yaw:0,pitch:0,turn:0,tilt:0,bank:0,health:100,energy:100,kills:0,score:0,time:0,fire:0,missile:0,roll:0,rollTime:0,lastHit:-10,spawn:1,enemies:[],shots:[],sparks:[],stars:[],rocks:[],id:0,shield:false,boost:false,events:[],seed};
 s.random=()=>{s.seed=(Math.imul(s.seed,1664525)+1013904223)>>>0;return s.seed/4294967296;};
 for(let i=0;i<220;i++)s.stars.push({x:(s.random()-.5)*1600,y:(s.random()-.5)*1600,z:(s.random()-.5)*1600});
 // Separate deterministic rock placement leaves combat's random stream intact.
 for(let i=0;i<28;i++){
  const angle=i*2.39996,distance=130+(i%7)*78,radius=14+(i*17%29);
  s.rocks.push({pos:{x:Math.cos(angle)*(80+i%5*48),y:Math.sin(angle)*(65+i%4*40),z:distance},radius,color:i%3===0?[162,124,100]:i%3===1?[107,127,153]:[144,112,143]});
 }
 return s;
}
function target(s,b){let best=null,alignment=.973;for(const e of s.enemies){const a=dot(unit(sub(e.pos,s.pos)),b.forward);if(a>alignment){alignment=a;best=e;}}return best;}
function explode(s,p,color,count=16){for(let i=0;i<count;i++)s.sparks.push({pos:{...p},vel:{x:(s.random()-.5)*80,y:(s.random()-.5)*80,z:(s.random()-.5)*80},life:.5+s.random()*.6,color});if(s.sparks.length>200)s.sparks.splice(0,s.sparks.length-200);}
function damage(s,amount){if(s.rollTime>0)return;if(s.shield){s.energy=Math.max(0,s.energy-amount*.7);s.events.push('shield');return;}s.health=Math.max(0,s.health-amount);s.lastHit=s.time;s.events.push('hit');}
export function flightStep(s,input,dt){
 dt=clamp(dt,0,1/30);s.events=[];s.time+=dt;
 for(const k of ['fire','missile','roll','rollTime'])s[k]=Math.max(0,s[k]-dt);
 if(s.health<=0)return;
 s.turn+=(clamp(input.x||0,-1,1)*1.15-s.turn)*(1-Math.exp(-7*dt));
 s.tilt+=(-clamp(input.y||0,-1,1)*.85-s.tilt)*(1-Math.exp(-7*dt));
 s.yaw+=s.turn*dt;s.pitch=clamp(s.pitch+s.tilt*dt,-1.15,1.15);s.bank+=(-s.turn*.3-s.bank)*(1-Math.exp(-5*dt));
 const b=basis(s.yaw,s.pitch);
 s.boost=!!input.boost&&s.energy>3;s.shield=!!input.shield&&s.energy>3;
 s.energy=clamp(s.energy+(s.boost?-24:0)*dt+(s.shield?-26:0)*dt+(!s.boost&&!s.shield?19:0)*dt,0,100);
 if(input.roll&&!s.roll&&s.energy>=20){s.roll=3;s.rollTime=.75;s.energy-=20;s.events.push('roll');}
 s.pos=add(s.pos,mul(b.forward,(s.boost?190:75)*dt));
 for(const star of s.stars)for(const axis of ['x','y','z']){if(star[axis]-s.pos[axis]>800)star[axis]-=1600;if(star[axis]-s.pos[axis]<-800)star[axis]+=1600;}
 for(const rock of s.rocks){
  const d=sub(rock.pos,s.pos);
  if(length(d)>1000||dot(d,b.forward)<-220){
   const angle=(s.time*.37+rock.radius)*2.39996,range=90+rock.radius*4;
   rock.pos=add(add(s.pos,mul(b.forward,650+rock.radius*3)),add(mul(b.right,Math.cos(angle)*range),mul(b.up,Math.sin(angle)*range)));
  }
 }
 if(input.fire&&!s.fire){
  const lock=target(s,b),aim=lock?unit(sub(lock.pos,s.pos)):b.forward;
  for(const sign of [-1,1]){const pos=add(add(s.pos,mul(b.right,sign*2.5)),mul(b.forward,5));s.shots.push({pos,old:pos,vel:mul(aim,650),life:1.6,enemy:false,missile:false});}
  s.fire=.15;s.events.push('fire');
 }
 if(input.missile&&!s.missile){const lock=target(s,b);if(lock){const pos=add(s.pos,mul(b.forward,8));s.shots.push({pos,old:pos,vel:mul(b.forward,260),life:5,enemy:false,missile:true,target:lock.id});s.missile=4;s.events.push('missile');}}
 s.spawn-=dt;
 if(s.spawn<=0&&s.enemies.length<6){
  const distance=260+s.random()*180,offset=add(mul(b.right,(s.random()-.5)*distance*.55),mul(b.up,(s.random()-.5)*distance*.35));
  const pos=add(add(s.pos,mul(b.forward,distance)),offset);
  s.enemies.push({id:++s.id,pos,hp:2,fire:2+s.random()*2,phase:s.random()*6.28,age:0});s.spawn=Math.max(1.5,3.5-s.kills*.035);
 }
 for(const e of s.enemies){
  e.age+=dt;const d=sub(s.pos,e.pos),range=length(d),approach=unit(d);
  // A slow weave makes the intercept visible without jerky screen-space motion.
  e.pos=add(e.pos,add(mul(approach,(range<85?-20:32)*dt),mul(b.right,Math.sin(e.age*1.4+e.phase)*9*dt)));
  e.fire-=dt;
  if(e.fire<=0&&range<500){const aim=unit(sub(add(s.pos,mul(b.forward,(s.boost?190:75)*range/330*.45)),e.pos));const pos={...e.pos};s.shots.push({pos,old:pos,vel:mul(aim,235),life:4,enemy:true});e.fire=Math.max(1.1,2.5-s.kills*.025)+s.random();}
  if(range<10){damage(s,12);e.hp=0;explode(s,e.pos,'orange');}
  if(range>1000)e.hp=0;
 }
 for(const shot of s.shots){
  shot.old=shot.pos;
  if(shot.missile){const e=s.enemies.find(e=>e.id===shot.target&&e.hp>0);if(e)shot.vel=mul(unit(add(mul(unit(shot.vel),.85),mul(unit(sub(e.pos,shot.pos)),.15))),310);}
  shot.pos=add(shot.pos,mul(shot.vel,dt));shot.life-=dt;
  if(shot.enemy){if(segmentSphere(shot.old,shot.pos,s.pos,5)){damage(s,9);shot.life=0;}}
  else for(const e of s.enemies)if(e.hp>0&&segmentSphere(shot.old,shot.pos,e.pos,8)){
   e.hp-=shot.missile?4:1;shot.life=0;
   if(e.hp<=0){s.kills++;s.score+=100;explode(s,e.pos,'orange');s.events.push('kill');s.health=Math.min(100,s.health+2);}else{explode(s,e.pos,'cyan',4);s.events.push('impact');}break;
  }
 }
 s.enemies=s.enemies.filter(e=>e.hp>0);s.shots=s.shots.filter(p=>p.life>0).slice(-120);
 for(const p of s.sparks){p.pos=add(p.pos,mul(p.vel,dt));p.life-=dt;}s.sparks=s.sparks.filter(p=>p.life>0);
}
export function autopilot(s){
 const b=basis(s.yaw,s.pitch);
 const e=[...s.enemies].sort((a,c)=>length(sub(a.pos,s.pos))-length(sub(c.pos,s.pos)))[0];
 if(!e)return {x:Math.sin(s.time*.3)*.12,y:s.pitch*.8,fire:false};
 const d=sub(e.pos,s.pos),range=length(d),yaw=Math.atan2(d.x,d.z),pitch=Math.atan2(d.y,Math.hypot(d.x,d.z));
 const turn=Math.atan2(Math.sin(yaw-s.yaw),Math.cos(yaw-s.yaw)),alignment=dot(unit(d),b.forward);
 const danger=s.shots.some(p=>p.enemy&&length(sub(p.pos,s.pos))<65);
 return {x:clamp(turn*3,-1,1),y:clamp((s.pitch-pitch)*4,-1,1),fire:alignment>.96,missile:alignment>.985&&range>140,shield:danger,roll:danger&&s.energy>35,boost:range>430&&alignment>.98&&s.energy>50};
}
let state=createFlight(),pad={},previous=0,keys=new Set(),freshKeys=new Set(),last=0,accum=0,helperAt=0,pending={},fps=0,fpsAt=0,frames=0,sound=true;
let auto=true,manualUntil=0,deathAt=0;
function startReader(system){system.pty2?.spawn('/bin/sh',['-c','exec /mnt/tools/ac-usb-controller'],40,10);helperAt=Date.now();}
export function boot({system}){system.startSSH?.();last=fpsAt=Date.now();try{pad=JSON.parse(system.readFile('/tmp/ac-controller.json')||'{}');}catch{}if(!pad.at||Date.now()-pad.at>1500)startReader(system);}
// Bounded, frame-driven audio: no timer catch-up or lingering music loops.
export function createSoundtrack(){
 const voices=[],cooldown={};let nextBeat=0,step=0,side=1;
 const hz=n=>440*2**((n-69)/12);
 function stop(){for(const v of voices)v.voice.kill?.(.025);voices.length=0;nextBeat=0;}
 function tick(api,events,now,enabled=true){
  if(!enabled){stop();return;}
  for(let i=voices.length-1;i>=0;i--){const v=voices[i],t=(now-v.at)/(v.duration*1000);if(t>=1){voices.splice(i,1);continue;}if(v.from!==v.to)v.voice.update?.({tone:v.from*(v.to/v.from)**t});}
  function note(from,to,duration,volume,pan=0,attack=.004){
   if(voices.length>=16)return;
   const voice=api.sound?.synth?.({type:'sine',tone:from,duration,volume,pan,attack,decay:duration*.85});
   if(voice)voices.push({voice,at:now,from,to,duration});
  }
  for(const event of new Set(events)){
   if(now<(cooldown[event]||0))continue;cooldown[event]=now+(event==='fire'?110:180);
   if(event==='fire'){side=-side;note(290,65,.16,.055,side*.3);}
   else if(event==='kill'||event==='hit'){note(110,28,.65,.13);note(73,34,.48,.08,-.2);note(175,45,.26,.045,.2);}
   else if(event==='missile')note(190,38,.42,.085);
   else if(event==='impact')note(145,55,.14,.055);
   else if(event==='shield')note(196,98,.24,.055);
   else if(event==='roll')note(65,130,.35,.05);
  }
  if(now>=nextBeat){
   // 96 BPM, eighth notes. A stalled frame advances once, never a burst.
   nextBeat=now+312.5;const beat=step%16,bar=Math.floor(step/16)%4,root=[36,32,39,34][bar];
   if(beat%4===0){note(hz(root),hz(root),1.05,.065,0,.012);note(78,35,.16,.04);}
   const melody=[12,19,15,22,19,15,24,22,19,12,15,19,22,19,15,10];
   note(hz(root+melody[beat]),hz(root+melody[beat]),beat%4===3?.5:.27,.028,beat%2?.28:-.28,.012);
   if(beat===0){note(hz(root+7),hz(root+7),2.3,.018,-.35,.25);note(hz(root+12),hz(root+12),2.3,.018,.35,.25);}
   step++;
  }
 }
 return {tick,stop,get voices(){return voices.length;}};
}
const soundtrack=createSoundtrack();
function play(api,events){soundtrack.tick(api,events,Date.now(),sound);}
export function leave(){soundtrack.stop();}
export function sim(api){
 connectSaved(api);const now=Date.now(),dt=clamp((now-last)/1000,0,.1);last=now;accum=Math.min(.1,accum+dt);
 try{pad=JSON.parse(api.system.readFile('/tmp/ac-controller.json')||'{}');}catch{pad={};}
 if(!pad.at||now-pad.at>1500){pad={};if(now-helperAt>5000)startReader(api.system);}
 const b=pad.connected&&pad.ready?pad.buttons:0,pressed=b&~previous;previous=b;
 if(pressed&4||freshKeys.has('p'))auto=!auto;
 if(pressed&0x80||freshKeys.has('r')){state=createFlight();accum=0;pending={};}
 let x=(b&0x800||keys.has('arrowright')?1:0)-(b&0x400||keys.has('arrowleft')?1:0),y=(b&0x200||keys.has('arrowdown')?1:0)-(b&0x100||keys.has('arrowup')?1:0);
 if(!x&&pad.ready&&Math.abs(pad.lx)>6500)x=clamp(pad.lx/32767,-1,1);if(!y&&pad.ready&&Math.abs(pad.ly)>6500)y=clamp(pad.ly/32767,-1,1);
 pending.roll ||= !!(pressed&0x1000)||freshKeys.has('l');pending.missile ||= !!(pressed&0x40)||freshKeys.has('x');freshKeys.clear();
 const input={x,y,fire:!!(b&0x10)||pad.rt>100||keys.has('space'),boost:!!(b&0x2000)||keys.has('shift'),shield:!!(b&0x20)||keys.has('b')};
 if(x||y||input.fire||input.boost||input.shield||pending.roll||pending.missile)manualUntil=now+4000;
 if(state.health>0)deathAt=0;else if(!deathAt)deathAt=now;
 if(auto&&deathAt&&now-deathAt>3000){state=createFlight();deathAt=0;}
 let events=[];while(accum>=1/120){flightStep(state,auto&&now>=manualUntil?autopilot(state):{...input,...pending},1/120);events.push(...state.events);pending={};accum-=1/120;}play(api,events);
}
// Analytic ray/ellipsoid intersection. Rays have view-space z=1, so the
// returned parameter is directly comparable to projected object depth.
export function rayEllipsoid(dx,dy,x,y,z,rx,ry,rz){
 const ix=1/(rx*rx),iy=1/(ry*ry),iz=1/(rz*rz);
 const a=dx*dx*ix+dy*dy*iy+iz,b=-(x*dx*ix+y*dy*iy+z*iz),c=x*x*ix+y*y*iy+z*z*iz-1;
 const discriminant=b*b-a*c;if(discriminant<0)return Infinity;
 const root=Math.sqrt(discriminant),near=(-b-root)/a,far=(-b+root)/a;
 return near>1?near:far>1?far:Infinity;
}
let rayCell=8,rayDepth=null,rayCols=0,rayRows=0,rayLastMS=0,raySmoothMS=0,rayCooldown=0,renderFrame=0,shipDepth=null,shipCols=0,shipRows=0;
let nativeShapes=new Float64Array(128*16),nativeUniforms=new Float64Array(21),nativeDepth=null;
function castScene(api,s,b,camera,third,cos,sin,cx,cy,f){
 const {screen,ink,box}=api,w=screen.width,h=third?screen.height:Math.ceil(screen.height*.78),shapes=[];
 let detail=false;
 const shape=(pos,rx,ry,rz,color,rock=false,glow=false)=>{
  const d=sub(pos,camera),x=dot(d,b.right),y=dot(d,b.up),z=dot(d,b.forward),r=Math.max(rx,ry,rz);
  if(z+r<2)return;
  const sx=cx+(x*cos+y*sin)*f/Math.max(2,z),sy=cy+(x*sin-y*cos)*f/Math.max(2,z);
  // Bound each ellipsoid axis separately. A thin wing must not cast rays
  // across the large square enclosing a sphere with the same wingspan.
  const spanR=z>rz?f*(rx+Math.abs(x)*rz/z)/(z-rz):Math.max(w,h)*2;
  const spanU=z>rz?f*(ry+Math.abs(y)*rz/z)/(z-rz):Math.max(w,h)*2;
  const spanX=Math.abs(cos)*spanR+Math.abs(sin)*spanU,spanY=Math.abs(sin)*spanR+Math.abs(cos)*spanU;
  if(sx+spanX<0||sx-spanX>w||sy+spanY<0||sy-spanY>h)return;
  shapes.push({x,y,z,rx,ry,rz,color,rock,glow,detail,left:Math.max(0,sx-spanX),right:Math.min(w,sx+spanX),top:Math.max(0,sy-spanY),bottom:Math.min(h,sy+spanY)});
 };
 // Actual sphere surfaces, rather than painted discs. The planet remains far
 // away; rocks wrap through a world-space field as the ship flies past.
 shape(add(s.pos,mul({x:.44,y:.19,z:.88},2500)),230,230,230,[77,114,167]);
 for(const rock of s.rocks)shape(rock.pos,rock.radius,rock.radius,rock.radius,rock.color,true);
 detail=true;
 const part=(pos,x,y,z)=>add(pos,add(mul(b.right,x),add(mul(b.up,y),mul(b.forward,z))));
 for(const e of s.enemies){
  shape(e.pos,4,3,8,[217,102,71]);
  for(const side of [-1,1]){shape(part(e.pos,side*8,0,-1),5,1.3,5,[150,68,77]);shape(part(e.pos,side*10,1,-2),1.1,2.5,5,[247,169,110]);}
  shape(part(e.pos,0,2,3),2,1.5,2,[246,220,137],false,true);
 }
 if(third){
  shape(s.pos,3,2,12,[96,166,182]);shape(part(s.pos,0,-.6,-4),12,.9,5,[89,174,186]);
  shape(part(s.pos,0,2,3),1.8,1.4,4,[255,202,114]);
  for(const sign of [-1,1]){shape(part(s.pos,sign*5,-.3,-5),1.6,1.4,5,[60,114,153]);shape(part(s.pos,sign*5,-.3,-10),1,1,s.boost?5:2,[124,217,250],false,true);}
 }
 if(api.raycast){
  const start=Date.now(),light=unit({x:-.5,y:.7,z:-.4});
  shapes.sort((a,b)=>Number(a.detail)-Number(b.detail)||a.z-b.z);
  if(!nativeDepth||nativeDepth.length!==w*h)nativeDepth=new Float32Array(w*h);
  for(let i=0;i<shapes.length;i++){
   const o=shapes[i];nativeShapes.set([o.x,o.y,o.z,o.rx,o.ry,o.rz,...o.color,+o.rock,+o.glow,+o.detail,o.left,o.right,o.top,o.bottom],i*16);
  }
  nativeUniforms.set([w,h,cx,cy,f,cos,sin,dot(light,b.right),dot(light,b.up),dot(light,b.forward),b.right.x,b.right.y,b.right.z,b.up.x,b.up.y,b.up.z,b.forward.x,b.forward.y,b.forward.z,2,1]);
  api.raycast(nativeShapes,shapes.length,nativeUniforms,nativeDepth);
  rayLastMS=Date.now()-start;raySmoothMS=raySmoothMS*.85+rayLastMS*.15;
  return {cell:2,cols:Math.ceil(w/2),rows:Math.ceil(h/2),depthAt:(x,y)=>x<0||y<0||x>=w||y>=h?Infinity:nativeDepth[Math.floor(y)*w+Math.floor(x)]};
 }
 let estimate=0;for(const o of shapes)if(!o.detail)estimate+=(o.right-o.left)*(o.bottom-o.top)/(rayCell*rayCell);
 const cell=Math.max(rayCell,estimate>14000?12:8);
 const cols=Math.ceil(w/cell),rows=Math.ceil(h/cell);
 if(!rayDepth||cols!==rayCols||rows!==rayRows){rayCols=cols;rayRows=rows;rayDepth=new Float32Array(cols*rows);}
 rayDepth.fill(Infinity);
 const fineCols=Math.ceil(w/2),fineRows=Math.ceil(h/2);
 if(!shipDepth||shipCols!==fineCols||shipRows!==fineRows){shipCols=fineCols;shipRows=fineRows;shipDepth=new Float32Array(fineCols*fineRows);}
 shipDepth.fill(Infinity);
 const light=unit({x:-.5,y:.7,z:-.4}),lx=dot(light,b.right),ly=dot(light,b.up),lz=dot(light,b.forward);
 const start=Date.now();
 for(const o of shapes.sort((a,b)=>Number(a.detail)-Number(b.detail)||a.z-b.z)){
  // Keep ships legible with fine rays after the coarse environment pass.
  const step=o.detail?2:cell,outCols=o.detail?fineCols:cols,outRows=o.detail?fineRows:rows,depth=o.detail?shipDepth:rayDepth;
  const ix=1/(o.rx*o.rx),iy=1/(o.ry*o.ry),iz=1/(o.rz*o.rz),c=o.x*o.x*ix+o.y*o.y*iy+o.z*o.z*iz-1;
  const left=Math.floor(o.left/step),right=Math.min(outCols,Math.ceil(o.right/step)),top=Math.floor(o.top/step),bottom=Math.min(outRows,Math.ceil(o.bottom/step));
  for(let row=top;row<bottom;row++){
   const v=((row+.5)*step-cy)/f,u=((left+.5)*step-cx)/f;
   let dx=u*cos+v*sin,dy=u*sin-v*cos;
   for(let col=left;col<right;col++,dx+=step/f*cos,dy+=step/f*sin){
    const a=dx*dx*ix+dy*dy*iy+iz,q=-(o.x*dx*ix+o.y*dy*iy+o.z*iz),disc=q*q-a*c;if(disc<0)continue;
    const root=Math.sqrt(disc);let t=(-q-root)/a;if(t<=1)t=(-q+root)/a;
    const index=row*outCols+col;
    const environment=o.detail?rayDepth[Math.floor((row+.5)*step/cell)*cols+Math.floor((col+.5)*step/cell)]:Infinity;
    if(t<=1||t>=depth[index]||t>=environment)continue;depth[index]=t;
    let nx=(t*dx-o.x)*ix,ny=(t*dy-o.y)*iy,nz=(t-o.z)*iz;
    const norm=1/Math.sqrt(nx*nx+ny*ny+nz*nz);nx*=norm;ny*=norm;nz*=norm;
    let shade=o.glow?1:.18+.82*Math.max(0,nx*lx+ny*ly+nz*lz);
    if(o.rock){
     const wx=nx*b.right.x+ny*b.up.x+nz*b.forward.x,wy=nx*b.right.y+ny*b.up.y+nz*b.forward.y,wz=nx*b.right.z+ny*b.up.z+nz*b.forward.z;
     const grain=((Math.floor(wx*13)*73856093)^(Math.floor(wy*13)*19349663)^(Math.floor(wz*13)*83492791))>>>0;
     shade*=grain%11<2?.48:.78+(grain%5)*.07;
    }
    // Quantized illumination keeps the ray layer deliberately pixelated.
    shade=Math.round(shade*32)/32;const fog=clamp(1-t/1700,.3,1);
    ink(Math.round(o.color[0]*shade*fog+6*(1-fog)),Math.round(o.color[1]*shade*fog+13*(1-fog)),Math.round(o.color[2]*shade*fog+28*(1-fog)));
    box(col*step,row*step,Math.min(step,w-col*step),Math.min(step,h-row*step));
   }
  }
 }
 rayLastMS=Date.now()-start;raySmoothMS=raySmoothMS*.85+rayLastMS*.15;
 if(++rayCooldown>=45){if(raySmoothMS>7&&rayCell<12)rayCell+=2;else if(raySmoothMS<4&&rayCell>8)rayCell-=2;rayCooldown=0;}
 return {cell,cols,rows,depthAt:(x,y)=>x<0||y<0||x>=w||y>=h?Infinity:Math.min(rayDepth[Math.floor(y/cell)*cols+Math.floor(x/cell)],shipDepth[Math.floor(y/2)*fineCols+Math.floor(x/2)])};
}

export function paint(api){
 const {wipe,ink,box,line,circle,write,screen}=api,w=screen.width,h=screen.height,s=state,b=basis(s.yaw,s.pitch),cx=w*.5,cy=h*.43,f=w*.68;
 const third=true,distance=120;
 const camera=third?add(sub(s.pos,mul(b.forward,distance)),mul(b.up,distance*.18)):s.pos;
 const roll=s.bank+(s.rollTime>0?(1-s.rollTime/.75)*Math.PI*2:0),cos=Math.cos(roll),sin=Math.sin(roll);
 const project=p=>{const d=sub(p,camera),z=dot(d,b.forward);if(z<2)return null;let x=dot(d,b.right)*f/z,y=-dot(d,b.up)*f/z;return {x:cx+x*cos-y*sin,y:cy+x*sin+y*cos,z,scale:f/z};};
 const visible=p=>p&&p.x>-30&&p.y>-30&&p.x<w+30&&p.y<h+30;
 const text=(t,x,y,size=1)=>write(t,{x:Math.round(x),y:Math.round(y),size,font:'font_1'});
 wipe(4,7,19);
 // Tiny distant stars and longer nearby trails describe the ship's speed.
 for(let i=0;i<s.stars.length;i++){const star=s.stars[i],p=project(star);if(!visible(p))continue;const v=clamp(230-p.z*.15,60,220);ink(i%4===0?v*.6:v,v*.8,v);const old=project(add(star,mul(b.forward,s.boost?25:6)));if(old&&s.boost)line(old.x,old.y,p.x,p.y);else box(p.x,p.y,p.z<110?2:1,1);}
 const rays=castScene(api,s,b,camera,third,cos,sin,cx,cy,f);
 const unobscured=(p,bias=4)=>visible(p)&&p.z<rays.depthAt(p.x,p.y)+bias;
 const lock=target(s,b);
 for(const e of [...s.enemies].sort((a,b)=>length(sub(b.pos,s.pos))-length(sub(a.pos,s.pos)))){
  const p=project(e.pos);if(!unobscured(p,12))continue;const k=p.scale;
  if(lock===e){ink(112,248,216);const r=clamp(15*k,10,60);for(const sign of [-1,1]){line(p.x+sign*r,p.y-r,p.x+sign*(r-5),p.y-r);line(p.x+sign*r,p.y+r,p.x+sign*(r-5),p.y+r);line(p.x+sign*r,p.y-r,p.x+sign*r,p.y-r+5);line(p.x+sign*r,p.y+r,p.x+sign*r,p.y+r-5);}text(`${Math.round(p.z)}m`,p.x+r+4,p.y-3);}
 }
 for(const shot of s.shots){const p=project(shot.pos),tail=project(sub(shot.pos,mul(unit(shot.vel),shot.missile?18:12)));if(!unobscured(p)||!tail)continue;ink(...(shot.enemy?[255,82,106]:shot.missile?[255,221,134]:[114,250,230]));line(tail.x,tail.y,p.x,p.y);if(shot.enemy)box(p.x-1,p.y-1,3,3);}
 for(const spark of s.sparks){const p=project(spark.pos);if(!unobscured(p))continue;ink(...(spark.color==='orange'?[255,145+Math.round(spark.life*70),90]:[120,230,255]));box(p.x,p.y,Math.min(4,Math.max(1,p.scale*2)),2);}
 const aim=third?project(add(s.pos,mul(b.forward,240))):{x:cx,y:cy};
 ink(...(lock?[118,250,210]:[95,152,174]));line(aim.x-11,aim.y,aim.x-5,aim.y);line(aim.x+5,aim.y,aim.x+11,aim.y);line(aim.x,aim.y-10,aim.x,aim.y-5);line(aim.x,aim.y+5,aim.x,aim.y+10);box(aim.x,aim.y,1,1);
 // Canopy rails: their heavy outer lines frame a clear central windshield.
 if(!third)for(const sign of [-1,1]){const x=sign<0?0:w;for(let i=-5;i<=5;i++){ink(17,30,45);line(x,h*.8+i,cx+sign*w*.40,h*.11+i);line(cx+sign*w*.40,h*.11+i,cx+sign*w*.20,i);}ink(65,87,103);line(x,h*.8-6,cx+sign*w*.40,h*.11-6);line(cx+sign*w*.40,h*.11-6,cx+sign*w*.20,-6);}
 const deck=Math.round(h*.78);
 if(!third){
 ink(13,24,35);box(0,deck,w,h-deck);ink(61,80,94);line(0,deck,w,deck);ink(29,44,57);box(0,deck+3,w,5);
 // Shield and energy instruments flank a small live radar.
 ink(3,12,21);box(12,deck+15,150,39);box(w-162,deck+15,150,39);box(cx-55,deck+10,110,51);
 ink(119,187,197);text('HULL',20,deck+21);text(s.shield?'SHIELD ACTIVE':'ENERGY',w-154,deck+21);
 for(let i=0;i<20;i++){ink(...(i<s.health/5?(s.health<30?[255,111,102]:[103,232,185]):[28,49,56]));box(20+i*6,deck+35,4,8);ink(...(i<s.energy/5?[118,198,249]:[28,49,56]));box(w-154+i*6,deck+35,4,8);}
 ink(32,79,83);circle(cx,deck+35,22,false);line(cx-45,deck+35,cx+45,deck+35);line(cx,deck+14,cx,deck+57);
 for(const e of s.enemies){const d=sub(e.pos,s.pos),rx=clamp(dot(d,b.right)*.08,-46,46),rz=clamp(dot(d,b.forward)*.045,-20,20);ink(255,155,113);box(cx+rx-1,deck+35-rz-1,3,3);}
 ink(121,244,205);box(cx-1,deck+34,3,3);
 }
 const now=Date.now();frames++;if(now-fpsAt>=500){fps=frames*1000/(now-fpsAt);frames=0;fpsAt=now;}
 if(++renderFrame%60===0)api.system.writeFile?.('/tmp/ac-starfighter-render.json',JSON.stringify({fps,rayMS:rayLastMS,averageRayMS:raySmoothMS,cell:rays.cell,view:third?'chase':'cockpit',time:s.time,audioVoices:soundtrack.voices,sound}));
 ink(168,199,211);text(`FIA-01   ${auto&&Date.now()>=manualUntil?'AUTO':s.boost?'BOOST':'MANUAL'} / ${third?'CHASE':'COCKPIT'}`,14,14);text(`${s.score} PTS   ${fps.toFixed(0)} FPS`,w-132,14);text(`RAY ${Math.ceil(w/rays.cell)}x${Math.ceil((third?h:h*.78)/rays.cell)}`,14,28);
 ink(139,226,207);text(s.missile>0?`MISSILE ${s.missile.toFixed(1)}s`:lock?'X: MISSILE LOCK':'MISSILE READY',cx-51,third?h-29:deck-15);
 ink(179,197,205);text('A/RT FIRE  B SHIELD  X MISSILE  RB BOOST  LB ROLL  MENU AUTO',Math.max(8,cx-168),h-12);
 if(s.shield){ink(78,176,193);line(12,40,12,deck-15);line(w-13,40,w-13,deck-15);}
 if(s.time-s.lastHit<.25){ink(240,76,105);for(let i=0;i<3;i++){line(i,i,w-1-i,i);line(i,i,i,deck);line(w-1-i,i,w-1-i,deck);}}
 if(!pad.ready||!pad.connected||now-pad.at>1500){ink(238,190,132);text('ARROWS STEER / SPACE FIRES / USB CONTROLLER OFFLINE',20,34);}
 if(s.health<=0){ink(5,10,23);box(cx-136,cy-38,272,76);ink(255,140,147);text('SHIP LOST',cx-54,cy-25,2);ink(196,221,226);text(`${s.kills} DRONES DOWN  /  Y TO RETRY`,cx-87,cy+10);}
}
export function act({event:e,system}){if(e.is('keyboard:down')){if(!keys.has(e.key))freshKeys.add(e.key);keys.add(e.key);}if(e.is('keyboard:up'))keys.delete(e.key);if(e.is('keyboard:down:m')){sound=!sound;if(!sound)soundtrack.stop();}if(e.is('keyboard:down:escape'))system.jump('prompt');}
