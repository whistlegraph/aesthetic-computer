// Bunny Hop, 26.10.04
// A 3D bunny hops across friendly turtle backs to reach Fia's island.
import { savedWifi } from '../lib/saved-wifi.mjs';
const connectSaved=savedWifi(),STEP=1/120;
const clamp=(v,a,b)=>Math.max(a,Math.min(b,v));
export function createMeadow(){
 const turtles=Array.from({length:12},(_,i)=>({x:Math.sin(i*1.15)*29,z:(i+1)*66,r:26,top:8,visited:false}));
 return {p:{x:0,y:9,z:0,vx:0,vy:0,vz:0,ground:true},turtles,time:0,checkpoint:-1,visited:0,apples:[],appleCount:0,nextApple:0,appleSerial:0,bird:null,nextBird:10,buffer:0,coyote:.1,events:[],won:false,splashes:0};
}
function platforms(s){return [{x:0,z:0,r:44,top:9,index:-1},...s.turtles.map((t,i)=>({...t,index:i})),{x:0,z:858,r:47,top:9,index:12}];}
function orchardStep(s,dt){
 const p=s.p;
 // Only one apple grows at a time; ripe and fallen apples may coexist.
 if(s.time>=s.nextApple&&!s.apples.some(a=>!a.falling&&a.age<7)){
  const nearby=[0,3,6,9].filter(i=>Math.abs(s.turtles[i].z-p.z)<260);
  const tree=nearby.sort((a,b)=>Math.abs(s.turtles[a].z-p.z)-Math.abs(s.turtles[b].z-p.z))[0]??9,t=s.turtles[tree];
  s.apples.push({id:s.appleSerial++,tree,x:t.x+8,z:t.z,y:48,age:0,vy:0,falling:false,ground:false});s.nextApple=s.time+7.5;
 }
 for(const a of s.apples){
  a.age+=dt;
  if(a.age>12)a.falling=true;
  if(a.falling&&!a.ground){
   a.vy-=95*dt;a.y+=a.vy*dt;
   const t=s.turtles[a.tree];
   if(a.y<=t.top+3&&Math.hypot(a.x-t.x,a.z-t.z)<t.r){a.y=t.top+3;a.vy=0;a.ground=true;}
  }
  if(a.falling&&a.age>=7&&Math.hypot(a.x-p.x,a.z-p.z)<13&&Math.abs(a.y-(p.y+12))<20){a.taken=true;s.appleCount++;s.events.push('apple');}
 }
 s.apples=s.apples.filter(a=>!a.taken&&a.y>-20&&a.age<35).slice(-10);
 if(s.time>=s.nextBird&&!s.bird){
  const a=s.apples.find(a=>a.age>=7&&!a.falling);
  if(a){s.bird={apple:a.id,time:0,x:a.x-110,y:a.y+38,z:a.z-15,shaken:false};s.nextBird=s.time+17;}
 }
 if(s.bird){
  const b=s.bird,a=s.apples.find(a=>a.id===b.apple);b.time+=dt;
  if(a&&b.time<1.5){const blend=Math.min(1,b.time/1.5);b.x=a.x-110*(1-blend);b.y=53+30*(1-blend);b.z=a.z;}
  else if(b.time<2.5){b.y=53+Math.sin(b.time*35)*2;if(a&&!b.shaken){a.falling=true;b.shaken=true;s.events.push('bird');}}
  else{b.x+=65*dt;b.y+=24*dt;}
  if(b.time>5)s.bird=null;
 }
}
export function meadowStep(s,input,dt=STEP){
 const p=s.p;s.time+=dt;s.events=[];
 if(s.won)input={};
 s.buffer=input.jumpPressed?.13:Math.max(0,s.buffer-dt);
 s.coyote=p.ground?.1:Math.max(0,s.coyote-dt);
 if(s.buffer>0&&s.coyote>0){p.vy=83;p.ground=false;s.buffer=s.coyote=0;s.events.push('hop');}
 let ax=input.x||0,az=input.z||0;const length=Math.hypot(ax,az);if(length>1){ax/=length;az/=length;}
 const speed=78,accel=p.ground?450:280;
 p.vx+=clamp(ax*speed-p.vx,-accel*dt,accel*dt);p.vz+=clamp(az*speed-p.vz,-accel*dt,accel*dt);
 const old=p.y;p.x+=p.vx*dt;p.z+=p.vz*dt;
 p.vy-=(input.jumpHeld&&p.vy>0?142:195)*dt;p.y+=p.vy*dt;p.ground=false;
 for(const t of platforms(s))if(p.vy<=0&&old>=t.top-.1&&p.y<=t.top&&Math.hypot(p.x-t.x,p.z-t.z)<t.r-2){
  p.y=t.top;p.vy=0;p.ground=true;
  if(t.index>=0&&t.index<12){
   s.checkpoint=t.index;
   if(!s.turtles[t.index].visited){s.turtles[t.index].visited=true;s.visited++;s.events.push('turtle');if([0,3,6,9].includes(t.index))s.restUntil=s.time+12;}
  }
  if(t.index===12&&s.visited===12&&!s.won){s.won=true;s.events.push('win');}
  break;
 }
 if(p.y<-32){
  const t=s.checkpoint<0?{x:0,z:0,top:9}:s.turtles[s.checkpoint];
  Object.assign(p,{x:t.x,y:t.top,z:t.z,vx:0,vy:0,vz:0,ground:true});s.buffer=0;s.coyote=.1;s.splashes++;s.events.push('splash');
 }
 orchardStep(s,dt);
}
export function bunnyPilot(s){
 if(s.time<(s.restUntil||0)){const t=s.turtles[s.checkpoint],dx=t.x+8-s.p.x;return {x:Math.abs(dx)>2?clamp(dx*.05,-.2,.2):0,z:Math.abs(t.z-s.p.z)>2?clamp((t.z-s.p.z)*.05,-.2,.2):0,jumpPressed:s.p.ground,jumpHeld:false};}
 const index=s.turtles.findIndex(t=>!t.visited),target=index<0?{x:0,z:858}:s.turtles[index],p=s.p;
 const dx=target.x-p.x,dz=target.z-p.z,d=Math.hypot(dx,dz);
 return {x:d>3?dx/d:0,z:d>3?dz/d:0,jumpPressed:p.ground&&!s.won,jumpHeld:true};
}
let world=createMeadow(),pad={},previous=0,last=0,accum=0,helperAt=0,pendingJump=false;
let demo=true,manualUntil=0,sound=true,toneAt=0,musicAt=0,musicStep=0,winAt=0,frames=0,fps=60,fpsAt=0,camX=0,camZ=0,camYaw=.35,camHeight=185,rayMS=0;
const keys=new Set(),pressedKeys=new Set(),voices=[];
function reader(system){system.pty2?.spawn('/bin/sh',['-c','exec /mnt/tools/ac-usb-controller'],40,10);helperAt=Date.now();}
function reset(){world=createMeadow();accum=0;camX=0;camZ=0;camYaw=.35;camHeight=185;winAt=0;}
export function boot({system}){system.startSSH?.();last=fpsAt=Date.now();try{pad=JSON.parse(system.readFile('/tmp/ac-controller.json')||'{}');}catch{}if(!pad.at||Date.now()-pad.at>1500)reader(system);}
function stop(){for(const v of voices)v.voice.kill?.(.025);voices.length=0;}
export function leave(){stop();}
function note(api,tone,duration,volume,now){if(!sound||voices.length>=8)return;const voice=api.sound?.synth?.({type:'sine',tone,volume,duration,attack:.012,decay:duration*.85});if(voice)voices.push({voice,end:now+duration*1000});}
export function sim(api){
 connectSaved(api);const now=Date.now(),dt=clamp((now-last)/1000,0,.1);last=now;accum=Math.min(.1,accum+dt);
 for(let i=voices.length-1;i>=0;i--)if(now>=voices[i].end)voices.splice(i,1);
 try{pad=JSON.parse(api.system.readFile('/tmp/ac-controller.json')||'{}');}catch{pad={};}
 if(!pad.at||now-pad.at>1500){pad={};if(now-helperAt>5000)reader(api.system);}
 const b=pad.connected&&pad.ready?pad.buttons:0,pressed=b&~previous;previous=b;
 let x=(b&0x800||keys.has('arrowright')?1:0)-(b&0x400||keys.has('arrowleft')?1:0),z=(b&0x100||keys.has('arrowup')?1:0)-(b&0x200||keys.has('arrowdown')?1:0);
 pendingJump ||= !!(pressed&0x10)||pressedKeys.has('space');pressedKeys.clear();
 const input={x:x*Math.cos(camYaw)+z*Math.sin(camYaw),z:z*Math.cos(camYaw)-x*Math.sin(camYaw),jumpHeld:!!(b&0x10)||keys.has('space')};
 if(x||z||input.jumpHeld||pendingJump)manualUntil=now+8000;
 demo=now>=manualUntil;const auto=demo;
 if(world.won){if(!winAt)winAt=now;if(now-winAt>5000||pendingJump)reset();}
 let events=[];while(accum>=STEP){meadowStep(world,auto?bunnyPilot(world):{...input,jumpPressed:pendingJump},STEP);events.push(...world.events);pendingJump=false;accum-=STEP;}
 if(sound&&events.length&&now-toneAt>70){toneAt=now;const e=['win','apple','bird','turtle','splash','hop'].find(e=>events.includes(e));note(api,{win:262,apple:247,bird:294,turtle:196,splash:55,hop:110}[e],.24,.065,now);}
 if(sound&&now>=musicAt){musicAt=now+410;const melody=[0,7,12,7,4,7,9,4,0,5,9,5,2,5,7,2];note(api,130.81*2**(melody[musicStep%16]/12),.35,.02,now);if(musicStep%4===0)note(api,musicStep%16<8?65.41:43.65,1.2,.035,now);musicStep++;}
 camYaw+=(.18+.52*(.5+.5*Math.sin(world.p.z*.009-.6))-camYaw)*(1-Math.exp(-1.5*dt));
 camHeight+=(175+24*Math.sin(world.p.z*.007)+Math.max(0,world.p.y-9)*.35-camHeight)*(1-Math.exp(-2.5*dt));
 camX+=(world.p.x-camX)*(1-Math.exp(-6*dt));camZ+=(world.p.z-camZ)*(1-Math.exp(-7*dt));
}
const shapes=new Float64Array(128*16),uniforms=new Float64Array(21);let depth;
export function paint(api){
 const {screen,wipe,ink,box,circle,line,write}=api,w=screen.width,h=screen.height,s=world,p=s.p;
 const text=(str,x,y,size=1)=>write(str,{x:Math.round(x),y:Math.round(y),size,font:'font_1'});
 // A rail-like camera eases around the route and lifts to frame a jump.
 const ox=Math.sin(camYaw)*240,oz=Math.cos(camYaw)*240,dy=camHeight-10,length=Math.hypot(240,dy);
 const camera={x:camX-ox,y:camHeight,z:camZ-oz};
 const forward={x:ox/length,y:-dy/length,z:oz/length},right={x:Math.cos(camYaw),y:0,z:-Math.sin(camYaw)},up={x:-forward.y*Math.sin(camYaw),y:240/length,z:-forward.y*Math.cos(camYaw)};
 const f=w*.95,cx=w*.5,cy=h*.58;
 const view=(x,y,z)=>{const dx=x-camera.x,dy=y-camera.y,dz=z-camera.z;return {x:dx*right.x+dz*right.z,y:dx*up.x+dy*up.y+dz*up.z,z:dx*forward.x+dy*forward.y+dz*forward.z};};
 const project=(x,y,z)=>{const v=view(x,y,z);if(v.z<2)return null;return {x:cx+v.x*f/v.z,y:cy-v.y*f/v.z,z:v.z,k:f/v.z,vx:v.x,vy:v.y};};
 wipe(85,161,166);
 for(let i=0;i<50;i++){const wz=Math.floor(p.z/60)*60+(i%10)*60-120,wx=(Math.floor(i/10)-2)*155+Math.sin(s.time+i)*4,q=project(wx,-6,wz);if(q&&q.y>0&&q.y<h){ink(128,196,192);box(q.x,q.y,Math.max(2,12*q.k),1);}}
 // Soft-edged blob shadows anchor turtles and trees to the water.
 const blob=(x,y,z,r,color)=>{const q=project(x,y,z);if(!q)return;const rx=r*q.k,ry=rx*.55;if(q.x+rx<0||q.x-rx>w||q.y+ry<0||q.y-ry>h)return;ink(...color);for(let row=-Math.ceil(ry);row<=ry;row+=2){const half=rx*Math.sqrt(Math.max(0,1-row*row/(ry*ry)));box(q.x-half,q.y+row,half*2,2);}};
 for(const t of s.turtles)if(t.z>p.z-100&&t.z<p.z+285){blob(t.x+6,-8,t.z+5,30,[77,148,151]);blob(t.x+6,-8,t.z+5,23,[67,135,141]);}
 for(const i of [0,3,6,9]){const t=s.turtles[i];if(t.z>p.z-90&&t.z<p.z+285)blob(t.x+27,-8,t.z+14,31,[70,139,144]);}
 let count=0;
 const shape=(x,y,z,rx,ry,rz,color,detail=false)=>{
  if(count>=128)return;const q=project(x,y,z);if(!q)return;
  // Conservative projected bounds include camera pitch and ellipsoid depth.
  const spanZ=Math.abs(forward.x)*rx+Math.abs(forward.y)*ry+Math.abs(forward.z)*rz,spanY=Math.abs(up.x)*rx+up.y*ry+Math.abs(up.z)*rz,spanX=Math.abs(right.x)*rx+Math.abs(right.z)*rz,near=Math.max(1,q.z-spanZ);
  const sx=f*(spanX+Math.abs(q.vx)*spanZ/q.z)/near,viewY=q.vy,sy=f*(spanY+Math.abs(viewY)*spanZ/q.z)/near;
  const viewRX=Math.hypot(right.x*rx,right.z*rz),viewRY=Math.hypot(up.x*rx,up.y*ry,up.z*rz),viewRZ=Math.hypot(forward.x*rx,forward.y*ry,forward.z*rz);
  if(q.x+sx<0||q.x-sx>w||q.y+sy<0||q.y-sy>h)return;
  // View-space ellipsoids are deliberately stylized rounded toy shapes.
  shapes.set([q.vx,viewY,q.z,viewRX,viewRY,viewRZ,...color,0,0,+detail,Math.max(0,q.x-sx),Math.min(w,q.x+sx),Math.max(0,q.y-sy),Math.min(h,q.y+sy)],count++*16);
 };
 for(const z of [0,858]){shape(0,-1,z,48,10,48,[148,183,108]);shape(0,-8,z,50,8,50,[185,146,99]);}
 for(let i=0;i<s.turtles.length;i++){
  const t=s.turtles[i];if(t.z<p.z-70||t.z>p.z+285)continue;
  const x=t.x,z=t.z,wiggle=Math.sin(s.time*4+i)*2;
  shape(x,0,z,26,8,26,t.visited?[128,182,102]:[69,135,94]);
  shape(x,5,z,16,4,16,t.visited?[185,210,121]:[122,174,101]);
  shape(x-13,3,z-10,8,3,8,[154,191,110]);shape(x+13,3,z+8,8,3,8,[154,191,110]);
  shape(x,-1,z-30,9,6,10,[191,207,124]);
  shape(x-4,3,z-37,1.7,2,1.5,[37,57,62]);shape(x+4,3,z-37,1.7,2,1.5,[37,57,62]);
  for(const side of [-1,1]){shape(x+side*26,-3,z-14+wiggle,9,2.5,7,[158,190,103]);shape(x+side*24,-3,z+15-wiggle,8,2.5,7,[158,190,103]);}
 }
 // Orchard branches hang over the turtle route; fruit falls onto shells.
 for(const i of [0,3,6,9]){
  const t=s.turtles[i];if(t.z<p.z-70||t.z>p.z+285)continue;
  const shake=s.bird&&s.bird.time>1.5&&s.bird.time<2.5&&Math.abs(s.bird.z-t.z)<3?Math.sin(s.time*40)*2:0;
  shape(t.x+56,10,t.z,18,8,22,[155,178,108]);
  shape(t.x+56,31,t.z,4,25,4,[151,104,80]);
  shape(t.x+31+shake,47,t.z,28,3,4,[151,104,80]);
  shape(t.x+52+shake,61,t.z,21,17,19,[95,151,91]);
  shape(t.x+24+shake,64,t.z,22,15,18,[130,174,98]);
 }
 for(const a of s.apples){
  if(a.z<p.z-70||a.z>p.z+285)continue;
  const size=1+Math.min(1,a.age/5)*3,color=a.age<4?[151,193,98]:a.age<7?[237,190,81]:[235,87,80];
  shape(a.x,a.y,a.z,size,size*1.05,size,color,true);shape(a.x+.9,a.y+size+1,a.z,1.6,1,1,[76,134,83],true);
 }
 if(s.bird){const b=s.bird,flap=Math.sin(s.time*22)*4;shape(b.x,b.y,b.z,4,3.5,6,[109,128,195],true);shape(b.x,b.y+3,b.z-4,3,3,3,[160,176,222],true);shape(b.x,b.y+2,b.z-7,1.5,1,2,[247,197,98],true);for(const side of [-1,1])shape(b.x+side*6,b.y+flap,b.z,5,1,3,[87,107,175],true);}
 // Bunny body, long pink-lined ears, feet and a cotton tail, all raycast.
 const bounce=p.ground?Math.sin(s.time*9)*.3:0,y=p.y;
 shape(p.x,y+10,p.z,6,9,6,[251,237,216],true);
 shape(p.x,y+20+bounce,p.z+1,7,7,6,[255,244,224],true);
 for(const side of [-1,1]){
  shape(p.x+side*3.6,y+32+bounce,p.z+1,2.3,9,2.2,[252,237,220],true);
  shape(p.x+side*3.6,y+32+bounce,p.z-1,1.1,6.3,1,[232,157,169],true);
  shape(p.x+side*4,y+2,p.z+2,3.2,2.2,5,[237,215,200],true);
  shape(p.x+side*6.4,y+22,p.z+2,1.2,1.6,1.8,[55,58,69],true);
 }
 shape(p.x,y+7,p.z-6,3.6,3.6,3.6,[255,248,234],true);
 shape(p.x,y+18,p.z+7,2,1.4,1.5,[231,149,157],true);
 if(!depth||depth.length!==w*h)depth=new Float32Array(w*h);
 uniforms.set([w,h,cx,cy,f,1,0,-.4,.65,-.64,right.x,right.y,right.z,up.x,up.y,up.z,forward.x,forward.y,forward.z,2,1]);
 const start=Date.now();if(api.raycast)api.raycast(shapes,count,uniforms,depth);rayMS=Date.now()-start;
 // The bunny's shadow stays on the landing surface while the body jumps.
 const landing=s.turtles.find(t=>Math.hypot(t.x-p.x,t.z-p.z)<t.r-3),onIsland=Math.hypot(p.x,p.z)<44||Math.hypot(p.x,p.z-858)<47;
 const shadow=project(p.x,landing?landing.top+2:onIsland?11:-7,p.z);
 if(shadow){const radius=(7+Math.max(0,p.y-9)*.035)*shadow.k,ry=radius*.55;ink(...(landing||onIsland?[57,89,63]:[48,111,121]));for(let row=-Math.ceil(ry);row<=ry;row++){
  const y=Math.round(shadow.y+row);if(y<0||y>=h)continue;const half=radius*Math.sqrt(Math.max(0,1-row*row/(ry*ry)));let run=-1;
  const left=Math.max(0,Math.floor(shadow.x-half)),right=Math.min(w-1,Math.ceil(shadow.x+half));
  for(let x=left;x<=right+1;x++){const visible=x<=right&&depth[y*w+x]>shadow.z-12;if(visible&&run<0)run=x;if(!visible&&run>=0){box(run,y,x-run,1);run=-1;}}
 }}
 const next=s.turtles.findIndex(t=>!t.visited),target=next<0?{x:0,z:858}:s.turtles[next];
 const q=project(target.x,22+Math.sin(s.time*3)*2,target.z);
 if(q&&q.z<540){ink(255,226,146);line(q.x-5,q.y-5,q.x,q.y);line(q.x,q.y,q.x+5,q.y-5);}
 const goal=project(0,32,858);if(goal&&goal.z<390){ink(255,236,182);text('FIA',goal.x-9,goal.y);}
 const now=Date.now();frames++;if(now-fpsAt>=500){fps=frames*1000/(now-fpsAt);frames=0;fpsAt=now;}
 ink(219,235,203);box(5,5,185,44);ink(40,75,72);text('BUNNY HOP',12,12,2);text(`${s.visited}/12 TURTLES   ${s.appleCount} APPLES`,12,34);text(`${fps.toFixed(0)} FPS`,w-48,13);
 if(demo)text('AUTO HOP / D-PAD TO PLAY',w-144,29);
 ink(38,75,73);box(0,h-22,w,22);ink(244,237,199);text('D-PAD MOVE      A HOP / HOLD HIGHER',10,h-14);
 if(s.won){ink(254,239,203);box(w/2-118,h/2-35,236,70);ink(67,107,79);text('LOVE U FIA',w/2-60,h/2-25,2);text(`${s.appleCount} APPLES / A TO HOP AGAIN`,w/2-99,h/2+10);}
 if(frames===1)api.system.writeFile?.('/tmp/ac-bunny-hop.json',JSON.stringify({fps,rayMS,x:p.x,y:p.y,z:p.z,turtles:s.visited,apples:s.appleCount,bird:!!s.bird,won:s.won,splashes:s.splashes,auto:demo,cameraYaw:camYaw,cameraHeight:camHeight}));
}
export function act({event:e,system}){
 if(e.is('keyboard:down')){if(!keys.has(e.key))pressedKeys.add(e.key);keys.add(e.key);}
 if(e.is('keyboard:up'))keys.delete(e.key);
 if(e.is('keyboard:down:m')){sound=!sound;if(!sound)stop();}
 if(e.is('keyboard:down:escape'))system.jump('prompt');
}
