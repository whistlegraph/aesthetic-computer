// Small fixed-step platform playground. Units are pixels and seconds.
export const STEP=1/120;
export const clamp=(v,a,b)=>Math.max(a,Math.min(b,v));
export function body(kind,x,y,w,h){return {kind,x,y,w,h,vx:0,vy:0,grounded:false,mass:kind==='crate'?2:1,bounce:kind==='ball'?.73:0};}
export function createWorld(){
 const floors=[[0,640],[735,485],[1330,550],[1980,720]].map(([x,w])=>({x,y:480,w,h:120}));
 const platforms=[[190,393,110],[365,324,105],[520,258,110],[790,374,170],[1070,312,115],[1425,391,140],[1630,310,120],[1830,244,125],[2080,364,130],[2310,286,160]].map(([x,y,w])=>({x,y,w,h:16}));
 return {player:body('player',100,462,18,28),objects:[body('ball',260,300,26,26),body('ball',315,260,20,20),body('crate',430,450,28,28),body('crate',460,416,28,28),body('ball',870,310,30,30),body('crate',1110,280,28,28)],solids:[...floors,...platforms],springs:[{x:565,y:480,w:44},{x:1490,y:391,w:40},{x:2200,y:480,w:44}],coyote:0,buffer:0,jumpAge:1,time:0,landings:0,bumps:0,finish:false};
}
export function spawnBall(s){if(s.objects.length>=24)s.objects.shift();const p=s.player,b=body('ball',p.x,p.y-50,24,24);b.vx=p.vx*.5;b.vy=-100;s.objects.push(b);}
function overlap(a,b){return a.x+a.w/2>b.x&&a.x-a.w/2<b.x+b.w&&a.y+a.h/2>b.y&&a.y-a.h/2<b.y+b.h;}
function move(b,solids,dt){
 b.grounded=false;b.x+=b.vx*dt;
 for(const r of solids)if(overlap(b,r)){if(b.vx>0)b.x=r.x-b.w/2;else if(b.vx<0)b.x=r.x+r.w+b.w/2;else continue;b.vx=-b.vx*b.bounce;}
 const impact=b.vy;b.y+=b.vy*dt;
 for(const r of solids)if(overlap(b,r)){
  if(b.vy>=0){b.y=r.y-b.h/2;b.grounded=true;b.vy=Math.abs(impact)>45?-impact*b.bounce:0;}
  else{b.y=r.y+r.h+b.h/2;b.vy=-b.vy*b.bounce;}
 }
 b.x=clamp(b.x,b.w/2,2700-b.w/2);
 if(b.x===b.w/2&&b.vx<0||b.x===2700-b.w/2&&b.vx>0)b.vx=-b.vx*b.bounce;
 return impact;
}
function contact(a,b){
 if(a.kind==='ball'&&b.kind==='ball'){
  const dx=b.x-a.x,dy=b.y-a.y,d=Math.hypot(dx,dy),r=(a.w+b.w)/2;
  return d<r?{nx:d?dx/d:1,ny:d?dy/d:0,depth:r-d}:null;
 }
 if(a.kind==='ball'||b.kind==='ball'){
  const c=a.kind==='ball'?a:b,r=c===a?b:a;
  const px=clamp(c.x,r.x-r.w/2,r.x+r.w/2),py=clamp(c.y,r.y-r.h/2,r.y+r.h/2);
  const dx=px-c.x,dy=py-c.y,d=Math.hypot(dx,dy),radius=c.w/2;
  if(d>=radius)return null;
  if(d>0){const sign=c===a?1:-1;return {nx:dx/d*sign,ny:dy/d*sign,depth:radius-d};}
 }
 const dx=b.x-a.x,dy=b.y-a.y,ox=(a.w+b.w)/2-Math.abs(dx),oy=(a.h+b.h)/2-Math.abs(dy);
 if(ox<=0||oy<=0)return null;
 return ox<oy?{nx:dx<0?-1:1,ny:0,depth:ox}:{nx:0,ny:dy<0?-1:1,depth:oy};
}
function collide(a,b){
 const c=contact(a,b);if(!c)return;
 const ia=1/a.mass,ib=1/b.mass,sum=ia+ib,correction=Math.max(0,c.depth-.01)*.85/sum;
 a.x-=c.nx*correction*ia;a.y-=c.ny*correction*ia;b.x+=c.nx*correction*ib;b.y+=c.ny*correction*ib;
 const speed=(b.vx-a.vx)*c.nx+(b.vy-a.vy)*c.ny;
 if(speed<0){const j=-(1+Math.min(.7,Math.max(a.bounce,b.bounce)))*speed/sum;a.vx-=j*c.nx*ia;a.vy-=j*c.ny*ia;b.vx+=j*c.nx*ib;b.vy+=j*c.ny*ib;}
 if(c.ny>.5)a.grounded=true;if(c.ny<-.5)b.grounded=true;
}
export function step(s,input,dt=STEP){
 const p=s.player;s.time+=dt;
 const wasGrounded=p.grounded;s.coyote=wasGrounded?.1:Math.max(0,s.coyote-dt);
 s.buffer=input.jumpPressed?.12:Math.max(0,s.buffer-dt);
 const target=clamp(input.axis||0,-1,1)*(input.run?300:195),accel=wasGrounded?1500:650;
 if(target)p.vx+=clamp(target-p.vx,-accel*dt,accel*dt);
 else p.vx*=Math.exp(-(wasGrounded?9:1.2)*dt);
 if(s.buffer>0&&s.coyote>0){p.vy=-365;p.grounded=false;s.buffer=s.coyote=0;s.jumpAge=0;}
 s.jumpAge+=dt;
 const lowGravity=input.jumpHeld&&p.vy<0&&s.jumpAge<.22;
 p.vy=Math.min(650,p.vy+(lowGravity?490:1100)*dt);
 const impact=move(p,s.solids,dt);
 if(p.grounded&&!wasGrounded&&impact>130)s.landings++;
 for(const o of s.objects){o.vy=Math.min(650,o.vy+1100*dt);o.vx*=Math.exp(-(o.grounded?(o.kind==='ball'?.28:3):.04)*dt);move(o,s.solids,dt);}
 const all=[p,...s.objects];
 for(let pass=0;pass<3;pass++){
  for(let i=0;i<all.length;i++)for(let j=i+1;j<all.length;j++)collide(all[i],all[j]);
  // Projection after object impulses keeps piles outside static platforms.
  for(const b of all)for(const r of s.solids)if(overlap(b,r)){
   const options=[{d:b.x+b.w/2-r.x,x:-1,y:0},{d:r.x+r.w-(b.x-b.w/2),x:1,y:0},{d:b.y+b.h/2-r.y,x:0,y:-1},{d:r.y+r.h-(b.y-b.h/2),x:0,y:1}];
   const c=options.reduce((v,n)=>n.d<v.d?n:v);b.x+=c.x*c.d;b.y+=c.y*c.d;
   if(c.y<0){b.grounded=true;if(b.vy>0)b.vy=0;}else if(c.y>0&&b.vy<0)b.vy=0;
   if(c.x&&b.vx*c.x<0)b.vx=0;
  }
 }
 for(const b of all)for(const spring of s.springs)if(b.grounded&&b.x+b.w/2>spring.x&&b.x-b.w/2<spring.x+spring.w&&Math.abs(b.y+b.h/2-spring.y)<2){b.vy=-640;b.grounded=false;if(b===p){s.coyote=0;s.jumpAge=1;s.bumps++;}}
 if(input.shove){const dir=input.facing||1;for(const b of s.objects)if(Math.hypot(b.x-p.x,b.y-p.y)<85){b.vx+=dir*310/b.mass;b.vy-=210/b.mass;}s.bumps++;}
 if(input.spawn)spawnBall(s);
 s.objects=s.objects.filter(b=>b.y<750);
 if(p.y>720){p.x=100;p.y=440;p.vx=p.vy=0;s.coyote=s.buffer=0;}
 if(p.x>2600)s.finish=true;
}
