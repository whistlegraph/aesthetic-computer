export const C={cream:[255,247,224],ink:[27,28,46],pink:[255,109,143],mint:[157,235,193],yellow:[255,218,96],blue:[118,171,255],muted:[180,181,200]};
export const pick=a=>a[Math.floor(Math.random()*a.length)];
export function shuffle(a){a=[...a];for(let i=a.length-1;i>0;i--){const j=Math.floor(Math.random()*(i+1));[a[i],a[j]]=[a[j],a[i]];}return a;}
export function ui(a){
 const v=a.view(),s=Math.min(1,v.width/900),ox=(v.width-900*s)/2,oy=(1080-1080*s)/2;
 const b=(x,y,w,h,c)=>a.box(ox+x*s,oy+y*s,w*s,h*s,...c);
 const text=(t,x,y,size,c=C.cream,center=false)=>{t=String(t);a.write(t,ox+x*s-(center?a.textWidth(t,size*s)/2:0),oy+y*s,size*s,...c);};
 const circle=(x,y,r,c)=>{for(let i=0;i<28;i++){const p=i*Math.PI/14,q=(i+1)*Math.PI/14;a.triangle(ox+x*s,oy+y*s,ox+(x+Math.cos(p)*r)*s,oy+(y+Math.sin(p)*r)*s,ox+(x+Math.cos(q)*r)*s,oy+(y+Math.sin(q)*r)*s,...c);}};
 const line=(x,y,u,v,w,c)=>a.line(ox+x*s,oy+y*s,ox+u*s,oy+v*s,w*s,...c);
 const hits=[];
 function button(label,x,y,w,h,action,fill=C.cream,ink=C.ink,size=36){b(x,y+6,w,h,C.ink);b(x,y,w,h,fill);text(label,x+w/2,y+(h-size*1.15)/2,size,ink,true);hits.push({x,y,w,h,action});}
 function tile(letter,x,y,size=100,fill=C.cream){b(x+5,y+7,size,size,C.ink);b(x,y,size,size,fill);text(letter,x+size/2,y+size*.15,size*.57,C.ink,true);}
 return {b,text,circle,line,button,tile,hits,point:p=>({x:(p.x-ox)/s,y:(p.y-oy)/s})};
}
export function shell(config,model,draw,act){
 let state=model(),hits=[],last=false,keys=[],muted=false,paused=false,audio=null,started=false;
 const input=[];
 function sound(hz=440){if(!muted&&audio)audio.tone(hz,.12);}
 function action(x){if(x==='mute'){muted=!muted;return;}if(x==='pause'){if(started&&state.phase!=='over')paused=!paused;return;}if(x==='start'||x==='restart'){state=model();started=true;paused=false;sound(330);return;}if(!started||paused||state.phase==='over')return;const outcome=act(state,x);if(outcome)sound(outcome==='bad'?110:outcome==='win'?660:440);}
 return {
 boot(a){audio=a;if(typeof addEventListener==='function'){addEventListener('keydown',e=>{if(!e.repeat)input.push(e.code);});addEventListener('blur',()=>{if(started&&state.phase!=='over')paused=true;});}if(typeof document!=='undefined'){const canvas=document.querySelector('canvas');if(canvas){canvas.setAttribute('aria-label',config.title+'. '+config.instructions);canvas.tabIndex=0;}}},
 sim(a){audio=a;const p=a.pointer(),u=ui(a),point=u.point(p);if(p.down&&!last){const h=hits.find(h=>point.x>=h.x&&point.x<h.x+h.w&&point.y>=h.y&&point.y<h.y+h.h);if(h)action(h.action);}last=p.down;
 const now=a.keys();const pressed=typeof addEventListener==='function'?input.splice(0):now.filter(k=>!keys.includes(k));keys=now;
 for(const k of pressed){if(k==='F2')action('mute');else if(k==='Escape')action('pause');else if(k==='Enter'&&(!started||state.phase==='over'))action('restart');else if(k==='Enter'&&paused)action('pause');else action(k);}
 if(started&&!paused&&state.phase!=='over'){state.time=Math.max(0,state.time-1/60);state.t=(state.t||0)+1/60;state.flash=Math.max(0,(state.flash||0)-1/60);if(!state.time)state.phase='over';}
 },
 paint(a){a.wipe(...config.bg);const u=ui(a);u.text(config.title,40,28,46,config.accent);u.button(muted?'Sound off':'Sound on',675,30,185,62,'mute',config.accent,C.ink,25);if(started)u.button(paused?'Resume':'Pause',675,112,185,62,'pause',C.cream,C.ink,26);
 if(!started){u.text(config.tagline,450,235,42,C.cream,true);config.hero(u,a.now());config.lines.forEach((l,i)=>u.text(l,450,660+i*48,31,C.cream,true));u.button('Let’s play',235,870,430,100,'start',config.accent,C.ink,44);u.text('Enter to start · F2 sound · Esc pause',450,1002,24,C.muted,true);}
 else if(state.phase==='over'){u.text('Time!',450,240,90,config.accent,true);u.text(state.score+' points',450,390,68,C.cream,true);u.text(state.round+' words rescued',450,510,34,C.cream,true);u.button('Play again',235,710,430,110,'restart',config.accent,C.ink,42);u.text('Enter to restart',450,875,27,C.muted,true);}
 else if(paused){u.text('Paused',450,350,78,config.accent,true);u.button('Keep playing',220,570,460,110,'pause',config.accent,C.ink,38);u.button('Start over',270,740,360,85,'restart',C.cream,C.ink,32);}
 else{u.text(state.score+' points',40,112,32,C.cream);u.text(Math.ceil(state.time)+'s',450,112,40,config.accent,true);u.b(40,191,820,7,C.ink);u.b(40,191,820*state.time/config.seconds,7,config.accent);draw(u,state,a.now());}
 hits=u.hits;
 },
 };
}
