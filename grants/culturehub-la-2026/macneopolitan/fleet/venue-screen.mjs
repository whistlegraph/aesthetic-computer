// Venue screen, 26.09.24 — what a seat shows between pieces.
// Two modes, read from /pieces/venue-screen-config.json:
//   starfield     — a classic starfield in the seat's colour, before people arrive
//   card          — a huge word edge to edge in the seat's colour ("oskiewartime!")
// The starfield hums: each seat swells its ring note (cfg.hz) as a slow sine bed,
// with an octave or fifth now and then — the preshow ambient mix. Mic closed; backlight up.
let cfg={mode:'starfield',text:''},color=[143,209,63],brightSteps=14,cfgAt=-Infinity,api=null,pulse=0,nextSwell=0,voices=[];
const N=160,SPREAD=40;const sx=new Float64Array(N),sy=new Float64Array(N),sz=new Float64Array(N);
const FONT='6x10';
function textWidth(s,size){return s.length*6*size;}
function fitSize(s,maxW,maxSize){let size=maxSize;while(size>1&&textWidth(s,size)>maxW)size--;return size;}
function centered(write,s,y,size,w){write(s,{x:Math.round((w-textWidth(s,size))/2),y,font:FONT,size});}
function reset(i){sx[i]=2*(Math.random()-.5)*SPREAD;sy[i]=2*(Math.random()-.5)*SPREAD;sz[i]=(Math.random()+.00001)*SPREAD;}
function readCfg(system){try{const c=JSON.parse(system.readFile('/pieces/venue-screen-config.json'));if(c&&typeof c==='object')cfg=c;}catch{}}
export function boot(a){api=a;try{a.sound.microphone.close();a.sound.volume.setMono(true);a.sound.volume.setMonoOutput('left');let mix=1;try{const v=JSON.parse(a.system.readFile('/pieces/composition-volume.json'));if(Number.isFinite(v.percent))mix=v.percent/100;}catch{}a.sound.volume.setMix(mix);/* the room's master (composition-volume.json, 100 % tonight): the rehearsal piece after us inherits it */}catch{}readCfg(a.system);cfgAt=a.sound.time;nextSwell=a.sound.time+1+(cfg.seat||0)*1.7;
 color=[[143,209,63],[90,87,211],[242,167,185]][(cfg.seat||0)%3];if(Array.isArray(cfg.color))color=cfg.color;
 for(let i=0;i<N;i++)reset(i);brightSteps=14;
 try{a.system.writeFile('/pieces/venue-screen-status.json',JSON.stringify({mode:cfg.mode,text:cfg.text||'',at:Date.now()/1000}));}catch{}}
export function sim({sound,system,screen}){
 if(brightSteps>0){brightSteps--;try{system.brightnessAdjust(1);}catch{}}
 if(sound.time-cfgAt>1){cfgAt=sound.time;const was=cfg.mode+'|'+(cfg.text||'');readCfg(system);if(cfg.mode+'|'+(cfg.text||'')!==was)try{system.writeFile('/pieces/venue-screen-status.json',JSON.stringify({mode:cfg.mode,text:cfg.text||'',at:Date.now()/1000}));}catch{}}
 pulse+=1/60;
 if(cfg.mode==='starfield'&&cfg.bed!==false&&sound.time>=nextSwell){const hz=cfg.hz||261.63;const pick=Math.random();const tone=pick<.55?hz:pick<.8?hz*1.5:pick<.92?hz*2:hz/2;const dur=7+Math.random()*7;
  try{const v=sound.synth({type:'sine',tone,volume:(cfg.bedGain||.07)*(tone>hz?.7:1),duration:dur,attack:dur*.45,decay:dur*.45});voices.push(v);if(voices.length>8)voices.shift();}catch{}
  nextSwell=sound.time+4+Math.random()*7;}
 if(cfg.mode==='starfield'){const speed=cfg.speed||2.1,w=screen.width,h=screen.height;
  for(let i=0;i<N;i++){sz[i]-=.01*speed;if(sz[i]<=0){reset(i);continue;}const x=Math.tan(sx[i]/sz[i])*w/2+w/2,y=Math.tan(sy[i]/sz[i])*h/2+h/2;if(x<0||x>=w||y<0||y>=h)reset(i);}}}
export function paint({wipe,ink,plot,write,screen}){
 const w=screen.width,h=screen.height;
 if(cfg.mode==='card'){wipe(...color);const lum=(0.2126*color[0]+0.7152*color[1]+0.0722*color[2])/255;const inkRGB=lum>.5?[8,8,12]:[250,250,250];
  const text=cfg.text||'oskiewartime!';const size=fitSize(text,w*.92,16);const bob=Math.round(Math.sin(pulse*2.2)*4);
  ink(...inkRGB);centered(write,text,Math.round(h/2-size*5)+bob,size,w);
  if(cfg.sub){ink(Math.round(inkRGB[0]*.5+color[0]*.5),Math.round(inkRGB[1]*.5+color[1]*.5),Math.round(inkRGB[2]*.5+color[2]*.5));centered(write,cfg.sub,Math.round(h/2+size*5+16),2,w);}
  return;}
 wipe(0,0,0);
 for(let i=0;i<N;i++){const x=Math.tan(sx[i]/sz[i])*w/2+w/2,y=Math.tan(sy[i]/sz[i])*h/2+h/2;const near=1-sz[i]/SPREAD;const a=Math.round(60+195*near);
  ink(Math.round(color[0]*.55+255*.45*near),Math.round(color[1]*.55+255*.45*near),Math.round(color[2]*.55+255*.45*near),a);plot(Math.round(x),Math.round(y));
  if(near>.8){plot(Math.round(x)+1,Math.round(y));plot(Math.round(x),Math.round(y)+1);}}
 if(cfg.text){ink(Math.round(color[0]*.7),Math.round(color[1]*.7),Math.round(color[2]*.7));centered(write,cfg.text,h-28,2,w);}}
export function act(){}
export function leave(){for(const v of voices)try{v?.kill?.(.05);}catch{}voices=[];}
