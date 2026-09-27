// Venue screen, 26.09.24 — what a seat shows between pieces.
// Two modes, read from /pieces/venue-screen-config.json:
//   starfield     — a classic starfield in the seat's colour, before people arrive
//   card          — a huge word edge to edge in the seat's colour ("oskiewartime!")
// The starfield hums: each seat swells its ring note (cfg.hz) as a slow sine bed,
// with an octave or fifth now and then — the preshow ambient mix. Mic closed; backlight up.
let cfg={mode:'starfield',text:''},color=[143,209,63],brightSteps=14,cfgAt=-Infinity,api=null,pulse=0,nextSwell=0,voices=[],motifStep=0;
const N=160,SPREAD=40;const sx=new Float64Array(N),sy=new Float64Array(N),sz=new Float64Array(N),lx=new Float64Array(N).fill(NaN),ly=new Float64Array(N).fill(NaN);let tailsPrimed=false;
const FONT='6x10';
function textWidth(s,size){return s.length*6*size;}
function fitSize(s,maxW,maxSize){let size=maxSize;while(size>1&&textWidth(s,size)>maxW)size--;return size;}
function centered(write,s,y,size,w){write(s,{x:Math.round((w-textWidth(s,size))/2),y,font:FONT,size});}
function reset(i){sx[i]=2*(Math.random()-.5)*SPREAD;sy[i]=2*(Math.random()-.5)*SPREAD;sz[i]=(Math.random()+.00001)*SPREAD;lx[i]=NaN;ly[i]=NaN;}
function readCfg(system){try{const c=JSON.parse(system.readFile('/pieces/venue-screen-config.json'));if(c&&typeof c==='object')cfg=c;}catch{}}
export function boot(a){api=a;try{a.sound.microphone.close();a.sound.volume.setMono(true);a.sound.volume.setMonoOutput('left');let mix=1;try{const v=JSON.parse(a.system.readFile('/pieces/composition-volume.json'));if(Number.isFinite(v.percent))mix=v.percent/100;}catch{}a.sound.volume.setMix(mix);/* the room's master (composition-volume.json, 100 % tonight): the rehearsal piece after us inherits it */}catch{}readCfg(a.system);cfgAt=a.sound.time;nextSwell=a.sound.time+1+(cfg.seat||0)*1.7;
 color=[[143,209,63],[90,87,211],[242,167,185]][(cfg.seat||0)%3];if(Array.isArray(cfg.color))color=cfg.color;
 for(let i=0;i<N;i++)reset(i);brightSteps=14;
 try{a.system.writeFile('/pieces/venue-screen-status.json',JSON.stringify({mode:cfg.mode,text:cfg.text||'',at:Date.now()/1000}));}catch{}}
export function sim({sound,system,screen}){
 if(brightSteps>0){brightSteps--;try{system.brightnessAdjust(1);}catch{}}
 if(sound.time-cfgAt>1){cfgAt=sound.time;const was=cfg.mode+'|'+(cfg.text||'');readCfg(system);if(cfg.mode+'|'+(cfg.text||'')!==was)try{system.writeFile('/pieces/venue-screen-status.json',JSON.stringify({mode:cfg.mode,text:cfg.text||'',at:Date.now()/1000}));}catch{}}
 pulse+=1/60;
 if(cfg.mode==='starfield'&&cfg.bed!==false&&sound.time>=nextSwell){const hz=cfg.hz||261.63;
  // A slow pentatonic motif over the seat's ring note, each seat entering a step later, so the room hums a canon.
  const MOTIF=[0,4,7,9,7,4,2,0,-3,0,4,7,12,9,7,4];const semi=MOTIF[(motifStep+(cfg.seat||0)*3)%MOTIF.length];motifStep++;
  const tone=hz*Math.pow(2,(cfg.octave??-1)+semi/12);const dur=5+Math.random()*4;   // an octave under the ring note unless cfg.octave says otherwise
  try{const v=sound.synth({type:'sine',tone,volume:(cfg.bedGain||.02)*(semi>=12?.7:1),duration:dur,attack:dur*.4,decay:dur*.45});voices.push(v);if(voices.length>8)voices.shift();}catch{}
  nextSwell=sound.time+3.2+Math.random()*1.6;}
 if(cfg.mode==='starfield'){const speed=cfg.speed||4.5,w=screen.width,h=screen.height;
  for(let i=0;i<N;i++){sz[i]-=.01*speed;if(sz[i]<=0){reset(i);continue;}const x=Math.tan(sx[i]/sz[i])*w/2+w/2,y=Math.tan(sy[i]/sz[i])*h/2+h/2;if(x<0||x>=w||y<0||y>=h)reset(i);}}}
export function paint({wipe,ink,plot,line,box,write,screen}){
 const w=screen.width,h=screen.height;
 if(cfg.mode==='card'){wipe(...color);const lum=(0.2126*color[0]+0.7152*color[1]+0.0722*color[2])/255;const inkRGB=lum>.5?[8,8,12]:[250,250,250];
  const text=cfg.text||'oskiewartime!';const size=fitSize(text,w*.92,16);const bob=Math.round(Math.sin(pulse*2.2)*4);
  ink(...inkRGB);centered(write,text,Math.round(h/2-size*5)+bob,size,w);
  if(cfg.sub){ink(Math.round(inkRGB[0]*.5+color[0]*.5),Math.round(inkRGB[1]*.5+color[1]*.5),Math.round(inkRGB[2]*.5+color[2]*.5));centered(write,cfg.sub,Math.round(h/2+size*5+16),2,w);}
  return;}
 // Tails: the frame is not wiped but dimmed, so every star drags a streak behind it, and each star
 // is drawn as a line from where it was last frame to where it is now (longer and brighter as it nears).
 if(!tailsPrimed){wipe(0,0,0);tailsPrimed=true;}else{ink(0,0,0,cfg.tailFade??22);box(0,0,w,h);}
 for(let i=0;i<N;i++){const x=Math.tan(sx[i]/sz[i])*w/2+w/2,y=Math.tan(sy[i]/sz[i])*h/2+h/2;const near=1-sz[i]/SPREAD;const a=Math.round(70+185*near);
  const r=Math.round(color[0]*.55+255*.45*near),g=Math.round(color[1]*.55+255*.45*near),b=Math.round(color[2]*.55+255*.45*near);
  ink(r,g,b,a);
  const px=Number.isFinite(lx[i])?lx[i]:x,py=Number.isFinite(ly[i])?ly[i]:y;
  if(Math.abs(px-x)+Math.abs(py-y)<40)line(Math.round(px),Math.round(py),Math.round(x),Math.round(y));else plot(Math.round(x),Math.round(y));
  if(near>.8){plot(Math.round(x)+1,Math.round(y));plot(Math.round(x),Math.round(y)+1);}
  lx[i]=x;ly[i]=y;}
 if(cfg.star!==false){   // a five-pointed star in the seat colour, breathing and slowly turning over the field
  const zoom=.5+.5*(1+Math.sin(pulse*.9))/2;   // zooms in and out: from a fifth of the screen to nearly its whole height
  const cx=w/2,cy=h/2,R=Math.min(w,h)*(.10+.38*zoom),r=R*.42,rot=pulse*.25;const pts=[];
  for(let k=0;k<10;k++){const a=rot+k*Math.PI/5-Math.PI/2,rad=k%2?r:R;pts.push([cx+Math.cos(a)*rad,cy+Math.sin(a)*rad]);}
  // rainbow: each edge takes its own hue, and the hues march round the star over time
  const hue=(hh)=>{const x=((hh%360)+360)%360,c=255,m=0,q=Math.round(255*(1-Math.abs((x/60)%2-1)));const [r1,g1,b1]=x<60?[c,q,m]:x<120?[q,c,m]:x<180?[m,c,q]:x<240?[m,q,c]:x<300?[q,m,c]:[c,m,q];return [r1,g1,b1];};
  for(let t=0;t<3;t++)for(let k=0;k<10;k++){const [x1,y1]=pts[k],[x2,y2]=pts[(k+1)%10];const c=cfg.starColor||hue(k*36+pulse*80);ink(c[0],c[1],c[2],240);line(Math.round(x1)+(t%2),Math.round(y1)+(t>1?1:0),Math.round(x2)+(t%2),Math.round(y2)+(t>1?1:0));}
  ink(255,255,255,90);for(let k=0;k<10;k+=2){const [x1,y1]=pts[k];line(Math.round(cx),Math.round(cy),Math.round(x1),Math.round(y1));}
 }
 if(cfg.text){ink(Math.round(color[0]*.7),Math.round(color[1]*.7),Math.round(color[2]*.7));centered(write,cfg.text,h-28,2,w);}}
export function act(){}
export function leave(){for(const v of voices)try{v?.kill?.(.05);}catch{}voices=[];}
