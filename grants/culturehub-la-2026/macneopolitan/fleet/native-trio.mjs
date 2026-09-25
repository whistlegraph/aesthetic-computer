// Trio receiver for the existing native engine. Starts are audio-clock based,
// dispatched on a simulation frame (NOT sample-accurate). Never auto-plays.
//
// Each seat plays its own vocal stem (the voice bounce), its backing events
// (sine beds and harmonies, GM instruments, percussion), shows the notation —
// the lyric being sung, the phrase this seat carries, its own marks on a
// timeline — and, on the held center, drives the wedge light on its USB DMX.
export const surface='drum'; // hide pointer emulation on the concert surface
let cfg,api,error='',phase='loading',origin=null,runId=null,seen='',lastReport=-1;
let assemblyStarted=0,target=null,eventsStarted=0;
let voices=[],cursor=0,startedAt=null,heartbeat=-Infinity,maxFrameGap=0,prior=null;
let brightSteps=0,volumeAt=-Infinity,lightAt=-Infinity,lastLight='';
const instance=String(Date.now())+'-'+Math.random().toString(36).slice(2);
function read(system,path){const bytes=new Uint8Array(system.readFileBytes(path));let text='';for(let i=0;i<bytes.length;i+=4096)text+=String.fromCharCode.apply(null,bytes.subarray(i,i+4096));return JSON.parse(text);}
function dark(system){if(cfg?.heldCenter){let ok=false;try{ok=system.dmxSend(new Array(64).fill(0));}catch{}lastLight='0,0,0';system.writeFile('/pieces/center-dmx-live.json',JSON.stringify({ok,rgb:[0,0,0],active:false,scoreTime:origin===null?-1:api.sound.time-origin,address:41}));}}
function stop(sound){dark(api.system);for(const v of voices)v?.kill?.(.025);voices=[];sound.deck.pause(0);origin=null;cursor=0;phase=error?'error':'ready';try{sound.room?.setMix?.(0);}catch{}}   // the air (room mix) never outlives a piece
export function boot(a){
 api=a;a.sound.microphone.close();a.sound.volume.setMono(true);a.sound.volume.setMonoOutput('left');
 try{
  cfg=read(a.system,'/pieces/trio-fleet-config.json');
  if(cfg.schema!=='trio-native-v1'||!Number.isFinite(cfg.duration)||cfg.duration<=0||!Array.isArray(cfg.events))throw Error('Invalid Trio config');
  for(const e of cfg.events)if(![e.t,e.dur,e.frequency,e.gain].every(Number.isFinite)||e.t<0||e.dur<=0||e.gain<0||e.gain>.25)throw Error('Invalid note');
  a.sound.volume.setMix(Number.isFinite(cfg.mix)?cfg.mix:.25);   // the global master the room agreed on
  brightSteps=14;                                                  // walk the backlight up to full
  if(cfg.center){
   if(!/^\/pieces\/trio-voices-[a-f0-9]{16}\.wav$/.test(cfg.center.file)||!cfg.center.parts.every(p=>/^\/pieces\/trio-[a-f0-9]{16}-[0-9]+\.part$/.test(p)))throw Error('Invalid Center paths');
   a.system.writeFile(cfg.center.file+'.sha256','');
   const command='cat '+cfg.center.parts.join(' ')+' > '+cfg.center.file+'.tmp && mv '+cfg.center.file+'.tmp '+cfg.center.file+' && sha256sum '+cfg.center.file+' > '+cfg.center.file+'.sha256';
   a.system.pty.spawn('/bin/sh',['-c',command],80,24);phase='assembling';assemblyStarted=a.sound.time;
  }else phase='ready';
 }catch(e){error=String(e);phase='error';}
}
// Live controls: the room's global volume file (blueberry's /volume writes
// it), and the held center's wedge, lit by whichever voice reaches seat 5.
function controls(a){
 const {sound,system}=a,now=sound.time;
 if(brightSteps>0){brightSteps--;try{system.brightnessAdjust(1);}catch{}}
 if(now-volumeAt>.25){volumeAt=now;try{const v=JSON.parse(system.readFile('/pieces/composition-volume.json'));if(Number.isFinite(v.percent)&&v.percent>=0&&v.percent<=100)sound.volume.setMix(v.percent/100);system.writeFile('/pieces/composition-volume-status.json',JSON.stringify({percent:sound.volume.mix*100,id:v.id,at:now}));}catch{}}
 if(!cfg?.heldCenter||now-lightAt<.05)return;lightAt=now;
 const t=origin===null?-1:now-origin,rgb=[0,0,0];
 if(phase==='playing')for(const e of cfg.lightCues||[]){const u=t-e.t;if(u<0||u>e.dur)continue;const env=Math.min(1,u/.12,(e.dur-u)/.25);for(let i=0;i<3;i++)rgb[i]=Math.max(rgb[i],Math.round(Math.min(220,e.rgb[i]*2.5)*Math.max(0,env)));}
 const key=rgb.join(',');if(key!==lastLight){const slots=new Array(64).fill(0);rgb.forEach((v,i)=>slots[40+i]=v);let ok=false;try{ok=system.dmxSend(slots);}catch{}if(ok)lastLight=key;system.writeFile('/pieces/center-dmx-live.json',JSON.stringify({ok,rgb,active:rgb.some(v=>v>0),scoreTime:t,address:41}));}
}
export function sim(a){
 const {sound,system}=a,now=sound.time;
 controls(a);
 if(phase==='assembling'){
  let receipt='';try{receipt=system.readFile(cfg.center.file+'.sha256')||'';}catch{}
  if(receipt.trim().split(/\s+/)[0]===cfg.center.sha256){
   if(!sound.deck.load(0,cfg.center.file)){error='Center decoder failed';phase='error';}
   else{sound.deck.pause(0);sound.deck.setSpeed(0,1);sound.deck.setCrossfader(0);sound.deck.setVolume(0,1);sound.deck.setMasterVolume(1);phase='ready';}
  }else if(now-assemblyStarted>20){error='Center checksum/assembly timeout';phase='error';}
 }
 if(prior!==null)maxFrameGap=Math.max(maxFrameGap,now-prior);prior=now;
 let cmd;try{cmd=JSON.parse(system.readFile('/pieces/trio-fleet-command.json'));}catch{}
 if(cmd&&cmd.id!==seen){
  seen=cmd.id;
  if(cmd.action==='clock')system.writeFile('/pieces/trio-fleet-clock.json',JSON.stringify({id:cmd.id,audioTime:now,instance}));
  if(cmd.action==='stop')stop(sound);
  if(cmd.action==='keepalive'&&cmd.runId===runId)heartbeat=now;
  if(cmd.action==='prepare'&&!error&&cmd.arrangementHash===cfg.arrangementHash&&Number.isFinite(cmd.startAt)&&cmd.startAt>now+2){
   stop(sound);origin=cmd.startAt;target=origin;eventsStarted=0;runId=cmd.runId;phase='prepared';heartbeat=now;startedAt=null;maxFrameGap=0;brightSteps=14;try{sound.room?.setMix?.(Number.isFinite(cfg.fx?.room)?cfg.fx.room:0);}catch{}   // air: the room reverb the plan asks for (cfg.fx.room)
   if(cfg.center)sound.deck.seek(0,0);
  }
  if(cmd.action==='play'&&cmd.runId===runId&&phase==='prepared'&&origin>now+.5){phase='countdown';heartbeat=now;}
 }
 if(['prepared','countdown','playing'].includes(phase)&&now-heartbeat>3){stop(sound);error='Controller heartbeat expired';phase='error';}
 if(phase==='countdown'&&now>=origin){
  if(now-origin>.1){error='Missed downbeat';stop(sound);}
  else{phase='playing';startedAt=now;if(cfg.center)sound.deck.play(0);}
 }
 if(phase==='playing'){
  const t=now-origin;
  if(t>=cfg.duration+.5){stop(sound);phase='finished';}
  else while(cursor<cfg.events.length&&cfg.events[cursor].t<=t){
   const e=cfg.events[cursor++];
   if(t-e.t>.1||t>=e.t+e.dur)continue;
   const opts={type:e.wave||'sine',tone:e.frequency,volume:e.gain,duration:e.t+e.dur-t,attack:e.attack||.015,decay:e.release||.12};
   if(Number.isInteger(e.gmProgram))opts.gmProgram=e.gmProgram;   // a General MIDI instrument instead of a bare wave
   const voice=sound.synth(opts);
   eventsStarted++;voices.push(voice);if(voices.length>256)voices.splice(0,128);
  }
 }
 if(now-lastReport>.05){
  lastReport=now;
  const deck=sound.deck.decks[0];
  const centerReady=!cfg?.center||(deck.loaded&&!deck.error&&Math.abs(deck.duration-cfg.center.duration)<.1);
  system.writeFile('/pieces/trio-fleet-status.json',JSON.stringify({schema:'trio-native-status-v1',instance,receiverId:cfg?.receiverId,
   arrangementHash:cfg?.arrangementHash,phase,error,command:seen,runId,origin,audioTime:now,startedAt,startLateMs:startedAt===null?null:1000*(startedAt-target),
   maxFrameGap,centerReady,center:cfg?.center?{...cfg.center,loaded:deck.loaded,playing:deck.playing,position:deck.position,duration:deck.duration,error:deck.error}:null,
   eventCount:cfg?.events.length,eventsStarted,duration:cfg?.duration,mono:sound.volume.mono,monoOutput:sound.volume.monoOutput,mix:sound.volume.mix,room:sound.room?.mix,brightness:system.brightness,
   microphoneHot:sound.microphone.hot,displayMode:'concert',fullscreen:true,pointerHidden:true,clockPrecision:'simulation-frame',
   output:sound.speaker.amplitudes}));
 }
}
// ---- notation ----------------------------------------------------------------
// Incoming notes. Time flows right to left toward a strike line a third of the
// way in; the next six seconds approach, the last second and a half trail
// off. Pitched notes sit by pitch, percussion in lanes underneath. Notes this
// seat plays are bright and glow on the hit; the rest of the room's notes are
// there too, dim, so every laptop shows the whole score and its own part.
// Feed: cfg.notes [{i,t,dur,midi,gain,pan,mine,label,text,rgb}] sorted by t,
// cfg.sections [{name,startSec,endSec}] (optional). Trio pieces get the same
// feed from their events; the lyric line and the phrase box stay on top.
const FONT='6x10';
function textWidth(s,size){return s.length*6*size;}
function fitSize(s,maxW,maxSize){let size=maxSize;while(size>1&&textWidth(s,size)>maxW)size--;return size;}
function centered(write,s,y,size,w){write(s,{x:Math.round((w-textWidth(s,size))/2),y,font:FONT,size});}
const NOTE_RGB={c:[255,50,50],d:[255,160,0],e:[255,230,0],f:[50,200,50],g:[50,120,255],a:[130,50,200],b:[180,80,255]};
const NAMES=['c','c#','d','d#','e','f','f#','g','g#','a','a#','b'];
function noteRgb(midi){const n=NAMES[((midi%12)+12)%12];return n.includes('#')?[235,235,235]:NOTE_RGB[n];}
const KIND={kick:{lane:0,rgb:[255,90,60]},boom:{lane:0,rgb:[200,40,40]},snare:{lane:1,rgb:[245,245,245]},donk:{lane:1,rgb:[255,170,40]},hat:{lane:2,rgb:[170,170,170]},perc:{lane:1,rgb:[220,220,220]},riser:{lane:3,rgb:[120,200,255]},voice:{lane:3,rgb:[255,120,200]}};
let noteCursor=0,lyricCursor=0,routeCursor=0,lastPaintT=-1,wordCursor=0;
// Per-frame budgets: the roll draws at most NOTES marks (this seat's own
// always, the room's until OTHERS), LABELS note names, and merges a run of
// ticks in one lane closer than MERGE seconds. No arrays are made per frame.
const NOTES=72,OTHERS=40,LABELS=16,MERGE=.06;
const laneLastT=[-9,-9,-9,-9];
function inkRgb(ink,r,g,b,k){ink(Math.round(r*k),Math.round(g*k),Math.round(b*k));}
function paintRoll({sound,wipe,ink,box,line,write,screen}){
 const w=screen.width,h=screen.height,t=origin===null?-1:sound.time-origin,active=phase==='playing';
 // The screen IS the seat's colour: full strength while a piece is prepared
 // or playing, a darker shade at rest so the room still reads as six colours.
 // Everything else is drawn in an ink chosen against that colour's luminance.
 const color=cfg?.color||[143,209,63],lit=['prepared','countdown','playing'].includes(phase)?1:.35;
 wipe(Math.round(color[0]*lit),Math.round(color[1]*lit),Math.round(color[2]*lit));
 const lum=(.2126*color[0]+.7152*color[1]+.0722*color[2])*lit,dark=lum>110;
 const ir=dark?16:248,ig=dark?16:248,ib=dark?24:250;                     // the ink
 const tint=k=>dark?[Math.round(color[0]*lit/k),Math.round(color[1]*lit/k),Math.round(color[2]*lit/k)]:[Math.round(255-(255-color[0]*lit)/k),Math.round(255-(255-color[1]*lit)/k),Math.round(255-(255-color[2]*lit)/k)];   // darker on a bright ground, lighter on a deep one
 if(error){ink(ir,ig,ib);write(error,{x:8,y:h-24,font:FONT,size:1});}
 if(!cfg)return;
 // seat name, top left
 ink(ir,ig,ib);write((cfg.label||cfg.receiverId||'').toLowerCase(),{x:10,y:10,font:FONT,size:2});
 if(phase==='countdown'){const left=Math.max(0,origin-sound.time);centered(write,String(Math.ceil(left)),Math.round(h/2-40),8,w);inkRgb(ink,ir,ig,ib,.75);centered(write,(cfg.title||'').toLowerCase(),Math.round(h/2+50),2,w);return;}
 if(phase==='prepared'){inkRgb(ink,ir,ig,ib,.75);centered(write,'ready',Math.round(h/2-10),3,w);return;}
 if(!active&&phase!=='finished'){inkRgb(ink,ir,ig,ib,.6);centered(write,phase,Math.round(h/2-10),2,w);return;}
 const beat=60/cfg.bpm,bpb=cfg.beatsPerBar||4,bar=Math.floor(t/beat/bpb)+1,beatIn=Math.floor(t/beat)%bpb+1;
 // the beat lamp (a flash of ink) and the bar count, top right
 const pulse=Math.max(0,1-((t/beat)%1)*1.6);const tp=tint(1.35);ink(Math.round(tp[0]+(ir-tp[0])*pulse),Math.round(tp[1]+(ig-tp[1])*pulse),Math.round(tp[2]+(ib-tp[2])*pulse));box(w-34,10,22,22);
 inkRgb(ink,ir,ig,ib,.8);const bc=`${bar}.${beatIn}`;write(bc,{x:w-44-textWidth(bc,2),y:14,font:FONT,size:2});
 // sections along the top: a thin strip, the current one in ink, its name
 const secs=cfg.sections||[];
 if(secs.length){const total=cfg.duration;let cur=null;const td=tint(1.35);
  for(const s of secs){const x0=Math.round(s.startSec/total*(w-20))+10,x1=Math.round(s.endSec/total*(w-20))+10,on=t>=s.startSec&&t<s.endSec;if(on)cur=s;
   if(on){ink(ir,ig,ib);box(x0,40,Math.max(2,x1-x0-2),6);}else{ink(td[0],td[1],td[2]);box(x0,40,Math.max(2,x1-x0-2),6,'outline');}}
  if(cur){ink(ir,ig,ib);write(cur.name,{x:10,y:52,font:FONT,size:2});const nxt=secs[secs.indexOf(cur)+1];if(nxt){const left=Math.ceil((cur.endSec-t)/beat/bpb);inkRgb(ink,ir,ig,ib,.6);write(`${nxt.name} in ${left}`,{x:10+textWidth(cur.name,2)+14,y:56,font:FONT,size:1});}}
 }
 // Trio: the lyric being sung and the phrase this seat carries. Cursors,
 // not scans: lyrics and routes are sorted by time; a new run (t went back)
 // rewinds them.
 if(t<lastPaintT){lyricCursor=0;routeCursor=0;noteCursor=0;}lastPaintT=t;
 const lyrics=cfg.lyrics||[];while(lyricCursor<lyrics.length&&t>=lyrics[lyricCursor].t+lyrics[lyricCursor].dur+.8)lyricCursor++;
 let curL=null;for(let i=lyricCursor;i<lyrics.length&&lyrics[i].t-.3<=t;i++)if(t<lyrics[i].t+lyrics[i].dur+.8)curL=lyrics[i];
 if(curL){const size=fitSize(curL.text,w*.9,6);ink(ir,ig,ib);centered(write,curL.text,Math.round(h*.13),size,w);inkRgb(ink,ir,ig,ib,.7);write(curL.member,{x:Math.round((w-textWidth(curL.text,size))/2),y:Math.round(h*.13)-13,font:FONT,size:1});}
 const routes=cfg.routes||[];while(routeCursor<routes.length&&t>=routes[routeCursor].t+routes[routeCursor].dur)routeCursor++;
 let r=null;for(let i=routeCursor;i<routes.length&&routes[i].t<=t;i++){const x=routes[i];if(t<x.t+x.dur&&(!r||x.gain>r.gain))r=x;}
 if(r){
  const size=fitSize(r.text,w*.6,4),tw=textWidth(r.text,size),bw=tw+30,bh=size*10+30,bx=Math.round((w-bw)/2),by=Math.round(h*.24);
  const fade=Math.min(1,(t-r.t)/.15,(r.t+r.dur-t)/.4),tb=tint(r.role==='lead'?1.5:1.25);ink(tb[0],tb[1],tb[2]);box(bx,by,bw,bh);ink(ir,ig,ib);box(bx,by,bw,bh,'outline');inkRgb(ink,ir,ig,ib,.4+.6*fade);write(r.text,{x:bx+15,y:by+20,font:FONT,size});
  ink(r.rgb[0],r.rgb[1],r.rgb[2]);box(bx,by+bh-5,bw,5);ink(ir,ig,ib);write(r.role==='lead'?'LEAD':'ECHO',{x:bx+15,y:by+5,font:FONT,size:1});}
 // the roll
 const notes=cfg.notes||[];if(!notes.length)return;
 const ahead=6,behind=1.5,head=Math.round(w/3),px=w/(ahead+behind),top=Math.round(h*.36),bottom=Math.round(h*.78),laneY=[bottom+14,bottom+32,bottom+46,bottom+60];
 const lo=cfg.midiLow||36,hi=cfg.midiHigh||108,yOf=m=>Math.round(bottom-(Math.max(lo,Math.min(hi,m))-lo)/(hi-lo)*(bottom-top));
 inkRgb(ink,ir,ig,ib,.35);line(0,bottom,w,bottom);for(const y of laneY)line(0,y,w,y);
 inkRgb(ink,ir,ig,ib,.5+.5*pulse);line(head,top-10,head,laneY[3]+8);
 while(noteCursor<notes.length&&notes[noteCursor].t<t-behind)noteCursor++;
 let drawn=0,others=0,labels=0;laneLastT[0]=laneLastT[1]=laneLastT[2]=laneLastT[3]=-9;
 for(let k=noteCursor;k<notes.length&&drawn<NOTES;k++){
  const n=notes[k],dt=n.t-t;if(dt>ahead)break;
  const kind=KIND[n.i];
  if(!n.mine&&others>=OTHERS)continue;
  if(kind&&n.i!=='riser'&&n.i!=='voice'){if(n.t-laneLastT[kind.lane]<MERGE&&!n.mine)continue;laneLastT[kind.lane]=n.t;}   // a run of ticks reads as one
  if(!n.mine)others++;drawn++;
  const x=Math.round(head+dt*px),len=Math.max(3,Math.round(Math.min(n.dur||.15,3)*px));
  const on=t>=n.t&&t<n.t+Math.max(n.dur||.15,.12),just=t-n.t,hit=just>=0&&just<.18?1-just/.18:0;
  const dimF=n.mine?1:.32,rgb=n.rgb||(kind?kind.rgb:noteRgb(n.midi||60));
  if(kind&&n.i!=='riser'&&n.i!=='voice'){   // percussion: a mark in its lane, bigger for louder
   const y=laneY[kind.lane],sz=Math.max(3,Math.round(4+(n.gain||.1)*24));
   inkRgb(ink,rgb[0],rgb[1],rgb[2],dimF*(on?1:.6));if(n.i==='hat'){ink(ir,ig,ib);box(x-1,y-2,3,3);}else if(n.i==='donk'){box(x-sz/2,y-sz/2,sz,sz,'outline');}else{box(x-Math.round(sz/2),y-Math.round(sz/2),sz,sz);if(n.mine){ink(ir,ig,ib);box(x-Math.round(sz/2),y-Math.round(sz/2),sz,sz,'outline');}}
   if(hit&&n.mine){ink(rgb[0],rgb[1],rgb[2]);box(x-sz,y-sz,sz*2,sz*2,'outline');}
   continue;
  }
  if(n.i==='riser'||n.i==='voice'){const y=laneY[3];inkRgb(ink,rgb[0],rgb[1],rgb[2],dimF);box(x,y-4,len,8,'outline');if(n.text&&x<w&&labels<LABELS){labels++;write(n.text,{x:x+3,y:y-3,font:FONT,size:1});}continue;}
  // pitched: a bar by pitch, reverse bells a wedge that grows into the hit
  const y=yOf(n.midi||60),hh=Math.max(4,Math.round(4+(n.gain||.05)*90));
  inkRgb(ink,rgb[0],rgb[1],rgb[2],dimF*(dt<0&&!on?.5:1));
  if(n.i==='rbell'){const steps=4;for(let i=0;i<steps;i++){const f=i/steps;box(x+Math.round(len*f),y-Math.round(hh*f/2),Math.max(2,Math.round(len/steps)),Math.max(2,Math.round(hh*f)));}}
  else if(n.i==='sub'||n.i==='throat'||n.i==='bass'){inkRgb(ink,150,90,255,dimF);box(x,y-3,len,6);}
  else{box(x,y-Math.round(hh/2),len,hh);if(n.mine){ink(ir,ig,ib);box(x,y-Math.round(hh/2),len,hh,'outline');}}   // an ink edge so it reads on the bright ground
  if(hit&&n.mine){ink(ir,ig,ib);box(x-4,y-Math.round(hh/2)-4,len+8,hh+8,'outline');}
  if(n.mine&&n.label&&dt>0&&dt<1.2&&labels<LABELS&&n.i!=='sub'&&n.i!=='throat'){labels++;ink(ir,ig,ib);write(n.label,{x,y:y-Math.round(hh/2)-11,font:FONT,size:1});}
 }
}
export function paint(api){
 if(!(cfg?.lyrics||[]).length&&(cfg?.notes||[]).length)return paintRoll(api);   // no words (Femrag): the incoming-notes roll
 const {sound,wipe,ink,write,screen}=api;
 // Active words only. The seat's colour edge to edge and the word being
 // sung, huge, centred; the singer's name small above it. Nothing else.
 const w=screen.width,h=screen.height,t=origin===null?-1:sound.time-origin,active=phase==='playing';
 const color=cfg?.color||[143,209,63];
 const live=phase==='playing'||phase==='countdown'||phase==='prepared';
 const bg=live?color:color.map(v=>Math.round(v*.35));wipe(...bg);
 const lum=(0.2126*bg[0]+0.7152*bg[1]+0.0722*bg[2])/255;
 const inkRGB=lum>.5?[8,8,12]:[250,250,250],dimRGB=lum>.5?[Math.round(bg[0]*.55),Math.round(bg[1]*.55),Math.round(bg[2]*.55)]:[Math.round(bg[0]*.5+110),Math.round(bg[1]*.5+110),Math.round(bg[2]*.5+110)];
 if(!cfg)return;
 if(error){ink(...inkRGB);write(error,{x:8,y:h-24,font:FONT,size:1});return;}
 if(phase==='countdown'){const left=Math.max(0,origin-sound.time);ink(...inkRGB);centered(write,String(Math.ceil(left)),Math.round(h/2-40),8,w);return;}
 if(!active)return;
 const lyrics=cfg.lyrics||[];
 while(wordCursor<lyrics.length&&t>=lyrics[wordCursor].t+lyrics[wordCursor].dur+2.5)wordCursor++;
 if(wordCursor>0&&t<lyrics[wordCursor-1].t)wordCursor=0;
 // the line being sung: the latest lead line that has started (hums are never words)
 let cur=null;for(let i=wordCursor;i<lyrics.length&&lyrics[i].t<=t;i++)if(lyrics[i].role!=='hum')cur=lyrics[i];
 if(!cur)return;
 const linger=cur.answer?2.5:.8;if(t>cur.t+cur.dur+linger)return;
 const syls=cur.syllables||[];let sung=-1;for(let i=0;i<syls.length;i++){if(syls[i].t<=t)sung=i;else break;}
 if(sung<0)return;
 // the word that owns the sung syllable: syllables run through the words in order
 const words=cur.text.split(' ');let k=0,word=words[0]||'';
 for(const wd of words){let acc='',take=0;while(k+take<syls.length&&acc.length<wd.length){acc+=syls[k+take].text;take++;}if(take===0)take=1;if(sung<k+take){word=wd;break;}k+=take;}
 const size=fitSize(word,w*.9,cur.answer?14:12);
 ink(...dimRGB);write(cur.member,{x:Math.round((w-textWidth(cur.member,2))/2),y:Math.round(h/2)-size*5-34,font:FONT,size:2});
 ink(...inkRGB);centered(write,word,Math.round(h/2-size*5),size,w);
}
export function act({event,sound}){if(event.is('keyboard:down')&&event.key==='Escape')stop(sound);}
export function leave(){if(api)stop(api.sound);}
