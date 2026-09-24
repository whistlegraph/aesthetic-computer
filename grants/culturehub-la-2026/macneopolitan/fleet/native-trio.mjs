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
function stop(sound){dark(api.system);for(const v of voices)v?.kill?.(.025);voices=[];sound.deck.pause(0);origin=null;cursor=0;phase=error?'error':'ready';}
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
   stop(sound);origin=cmd.startAt;target=origin;eventsStarted=0;runId=cmd.runId;phase='prepared';heartbeat=now;startedAt=null;maxFrameGap=0;brightSteps=14;
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
   eventCount:cfg?.events.length,eventsStarted,duration:cfg?.duration,mono:sound.volume.mono,monoOutput:sound.volume.monoOutput,mix:sound.volume.mix,brightness:system.brightness,
   microphoneHot:sound.microphone.hot,displayMode:'concert',fullscreen:true,pointerHidden:true,clockPrecision:'simulation-frame',
   output:sound.speaker.amplitudes}));
 }
}
// ---- notation ----------------------------------------------------------------
const FONT='6x10';
function textWidth(s,size){return s.length*6*size;}
function fitSize(s,maxW,maxSize){let size=maxSize;while(size>1&&textWidth(s,size)>maxW)size--;return size;}
function centered(write,s,y,size,w){write(s,{x:Math.round((w-textWidth(s,size))/2),y,font:FONT,size});}
const LAYER_RGB={inst:[255,214,120],perc:[200,200,200],harmony:[160,180,255],bed:[120,140,220],ornament:[255,170,220],sub:[120,120,120]};
export function paint({sound,wipe,ink,box,line,write,screen}){
 const w=screen.width,h=screen.height,t=origin===null?-1:sound.time-origin,active=phase==='playing';
 const color=cfg?.color||[143,209,63];wipe(12,15,23);
 if(error){ink(255,140,130);write(error,{x:8,y:h-24,font:FONT,size:1});}
 if(!cfg)return;
 // the breathing rings, dimmer now: the words sit in front of them
 if(active){
  const pulse=Math.pow(Math.max(0,1-(t*cfg.bpm/60)%1),3);
  for(let i=0;i<5;i++){const sc=((t*.25+i*.2)%1),rw=w*sc,rh=h*sc;ink(...color.map(v=>Math.round(v*(.08+.3*pulse)*(1-sc*.5))));box((w-rw)/2,(h-rh)/2,rw,rh,'outline');}
 }
 // seat name, top left; the bar count, top right
 ink(...color);write((cfg.label||cfg.receiverId||'').toLowerCase(),{x:10,y:10,font:FONT,size:2});
 if(phase==='countdown'){const left=Math.max(0,origin-sound.time);ink(240,240,240);centered(write,String(Math.ceil(left)),Math.round(h/2-40),8,w);ink(150,150,150);centered(write,'good morning, sophia',Math.round(h/2+50),2,w);return;}
 if(phase==='prepared'){ink(150,150,150);centered(write,'ready',Math.round(h/2-10),3,w);return;}
 if(!active&&phase!=='finished'){ink(110,110,110);centered(write,phase,Math.round(h/2-10),2,w);return;}
 const beat=60/cfg.bpm,bar=Math.floor(t/beat/3)+1;ink(120,120,120);write('bar '+bar,{x:w-10-textWidth('bar '+bar,2),y:10,font:FONT,size:2});
 // 1. the lyric being sung in the room, in the singer's colour; the next one dim
 const lyrics=cfg.lyrics||[];const cur=lyrics.filter(l=>t>=l.t-.3&&t<l.t+l.dur+.8).pop();const next=lyrics.find(l=>l.t>(cur?cur.t:t));
 const topY=Math.round(h*.16);
 if(cur){const size=fitSize(cur.text,w*.9,7);ink(...cur.rgb);centered(write,cur.text,topY,size,w);ink(...cur.rgb.map(v=>Math.round(v*.55)));write(cur.member,{x:Math.round((w-textWidth(cur.text,size))/2),y:topY-14,font:FONT,size:1});}
 if(next){const size=Math.max(1,fitSize(next.text,w*.7,3));ink(70,74,90);centered(write,next.text,topY+(cur?fitSize(cur.text,w*.9,7)*10+18:0),size,w);}
 // 2. what THIS seat carries right now: lead or echo of a phrase, boxed in the member's colour
 const mine=(cfg.routes||[]).filter(r=>t>=r.t&&t<r.t+r.dur);
 const midY=Math.round(h*.5);
 if(mine.length){
  const r=mine.sort((a,b)=>b.gain-a.gain)[0],size=fitSize(r.text,w*.8,5),tw=textWidth(r.text,size);
  const bw=tw+40,bh=size*10+34,bx=Math.round((w-bw)/2),by=midY-Math.round(bh/2);
  const fade=Math.min(1,(t-r.t)/.15,(r.t+r.dur-t)/.4);
  ink(...r.rgb.map(v=>Math.round(v*(r.role==='lead'?.35:.18)*fade)));box(bx,by,bw,bh);
  ink(...r.rgb.map(v=>Math.round(v*fade)));box(bx,by,bw,bh,'outline');
  write(r.text,{x:bx+20,y:by+22,font:FONT,size});
  const tag=r.role==='lead'?'LEAD':'ECHO '+(r.delay<beat*.75?'½':'1');ink(...r.rgb.map(v=>Math.round(v*.8*fade)));write(tag,{x:bx+20,y:by+6,font:FONT,size:1});
 }
 // 3. this seat's timeline: the next eight seconds of its own marks, a playhead at a third
 const y0=Math.round(h*.8),span=8,px=w/span,head=Math.round(w/3);
 ink(40,44,58);line(0,y0,w,y0);ink(200,200,200);line(head,y0-28,head,y0+28);
 for(const e of cfg.events){const dt=e.t-t;if(dt<-span/3||dt>span*2/3)continue;const x=Math.round(head+dt*px),ww=Math.max(3,Math.round(Math.min(e.dur,2)*px));const c=LAYER_RGB[e.layer]||[150,150,150],on=t>=e.t&&t<e.t+e.dur;
  const hh=e.layer==='perc'?6:Math.max(6,Math.min(24,Math.round(((e.note||48)-36)/2)));ink(...c.map(v=>Math.round(v*(on?1:.45))));if(on)box(x,y0-hh,ww,hh);else box(x,y0-hh,ww,hh,'outline');
  if(e.name&&on){ink(...c);write(e.name,{x,y:y0+6,font:FONT,size:1});}
 }
 for(const r of cfg.routes||[]){const dt=r.t-t;if(dt<-span/3||dt>span*2/3)continue;const x=Math.round(head+dt*px);ink(...r.rgb.map(v=>Math.round(v*(r.role==='lead'?.9:.5))));box(x,y0+10,Math.max(4,Math.round(r.dur*px)),r.role==='lead'?8:4);}
}
export function act({event,sound}){if(event.is('keyboard:down')&&event.key==='Escape')stop(sound);}
export function leave(){if(api)stop(api.sound);}
