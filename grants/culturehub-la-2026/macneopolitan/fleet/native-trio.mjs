// Trio receiver for the existing native engine. Starts are audio-clock based,
// dispatched on a simulation frame (NOT sample-accurate). Never auto-plays.
export const surface='drum'; // hide pointer emulation on the concert surface
let cfg,api,error='',phase='loading',origin=null,runId=null,seen='',lastReport=-1;
let assemblyStarted=0,target=null,eventsStarted=0;
let voices=[],cursor=0,startedAt=null,heartbeat=-Infinity,maxFrameGap=0,prior=null;
const instance=String(Date.now())+'-'+Math.random().toString(36).slice(2);
function read(system,path){const bytes=new Uint8Array(system.readFileBytes(path));let text='';for(let i=0;i<bytes.length;i+=4096)text+=String.fromCharCode.apply(null,bytes.subarray(i,i+4096));return JSON.parse(text);}
function stop(sound){for(const v of voices)v?.kill?.(.025);voices=[];sound.deck.pause(0);origin=null;cursor=0;phase=error?'error':'ready';}
export function boot(a){
 api=a;a.sound.microphone.close();a.sound.volume.setMono(true);a.sound.volume.setMonoOutput('left');
 try{
  cfg=read(a.system,'/pieces/trio-fleet-config.json');
  if(cfg.schema!=='trio-native-v1'||!Number.isFinite(cfg.duration)||cfg.duration<=0||!Array.isArray(cfg.events))throw Error('Invalid Trio config');
  for(const e of cfg.events)if(![e.t,e.dur,e.frequency,e.gain].every(Number.isFinite)||e.t<0||e.dur<=0||e.gain<0||e.gain>.25)throw Error('Invalid note');
  if(cfg.center){
   if(!/^\/pieces\/trio-voices-[a-f0-9]{16}\.wav$/.test(cfg.center.file)||!cfg.center.parts.every(p=>/^\/pieces\/trio-[a-f0-9]{16}-[0-9]+\.part$/.test(p)))throw Error('Invalid Center paths');
   a.system.writeFile(cfg.center.file+'.sha256','');
   const command='cat '+cfg.center.parts.join(' ')+' > '+cfg.center.file+'.tmp && mv '+cfg.center.file+'.tmp '+cfg.center.file+' && sha256sum '+cfg.center.file+' > '+cfg.center.file+'.sha256';
   a.system.pty.spawn('/bin/sh',['-c',command],80,24);phase='assembling';assemblyStarted=a.sound.time;
  }else phase='ready';
 }catch(e){error=String(e);phase='error';}
}
export function sim(a){
 const {sound,system}=a,now=sound.time;
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
   stop(sound);origin=cmd.startAt;target=origin;eventsStarted=0;runId=cmd.runId;phase='prepared';heartbeat=now;startedAt=null;maxFrameGap=0;
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
   const voice=sound.synth({type:e.wave||'sine',tone:e.frequency,volume:e.gain,duration:e.t+e.dur-t,attack:e.attack||.015,decay:e.release||.12});
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
   eventCount:cfg?.events.length,eventsStarted,duration:cfg?.duration,mono:sound.volume.mono,monoOutput:sound.volume.monoOutput,
   microphoneHot:sound.microphone.hot,displayMode:'concert',fullscreen:true,pointerHidden:true,clockPrecision:'simulation-frame',
   output:sound.speaker.amplitudes}));
 }
}
export function paint({sound,wipe,ink,box,screen}){
 const t=origin===null?-1:sound.time-origin,active=phase==='playing';
 const color=cfg?.color||[143,209,63];wipe(12,15,23);
 if(!active)return;
 const pulse=Math.pow(Math.max(0,1-(t*cfg.bpm/60)%1),3);
 for(let i=0;i<5;i++){
  const sc=((t*.25+i*.2)%1),w=screen.width*sc,h=screen.height*sc;
  ink(...color.map(v=>Math.round(v*(.2+.65*pulse)*(1-sc*.5))));box((screen.width-w)/2,(screen.height-h)/2,w,h,'outline');
 }
}
export function act({event,sound}){if(event.is('keyboard:down')&&event.key==='Escape')stop(sound);}
export function leave(){if(api)stop(api.sound);}
