import {trackInfo,trackLines} from './track-info.mjs';
import font from './font.mjs';
import {voicePosition,sourceGain,noteColor} from './spatial.mjs';
export async function startDisplay(canvas,snapshot){
 const g=canvas.getContext('2d');const W=683,H=384;canvas.width=W;canvas.height=H;g.imageSmoothingEnabled=false;
 const own=[195,221,133],names=['LEFT FRONT','RIGHT FRONT','RIGHT REAR','LEFT REAR','CENTER REAR','HELD CENTER'];
 const ink=(...c)=>{g.fillStyle=g.strokeStyle=`rgb(${c.join(',')})`;};
 const box=(x,y,w,h,outline=false)=>outline?g.strokeRect(Math.round(x)+.5,Math.round(y)+.5,Math.round(w),Math.round(h)):g.fillRect(Math.round(x),Math.round(y),Math.round(w),Math.round(h));
 const line=(x,y,x2,y2)=>{g.beginPath();g.moveTo(Math.round(x)+.5,Math.round(y)+.5);g.lineTo(Math.round(x2)+.5,Math.round(y2)+.5);g.stroke();};
 const circle=(x,y,r,fill=true)=>{g.beginPath();g.arc(Math.round(x),Math.round(y),Math.round(r),0,Math.PI*2);fill?g.fill():g.stroke();};
 function write(text,x,y,size=1){for(const char of String(text)){const glyph=font[char.charCodeAt(0)-32]||font[31];for(let row=0;row<10;row++)for(let col=0;col<6;col++)if(glyph[row]&(0x80>>col))g.fillRect(Math.round(x+col*size),Math.round(y+row*size),size,size);x+=6*size;}}
 function drawTrack(track){
  ink(8,12,18);box(0,H-65,W,65);
  ink(245,241,220);const lines=trackLines(track.title);
  lines.forEach((text,i)=>write(text,10,H-60+i*21,2));
  const time=v=>Math.floor(v/60)+':'+String(Math.floor(v%60)).padStart(2,'0');
  ink(155,180,195);write(track.phase.toUpperCase()+'  '+time(track.elapsed)+' / '+time(track.duration),10,H-14);
  ink(42,53,66);box(W-170,H-13,160,4);
  ink(195,221,133);if(track.duration)box(W-170,H-13,160*track.elapsed/track.duration,4);
 }
 function draw(){
  const concert=document.body.classList.contains('concert');
  const s=snapshot(),world=s.world||{},track=trackInfo(s.score,s.state,s.t),t=track.elapsed;const sounding=s.live&&s.armed;
  const subEvents=s.score?.events||[],current=subEvents.filter(e=>e.t<=t&&e.t+e.dur>t);const pulse=sounding&&current.length>0;
  ink(pulse?23:12,pulse?33:15,pulse?26:23);box(0,0,W,H);
  const frames=subEvents.filter(e=>e.t+e.dur+.6>=t&&e.t<=t+3).sort((a,b)=>b.t-a.t);
  if(s.live)for(const e of frames){
   const until=e.t-t,hit=until<=0,held=Math.max(.3,e.dur),left=hit?Math.max(0,1+until/held):1;
   const near=hit?left:1-Math.max(0,Math.min(1,until/3));const sc=hit&&-until<.08?1:.05+.95*Math.pow(near,hit?1.6:2.2);
   const w=Math.round(W*sc),h=Math.round(H*sc),x=Math.round((W-w)/2),y=Math.round((H-h)/2),bright=hit?.35+.65*left:.3+.7*near;
   const base=noteColor(e.note)||own,col=base.map(v=>Math.round(v*bright)),dark=base.map(v=>Math.round(v*bright*.45));
   if(hit&&-until<.08){ink(...col);box(x,y,w,h);ink(...dark);for(let yy=y+4;yy<y+h;yy+=8)line(x,yy,x+w,yy);}
   else{if(sc>.12){ink(...dark);for(let yy=y+3;yy<y+h-1;yy+=hit?6:4)line(x+2,yy,x+w-3,yy);}ink(...(e.note?.includes('#')?[225,225,235]:col));box(x,y,w,h,true);if(sc>.25){ink(...dark);box(x+2,y+2,w-4,h-4,true);}}
   if(!concert&&e.note&&sc>.18){ink(...col);write(e.note,x+4,y+3,sc>.6?3:sc>.35?2:1);}
  }
  if(concert){drawTrack(track);requestAnimationFrame(draw);return;}
  ink(...own);write('OUTPUT',10,10);write('S',10,28,5);ink(210,225,240);write('SUB',75,9);write('AC OS / '+(s.state?.source||'CONNECTING'),75,26);
  ink(255,220,100);write('SUB',W-44,28,2);ink(240,245,250);const vol=Math.round(s.level*100)+'%';write(vol,W-8-vol.length*6,8);
  const cx=W/2,cy=H*.46,r=Math.min(W*.28,H*.24),angles=[324,36,108,252,180],levels=Array(6).fill(0);
  if(s.live)(world.lanes||[]).forEach((lane,k)=>{for(const e of lane.events){if(e.t>t)break;if(e.t+e.dur<t)continue;const p=voicePosition(world,k,t);for(let seat=0;seat<6;seat++)levels[seat]+=e.g*sourceGain(world,p,seat,6);}});
  ink(95,110,130);circle(cx,cy,r,false);
  for(let seat=0;seat<6;seat++){const a=(angles[seat]||0)*Math.PI/180,x=seat===5?cx:cx+Math.sin(a)*r,y=seat===5?cy:cy-Math.cos(a)*r,lit=levels[seat]>.005;
   ink(...(lit?(world.seatColors?.[seat]||[160,205,235]):[65,78,95]));circle(x,y,lit?15+Math.min(12,levels[seat]*12):10);
   ink(lit?255:165,lit?245:180,lit?220:195);write(names[seat],x-names[seat].length*3,y+33);
  }
  const sx=W-66,sy=H*.51;ink(12,18,27);box(sx-49,sy-45,98,114);ink(...(s.armed?own:[75,85,95]));circle(sx,sy,pulse?23:17);ink(255,255,255);circle(sx,sy,31,false);
  ink(238,243,221);write('SUB',sx-18,sy+34,2);const subLabel=s.armed?Math.round(s.level*100)+'%':'AUDIO OFF';ink(...own);write(subLabel,sx-subLabel.length*3,sy+57);
  drawTrack(track);
  requestAnimationFrame(draw);
 }draw();
}
