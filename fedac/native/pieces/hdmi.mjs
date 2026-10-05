// HDMI, 26.10.04
// Sharpness, color, motion and overscan checks for an attached display.
import { savedWifi } from '../lib/saved-wifi.mjs';
const connectSaved=savedWifi();
export function sim(api){connectSaved(api);}
let page=0,frame=0,fps=0,fpsFrames=0,fpsAt=0;
const names=['PIXEL GRID','TYPE + EDGES','COLOR','MOTION','DISPLAY INFO'];
let info='',output=null,probe=null,requestSerial=0,notice='';
export function boot({system}) { system.startSSH?.(); frame=0;page=0;fpsAt=Date.now();fpsFrames=0;info=system.readFile?.('/tmp/ac-hdmi-info.txt')||'Display details unavailable'; }
export function paint({wipe,ink,box,line,circle,write,screen,system}) {
 frame++;
 fpsFrames++;
 const now=Date.now();
 if(now-fpsAt>=500){fps=fpsFrames*1000/(now-fpsAt);fpsFrames=0;fpsAt=now;}
 if(frame===1||frame%30===0){
  try{output=JSON.parse(system.readFile?.('/tmp/ac-hdmi-state.json')||'null');}catch{output=null;}
  try{probe=JSON.parse(system.readFile?.('/tmp/ac-hdmi-probe.json')||'null');}catch{probe=null;}
  info=system.readFile?.('/tmp/ac-hdmi-info.txt')||info;
 }
 const w=screen.width,h=screen.height,m=Math.max(8,Math.round(w*.035));
 const size=Math.max(1,Math.floor(w/340)),head=Math.max(2,Math.floor(w/190));
 wipe(12,12,16);
 ink(255,255,255);line(0,0,w-1,0);line(w-1,0,w-1,h-1);line(w-1,h-1,0,h-1);line(0,h-1,0,0);
 for(const [x,y] of [[0,0],[w-9,0],[0,h-9],[w-9,h-9]]) {ink(255,60,130);box(x,y,9,9);}
 const fpsText=fps ? `${fps.toFixed(1)} FPS` : '-- FPS';
 ink(110,255,170);write(fpsText,{x:w-m-fpsText.length*6*head,y:m,size:head,font:'font_1'});
 ink(255,255,255);write(names[page],{x:m,y:m,size:head,font:'font_1'});
 const scale=output?.width ? Number(Math.min(output.width/w,output.height/h).toFixed(2)) : null;
 ink(185,190,205);write(output?.width ? `HDMI ${output.width}x${output.height} @ ${output.hz} Hz` : 'HDMI output: unavailable',{x:m,y:m+head*12,size,font:'font_1'});
 write(`SOURCE ${w}x${h}   SCALE ${scale??'?'}x   NEAREST`,{x:m,y:m+head*12+size*13,size,font:'font_1'});
 const top=m+head*12+size*29, bottom=h-m-54, ph=bottom-top;
 if(page===0) {
  const gap=8,pw=Math.floor((w-2*m-2*gap)/3);
  for(let panel=0;panel<3;panel++) {
   const x=m+panel*(pw+gap),step=panel+1;
   ink(255,255,255);box(x,top,pw,ph-18);
   ink(0,0,0);
   for(let i=0;i<pw;i+=step*2)box(x+i,top,Math.min(step,pw-i),Math.floor((ph-18)/2));
   for(let j=0;j<Math.floor((ph-18)/2);j+=step*2)box(x,top+Math.floor((ph-18)/2)+j,pw,step);
   ink(230,230,240);write(`${step}px stripes`,{x,y:bottom-14,size,font:'font_1'});
  }
 } else if(page===1) {
  let y=top;
  for(const s of [1,2,3]) {ink(250,250,250);write('Aa 0123456789',{x:m,y,size:s,font:'font_1'});y+=s*12+8;}
  ink(255,255,255);const cx=w*.77,cy=top+ph*.45,r=Math.min(ph*.35,w*.13);
  for(let n=0;n<64;n++){const a=n*Math.PI*2/64,b=(n+1)*Math.PI*2/64;line(cx+Math.cos(a)*r,cy+Math.sin(a)*r,cx+Math.cos(b)*r,cy+Math.sin(b)*r);}
  line(cx-r,cy,cx+r,cy);line(cx,cy-r,cx,cy+r);
 } else if(page===2) {
  const colors=[[255,255,255],[255,255,0],[0,255,255],[0,255,0],[255,0,255],[255,0,0],[0,0,255]];
  colors.forEach((c,i)=>{ink(...c);box(m+Math.floor(i*(w-2*m)/7),top,Math.ceil((w-2*m)/7),ph*.64);});
  for(let i=0;i<16;i++){ink(i*17,i*17,i*17);box(m+Math.floor(i*(w-2*m)/16),top+ph*.69,Math.ceil((w-2*m)/16),ph*.27);}
 } else if(page===3) {
  ink(32,35,43);for(let x=m;x<w-m;x+=16)line(x,top,x,bottom);
  const x=m+(frame*2%(w-2*m-12));ink(255,255,255);box(x,top,12,ph);
  ink(255,80,145);box(w-m-12-(frame%(w-2*m-12)),top+ph*.35,12,ph*.3);
 } else {
  const lines=[...info.split('\n'), 'FPS counts rendered frames, not display refresh.', probe?.error || notice].filter(Boolean).slice(0,8);
  ink(225,228,238);lines.forEach((t,i)=>write(t.slice(0,Math.floor((w-2*m)/(6*size))),{x:m,y:top+i*size*14,size,font:'font_1'}));
 }
 ink(185,190,205);
 const modeLine=probe?.pending ? `TEST ${probe.width}x${probe.height}@${probe.hz}: ENTER keeps; otherwise reverts` : (probe ? '1: 720p60   2: 1080p60   3: 4K30   Enter: keep test' : '');
 ink(...(probe?.pending?[255,191,90]:[185,190,205]));write(modeLine,{x:m,y:h-m-39,size:1,font:'font_1'});
 write('Ctrl -: finer pixels   Ctrl +: larger   Ctrl 0: reset',{x:m,y:h-m-26,size:1,font:'font_1'});
 write('Arrows / tap: test   F: Fia   N: news   Esc: prompt',{x:m,y:h-m-13,size:1,font:'font_1'});
}
export function act({event:e,system}) {
 const modes={'1':[1280,720,60],'2':[1920,1080,60],'3':[3840,2160,30]};
 if(e.is('keyboard:down')&&modes[e.key]){
   if(!probe){notice='Mode tests unavailable on this runtime';return;}
   requestSerial=Math.max(requestSerial+1,Math.floor(Date.now()/1000));
   if(system.writeFile('/tmp/ac-hdmi-request',modes[e.key].join(' ')+' '+requestSerial))notice='Mode requested; waiting for HDMI';
   else notice='Could not request mode';
   return;
 }
 if(e.is('keyboard:down:enter')&&probe?.pending){
   if(output?.width!==probe.width||output?.height!==probe.height||output?.hz!==probe.hz){notice='Waiting for the requested output';return;}
   system.writeFile('/tmp/ac-hdmi-confirm',String(probe.serial));
   const saved=system.writeFile('/mnt/hdmi-mode.txt',`${probe.width} ${probe.height} ${probe.hz} ${probe.serial}`);
   notice=saved?'Mode kept and saved':'Mode kept for this session; USB save failed';return;
 }
 if(e.is('keyboard:down:arrowright')||e.is('touch'))page=(page+1)%names.length;
 if(e.is('keyboard:down:arrowleft'))page=(page+names.length-1)%names.length;
 if(e.is('keyboard:down:f'))system.jump('fia-stars');
 if(e.is('keyboard:down:n'))system.jump('feral-file-now');
 if(e.is('keyboard:down:escape'))system.jump('prompt');
}
