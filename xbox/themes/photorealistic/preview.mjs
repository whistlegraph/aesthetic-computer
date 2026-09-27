import {manifest,createThemeRenderer,canvasDriver} from './theme.mjs';
const canvas=document.querySelector('canvas'),ctx=canvas.getContext('2d',{alpha:false});
const images={};
await Promise.all(Object.entries(manifest.assets).map(async([key,path])=>{
  const image=new Image();image.src=path;await image.decode();images[key]=image;
}));
const renderer=createThemeRenderer(canvasDriver(ctx,images));
let paused=false,time=0,previous=performance.now(),samples=[],frames=0;
document.querySelector('select').onchange=e=>renderer.setTheme(e.target.value);
document.querySelector('#motion').onclick=e=>{paused=!paused;e.target.textContent=paused?'Animate':'Pause';};
function pose(seat,t) {
  const facing=seat===0?1:-1,phase=t*3.4+seat*1.8;
  const x=(seat===0?360:920)+Math.sin(t*.5+seat)*36;
  const y=490+Math.sin(phase)*4,hip=[x,y],neck=[x+facing*5,y-59];
  const shoulder=[x+facing*5,y-47],elbow=[x+facing*29,y-40],hand=[x+facing*53,y-51];
  const kneeA=[x-18+Math.sin(phase)*9,y+34],footA=[x-27+Math.sin(phase)*15,y+74];
  const kneeB=[x+18-Math.sin(phase)*9,y+34],footB=[x+27-Math.sin(phase)*15,y+74];
  return {seat,facing,head:[neck[0],neck[1]-29],radius:33,hand,
    segments:[[hip,neck,18],[shoulder,elbow,12],[elbow,hand,12],[hip,kneeA,14],
      [kneeA,footA,12],[hip,kneeB,14],[kneeB,footB,12]]};
}
function render(t) {
  renderer.background(1280,720);
  renderer.platform(640,590,1120,44);
  renderer.platform(220,335,230,29);renderer.platform(1060,335,230,29);
  renderer.platform(640,257,250,29);
  renderer.prop('skateboard',620+Math.sin(t)*95,563,99,33,Math.sin(t)*.015);
  renderer.prop('rocket',640,370+Math.sin(t*1.9)*17,62,39,Math.sin(t)*.1);
  renderer.fighter(pose(0,t));renderer.fighter(pose(1,t));
  ctx.font='bold 27px system-ui';ctx.textAlign='left';ctx.fillStyle='#ba99ff';ctx.fillText('AC',50,51);
  ctx.textAlign='right';ctx.fillStyle='#a4e773';ctx.fillText('XBOX',1230,51);
  ctx.font='18px system-ui';ctx.fillStyle='#ccd6e2';ctx.fillText('UNDERPASS',1230,81);
}
function frame(now) {
  if(!paused)time+=Math.min(.05,(now-previous)/1000);previous=now;
  const start=performance.now();render(time);samples.push(performance.now()-start);frames++;
  if(samples.length>600)samples.shift();
  if(frames%30===0)document.querySelector('output').textContent=`${renderer.theme==='flat'?'Flat':'Miniature'} · ${(samples.reduce((a,b)=>a+b,0)/samples.length).toFixed(2)} ms draw submission`;
  requestAnimationFrame(frame);
}
globalThis.themePreview={ready:true,renderer,render,setTime(t){time=t;paused=true;render(t);},
  state:()=>({time,theme:renderer.theme,frames,samples:[...samples]})};
requestAnimationFrame(frame);
