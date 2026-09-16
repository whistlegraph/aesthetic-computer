const {readFileSync,watch}=require('node:fs');
const {join}=require('node:path');
const {homedir}=require('node:os');
const FALLBACK={background:'#463264',foreground:'#ffffff',cursor:'#c81e64',selectionBackground:'#ff64ff55',black:'#463264',red:'#ff5a5a',green:'#00ff00',yellow:'#ff6400',blue:'#dcb4ff',magenta:'#ff64ff',cyan:'#dcb4ff',white:'#ffffff',brightBlack:'#aa96cd',brightRed:'#ff5aa0',brightGreen:'#82ff82',brightYellow:'#ffa03c',brightBlue:'#dcb4ff',brightMagenta:'#c81e64',brightCyan:'#dcb4ff',brightWhite:'#ffffff'};
function hex(rgb){if(!Array.isArray(rgb)||rgb.length!==3||rgb.some(c=>!Number.isInteger(c)||c<0||c>255))throw new Error('Invalid Slab RGB');return '#'+rgb.map(c=>c.toString(16).padStart(2,'0')).join('');}
function channels(color){return color.slice(1).match(/../g).map(c=>parseInt(c,16));}
function luminance(rgb){return rgb.map(c=>{c/=255;return c<=.04045?c/12.92:((c+.055)/1.055)**2.4;}).reduce((sum,c,i)=>sum+c*[.2126,.7152,.0722][i],0);}
function contrast(a,b){const x=luminance(channels(a)),y=luminance(channels(b));return (Math.max(x,y)+.05)/(Math.min(x,y)+.05);}
// Preserve each ANSI hue, darkening/lightening only as far as readability needs.
function readable(color,background){
 if(contrast(color,background)>=4.5)return color;
 const source=channels(color),target=contrast('#000000',background)>contrast('#ffffff',background)?0:255;
 for(let step=1;step<=100;step++){
  const candidate=hex(source.map(c=>Math.round(c+(target-c)*step/100)));
  if(contrast(candidate,background)>=4.5)return candidate;
 }
 return hex([target,target,target]);
}
function resolveTheme(document,status){
 if(document?.version!==1||document.enabled!==true)return {...FALLBACK};
 const value=document.palettes?.[status]||document.palettes?.blank;
 try {
  const background=hex(value.background),foreground=hex(value.foreground),bold=hex(value.bold),cursor=hex(value.cursor);
  const accents=Object.fromEntries(['red','green','yellow','blue','magenta','cyan','brightBlack','brightRed','brightGreen','brightYellow','brightBlue','brightMagenta','brightCyan'].map(key=>[key,readable(FALLBACK[key],background)]));
  return {...accents,background,foreground,cursor,selectionBackground:cursor+'55',black:background,white:foreground,brightWhite:bold};
 }catch{return {...FALLBACK};}
}
function followSlabTheme(onTheme,{directory=join(process.env.EASEL_SLAB_HOME||process.env.SLAB_HOME||join(homedir(),'.local','share','slab'),'state')}={}){
 let status='blank',previous='',watcher;
 const refresh=()=>{
  let document;try {const data=readFileSync(join(directory,'theme.json'),'utf8');if(data.length<=65536)document=JSON.parse(data);}catch{}
  const theme=resolveTheme(document,status),key=JSON.stringify(theme);
  if(key!==previous){previous=key;onTheme(theme);}
 };
 try{watcher=watch(directory,(_event,name)=>{if(!name||String(name)==='theme.json')refresh();});}catch{}
 refresh();return {setStatus(value){status=value||'blank';refresh();},close(){watcher?.close();}};
}
module.exports={resolveTheme,followSlabTheme,FALLBACK};
