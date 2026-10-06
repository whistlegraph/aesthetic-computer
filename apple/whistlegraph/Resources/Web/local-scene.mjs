// Only canonical scenes opt into local edits. Modified/generated code falls back safely.
export const COLORS=['pink','red','blue','green','yellow','orange','purple','white'];
const valid=s=>s&&Object.keys(s).sort().join(',')==='bounce,color,shape,size,speed,x,y'&&['circle','square','line'].includes(s.shape)&&COLORS.includes(s.color)&&typeof s.bounce==='boolean'&&['size','speed','x','y'].every(k=>Number.isFinite(s[k]))&&s.size>=.04&&s.size<=.4&&s.speed>=.25&&s.speed<=4&&s.x>=.1&&s.x<=.9&&s.y>=.1&&s.y<=.9;
function legacySource(scene){
 if(!valid(scene))throw Error('Invalid local scene');
 const s=Object.fromEntries(['shape','color','x','y','size','bounce','speed'].map(k=>[k,scene[k]]));
 return `// Whistlegraph scene v1\nconst scene = ${JSON.stringify(s)};\nlet phase = 0;\nexport function sim() { phase += 0.06 * scene.speed; }\nexport function paint({wipe, ink, screen}) {\n  wipe(24, 18, 30);\n  const r = Math.min(screen.width, screen.height) * scene.size;\n  const x = Math.max(r, Math.min(screen.width - r, screen.width * scene.x));\n  const baseY = screen.height * scene.y;\n  const y = Math.max(r, Math.min(screen.height - r, baseY - (scene.bounce ? Math.abs(Math.sin(phase)) * screen.height * 0.15 : 0)));\n  ink(scene.color);\n  if (scene.shape === "circle") ink(scene.color).circle(x, y, r, true);\n  else ink(scene.color).box(x - r, y - r, r * 2, r * 2);\n}\n`;
}
export function sceneSource(scene,motionId='local'){
 if(!/^[a-z0-9-]{1,64}$/i.test(motionId))throw Error('Invalid motion identity');
 const base=legacySource(scene);
 return (scene.shape==='line'?base.replace('if (scene.shape === \"circle\") ink(scene.color).circle(x, y, r, true);\n  else ink(scene.color).box(x - r, y - r, r * 2, r * 2);','ink(scene.color).line(x - r, y, x + r, y);'):base)
  .replace('// Whistlegraph scene v1\n',`// Whistlegraph scene v2\nconst motionId = "${motionId}";\n`)
  .replace('let phase = 0;\nexport function sim() { phase += 0.06 * scene.speed; }',`let motion = {id: motionId, phase: 0};
export function boot({store}) {
  const saved = store["whistlegraph:local-motion"];
  motion = saved && saved.id === motionId && Number.isFinite(saved.phase) ? saved : motion;
  store["whistlegraph:local-motion"] = motion;
}
export function sim() { if (scene.bounce) motion.phase = (motion.phase + 0.06 * scene.speed) % (Math.PI * 2); }`)
  .replace('scene.bounce ? Math.abs(Math.sin(phase)) * screen.height * 0.15 : 0','Math.abs(Math.sin(motion.phase)) * screen.height * 0.15');
}
function identity(source){return source.match(/^\/\/ Whistlegraph scene v2\nconst motionId = "([a-z0-9-]{1,64})";\n/i)?.[1];}
export function readScene(source){
 source=source.replaceAll('Walkieware scene','Whistlegraph scene').replaceAll('walkieware:local-motion','whistlegraph:local-motion');
 try{
  const id=identity(source),m=source.match(/(?:^|\n)const scene = (.+);\n/);if(!m)return null;
  const s=JSON.parse(m[1]),canonical=id?sceneSource(s,id):legacySource(s);
  return canonical.trimEnd()===source.trimEnd()?s:null;
 }catch{return null;}
}
export function localEdit(source,text){
 if(typeof text!=='string')return null;
 const t=text.trim().toLowerCase().replace(/[.!?]+$/,'').replace(/^please /,'');
 let scene=readScene(source),action;
 if(!source){
  const m=t.match(/^(?:(?:make|draw|create)(?: me)? |i want )?(?:a |an )?(pink|red|blue|green|yellow|orange|purple|white)?\s*(circle|square|line)$/);
  if(!m)return null;
  scene={shape:m[2],color:m[1]||'pink',x:.5,y:.5,size:.2,bounce:false,speed:1};action='create';
 }else{
  if(!scene)return null;
  scene={...scene};
  const color=t.match(/^(?:(?:make|turn) (?:it|the (?:circle|square)) |(?:recolor|colour|color)(?: it)? )?(pink|red|blue|green|yellow|orange|purple|white)$/);
  if(color){scene.color=color[1];action='color';}
  else if(/^(?:make it |a little )?(bigger|larger|smaller)$/.test(t)){scene.size=Math.min(.4,Math.max(.04,scene.size+(/smaller/.test(t)?-.025:.025)));action='size';}
  else if(/^(?:move (?:it )?)?(left|right|up|down)$/.test(t)){const d=t.split(' ').at(-1),k=['left','right'].includes(d)?'x':'y';scene[k]=Math.min(.9,Math.max(.1,scene[k]+(['left','up'].includes(d)?-.05:.05)));action='position';}
  else if(/^(?:make it )?(faster|slower)$/.test(t)&&scene.bounce){scene.speed=Math.min(4,Math.max(.25,scene.speed*(t.endsWith('faster')?1.25:.8)));action='speed';}
  else if(['bounce','make it bounce','start bouncing'].includes(t)){scene.bounce=true;action='bounce';}
  else if(['stop bouncing','stop moving','freeze'].includes(t)){scene.bounce=false;action='freeze';}
  else if(['center it','centre it','center'].includes(t)){scene.x=.5;scene.y=.5;action='center';}
  else return null;
  for(const k of ['x','y','size','speed'])scene[k]=Math.round(scene[k]*100000)/100000;
 }
 const next=sceneSource(scene,identity(source.replace('Walkieware scene','Whistlegraph scene'))||crypto.randomUUID());return {action,scene,source:next,changed:next.trimEnd()!==source.trimEnd()};
}
