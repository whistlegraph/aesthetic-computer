// Editable still-image data. No scripts, remote images, animation, or audio.
export const starterPainting=()=>({format:1,title:'Untitled',width:768,height:768,background:'#ffffff',layers:[]});
const color=value=>typeof value==='string'&&(/^(#[0-9a-f]{6}(?:[0-9a-f]{2})?|transparent)$/i.test(value));
export function validatePainting(input){
 const p=structuredClone(input);
 if(!p||p.format!==1||typeof p.title!=='string'||p.title.length>80||!p.title.trim())throw Error('Give the painting a name of 1–80 characters.');
 if(![p.width,p.height].every(n=>Number.isInteger(n)&&n>=16&&n<=2048)||p.width*p.height>4194304)throw Error('Painting dimensions must be 16–2048 pixels.');
 if(!color(p.background)||!Array.isArray(p.layers)||p.layers.length>512)throw Error('Use a hex background and at most 512 layers.');
 const ids=new Set();
 for(const l of p.layers){
  if(!l||typeof l.id!=='string'||!/^[a-z0-9_-]{1,64}$/i.test(l.id)||ids.has(l.id))throw Error('Every layer needs a unique ID.');ids.add(l.id);
  if(!['rect','ellipse','polygon','path','text','raster'].includes(l.kind))throw Error('Unsupported painting layer.');
  if(l.opacity!==undefined&&(!Number.isFinite(l.opacity)||l.opacity<0||l.opacity>1))throw Error('Opacity must be 0–1.');
  if(l.kind==='raster'){
   if(l.asset!==undefined){if(typeof l.asset!=='string'||!/^[a-f0-9-]{36}$/.test(l.asset))throw Error('Invalid painting asset.');}
   else if(typeof l.data!=='string'||l.data.length>2800000||!/^iVBORw0KGgo[A-Za-z0-9+/]*={0,2}$/.test(l.data))throw Error('Use a PNG under 2 MB.');
  }else{
   if(!color(l.color))throw Error('Layer colors must be hex colors.');
   if(l.kind==='polygon'||l.kind==='path'){
    if(!Array.isArray(l.points)||l.points.length<2||l.points.length>2048||l.points.some(pt=>!Array.isArray(pt)||pt.length!==2||pt.some(n=>!Number.isFinite(n)||Math.abs(n)>8192)))throw Error('Use 2–2048 coordinate pairs.');
    if(l.kind==='path'&&(!Number.isFinite(l.stroke)||l.stroke<=0||l.stroke>512))throw Error('Path stroke must be 0–512 pixels.');
   }else{
    if(![l.x,l.y].every(n=>Number.isFinite(n)&&Math.abs(n)<=8192))throw Error('Invalid layer position.');
    if(l.kind==='text'){
     if(typeof l.text!=='string'||l.text.length>256||!Number.isFinite(l.size)||l.size<1||l.size>512)throw Error('Text needs a size of 1–512 and at most 256 characters.');
    }
   }
  }
  if(['rect','ellipse','raster'].includes(l.kind)&&![l.x,l.y,l.width,l.height].every(n=>Number.isFinite(n)&&Math.abs(n)<=8192)||['rect','ellipse','raster'].includes(l.kind)&&(l.width<=0||l.height<=0))throw Error('Invalid layer rectangle.');
 }
 if(JSON.stringify(p).length>3000000)throw Error('Painting is too large to save on this phone.');
 return p;
}
export const paintingFilename=p=>(p.title.normalize('NFKD').replace(/[\u0300-\u036f]/g,'').toLowerCase().replace(/[^a-z0-9]+/g,'-').replace(/^-|-$/g,'').slice(0,80)||'untitled')+'.png';
export async function renderPainting(canvas,input,loadAsset){
 const p=validatePainting(input),images=new Map();
 for(const l of p.layers.filter(l=>l.kind==='raster')){
  const image=new Image();image.src='data:image/png;base64,'+(l.asset?await loadAsset(l.asset):l.data);await image.decode();
  if(image.naturalWidth>2048||image.naturalHeight>2048)throw Error('Imported PNG dimensions exceed 2048 pixels.');images.set(l.id,image);
 }
 canvas.width=p.width;canvas.height=p.height;const c=canvas.getContext('2d');
 c.clearRect(0,0,p.width,p.height);c.fillStyle=p.background;c.fillRect(0,0,p.width,p.height);
 for(const l of p.layers){
  c.save();c.globalAlpha=l.opacity??1;c.fillStyle=l.color||'#000000';
  if(l.kind==='rect')c.fillRect(l.x,l.y,l.width,l.height);
  if(l.kind==='ellipse'){c.beginPath();c.ellipse(l.x+l.width/2,l.y+l.height/2,l.width/2,l.height/2,0,0,Math.PI*2);c.fill();}
  if(l.kind==='polygon'||l.kind==='path'){
   c.beginPath();l.points.forEach((pt,i)=>i?c.lineTo(...pt):c.moveTo(...pt));
   if(l.kind==='polygon'){c.closePath();c.fill();}else{c.strokeStyle=l.color;c.lineWidth=l.stroke;c.lineCap='round';c.lineJoin='round';c.stroke();}
  }
  if(l.kind==='text'){c.font=`${l.size}px Comic, sans-serif`;c.textBaseline='top';c.fillText(l.text,l.x,l.y);}
  if(l.kind==='raster')c.drawImage(images.get(l.id),l.x,l.y,l.width,l.height);
  c.restore();
 }
 return canvas;
}
export const PAINTING_SCHEMA={type:'object',properties:{format:{type:'integer',enum:[1]},title:{type:'string'},width:{type:'integer'},height:{type:'integer'},background:{type:'string'},layers:{type:'array',items:{type:'object',properties:{id:{type:'string'},kind:{type:'string',enum:['rect','ellipse','polygon','path','text']},color:{type:'string'},opacity:{type:'number'},x:{type:'number'},y:{type:'number'},width:{type:'number'},height:{type:'number'},points:{type:'array',items:{type:'array',items:{type:'number'}}},stroke:{type:'number'},text:{type:'string'},size:{type:'number'}},required:['id','kind','color'],additionalProperties:false}}},required:['format','title','width','height','background','layers'],additionalProperties:false};
export const requestsNewPainting=text=>/^(?:please\s+)?(?:(?:make|create|start)\s+)?(?:a\s+)?new\s+(?:empty\s+|blank\s+)?(?:painting|png|still image)[.!]?$/i.test(String(text).trim());
