// Editable classic Doom/Doom II projects. Geometry goes through ZDBSP; artwork stays editable.
import {compileWad,inspectWad,validateLevel} from './wad.mjs';
const encoder=new TextEncoder(),decoder=new TextDecoder();
const MAP=/^(?:E[1-4]M[1-9]|MAP(?:0[1-9]|[12][0-9]|3[0-2]))$/;
const DERIVED=new Set(['SEGS','SSECTORS','NODES','REJECT','BLOCKMAP']);
const RESERVED=new Set(['THINGS','LINEDEFS','SIDEDEFS','VERTEXES','SECTORS',...DERIVED,'PNAMES','TEXTURE1','TEXTURE2','S_START','S_END','F_START','F_END']);
const MAP_DATA=new Set(['THINGS','LINEDEFS','SIDEDEFS','VERTEXES','SECTORS',...DERIVED]);
function integer(n,min,max,label){if(!Number.isInteger(n)||n<min||n>max)throw Error(`Invalid ${label}`);return n;}
function named(n){if(typeof n!=='string'||!/^[-A-Z0-9_\[\]\\]{1,8}$/.test(n))throw Error('WAD names use 1–8 uppercase ASCII characters');return n;}
function array(a,max,label){if(!Array.isArray(a)||a.length>max)throw Error(`Invalid ${label}`);return a;}
function text(n,min,max,label){if(typeof n!=='string'||n.length<min||n.length>max)throw Error(`Invalid ${label}`);return n;}
function nameAt(data,at){return decoder.decode(data.subarray(at,at+8)).replace(/\0.*$/s,'');}
function nameTo(data,at,n){data.set(encoder.encode(n),at);}
function bytes(size,write){const d=new Uint8Array(size);write?.(new DataView(d.buffer),d);return d;}
export function wadLumps(data){return inspectWad(data).lumps.map(l=>({name:l.name,data:data.slice(l.offset,l.offset+l.size)}));}
export function packWad(lumps){
 const dir=12+lumps.reduce((n,l)=>n+l.data.length,0);if(dir+lumps.length*16>32*1024*1024)throw Error('WAD exceeds 32 MB');
 return bytes(dir+lumps.length*16,(v,d)=>{nameTo(d,0,'PWAD');v.setInt32(4,lumps.length,true);v.setInt32(8,dir,true);let offset=12;lumps.forEach((l,i)=>{d.set(l.data,offset);v.setInt32(dir+i*16,offset,true);v.setInt32(dir+i*16+4,l.data.length,true);nameTo(d,dir+i*16+8,named(l.name));offset+=l.data.length;});});
}
export function encode64(data){let s='';for(let at=0;at<data.length;at+=8192)s+=String.fromCharCode(...data.subarray(at,at+8192));return btoa(s);}
export function decode64(s){text(s,0,44*1024*1024,'binary data');if(!/^(?:[A-Za-z0-9+/]{4})*(?:[A-Za-z0-9+/]{2}==|[A-Za-z0-9+/]{3}=)?$/.test(s))throw Error('Invalid base64');return Uint8Array.from(atob(s),c=>c.charCodeAt(0));}
function sector(s){return {floor:integer(s.floor,-32768,32767,'floor height'),ceiling:integer(s.ceiling,-32768,32767,'ceiling height'),floorTexture:named(s.floorTexture),ceilingTexture:named(s.ceilingTexture),light:integer(s.light,0,255,'sector light'),special:integer(s.special??0,0,65535,'sector special'),tag:integer(s.tag??0,0,65535,'sector tag')};}
function thing(t){return {x:integer(t.x,-32768,32767,'thing x'),y:integer(t.y,-32768,32767,'thing y'),angle:integer(t.angle??0,0,359,'thing angle'),type:integer(t.type,1,65535,'thing type'),flags:integer(t.flags??7,0,65535,'thing flags')};}
function side(s,count){return {sector:integer(s.sector,0,count-1,'side sector'),x:integer(s.x??0,-32768,32767,'texture x'),y:integer(s.y??0,-32768,32767,'texture y'),upper:named(s.upper??'-'),lower:named(s.lower??'-'),middle:named(s.middle??'-')};}
function point(p){if(!Array.isArray(p)||p.length!==2)throw Error('Expected [x,y]');return [integer(p[0],-32768,32767,'vertex x'),integer(p[1],-32768,32767,'vertex y')];}
function mapData(m){
 if(!MAP.test(m.name))throw Error('Use E1M1–E4M9 or MAP01–MAP32');
 let raw=m;
 if(m.rooms){const rooms=array(m.rooms,1024,'rooms').map(r=>({...sector(r),wall:named(r.wall),polygon:array(r.polygon,2048,'polygon').map(point)}));raw={...roomsGeometry(rooms),name:m.name,things:m.things};}
 const vertices=array(raw.vertices,16384,'vertices').map(point),sectors=array(raw.sectors,2048,'sectors').map(sector);
 if(vertices.length<3||!sectors.length)throw Error('A map needs vertices and sectors');
 for(const s of sectors)if(s.ceiling<s.floor)throw Error('Ceiling is below floor');
 const sides=array(raw.sides,32767,'sides').map(s=>side(s,sectors.length));
 const lines=array(raw.lines,16384,'lines').map(l=>({a:integer(l.a,0,vertices.length-1,'line start'),b:integer(l.b,0,vertices.length-1,'line end'),right:integer(l.right,0,sides.length-1,'right side'),left:integer(l.left??-1,-1,sides.length-1,'left side'),flags:integer(l.flags??(l.left>=0?4:1),0,65535,'line flags'),special:integer(l.special??0,0,65535,'line special'),tag:integer(l.tag??0,0,65535,'line tag')}));
 if(!lines.length)throw Error('A map needs walls');
 for(const l of lines){if(l.a===l.b||vertices[l.a].every((n,i)=>n===vertices[l.b][i]))throw Error('Zero-length wall');if(l.left>=0&&!(l.flags&4))throw Error('A two-sided wall needs flag 4');}
 const things=array(raw.things,4096,'things').map(thing);if(things.filter(t=>t.type===1).length!==1)throw Error('Exactly one Player 1 start is required');
 // Every directed sector boundary must form closed loops. BSP is derived later.
 const balance=new Map();for(const l of lines){for(const [index,a,b] of [[l.right,l.a,l.b],[l.left,l.b,l.a]]){if(index<0)continue;const s=sides[index].sector;for(const [p,d] of [[a,1],[b,-1]]){const k=s+':'+p;balance.set(k,(balance.get(k)||0)+d);}}}
 if([...balance.values()].some(n=>n!==0))throw Error('Sector boundaries must close; connect shared walls with two sides');
 const player=things.find(t=>t.type===1);let spawn=false;
 sectors.forEach((s,i)=>{if(s.ceiling-s.floor<56)return;let inside=false;for(const l of lines){if((sides[l.right].sector===i)===(l.left>=0&&sides[l.left].sector===i))continue;const a=vertices[l.a],b=vertices[l.b];if((a[1]>player.y)!==(b[1]>player.y)&&player.x<(b[0]-a[0])*(player.y-a[1])/(b[1]-a[1])+a[0])inside=!inside;}if(inside)spawn=true;});
 if(!spawn)throw Error('Place Player 1 inside an open sector');
 return {name:m.name,vertices,sectors,sides,lines,things};
}
// Polygon convenience authoring still compiles into unrestricted classic map records.
export function roomsGeometry(rooms){
 const vertices=[],index=new Map(),sides=[],lines=[],edges=new Map();
 const vertex=p=>{const key=p.join(',');if(!index.has(key)){index.set(key,vertices.length);vertices.push(p);}return index.get(key);};
 const polygons=rooms.map(r=>{let p=r.polygon.map(point);if(p.length<3)throw Error('Room polygon needs 3 points');const area=p.reduce((n,a,i)=>{const b=p[(i+1)%p.length];return n+a[0]*b[1]-b[0]*a[1];},0);if(!area)throw Error('Room has no area');if(area>0)p=p.reverse();return p;});
 const all=polygons.flat();
 polygons.forEach((polygon,si)=>{polygon.forEach((p,i)=>{const q=polygon[(i+1)%polygon.length],dx=q[0]-p[0],dy=q[1]-p[1];if(!dx&&!dy)throw Error('Zero-length polygon edge');
  // Split T junctions so adjoining rooms can share part of a long edge.
  const points=[p,q,...all.filter(v=>((v[0]-p[0])*dy-(v[1]-p[1])*dx)===0&&(v[0]-p[0])*dx+(v[1]-p[1])*dy>0&&(v[0]-p[0])*dx+(v[1]-p[1])*dy<dx*dx+dy*dy)].sort((a,b)=>((a[0]-b[0])*dx+(a[1]-b[1])*dy));
  for(let j=0;j<points.length-1;j++){const a=vertex(points[j]),b=vertex(points[j+1]);if(a===b)continue;const key=[Math.min(a,b),Math.max(a,b)].join(':'),old=edges.get(key),sid=sides.length;
   sides.push({sector:si,x:0,y:0,upper:rooms[si].wall,lower:rooms[si].wall,middle:old?'-':rooms[si].wall});
   if(old){if(old.left>=0||old.a!==b||old.b!==a)throw Error('Rooms overlap or share an incorrectly oriented edge');old.left=sid;old.flags=4;sides[old.right].middle='-';}
   else{const line={a,b,right:sid,left:-1,flags:1,special:0,tag:0};edges.set(key,line);lines.push(line);}
  }
 });});
 return {vertices,sectors:rooms.map(sector),sides,lines};
}
export function validateProject(p){
 if(!p||p.format!==2)throw Error('Expected Doom project format 2');
 text(p.title,1,80,'title');if(!['doom','doom2'].includes(p.game))throw Error('Choose Doom or Doom II');
 const maps=array(p.maps,36,'maps');if(!maps.length)throw Error('A project needs a map');const names=new Set();
 for(const m of maps){mapData(m);if(names.has(m.name))throw Error('Duplicate map');names.add(m.name);if((p.game==='doom2')!==m.name.startsWith('MAP'))throw Error('Map marker does not match game');}
 array(p.graphics??[],256,'graphics');array(p.lumps??[],256,'lumps');
 for(const l of array(p.assets??[],4096,'imported assets')){named(l.name);if(MAP_DATA.has(l.name)||MAP.test(l.name))throw Error('Imported assets cannot replace map geometry');decode64(l.data);}
 if(JSON.stringify(p).length>4*1024*1024)throw Error('Editable project exceeds 4 MB');
 for(const g of p.graphics??[])validateGraphic(g);
 const assetNames=new Set();for(const g of p.graphics??[])for(const n of g.names??[g.name]){if(assetNames.has(n))throw Error('Duplicate custom graphic');assetNames.add(n);}
 for(const l of p.lumps??[]){named(l.name);if(RESERVED.has(l.name)||MAP.test(l.name))throw Error('Use geometry or graphic tools for structural lumps');const data=decode64(l.data);if(l.name==='PLAYPAL'&&(data.length<768||data.length%768))throw Error('PLAYPAL needs complete 256-color palettes');if(l.name==='COLORMAP'&&data.length<8192)throw Error('COLORMAP is too short');}
 return structuredClone({...p,graphics:p.graphics??[],lumps:p.lumps??[]});
}
export function projectFromLevel(l){
 const value=validateLevel(l),lumps=wadLumps(compileWad(value));return {format:2,title:value.title,game:'doom',maps:[decodeMap(lumps,'E1M1')],graphics:[],lumps:[]};
}
export function asProject(value){return value.format===2?validateProject(value):projectFromLevel(value);}
export function projectFromWad(data,title='Remix',{base}={}){
 const lumps=wadLumps(data),names=lumps.filter(l=>MAP.test(l.name)).map(l=>l.name);
 if(!names.length||lumps.some(l=>['BEHAVIOR','TEXTMAP','ENDMAP'].includes(l.name)))throw Error('Remix supports classic Doom maps.');
 const shared=new Map(base?wadLumps(base).map(l=>[l.name,l.data]):[]);
 const assets=lumps.filter(l=>!MAP.test(l.name)&&!MAP_DATA.has(l.name)).filter(l=>{
  if(/^(S|F)\1?_(START|END)$/.test(l.name))return true;
  const original=shared.get(l.name);return !original||original.length!==l.data.length||!original.every((byte,i)=>byte===l.data[i]);
 });
 return validateProject({format:2,title:title.slice(0,80)||'Remix',game:names[0].startsWith('MAP')?'doom2':'doom',maps:names.map(name=>decodeMap(lumps,name)),graphics:[],lumps:[],
  assets:assets.map(l=>({name:l.name,data:encode64(l.data)}))});
}
export async function importWad(data,title){
 const game=wadLumps(data).some(l=>/^MAP\d\d$/.test(l.name))?'doom2':'doom';
 return projectFromWad(data,title,{base:await baseWad(game)});
}

// Imported art keeps its original bytes. Build one effective asset namespace
// for validation and later edits, with the imported overrides taking priority.
function mergedAssets(base,added){
 const ordinary=new Map(),groups={S:new Map(),F:new Map()};
 for(const list of [base,added]){let group=null;for(const lump of list){const marker=/^(S|F)\1?_((?:START)|(?:END))$/.exec(lump.name);if(marker){group=marker[2]==='START'?marker[1]:null;continue;}if(group)groups[group].set(lump.name,lump);else ordinary.set(lump.name,lump);}}
 return [...ordinary.values(),...Object.entries(groups).flatMap(([name,items])=>items.size?[{name:name+'_START',data:new Uint8Array()},...items.values(),{name:name+'_END',data:new Uint8Array()}]:[])];
}
export function decodeMap(lumps,name){
 const start=lumps.findIndex(l=>l.name===name);if(start<0)throw Error('Map unavailable');
 const get=n=>{const l=lumps.slice(start+1,start+11).find(l=>l.name===n);if(!l)throw Error('Missing '+n);return l.data;};
 const records=(n,size,parse)=>{const d=get(n),v=new DataView(d.buffer,d.byteOffset,d.byteLength);if(d.length%size)throw Error('Invalid '+n);return Array.from({length:d.length/size},(_,i)=>parse(v,d,i*size));};
 return {name,vertices:records('VERTEXES',4,(v,d,a)=>[v.getInt16(a,true),v.getInt16(a+2,true)]),things:records('THINGS',10,(v,d,a)=>({x:v.getInt16(a,true),y:v.getInt16(a+2,true),angle:v.getUint16(a+4,true),type:v.getUint16(a+6,true),flags:v.getUint16(a+8,true)})),
 sectors:records('SECTORS',26,(v,d,a)=>({floor:v.getInt16(a,true),ceiling:v.getInt16(a+2,true),floorTexture:nameAt(d,a+4),ceilingTexture:nameAt(d,a+12),light:v.getUint16(a+20,true),special:v.getUint16(a+22,true),tag:v.getUint16(a+24,true)})),
 sides:records('SIDEDEFS',30,(v,d,a)=>({x:v.getInt16(a,true),y:v.getInt16(a+2,true),upper:nameAt(d,a+4),lower:nameAt(d,a+12),middle:nameAt(d,a+20),sector:v.getUint16(a+28,true)})),
 lines:records('LINEDEFS',14,(v,d,a)=>({a:v.getUint16(a,true),b:v.getUint16(a+2,true),flags:v.getUint16(a+4,true),special:v.getUint16(a+6,true),tag:v.getUint16(a+8,true),right:v.getUint16(a+10,true),left:v.getUint16(a+12,true)===65535?-1:v.getUint16(a+12,true)}))};
}
function mapLumps(map){
 const m=mapData(map),records=(name,values,size,write)=>({name,data:bytes(values.length*size,(v,d)=>values.forEach((x,i)=>write(v,d,i*size,x)))}),ints=(v,at,values)=>values.forEach((n,i)=>v.setInt16(at+i*2,n,true));
 return [{name:m.name,data:new Uint8Array()},records('THINGS',m.things,10,(v,d,a,t)=>ints(v,a,[t.x,t.y,t.angle,t.type,t.flags])),
 records('LINEDEFS',m.lines,14,(v,d,a,l)=>ints(v,a,[l.a,l.b,l.flags,l.special,l.tag,l.right,l.left])),
 records('SIDEDEFS',m.sides,30,(v,d,a,s)=>{ints(v,a,[s.x,s.y]);nameTo(d,a+4,s.upper);nameTo(d,a+12,s.lower);nameTo(d,a+20,s.middle);v.setInt16(a+28,s.sector,true);}),
 records('VERTEXES',m.vertices,4,(v,d,a,p)=>ints(v,a,p)),...['SEGS','SSECTORS','NODES'].map(name=>({name,data:new Uint8Array()})),
 records('SECTORS',m.sectors,26,(v,d,a,s)=>{ints(v,a,[s.floor,s.ceiling]);nameTo(d,a+4,s.floorTexture);nameTo(d,a+12,s.ceilingTexture);ints(v,a+20,[s.light,s.special,s.tag]);}),...['REJECT','BLOCKMAP'].map(name=>({name,data:new Uint8Array()}))];
}
let nodeRun=0;
export async function buildNodes(data){
 const {default:factory}=await import('./vendor/Zdbsp.mjs?run='+nodeRun++),log=[];
 const mod=await factory({locateFile:name=>name.endsWith('.wasm')?new URL('./vendor/zdbsp.wasm',import.meta.url).href:name,print:s=>log.push(String(s)),printErr:s=>log.push(String(s))});
 mod.FS.writeFile('/source.wad',data);const args=['zdbsp','--zero-reject','--no-prune','/source.wad','-o','/built.wad'];
 const pointers=args.map(s=>{const p=mod._malloc(mod.lengthBytesUTF8(s)+1);mod.stringToUTF8(s,p,mod.lengthBytesUTF8(s)+1);return p;}),argv=mod._malloc(args.length*4);pointers.forEach((p,i)=>mod.setValue(argv+i*4,p,'*'));
 try{let code;try{code=await mod.ccall('main','number',['number','number'],[args.length,argv],{async:true});}catch(e){if(e.status!==0)throw e;}if(code&&code!==0)throw Error('ZDBSP failed: '+log.slice(-4).join(' '));const result=new Uint8Array(mod.FS.readFile('/built.wad'));inspectWad(result);return result;}
 finally{pointers.forEach(p=>mod._free(p));mod._free(argv);}
}
const baseCache=new Map();
export async function baseWad(game='doom'){
 if(!baseCache.has(game))baseCache.set(game,(async()=>{const response=await fetch('https://aesthetic.computer/doom-player/vendor/freedoom'+(game==='doom2'?'2':'1')+'.wad',{signal:AbortSignal.timeout(20000)});if(!response.ok)throw Error('Could not load Freedoom assets');return new Uint8Array(await response.arrayBuffer());})().catch(e=>{baseCache.delete(game);throw Object.assign(Error('Could not load Freedoom assets: '+e.message),{name:'DoomAssetError'});}));return baseCache.get(game);
}
function lumpNamed(lumps,n){const l=lumps.find(l=>l.name===n);if(!l)throw Error('Base asset unavailable: '+n);return l.data;}
function rgb(s){if(typeof s!=='string'||!/^#[0-9a-f]{6}$/i.test(s))throw Error('Colors use #RRGGBB');return [parseInt(s.slice(1,3),16),parseInt(s.slice(3,5),16),parseInt(s.slice(5,7),16)];}
function validateGraphic(g){
 if(!['texture','flat','sprite'].includes(g.kind))throw Error('Choose texture, flat or sprite');
 const names=g.names??[g.name];array(names,32,'graphic names');if(!names.length)throw Error('Graphic needs a name');for(const n of names){named(n);if(RESERVED.has(n)||MAP.test(n))throw Error('Reserved graphic name');if(g.kind==='sprite'&&!/^[A-Z0-9_]{4}[A-Z\[\]\\][0-8](?:[A-Z\[\]\\][0-8])?$/.test(n))throw Error('Sprite names use a four-letter family, frame letter and rotation digit');}
 integer(g.width,1,256,'graphic width');integer(g.height,1,254,'graphic height');if(g.kind==='flat'&&(g.width!==64||g.height!==64))throw Error('Classic flats are 64 × 64');
 integer(g.left??Math.floor(g.width/2),-32768,32767,'sprite left');integer(g.top??g.height,-32768,32767,'sprite top');
 if(g.background!=='transparent')rgb(g.background);array(g.layers??[],128,'drawing layers');
 for(const l of g.layers??[]){if(!['rect','ellipse','polygon','line'].includes(l.shape))throw Error('Unknown drawing shape');rgb(l.color);if(l.shape==='polygon'||l.shape==='line'){array(l.points,256,'drawing points').forEach(point);if(l.points.length<(l.shape==='line'?2:3))throw Error('Drawing needs more points');}else for(const k of ['x','y','width','height'])integer(l[k],-32768,32767,'shape '+k);if(l.shape==='line')integer(l.thickness??1,1,256,'line thickness');}
 if(g.pixels!==undefined&&decode64(g.pixels).length!==g.width*g.height)throw Error('Pixel bytes must match dimensions');return g;
}
function bitmap(g,palette){
 validateGraphic(g);const colors=new Map(),nearest=s=>{if(colors.has(s))return colors.get(s);const c=rgb(s);let best=0,min=Infinity;for(let i=0;i<256;i++){const d=c.reduce((n,v,j)=>n+(v-palette[i*3+j])**2,0);if(d<min){min=d;best=i;}}colors.set(s,best);return best;};
 const pixels=g.pixels?Int16Array.from(decode64(g.pixels),n=>g.kind==='sprite'&&n===255?-1:n):new Int16Array(g.width*g.height).fill(g.background==='transparent'?-1:nearest(g.background));
 const inside=(x,y,p)=>{let yes=false;for(let i=0,j=p.length-1;i<p.length;j=i++){const a=p[i],b=p[j];if((a[1]>y)!==(b[1]>y)&&x<(b[0]-a[0])*(y-a[1])/(b[1]-a[1])+a[0])yes=!yes;}return yes;};
 for(const l of g.layers??[]){const color=nearest(l.color);for(let y=0;y<g.height;y++)for(let x=0;x<g.width;x++){let on=false;
  if(l.shape==='rect')on=x>=l.x&&x<l.x+l.width&&y>=l.y&&y<l.y+l.height;
  else if(l.shape==='ellipse')on=l.width>0&&l.height>0&&((x+.5-l.x-l.width/2)/(l.width/2))**2+((y+.5-l.y-l.height/2)/(l.height/2))**2<=1;
  else if(l.shape==='polygon')on=inside(x+.5,y+.5,l.points);
  else for(let i=0;i<l.points.length-1;i++){const a=l.points[i],b=l.points[i+1],dx=b[0]-a[0],dy=b[1]-a[1],t=Math.max(0,Math.min(1,((x-a[0])*dx+(y-a[1])*dy)/(dx*dx+dy*dy||1)));if(Math.hypot(x-a[0]-t*dx,y-a[1]-t*dy)<=(l.thickness??1)/2)on=true;}
  if(on)pixels[y*g.width+x]=color;
 }}return pixels;
}
function patch(g,pixels){
 const columns=[];for(let x=0;x<g.width;x++){const d=[];let y=0;while(y<g.height){if(pixels[y*g.width+x]<0){y++;continue;}const at=y,p=[];while(y<g.height&&pixels[y*g.width+x]>=0)p.push(pixels[y++*g.width+x]);d.push(at,p.length,0,...p,0);}d.push(255);columns.push(Uint8Array.from(d));}
 return bytes(8+g.width*4+columns.reduce((n,d)=>n+d.length,0),(v,d)=>{v.setInt16(0,g.width,true);v.setInt16(2,g.height,true);v.setInt16(4,g.kind==='sprite'?(g.left??Math.floor(g.width/2)):0,true);v.setInt16(6,g.kind==='sprite'?(g.top??g.height):0,true);let at=8+g.width*4;columns.forEach((c,x)=>{v.setInt32(8+x*4,at,true);d.set(c,at);at+=c.length;});});
}
function textures(data){const v=new DataView(data.buffer,data.byteOffset,data.byteLength),count=v.getInt32(0,true);return Array.from({length:count},(_,i)=>{const at=v.getInt32(4+i*4,true),pc=v.getInt16(at+20,true);return {name:nameAt(data,at),width:v.getInt16(at+12,true),height:v.getInt16(at+14,true),patches:Array.from({length:pc},(_,j)=>{const a=at+22+j*10;return [v.getInt16(a,true),v.getInt16(a+2,true),v.getUint16(a+4,true),v.getInt16(a+6,true),v.getInt16(a+8,true)];})};});}
function textureBytes(list){const size=4+list.length*4+list.reduce((n,t)=>n+22+t.patches.length*10,0);return bytes(size,(v,d)=>{v.setInt32(0,list.length,true);let at=4+list.length*4;list.forEach((t,i)=>{v.setInt32(4+i*4,at,true);nameTo(d,at,t.name);v.setInt16(at+12,t.width,true);v.setInt16(at+14,t.height,true);v.setInt16(at+20,t.patches.length,true);t.patches.forEach((p,j)=>p.forEach((n,k)=>v.setInt16(at+22+j*10+k*2,n,true)));at+=22+t.patches.length*10;});});}
function graphicLumps(project,base){
 if(!project.graphics.length)return [];
 const out=[],palette=project.lumps.find(l=>l.name==='PLAYPAL')?decode64(project.lumps.find(l=>l.name==='PLAYPAL').data):lumpNamed(base,'PLAYPAL'),wall=project.graphics.filter(g=>g.kind==='texture'),flats=project.graphics.filter(g=>g.kind==='flat'),sprites=project.graphics.filter(g=>g.kind==='sprite');
 if(wall.length){const names=lumpNamed(base,'PNAMES'),nv=new DataView(names.buffer,names.byteOffset,names.byteLength),pn=Array.from({length:nv.getInt32(0,true)},(_,i)=>nameAt(names,4+i*8)),list=textures(lumpNamed(base,'TEXTURE1'));
  for(const g of wall)for(const name of g.names??[g.name]){const patchName=name;let index=pn.indexOf(patchName);if(index<0){index=pn.length;pn.push(patchName);}const t={name,width:g.width,height:g.height,patches:[[0,0,index,0,0]]},at=list.findIndex(t=>t.name===name);if(at<0)list.push(t);else list[at]=t;out.push({name:patchName,data:patch(g,bitmap(g,palette))});}
  out.push({name:'PNAMES',data:bytes(4+pn.length*8,(v,d)=>{v.setInt32(0,pn.length,true);pn.forEach((n,i)=>nameTo(d,4+i*8,n));})},{name:'TEXTURE1',data:textureBytes(list)});
 }
 if(flats.length){out.push({name:'F_START',data:new Uint8Array()});for(const g of flats)for(const name of g.names??[g.name])out.push({name,data:Uint8Array.from(bitmap(g,palette),n=>n<0?0:n)});out.push({name:'F_END',data:new Uint8Array()});}
 if(sprites.length){const overrides=new Map();for(const g of sprites){const data=patch(g,bitmap(g,palette));for(const name of g.names??[g.name])overrides.set(name,data);}
  // Vanilla indexes one sprite namespace. Preserve all base families, then replace requested frames.
  const start=base.findIndex(l=>l.name==='S_START'),end=base.findIndex(l=>l.name==='S_END');if(start<0||end<start)throw Error('Base sprite namespace unavailable');
  const zeroFrames=new Set([...overrides.keys()].filter(n=>n.length===6&&n[5]==='0').map(n=>n.slice(0,5)));
  out.push({name:'S_START',data:new Uint8Array()});for(const l of base.slice(start+1,end)){if(l.name[5]!=='0'&&(zeroFrames.has(l.name.slice(0,5))||(l.name.length===8&&zeroFrames.has(l.name.slice(0,4)+l.name[6]))))continue;out.push({name:l.name,data:overrides.get(l.name)??l.data});overrides.delete(l.name);}for(const [name,data] of overrides)out.push({name,data});out.push({name:'S_END',data:new Uint8Array()});
 }
 return out;
}
export async function compileProject(input,{base,build=buildNodes}={}){
 const p=validateProject(input),imported=(p.assets??[]).map(l=>({name:l.name,data:decode64(l.data)})),original=wadLumps(base??await baseWad(p.game)),baseLumps=imported.length?mergedAssets(original,imported):original;
 const walls=new Set(textures(lumpNamed(baseLumps,'TEXTURE1')).map(t=>t.name));const t2=baseLumps.find(l=>l.name==='TEXTURE2');if(t2)for(const t of textures(t2.data))walls.add(t.name);
 const flatStart=baseLumps.findIndex(l=>l.name==='F_START'),flatEnd=baseLumps.findIndex(l=>l.name==='F_END'),flats=new Set(baseLumps.slice(flatStart+1,flatEnd).map(l=>l.name));flats.add('F_SKY1');
 for(const g of p.graphics)for(const name of g.names??[g.name]){if(g.kind==='texture')walls.add(name);if(g.kind==='flat')flats.add(name);}
 for(const m of p.maps){const raw=mapData(m);for(const s of raw.sectors)for(const n of [s.floorTexture,s.ceilingTexture])if(!flats.has(n))throw Error('Unknown flat '+n);for(const s of raw.sides)for(const n of [s.upper,s.lower,s.middle])if(n!=='-'&&!walls.has(n))throw Error('Unknown wall texture '+n);}
 const built=await build(packWad(p.maps.flatMap(mapLumps))),lumps=wadLumps(built);
 // The current engine reads classic nodes, not ZDoom's extended/compressed node formats.
 for(const l of lumps)if(l.name==='NODES'&&(l.data.length%28||['XNOD','ZNOD','XGLN','ZGLN'].includes(decoder.decode(l.data.slice(0,4)))))throw Error('Map exceeds classic Doom BSP limits');
 // Vanilla reads one sprite namespace. Reconstitute shared frames omitted
 // from the saved project and merge later artwork edits into that namespace.
 const art=graphicLumps(p,baseLumps);
 if(imported.some(l=>/^SS?_START$/.test(l.name))&&!art.some(l=>l.name==='S_START')){
  art.push(...baseLumps.slice(baseLumps.findIndex(l=>l.name==='S_START'),baseLumps.findIndex(l=>l.name==='S_END')+1));
 }
 return packWad([...lumps,...mergedAssets(imported,art),...p.lumps.map(l=>({name:l.name,data:decode64(l.data)}))]);
}
export async function compileDocument(value,options){return value.format===2?compileProject(value,options):compileWad(validateLevel(value));}
export function documentSummary(value){if(value.format!==2)return JSON.stringify(value);const p=validateProject(value);return JSON.stringify({...p,assets:(p.assets??[]).map(l=>({name:l.name,bytes:l.data.length*3/4})),maps:p.maps.map(m=>({name:m.name,rooms:m.rooms,vertices:m.vertices?.length,sectors:m.sectors?.length,lines:m.lines?.length,things:m.things})),graphics:p.graphics.map(g=>({kind:g.kind,names:g.names??[g.name],width:g.width,height:g.height})),lumps:p.lumps.map(l=>({name:l.name,bytes:l.data.length*3/4}))});}
export async function assetCatalog(game,base){const lumps=wadLumps(base??await baseWad(game));return {textures:textures(lumpNamed(lumps,'TEXTURE1')).map(t=>t.name),flats:lumps.slice(lumps.findIndex(l=>l.name==='F_START')+1,lumps.findIndex(l=>l.name==='F_END')).map(l=>l.name),sprites:lumps.slice(lumps.findIndex(l=>l.name==='S_START')+1,lumps.findIndex(l=>l.name==='S_END')).map(l=>l.name),sounds:lumps.filter(l=>l.name.startsWith('DS')).map(l=>l.name),music:lumps.filter(l=>l.name.startsWith('D_')).map(l=>l.name)};}
