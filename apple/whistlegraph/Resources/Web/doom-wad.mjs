// Classic Doom arena compiler. One convex sector needs no BSP nodes.
export const WAD_MIME = 'application/x-doom-wad';
export const WALLS = ['STARTAN3','STONE2','BROWN1','COMPTALL','TEKWALL1'];
export const FLOORS = ['FLOOR0_1','FLOOR4_8','FLAT5_4','CEIL3_5'];
export const THINGS = [3004,3001,9,2001,2002,2003,2007,2008,2011,2012,2018,2019,2022];
export function starterLevel() {
  return {version:1,title:'Untitled',width:768,depth:768,height:128,light:192,wall:'STARTAN3',floor:'FLOOR0_1',ceiling:'CEIL3_5',things:[]};
}
// Only replace the untouched seed from the first release. Edited histories stay intact.
export function emptySeedHistory(history) {
  if(history?.head!==0||history.versions?.length!==1||history.versions[0].request!==null)return null;
  const legacy={...starterLevel(),title:'First arena',things:[{x:-192,y:128,type:3004,angle:270},{x:192,y:128,type:3001,angle:270},{x:128,y:-224,type:2001,angle:0},{x:-128,y:-224,type:2008,angle:0},{x:0,y:256,type:2012,angle:0}]};
  if(history.versions[0].source!==JSON.stringify(legacy))return null;
  return {...history,versions:[{...history.versions[0],source:JSON.stringify(starterLevel())}]};
}
export const LEVEL_SCHEMA = {type:'object',additionalProperties:false,required:['version','title','width','depth','height','light','wall','floor','ceiling','things'],properties:{version:{type:'integer',enum:[1]},title:{type:'string',maxLength:80},width:{type:'integer',minimum:384,maximum:2048},depth:{type:'integer',minimum:384,maximum:2048},height:{type:'integer',minimum:64,maximum:512},light:{type:'integer',minimum:32,maximum:255},wall:{type:'string',enum:WALLS},floor:{type:'string',enum:FLOORS},ceiling:{type:'string',enum:FLOORS},things:{type:'array',maxItems:64,items:{type:'object',additionalProperties:false,required:['x','y','type','angle'],properties:{x:{type:'integer'},y:{type:'integer'},type:{type:'integer',enum:THINGS},angle:{type:'integer',minimum:0,maximum:359}}}}}};
function integer(value,min,max,label){if(!Number.isInteger(value)||value<min||value>max)throw Error(`Invalid ${label}`);return value;}
export function validateLevel(input){
  if(!input||typeof input!=='object'||Array.isArray(input)||Object.keys(input).some(k=>!Object.hasOwn(LEVEL_SCHEMA.properties,k)))throw Error('Invalid Doom level');
  if(input.version!==1||typeof input.title!=='string'||!input.title.trim()||input.title.length>80)throw Error('Invalid Doom title or version');
  const level={version:1,title:input.title.trim(),width:integer(input.width,384,2048,'width'),depth:integer(input.depth,384,2048,'depth'),height:integer(input.height,64,512,'height'),light:integer(input.light,32,255,'light')};
  if(level.width%2||level.depth%2)throw Error('Arena dimensions must be even');
  for(const [key,choices] of [['wall',WALLS],['floor',FLOORS],['ceiling',FLOORS]]){if(!choices.includes(input[key]))throw Error(`Invalid ${key} texture`);level[key]=input[key];}
  if(!Array.isArray(input.things)||input.things.length>64)throw Error('Too many Doom objects');
  const spawn=[0,-level.depth/2+96];
  level.things=input.things.map(t=>{
    if(!t||Object.keys(t).some(k=>!['x','y','type','angle'].includes(k))||!THINGS.includes(t.type))throw Error('Invalid Doom object');
    const x=integer(t.x,-level.width/2+48,level.width/2-48,'object x'),y=integer(t.y,-level.depth/2+48,level.depth/2-48,'object y');
    if(Math.hypot(x-spawn[0],y-spawn[1])<64)throw Error('Keep the player start clear');
    return {x,y,type:t.type,angle:integer(t.angle,0,359,'object angle')};
  });
  return level;
}
function bytes(size,write){const data=new Uint8Array(size);write?.(new DataView(data.buffer),data);return data;}
function name(data,offset,value){data.set(new TextEncoder().encode(value),offset);}
function shorts(values){return bytes(values.length*2,v=>values.forEach((n,i)=>v.setUint16(i*2,n&65535,true)));}
export function compileWad(input){
  const l=validateLevel(input),w=l.width/2,h=l.depth/2;
  const vertices=[[-w,-h],[-w,h],[w,h],[w,-h]];
  const things=[{x:0,y:-h+96,type:1,angle:90},...l.things];
  const lumps=[['E1M1',new Uint8Array()],['THINGS',shorts(things.flatMap(t=>[t.x,t.y,t.angle,t.type,7]))],
    ['LINEDEFS',shorts(vertices.flatMap((_,i)=>[i,(i+1)%4,1,i===3?11:0,0,i,65535]))],
    ['SIDEDEFS',bytes(4*30,(v,d)=>{for(let i=0;i<4;i++){name(d,i*30+4,'-');name(d,i*30+12,'-');name(d,i*30+20,l.wall);v.setUint16(i*30+28,0,true);}})],
    ['VERTEXES',shorts(vertices.flat())],['SEGS',shorts([16384,0,49152,32768].flatMap((angle,i)=>[i,(i+1)%4,angle,i,0,0]))],
    ['SSECTORS',shorts([4,0])],['NODES',new Uint8Array()],
    ['SECTORS',bytes(26,(v,d)=>{v.setInt16(2,l.height,true);name(d,4,l.floor);name(d,12,l.ceiling);v.setInt16(20,l.light,true);})],['REJECT',new Uint8Array(1)]];
  const cols=Math.ceil(l.width/128)+1,rows=Math.ceil(l.depth/128)+1,cells=cols*rows;
  lumps.push(['BLOCKMAP',shorts([-w,-h,cols,rows,...Array(cells).fill(4+cells),0,0,1,2,3,65535])]);
  const directory=12+lumps.reduce((n,[,data])=>n+data.length,0);
  return bytes(directory+lumps.length*16,(v,d)=>{name(d,0,'PWAD');v.setInt32(4,lumps.length,true);v.setInt32(8,directory,true);let offset=12;lumps.forEach(([label,data],i)=>{d.set(data,offset);v.setInt32(directory+i*16,offset,true);v.setInt32(directory+i*16+4,data.length,true);name(d,directory+i*16+8,label);offset+=data.length;});});
}
export function inspectWad(data){
  if(!(data instanceof Uint8Array)||data.length<12||data.length>32*1024*1024)throw Error('Invalid WAD size');
  const v=new DataView(data.buffer,data.byteOffset,data.byteLength),text=(a,b)=>new TextDecoder().decode(data.subarray(a,b)).replace(/\0.*$/s,'');
  const kind=text(0,4),count=v.getInt32(4,true),dir=v.getInt32(8,true);
  if(!['IWAD','PWAD'].includes(kind)||count<1||count>10000||dir<12||dir+count*16>data.length)throw Error('Invalid WAD directory');
  const lumps=Array.from({length:count},(_,i)=>{const at=dir+i*16,offset=v.getInt32(at,true),size=v.getInt32(at+4,true);if(offset<0||size<0||offset+size>data.length)throw Error('WAD lump outside file');return {name:text(at+8,at+16),offset,size};});
  return {kind,lumps};
}
