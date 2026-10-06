// Room grammar shared by the maker and its service. No executable user source.
export const ROOM_LIMITS=Object.freeze({objects:64,coordinate:64,size:64,history:128});
const keys=(value,allowed)=>value&&typeof value==='object'&&!Array.isArray(value)&&Object.keys(value).every(k=>allowed.includes(k));
const vector=(v,min,max,integer=false)=>Array.isArray(v)&&v.length===3&&Array.from(v).every(n=>typeof n==='number'&&Number.isFinite(n)&&n>=min&&n<=max&&(!integer||Number.isInteger(n)));
export function validateRoom(value){
  if(!keys(value,['schemaVersion','spawn','objects'])||value.schemaVersion!==1||!vector(value.spawn,-64,64)||!Array.isArray(value.objects)||!value.objects.length||value.objects.length>ROOM_LIMITS.objects)throw Error('Invalid room: use a spawn and 1–64 objects');
  const ids=new Set();
  for(const item of value.objects){
    if(!keys(item,['id','kind','position','size','color','yaw','bounce'])||typeof item.id!=='string'||!/^[a-z][a-z0-9-]{0,39}$/.test(item.id)||ids.has(item.id)||!['platform','goal'].includes(item.kind)||!vector(item.position,-64,64)||!vector(item.size,0.5,64)||!vector(item.color,0,255,true)||!Number.isFinite(item.yaw)||Math.abs(item.yaw)>180||!Number.isFinite(item.bounce)||item.bounce<0||item.bounce>100)throw Error('Invalid room object');
    ids.add(item.id);
  }
  return JSON.parse(JSON.stringify(value));
}
export function starterRoom(){return {schemaVersion:1,spawn:[0,4,20],objects:[
  {id:'start',kind:'platform',position:[0,0,20],size:[16,1,12],color:[103,164,187],yaw:0,bounce:0},
  {id:'bridge',kind:'platform',position:[0,0,0],size:[6,1,28],color:[180,72,135],yaw:0,bounce:0},
  {id:'finish',kind:'platform',position:[0,0,-20],size:[16,1,12],color:[103,164,187],yaw:0,bounce:0},
  {id:'goal',kind:'goal',position:[0,1,-20],size:[4,1,4],color:[229,207,86],yaw:0,bounce:0},
]};}
export function changeObject(room,id,patch){
  if(!room.objects.some(o=>o.id===id))throw Error('Select an object first');
  return validateRoom({...room,objects:room.objects.map(o=>o.id===id?{...o,...patch}:o)});
}
export function drawPath(room,points){
  if(!Array.isArray(points)||points.length<2||points.length>128||points.some(p=>!Array.isArray(p)||p.length!==2||p.some(v=>!Number.isFinite(v)||Math.abs(v)>48)))throw Error('Draw inside the room');
  const objects=[...room.objects];let next=1;
  const used=new Set(objects.map(o=>o.id));
  let last=points[0];
  for(const point of points.slice(1)){
    const dx=point[0]-last[0],dz=point[1]-last[1],length=Math.hypot(dx,dz);
    if(length<2)continue;
    if(length>60)throw Error('Draw shorter path segments');
    while(used.has('path-'+next))next++;
    const id='path-'+next;used.add(id);
    objects.push({id,kind:'platform',position:[(last[0]+point[0])/2,0,(last[1]+point[1])/2],size:[4,1,length+1],color:[180,72,135],yaw:Math.atan2(dx,dz)*180/Math.PI,bounce:0});
    last=point;
  }
  if(objects.length===room.objects.length)throw Error('Draw a longer path');
  return validateRoom({...room,objects});
}
export function localRoomEdit(room,text,selected='bridge'){
  const t=String(text).trim().toLowerCase().replace(/[.!]+$/,'');
  const object=room.objects.find(o=>o.id===selected);
  if(!object)return null;
  if(/^(?:make (?:it|this|the bridge) )?(?:bounce|bouncy)$/.test(t))return changeObject(room,selected,{bounce:55});
  if(/^(?:stop bouncing|remove the bounce)$/.test(t))return changeObject(room,selected,{bounce:0});
  if(/^make (?:it|this|the bridge) (?:wider|narrower)$/.test(t))return changeObject(room,selected,{size:[Math.max(.5,Math.min(64,object.size[0]+(t.endsWith('narrower')?-2:2))),...object.size.slice(1)]});
  const colors={pink:[180,72,135],blue:[103,164,187],yellow:[229,207,86],green:[94,176,127],red:[218,83,80],white:[238,236,229]};
  const color=t.match(/^(?:make (?:it|this|the bridge) )?(pink|blue|yellow|green|red|white)$/)?.[1];
  return color?changeObject(room,selected,{color:colors[color]}):null;
}
export const ROOM_SCHEMA={type:'object',additionalProperties:false,required:['schemaVersion','spawn','objects'],properties:{
  schemaVersion:{type:'integer',enum:[1]},spawn:{type:'array',minItems:3,maxItems:3,items:{type:'number',minimum:-64,maximum:64}},
  objects:{type:'array',minItems:1,maxItems:64,items:{type:'object',additionalProperties:false,required:['id','kind','position','size','color','yaw','bounce'],properties:{
    id:{type:'string',pattern:'^[a-z][a-z0-9-]{0,39}$'},kind:{type:'string',enum:['platform','goal']},
    position:{type:'array',minItems:3,maxItems:3,items:{type:'number',minimum:-64,maximum:64}},
    size:{type:'array',minItems:3,maxItems:3,items:{type:'number',minimum:.5,maximum:64}},
    color:{type:'array',minItems:3,maxItems:3,items:{type:'integer',minimum:0,maximum:255}},
    yaw:{type:'number',minimum:-180,maximum:180},bounce:{type:'number',minimum:0,maximum:100},
  }}},
}};
