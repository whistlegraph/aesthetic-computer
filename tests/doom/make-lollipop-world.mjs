// Regenerate the candy test WAD from its editable level JSON and procedural artwork.
import {readFile,writeFile} from 'node:fs/promises';
import {fileURLToPath} from 'node:url';
import {compileWad,inspectWad} from '../../system/public/doom-player/wad.mjs';
const root=fileURLToPath(new URL('../../system/public/doom-player/',import.meta.url));
const base=new Uint8Array(await readFile(root+'/vendor/freedoom1.wad'));
const info=inspectWad(base),get=name=>{const l=info.lumps.find(l=>l.name===name);return base.slice(l.offset,l.offset+l.size);};
const palette=get('PLAYPAL');
function color(r,g,b){let best=0,dist=Infinity;for(let i=0;i<256;i++){const d=(r-palette[i*3])**2+(g-palette[i*3+1])**2+(b-palette[i*3+2])**2;if(d<dist){dist=d;best=i;}}return best;}
const pink=color(255,100,185),cream=color(255,245,215),blue=color(110,200,255),dark=color(155,45,110);
function patch(w,h,pixel,left=0,top=0){
 const columns=[];for(let x=0;x<w;x++){const bytes=[];let y=0;while(y<h){if(pixel(x,y)===null){y++;continue;}const start=y,values=[];while(y<h&&pixel(x,y)!==null)values.push(pixel(x,y++));bytes.push(start,values.length,0,...values,0);}bytes.push(255);columns.push(Uint8Array.from(bytes));}
 const data=new Uint8Array(8+w*4+columns.reduce((n,c)=>n+c.length,0)),v=new DataView(data.buffer);v.setInt16(0,w,true);v.setInt16(2,h,true);v.setInt16(4,left,true);v.setInt16(6,top,true);let offset=8+w*4;columns.forEach((c,x)=>{v.setInt32(8+x*4,offset,true);data.set(c,offset);offset+=c.length;});return data;
}
const level=JSON.parse(await readFile(root+'/examples/lollipop-world.json','utf8'));
const map=compileWad(level),lumps=inspectWad(map).lumps.map(l=>[l.name,map.slice(l.offset,l.offset+l.size)]);
// Bonus health stays collectible at full starting health. Override its complete sprite family below.
const things=lumps.find(([name])=>name==='THINGS')[1],thingsView=new DataView(things.buffer);
for(let at=10;at<things.length;at+=10)thingsView.setInt16(at+6,2014,true);
const names=get('PNAMES'),count=new DataView(names.buffer).getInt32(0,true),pnames=new Uint8Array(names.length+8);pnames.set(names);new DataView(pnames.buffer).setInt32(0,count+1,true);pnames.set(new TextEncoder().encode('LOLLIWAL'),names.length);
const textures=get('TEXTURE1'),tv=new DataView(textures.buffer),num=tv.getInt32(0,true);let entry=-1;
for(let i=0;i<num;i++){const at=tv.getInt32(4+i*4,true),name=new TextDecoder().decode(textures.slice(at,at+8)).replace(/\0.*$/s,'');if(name==='STARTAN3')entry=4+i*4;}
if(entry<0)throw Error('Base wall texture absent');
const texture=new Uint8Array(textures.length+32);texture.set(textures);const v=new DataView(texture.buffer),at=textures.length;v.setInt32(entry,at,true);texture.set(new TextEncoder().encode('STARTAN3'),at);v.setInt16(at+12,128,true);v.setInt16(at+14,128,true);v.setInt16(at+20,1,true);v.setInt16(at+26,count,true);
const wall=patch(128,128,(x,y)=>{
 if(y>100)return Math.floor((x+y)/14)%2?pink:cream;
 const dx=(x%64)-32,dy=y-40,r=Math.hypot(dx,dy);
 if(r<25)return Math.floor(r*.6+Math.atan2(dy,dx)*2)%2===0?pink:cream;
 if(Math.abs(dx)<3&&y>40&&y<100)return cream;
 return dark;
});
const candy=patch(48,80,(x,y)=>{const dx=x-24,dy=y-23,r=Math.hypot(dx,dy);if(r<22)return Math.floor(r*.65+Math.atan2(dy,dx)*2)%2===0?pink:cream;if(Math.abs(dx)<3&&y>=23&&y<78)return cream;return null;},24,80);
const flat=(a,b)=>Uint8Array.from({length:4096},(_,i)=>(Math.floor((i%64)/16)+Math.floor(Math.floor(i/64)/16))%2?a:b);
lumps.push(['PNAMES',pnames],['TEXTURE1',texture],['LOLLIWAL',wall],['F_START',new Uint8Array()],['FLOOR0_1',flat(pink,cream)],['CEIL3_5',flat(blue,cream)],['F_END',new Uint8Array()],['S_START',new Uint8Array()],...info.lumps.slice(info.lumps.findIndex(l=>l.name==='S_START')+1,info.lumps.findIndex(l=>l.name==='S_END')).map(l=>[l.name,/^BON1[A-Z]0$/.test(l.name)?candy:base.slice(l.offset,l.offset+l.size)]),['S_END',new Uint8Array()]);
const dir=12+lumps.reduce((n,[,d])=>n+d.length,0),out=new Uint8Array(dir+lumps.length*16),dv=new DataView(out.buffer);out.set(new TextEncoder().encode('PWAD'));dv.setInt32(4,lumps.length,true);dv.setInt32(8,dir,true);let offset=12;lumps.forEach(([name,data],i)=>{out.set(data,offset);dv.setInt32(dir+i*16,offset,true);dv.setInt32(dir+i*16+4,data.length,true);out.set(new TextEncoder().encode(name),dir+i*16+8);offset+=data.length;});
inspectWad(out);await writeFile(root+'/examples/lollipop-world.wad',out);console.log(out.length+' bytes, '+level.things.length+' candy pickups');
