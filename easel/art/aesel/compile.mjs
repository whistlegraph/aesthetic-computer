// Deterministic sprite extraction: chroma key, fixed4x sampling, shared ground/right anchors.
import {readFileSync,writeFileSync,mkdirSync} from 'node:fs';
import {fileURLToPath} from 'node:url';
import {decode,encode} from '../../media/picture/png.mjs';
const image=decode(readFileSync(new URL('./sheet-source.png',import.meta.url)));
if(image.width!==1024||image.height!==1024)throw new Error('Expected4×4 sheet at1024px');
const dir=fileURLToPath(new URL('../../desktop/assets/',import.meta.url));mkdirSync(dir,{recursive:true});
const data=Buffer.alloc(256*256*4),frames=[];
const green=(r,g,b)=>g>r+12&&g>b+12;
for(let n=0;n<16;n++){
 const cell=Buffer.alloc(64*64*4);let maxX=-1,maxY=-1,minX=64,minY=64;
 for(let y=0;y<64;y++)for(let x=0;x<64;x++){
  const i=(((n>>2)*256+y*4+2)*1024+(n%4)*256+x*4+2)*4;
  const [r,g,b,a]=image.data.subarray(i,i+4);if(green(r,g,b)||!a)continue;
  const j=(y*64+x)*4;cell.set([r,g,b,255],j);maxX=Math.max(maxX,x);maxY=Math.max(maxY,y);minX=Math.min(minX,x);minY=Math.min(minY,y);
 }
 if(maxX<0)throw new Error('Empty sprite '+n);
 const dx=60-maxX,dy=59-maxY,aligned=Buffer.alloc(64*64*4);
 for(let y=0;y<64;y++)for(let x=0;x<64;x++){
  const xx=x+dx,yy=y+dy;if(xx<0||xx>63||yy<0||yy>63)continue;
  aligned.set(cell.subarray((y*64+x)*4,(y*64+x)*4+4),(yy*64+xx)*4);
 }
 for(let y=0;y<64;y++)aligned.copy(data,(((n>>2)*64+y)*256+(n%4)*64)*4,y*64*4,(y+1)*64*4);
 frames.push({index:n,source:{x:(n%4)*256,y:(n>>2)*256,width:256,height:256},offset:{x:dx,y:dy},bounds:{x:minX,y:minY,width:maxX-minX+1,height:maxY-minY+1}});
 if(n===9)writeFileSync(dir+'aesel-icon.png',encode({width:64,height:64,data:aligned}));
}
writeFileSync(dir+'aesel.png',encode({width:256,height:256,data}));
const manifest={name:'Aesel',cellWidth:64,cellHeight:64,columns:4,rows:4,anchor:{x:60,y:59},animations:{idle:{frames:[0,1],durations:[1400,180]},awake:{frames:[2,3],durations:[500,500]},sleeping:{frames:[4,5,6,7],durations:[900,900,900,900]},working:{frames:[8,9,10,11],durations:[180,180,180,180]},running:{frames:[12,13,14,15],durations:[120,120,120,120]}},extraction:frames};
writeFileSync(dir+'aesel.json',JSON.stringify(manifest,null,2)+'\n');
console.log('Compiled16 anchored64px frames.');

const icon=new URL('../../desktop/build/icon.svg',import.meta.url);
writeFileSync(icon,readFileSync(icon,'utf8').replace(/href="data:image\/png;base64,[^"]+"/,`href="data:image/png;base64,${readFileSync(dir+'aesel-icon.png').toString('base64')}"`));
