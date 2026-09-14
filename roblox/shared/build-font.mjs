// Convert AC's original font_1 drawing commands into horizontal pixel runs.
import {font_1} from '../../system/public/aesthetic.computer/disks/common/fonts.mjs';
import {readFileSync,writeFileSync} from 'node:fs';
const glyphs={};
for(const [char,file] of Object.entries(font_1)){
  if(char.length!==1 || typeof file!=='string')continue;
  let data;try{data=JSON.parse(readFileSync(new URL('../../system/public/aesthetic.computer/disks/drawings/font_1/'+encodeURI(file)+'.json',import.meta.url)))}catch{continue}
  const pixels=new Set();const point=(x,y)=>{if(x>=0&&x<6&&y>=0&&y<10)pixels.add(`${x},${y}`)};
  for(const c of data.commands){if(c.name==='point')point(...c.args);else if(c.name==='line'){
    let[x,y,x2,y2]=c.args;let dx=Math.abs(x2-x),dy=-Math.abs(y2-y),sx=x<x2?1:-1,sy=y<y2?1:-1,err=dx+dy;
    for(;;){point(x,y);if(x===x2&&y===y2)break;const e=2*err;if(e>=dy){err+=dy;x+=sx}if(e<=dx){err+=dx;y+=sy}}
  }}
  const runs=[];for(let y=0;y<10;y++)for(let x=0;x<6;x++){if(!pixels.has(`${x},${y}`))continue;const start=x;while(x+1<6&&pixels.has(`${x+1},${y}`))x++;runs.push([start,y,x-start+1])}glyphs[char]=runs;
}
glyphs[' ']=[];
writeFileSync(new URL('Glyphs.luau',import.meta.url),'-- Generated from AC font_1; regenerate with node roblox/shared/build-font.mjs.\nreturn {\n'+Object.entries(glyphs).map(([c,r])=>`[${JSON.stringify(c)}] = {${r.map(a=>'{'+a.join(',')+'}').join(',')}},`).join('\n')+'\n}\n');
