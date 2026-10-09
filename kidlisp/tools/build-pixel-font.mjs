#!/usr/bin/env node
import {font_1} from '../../system/public/aesthetic.computer/disks/common/fonts.mjs';
import * as graph from '../../system/public/aesthetic.computer/lib/graph.mjs';
import {readFile,writeFile} from 'node:fs/promises';
const rows={};
for(let i=32;i<127;i++) {
 const char=String.fromCharCode(i),path=font_1[char];const buffer={width:6,height:10,pixels:new Uint8ClampedArray(240)};
 if(path){const drawing=JSON.parse(await readFile(new URL(`../../system/public/aesthetic.computer/disks/drawings/font_1/${path}.json`,import.meta.url),'utf8'));graph.setBuffer(buffer);graph.color(255,255,255,255);for(const c of drawing.commands){if(!['line','point'].includes(c.name))throw new Error(`Unknown glyph operation ${c.name}`);graph[c.name](...c.args);}}
 rows[char]=Array.from({length:10},(_,y)=>{let row=0;for(let x=0;x<6;x++)if(buffer.pixels[(y*6+x)*4+3])row|=1<<x;return row;});
}
await writeFile(new URL('../../system/public/aesthetic.computer/lib/pixel-font-data.mjs',import.meta.url),`// Generated from AC font_1 drawings by kidlisp/tools/build-pixel-font.mjs.\nexport const glyphRows=${JSON.stringify(rows)};\n`);
