import test from 'node:test';import assert from 'node:assert/strict';
import {readFileSync} from 'node:fs';
import {apiLookup,loadMap} from '../src/tools.mjs';
import {apiEntries,API_WORKFLOW} from '../src/api-context.mjs';
const map=loadMap();
test('one typography lookup resolves write overloads, font names and instance-vs-function trap',()=>{
 const text=apiLookup(map,'text fonts');for(const pattern of [/SIXTH argument/,/INSTANCE, not a typeface\(name\) function/,/MatrixChunky8/,/text.width/,/NOT a callable API/])assert.match(text,pattern);
 assert.ok(text.length<5000,'A typography answer remains compact');
 assert.match(apiLookup(map,'typeface'),/^typeface\n/);assert.match(apiLookup(map,'write'),/^write\n/);
 assert.match(apiLookup(map,'how do I measure text with a font'),/text.width/);
});
test('map corrections survive regeneration without duplicate symbols or misleading old write signature',()=>{
 const entries=apiEntries(map);assert.equal(entries.filter(e=>e.path==='write').length,1);assert.equal(entries.filter(e=>e.path==='text.width').length,1);
 assert.match(apiLookup({entries:[]},'fonts'),/microtype/);assert.match(API_WORKFLOW,/ac_api.*ac_examples.*ac_outline\/ac_symbol.*ac_preview/);
});
test('typography guidance remains tied to actual runtime overloads and exported font assets',()=>{
 const disk=readFileSync(new URL('../../system/public/aesthetic.computer/lib/disk.mjs',import.meta.url),'utf8');
 const fonts=readFileSync(new URL('../../system/public/aesthetic.computer/disks/common/fonts.mjs',import.meta.url),'utf8');
 assert.match(disk,/customTypeface = options\?\.typeface/);assert.match(disk,/customTypeface = arguments\[5\]/);assert.match(disk,/\$commonApi\.typeface = tf/);assert.match(disk,/width: \(text, fontName\)/);
 for(const name of ['font_1','microtype','unifont','MatrixChunky8'])assert.match(fonts,new RegExp(`export const ${name} =`));
});
