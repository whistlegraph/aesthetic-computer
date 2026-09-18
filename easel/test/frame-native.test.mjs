import test from 'node:test';
import assert from 'node:assert/strict';
import {readFile,access,mkdtemp,symlink,rm} from 'node:fs/promises';
import {execFile} from 'node:child_process';
import {promisify} from 'node:util';
import {tmpdir} from 'node:os';
import {join} from 'node:path';
import {fileURLToPath} from 'node:url';
import {CAPTURE_SCRIPT} from '../src/frame-capture-script.mjs';
const exec=promisify(execFile);
const binary=fileURLToPath(new URL('../bin/frame-ocr',import.meta.url));
const fixture=fileURLToPath(new URL('./fixtures/frame-ocr.png',import.meta.url));
// Fixture is a synthetic 500×140 canvas: white background and Helvetica 38px
// black text "Easel frame 123" at canvas baseline (24,75); no user content.
test('native capture expression exactly matches the desktop trusted expression',async()=>{
 const source=await readFile(new URL('../../slab/menubar-swift/Sources/SlabMenubar/PromptPreview.swift',import.meta.url),'utf8');
 const literal=/private static let frameCaptureExpression = #"""\n([\s\S]*?)\n"""#/.exec(source);
 assert.ok(literal);assert.equal(literal[1],CAPTURE_SCRIPT);
});
test('offline Vision OCR finds actual text and returns top-left pixel coordinates',async t=>{
 if(process.platform!=='darwin')return t.skip('Vision requires macOS');
 try{await access(binary);}catch{return t.skip('Build frame-ocr.swift first');}
 const {stdout}=await exec(binary,[fixture],{timeout:15000,maxBuffer:300000});
 const {regions}=JSON.parse(stdout);assert.ok(regions.length<=100);
 const text=regions.find(r=>r.text==='Easel frame 123');assert.ok(text);
 assert.ok(text.confidence>=.5);assert.ok(text.x>=20&&text.x<=32);
 assert.ok(text.y>=40&&text.y<=52);assert.ok(text.width>250&&text.width<300);assert.ok(text.height>25&&text.height<40);
 const temporary=await mkdtemp(join(tmpdir(),'easel-ocr-link-'));
 try{const link=join(temporary,'image.png');await symlink(fixture,link);await assert.rejects(exec(binary,[link],{timeout:5000}));}finally{await rm(temporary,{recursive:true,force:true});}
});
