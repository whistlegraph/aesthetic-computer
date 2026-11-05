import test from 'node:test';
import assert from 'node:assert/strict';
import { NotepatPainter, notepatPadColors, notepatNoteColor, notepatPalette } from '../public/aesthetic.computer/lib/notepat-render.mjs';
import { learningPadLabels } from '../public/aesthetic.computer/lib/notepat-pad-labels.mjs';
import { notepatModeBar, notepatPianoKeys } from '../public/aesthetic.computer/lib/notepat-mode-bar.mjs';
import { readFrame } from '../public/aesthetic.computer/lib/gpu-frame.mjs';

test('all natural keys share a font scale; sharps use the next crisp size and have no corner label',()=>{
  for(const w of [12,24,45,100]) for(const h of [12,24,45,100]) {
    const box={x:3,y:7,w,h};let size;
    for(const [key,width] of [['I',3],['W',8],['C',7]]) {
      const label=learningPadLabels(box,key,'c',{width,height:12});
      size ??=label.primary.size;
      assert.equal(label.primary.size,size);assert.equal(label.secondary,undefined);
      assert.ok(Math.abs(label.primary.y+label.primary.h/2-(box.y+h/2))<=.5);
      assert.ok(Number.isInteger(label.primary.size)&&Number.isInteger(label.primary.x)&&Number.isInteger(label.primary.y));
      assert.ok(label.primary.x>=box.x && label.primary.x+label.primary.w<=box.x+w);
    }
    assert.equal(learningPadLabels(box,'V','c#').primary.size,Math.max(1,size-1));
  }
});
test('header uses the freed mini-piano space for the visualizer',()=>{
  for(const width of [187,195,240,342,480,960]) for(const sound of ['piano','sine','g128']) {
    const bar=notepatModeBar(width,{sound}),{piano,viz}=bar.header;
    assert.equal(piano,null);
    assert.ok(viz.w>=4 && viz.x+viz.w<bar.controls[0].x);
    for(const c of bar.controls) assert.ok(c.x+c.w<=width);
  }
});
test('CPU fallback and WebGPU consume the same scene, including enlarged glyphs and return from GPU loss',()=>{
  const commands=[];let sent;
  const api={wipe:(...v)=>commands.push(['clear',...v]),ink:(...c)=>({box:(...v)=>commands.push(['box',...v,...c]),line:(...v)=>commands.push(['line',...v,...c])}),gpuFrame:{available:false,probe(){},send(b){sent=b;}}};
  const glyph={width:3,height:2,pixels:new Uint8ClampedArray([255,255,255,255,255,255,255,255,0,0,0,0,0,0,0,0,255,255,255,255,0,0,0,0])};
  const p=new NotepatPainter();
  const draw=()=>p.begin(api,[20,20,25]).ink(200,0,0).box(2,3,40,40).glyph(glyph,5,7,3).line(0,0,10,0).finish();
  assert.equal(draw(),'cpu'); const expected=commands.splice(0);
  api.gpuFrame.available=true; assert.equal(draw(),'webgpu');
  const actual=[];
  readFrame(sent,{clear:(...v)=>actual.push(['clear',...v]),box:(x,y,w,h,fill,...c)=>actual.push(['box',x,y,w,h,...c]),line:(x1,y1,x2,y2,t,...c)=>actual.push(['line',x1,y1,x2,y2,...c])});
  assert.deepEqual(actual,expected);assert.equal(actual.filter(c=>c[0]==='box').length,3);
  api.gpuFrame.available=false;commands.length=0;assert.equal(draw(),'cpu');assert.deepEqual(commands,expected);
  api.gpuFrame.available=true;p.forceCpu=true;assert.equal(draw(),'cpu');
});
test('ACOS palettes distinguish sharp keys, held notes, and light/dark backgrounds',()=>{
  for(const dark of [true,false]) {
    const color=notepatNoteColor('c#'),idle=notepatPadColors(color,true,dark,false),held=notepatPadColors(color,true,dark,true);
    assert.deepEqual(held.fill,color);assert.notDeepEqual(idle.fill,held.fill);
    for(const v of [...idle.fill,...held.fill,...notepatPalette(dark).background]) assert.ok(v>=0&&v<=255);
  }
  assert.notDeepEqual(notepatPalette(true).background,notepatPalette(false).background);
});
