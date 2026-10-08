import assert from 'node:assert/strict';
import test from 'node:test';
import {spawnSync} from 'node:child_process';
import {contrast, readableRGB, terminalRGB} from '../src/contrast.mjs';

test('fixed-surface colors stay readable after both RGB and 256-color output', () => {
  const inks = [[255,255,255],[200,30,100],[255,100,0],[255,100,255],
    [220,180,255],[170,150,205],[0,255,0],[255,90,90],[120,200,255],
    [42,28,66],[112,72,166],[196,74,22],[132,22,84]];
  for(const truecolor of [true,false]) {
    for(const background of [[28,28,28],truecolor?[248,228,240]:terminalRGB([248,228,240])]) {
      for(const ink of inks) {
        const output=readableRGB(ink,background,{truecolor});
        assert.ok(contrast(output,background)>=4.5, `${output} on ${background}`);
        if(!truecolor)assert.deepEqual(terminalRGB(output),output);
      }
    }
  }
});

test('Slab page colors follow the tab palette across system appearance changes', () => {
  const program = `
    import assert from 'node:assert/strict';
    import {color,renderFrame,setAppearance,setPageBackground,coloredHandle} from './aesel/src/render.mjs';
    import {contrast} from './aesel/src/contrast.mjs';
    for(const role of ['prompt','highlight','handle','soft','muted','status','error','you','run','edit','inbox']) {
      assert.match(color[role],/^\\x1b\\[38;5;([0-9]|1[0-5])m$/);
    }
    const state={profile:{name:'pro'},account:'@jeffrey',input:'',entries:[
      {kind:'assistant',text:'# Heading\\n**Bold** and \\x60inline\\x60 and [link](https://example.com).'}]};
    setAppearance('light');const light=renderFrame(state,80,16,true);
    setAppearance('dark');assert.equal(renderFrame(state,80,16,true),light);
    assert.ok(!light.includes('\\x1b[2m'),'no dim text against a status background');
    const inks=[...coloredHandle('@jeffrey').matchAll(/\\x1b\\[38;5;(\\d+)m/g)].map(m=>Number(m[1]));
    assert.ok(inks.every(n=>n<16));assert.ok(new Set(inks).size>=4);
    let previous=light;
    for(const background of [[23,0,27],[93,16,30],[255,157,184],[230,200,240],[120,120,120]]) {
      setPageBackground(background);
      for(const role of ['text','prompt','highlight','handle','soft','muted','status','error','you','run','edit','inbox']) {
        const match=/^\\x1b\\[38;5;(\\d+)m$/.exec(color[role]);
        assert.ok(match,role);const index=Number(match[1])-16;
        assert.ok(index>=0 && index<216);
        const levels=[0,95,135,175,215,255];
        const rgb=[levels[Math.floor(index/36)],levels[Math.floor(index/6)%6],levels[index%6]];
        const target=Math.min(5,Math.max(contrast([0,0,0],background),contrast([255,255,255],background)));
        assert.ok(contrast(rgb,background)>=target,role+' against '+background);
      }
      const frame=renderFrame(state,80,16,true);
      assert.notEqual(frame,previous,'cached replies repaint after a page-color change');previous=frame;
    }
  `;
  const result=spawnSync(process.execPath,['--input-type=module','--eval',program],{
    cwd:new URL('../..',import.meta.url),encoding:'utf8',env:{...process.env,AESEL_THEME:'slab',COLORTERM:''},
  });
  assert.equal(result.status,0,result.stderr);
});
