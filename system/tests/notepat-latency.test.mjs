import test from 'node:test';
import assert from 'node:assert/strict';
import { fastNotepatKey, createNotepatLatency, summarizeLatency } from '../public/aesthetic.computer/lib/notepat-latency.mjs';
import { notepatModeBar } from '../public/aesthetic.computer/lib/notepat-mode-bar.mjs';
import { classicNotepatLayout, notepatGridHeight } from '../public/aesthetic.computer/lib/notepat-layout.mjs';

test('only musical keys bypass frames, preserving pending shortcut order and cold audio', () => {
  for (const key of ['c', 'z', '9', ';', "'", ']', 'shift']) for (const action of ['down', 'up']) {
    const e = { name: `keyboard:${action}:${key}` };
    assert.equal(fastNotepatKey(e, { ready: true }), true);
    for (const state of [{ready:false}, {ready:true,queued:1}, {ready:true,mode:'batched'}]) assert.equal(fastNotepatKey(e,state),false);
  }
  for (const key of ['escape', 'home', 'tab', '`', 'quick-tape']) assert.equal(fastNotepatKey({name:`keyboard:down:${key}`},{ready:true}),false);
  for (const modifier of ['meta','ctrl','alt']) assert.equal(fastNotepatKey({name:'keyboard:down:c',[modifier]:true},{ready:true}),false);
});
test('probe bounds capture, counts missing notes and deduplicates composite voices', () => {
  const p = createNotepatLatency({ limit: 2, now: () => 100 });
  const a = p.begin(), b = p.begin();
  assert.equal(p.begin(), null);
  p.record({ ...a, pieceAt: 102, bridgeAt: 103, estimatedOutputAt: 110 });
  p.record({ ...a, pieceAt: 900, bridgeAt: 1000 });
  assert.equal(p.report().missing, 1);
  assert.equal(p.report().samples.length, 1);
  assert.equal(p.report().summary.inputToBridgeMs.p50, 3);
  assert.equal(p.report().summary.inputToEstimatedOutputMs.p95, 10);
  assert.deepEqual(Object.keys(b).sort(), ['id','inputAt','mode']);
});
test('unavailable output timestamps are not presented as zero latency', () => {
  const summary = summarizeLatency([{ inputAt: 100, pieceAt: 102, bridgeAt: 105, estimatedOutputAt: null }]);
  assert.equal(summary.inputToEstimatedOutputMs.count,0);
  assert.equal(summary.inputToEstimatedOutputMs.p50,null);
});
test('pads and one-row controls stay inside short, tall and narrow viewports', () => {
  for (let width = 96; width <= 960; width += 13) for (let height = 48; height <= 700; height += 17) {
    const l = classicNotepatLayout({ width,height }, { top:21 });
    const bottom = l.topButtonY + l.totalRows*l.buttonHeight + (l.splitLayout ? 0 : l.octaveGap);
    assert.ok(bottom <= height-2, `${width}x${height}: ${bottom}`);
    assert.ok(l.leftOctaveX >= 0);
    assert.ok((l.splitLayout ? l.rightOctaveX : l.leftOctaveX) + 4*l.buttonWidth <= width);
  }
  for (const width of [96,120,195,320,342,480,960]) for (const mode of ['main','fx','tempo']) {
    const bar=notepatModeBar(width,{mode,sound:'composite',bpm:300});
    for(const c of bar.controls) { assert.ok(c.x>=0); assert.ok(c.x+c.w<=width,`${width}:${mode}:${c.id}`); assert.equal(c.y,2); assert.equal(c.h,17); }
  }
});

test('keyboard dispatch delivers a rapid chord exactly once, with frame fallback and focus suppression', async () => {
  const { readFile } = await import('node:fs/promises');
  const source = (await readFile(new URL('../public/aesthetic.computer/lib/keyboard.mjs', import.meta.url),'utf8'))
    .replace(/import \{ MetaBrowser, Aesthetic, MacOS \} from "\.\/platform.mjs";/,'const MetaBrowser=false, Aesthetic=false, MacOS=false;');
  const saved = { window:globalThis.window, document:globalThis.document };
  globalThis.window = new EventTarget(); globalThis.document = new EventTarget();
  document.activeElement = {}; document.hidden = false;
  const sent=[];
  try {
    const { Keyboard } = await import(`data:text/javascript;base64,${Buffer.from(source).toString('base64')}`);
    let enabled=true, focused=false;
    const keyboard=new Keyboard(e=>{if(!enabled)return false;sent.push(e);return true;});
    keyboard.focusHandler=()=>focused;
    // Event.target is read-only; dispatch onto window, with key/code data only.
    const key=(type,k)=>{const e=new Event(type,{cancelable:true});Object.assign(e,{key:k,code:`Key${k.toUpperCase()}`});window.dispatchEvent(e);};
    key('keydown','c');key('keydown','e');key('keyup','c');key('keyup','e');
    assert.deepEqual(sent.map(e=>e.name),['keyboard:down:c','keyboard:down:e','keyboard:up:c','keyboard:up:e']);
    assert.equal(keyboard.events.length,0);
    enabled=false;key('keydown','g');key('keyup','g');assert.equal(keyboard.events.length,2);
    focused=true;key('keydown','a');assert.equal(keyboard.events.length,2);
  } finally { globalThis.window=saved.window;globalThis.document=saved.document; }
});

test('cached pad bitmap includes the octave gap and the entire bottom row', () => {
  for (const screen of [{width:342,height:372},{width:195,height:379},{width:480,height:120}]) {
    const l=classicNotepatLayout(screen,{top:21});
    const lastRowY=l.splitLayout ? 2*l.buttonHeight : 5*l.buttonHeight+l.octaveGap;
    assert.equal(notepatGridHeight(l),lastRowY+l.buttonHeight);
  }
});

test('learning labels prioritize keyboard keys and keep note names separate', async () => {
  const { learningPadLabels }=await import('../public/aesthetic.computer/lib/notepat-pad-labels.mjs');
  for(const w of [12,18,24,45,80,120]) for(const h of [12,18,24,45,80,120]) {
    const box={x:7,y:11,w,h}, {primary,secondary}=learningPadLabels(box,'h','+C#');
    assert.equal(primary.text,'H');
    for(const label of [primary,secondary].filter(Boolean)) {
      assert.ok(label.x>=box.x && label.y>=box.y);
      assert.ok(label.x+label.w<=box.x+w && label.y+label.h<=box.y+h);
    }
    if(secondary) assert.ok(primary.y+primary.h<=secondary.y);
    if(w>=45 && h>=45) assert.ok(primary.size>=3);
  }
});
