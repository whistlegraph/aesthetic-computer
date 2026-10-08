import './own-ink.mjs';
import assert from 'node:assert/strict';
import test from 'node:test';
import {mkdtempSync, readFileSync, writeFileSync, existsSync, rmSync} from 'node:fs';
import {tmpdir} from 'node:os';
import {join} from 'node:path';
import {SlabHandle} from '../src/slab-handle.mjs';
import {renderFrame, cleanText, headerAction} from '../src/render.mjs';

const state = () => ({profile:{name:'pro'}, account:'@jeffrey', input:'draft',
  entries:[{kind:'assistant',text:'Last reply.'}]});

test('one empty line separates the reply and the composer', () => {
  const s = state(), lines = cleanText(renderFrame(s, 80, 16, true)).split('\n');
  const reply = lines.findIndex(line => line.includes('Last reply.'));
  const input = lines.findIndex(line => line.includes('draft'));
  assert.equal(input, reply + 2);
  assert.equal(lines[input - 1].trim(), '');
  assert.equal(s.cursorCell.row, input + 1);
});

test('only a matching native slot conceals the handle, preserving its text and cell width', () => {
  const s = state();
  const fallback = renderFrame(s, 80, 16, true);
  assert.doesNotMatch(fallback, /\x1b\[8m/);
  assert.equal(s.handleGeometry.text, '@jeffrey');
  assert.equal(s.handleGeometry.row, 15);
  s.slabHandle = s.handleGeometry;
  const native = renderFrame(s, 80, 16, true);
  assert.match(native, /\x1b\[8m@jeffrey\x1b\[28m/);
  assert.equal(cleanText(native), cleanText(fallback));
  assert.equal(headerAction(s,80,16,2,16),'profile');
  assert.equal(headerAction(s,80,16,9,16),'profile');
  assert.equal(headerAction(s,80,16,10,16),'');
  assert.doesNotMatch(renderFrame(s, 81, 16, true), /\x1b\[8m/);
  assert.doesNotMatch(renderFrame(s, 80, 16, false), /\x1b\[8m/);
  assert.equal(s.handleGeometry, null);
  for (const mode of ['about', 'settings', 'approval']) {
    const hidden = {...state(), [mode]: mode === 'about' ? true : mode === 'settings' ? {row:0,options:[]} : {}, slabHandle:s.slabHandle};
    assert.doesNotMatch(renderFrame(hidden, 80, 16, true), /\x1b\[8m/);
    assert.equal(hidden.handleGeometry, null);
  }
  const narrow = {...state(), spend:{tokens:999999,usd:0}, recoveryNotice:'Reconnect: waiting for a connection'};
  renderFrame(narrow, 32, 16, true);
  assert.equal(narrow.handleGeometry, null, 'a handle dropped from the footer has no overlay');
});

test('the handoff expires, rejects stale geometry and cleans up after a reload', () => {
  const directory = mkdtempSync(join(tmpdir(), 'aesel-handle-'));
  let now = 10000;
  const handoff = new SlabHandle({directory, sessionId:'session', pid:123, now:()=>now});
  try {
    writeFileSync(handoff.file + '.palette',JSON.stringify({schema:1,sessionId:'session',background:[23,0,27]}));
    handoff.poll(); assert.deepEqual(handoff.background,[23,0,27], 'page colors arrive even without a handle slot');
    writeFileSync(handoff.file + '.palette',JSON.stringify({schema:1,sessionId:'other',background:[255,255,255]}));
    handoff.poll(); assert.deepEqual(handoff.background,[23,0,27]);
    const s = state(); renderFrame(s, 80, 16, true);
    handoff.update(s.handleGeometry);
    const record = JSON.parse(readFileSync(handoff.file, 'utf8'));
    assert.equal(record.pid, 123);
    assert.equal(handoff.readyLayout, null);
    const ack = {schema:1,sessionId:'session',token:record.token,at:now};
    writeFileSync(handoff.ack, JSON.stringify({...ack,token:'wrong'}));
    handoff.poll(); assert.equal(handoff.readyLayout, null);
    writeFileSync(handoff.ack, JSON.stringify(ack));
    handoff.poll(); assert.deepEqual(handoff.readyLayout, s.handleGeometry);
    handoff.setHovered(true);
    const hovering = JSON.parse(readFileSync(handoff.file,'utf8'));
    assert.equal(hovering.hovered,true);
    assert.equal(hovering.token,record.token,'hover preserves the native handoff');
    assert.deepEqual(handoff.readyLayout,s.handleGeometry);
    now += 3000;
    handoff.poll(); assert.equal(handoff.readyLayout, null);
    writeFileSync(handoff.ack, JSON.stringify({...ack,at:now}));
    handoff.poll(); assert.ok(handoff.readyLayout);
    handoff.update({...s.handleGeometry,columns:81});
    handoff.poll(); assert.equal(handoff.readyLayout, null);
    handoff.update(null);
    assert.equal(existsSync(handoff.file), false);
    assert.equal(existsSync(handoff.ack), false);
  } finally { handoff.close(); rmSync(directory,{recursive:true,force:true}); }
});
