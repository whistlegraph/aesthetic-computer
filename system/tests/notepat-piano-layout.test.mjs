import test from 'node:test';
import assert from 'node:assert/strict';
import { notepatPianoLayout,pianoNoteAt } from '../public/aesthetic.computer/lib/notepat-piano-layout.mjs';
import { learningPadLabels } from '../public/aesthetic.computer/lib/notepat-pad-labels.mjs';

test('piano has two complete octaves, fits both orientations, and leaves white-key letters below the black keys',()=>{
  for(const [width,height] of [[96,48],[187,310],[195,400],[320,180],[472,190],[960,700]]) {
    const layout=notepatPianoLayout({width,height});
    assert.equal(layout.keys.length,24);assert.equal(new Set(layout.keys.map(k=>k.note)).size,24);
    assert.equal(layout.stacked,width<=height);
    for(const key of layout.keys) {
      assert.ok(key.x>=0&&key.x+key.w<=width,`${width}: ${key.note} width`);
      assert.ok(key.y>=21&&key.y+key.h<=height,`${height}: ${key.note} height`);
      const label=learningPadLabels(key.label,'W',key.note,{width:8,height:12},layout.fontScale).primary;
      if(label) {
        assert.ok(label.x>=key.x && label.x+label.w<=key.x+key.w);
        assert.ok(label.y>=key.y && label.y+label.h<=key.y+key.h);
        assert.ok(Number.isInteger(label.size)&&Number.isInteger(label.x)&&Number.isInteger(label.y));
      }
    }
    assert.equal(pianoNoteAt(layout,0,0),null);
    for(const key of layout.keys.filter(k=>k.sharp)) {
      const x=key.x+key.w/2,y=key.y+key.h/2;
      assert.equal(pianoNoteAt(layout,x,y),key.note,'black key wins overlap');
      assert.ok(layout.keys.some(k=>!k.sharp&&x>=k.x&&x<k.x+k.w&&y>=k.y&&y<k.y+k.h));
    }
    for(const key of layout.keys.filter(k=>!k.sharp)) assert.equal(pianoNoteAt(layout,key.x+key.w/2,key.y+key.h-1),key.note);
  }
});
