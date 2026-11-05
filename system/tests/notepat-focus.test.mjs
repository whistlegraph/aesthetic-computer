import test from 'node:test';
import assert from 'node:assert/strict';
import {readFile} from 'node:fs/promises';
import {notepatHasEditor,focusNotepatSurface} from '../public/aesthetic.computer/lib/notepat-focus.mjs';
test('entry reclaims the prompt bridge and reactivation preserves real editors',()=>{
 const doc={hidden:false};let blurred=0,focused=0;
 const bridge={closest(){return this},blur(){blurred++;doc.activeElement=null}};
 const surface={focus(options){assert.equal(options.preventScroll,true);focused++;doc.activeElement=this}};
 doc.activeElement=bridge;assert.equal(notepatHasEditor(bridge,bridge),false);
 assert.equal(focusNotepatSurface(surface,bridge,doc),true);assert.equal(blurred,1);assert.equal(surface.tabIndex,-1);
 doc.activeElement={};assert.equal(focusNotepatSurface(surface,bridge,doc),true);
 const editor={closest(){return this}};doc.activeElement=editor;assert.equal(notepatHasEditor(editor,bridge),true);
 assert.equal(focusNotepatSurface(surface,bridge,doc),false);assert.equal(doc.activeElement,editor);assert.equal(focused,2);
 doc.activeElement={};doc.hidden=true;assert.equal(focusNotepatSurface(surface,bridge,doc),false);
});
test('real keyboard routing plays once with a leftover hidden prompt field and still protects text entry',async()=>{
 const source=(await readFile(new URL('../public/aesthetic.computer/lib/keyboard.mjs',import.meta.url),'utf8'))
  .replace(/import \{ MetaBrowser, Aesthetic, MacOS \} from "\.\/platform.mjs";/,'const MetaBrowser=false,Aesthetic=false,MacOS=false;')
  .replace('./notepat-chords.mjs',new URL('../public/aesthetic.computer/lib/notepat-chords.mjs',import.meta.url).href)
  .replace('./notepat-focus.mjs',new URL('../public/aesthetic.computer/lib/notepat-focus.mjs',import.meta.url).href);
 const saved={window:globalThis.window,document:globalThis.document};
 globalThis.window=new EventTarget();globalThis.document=new EventTarget();
 const bridge={closest(){return this}},sent=[];document.activeElement=bridge;
 try{
  const {Keyboard}=await import(`data:text/javascript;base64,${Buffer.from(source).toString('base64')}`);
  const keyboard=new Keyboard(e=>{if(!notepatHasEditor(document.activeElement,bridge)||e.name.includes(':up:'))sent.push(e);return true},{isNotepat:()=>true});
  keyboard.input=bridge;keyboard.focusHandler=()=>false;
  const key=(type,k,flags={})=>{const e=new Event(type,{cancelable:true});Object.assign(e,{key:k,code:'KeyC',...flags});window.dispatchEvent(e);return e};
  assert.equal(key('keydown','ç',{altKey:true}).defaultPrevented,true);key('keyup','c');
  assert.deepEqual(sent.map(e=>e.name),['keyboard:down:c','keyboard:up:c']);assert.equal(sent[0].notepatMusical,true);
  document.activeElement={closest(){return this}};key('keydown','c');assert.equal(sent.length,2);
 }finally{globalThis.window=saved.window;globalThis.document=saved.document}
});
