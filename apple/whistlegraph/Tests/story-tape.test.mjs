import test from 'node:test';
import assert from 'node:assert/strict';
import {readFile} from 'node:fs/promises';
import vm from 'node:vm';
const source = await readFile(new URL('../Resources/Web/story-tape.js', import.meta.url), 'utf8');
function fixture(font = Promise.resolve()) {
  const messages = [], recorders = [];
  const context = {fillRect(){}, drawImage(){}, fillText(){}, measureText:s=>({width:s.length * 20})};
  const layer = {width:320};
  const window = {top:{}, whistlegraphStoryFont:font, webkit:{messageHandlers:{whistlegraph:{postMessage:m=>messages.push(m)}}}};
  vm.runInNewContext(source, {
    window, location:{origin:'https://aesthetic.computer'}, Uint8Array,
    document:{createElement:()=>({getContext:()=>context}),querySelector:()=>layer,querySelectorAll:()=>[]},
    getComputedStyle:()=>({display:'block'}),setInterval:()=>1,clearInterval(){},btoa:s=>Buffer.from(s,'binary').toString('base64'),
    createCanvasTapeRecorder:()=>{
      const recorder={state:'inactive',start(){this.state='recording';},pause(){this.state='paused';},resume(){this.state='recording';},stop(){this.state='inactive';this.onstop?.();}};
      recorders.push(recorder);return recorder;
    }
  });
  return {tape:window.whistlegraphStoryTape,recorders,messages};
}
const drain = () => new Promise(resolve=>setImmediate(resolve));
test('pause/resume omits pauses from the recorded card and completion keeps its identity',async()=>{
  const {tape,recorders,messages}=fixture();await tape.start('one');
  tape.pause();assert.equal(recorders[0].state,'paused');
  tape.resume();assert.equal(recorders[0].state,'recording');
  tape.stop();await drain();
  assert.equal(messages.at(-1).kind,'done');assert.equal(messages.at(-1).session,'one');
  await tape.start('two');assert.equal(recorders.length,2);tape.cancel();
});
test('cancel during font loading cannot start a late encoder',async()=>{
  let release;const font=new Promise(resolve=>release=resolve);
  const {tape,recorders}=fixture(font);const starting=tape.start('old');
  tape.cancel();release();await starting;assert.equal(recorders.length,0);
  await tape.start('new');assert.equal(recorders.length,1);tape.cancel();
});
test('a cancelled chunk cannot leak into the next card or finish it',async()=>{
  let release;const bytes=new Promise(resolve=>release=resolve);
  const {tape,recorders,messages}=fixture();await tape.start('old');
  recorders[0].ondataavailable({data:{size:1,arrayBuffer:()=>bytes}});
  await drain();tape.stop();tape.cancel();await tape.start('new');
  release(new Uint8Array([1]).buffer);await drain();assert.equal(messages.length,0);
  tape.stop();await drain();assert.deepEqual(messages.map(m=>[m.session,m.kind]),[['new','done']]);
});
