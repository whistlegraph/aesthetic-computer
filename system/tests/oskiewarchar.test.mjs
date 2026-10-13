import assert from 'node:assert/strict';
import test from 'node:test';
import * as piece from '../public/aesthetic.computer/disks/oskiewarchar.mjs';
import { defaultCharacter } from '../public/aesthetic.computer/lib/oskiewar-character.mjs';

const pause = ms => new Promise(resolve => setTimeout(resolve, ms));
function canvas(width=390,height=750) {
  const words=[];
  const chain={write:(text,at)=>{words.push({text,...at});return chain;},box:()=>chain,circle:()=>chain};
  return {screen:{width,height},ink:()=>chain,wipe:()=>{},words};
}
const touch=(x,y)=>piece.act({event:{x,y,is:name=>name==='touch'}});

test('piece immediately previews a color, coalesces edits, and saves as the resolved owner', async()=>{
  const calls=[];
  await piece.boot({user:{sub:'email|otp'},colon:['@tester'],help:{repeat(){}},hud:{label(){}},
    net:{userRequest:async(method,url,body)=>{
      calls.push({method,url,body});
      return method==='GET'?{status:200,handle:'tester',revision:0,appearance:null}:
        {status:200,handle:'tester',revision:body.baseRevision+1,appearance:body.appearance};
    }}});
  const screen=canvas();piece.paint(screen);
  assert.ok(screen.words.some(({text})=>text==='@tester'));
  // Color swatches begin below the 150px preview, two tabs, and part selector.
  touch(20,280); touch(65,280);
  await pause(400);
  const saves=calls.filter(call=>call.method==='POST');
  assert.equal(saves.length,1);
  assert.equal(saves[0].body.handle,'tester');
  assert.equal(saves[0].body.baseRevision,0);
  assert.deepEqual(saves[0].body.appearance.colors.head,[255,190,135]);
  piece.leave();
});

test('leaving the editor cancels an unsent edit', async()=>{
  let saves=0;
  await piece.boot({user:{sub:'a'},colon:['tester'],help:{repeat(){}},net:{userRequest:async method=>{
    if(method==='POST')saves++;
    return {status:200,handle:'tester',revision:3,appearance:defaultCharacter()};
  }}});
  piece.paint(canvas());touch(20,280);piece.leave();await pause(360);
  assert.equal(saves,0);
});

test('QR target never grants editing to a different signed-in account', async()=>{
  const original=globalThis.fetch;let saves=0;
  globalThis.fetch=async()=>({status:200,json:async()=>({handle:'other',revision:0,appearance:null})});
  try {
    await piece.boot({user:{sub:'a'},colon:['other'],help:{repeat(){}},net:{userRequest:async method=>{
      if(method==='POST')saves++;
      return {status:200,handle:'tester',revision:1,appearance:defaultCharacter()};
    }}});
    const screen=canvas();piece.paint(screen);touch(20,280);await pause(350);
    assert.ok(screen.words.some(({text})=>text==='Log in as @other'));
    assert.ok(screen.words.some(({text})=>text==='Switch account'));
    assert.equal(saves,0);
  } finally {piece.leave();globalThis.fetch=original;}
});
