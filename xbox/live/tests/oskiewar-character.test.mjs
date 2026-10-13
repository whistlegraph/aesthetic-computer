import test from 'node:test';
import assert from 'node:assert/strict';
import { fixture } from './fixtures/dojo.mjs';
import { defaultCharacter, CHARACTER_PARTS } from '../../../system/public/aesthetic.computer/lib/oskiewar-character.mjs';

test('each signed-in seat gets its own public editor QR and live appearance',()=>{
 const f=fixture(), first=defaultCharacter(), second=defaultCharacter([80,120,220]);
 CHARACTER_PARTS.forEach((part,i)=>first.colors[part]=[25+i*35,180-i*20,50+i*15]);
 const state={status:'signed-in',handle:'first',character:{handle:'first',revision:1,appearance:first},
  localPlayer2:{status:'signed-in',handle:'fifi',character:{handle:'fifi',revision:7,appearance:second}}};
 f.state(state);
 assert.equal(f.run('characterEditorCode(players[0]).url'),'https://aesthetic.computer/oskiewarchar:@first');
 assert.equal(f.run('characterEditorCode(players[1]).url'),'https://aesthetic.computer/oskiewarchar:@fifi');
 assert.deepEqual(JSON.parse(f.run('JSON.stringify(players.map(p=>p.characterAppearance))')),[first,second]);
 const next=structuredClone(state);next.character.revision=2;next.character.appearance.colors.leftArm=[1,2,3];
 f.state(next);
 assert.equal(f.run('figurePaletteOf(players[0]).slice(9,12).join()'),'1,2,3');
 assert.deepEqual(JSON.parse(f.run('JSON.stringify(players[1].characterAppearance)')),second);
 f.state({status:'signed-in',handle:'first',localPlayer2:{status:'signed-out'}});
 assert.equal(f.run('characterEditorCode(players[1])'),null);
 assert.equal(f.run('players[1].characterAppearance'),null);
 assert.equal(f.run('players[0].characterAppearance'),null);
});

test('wrong-account and malformed appearances cannot dress a seat, and all face choices compile',()=>{
 const f=fixture();
 f.state({status:'signed-in',handle:'first',character:{handle:'fifi',revision:1,appearance:defaultCharacter()}});
 assert.equal(f.run('players[0].characterAppearance'),null);
 const bad=defaultCharacter();bad.colors.head=[999,0,0];
 f.state({status:'signed-in',handle:'first',character:{handle:'first',revision:1,appearance:bad}});
 assert.equal(f.run('players[0].characterAppearance'),null);
 f.run(`for(const eyes of gameObjects.characterProfiles.CHARACTER_EYES)
  for(const mouth of gameObjects.characterProfiles.CHARACTER_MOUTHS){
   const figure=gameObjects.characterFigure(eyes,mouth);
   if(typeof figure!=='function'||!figure.sketches.length)throw Error(eyes+':'+mouth);
  }`);
});

test('replay keeps recorded colors while live editing waits for playback to end',()=>{
 const f=fixture(), appearance=defaultCharacter([210,140,100]);
 f.state({status:'signed-in',handle:'first',character:{handle:'first',revision:1,appearance}});
 f.run('updateSpineBodies(1000000);globalThis.frame=makeRoundReplayFrame(1000000);instantReplay={};');
 f.state({status:'signed-in',handle:'first',character:{handle:'first',revision:2,appearance:defaultCharacter([10,20,30])}});
 assert.equal(f.run('players[0].characterAppearance.colors.head.join()'),'210,140,100');
 f.run('instantReplay=null;players[0].characterAppearance=null;applyRoundReplayFrame(frame,2000000);');
 assert.equal(f.run('players[0].characterAppearance.colors.head.join()'),'210,140,100');
});
