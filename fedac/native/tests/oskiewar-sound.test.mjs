import test from 'node:test';
import assert from 'node:assert/strict';
import {readFileSync,writeFileSync,mkdtempSync,rmSync} from 'node:fs';
import {tmpdir} from 'node:os';
import path from 'node:path';
import {execFileSync} from 'node:child_process';
import {OSKIEWAR_DRUM_LAYERS,renderOskiewarDrum,createOskiewarSound} from '../lib/oskiewar-host.mjs';
const xbox=readFileSync(new URL('../../../xbox/native-bios/App.cpp',import.meta.url),'utf8');
test('all seven AC drum waveforms match the actual Xbox layer renderer sample by sample',()=>{
  const dir=mkdtempSync(path.join(tmpdir(),'oskiewar-sound-'));
  try {
    const start=xbox.indexOf('    enum class Wave',xbox.indexOf('  void PlayDrum'));
    const end=xbox.indexOf('\n    m_voice->Stop',start);
    let native=xbox.slice(start,end).replace('static_cast<uint32_t>(GetTickCount64()) | 1u','0x6f736b69u');
    const cpp=`#include <algorithm>\n#include <cmath>\n#include <vector>\n#include <string_view>\n#include <cstdint>\n#include <cstdio>\nint main(int argc,char**argv){ unsigned m_sampleRate=48000; float velocity=1; std::string_view name=argv[1];\n${native}\nfor(double value:mixed){float sample=value*.28;fwrite(&sample,sizeof(sample),1,stdout);}}`;
    writeFileSync(path.join(dir,'sound.cpp'),cpp);
    execFileSync('c++',['-std=c++17','-O2',path.join(dir,'sound.cpp'),'-o',path.join(dir,'sound')]);
    let total=0;
    for(const name of Object.keys(OSKIEWAR_DRUM_LAYERS)) {
      const reference=execFileSync(path.join(dir,'sound'),[name]);
      const actual=renderOskiewarDrum(name);
      assert.equal(actual.length*4,reference.length,name);
      let peak=0,energy=0;
      for(let i=0;i<actual.length;i++) {
        assert.ok(Math.abs(actual[i]-reference.readFloatLE(i*4))<2e-7,`${name} sample ${i}`);
        assert.ok(Number.isFinite(actual[i]));
        peak=Math.max(peak,Math.abs(actual[i]));energy+=actual[i]*actual[i];
      }
      assert.ok(peak>.025&&peak<1,name+' has a bounded useful signal');
      assert.ok(energy>0);total+=actual.byteLength;
    }
    assert.ok(total<300000,'all precomputed drums fit in 300 KB');
  } finally {rmSync(dir,{recursive:true,force:true});}
});
function audioFixture(replay=true) {
  const events=[];let nextId=1;
  const sound={synth:opts=>{events.push(['synth',opts]);return {id:nextId++};},
    kill:(voice,fade)=>events.push(['kill',voice,fade])};
  if(replay)sound.replay={loadData:(data,rate)=>{events.push(['load',data,rate]);return true;},
    play:opts=>{events.push(['play',opts]);return {id:nextId++};},
    kill:(voice,fade)=>events.push(['replay-kill',voice,fade])};
  return {events,audio:createOskiewarSound(()=>sound)};
}
test('drums use cached mono PCM and replace the prior event without accumulating voices',()=>{
  const {audio,events}=audioFixture();
  audio.drum('kick',1.2,-1);audio.drum('kick',.6,1);
  const loads=events.filter(e=>e[0]==='load');
  assert.equal(loads.length,2);assert.equal(loads[0][1],loads[1][1]);
  assert.equal(loads[0][2],48000);
  assert.deepEqual(events.filter(e=>e[0]==='play').map(e=>e[1]),[
    {tone:440,base:440,volume:1.2,pan:0,loop:false},
    {tone:440,base:440,volume:.6,pan:0,loop:false}]);
  assert.equal(events.filter(e=>e[0]==='replay-kill').length,1);
  assert.equal(events.filter(e=>e[0]==='synth').length,0);
  audio.synth(880,.12);
  assert.equal(events.filter(e=>e[0]==='replay-kill').length,2);
  assert.deepEqual(events.at(-1),['synth',{type:'sine',tone:880,duration:.12,volume:.5,attack:0,decay:.12,pan:0}]);
  audio.drum('whoosh');assert.equal(events.filter(e=>e[0]==='kill').length,1);
});
test('sine defaults, bounds and release match Xbox rather than native default envelopes',()=>{
  const {audio,events}=audioFixture();
  audio.synth(10000,5,.99,'square',1);
  assert.deepEqual(events.at(-1)[1],{type:'sine',tone:5000,duration:2,volume:.5,attack:0,decay:2,pan:0});
  audio.synth(10,-1);
  assert.deepEqual(events.at(-1)[1],{type:'sine',tone:20,duration:.005,volume:.5,attack:0,decay:.005,pan:0});
  audio.synth(440);assert.equal(events.at(-1)[1].duration,.05);
  assert.equal(events.filter(e=>e[0]==='kill').length,2);
});
test('older-host fallback retains the full instruments and a six-voice maximum',()=>{
  const {audio,events}=audioFixture(false);
  audio.drum('snare',1);assert.equal(events.filter(e=>e[0]==='synth').length,6);
  audio.drum('bell',1);assert.equal(events.filter(e=>e[0]==='kill').length,6);
  assert.deepEqual(events.filter(e=>e[0]==='synth').slice(-3).map(e=>e[1].tone),[880,1320,1760]);
  audio.stop();assert.equal(events.filter(e=>e[0]==='kill').length,9);
});
