import { readFile, writeFile } from 'node:fs/promises';
import assert from 'node:assert/strict';
const here = new URL('./', import.meta.url);
const np = await readFile(new URL('../pop/marimba/marimbaba.np', here), 'utf8');
const sections = [];
for (const line of np.split('\n')) {
  const marker = line.match(/^# (hush|twinkle|wow|baba|sleep) (\d+) /);
  if (marker) sections.push({name: marker[1], bars: Number(marker[2]), tokens: [], events: []});
  if (!line.trim() || line.startsWith('#') || !sections.length) continue;
  const section = sections.at(-1);
  for (const token of line.trim().split(/\s+/)) {
    const match = token.match(/^(.*):_\*([\d.]+)$/);
    assert(match, `Unrecognized score token: ${token}`);
    const pitches = match[1].toLowerCase().split('+').filter(Boolean);
    const beats = Number(match[2]);
    const pitch = pitches.length > 1 ? '[' + pitches.join(',') + ']' : pitches[0] || '~';
    section.tokens.push(pitch + '@' + beats);
    section.events.push({pitches, beats});
  }
}
assert.equal(sections.length, 5);
assert.equal(sections.reduce((n, s) => n + s.bars, 0), 24);
for (const s of sections) assert.equal(s.events.reduce((n,e) => n + e.beats, 0), s.bars * 3);
let beat = 0;
const events = sections.flatMap(s => s.events.map(e => { const event={...e, at:beat}; beat+=e.beats; return event; }));
await writeFile(new URL('marimbaba-score.json', here), JSON.stringify({bars:24, beats:72, bpm:56, events}, null, 2)+'\n');
const notation = sections.map(s => `  // ${s.name}: ${s.bars} bars\n  '${s.tokens.join(' ')}',`).join('\n');
const bass = '<f2 f2 f2 f2 f2 f3 f2 f3 f2 f3 f2 f2 f2 f2 f2 c3 bb1 f2 f2 f2 f2 f2 f2 f2>';
const pad = '<[f3,a3,c4] [f3,a3,c4] [f3,a3,c4] [f3,a3,c4] [f3,a3,c4] [f3,a3,c4] [f3,a3,c4] [bb2,d3,f3] [bb2,d3,f3] [bb2,d3,f3] [g3,bb3,d4] [g3,bb3,d4] [g3,bb3,d4] [g3,bb3,d4] [f3,a3,c4] [c3,e3,g3] [f3,a3,c4] [f3,a3,c4] [f3,a3,c4] [f3,a3,c4] [f3,a3,c4] [f3,a3,c4] [f3,a3,c4] [f3,a3,c4]>';
for (const remix of [false, true]) {
  const name = remix ? 'marimbaba-orbit' : 'marimbaba';
  const source = `// MARIMBABA${remix ? ' / ORBIT REMIX' : ''} — @jeffrey / aesthetic.computer
// https://pat.aesthetic.computer/${name}
// Lead: pop/marimba/marimbaba.np — all 24 bars, original pitches and durations.
// Accompaniment: a reduced arrangement informed by bin/render-marimbaba.mjs.
// This is an editable score adaptation, not playback of the released master.
await import('https://pat.aesthetic.computer/s')

// TEMPO — one cycle = one 3/4 bar. ${remix ? '84 BPM remix; set 56/3 for the lullaby.' : 'Original 56 BPM.'}
setcpm(${remix ? 84 : 56}/3)

// MELODY — @ weights are beats. mini(...) parses the assembled strings.
// Five sections: hush, twinkle, wow, ba-ba, sleep. Total: 72 beats / 24 bars.
const melody = mini([
${notation}
].join(' ')).note().slow(24)

// SINGER — ac_marimba uses the source rosewood ratios 1:4:9.2.
// .n(0..1) changes mallet brightness and ring; change .s(...) to recast it.
$: melody.s('ac_marimba').n(${remix ? 'perlin.slow(13).range(.35,.9)' : '.45'})
  .attack(.001).release(.5).gain(.8).room(.3)._pianoroll()

// ROCKING BASS — starts on beat one. Roots follow the source bass figure.
$: note("${bass}").s('${remix ? 'ac_swarm' : 'ac_sine'}')
  .n(.15).attack(.008).decay(.25).sustain(.25).release(.3).gain(${remix ? '.3' : '.38'}).lpf(450)

// HARMONY — F / Bb / Gm / F / C / F, following the renderer's chord map.
// Rearticulated once per bar; intentionally reduced from the finished mix.
$: note("${pad}").s('ac_bloom')
  .n(.08).attack(.06).release(.65).gain(.16).lpf(1800).room(.4)
${remix ? `
// ORBIT DOUBLE — exact same melody, different spectral rhythm and stereo motion.
// Comment out this block to hear just the marimba-led arrangement.
$: melody.s('ac_orbit').n(perlin.slow(17).range(.15,.85))
  .gain(sine.slow(23).range(.1,.22)).pan(sine.slow(19))
  .attack(.004).release(.4).delay(.2).delaytime(.357).room(.3)
` : ''}
// All active blocks play together. Comment out a whole $: block to mute it.
// Voices/controls: https://pat.aesthetic.computer/source
`;
  await writeFile(new URL(name+'.strudel',here),source);
}
console.log('Built two Marimbaba arrangements from the 72-beat source score.');
