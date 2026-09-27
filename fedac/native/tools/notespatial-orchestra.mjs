// A chapter orchestra: program changes belong to phrases, never to partials.
export const GM_NAMES = [
  'Grand Piano', 'Bright Piano', 'Electric Grand', 'Honky Tonk', 'Electric Piano 1', 'Electric Piano 2', 'Harpsichord', 'Clavinet',
  'Celesta', 'Glockenspiel', 'Music Box', 'Vibraphone', 'Marimba', 'Xylophone', 'Tubular Bells', 'Dulcimer',
  'Drawbar Organ', 'Percussive Organ', 'Rock Organ', 'Church Organ', 'Reed Organ', 'Accordion', 'Harmonica', 'Tango Accordion',
  'Nylon Guitar', 'Steel Guitar', 'Jazz Guitar', 'Clean Guitar', 'Muted Guitar', 'Overdriven Guitar', 'Distortion Guitar', 'Guitar Harmonics',
  'Acoustic Bass', 'Finger Bass', 'Pick Bass', 'Fretless Bass', 'Slap Bass 1', 'Slap Bass 2', 'Synth Bass 1', 'Synth Bass 2',
  'Violin', 'Viola', 'Cello', 'Contrabass', 'Tremolo Strings', 'Pizzicato Strings', 'Orchestral Harp', 'Timpani',
  'String Ensemble 1', 'String Ensemble 2', 'Synth Strings 1', 'Synth Strings 2', 'Choir Aahs', 'Voice Oohs', 'Synth Voice', 'Orchestra Hit',
  'Trumpet', 'Trombone', 'Tuba', 'Muted Trumpet', 'French Horn', 'Brass Section', 'Synth Brass 1', 'Synth Brass 2',
  'Soprano Sax', 'Alto Sax', 'Tenor Sax', 'Baritone Sax', 'Oboe', 'English Horn', 'Bassoon', 'Clarinet',
  'Piccolo', 'Flute', 'Recorder', 'Pan Flute', 'Blown Bottle', 'Shakuhachi', 'Whistle', 'Ocarina',
  'Square Lead', 'Saw Lead', 'Calliope Lead', 'Chiff Lead', 'Charang Lead', 'Voice Lead', 'Fifths Lead', 'Bass and Lead',
  'New Age Pad', 'Warm Pad', 'Polysynth Pad', 'Choir Pad', 'Bowed Pad', 'Metallic Pad', 'Halo Pad', 'Sweep Pad',
  'Rain', 'Soundtrack', 'Crystal', 'Atmosphere', 'Brightness', 'Goblins', 'Echoes', 'Science Fiction',
  'Sitar', 'Banjo', 'Shamisen', 'Koto', 'Kalimba', 'Bagpipe', 'Fiddle', 'Shanai',
  'Tinkle Bell', 'Agogo', 'Steel Drums', 'Woodblock', 'Taiko', 'Melodic Tom', 'Synth Drum', 'Reverse Cymbal',
  'Fret Noise', 'Breath Noise', 'Seashore', 'Bird Tweet', 'Telephone', 'Helicopter', 'Applause', 'Gunshot',
];
const range = (a, b) => Array.from({ length: b - a + 1 }, (_, i) => a + i);
// Each group shares a timbre. Chords and simultaneous answers stay together;
// an equal share of successive onsets goes to each member of the palette.
const PALETTES = [
  [['held', range(0, 7)], ['ring echo top', range(72, 79)]],
  [['held top', range(8, 15)], ['ring echo answer', range(24, 31)], ['bass', range(32, 39)]],
  [['held top', range(64, 71)], ['ring echo answer', range(40, 46)]],
  [['ring theme answer stacc', range(104, 111)], ['held echo top', range(112, 119)]],
  [['held ring echo stacc top', range(16, 23)]],
  [['held ring echo', range(48, 54)], ['pad top', range(88, 95)]],
  [['held ring theme answer top', range(80, 87)], ['pad echo', range(96, 103)]],
  [['held theme', [81, 84, 85, 87]], ['ring echo answer', [24, 28, 30, 11]], ['pad', [90, 94]], ['bass', [38, 39]]],
  [['brass held ring echo top', range(56, 63)]],
  [['held', [0, 4, 11, 73]], ['ring echo answer', [25, 45, 12, 46]], ['pad top', [48, 89]], ['bass', [33, 35, 32]]],
  [['held ring echo top', [10, 8, 0]]],
];

export function orchestrate128(lanes, movements) {
  const cues = [];
  movements.forEach((m, mi) => {
    for (const [roles, programs] of PALETTES[mi]) {
      const events = lanes.flatMap(l => l.events).filter(e => e.t >= m.t0 && e.t < m.t1 && roles.split(' ').includes(e.role));
      const onsets = [...new Set(events.map(e => e.t))].sort((a, b) => a - b);
      if (onsets.length < programs.length) throw Error(`${m.name}: too few ${roles} onsets for palette`);
      const byTime = new Map(onsets.map((t, i) => [t, programs[Math.min(programs.length - 1, Math.floor(i * programs.length / onsets.length))]]));
      for (const e of events) {
        e.gm = byTime.get(e.t);
        // Slow voices need time to speak; the composition's long pad notes
        // carry those programs, while rhythm and bass keep their short gates.
        if (e.role === 'pad') e.attack = Math.min(.25, e.dur * .15);
      }
      for (const program of programs) {
        const notes = events.filter(e => e.gm === program);
        cues.push({ chapter: mi + 1, roles, program, name: GM_NAMES[program], t: Math.min(...notes.map(e => e.t)), notes: notes.length });
      }
    }
  });
  // GM's last bank is sound effects, not the separate MIDI drum channel.
  // These are stage cues, kept out of the melody and the subwoofer feed.
  const accents = [
    [1, 120, 1, .65, .22], [0, 121, .4, 1.1, .13],
    [5, 122, 1, 6, .16], [10, 123, 5, 2, .2],
    [4, 124, 2, 1.5, .17], [3, 125, 1, 3, .14],
    [8, 126, 1, 2.2, .14], [3, 127, -1, .65, .17],
    [8, 55, -.6, .6, .26], [8, 47, 0, .8, .3],
  ];
  const cueLane = lanes.find(l => l.name === 'echo 1');
  for (const [mi, program, offset, dur, g] of accents) {
    const m = movements[mi], t = +(offset < 0 ? m.t1 + offset : m.t0 + offset).toFixed(4);
    cueLane.events.push({ t, dur, hz: program === 47 ? 130.81 : 261.63, g, wave: 'sine', attack: .005, decay: dur * .45, gm: program, role: 'cue' });
    cues.push({ chapter: mi + 1, roles: 'cue', program, name: GM_NAMES[program], t, notes: 1 });
  }
  const used = new Set(lanes.flatMap(l => l.events).filter(e => Number.isInteger(e.gm)).map(e => e.gm));
  const missing = range(0, 127).filter(p => !used.has(p));
  if (missing.length) throw Error(`Missing GM programs: ${missing.join(', ')}`);
  return { programs: 128, cues: cues.sort((a, b) => a.t - b.t) };
}
