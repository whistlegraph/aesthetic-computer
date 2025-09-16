#!/usr/bin/env node
/**
 * Ableton Timeline Simulation (ncurses-style)
 *
 * Uses pre-extracted notes + timeline from analyze-ableton.mjs output to run a
 * silent clock and display progress, remaining notes, and simple track meters.
 *
 * Inputs (flags):
 *   --project-xml <path>   (optional, for future: direct parse) NOT USED yet
 *   --notes <notes.json>   (required unless --auto)
 *   --report <report.json> (required unless --auto)
 *   --auto                 (infer notes.json & report.json in current dir)
 *   --rate <multiplier>    playback speed (default 1.0)
 *   --fps <number>         UI refresh frames per second (default 20)
 *   --length <beats>       override total beats (else infer from last note or timeline)
 *   --filter-pitch <num>   only count/render notes of this pitch (others ignored in meters)
 *   --filter-pitches a,b   comma list alternative to select multiple pitches
 *   --aggregate-window <b> rolling window (beats) for density bars (default 4)
 *   --show-stream          display a scrolling recent note stream (pitch-coded)
 *   --stream-width <n>     width of note stream (default 60)
 *   --hat-pitches a,b      define a set of "hi-hat" (or target) pitches to aggregate
 *   --density-full <n>     notes/beat for full (red) bar (default 8)
 *   --adaptive-density     dynamically rescale density-full when exceeded
 *   --snapshot-csv <file>  write periodic aggregate snapshots to CSV
 *   --snapshot-interval-beats <b> beats interval between CSV rows (default 4)
 *   --sparklines            append ASCII mini density sparkline per track
 *   --spark-width <n>       width (frames) of sparkline history (default 30)
 *   --bpm <n>               override/global tempo (ignores parsed tempo map)
 *   --sandcastle            accumulate per-column note bursts over song forming a bottom 'sand' visualization
 *   --sand-height <rows>    vertical rows for sandcastle (default 16)
 *   --burst-line            append a scrolling bottom line mark for every track burst (each asterisk)
 *   --minimal-line          ultra-minimal mode: print legend once then a single continuous line of glyphs as notes fire
 *   --minimal-pitch         (with --minimal-line) emit pitch-class letters instead of track dots when not matched by group/hat
 *   --groups-grid           show one scrolling lane per pitch group (or hat / top tracks if no groups) with time on X
 *   --start <beat>         start simulation from this beat (default 0)
 *   --end <beat>           end simulation early (default inferred length)
 *   --help
 *
 * Display:
 *   Top line: Beat mm:ss.beat  |  Progress bar  |  Notes played / total (pct)  | ETA
 *   Next lines: Per-track meter (# symbols for notes this frame) limited to width.
 *   Locator line updates when passing locator times.
 *
 * Press 'q' to quit early.
 *
 * Run example (after analyze step):
 *   node simulate-ableton.mjs --auto --rate 2
 */

import fs from 'fs';
import readline from 'readline';
import process from 'process';
import { SaxesParser } from 'saxes';

/* Minimal color helpers (avoid extra deps) */
const color = {
  dim: s => `\x1b[2m${s}\x1b[0m`,
  cyan: s => `\x1b[36m${s}\x1b[0m`,
  green: s => `\x1b[32m${s}\x1b[0m`,
  yellow: s => `\x1b[33m${s}\x1b[0m`,
  magenta: s => `\x1b[35m${s}\x1b[0m`,
  bold: s => `\x1b[1m${s}\x1b[0m`,
  red: s => `\x1b[31m${s}\x1b[0m`
};

const args = process.argv.slice(2);
function getFlag(name, def = undefined) {
  const i = args.indexOf(`--${name}`);
  if (i !== -1) return args[i+1];
  return def;
}
const has = name => args.includes(`--${name}`);

if (has('help')) {
  console.log(`Ableton Timeline Simulation\n\nFlags:\n  --auto\n  --notes notes.json\n  --report report.json\n  --project-xml <file> (parse raw Ableton extracted XML directly)\n  --rate <multiplier>\n  --fps <n>\n  --length <beats>\n  --filter-pitch <num>\n  --filter-pitches a,b\n  --aggregate-window <beats>\n  --show-stream --stream-width <n>\n  --hat-pitches a,b\n  --density-full <n> --adaptive-density\n  --snapshot-csv <file> --snapshot-interval-beats <b>\n  --sparklines --spark-width <n>\n  --bpm <n>\n  --start <beat> --end <beat>\n  --help\n`);
  process.exit(0);
}

const auto = has('auto');
const projectXmlPath = getFlag('project-xml');
const notesPath = getFlag('notes', auto ? './notes.json' : null);
const reportPath = getFlag('report', auto ? './report.json' : null);
if (!projectXmlPath && (!notesPath || !reportPath)) {
  console.error('Need either --project-xml <file> or both --notes and --report (or --auto).');
  process.exit(1);
}

function loadJSON(path) {
  return JSON.parse(fs.readFileSync(path, 'utf8'));
}

let notes = [];
let report = {};

async function parseProjectXML(file) {
  const parser = new SaxesParser({ xmlns:false });
  const stack = [];
  const trackSummaries = [];
  const trackStack = [];
  const clips = [];
  let currentClip = null;
  const locators = [];
  const tempoChanges = [];
  let currentTrack = null;
  let currentText = '';
  const timeLike = new Set(['CurrentStart','CurrentEnd','LoopStart','LoopEnd','Start','End','StartRelative','StartMarker','EndMarker']);

  parser.on('opentag', node => {
    const name = node.name;
    const attrs = Object.fromEntries(Object.entries(node.attributes).map(([k,v])=>[k, v.value ?? v]));
    stack.push(name);
    // Track detection
    if (/(MidiTrack|AudioTrack|GroupTrack|ReturnTrack|MainTrack|PreHearTrack)$/.test(name)) {
      const id = attrs.Id || attrs.ID || attrs.id;
      currentTrack = { id, type: name, name: null, clipCount:0, clips:[], devices:[] };
      trackSummaries.push(currentTrack);
      trackStack.push(currentTrack);
    }
    // Clip detection
    if (/Clip$/.test(name) && name !== 'ClipSlot') {
      currentClip = { type:name, trackId: currentTrack?.id, times:{}, notes:[], name: attrs.Name || '' };
      clips.push(currentClip);
      if (currentTrack) currentTrack.clipCount++;
    }
    // Note events
    if (name === 'MidiNoteEvent' && currentClip) {
      const rel = parseFloat(attrs.Time || attrs.time || '0') || 0;
      const dur = parseFloat(attrs.Duration || attrs.duration || '0') || 0;
      const vel = parseFloat(attrs.Velocity || attrs.velocity || '0') || 0;
      const pitch = attrs.Pitch || attrs.Note || attrs.NoteNumber || attrs.pitch;
      currentClip.notes.push({ relTime: rel, duration: dur, velocity: vel, pitch: pitch!=null?Number(pitch):undefined });
    }
    // Locators
    if (name === 'Locator') {
      const time = parseFloat(attrs.Time || attrs.time || attrs.Start || '0') || 0;
      locators.push({ id: attrs.Id || attrs.id, time, name: attrs.Name || attrs.name || '' });
    }
    // Tempo (rough heuristic)
    if (/Tempo/i.test(name) && attrs.Value) {
      const val = parseFloat(attrs.Value);
      if (!Number.isNaN(val)) tempoChanges.push({ beat: 0, value: val });
    }
    // Time fields inside clips
    if (currentClip && timeLike.has(name) && (attrs.Value!=null)) {
      const v = parseFloat(attrs.Value);
      if (!Number.isNaN(v)) currentClip.times[name] = v;
    }
  });
  parser.on('closetag', name => {
    // capture accumulated text for names
    const text = currentText.trim();
    currentText = '';
    if (text) {
      const parent = stack[stack.length-1];
      if (currentTrack && /^(EffectiveName|UserName)$/.test(name) && !currentTrack.name) currentTrack.name = text;
      if (currentClip && /^(EffectiveName|UserName)$/.test(name) && !currentClip.name) currentClip.name = text;
    }
    stack.pop();
    if (/Clip$/.test(name) && currentClip && currentClip.type === name) {
      currentClip = null;
    }
    if (/(MidiTrack|AudioTrack|GroupTrack|ReturnTrack|MainTrack|PreHearTrack)$/.test(name)) {
      trackStack.pop();
      currentTrack = trackStack[trackStack.length-1] || null;
    }
  });
  parser.on('text', t => { currentText += t; });
  await new Promise((res,rej)=>{
    parser.on('error', rej);
    parser.on('end', res);
    const stream = fs.createReadStream(file, { encoding:'utf8' });
    stream.on('data', chunk => parser.write(chunk));
    stream.on('end', ()=>parser.close());
    stream.on('error', rej);
  });
  // Build notes absolute times
  const flatNotes = [];
  for (const clip of clips) {
    const start = clip.times.CurrentStart ?? clip.times.Start ?? 0;
    for (const n of clip.notes) {
      flatNotes.push({ beat: start + n.relTime, relBeat: n.relTime, duration: n.duration, velocity: n.velocity, pitch: n.pitch, trackId: clip.trackId, clipName: clip.name });
    }
  }
  flatNotes.sort((a,b)=>a.beat-b.beat);
  // Timeline (simple)
  const timeline = clips.map((c,i)=>({ index:i, trackId:c.trackId, name:c.name, times:c.times, noteCount:c.notes.length }));
  return { notes: flatNotes, report: { trackSummaries, timeline, tempoChanges, locators } };
}

if (projectXmlPath) {
  const t0 = Date.now();
  const parsed = await parseProjectXML(projectXmlPath);
  notes = parsed.notes;
  report = parsed.report;
  console.error(`[simulate] Parsed XML ${projectXmlPath} -> notes=${notes.length} tracks=${report.trackSummaries.length} clips=${report.timeline.length} in ${Date.now()-t0}ms`);
} else {
  notes = loadJSON(notesPath);
  report = loadJSON(reportPath);
}

// Pitch filtering (single or multi)
const pitchFilterSingle = getFlag('filter-pitch');
const pitchFilterMulti = getFlag('filter-pitches');
let pitchFilterSet = null;
if (pitchFilterMulti) {
  pitchFilterSet = new Set(pitchFilterMulti.split(',').map(s=>s.trim()).filter(Boolean));
} else if (pitchFilterSingle) {
  pitchFilterSet = new Set([String(pitchFilterSingle)]);
}
// Preserve original unfiltered note list for omni-line output (so filters don't hide events)
const allNotesUnfiltered = [...notes];
if (pitchFilterSet && !has('omni-line')) {
  notes = notes.filter(n => pitchFilterSet.has(String(n.pitch)));
}

// Hat / target pitch set (for aggregation stats only)
const hatPitchFlag = getFlag('hat-pitches','');
const hatPitchSet = new Set(hatPitchFlag.split(',').map(s=>s.trim()).filter(Boolean));

// Determine length (beats)
let inferredLength = 0;
if (notes.length) inferredLength = Math.max(inferredLength, notes[notes.length - 1].beat);
if (report.timeline && report.timeline.length) {
  for (const c of report.timeline) {
    if (c.times && c.times.CurrentEnd != null) inferredLength = Math.max(inferredLength, c.times.CurrentEnd);
  }
}
const overrideLength = getFlag('length');
const totalBeats = overrideLength ? parseFloat(overrideLength) : inferredLength;

const startBeat = parseFloat(getFlag('start', '0'));
const endBeat = parseFloat(getFlag('end', totalBeats));

// Playback config
const rate = parseFloat(getFlag('rate', '1'));
const fps = parseFloat(getFlag('fps', '20'));
const aggWindow = parseFloat(getFlag('aggregate-window','4')); // beats
const showStream = has('show-stream');
const streamWidth = parseInt(getFlag('stream-width','60'));
const useSparklines = has('sparklines');
const sparkWidth = parseInt(getFlag('spark-width','30'));
const useSandcastle = has('sandcastle');
const sandHeight = parseInt(getFlag('sand-height','16'));
const useBurstLine = has('burst-line');
const useMinimalLine = has('minimal-line');
const useMinimalPitch = has('minimal-pitch');
const noMinimalLegend = has('no-minimal-legend'); // suppress legend header in minimal mode
const useOmniLine = has('omni-line'); // new: stream ALL timeline events (notes, locators, clip boundaries, tempo changes)
const noBeatMarks = has('no-beat-marks'); // suppress beat markers in minimal/omni modes
if (useMinimalLine && useOmniLine) {
  console.error('[simulate] Warning: --omni-line overrides --minimal-line');
}
const useGroupsGrid = has('groups-grid');
const groupsSpecRaw = getFlag('groups'); // format: Name:p1,p2,p3:Glyph;Name2:p4:Glyph2
const gridCsvPath = getFlag('grid-csv'); // optional: export per-beat group counts
const topPitchesFlag = getFlag('top-pitches'); // optional: list top N pitches then continue
const showAllTracks = has('show-all-tracks'); // new: do not hide inactive tracks during run
const trackLimitFlag = parseInt(getFlag('track-limit', '20')); // configurable max before clipping (ignored if show-all)
const fullView = has('full-view'); // enable most visual layers at once
const condenseTracks = has('condense-tracks'); // multi-column compact track display
const windowPitchStatsN = parseInt(getFlag('window-pitch-stats','0')); // top N pitch counts in current aggregate window (0=off)

// Full view implies enabling key layers unless user already supplied specific flags
if (fullView) {
  if (!useGroupsGrid) args.push('--groups-grid');
  if (!useSandcastle) args.push('--sandcastle');
  if (!useBurstLine) args.push('--burst-line');
  if (!showStream) args.push('--show-stream');
}

// Parse groups
let pitchGroups = [];
if (groupsSpecRaw) {
  const palette = ['\x1b[32m','\x1b[33m','\x1b[31m','\x1b[36m','\x1b[35m','\x1b[34m','\x1b[92m','\x1b[93m'];
  let colorIdx = 0;
  for (const seg of groupsSpecRaw.split(/;+/)) {
    const trimmed = seg.trim(); if (!trimmed) continue;
    const parts = trimmed.split(':');
    if (parts.length < 3) continue;
    const name = parts[0];
    const pitches = parts[1].split(',').map(s=>s.trim()).filter(Boolean);
    const glyph = parts.slice(2).join(':').trim()[0]; // first char of remaining as glyph
    if (!glyph) continue;
    const set = new Set(pitches);
    const colorCode = palette[colorIdx++ % palette.length];
    pitchGroups.push({ name, set, glyph, colorCode });
  }
}

// Warn if defined pitch groups don't match any present pitches (common cause of empty grid lanes)
if (pitchGroups.length) {
  const presentPitches = new Set(notes.map(n => String(n.pitch)));
  const inactive = pitchGroups.filter(g => ![...g.set].some(p => presentPitches.has(p)));
  if (inactive.length === pitchGroups.length) {
    console.error('[groups] Warning: none of the defined pitch groups match any note pitches.');
    if (presentPitches.size) {
      console.error('[groups] Example available pitches:', [...presentPitches].slice(0,12).join(','));
    }
  } else if (inactive.length) {
    console.error('[groups] Inactive groups (no matching pitches):', inactive.map(g=>g.name).join(', '));
  }
}

// Optional: report top pitches
if (topPitchesFlag) {
  const N = parseInt(topPitchesFlag)||10;
  const freq = new Map();
  for (const n of notes) freq.set(n.pitch, (freq.get(n.pitch)||0)+1);
  const sorted = [...freq.entries()].sort((a,b)=>b[1]-a[1]).slice(0,N);
  console.error(`[top-pitches] Top ${sorted.length} pitches:`);
  console.error(sorted.map(([p,c])=>`${p}:${c}`).join(' '));
}

// Assume base tempo constant for mm:ss (use first tempo change if present)
let bpm = 120; // default fallback
const bpmOverride = getFlag('bpm');
if (bpmOverride) {
  const parsed = parseFloat(bpmOverride);
  if (!Number.isNaN(parsed)) bpm = parsed;
} else if (report.tempoChanges && report.tempoChanges.length) {
  const first = report.tempoChanges[0];
  if (first && (first.value || first.Value)) bpm = parseFloat(first.value || first.Value) || bpm;
}
// Density scaling flags
let densityFull = parseFloat(getFlag('density-full','8'));
const adaptiveDensity = has('adaptive-density');

// Snapshot CSV
const snapshotCsvPath = getFlag('snapshot-csv');
const snapshotIntervalBeats = parseFloat(getFlag('snapshot-interval-beats','4'));
let nextSnapshotBeat = snapshotIntervalBeats;
let csvStream = null;
if (snapshotCsvPath) {
  csvStream = fs.createWriteStream(snapshotCsvPath, { flags: 'w' });
}

// Tempo map (piecewise bpm)
let tempoMap = [];
if (report.tempoChanges && report.tempoChanges.length) {
  for (const tc of report.tempoChanges) {
    const beat = tc.beatTime ?? tc.beat ?? tc.time;
    const bpmVal = tc.value ?? tc.Value;
    if (beat != null && bpmVal != null) tempoMap.push({ beat: Number(beat), bpm: Number(bpmVal) });
  }
  tempoMap.sort((a,b)=>a.beat-b.beat);
}
if (tempoMap.length === 0 || bpmOverride) tempoMap = [{ beat: 0, bpm }];
function bpmAtBeat(b) {
  let last = tempoMap[0];
  for (const seg of tempoMap) { if (seg.beat <= b) last = seg; else break; }
  return last.bpm;
}
function beatToSeconds(b) {
  let seconds = 0;
  for (let i=0;i<tempoMap.length;i++) {
    const cur = tempoMap[i];
    const next = tempoMap[i+1];
    const segStart = cur.beat;
    const segEnd = next ? Math.min(next.beat, b) : b;
    if (b <= segStart) break;
    const span = Math.max(0, segEnd - segStart);
    seconds += span * (60 / cur.bpm);
    if (!next || next.beat >= b) break;
  }
  return seconds;
}

// Index notes by beat for faster simulation scanning
let noteIndex = 0; // global moving pointer (sorted by beat already)

// Track mapping
const trackMap = new Map();
if (report.trackSummaries) {
  for (const t of report.trackSummaries) trackMap.set(String(t.id), t);
}

// Locator awareness
const locators = (report.locators || []).slice().sort((a,b)=>a.time-b.time);
let nextLocatorIdx = locators.findIndex(l => l.time >= startBeat);
if (nextLocatorIdx === -1) nextLocatorIdx = locators.length; // none ahead

// Stats
const totalNotes = notes.length;
let playedNotes = 0;
let playedHats = 0;
const trackTotals = new Map();
const trackHatTotals = new Map();
const trackHistory = new Map(); // trackId -> array of densityRate snapshots (notes/beat)

// Rolling window store (simple queue)
const windowEvents = []; // each {beat, trackId, pitch}

// Recent stream characters
const recentEvents = [];

// Sandcastle accumulation: per song-progress column heights (capped at sandHeight)
let sandCols = [];
let lastSandWidth = 0;
const burstMarks = []; // sequence of marks per track burst

// Minimal line mode state
let minimalLegendPrinted = false;
let lastBeatFloor = 0;
let omniEvents = [];
let omniIndex = 0;
let lastOmniBeatFloor = 0;
if (useOmniLine) {
  // Notes (all, unfiltered)
  for (const n of allNotesUnfiltered) omniEvents.push({ beat: n.beat, kind: 'note', data: n });
  // Locators
  for (const l of (report.locators||[])) if (l.time!=null) omniEvents.push({ beat: l.time, kind: 'locator', data: l });
  // Clip boundaries
  for (const c of (report.timeline||[])) {
    const start = c.times?.CurrentStart ?? c.times?.Start;
    const end = c.times?.CurrentEnd ?? c.times?.End;
    if (start!=null) omniEvents.push({ beat: start, kind: 'clipStart', data: c });
    if (end!=null) omniEvents.push({ beat: end, kind: 'clipEnd', data: c });
  }
  // Tempo changes
  for (const tc of (report.tempoChanges||[])) {
    const beat = tc.beatTime ?? tc.beat ?? tc.time;
    if (beat!=null) omniEvents.push({ beat: Number(beat), kind: 'tempo', data: tc });
  }
  omniEvents.sort((a,b)=>a.beat-b.beat || (a.kind==='note'? -1:1));
}

// Groups grid state
let gridInitialized = false;
let gridRows = []; // each {label, glyph, colorCode, cols:[]}
let gridWidth = 0;
let lastTermCols = 0;
let groupBeatHits = [];
let groupBeatCounts = [];
let lastBeatBucket = null; // beat index currently accumulating
let beatTickCols = []; // beat marker row (| every beat, bold every 4/16) aligned with committed cols
let gridCsvStream = null;
if (gridCsvPath) {
  gridCsvStream = fs.createWriteStream(gridCsvPath, { flags: 'w' });
}

// Prepare readline for keypress
readline.emitKeypressEvents(process.stdin);
if (process.stdin.isTTY) process.stdin.setRawMode(true);
process.stdin.on('keypress', (str, key) => {
  if (key.name === 'q' || (key.ctrl && key.name === 'c')) {
    cleanupAndExit();
  }
});

function cleanupAndExit(code=0) {
  process.stdout.write('\x1b[0m\n');
  process.exit(code);
}

function formatTime(beat) {
  const seconds = beatToSeconds(beat);
  const m = Math.floor(seconds / 60);
  const s = Math.floor(seconds % 60);
  const fracBeat = (beat % 1).toFixed(2).padStart(5,' ');
  return `${m}:${String(s).padStart(2,'0')}@${fracBeat}`;
}

function renderBar(pct, width) {
  const filled = Math.round(pct * width);
  return '[' + '#'.repeat(filled).padEnd(width,' ') + ']';
}

// Per-frame we gather notes in (currentBeat, nextBeatFrame]
const frameBeatIncrement = (rate / fps); // beats progressed per frame given base 1 beat per second at bpm=60? Wait: we base on beat unit.
// Clarify: we want real time reflect beats according to BPM; easier: advance beats by rate * (bpm/60)/fps? Actually 1 beat duration in seconds = 60/bpm.
// Each frame is 1/fps seconds -> beats advanced = (1/fps) / (60/bpm) = (bpm/60)/fps
const beatsPerFrameBase = (bpm / 60) / fps; // beats progressed at rate=1

let currentBeat = startBeat;
let lastFrameTime = Date.now();

const trackActivity = new Map(); // trackId -> transient count this frame

function stepFrame() {
  const now = Date.now();
  const dtSec = (now - lastFrameTime) / 1000;
  lastFrameTime = now;

  // Advance beats factoring dt to keep more stable timing
  const localBpm = bpmAtBeat(currentBeat);
  const beatsAdvance = dtSec * (localBpm / 60) * rate;
  currentBeat += beatsAdvance;
  if (currentBeat > endBeat) {
    currentBeat = endBeat;
  }

  // Clear activity
  trackActivity.clear();
  // Collect notes whose beat is <= currentBeat (play them)
  let frameNoteCount = 0;
  const frameNotes = [];
  while (noteIndex < notes.length && notes[noteIndex].beat <= currentBeat) {
    const n = notes[noteIndex];
    if (n.beat >= startBeat && n.beat <= endBeat) {
      playedNotes++;
      frameNoteCount++;
      frameNotes.push(n);
      const tId = String(n.trackId ?? '');
      trackActivity.set(tId, (trackActivity.get(tId) || 0) + 1);
      trackTotals.set(tId, (trackTotals.get(tId)||0)+1);
      windowEvents.push({ beat: n.beat, trackId: tId, pitch: n.pitch });
      // hat tracking independent of pitch filter (only among displayed notes already filtered)
      if (hatPitchSet.size && hatPitchSet.has(String(n.pitch))) {
        playedHats++;
        trackHatTotals.set(tId, (trackHatTotals.get(tId)||0)+1);
      }
      if (showStream) {
        recentEvents.push(n);
        if (recentEvents.length > streamWidth * 5) recentEvents.shift();
      }
  if (useMinimalLine && !useOmniLine) {
        // Emit glyph per note using group precedence, then hat, then track dot
        const pitchStr = String(n.pitch);
        let outGlyph = '';
        if (pitchGroups.length) {
          for (const g of pitchGroups) {
            if (g.set.has(pitchStr)) { outGlyph = g.colorCode + g.glyph + '\x1b[0m'; break; }
          }
        }
        if (!outGlyph) {
          if (hatPitchSet.size && hatPitchSet.has(pitchStr)) outGlyph = '\x1b[35mH\x1b[0m';
          else if (useMinimalPitch) {
            outGlyph = colorPitchGlyph(n.pitch);
          } else outGlyph = minimalTrackGlyph(tId);
        }
        process.stdout.write(outGlyph);
      }
    }
    noteIndex++;
  }

  if (useMinimalLine) {
    const bf = Math.floor(currentBeat);
    if (bf !== lastBeatFloor) {
      // Beat boundary marker (every 4 beats stronger)
      const bar = (bf % 16 === 0) ? '\x1b[1m|\x1b[0m' : (bf % 4 === 0 ? '|' : '');
      if (bar) process.stdout.write(color.dim(bar));
      lastBeatFloor = bf;
  if (useMinimalLine && !useOmniLine) {
    const bf = Math.floor(currentBeat);
    if (bf !== lastBeatFloor) {
      if (!noBeatMarks) {
        const bar = (bf % 16 === 0) ? '\x1b[1m|\x1b[0m' : (bf % 4 === 0 ? '|' : '');
        if (bar) process.stdout.write(color.dim(bar));
      }
      lastBeatFloor = bf;
    }
  }
  // Omni-line emission (independent of filtered note stream)
  if (useOmniLine) {
    while (omniIndex < omniEvents.length && omniEvents[omniIndex].beat <= currentBeat) {
      const ev = omniEvents[omniIndex];
      let glyph = '';
      switch (ev.kind) {
        case 'note': {
          const tId = String(ev.data.trackId ?? '');
          glyph = minimalTrackGlyph(tId);
          break; }
        case 'locator': glyph = '\x1b[35m|\x1b[0m'; break;
        case 'clipStart': glyph = '\x1b[32m[\x1b[0m'; break;
        case 'clipEnd': glyph = '\x1b[31m]\x1b[0m'; break;
        case 'tempo': glyph = '\x1b[33mt\x1b[0m'; break;
        default: glyph = '.'; break;
      }
      process.stdout.write(glyph);
      omniIndex++;
    }
    const bf2 = Math.floor(currentBeat);
    if (bf2 !== lastOmniBeatFloor) {
      if (!noBeatMarks) {
        const mark = (bf2 % 16 === 0) ? '\x1b[1m|\x1b[0m' : (bf2 % 4 === 0 ? '|' : '');
        if (mark) process.stdout.write(color.dim(mark));
      }
      lastOmniBeatFloor = bf2;
    }
  }
    }
  }

  // Sandcastle update
  if (useSandcastle && frameNoteCount > 0 && endBeat > startBeat) {
    // Determine current width and ensure sandCols sized
    const width = process.stdout.columns || 100;
    if (width !== lastSandWidth) {
      // Resize preserving proportional mapping
      const newCols = new Array(width).fill(0);
      for (let i=0;i<width;i++) {
        const srcIdx = sandCols.length ? Math.floor(i / width * sandCols.length) : 0;
        newCols[i] = sandCols[srcIdx] || 0;
      }
      sandCols = newCols;
      lastSandWidth = width;
    } else if (sandCols.length === 0) {
      sandCols = new Array(width).fill(0);
      lastSandWidth = width;
    }
    const songPct = (currentBeat - startBeat) / (endBeat - startBeat || 1);
    const col = Math.min(sandCols.length - 1, Math.max(0, Math.floor(songPct * sandCols.length)));
    sandCols[col] = Math.min(sandHeight, sandCols[col] + frameNoteCount); // cap
  }

  // Burst line update: one mark per active track with burst (act>0)
  if (useBurstLine) {
    for (const [tId, act] of trackActivity.entries()) {
      if (act > 0) {
        const glyph = act > 4 ? '█' : act > 2 ? '▆' : '▃';
        // color by track id hash
        let hash = 0; for (let i=0;i<tId.length;i++) hash = (hash*31 + tId.charCodeAt(i)) & 0xffff;
        const hueBucket = hash % 3; // 0 green,1 yellow,2 red
        const colorCode = hueBucket===0?'\x1b[32m':hueBucket===1?'\x1b[33m':'\x1b[31m';
        burstMarks.push(colorCode + glyph + '\x1b[0m');
      }
    }
    // Cap burstMarks length to avoid unbounded growth (keep last width* sandHeight *2)
    const maxBurst = (process.stdout.columns||100) * sandHeight * 2;
    if (burstMarks.length > maxBurst) burstMarks.splice(0, burstMarks.length - maxBurst);
  }

  // Prune old window events
  const windowStartBeat = currentBeat - aggWindow;
  while (windowEvents.length && windowEvents[0].beat < windowStartBeat) windowEvents.shift();

  // Locator passed?
  let locatorMsg = '';
  while (nextLocatorIdx < locators.length && locators[nextLocatorIdx].time <= currentBeat) {
    locatorMsg = ` Locator: ${locators[nextLocatorIdx].name || '(unnamed)'} @ ${locators[nextLocatorIdx].time}`;
    nextLocatorIdx++;
  }
  // Groups-grid aggregation BEFORE drawing (eliminates one-frame lag)
  if (useGroupsGrid) {
    const termCols = process.stdout.columns || 100;
    if (!gridInitialized || termCols !== lastTermCols) {
      gridRows = [];
      const palette = ['\x1b[32m','\x1b[33m','\x1b[31m','\x1b[36m','\x1b[35m','\x1b[34m','\x1b[92m','\x1b[95m'];
      if (pitchGroups.length) {
        pitchGroups.forEach(g=>{ gridRows.push({ label: g.name, glyph: g.glyph, colorCode: g.colorCode, cols: [] }); });
      } else if (hatPitchSet.size) {
        gridRows.push({ label: 'Hats', glyph: 'H', colorCode: '\x1b[35m', cols: [] });
      }
      if (!gridRows.length) {
        (report.trackSummaries||[]).slice(0,6).forEach((t,i)=>{
          let hash = 0; const idStr = String(t.id); for (let k=0;k<idStr.length;k++) hash=(hash*31+idStr.charCodeAt(k))&0xffff;
          const col = palette[i % palette.length];
          gridRows.push({ label: idStr, glyph: '•', colorCode: col, cols: [] });
        });
      }
      gridWidth = termCols - 15;
      lastTermCols = termCols;
      gridInitialized = true;
      groupBeatHits = new Array(gridRows.length).fill(false);
      groupBeatCounts = new Array(gridRows.length).fill(0);
      lastBeatBucket = Math.floor(currentBeat);
      beatTickCols = []; // reset on resize
      if (gridCsvStream) {
        const header = ['beat', ...gridRows.map(r=>r.label.replace(/[,\n]/g,'_'))].join(',');
        if (gridCsvStream.bytesWritten === 0) gridCsvStream.write(header + '\n');
      }
    }
    const beatBucket = Math.floor(currentBeat);
    // accumulate into current bucket
    for (let r=0; r<gridRows.length; r++) {
      let hit = false; let countInc = 0;
      if (pitchGroups.length) {
        const group = pitchGroups.find(g=>g.name===gridRows[r].label);
        if (group) {
          for (const n of frameNotes) if (group.set.has(String(n.pitch))) { hit = true; countInc++; }
      } } else if (gridRows[r].label === 'Hats' && hatPitchSet.size) {
        for (const n of frameNotes) if (hatPitchSet.has(String(n.pitch))) { hit = true; countInc++; }
      } else {
        for (const n of frameNotes) if (String(n.trackId) === gridRows[r].label) { hit = true; countInc++; }
      }
      if (hit) { groupBeatHits[r] = true; groupBeatCounts[r] += countInc; }
    }
    // finalize previous bucket when we move to next
    if (beatBucket !== lastBeatBucket) {
      const finalizedBeat = lastBeatBucket; // the one we just finished accumulating
      // push beat tick marker
      let tickChar = '.';
      if (finalizedBeat % 16 === 0) tickChar = '\x1b[1m|\x1b[0m';
      else if (finalizedBeat % 4 === 0) tickChar = '|';
      beatTickCols.push(tickChar);
      if (beatTickCols.length > gridWidth) beatTickCols.shift();
      // capture counts for CSV BEFORE resetting
      const csvCounts = gridRows.map((_,i)=> groupBeatHits[i] ? groupBeatCounts[i] : 0);
      // push each group cell
      for (let r=0; r<gridRows.length; r++) {
        const row = gridRows[r];
        const count = groupBeatCounts[r];
        const cell = groupBeatHits[r] ? groupIntensityGlyph(count, row, false) : ' ';
        row.cols.push(cell);
        if (row.cols.length > gridWidth) row.cols.shift();
        groupBeatHits[r] = false; groupBeatCounts[r] = 0;
      }
      if (gridCsvStream) {
        gridCsvStream.write([finalizedBeat, ...csvCounts].join(',') + '\n');
      }
      lastBeatBucket = beatBucket;
      // write csv using counts captured before reset; need to capture earlier
    }
  }

  drawScreen(locatorMsg);
  // CSV snapshot
  if (csvStream && currentBeat >= nextSnapshotBeat - 1e-6) {
    writeSnapshotRow(currentBeat);
    nextSnapshotBeat += snapshotIntervalBeats;
  }

  if (currentBeat >= endBeat || playedNotes >= totalNotes) {
    drawScreen(locatorMsg, true);
    cleanupAndExit();
  }
}

function drawScreen(locatorMsg, final=false) {
  if (useMinimalLine || useOmniLine) {
    // One-time legend header
  if (useMinimalLine && !useOmniLine && !minimalLegendPrinted && !noMinimalLegend) {
      const width = process.stdout.columns || 100;
      const sampleTracks = (report.trackSummaries||[]).slice(0,6).map(t=>{
        const idStr = String(t.id);
        return minimalTrackGlyph(idStr) + color.dim(idStr);
      }).join(' ');
      const legendParts = [];
      legendParts.push('Key');
      legendParts.push('Hat='+ '\x1b[35mH\x1b[0m');
      if (pitchGroups.length) {
        legendParts.push('Groups=' + pitchGroups.map(g=>g.colorCode+g.glyph+'\x1b[0m'+g.name).join(','));
      }
  if (useMinimalPitch) legendParts.push('PitchClass letters');
      legendParts.push('Track='+sampleTracks);
      legendParts.push('Beat markers: | (4), bold | (16)');
      const legendLine = truncate(legendParts.join('  '), width);
      process.stdout.write('\n'+color.dim(legendLine)+'\n');
      minimalLegendPrinted = true;
    }
    if (final) {
      process.stdout.write('\n'+color.green('Done.')+'\n');
    }
    if (useMinimalLine) return; // prevent full UI below
  }
  const width = process.stdout.columns || 100;
  const barWidth = Math.max(10, Math.min(40, Math.floor(width * 0.25)));
  const pct = totalNotes ? playedNotes / totalNotes : 0;
  const progressBar = renderBar(pct, barWidth);
  const etaBeatsRemaining = (totalNotes - playedNotes) / ((playedNotes / (currentBeat - startBeat + 0.00001)) || 1);
  const etaTimeSec = (function(){
    const beatsPerSec = currentBeat>startBeat ? currentBeat / (beatToSeconds(currentBeat)||1) : bpm/60;
    return (totalNotes - playedNotes) / ((playedNotes/(currentBeat-startBeat+1e-6))||1) * (60 / bpmAtBeat(currentBeat));
  })();
  const etaMin = Math.floor(etaTimeSec / 60);
  const etaSec = Math.floor(etaTimeSec % 60);
  const headerLeft = `${formatTime(currentBeat)} Beat ${currentBeat.toFixed(2)}/${endBeat.toFixed(2)}`;
  const headerMid = `${progressBar} ${(pct*100).toFixed(1)}%`;
  const headerRight = `Notes ${playedNotes}/${totalNotes} ETA ${isFinite(etaSec)?etaMin+':' + String(etaSec).padStart(2,'0'):'--:--'}`;
  const line1 = color.bold(truncate(`${headerLeft}  ${headerMid}  ${headerRight}`, width));

  // Build track lines (limit maybe 20 lines to avoid overflow)
  // Build density map for window
  const densityPerTrack = new Map();
  if (aggWindow > 0) {
    for (const ev of windowEvents) {
      densityPerTrack.set(ev.trackId, (densityPerTrack.get(ev.trackId) || 0) + 1);
    }
  }

  const trackLines = [];
  const trackLimit = showAllTracks ? Infinity : (isFinite(trackLimitFlag)?trackLimitFlag:20);
  let shown = 0;
  for (const t of report.trackSummaries || []) {
    if (shown >= trackLimit) break;
    const idStr = String(t.id);
    const act = trackActivity.get(idStr) || 0;
    const densityCount = densityPerTrack.get(idStr) || 0;
    const densityRate = aggWindow > 0 ? densityCount / aggWindow : 0;
  if (!showAllTracks && act === 0 && densityCount === 0 && !final) continue; // hide silent unless flag
    if (adaptiveDensity && densityRate > densityFull) densityFull = densityRate;
    const intensity = Math.min(1, densityRate / (densityFull||1));
    // Update history
    if (useSparklines) {
      if (!trackHistory.has(idStr)) trackHistory.set(idStr, []);
      const arr = trackHistory.get(idStr);
      arr.push(densityRate);
      if (arr.length > sparkWidth) arr.shift();
    }
    const meterLen = 20;
    const filled = Math.round(intensity * meterLen);
    const meter = colorizeBar(filled, meterLen);
    const burst = act ? color.bold(color.yellow('*'.repeat(Math.min(10, act)))) : ' ';
  const totals = trackTotals.get(idStr)||0;
  const hats = hatPitchSet.size ? (trackHatTotals.get(idStr)||0) : null;
  const tail = hats!=null ? ` ${totals}/${hats}` : ` ${totals}`;
    const spark = useSparklines ? ' ' + buildSparkline(trackHistory.get(idStr)||[], densityFull) : '';
  const nameCol = t.name ? pad(t.name,12) : ''.padEnd(12,' ');
  const line = `${idStr.padStart(4,' ')} ${pad(t.type,10)} ${nameCol} ${meter} ${burst}${tail}${spark}`;
    trackLines.push(truncate(line, width));
    shown++;
  }
  if (!final && (report.trackSummaries||[]).length > shown && trackLimit !== Infinity) {
    const remaining = (report.trackSummaries||[]).length - shown;
    trackLines.push(color.dim(`(+${remaining} more tracks hidden; use --show-all-tracks or --track-limit ${shown+remaining})`));
  }

  // Compose output
  const locatorLine = locatorMsg ? color.magenta(truncate(locatorMsg, width)) : '';
  // Aggregation summary
  const windowNoteCount = windowEvents.length;
  const hatsInWindow = hatPitchSet.size ? windowEvents.filter(ev=>hatPitchSet.has(String(ev.pitch))).length : 0;
  const densityLine = `Win(${aggWindow}b) notes=${windowNoteCount} rate=${(windowNoteCount/aggWindow).toFixed(2)}${hatPitchSet.size?` hats=${hatsInWindow}`:''} scale=${densityFull.toFixed(2)}`;
  let pitchStatsLine = '';
  if (windowPitchStatsN > 0 && windowEvents.length) {
    const freq = new Map();
    for (const ev of windowEvents) freq.set(ev.pitch, (freq.get(ev.pitch)||0)+1);
    const sorted = [...freq.entries()].sort((a,b)=>b[1]-a[1]).slice(0, windowPitchStatsN);
    pitchStatsLine = 'TopP ' + sorted.map(([p,c])=>`${p}:${c}`).join(' ');
  }

  // Recent stream
  let streamLine = '';
  if (showStream) {
    const slice = recentEvents.slice(-streamWidth);
    streamLine = slice.map(ev => pitchGlyph(ev.pitch, hatPitchSet)).join('');
    streamLine = color.dim(streamLine);
  }

  const footer = final ? color.green('Simulation complete.') : color.dim("Press 'q' to quit");

  // Clear screen and write
  process.stdout.write('\x1b[H\x1b[2J'); // home + clear
  process.stdout.write(line1 + '\n');
  // Legend (color / glyph key) – always show once per frame for clarity
  // Explains: meter colors, hat pitch glyph, sandcastle colors, burst line, star bursts
  const legend = (() => {
    const g = '\x1b[32m█\x1b[0m';
    const y = '\x1b[33m█\x1b[0m';
    const r = '\x1b[31m█\x1b[0m';
    const hat = '\x1b[35mH\x1b[0m';
    const sandG = '\x1b[42m \x1b[0m';
    const sandY = '\x1b[43m \x1b[0m';
    const sandR = '\x1b[41m \x1b[0m';
    const bursts = '\x1b[32m▃\x1b[0m\x1b[33m▆\x1b[0m\x1b[31m█\x1b[0m';
    // Key segments kept short; truncate at width
    let parts = [];
    parts.push('Key');
    parts.push(`Meter ${g}${y}${r}`);
    parts.push(`Hat ${hat}`);
    if (useSandcastle) parts.push(`Sand ${sandG}${sandY}${sandR}`);
    if (useBurstLine) parts.push(`Burst ${bursts}`);
    parts.push(`* instant count`);
    return truncate(parts.join('  '), width);
  })();
  process.stdout.write(color.dim(legend) + '\n');
  if (locatorLine) process.stdout.write(locatorLine + '\n');
  process.stdout.write(color.cyan(densityLine) + '\n');
  if (showStream) process.stdout.write(streamLine + '\n');
  if (useGroupsGrid && !useMinimalLine) {
    // Render group lanes
    if (beatTickCols.length) {
      const beatLine = 'Beats'.padEnd(12,' ') + ' ' + beatTickCols.join('') + (useGroupsGrid?'' :'');
      process.stdout.write(truncate(beatLine, width) + '\n');
    }
    for (const row of gridRows) {
      const label = row.label.padEnd(12,' ').slice(0,12);
      // live (in-progress) bucket preview (dim) if accumulating
      let liveCell = '';
      if (groupBeatCounts.length && groupBeatCounts[gridRows.indexOf(row)] !== undefined) {
        const idx = gridRows.indexOf(row);
        const count = groupBeatCounts[idx];
        if (count > 0 || groupBeatHits[idx]) {
          liveCell = color.dim(stripReset(groupIntensityGlyph(count || 1, row, false))); // dim preview
        } else {
          liveCell = ' ';
        }
      }
      const line = label + ' ' + row.cols.join('') + liveCell;
      process.stdout.write(truncate(line, width) + '\n');
    }
  }
  if (condenseTracks && trackLines.length) {
    // Build multi-column grid of track lines truncated to available width
    const termWidth = width;
    const colWidth = Math.min(38, Math.max(28, Math.floor(termWidth / Math.min(3, Math.ceil(trackLines.length/12))))); // dynamic
    const cols = Math.max(1, Math.floor(termWidth / colWidth));
    const rows = Math.ceil(trackLines.length / cols);
    for (let r=0;r<rows;r++) {
      let line = '';
      for (let c=0;c<cols;c++) {
        const idx = r + c*rows;
        if (idx < trackLines.length) {
          line += pad(trackLines[idx], colWidth);
        }
      }
      process.stdout.write(truncate(line, termWidth) + '\n');
    }
  } else {
    for (const tl of trackLines) process.stdout.write(tl + '\n');
  }
  if (pitchStatsLine) process.stdout.write(color.dim(truncate(pitchStatsLine, width)) + '\n');
  // Sandcastle render (after track lines, before footer)
  if (useSandcastle) {
    const width2 = process.stdout.columns || 100;
    if (sandCols.length !== width2) {
      // adjust to new width (simple stretch)
      const newCols = new Array(width2).fill(0);
      for (let i=0;i<width2;i++) {
        const srcIdx = sandCols.length ? Math.floor(i / width2 * sandCols.length) : 0;
        newCols[i] = sandCols[srcIdx] || 0;
      }
      sandCols = newCols;
      lastSandWidth = width2;
    }
    // Build rows from top (sandHeight-1) to 0
    let maxCol = 0; for (const h of sandCols) if (h>maxCol) maxCol = h;
    const effectiveHeight = Math.min(sandHeight, Math.max(1, maxCol));
    for (let row = effectiveHeight-1; row >= 0; row--) {
      let line = '';
      for (let c=0;c<sandCols.length;c++) {
        const h = sandCols[c];
        if (h > row) {
          const intensity = h / sandHeight;
          let colCode;
            if (intensity < 0.33) colCode = '\x1b[42m'; // green bg
            else if (intensity < 0.66) colCode = '\x1b[43m'; // yellow bg
            else colCode = '\x1b[41m'; // red bg
          line += colCode + ' ' + '\x1b[0m';
        } else {
          line += '  ';
        }
      }
      process.stdout.write(line.slice(0, width2) + '\n');
    }
  }
  if (useBurstLine) {
    const width3 = process.stdout.columns || 100;
    const slice = burstMarks.slice(-width3);
    process.stdout.write(slice.join('') + '\n');
  }
  process.stdout.write(footer + '\n');
}

function truncate(str, width) {
  if (str.length <= width) return str;
  return str.slice(0, width-1) + '…';
}
function pad(str, w) { str = String(str||''); if (str.length >= w) return str.slice(0,w); return str + ' '.repeat(w - str.length); }

function colorizeBar(filled, total) {
  let out = '';
  for (let i=0;i<total;i++) {
    if (i < filled) out += heatColor(i/total) + '█' + '\x1b[0m'; else out += ' ';
  }
  return out;
}
function heatColor(x) {
  if (x < 0.33) return '\x1b[32m'; // green
  if (x < 0.66) return '\x1b[33m'; // yellow
  return '\x1b[31m'; // red
}
function pitchGlyph(pitch, hatSet) {
  if (hatSet && hatSet.has(String(pitch))) return '\x1b[35mH\x1b[0m';
  if (pitch == null) return '.';
  const mod = Number(pitch) % 12;
  const glyphs = ['C','c','D','d','E','F','f','G','g','A','a','B'];
  return glyphs[mod] || '?';
}

function buildSparkline(values, scale) {
  if (!values.length) return ''.padStart(sparkWidth,' ');
  const chars = ' ▁▂▃▄▅▆▇█'; // 8 levels plus space
  const out = [];
  const s = scale || 1;
  for (const v of values) {
    const ratio = Math.min(1, v / (s||1));
    const idx = Math.round(ratio * (chars.length - 1));
    out.push(chars[idx]);
  }
  return out.join('').padStart(sparkWidth,' ');
}

// Intensity glyph mapping for groups grid
function groupIntensityGlyph(count, row, finalized=true) {
  // finalized bool reserved for future styling differences
  if (count < 2) return row.colorCode + row.glyph + '\x1b[0m';
  if (count < 4) return row.colorCode + '░' + '\x1b[0m';
  if (count < 8) return row.colorCode + '▒' + '\x1b[0m';
  if (count < 16) return row.colorCode + '▓' + '\x1b[0m';
  return row.colorCode + '█' + '\x1b[0m';
}
function stripReset(s){
  return s.replace(/\x1b\[[0-9;]*m/g,'');
}

function minimalTrackGlyph(tId){
  // Stable small color-coded dot based on track id hash
  let hash = 0; for (let i=0;i<tId.length;i++) hash = (hash*31 + tId.charCodeAt(i)) & 0xffff;
  const bucket = hash % 6;
  const colors = ['\x1b[32m','\x1b[33m','\x1b[31m','\x1b[36m','\x1b[35m','\x1b[34m'];
  return (colors[bucket]||'') + '•' + '\x1b[0m';
}

function colorPitchGlyph(p){
  if (p==null) return '.';
  const mod = Number(p)%12;
  const letters = ['C','c','D','d','E','F','f','G','g','A','a','B'];
  const base = letters[mod]||'?';
  // simple hue buckets by mod
  const col = ['\x1b[32m','\x1b[32m','\x1b[33m','\x1b[33m','\x1b[31m','\x1b[36m','\x1b[36m','\x1b[35m','\x1b[35m','\x1b[34m','\x1b[34m','\x1b[31m'][mod]||'';
  return col + base + '\x1b[0m';
}

// Initial draw
lastFrameTime = Date.now();
drawScreen('', false);

const interval = setInterval(stepFrame, 1000 / fps);

process.on('SIGINT', () => cleanupAndExit(0));
process.on('exit', () => { clearInterval(interval); if (csvStream) csvStream.end(); });
process.on('exit', () => { if (gridCsvStream) gridCsvStream.end(); });

// CSV snapshot utilities
if (csvStream) { writeSnapshotHeader(); writeSnapshotRow(currentBeat); }
function writeSnapshotHeader() {
  const ids = (report.trackSummaries||[]).map(t=>t.id);
  const cols = ['beat','seconds','playedNotes','playedHats', ...ids.map(id=>`track_${id}_total`), ...(hatPitchSet.size? ids.map(id=>`track_${id}_hat`):[])];
  csvStream.write(cols.join(',')+'\n');
}
function writeSnapshotRow(beat) {
  if (!csvStream) return;
  const seconds = beatToSeconds(beat).toFixed(4);
  const ids = (report.trackSummaries||[]).map(t=>t.id);
  const totals = ids.map(id=>trackTotals.get(String(id))||0);
  const hats = hatPitchSet.size? ids.map(id=>trackHatTotals.get(String(id))||0):[];
  const row = [beat.toFixed(4), seconds, playedNotes, playedHats, ...totals, ...hats];
  csvStream.write(row.join(',')+'\n');
}
