#!/usr/bin/env node
/**
 * Ableton Live XML Project Analyzer
 *
 * Goals:
 *  - Parse a large extracted Ableton .als XML (e.g. zzzZWAP_extracted.xml)
 *  - Compare against a blank template project XML
 *  - Extract timeline-relevant entities for rebuilding a visual timeline:
 *      * Tracks (type, id, name, color, group membership)
 *      * Clips (name, type, track, start/end/loop times, warp markers if present)
 *      * Devices on tracks (type, on/off state, user name)
 *      * Automation envelopes & tempo / time signature changes
 *      * Locators / cue points
 *      * Scenes (index, name if any)
 *  - Produce:
 *      * Structured JSON summary (optionally saved to a file)
 *      * Human-readable markdown-ish report printed to stdout
 *  - Designed to be memory efficient (streaming SAX parse, not full DOM)
 *
 * Usage (from reference/ directory):
 *   node analyze-ableton.mjs \
 *     --project ../system/public/assets/wipppps/zzzZWAP_extracted.xml \
 *     --blank ./live-12-blank.xml \
 *     --out report.json
 *
 * All flags optional; defaults:
 *   --project: env ABLETON_PROJECT or zzzZWAP_extracted.xml
 *   --blank:   live-12-blank.xml
 *
 * Implementation notes:
 *  - We use saxes (SAX parser) to stream parse.
 *  - We maintain a stack of element names for path context (array join('/')).
 *  - We detect clips by element names ending with 'Clip' (excluding 'ClipSlot').
 *  - Timing fields often are empty-element tags with attribute Value="..."; we watch for known time-ish keys.
 *  - We gather attribute Value and child <Value Value="..."/> patterns generically.
 *  - We build a baseline of tag counts from the blank XML to compute diffs.
 */

import { createReadStream, promises as fs } from 'node:fs';
import { basename, resolve } from 'node:path';
import { performance } from 'node:perf_hooks';
import { SaxesParser } from 'saxes';
import chalk from 'chalk';

// ---------- CLI ARG PARSING ----------
const args = process.argv.slice(2);
function getFlag(name, def = undefined) {
  const idx = args.indexOf(`--${name}`);
  if (idx !== -1) return args[idx + 1];
  return def;
}

const projectPath = resolve(getFlag('project', process.env.ABLETON_PROJECT || '../system/public/assets/wipppps/zzzZWAP_extracted.xml'));
const blankPath = resolve(getFlag('blank', './live-12-blank.xml'));
const outPath = getFlag('out');
const outMarkdown = args.includes('--markdown') || getFlag('markdown');
const notesOutPath = getFlag('notes-out'); // optional separate notes export

console.error(chalk.cyan(`Analyzing Ableton project:\n  Target: ${projectPath}\n  Blank : ${blankPath}`));

// ---------- UTILITIES ----------
const timeLikeKeys = new Set([
  'CurrentStart','CurrentEnd','StartMarker','EndMarker','LoopStart','LoopEnd','LoopLength','StartRelative','Time','Length','Start','End'
]);

function isClipElement(name) {
  return /Clip$/.test(name) && name !== 'ClipSlot';
}
function isDeviceElement(name) {
  return /(Device|Instrument|Effect|PlugInDevice)$/.test(name) || name === 'Device';
}
function niceNum(n) { return (typeof n === 'number' && !Number.isNaN(n)) ? Number(n.toFixed(6)) : n; }

// Streaming parse harness
async function parseAbletonXML(path, options={ mode:'project' }) {
  const start = performance.now();
  const parser = new SaxesParser({ xmlns: false, fragment: false });
  const stack = []; // element name stack
  const state = {
    tagCounts: Object.create(null),
    paths: new Set(),
    tracks: [],
    byTrackId: new Map(),
    currentTrack: null,
    clips: [],
    currentClip: null,
    devices: [],
    currentDevice: null,
    scenes: [],
    currentScene: null,
    tempoChanges: [],
    timeSigChanges: [],
    automationCount: 0,
    errors: [],
    totalElements: 0,
    file: path,
    notesCaptured: 0,
    warpMarkers: [],
    locators: [],
  currentKeyTrack: null,
  pitchCounts: {},
  };

  function pushTrack(elName, attrs) {
    const id = attrs.Id ?? attrs.ID ?? attrs.id;
    const track = { id, type: elName, name: null, color: null, groupId: null, devices: [], clips: [] };
    state.tracks.push(track);
    if (id) state.byTrackId.set(id, track);
    state.currentTrack = track;
  }
  function finalizeTrack(elName) {
    if (state.currentTrack && state.currentTrack.type === elName) {
      state.currentTrack = null;
    }
  }
  function pushClip(elName, attrs) {
    const clip = { type: elName, trackId: state.currentTrack?.id ?? null, name: null, times: {}, notes: [], rawAttrs: { ...attrs } };
    state.clips.push(clip);
    state.currentClip = clip;
    if (state.currentTrack) state.currentTrack.clips.push(clip);
  }
  function finalizeClip(elName) {
    if (state.currentClip && state.currentClip.type === elName) {
      state.currentClip = null;
    }
  }
  function pushDevice(elName, attrs) {
    const device = { type: elName, trackId: state.currentTrack?.id ?? null, name: null, on: null, rawAttrs: { ...attrs }, params: {} };
    state.devices.push(device);
    state.currentDevice = device;
    if (state.currentTrack) state.currentTrack.devices.push(device);
  }
  function finalizeDevice(elName) {
    if (state.currentDevice && state.currentDevice.type === elName) {
      state.currentDevice = null;
    }
  }

  parser.on('error', e => {
    state.errors.push(e.message);
  });
  parser.on('opentag', node => {
    const name = node.name;
    const attrs = Object.fromEntries(Object.entries(node.attributes).map(([k,v]) => [k, v.value ?? v]));
    stack.push(name);
    state.totalElements++;
    state.tagCounts[name] = (state.tagCounts[name] || 0) + 1;
    if (state.totalElements % 50000 === 0) {
      process.stderr.write(chalk.gray(`.. ${state.totalElements} elements\n`));
    }
    const pathStr = stack.join('/');
    state.paths.add(pathStr);

    if (/^(MidiTrack|AudioTrack|ReturnTrack|GroupTrack|MainTrack|PreHearTrack)$/.test(name)) {
      if (options.mode === 'project') pushTrack(name, attrs);
    }
    else if (isClipElement(name) && options.mode === 'project') {
      pushClip(name, attrs);
    }
    else if (isDeviceElement(name) && options.mode === 'project') {
      pushDevice(name, attrs);
    }
    // Enter KeyTrack context
    if (name === 'KeyTrack') {
      state.currentKeyTrack = { pendingNoteIndexes: [], pitch: undefined };
    }

    if (attrs.Value !== undefined) {
      const parent = stack[stack.length - 2];
      const key = name;
      if (state.currentClip && timeLikeKeys.has(key)) {
        const num = Number(attrs.Value);
        if (!Number.isNaN(num)) state.currentClip.times[key] = num;
      }
      if (state.currentTrack && key === 'Color') {
        state.currentTrack.color = attrs.Value;
      }
      if (state.currentTrack && key === 'TrackGroupId') {
        state.currentTrack.groupId = attrs.Value;
      }
      if (state.currentDevice && parent === 'On' && key === 'Manual') {
        state.currentDevice.on = attrs.Value === 'true';
      }
      if (key === 'Tempo' || key === 'TempoAutomationTarget') {
        const num = Number(attrs.Value);
        if (!Number.isNaN(num)) state.tempoChanges.push({ path: pathStr, tempo: num });
      }
      if (key === 'TimeSignature') {
        state.timeSigChanges.push({ path: pathStr, value: attrs.Value });
      }
      // Locator child attributes
      if (parent === 'Locator' && key === 'Time') {
        const loc = state.locators[state.locators.length - 1];
        if (loc) loc.time = Number(attrs.Value);
      }
      if (parent === 'Locator' && key === 'Name') {
        const loc = state.locators[state.locators.length - 1];
        if (loc) loc.name = attrs.Value;
      }
      // MidiKey inside KeyTrack defines pitch for all contained notes (often appears after notes list)
      if (state.currentKeyTrack && key === 'MidiKey') {
        const pitchVal = Number(attrs.Value);
        if (!Number.isNaN(pitchVal)) {
          state.currentKeyTrack.pitch = pitchVal;
          // Backfill pending notes without pitch
            for (const idx of state.currentKeyTrack.pendingNoteIndexes) {
              const note = state.currentClip?.notes[idx];
              if (note && note.pitch == null) {
                note.pitch = pitchVal;
                state.pitchCounts[pitchVal] = (state.pitchCounts[pitchVal]||0)+1;
              }
            }
          state.currentKeyTrack.pendingNoteIndexes = [];
        }
      }
    }

    // Midi notes (leaf empty elements) inside clips
    if (state.currentClip && name === 'MidiNoteEvent') {
      const note = {
        time: attrs.Time !== undefined ? Number(attrs.Time) : undefined,
        duration: attrs.Duration !== undefined ? Number(attrs.Duration) : undefined,
        velocity: attrs.Velocity !== undefined ? Number(attrs.Velocity) : undefined,
        offVelocity: attrs.OffVelocity !== undefined ? Number(attrs.OffVelocity) : undefined,
        noteId: attrs.NoteId !== undefined ? Number(attrs.NoteId) : undefined,
        pitch: undefined,
      };
      if (!Number.isNaN(note.time)) {
        state.currentClip.notes.push(note);
        // Defer pitch assignment until MidiKey encountered
        if (state.currentKeyTrack) {
          state.currentKeyTrack.pendingNoteIndexes.push(state.currentClip.notes.length - 1);
        }
        state.notesCaptured++;
      }
    }
    // Warp markers inside AudioClip context
    if (state.currentClip && name === 'WarpMarker') {
      state.warpMarkers.push({
        clipIndex: state.clips.length - 1,
        secTime: attrs.SecTime !== undefined ? Number(attrs.SecTime) : undefined,
        beatTime: attrs.BeatTime !== undefined ? Number(attrs.BeatTime) : undefined,
      });
    }
    // Locators at arrangement level
    if (name === 'Locator') {
      state.locators.push({ id: attrs.Id || attrs.ID, time: undefined, name: undefined });
    }
  });
  let currentText = '';
  parser.on('text', txt => { currentText += txt; });
  parser.on('closetag', name => {
    const text = currentText.trim();
    currentText = '';
    if (state.currentClip && /^(EffectiveName|UserName)$/.test(name) && text) {
      state.currentClip.name = state.currentClip.name || text;
    }
    if (state.currentDevice && /^(UserName|EffectiveName)$/.test(name) && text) {
      state.currentDevice.name = state.currentDevice.name || text;
    }
    if (state.currentTrack && /^(EffectiveName|UserName)$/.test(name) && text) {
      state.currentTrack.name = state.currentTrack.name || text;
    }
    if (isClipElement(name)) finalizeClip(name);
    else if (isDeviceElement(name)) finalizeDevice(name);
    else if (/^(MidiTrack|AudioTrack|ReturnTrack|GroupTrack|MainTrack|PreHearTrack)$/.test(name)) finalizeTrack(name);
    else if (name === 'KeyTrack') {
      // If MidiKey not provided after notes, pitch remains undefined; optionally could look for other hints.
      state.currentKeyTrack = null;
    }
    stack.pop();
  });

  await new Promise((resolvePromise, reject) => {
    const rs = createReadStream(path, { encoding: 'utf8' });
    rs.on('data', chunk => {
      try { parser.write(chunk); } catch (e) { reject(e); }
    });
    rs.on('error', reject);
    rs.on('end', () => {
      try { parser.close(); } catch (e) { return reject(e); }
      resolvePromise();
    });
  });

  const elapsed = performance.now() - start;
  state.elapsedMs = elapsed;
  return state;
}

// ---------- MAIN FLOW ----------
async function main() {
  const blank = await parseAbletonXML(blankPath, { mode: 'blank' });
  const project = await parseAbletonXML(projectPath, { mode: 'project' });

  const diffCounts = {};
  for (const [tag, count] of Object.entries(project.tagCounts)) {
    const base = blank.tagCounts[tag] || 0;
    const delta = count - base;
    if (delta !== 0) diffCounts[tag] = { project: count, blank: base, delta };
  }
  const stats = {
    projectFile: project.file,
    blankFile: blank.file,
    parsing: {
      projectMs: Math.round(project.elapsedMs),
      blankMs: Math.round(blank.elapsedMs),
      totalElementsProject: project.totalElements,
      totalElementsBlank: blank.totalElements,
    },
    tracks: project.tracks.length,
    clips: project.clips.length,
    devices: project.devices.length,
    tempoEvents: project.tempoChanges.length,
    timeSignatureEvents: project.timeSigChanges.length,
  notes: project.notesCaptured,
  warpMarkers: project.warpMarkers.length,
  locators: project.locators.length,
  };

  const trackSummaries = project.tracks.map(t => ({
    id: t.id,
    type: t.type,
    name: t.name,
    color: t.color,
    groupId: t.groupId,
    devices: t.devices.map(d => ({ type: d.type, name: d.name, on: d.on })),
    clipCount: t.clips.length,
    clips: t.clips.slice(0, 50).map(c => ({ name: c.name, times: c.times }))
  }));

  const timeline = project.clips.map((c, idx) => ({ index: idx, trackId: c.trackId, name: c.name, times: c.times, noteCount: c.notes.length }));

  // Build full notes list if requested (can be large). Each note augmented with clip + absolute beat time.
  let allNotes = null;
  if (notesOutPath) {
    allNotes = [];
    project.clips.forEach((clip, clipIndex) => {
      const clipStart = clip.times.CurrentStart || 0; // beat-based
      clip.notes.forEach(n => {
        const absBeat = (clipStart || 0) + (n.time || 0);
        allNotes.push({
          clipIndex,
          trackId: clip.trackId,
            clipName: clip.name,
          beat: absBeat,
          relBeat: n.time,
          duration: n.duration,
          velocity: n.velocity,
          pitch: n.pitch
        });
      });
    });
    // Sort by absolute beat
    allNotes.sort((a,b)=>a.beat-b.beat);
  }

  const result = {
    stats,
    trackSummaries,
    timeline,
    tempoChanges: project.tempoChanges,
    timeSignatureChanges: project.timeSigChanges,
    locators: project.locators,
    warpMarkersSample: project.warpMarkers.slice(0,50),
    tagDiffCounts: diffCounts,
    errors: { project: project.errors, blank: blank.errors }
  };

  if (outPath) {
    await fs.writeFile(outPath, JSON.stringify(result, null, 2), 'utf8');
    console.error(chalk.green(`Wrote JSON report: ${outPath}`));
  }
  if (notesOutPath && allNotes) {
    await fs.writeFile(notesOutPath, JSON.stringify(allNotes, null, 2), 'utf8');
    console.error(chalk.green(`Wrote notes list: ${notesOutPath} (notes=${allNotes.length})`));
  }

  console.log('\n=== Ableton Project Analysis Report ===');
  console.log(chalk.bold('Files:'));
  console.log(`  Project: ${basename(projectPath)}`);
  console.log(`  Blank  : ${basename(blankPath)}`);
  console.log('\nStats:');
  for (const [k,v] of Object.entries(stats)) {
    if (typeof v === 'object') continue;
    console.log(`  ${k}: ${v}`);
  }
  console.log('  parse(project) ms:', stats.parsing.projectMs, 'elements:', stats.parsing.totalElementsProject);
  console.log('  parse(blank)   ms:', stats.parsing.blankMs, 'elements:', stats.parsing.totalElementsBlank);

  console.log('\nTracks (first 10):');
  for (const t of trackSummaries.slice(0,10)) {
    console.log(`  [${t.id}] ${t.type} ${t.name || ''} clips=${t.clipCount} devices=${t.devices.length}`);
  }

  console.log('\nTop Tag Deltas (by absolute delta, top 25):');
  const topTagDeltas = Object.entries(diffCounts)
    .sort((a,b)=>Math.abs(b[1].delta)-Math.abs(a[1].delta))
    .slice(0,25);
  for (const [tag, info] of topTagDeltas) {
    console.log(`  ${tag}: +${info.delta} (project ${info.project} vs blank ${info.blank})`);
  }

  console.log('\nSample Timeline Entries (first 25):');
  for (const c of timeline.slice(0,25)) {
    const times = Object.entries(c.times).map(([k,v])=>`${k}=${niceNum(v)}`).join(' ');
    console.log(`  Clip#${c.index} Track ${c.trackId} :: ${c.name || '(unnamed)'} :: notes=${c.noteCount} :: ${times}`);
  }

  console.log('\nLocators (first 10):');
  for (const l of project.locators.slice(0,10)) {
    console.log(`  Locator ${l.id} @ ${l.time} :: ${l.name}`);
  }

  console.log('\nUse --out report.json to capture full JSON including all clips.');
  console.log('Add --markdown to emit ABLETON_TIMELINE_GUIDE.md with schema + samples.');

  // Pitch histogram (top 20)
  const pitchEntries = Object.entries(project.pitchCounts).sort((a,b)=>b[1]-a[1]).slice(0,20);
  if (pitchEntries.length) {
    console.log('\nTop Note Pitches (count):');
    for (const [p,c] of pitchEntries) console.log(`  ${p}: ${c}`);
  }

  if (outMarkdown) {
    const md = [];
    md.push('# Ableton Live Project Structural Report');
    md.push('');
    md.push('Generated by analyze-ableton.mjs');
    md.push('');
    md.push('## Summary Stats');
    Object.entries(result.stats).forEach(([k,v])=> md.push(`- ${k}: ${v}`));
    md.push('\n## Locators');
    if (result.locators.length === 0) md.push('*None*');
    result.locators.forEach(l=> md.push(`- ${l.id}@${l.time} : ${l.name}`));
    md.push('\n## Tracks (first 10)');
    trackSummaries.slice(0,10).forEach(t=> md.push(`- [${t.id}] ${t.type} ${t.name||''} clips=${t.clipCount} devices=${t.devices.length}`));
    md.push('\n## Sample Notes (first 40 across clips)');
  const sampleNotes = [];
    for (const clip of project.clips) {
      for (const note of clip.notes) {
    sampleNotes.push({ clipTrack: clip.trackId, time: note.time, dur: note.duration, vel: note.velocity, pitch: note.pitch });
        if (sampleNotes.length>=40) break;
      }
      if (sampleNotes.length>=40) break;
    }
    if (sampleNotes.length === 0) md.push('*No notes captured*');
  sampleNotes.forEach(n=> md.push(`- Track ${n.clipTrack} t=${n.time} dur=${n.dur} vel=${n.vel} pitch=${n.pitch ?? ''}`));
    md.push('\n## Warp Markers (sample 50)');
    if (result.warpMarkersSample.length === 0) md.push('*No warp markers*');
    result.warpMarkersSample.forEach((m)=> md.push(`- ClipIdx ${m.clipIndex} beat=${m.beatTime} sec=${m.secTime}`));
    md.push('\n## Top Tag Deltas');
    Object.entries(diffCounts).sort((a,b)=>Math.abs(b[1].delta)-Math.abs(a[1].delta)).slice(0,25).forEach(([tag,info])=> md.push(`- ${tag}: +${info.delta}`));
    md.push('\n## Timeline JSON Shape');
    md.push('Each timeline entry:');
    md.push('```json');
    md.push(JSON.stringify({ index:0, trackId:'<track-id>', name:'Clip Name', times:{ CurrentStart:0, CurrentEnd:8, LoopStart:0, LoopEnd:8 }, noteCount: 12 }, null, 2));
    md.push('```');
    md.push('\n### Clip Object (internal)');
    md.push('```json');
    md.push(JSON.stringify({ type:'AudioClip|MidiClip', trackId:'61', name:'', times:{}, notes:[{time:0, duration:0.25, velocity:100}], rawAttrs:{/* original XML attrs */} }, null, 2));
    md.push('```');
  md.push('\n## Pitch Histogram (top 20)');
  if (pitchEntries.length === 0) md.push('*No pitches*');
  pitchEntries.forEach(([p,c])=> md.push(`- ${p}: ${c}`));
    md.push('\n### Locator Object');
    md.push('```json');
    md.push(JSON.stringify({ id:'3', time:32, name:'DROP' }, null, 2));
    md.push('```');
    md.push('\n### Warp Marker Object');
    md.push('```json');
    md.push(JSON.stringify({ clipIndex:5, beatTime:0, secTime:0 }, null, 2));
    md.push('```');
    md.push('\nUse these shapes to drive a timeline visualization:');
    md.push('- Horizontal axis: beat or absolute time (choose one; beatTime from warp markers or clip start times)');
    md.push('- Rows: tracks (group by trackSummaries)');
    md.push('- Clip bars: from times.CurrentStart to times.CurrentEnd (or use LoopStart/LoopEnd for loop overlay)');
    md.push('- Notes: render inside MIDI clips using note.time and note.duration relative to clip start');
    md.push('- Locators: vertical lines at locator.time with labels');
    md.push('- Warp markers: micro markers inside audio clips to show warping');
    await fs.writeFile('ABLETON_TIMELINE_GUIDE.md', md.join('\n'), 'utf8');
    console.error(chalk.green('Wrote ABLETON_TIMELINE_GUIDE.md'));
  }
}

main().catch(err => {
  console.error(chalk.red('Analysis failed:'), err);
  process.exit(1);
});
