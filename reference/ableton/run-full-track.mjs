#!/usr/bin/env node
/**
 * One-shot: parse project XML then run simulation in near real-time with sensible defaults.
 * Usage:
 *   node run-full-track.mjs [--project <xml>] [--bpm 144] [--rate 1] [--groups <spec>] [--hat-pitches a,b]
 * Defaults:
 *   project: ../system/public/assets/wipppps/zzzZWAP_extracted.xml
 *   rate: 1 (real-time)
 *   fps: 20
 *   aggregate-window: 8
 *   auto groups: top 2 pitches if --groups omitted & groups-grid is used
 */
import { spawn } from 'node:child_process';
import { resolve, dirname } from 'node:path';
import { fileURLToPath } from 'node:url';
import fs from 'node:fs';

const __dirname = dirname(fileURLToPath(import.meta.url));

const args = process.argv.slice(2);
function getFlag(name, def){ const i=args.indexOf('--'+name); return i!==-1? args[i+1]: def; }
const has = n => args.includes('--'+n);

const project = resolve(getFlag('project','../system/public/assets/wipppps/zzzZWAP_extracted.xml'));
const bpm = getFlag('bpm');
const rate = getFlag('rate','1');
const fps = getFlag('fps','20');
const agg = getFlag('aggregate-window','8');
const hat = getFlag('hat-pitches','');
let groupsSpec = getFlag('groups');
const wantGrid = true; // always show groups grid for overview

// If groups not provided, we will quick-scan pitches from analyzer JSON (or generate one) to pick top 2.
async function ensureReportAndNotes(){
  if (!fs.existsSync('report.json') || !fs.existsSync('notes.json')) {
    await new Promise((res,rej)=>{
      const a = spawn('node', ['analyze-ableton.mjs','--project', project,'--blank','./live-12-blank.xml','--out','report.json','--notes-out','notes.json'], { cwd: __dirname, stdio:'inherit' });
      a.on('exit', c=> c===0?res():rej(new Error('analyze failed')));
    });
  }
}

function deriveGroups(){
  if (groupsSpec) return;
  try {
  const notesPath = resolve(__dirname,'notes.json');
  const notes = JSON.parse(fs.readFileSync(notesPath,'utf8'));
    const freq = new Map();
    for (const n of notes) { if (n.pitch!=null) freq.set(n.pitch,(freq.get(n.pitch)||0)+1); }
    const top = [...freq.entries()].sort((a,b)=>b[1]-a[1]).slice(0,2);
    if (top.length) {
      groupsSpec = top.map(([p],i)=>`P${p}:${p}:${i===0?'P':'Q'}`).join(';');
      console.error('[run-full] Auto groups:', groupsSpec);
    }
  } catch(e){ /* ignore */ }
}

(async () => {
  await ensureReportAndNotes();
  deriveGroups();
  const simArgs = ['simulate-ableton.mjs','--auto','--rate',rate,'--fps',fps,'--aggregate-window',agg];
  if (bpm) simArgs.push('--bpm', bpm);
  if (hat) simArgs.push('--hat-pitches', hat);
  if (groupsSpec) simArgs.push('--groups-grid','--groups', groupsSpec);
  else simArgs.push('--groups-grid');
  const child = spawn('node', simArgs, { cwd: __dirname, stdio:'inherit' });
  child.on('exit', code => process.exit(code));
})();
