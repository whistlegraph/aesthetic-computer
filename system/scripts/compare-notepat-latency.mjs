#!/usr/bin/env node
// node system/scripts/compare-notepat-latency.mjs fast.json batched.json [--menuband-log /tmp/menuband.err]
import { readFile } from 'node:fs/promises';
import { summarizeLatency } from '../public/aesthetic.computer/lib/notepat-latency.mjs';
const args=process.argv.slice(2), reports=[];
for(let i=0;i<args.length;i++) {
  if(args[i]==='--menuband-log') {
    const log=await readFile(args[++i],'utf8');
    const matches=[...log.matchAll(/MenuBand latency: buffer=(\d+) frames \(([\d.]+) ms\) @ ([\d.]+) Hz · presentation ([\d.]+) ms/g)];
    if(!matches.length) throw Error('No Menu Band output-budget measurement in this log');
    const m=matches.at(-1);
    reports.push({app:'menuband',scope:'output budget only; not keypress latency',frames:+m[1],bufferMs:+m[2],sampleRate:+m[3],presentationMs:+m[4]});
  } else {
    const r=JSON.parse(await readFile(args[i],'utf8'));
    if(r.schema!==1 || !Array.isArray(r.samples)) throw Error('Expected a version 1 Notepat latency report');
    reports.push({file:args[i],app:r.app,mode:r.mode,attempted:r.attempted,missing:r.missing,
      stallMs:r.stallMs,synthetic:r.synthetic,summary:summarizeLatency(r.samples),
      sampleRates:[...new Set(r.samples.map(s=>s.sampleRate))]});
  }
}
if(!reports.length) throw Error('Pass benchmark JSON files and optionally --menuband-log PATH');
console.log(JSON.stringify({reports,comparison:'Compare identical output routes, rates, instruments, viewport and load. Estimated output timing excludes physical key travel and acoustic propagation. Menu Band buffer/presentation values are separate budgets, not added or treated as measured keypress latency.'},null,2));
if(reports.some(r=>r.app==='notepat' && (r.missing || !r.summary.inputToBridgeMs.count))) process.exitCode=1;
