from pathlib import Path
import csv,json,statistics,math,hashlib
root=Path(__file__).parent
score=json.loads(Path('fedac/native/candidates/notespatial-2026-09-24/notespatial-echo-flange.nsscore').read_text())
prior=json.loads((root/'prior-levels.json').read_text())
rows=list(csv.DictReader((root/'measurements.csv').open()));result=[]
db=lambda x:20*math.log10(max(1e-12,x))
for gm in range(128):
 rs=[r for r in rows if int(r['program'])==gm];trim=score['gmGains'][gm]
 levels=sorted(float(r['rms100'])*trim for r in rs);peak=max(float(r['peak'])*trim for r in rs)
 result.append({'program':gm,'name':prior['programs'][gm]['name'],'tests':len(rs),'trim':trim,'medianDb':db(statistics.median(levels)),'maxDb':db(max(levels)),'peakDb':db(peak),'spreadDb':db(max(levels)/min(levels))})
reference=statistics.median(r['medianDb'] for r in result)
for r in result:r['aboveMedianDb']=r['medianDb']-reference
result.sort(key=lambda r:r['aboveMedianDb'],reverse=True)
(root/'summary.json').write_text(json.dumps({'referenceDb':reference,'programs':result},indent=2)+'\n')
lines=['# GM instrument balance measurement','',f'{len(rows)} offline note renders; all128 programs, two seeds, 0.35s and1.5s notes, 10ms attack/120ms release, 44.1kHz. Each program uses its score pitch-range quartiles plus C4. Strongest100ms RMS and sample peaks are measured before effects, mastering, routing and speakers. Existing score GM trims are applied to the reported values.','',f'Median program reference: {reference:.2f}dBFS. These are electrical RMS measurements, not LUFS or room loudness; they do not prove which patch the listener heard.','', '| GM (1-based) | Instrument | Above median | Pitch/envelope spread | Max peak |','|---|---|---:|---:|---:|']
for r in result[:20]:lines.append(f"| {r['program']+1} | {r['name']} | {r['aboveMedianDb']:+.1f}dB | {r['spreadDb']:.1f}dB | {r['peakDb']:.1f}dBFS |")
core=Path('fedac/native/src/gm_synth.c').read_bytes();local=core
lines+=['',f'Synth SHA256: `{hashlib.sha256(core).hexdigest()}`. Matches local source: {core==local}. Prior calibration source hash: `{prior.get("coreHash")}`. The installed OS binary/source match was not independently verified.','', 'No new trims deployed. A single trim per patch does not remove register/envelope variation; next audition should target the outliers at their loudest measured pitches. Positive peaks here are unit-input diagnostics, not evidence of output clipping: actual note gain, spatial gain, master gain and processing follow this stage.','', 'Reproduce from repository root:','```sh','cc -O2 -I fedac/native/src fedac/native/docs/performance/gm-balance-2026-09-24/measure.c fedac/native/src/gm_synth.c -lm -o /tmp/gm-measure','/tmp/gm-measure < fedac/native/docs/performance/gm-balance-2026-09-24/input.txt > /tmp/gm-measurements.csv','```']
(root/'README.md').write_text('\n'.join(lines)+'\n')
print('\n'.join(lines[4:17]))
