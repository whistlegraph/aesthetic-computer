#!/usr/bin/env python3
"""Select alternate how/was/blind attacks without replacing the second line."""
import json
from pathlib import Path
import subprocess
import sys
import numpy as np
import soundfile as sf
base, alternate, out = map(Path, sys.argv[1:4])
out.mkdir(parents=True,exist_ok=True)
r=json.loads((base/'arrangement-receipt.json').read_text())
source=Path(r['source'])
words=json.loads((source/'vox-receipt.json').read_text())['words']
sr=48000
verse=np.zeros(sf.info(source/'vox.wav').frames)
for w in words:
    i=w['i']
    p=alternate/f'{i:02d}-sung.wav' if i in [2,20,21] else (
        base/'lead-but-repaired.wav' if i==16 else next((source/'sung').glob(f'{i:02d}-*.wav')))
    y,rate=sf.read(p); assert rate==sr
    at=round(w['start']*sr)
    verse[at:at+len(y)]+=y
verse*=10**(-3/20)/np.abs(verse).max()
full=np.zeros(sf.info(base/'lead-centered.wav').frames)
for offset,gain in [(6,1),(r['secondPassStart'],.98)]:
    at=round(offset*sr); full[at:at+len(verse)]+=verse*gain
sf.write(out/'lead-raw.wav',full,sr,subtype='FLOAT')
subprocess.run(['ffmpeg','-y','-v','error','-i',str(out/'lead-raw.wav'),'-af',
 'highpass=f=85,equalizer=f=280:t=q:w=.8:g=-1.3,acompressor=threshold=.14:ratio=2:attack=18:release=180:knee=3:makeup=1.12',
 '-c:a','pcm_f32le',str(out/'lead-centered.wav')],check=True)
(out/'source-selection.json').write_text(json.dumps({'base':str(base),'alternate':str(alternate),
 'alternateWordIndices':[2,20,21],'originalSecondLinePreserved':True},indent=2)+'\n')
