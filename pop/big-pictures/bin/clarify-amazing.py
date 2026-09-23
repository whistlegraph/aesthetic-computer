#!/usr/bin/env python3
"""Alternate word sources for the lead; evaluate blind before adopting."""
import json
from pathlib import Path
import subprocess
import sys
import numpy as np
import soundfile as sf
REPO=Path(__file__).resolve().parents[3]
SOURCE,OUT=[Path(p).resolve() for p in sys.argv[1:3]]
OUT.mkdir(parents=True,exist_ok=True)
SR=48000; SPB=60/70
words=json.loads((SOURCE/'vox-receipt.json').read_text())['words']
arch=REPO/'system/public/assets/pop/big-pictures'
v1=json.loads((arch/'amazing/vocal/amazing-vocal-words.json').read_text())
changes={2,6,7,8,9,14,20,21}
reports=[]
def run(args):
    return subprocess.run(list(map(str,args)),check=True,capture_output=True,text=True).stdout.strip()
def read(p):
    y,sr=sf.read(p); assert sr==SR; return y
lead=np.zeros(round((64*SPB+1.5)*SR))
for w in words:
    if w['i'] in changes:
        i=w['i']; clip=OUT/f'{i:02d}-spoken.wav'; dest=OUT/f'{i:02d}-sung.wav'
        if i==8:
            audio=arch/'amazing-7verse/vocal/amazing-7verse-vocal.mp3'
            start,end=39.06,39.15
        else:
            index=22 if i==16 else i
            cur=v1[index]; prev=v1[index-1]; nxt=v1[index+1]
            start=(cur['fromMs']-min(25,(cur['fromMs']-prev['toMs'])/2))/1000
            end=(cur['toMs']+min(40,(nxt['fromMs']-cur['toMs'])/2))/1000
            audio=arch/'amazing/vocal/amazing-vocal.mp3'
        run(['ffmpeg','-y','-v','error','-ss',start,'-t',end-start,'-i',audio,'-ac','1','-ar',SR,'-c:a','pcm_f32le',clip])
        report=run([sys.executable,'-W','ignore',REPO/'pop/cult/bin/sing.py',clip,dest,
                   '--notes',w['spec'],'--f0-floor','65','--f0-ceil','520',
                   '--vibrato-hz','5.2','--vibrato-cents','12','--vibrato-onset-ms','420',
                   '--overshoot-cents','8','--formant-db','1.5','--attack-ms','6',
                   '--release-ms','110' if w['text'] in ['sound','me','found','see'] else '55',
                   '--xfade-ms','90','--shimmer-frames','.25','--deess','.05','--gain','.9','--verify'])
        reports.append({'wordIndex':i,'word':w['text'],'source':str(audio),'from':start,'to':end,'report':report})
        print(w['text'],report,flush=True)
        y=read(dest)
    elif w['i']==16:
        y=read(SOURCE.parent/'amazing-grace-harmonized-2026-09-23'/'lead-but-repaired.wav')
    else:
        y=read(list((SOURCE/'sung').glob(f"{w['i']:02d}-*.wav"))[0])
    at=round(w['start']*SR); lead[at:at+len(y)]+=y
lead*=10**(-3/20)/np.abs(lead).max()
sf.write(OUT/'lead-clear-verse.wav',lead,SR,subtype='FLOAT')
# Match the original C-bed length precisely.
harmony=SOURCE.parent/'amazing-grace-harmonized-2026-09-23'
n=len(read(harmony/'lead-centered.wav'))
full=np.zeros(n)
for offset,gain in [(6,1),(6+68*SPB,.98)]:
    at=round(offset*SR); full[at:at+len(lead)]+=lead*gain
sf.write(OUT/'lead-clear-raw.wav',full,SR,subtype='FLOAT')
run(['ffmpeg','-y','-v','error','-i',OUT/'lead-clear-raw.wav','-af',
     'highpass=f=85,equalizer=f=280:t=q:w=.8:g=-1.3,acompressor=threshold=.14:ratio=2:attack=18:release=180:knee=3:makeup=1.12',
     '-c:a','pcm_f32le',OUT/'lead-centered.wav'])
(OUT/'clarity-source-receipt.json').write_text(json.dumps({'changes':reports,'status':'candidate; blind transcription pending','lyricVariant':'now I am found / now I’m found from existing source'},indent=2)+'\n')
