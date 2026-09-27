#!/usr/bin/env python3
"""Blind local Whisper lyric check, following macneopolitan/bin/hear.mjs.
Usage: python hear-amazing.py AUDIO MODEL OUT_JSON
No lyric prompt or forced alignment; reference text is used only after decoding.
"""
import hashlib
import json
from pathlib import Path
import re
import subprocess
import sys
import tempfile

audio, model, out = map(Path, sys.argv[1:4])
phrases = [(6,21.429,'amazing grace how sweet the sound'),
           (21.429,33.429,'that saved a wretch like me'),
           (33.429,48.857,'i once was lost but now am found'),
           (48.857,60.857,'was blind but now i see')]

def norm(s):
    s=re.sub(r"\bi'm\b",'i am',s.lower())
    return re.findall(r'[a-z]+',s)

def wer(ref,hyp):
    a,b=norm(ref),norm(hyp)
    d=list(range(len(b)+1))
    for i,x in enumerate(a,1):
        row=[i]
        for j,y in enumerate(b,1):
            row.append(min(row[-1]+1,d[j]+1,d[j-1]+(x!=y)))
        d=row
    return d[-1],len(a)

rows=[]
duration=float(subprocess.check_output(['ffprobe','-v','error','-show_entries','format=duration','-of','csv=p=0',str(audio)],text=True))
with tempfile.TemporaryDirectory(prefix='hear-amazing-') as tmp:
    for verse in range(2 if duration>100 else 1):
        offset=verse*68*60/70
        for index,(start,end,expected) in enumerate(phrases):
            wav=Path(tmp)/f'{verse}-{index}.wav'
            subprocess.run(['ffmpeg','-y','-v','error','-ss',str(start+offset-.15),'-t',str(end-start+.15),
                '-i',str(audio),'-ar','16000','-ac','1',str(wav)],check=True)
            r=subprocess.run(['whisper-cli','-m',str(model),'-f',str(wav),'-l','en','-nt','-np','-t','2'],
                capture_output=True,text=True,check=True)
            heard=re.sub(r'\[[^\]]*\]',' ',r.stdout)
            heard=' '.join(heard.split())
            errors,words=wer(expected,heard)
            row={'verse':verse+1,'line':index+1,'start':start+offset,'expected':expected,
                 'heard':heard,'errors':errors,'words':words,'wer':errors/words}
            rows.append(row)
            print(json.dumps(row),flush=True)
errors=sum(x['errors'] for x in rows); words=sum(x['words'] for x in rows)
report={'model':model.name,'audio':str(audio),'sha256':hashlib.sha256(audio.read_bytes()).hexdigest(),
        'method':'Blind local Whisper per sung line; no lyric prompt or forced alignment',
        'limits':'ASR is a proxy, not a listening verdict; a familiar hymn may be inferred from context.',
        'lines':rows,'errors':errors,'words':words,'wer':errors/words}
out.write_text(json.dumps(report,indent=2)+'\n')
print(f'WER {100*errors/words:.1f}% ({errors}/{words})',flush=True)
