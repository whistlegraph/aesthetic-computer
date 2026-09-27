#!/usr/bin/env python3
"""Add independently sung octave choir layers to the reviewed harmony stems.
Usage: pop/.venv/bin/python pop/big-pictures/bin/choir-amazing.py HARMONY_OUT CHOIR_OUT
"""
import hashlib
import json
import math
import os
from pathlib import Path
import subprocess
import sys
import numpy as np
import soundfile as sf
REPO = Path(__file__).resolve().parents[3]
SOURCE, OUT = [Path(p).resolve() for p in sys.argv[1:3]]
assert SOURCE != OUT
OUT.mkdir(parents=True, exist_ok=True)
BASE = json.loads((SOURCE/'arrangement-receipt.json').read_text())
SMOOTH = Path(BASE['source'])
WORDS = json.loads((SMOOTH/'vox-receipt.json').read_text())['words']
SR=48000
SPB=60/70
START=BASE['secondPassStart']
FORWARD=os.environ.get('CHOIR_FORWARD','0')=='1'

def run(args):
    return subprocess.run(list(map(str,args)),check=True,capture_output=True,text=True).stdout.strip()
def read(p):
    y,sr=sf.read(p,always_2d=True); assert sr==SR and np.isfinite(y).all(); return y
def write(name,y):
    sf.write(OUT/name,y,SR,subtype='FLOAT')
def process(y,name,filt):
    write(name+'-raw.wav',y)
    run(['ffmpeg','-y','-v','error','-i',OUT/(name+'-raw.wav'),'-af',filt,'-c:a','pcm_f32le',OUT/(name+'.wav')])
    return read(OUT/(name+'.wav'))
lead_path=Path(os.environ.get('LEAD_OVERRIDE', str(SOURCE/'lead-centered.wav')))
lead=read(lead_path)[:,0]
n=len(lead)
def place(target,y,at,gain=1):
    start=round(at*SR); end=min(n,start+len(y)); target[start:end]+=y[:end-start]*gain

def note(m):
    return ['C','C#','D','D#','E','F','F#','G','G#','A','A#','B'][m%12]+str(m//12-1)
verification=[]
choir=np.zeros((n,2))
parts=([('octave-low',-12,0,-.80,.44),('octave-high',12,6,.80,.38)] if FORWARD else
       [('octave-low',-12,0,-.66,.22),('octave-high',12,12,.66,.19)])
for part,shift,entry,pan,gain in parts:
    folder=Path(os.environ.get('CHOIR_CACHE', str(OUT)))/part; folder.mkdir(exist_ok=True)
    bus=np.zeros(n)
    for w in WORDS:
        if w['i']<entry: continue
        src=list((SMOOTH/'words').glob(f"{22 if w['i']==16 else w['i']:02d}-*.wav"))[0]
        phrase=w['text'] in ['sound','me','found','see']
        # The final SEE expands over the instrumental amen, creating the choir bloom.
        dur=[v['beats']*SPB for v in w['notes']]
        dur[-1]+=4*SPB if w['i']==25 else (-.12 if phrase else .04)
        spec=','.join(f"{note(v['midi']+shift)}:{d:.3f}" for v,d in zip(w['notes'],dur))
        dest=folder/f"{w['i']:02d}.wav"
        args=[sys.executable,'-W','ignore',REPO/'pop/cult/bin/sing.py',src,dest,'--notes',spec,
              '--f0-floor','65','--f0-ceil','900','--vibrato-hz','4.6' if shift<0 else '5.6',
              '--vibrato-cents','11' if shift<0 else '17','--vibrato-onset-ms','480',
              '--overshoot-cents','6','--formant-db','0.8','--attack-ms','28',
              '--release-ms','180' if phrase else '65','--xfade-ms','110','--shimmer-frames','.2',
              '--deess','.045','--gain','.707','--verify']
        key=hashlib.sha256(src.read_bytes()+(REPO/'pop/cult/bin/sing.py').read_bytes()+str(args).encode()).hexdigest()
        cache=dest.with_suffix('.json')
        if cache.exists() and dest.exists() and json.loads(cache.read_text())['key']==key:
            report=json.loads(cache.read_text())['report']
        else:
            report=run(args); cache.write_text(json.dumps({'key':key,'report':report}))
        verification.append({'part':part,'word':w['text'],'notes':spec,'verification':report})
        print(part,w['text'],report,flush=True)
        # Choir answers start after lead consonants; density grows phrase by phrase.
        grow=(.85 if w['i']<12 else 1) if FORWARD else (.55 if w['i']<12 else (.8 if w['i']<20 else 1))
        at=START+w['start']+((.10 if shift<0 else .16) if FORWARD else (.055 if shift<0 else .085))
        place(bus,read(dest)[:,0],at,grow)
        if FORWARD and w['i']>=20:
            # Foreshadow the choir on the first verse's final line.
            early=read(dest)[:,0].copy()
            keep=round((sum(v['beats'] for v in w['notes'])*SPB)*SR)
            early=early[:keep]
            tail=min(len(early),round(.15*SR))
            early[-tail:]*=np.linspace(1,0,tail)
            place(bus,early,6+w['start']+.12,.55)
    filt=('highpass=f=90,lowpass=f=5200,' if shift<0 else 'highpass=f=230,lowpass=f=8800,')
    bus=process(bus,part,filt+'acompressor=threshold=.12:ratio=2:attack=25:release=240:knee=3')[:,0]
    angle=(pan+1)*math.pi/4
    choir+=bus[:,None]*np.array([math.cos(angle),math.sin(angle)])*gain
# Existing scored thirds/fifths become the inside choir voices.
inner=1.65 if FORWARD else .96
choir+=read(SOURCE/'lower-panned.wav')*inner+read(SOURCE/'upper-panned.wav')*inner
# Slow, shallow positive-polarity flanging sits predominantly on backing voices.
# Independent left/right LFO phase creates motion without an inverted side channel.
t=np.arange(n)/SR
flanged=np.zeros_like(choir)
for ch in range(2):
    delay=.003+.0018*np.sin(2*np.pi*.19*t+ch*1.1)
    delayed=np.interp(np.arange(n)-delay*SR,np.arange(n),choir[:,ch],left=0,right=0)
    flanged[:,ch]=choir[:,ch]*.82+delayed*.27
flanged=process(flanged,'choir-flanged','highpass=f=95,equalizer=f=2400:t=q:w=1.2:g=-1.8')
# A little flange is audible on the solo voice, with its dry center dominant.
lead_stereo=np.repeat(lead[:,None],2,axis=1)*1.10
if FORWARD:
    # Make room for separate singers as the second pass arrives.
    lead_stereo*= (1-.18*np.clip((t-START)/1.5,0,1))[:,None]
for ch in range(2):
    delay=.0024+.001*np.sin(2*np.pi*.14*t+ch*.8)
    lead_stereo[:,ch]+=.065*np.interp(np.arange(n)-delay*SR,np.arange(n),lead,left=0,right=0)
voice=lead_stereo+flanged
room_source=process(flanged+lead_stereo*.14,'choir-room-send','highpass=f=280,lowpass=f=6500')
room=np.zeros_like(voice)
# Dense staggered reflections bloom only during the last line and final held SEE.
angel=np.clip((t-(START+42))/12,0,1)
for k in range(1,19):
    delay=.061+k*.119
    decay=.065*np.exp(-k/6)
    weights=np.array([1,.76]) if k%2 else np.array([.76,1])
    place(room,room_source*weights*(.55+.85*angel[:,None]),delay,decay)
voice+=room
write('vocal-spatial.wav',voice)
# Rebuild from buses rather than processing the previous full master.
mix=read(SOURCE/'bed-arranged.wav')*.64+voice
place(mix,read(SMOOTH/'stamp.wav'),65.8+(START-6),.68)
fade=round(2.5*SR); mix[-fade:]*=np.linspace(1,0,fade)[:,None]
voice[-fade:]*=np.linspace(1,0,fade)[:,None]
write('vocal-spatial.wav',voice)
write('pre-choir.wav',mix)
receipt={**BASE,'sourceHarmony':str(SOURCE),'version':'choir + octave voices + flange', 'leadPath':str(lead_path),
         'verification':BASE['verification']+verification,
         'choir':{'parts':['center lead','lower harmony','upper harmony','octave low','octave high'],
                  'forward':FORWARD,'firstChoirHint':6+WORDS[20]['start'] if FORWARD else None,
                  'highOctaveEntry':START+WORDS[6 if FORWARD else 12]['start'],'lastSeeExtensionBeats':4,
                  'flange':'3 ± 1.8 ms / 0.19 Hz backing; 2.4 ± 1 ms / 0.14 Hz at 6.5% on lead',
                  'room':'18 positive-polarity filtered reflections, blooming in last line'},
         'mix':'center lead; progressively entering independently resynthesized Jeffrey choir; octave layers; soft flange; angelic held ending'}
(OUT/'arrangement-receipt.json').write_text(json.dumps(receipt,indent=2)+'\n')
print('CHOIR PREMIX',OUT/'pre-choir.wav',flush=True)
