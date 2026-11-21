"""Render wada jetter QD UR (napsines) from the existing Wada Jetter vocal jobs and choir score."""
import argparse
import json
import subprocess
from pathlib import Path

import numpy as np
from scipy.io import wavfile
from scipy.signal import butter, sosfilt

p=argparse.ArgumentParser(description=__doc__)
for name in ('jobs','score','out'):
    p.add_argument(name,type=Path)
a=p.parse_args()
a.out.mkdir(exist_ok=False,parents=True)
jobs=json.loads(a.jobs.read_text())['jobs']
samples={Path(v[2]).stem:Path(v[2]) for row in a.score.read_text().splitlines()
         if (v:=row.split()) and v[0]=='smp'}
sr=48000
beat=60/76
bar=beat*3
buf=np.zeros((round((24*bar+3.5)*sr),2))
def add(y,t,gain,pan):
    start=round(t*sr)
    n=min(len(y),len(buf)-start)
    buf[start:start+n] += gain*y[:n,None]*np.sqrt([(1-pan)/2,(1+pan)/2])
def tine(midi,length,soft=False):
    t=np.arange(round(length*sr))/sr
    phase=2*np.pi*t*440*2**((midi-69)/12)
    if soft:
        y=np.sin(phase)*np.exp(-1.8*t)+.17*np.sin(2*phase)*np.exp(-3*t)
    else:
        y=(np.sin(phase)*np.exp(-2.4*t)+.18*np.sin(2*phase)*np.exp(-5*t)
           +.085*np.sin(3.99*phase)*np.exp(-7*t))
    return y*(-np.expm1(-t/(.055 if soft else .011)))*np.minimum(1,t[::-1]/.14)
def slower(path):
    data=subprocess.check_output(['ffmpeg','-loglevel','error','-f','f32le','-ar','48000','-ac','1','-i',str(path),
                                  '-af','atempo=0.75,lowpass=f=5700','-f','f32le','pipe:1'])
    return np.frombuffer(data,'<f4')
hook=next(j for j in jobs if j['line']=='lead' and j['mode']=='angel')
pink=next(j for j in jobs if j['line']=='lip-red' and j['mode']=='angel')
# A three-beat rocking bass beneath the title tune. Leave the third beat empty at cadences.
chords=[(40,55,59),(40,54,59),(45,57,61),(47,54,59)]
for i in range(24):
    bass,third,fifth=chords[(i//2)%4] if i<22 else chords[0]
    at=i*bar
    add(tine(bass,bar+.3,True),at,.118,0)
    for j,note in enumerate([third,fifth]):
        if i in (7,15,23) and j==1: continue
        add(tine(note+12,1.4),at+(j+1)*beat,.044 if j else .052,(-.27,.32)[j])
for block,j in [(0,hook),(8,pink),(16,hook)]:
    # Let the music box pose the question, then answer with the original warm voice.
    for onset,note in zip(j['starts'],j['midis']):
        add(tine(note+24,1.5),block*bar+onset*4/3,.094,.11)
    y=slower(samples[j['key']])
    add(y,(block+3)*bar,.65,-.035)
add(slower(samples['superstar-soft-answer']),21*bar,.52,.07)
for k,note in enumerate([59,64,71,76]):
    add(tine(note,2.6),22.75*bar+k*.5*beat,.063/(1+.23*k),.13*k-.22)
# Short alternating room reflections and a gentle peak ceiling for the bedside mix.
dry=buf.copy()
for delay,gain in [(.085,.105),(.173,.071),(.294,.048),(.433,.032)]:
    n=round(delay*sr)
    buf[n:]+=dry[:-n,::-1]*gain
buf=sosfilt(butter(2,6800,fs=sr,output='sos'),buf,axis=0)
buf*=min(1.5,.58/max(np.max(np.abs(buf)),1e-9))
wavfile.write(a.out/'napsines.wav',sr,np.int16(np.clip(buf,-1,1)*32767))
subprocess.run(['ffmpeg','-hide_banner','-loglevel','error','-i',str(a.out/'napsines.wav'),
                '-codec:a','libmp3lame','-b:a','320k','-metadata','title=wada jetter QD UR (napsines)',
                '-metadata','artist=Aesthetic Dot Computer','-metadata','track=2',str(a.out/'napsines.mp3')],check=True)
