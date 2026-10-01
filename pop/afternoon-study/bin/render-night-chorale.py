#!/usr/bin/env python3
"""Aesthetivox chorale revision of the vocal dance suite.

Full recorded phrases -> WORLD note lock -> original unvoiced composite.
Three individually voiced choir parts retain the recording's spectral shape.
The house vowel-hold technique extends phrase endings, with room to breathe.
"""
import argparse
import hashlib
import importlib.util
import json
from pathlib import Path
from types import SimpleNamespace
import numpy as np
import soundfile as sf
from scipy.signal import resample_poly, butter, sosfilt

spec = importlib.util.spec_from_file_location('suite', Path(__file__).with_name('render-night-suite.py'))
s = importlib.util.module_from_spec(spec)
spec.loader.exec_module(s)
n, ac = s.n, s.ac
SR, FS = ac.SR, 24000
OUT = n.ROOT / 'out/nighttime-chorale'
EXTRA = [
    ('indigo-shadows', 0, 6, 0), ('saffron-swallows', 34.3, 10, 16),
    ('indigo-shadows', 13.4, 8, 28), ('lemon-kittens', 16.5, 8, 40),
    ('indigo-shadows', 8.5, 6, 52), ('saffron-swallows', 27.3, 8, 64),
    ('lemon-kittens', 41, 10, 90), ('indigo-shadows', 34.4, 8, 104),
    ('saffron-swallows', 0, 8, 115), ('lemon-kittens', 55, 5, 126),
    ('saffron-swallows', 9.1, 12, 142), ('indigo-shadows', 13.4, 8, 162),
    ('lemon-kittens', 5.6, 10, 174), ('saffron-swallows', 0, 7, 186),
    ('indigo-shadows', 22, 8, 196), ('lemon-kittens', 16.5, 6.6, 210),
    ('indigo-shadows', 17.7, 8, 216), ('saffron-swallows', 43.0, 1.7, 235),
]
# Bass / tenor / upper voice, independently led rather than parallel shifts.
VOICINGS = {'C6add9': [48,55,64], 'Fmaj9': [48,57,65],
            'G6sus': [50,55,62], 'Dm9': [50,57,65],
            'Gmajor': [47,55,62], 'D7sus': [50,55,60], 'Cmaj9': [48,55,64]}
ROUTES = [
    ['C6add9','C6add9','Fmaj9','G6sus'],
    ['C6add9','Fmaj9','Dm9','G6sus','C6add9'],
    ['Gmajor','D7sus','Gmajor','Cmaj9','Dm9','G6sus','Gmajor'],
    ['Fmaj9','C6add9','Fmaj9'],
    ['C6add9','Fmaj9','G6sus','C6add9','Fmaj9','Dm9','G6sus','C6add9'],
    ['Fmaj9','G6sus','C6add9'],
]


def chord_at(seconds):
    bar = min(s.BARS-1, max(0, int(seconds/s.BAR)))
    for (start,bars,_),route in zip(s.SECTIONS,ROUTES):
        if start <= bar < start+bars:
            return route[(bar-start)//8]


def level(y, rms=.15):
    active = np.sqrt(np.percentile([np.mean(a*a) for a in
                     np.array_split(y,max(1,len(y)//1200))],70))
    return .42*np.tanh(y*min(18,rms/max(active,1e-6))/.42)


class AesthetivoxVoice:
    def __init__(self,name,offset):
        self.bank = SimpleNamespace(name=name)
        self.path=ac.STUDY/'sources'/name/'stems/voice.wav'
        self.sha=hashlib.sha256(self.path.read_bytes()).hexdigest()
        self.offset,self.events=offset,[]
        self.raw=ac.load_mono(self.path,FS)
        cached=OUT/'cache'/f'{name}-{self.sha[:10]}-analysis.npz'
        if cached.exists():
            with np.load(cached) as z:
                self.f0,self.t,self.sp,self.ap=[z[k] for k in ('f0','t','sp','ap')]
        else:
            print('WORLD analysis',name,flush=True)
            x=np.ascontiguousarray(self.raw)
            self.f0,self.t=ac.pw.harvest(x,FS,f0_floor=65,f0_ceil=1000,frame_period=5)
            self.f0=ac.pw.stonemask(x,self.f0,self.t,FS)
            fft=ac.pw.get_cheaptrick_fft_size(FS,f0_floor=65)
            self.sp=ac.pw.cheaptrick(x,self.f0,self.t,FS,fft_size=fft,f0_floor=65)
            self.ap=ac.pw.d4c(x,self.f0,self.t,FS,fft_size=fft)
            np.savez_compressed(cached,f0=self.f0,t=self.t,sp=self.sp,ap=self.ap)
        score=json.loads((self.path.parents[1]/'score.mbscore').read_text())['notes']
        self.lock=self.f0.copy()
        observed=np.zeros_like(self.f0)
        voiced=self.f0>0
        observed[voiced]=69+12*np.log2(self.f0[voiced]/440)
        # Every note owns the space to the next note; preserve 4% of its
        # internal pitch motion. The fixed target retains the sung octave.
        for i,note in enumerate(score):
            a=note['start']+offset
            b=score[i+1]['start']+offset if i+1<len(score) else self.t[-1]+.005
            mask=(self.t>=a)&(self.t<b)&voiced
            core=mask&(self.t<a+note['dur'])
            if core.sum()<3: core=mask
            if not core.any(): continue
            center=float(np.median(observed[core]))
            target=note['midi']+12*round((center-note['midi'])/12)
            corrected=target+np.clip((observed[mask]-center)*.04,-.07,.07)
            self.lock[mask]=440*2**((corrected-69)/12)

    def place(self,mix,start,duration,bar):
        source=max(0,start+self.offset-.06)
        a=max(0,round(source/.005)); b=min(len(self.f0),a+round(duration/.005))
        f0=self.lock[a:b].copy(); t=np.arange(b-a)*.005
        voiced=self.f0[a:b]>0
        sp=np.ascontiguousarray(self.sp[a:b]); ap=np.ascontiguousarray(self.ap[a:b])
        x=self.raw[round(source*FS):round(source*FS)+round(duration*FS)]
        tag=f'{self.bank.name}-{start}-{duration}-{bar}'
        prefix=OUT/'cache'/tag
        at=bar*s.BAR
        next_at=min([row[3]*s.BAR for row in s.TAKES if row[3]>bar] or [s.BARS*s.BAR])
        hold=min(4.5,max(0,next_at-at-duration-.4))
        if bar==235: hold=8.5
        print('chorale phrase',self.bank.name,bar,flush=True)
        tail_frames=np.where(voiced & (t>max(0,t[-1]-.7)))[0]
        if len(tail_frames)<6: hold=0
        for part,bus,pan,gain in [(-1,'voice',0,1.45),(0,'choir-bass',-.12,.50),
                                  (1,'choir-tenor',-.42,.64),(2,'choir-upper',.42,.57)]:
            path=Path(str(prefix)+f'-{bus}.wav')
            if path.exists():
                y=ac.load_mono(path)
            else:
                pitch=f0.copy()
                if part>=0:
                    midis=np.array([VOICINGS[chord_at(at+v)][part] for v in t],dtype=float)
                    # One upper suspension resolves by step a beat after the
                    # chord changes; lower voices arrive with the harmony.
                    if part==2:
                        lag=round(s.BEAT/.005)
                        for change in np.where(np.diff(midis)!=0)[0]+1:
                            if abs(midis[change]-midis[change-1])<=2:
                                midis[change:min(len(midis),change+lag)]=midis[change-1]
                    pitch[voiced]=440*2**((midis[voiced]-69)/12)
                y=ac.pw.synthesize(np.ascontiguousarray(pitch),sp,ap,FS,5)[:len(x)]
                y=np.pad(y,(0,max(0,len(x)-len(y))))
                mask=np.interp(np.arange(len(x))/FS,t,voiced.astype(float))
                mask=np.convolve(mask,np.ones(121)/121,mode='same')
                if part<0:
                    raw_rms=np.sqrt(np.mean((x*mask)**2))
                    y*=np.clip(raw_rms/max(1e-7,np.sqrt(np.mean((y*mask)**2))),.3,3)
                    y=y*mask*.96+x*((1-mask)+.04*mask)
                else:
                    y*=mask
                y=level(y,.18 if part<0 else .13)
                if hold>0:
                    ix=tail_frames[-min(50,len(tail_frames)):]
                    count=round(hold/.005)
                    spectrum=np.exp(np.mean(np.log(sp[ix]+1e-12),axis=0))
                    noise=np.median(ap[ix],axis=0)
                    freq=np.linspace(0,FS/2,len(noise))
                    noise[freq>4000]=np.maximum(noise[freq>4000],.35)
                    hz=float(np.median(pitch[ix]))
                    tt=np.arange(count)*.005
                    hold_pitch=hz*2**(.035*np.sin(2*np.pi*4.7*tt)*np.minimum(1,tt/.5)/12)
                    held=ac.pw.synthesize(hold_pitch,np.tile(spectrum,(count,1)),np.tile(noise,(count,1)),FS,5)
                    ref=y[max(0,len(y)-round(.6*FS)):]
                    held*=min(5,np.sqrt(np.mean(ref*ref))/max(1e-6,np.sqrt(np.mean(held*held))))
                    held*=np.sin(np.minimum(1,np.arange(len(held))/FS/.06)*np.pi/2)
                    held*=np.clip(1-np.arange(len(held))/len(held),0,1)**.65
                    y[-min(len(y),1200):]*=np.linspace(1,0,min(len(y),1200))
                    y=np.concatenate([y,held])
                y=resample_poly(y,2,1)
                y*=ac.fades(len(y),.012 if part<0 else .10,.09)
                sf.write(path,y,SR,subtype='FLOAT')
            # More exposed lead at the start, then gradually grow the choir.
            choir_level=.50 if bar<8 else .8 if bar<32 else 1
            if 128<=bar<152: choir_level=1.15
            mix.place(bus,y,at,gain*(1 if part<0 else choir_level),pan)
        self.events.append(dict(source=str(self.path.relative_to(ac.STUDY)),sourceSha256=self.sha,
                                sourceStartSeconds=source,seconds=duration+hold,atSeconds=at,
                                holdSeconds=hold,processing='WORLD note lock, 4% pitch motion, original unvoiced composite; bass/tenor/upper independently voiced'))


def main():
    parser=argparse.ArgumentParser()
    parser.add_argument('--study',type=Path,default=ac.STUDY)
    args=parser.parse_args();ac.STUDY=args.study
    for sub in ('cache','stems'): (OUT/sub).mkdir(parents=True,exist_ok=True)
    s.TAKES=sorted(s.TAKES+EXTRA,key=lambda row:row[3])
    s.RecordedVoice=AesthetivoxVoice
    n.OUT,n.BPM,n.BEAT,n.BAR,n.BARS=OUT,s.BPM,s.BEAT,s.BAR,s.BARS
    n.DURATION=s.BARS*s.BAR+18;n.PHRASES=s.SECTIONS;n.MOTIFS=[s.A,s.B]
    ac.BEAT,ac.BAR=s.BEAT,s.BAR
    mix,voices,harmony=s.arrange()
    settings={'voice':(.07,1.7,11500),'choir-bass':(.17,3.8,6400),
              'choir-tenor':(.22,4.5,7800),'choir-upper':(.25,5.2,9000),
              'piano':(.20,4,8500),'bells':(.25,7,10500),'sines':(.24,6,7000),
              'kick':(0,0,8000),'bass':(0,0,2100),'shuffle':(.05,.8,12000),'clap':(.08,1,10000)}
    n.finish(mix,voices,harmony,title='Nighttime Study — Aesthetivox Chorale',slug='nighttime-study-chorale',
             settings=settings,bus_gains={'piano':1.15,'bells':.75,'sines':.65},
             target_lufs=-15.5,measure=s.d.meter,
             vocal_source='Jeffrey original Sep 26 mic recordings; WORLD hard note lock with original unvoiced composite, independently led three-part vocal choir, extended source vowels')
    receipt=json.loads((OUT/'receipts.json').read_text())
    receipt.update(choraleVoicings=VOICINGS,vocalPhrases=len(s.TAKES),
                   tuning=dict(intraNoteMotion=.04,maxDeviationSemitones=.07,voicedDryBlend=.04),
                   sources=n.SOURCES+['pop/bin/autotune.py','pop/bin/vowel-hold.py'])
    (OUT/'receipts.json').write_text(json.dumps(receipt,indent=2))


if __name__=='__main__':main()
