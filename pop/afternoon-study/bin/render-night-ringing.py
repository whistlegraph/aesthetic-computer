#!/usr/bin/env python3
"""Your voice as ringing, chorded bells around the Aesthetivox chorale.

Vocal bell = measured vowel spectrum + pitched WORLD excitation, individual
frequency-band decays. The original vowel formants supply the instrument.
"""
import argparse
import importlib.util
import json
from pathlib import Path
import numpy as np
import soundfile as sf
from scipy.signal import resample_poly

spec=importlib.util.spec_from_file_location('chorale',Path(__file__).with_name('render-night-chorale.py'))
c=importlib.util.module_from_spec(spec)
spec.loader.exec_module(c)
s,n,ac=c.s,c.n,c.ac
SR,FS=ac.SR,24000
OUT=n.ROOT/'out/nighttime-ringing'


class VoiceBells:
    def __init__(self):
        self.memory={};self.events=[];self.vowels=[]
        # Three different mouths of the same singer, from the same takes.
        for name,seconds in [('saffron-swallows',3.0),('indigo-shadows',6.0),('lemon-kittens',8.0)]:
            files=list((c.OUT/'cache').glob(f'{name}-*-analysis.npz'))
            assert files,'render the chorale once to prepare its source analysis'
            with np.load(files[0]) as z:
                t=z['t'];f0=z['f0'];sp=z['sp'];ap=z['ap']
                ix=np.where((t>=seconds)&(t<seconds+.6)&(f0>0))[0]
                assert len(ix)>8,(name,'no stable vowel')
                spectrum=np.exp(np.mean(np.log(sp[ix]+1e-12),axis=0))
                noise=np.median(ap[ix],axis=0)
                self.vowels.append((name,spectrum,noise))

    def __call__(self,midi,seconds=15):
        # Long fundamental, quicker loss of the upper formants: a vocal
        # strike gradually turns into a pure ringing pitch.
        seconds=round(seconds*1.6,2)
        key=(midi,seconds)
        self.events.append(dict(midi=midi,decaySeconds=seconds))
        if key in self.memory:return self.memory[key]
        path=OUT/'cache'/f'vocal-bell-{midi}-{seconds}.wav'
        if path.exists():
            y=ac.load_mono(path)
        else:
            bank,spectrum,noise=self.vowels[midi%3]
            count=round(seconds/.005)
            t=np.arange(count)*.005
            hz=ac.midi_hz(midi)
            freq=np.linspace(0,FS/2,len(spectrum))
            decay=seconds/np.clip(1+np.maximum(0,freq-450)/1800,1,5)
            env=np.exp(-6.91*t[:,None]/decay[None,:])
            attack=np.minimum(1,t/.009)
            sp=np.ascontiguousarray(spectrum[None,:]*(env*attack[:,None])**2)
            noise=noise.copy()
            noise[freq<3500]*=.6
            noise[freq>4500]=np.maximum(noise[freq>4500],.45)
            ap=np.ascontiguousarray(np.tile(noise,(count,1)))
            # Almost still pitch, with a very slow beat instead of vibrato.
            f0=hz*2**(.008*np.sin(2*np.pi*.33*t)/12)
            y=ac.pw.synthesize(f0,sp,ap,FS,5)
            y=resample_poly(y,2,1)
            y*=.9/max(1e-8,np.max(abs(y)))
            y*=ac.fades(len(y),.005,1.0)
            sf.write(path,y,SR,subtype='FLOAT')
        self.memory[key]=y
        return y


def main():
    parser=argparse.ArgumentParser()
    parser.add_argument('--study',type=Path,default=ac.STUDY)
    args=parser.parse_args();ac.STUDY=args.study
    for sub in ('stems','cache'):(OUT/sub).mkdir(parents=True,exist_ok=True)
    s.TAKES=sorted(s.TAKES+c.EXTRA,key=lambda row:row[3])
    s.RecordedVoice=c.AesthetivoxVoice
    # Reuse the already-rendered vocal phrases without altering them.
    bells=VoiceBells();s.bell=bells
    n.OUT,n.BPM,n.BEAT,n.BAR,n.BARS=OUT,s.BPM,s.BEAT,s.BAR,s.BARS
    n.DURATION=s.BARS*s.BAR+34;n.PHRASES=s.SECTIONS;n.MOTIFS=[s.A,s.B]
    ac.BEAT,ac.BAR=s.BEAT,s.BAR
    mix,voices,harmony=s.arrange()
    # Exposed vocal-bell chords give the listener a clear demonstration of
    # the new instrument; later chords broaden the slow movement and coda.
    for bar,chord,gain in [(4,'C6add9',.14),(20,'Fmaj9',.13),
                           (128,'Fmaj9',.14),(144,'C6add9',.14),
                           (216,'Fmaj9',.11),(236,'C6add9',.10)]:
        pitches=[m+12 for m in c.VOICINGS[chord]]
        for k,midi in enumerate(pitches):
            mix.place('bells',bells(midi,24),bar*s.BAR+k*.075,gain,[-.3,0,.3][k])
    settings={'voice':(.27,10,11500),'choir-bass':(.38,13,6400),
              'choir-tenor':(.48,17,7800),'choir-upper':(.53,20,9000),
              'piano':(.25,7,8500),'bells':(.42,16,10500),'sines':(.32,11,7000),
              'kick':(0,0,8000),'bass':(0,0,2100),'shuffle':(.05,.8,12000),'clap':(.08,1,10000)}
    n.finish(mix,voices,harmony,title='Nighttime Study — Ringing Voice',slug='nighttime-study-ringing',
             settings=settings,bus_gains={'piano':1.05,'bells':.90,'sines':.55},
             target_lufs=-16,measure=s.d.meter,fade_seconds=10,
             vocal_source='Jeffrey Sep 26 voice, hard-tuned lead and chorale; bells synthesized from the measured vowel spectra of the same three takes')
    receipt=json.loads((OUT/'receipts.json').read_text())
    receipt.update(choraleVoicings=c.VOICINGS,vocalPhrases=len(s.TAKES),
                   voiceBellDesign='WORLD vowel formants; 9ms strike, frequency-dependent exponential decay, tiny slow pitch drift',
                   voiceBells=bells.events,roomDecays={k:v[1] for k,v in settings.items()},
                   tuning=dict(intraNoteMotion=.04,maxDeviationSemitones=.07,voicedDryBlend=.04),
                   sources=n.SOURCES+['pop/bin/autotune.py','pop/bin/vowel-hold.py'])
    (OUT/'receipts.json').write_text(json.dumps(receipt,indent=2))


if __name__=='__main__':main()
