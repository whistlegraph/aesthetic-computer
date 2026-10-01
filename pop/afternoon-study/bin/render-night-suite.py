#!/usr/bin/env python3
"""Nighttime Study: vocal dance suite, overture / exposition / development /
slow movement / transformed return / coda. Original dry mic phrases lead.

Applies chamber platter 01 (phrase proportions, contrast, transformed return)
and 03 (motivic growth, canon, individually decaying bell partials). The suite
is a loose narrative design, not a claim of strict historical sonata form.
"""
import argparse
import hashlib
import importlib.util
import json
from pathlib import Path
from types import SimpleNamespace
import numpy as np
from scipy.signal import butter, sosfilt

spec = importlib.util.spec_from_file_location('dance', Path(__file__).with_name('render-night-dance.py'))
d = importlib.util.module_from_spec(spec)
spec.loader.exec_module(d)
n, ac = d.n, d.ac
SR = ac.SR
BPM, BARS = 104, 240
BEAT = 60 / BPM
BAR = BEAT * 4
OUT = n.ROOT / 'out/nighttime-suite'
SECTIONS = [(0, 32, 'overture'), (32, 40, 'exposition'),
            (72, 56, 'development'), (128, 24, 'slow movement'),
            (152, 64, 'transformed return'), (216, 24, 'coda')]
# Wide upper voicings; the major home is established by the overture.
CHORDS = {
    'C6add9': (36, [60, 64, 67, 69, 74]),
    'Fmaj9': (41, [60, 64, 65, 69, 74]),
    'G6sus': (43, [60, 62, 67, 69, 76]),
    'Dm9': (38, [60, 64, 65, 69, 74]),
    'Gmajor': (43, [59, 62, 67, 71, 74]),
    'D7sus': (38, [60, 62, 67, 69, 74]),
    'Cmaj9': (36, [59, 62, 64, 67, 72]),
}
# Newly written themes, introduced separately before development.
A = [(0, 1.5, 67), (2, 1, 69), (3.5, 3, 72), (8, 2, 71), (11, 3, 67)]
B = [(0, 3, 74), (4, 2, 72), (7, 1.5, 69), (10, 1, 67), (12, 3, 65)]
# Preserve real phrasing: take, source MIDI seconds, length, destination bar.
# No voice pitch shifting or time stretching; source breaths stay in place.
TAKES = [
    ('saffron-swallows', 0, 7.8, 8),
    ('lemon-kittens', 0, 5.7, 24),
    ('lemon-kittens', 5.6, 10.2, 34),
    ('indigo-shadows', 0, 8.5, 46),
    ('saffron-swallows', 34.3, 10.2, 58),
    ('lemon-kittens', 16.5, 6.6, 74),
    ('indigo-shadows', 17.7, 11.5, 84),
    ('saffron-swallows', 9.1, 10.5, 98),
    ('lemon-kittens', 24.0, 8.0, 110),
    ('indigo-shadows', 29.1, 11.1, 120),
    ('saffron-swallows', 19.6, 14.8, 132),
    ('lemon-kittens', 0, 15.6, 154),
    ('indigo-shadows', 41.0, 10.2, 168),
    ('saffron-swallows', 34.3, 10.2, 180),
    ('lemon-kittens', 32.0, 9.0, 190),
    ('indigo-shadows', 0, 13.2, 202),
    ('saffron-swallows', 27.3, 7.4, 220),
    ('lemon-kittens', 16.5, 6.6, 230),
]


class RecordedVoice:
    def __init__(self, name, offset):
        self.bank = SimpleNamespace(name=name)
        self.path = ac.STUDY / 'sources' / name / 'stems/voice.wav'
        self.raw = ac.load_mono(self.path)
        self.offset, self.events = offset, []
        self.sha = hashlib.sha256(self.path.read_bytes()).hexdigest()

    def place(self, mix, start, duration, bar):
        start = max(0, start + self.offset - .06)
        x = self.raw[round(start * SR):round((start + duration) * SR)].copy()
        assert len(x) > .98 * duration * SR, self.path
        x = sosfilt(butter(2, [85, 11500], btype='band', fs=SR, output='sos'), x)
        # Gentle level control on real microphone audio. No resynthesis.
        block = SR // 20
        powers = [np.mean(a*a) for a in np.array_split(x, max(1, len(x)//block))]
        active = np.sqrt(np.percentile(powers, 70))
        x *= min(12, .14 / max(active, 1e-6))
        x = .40 * np.tanh(x / .40)
        x *= ac.fades(len(x), .012, .07)
        mix.place('voice', x, bar * BAR, 1.25, 0)
        self.events.append(dict(source=str(self.path.relative_to(ac.STUDY)),
                                sourceSha256=self.sha, sourceStartSeconds=start,
                                seconds=len(x)/SR, atSeconds=bar*BAR,
                                processing='HP/LP, gain, gentle saturation, 12/70ms seams; original pitch and timing'))


def sine(midi, seconds):
    t = np.arange(round(seconds * SR)) / SR
    f = ac.midi_hz(midi)
    y = (np.sin(2*np.pi*f*t) + .15*np.sin(2*np.pi*2*f*t))
    return y * ac.fades(len(t), 1.6, min(5, seconds/2)) * (.94 + .06*np.cos(t*.8))


def bell(midi, seconds=15):
    t = np.arange(round(seconds * SR)) / SR
    f = ac.midi_hz(midi)
    y = np.zeros(len(t))
    for ratio, amp, decay in [(1, 1, 1), (2, .40, .65), (3, .18, .3), (4.07, .12, .12)]:
        y += amp*np.sin(2*np.pi*f*ratio*t)*np.exp(-6.9*t/(seconds*decay))
    return y/1.7*ac.fades(len(t), .009, 1.0)


def theme(mix, piano, line, bar, gain, instrument='piano', speed=1, transpose=0):
    for beat, length, midi in line:
        at = bar*BAR + beat*BEAT*speed
        if instrument == 'piano':
            piano.key(mix, midi+transpose, at, length*BEAT*speed+1.5, gain, vel=.48)
        else:
            mix.place('bells', bell(midi+transpose, 18 if speed > 1 else 14), at, gain, .15)


def arrange():
    mix = n.Mixer(n.DURATION)
    rng = np.random.default_rng(n.SEED)
    piano = ac.Piano(rng)
    offsets = {r['name']: r['alignment']['offsetSeconds'] for r in
               json.loads((ac.STUDY/'vocal-receipts.json').read_text())}
    voices = {name: RecordedVoice(name, offsets[name]) for name in ac.NAMES}
    for name, src, length, bar in TAKES:
        voices[name].place(mix, src, length, bar)
    harmony = []
    kick = ac.soft_kick()
    hat = ac.brush(.10, .006, 3200, 10000, 14)
    clap = ac.brush(.16, .012, 1000, 6000, 45)
    routes = {
        'overture': ['C6add9','C6add9','Fmaj9','G6sus'],
        'exposition': ['C6add9','Fmaj9','Dm9','G6sus','C6add9'],
        'development': ['Gmajor','D7sus','Gmajor','Cmaj9','Dm9','G6sus','Gmajor'],
        'slow movement': ['Fmaj9','C6add9','Fmaj9'],
        'transformed return': ['C6add9','Fmaj9','G6sus','C6add9','Fmaj9','Dm9','G6sus','C6add9'],
        'coda': ['Fmaj9','G6sus','C6add9'],
    }
    for start, bars, name in SECTIONS:
        print('arrange', name, flush=True)
        for b in range(bars):
            bar = start+b
            root, notes = CHORDS[routes[name][b//8]]
            harmony.append(dict(bar=bar,chord=routes[name][b//8]))
            slow = name == 'slow movement'
            overture = name == 'overture'
            ending = name == 'coda'
            growth = .75 + .25*b/bars if name == 'development' else 1
            gain = growth*(1-.6*b/bars if ending else 1)
            # Sustained sine harmony crosses bar lines; bell partials ring
            # independently. Breathing room replaces repeated short toplines.
            if b%8 == 0:
                for k,m in enumerate([notes[0],notes[2]]):
                    mix.place('sines', sine(m, BAR*8+3), bar*BAR,
                              (.016 if slow else .012)*gain, -.3 if k==0 else .3)
                piano.roll(mix,[root+12,notes[1]+12,notes[3]+12],bar*BAR,BAR*4,.24*gain,.4)
                mix.place('bells',bell(notes[2]+12,22 if slow else 15),bar*BAR,.08*gain,.2)
            # Overture begins freely; the dance pulse enters in stages.
            pulse = not slow and (not overture or b>=16) and bar<232
            if pulse:
                for beat in ([0,2] if overture or ending else range(4)):
                    mix.place('kick',kick,(bar*4+beat)*BEAT,.25*gain)
                for beat in range(4):
                    mix.place('shuffle',hat,(bar*4+beat+.54)*BEAT,.035*gain,(-1)**beat*.16)
                if not overture and not ending:
                    for beat in [1,3]:
                        mix.place('clap',clap,(bar*4+beat+.02)*BEAT,.042*gain)
                for beat,m,length in [(0.6,root,.6),(2.6,root,.6),(3.6,root+7,.25)]:
                    mix.place('bass',ac.bass_tone(m,length*BEAT,.7),(bar*4+beat)*BEAT,.065*gain)
            # Different accompanimental grammar per movement; rests keep the
            # sung phrases audible. The return develops earlier piano cells.
            if name in ('exposition','development','transformed return') and b%4<3:
                order = [0,2,1,3,2,4] if name!='development' else [4,2,3,1,2,0]
                subdivision = BEAT/2 if name!='transformed return' else BEAT/3
                for k in range(6 if name!='transformed return' else 9):
                    m=notes[order[(bar+k)%6]]+12
                    piano.key(mix,m,bar*BAR+k*subdivision,2.5*BEAT,.20*gain,vel=.38)
            if overture and b in (0,12,24):
                theme(mix,piano,A if b!=12 else B,bar,.34, speed=1.4)
            if name=='exposition' and b in (8,24):
                theme(mix,piano,A if b==8 else B,bar,.25,'bells')
            if name=='development' and b in (0,12,28,44):
                # Motif fragments invert their contour and answer in canon.
                line=[(at,length,134-midi) for at,length,midi in A[:3]]
                theme(mix,piano,line,bar,.24,speed=.75,transpose=2)
                theme(mix,piano,line,bar+2,.08,'bells',speed=1.5,transpose=14)
            if slow and b in (0,12):
                theme(mix,piano,B,bar,.16,'bells',speed=2)
            if name=='transformed return' and b in (0,16,36,52):
                theme(mix,piano,A if b%32==0 else B,bar,.30,speed=1.5)
            if ending and b in (0,12):
                theme(mix,piano,A[:3],bar,.22 if b==0 else .14,'bells',speed=2)
        if name=='coda':
            mix.place('bells',bell(72,24),236*BAR,.07)
    # The actual recorded voice owns the foreground. Instrumental mids duck
    # beneath each continuous phrase; the kick and bass keep the dance moving.
    duck=np.ones(mix.total,dtype='float32')
    for voice in voices.values():
        for event in voice.events:
            a=round(event['atSeconds']*SR); z=min(mix.total,a+round(event['seconds']*SR))
            duck[a:z]=np.minimum(duck[a:z],.42)
            attack=min(round(.25*SR),a)
            duck[a-attack:a]=np.minimum(duck[a-attack:a],np.linspace(1,.42,attack))
            end=min(mix.total,z+round(.8*SR))
            duck[z:end]=np.minimum(duck[z:end],np.linspace(.42,1,end-z))
    for bus in ('piano','bells','sines'):
        mix.buses[bus][:] *= duck[:,None]
    return mix,list(voices.values()),harmony


def main():
    parser=argparse.ArgumentParser()
    parser.add_argument('--study',type=Path,default=ac.STUDY)
    args=parser.parse_args(); ac.STUDY=args.study
    for sub in ('stems','cache'): (OUT/sub).mkdir(parents=True,exist_ok=True)
    n.OUT,n.BPM,n.BEAT,n.BAR,n.BARS=OUT,BPM,BEAT,BAR,BARS
    n.DURATION=BARS*BAR+18; n.PHRASES=SECTIONS; n.MOTIFS=[A,B]
    ac.BEAT,ac.BAR=BEAT,BAR
    mix,voices,harmony=arrange()
    settings={'voice':(.045,1.2,11500),'piano':(.2,4,8500),
              'bells':(.25,7,10500),'sines':(.24,6,7000),
              'kick':(0,0,8000),'bass':(0,0,2100),
              'shuffle':(.05,.8,12000),'clap':(.08,1,10000)}
    n.finish(mix,voices,harmony,title='Nighttime Study — Vocal Suite',slug='nighttime-study-suite',
             settings=settings,bus_gains={'piano':2.2},target_lufs=-16,measure=d.meter,
             vocal_source='Original Sep 26 stems/voice.wav microphone recordings; continuous phrases, original pitch and timing, no WORLD or phase-vocoder processing')


if __name__=='__main__': main()
