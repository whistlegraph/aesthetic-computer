"""Revoice the eight colour entries in a choir-cutie.py score. Render the result with run-cutie.mjs."""
import argparse
from pathlib import Path
import shutil

import numpy as np
from scipy.ndimage import gaussian_filter1d
from scipy.signal import butter, sosfilt

SR = 48000
COLOURS = ['lip-red', 'lip-orange', 'lip-yellow', 'lip-green', 'lip-blue', 'lip-purple', 'lip', 'lip-black']
FUNDAMENTALS = [82.41, 123.47, 164.81, 207.65, 246.94, 329.63, 369.99, 415.30]


def level(x, target):
    return x * target / max(1e-8, np.sqrt(np.mean(x*x)))


def band(x, lo, hi):
    return sosfilt(butter(2, [lo, hi], fs=SR, btype='bandpass', output='sos'), x)


def envelope(t, duration, attack, release):
    return np.sin(.5*np.pi*np.clip(t/attack, 0, 1))**2 * np.sin(.5*np.pi*np.clip((duration-t)/release, 0, 1))**2


def scream_word(word, scream, freq):
    # Keep the actual colour articulation, imposing its moving vowel shape on a recorded scream.
    t = np.arange(len(word))/SR
    grain = scream[:min(len(scream), SR//2)]
    grain = level(grain*np.hanning(len(grain)), 1.)
    carrier = np.resize(grain, len(t)) + .65*np.resize(np.roll(grain, len(grain)//2), len(t))
    carrier += .45*np.tanh(3*np.sin(2*np.pi*freq*t + 1.4*np.sin(2*np.pi*freq*.5*t)))
    result = np.zeros_like(t)
    edges = np.geomspace(140, 6400, 16)
    for lo, hi in zip(edges, edges[1:]):
        shape = gaussian_filter1d(np.abs(band(word, lo, hi)), SR*.004)
        result += level(band(carrier, lo, hi), 1.)*shape
    return level(np.tanh(2.8*level(result, .22)), .16)


def handshake(t, beat, rng):
    # An answer tone with phase reversals breaks into paired carriers, FSK chatter and training noise.
    phases = np.floor(t/.45).astype(int)%2
    answer = np.cos(2*np.pi*2100*t + np.pi*phases)
    bit = np.repeat(rng.integers(0, 2, int(len(t)/200)+1), 200)[:len(t)]
    chatter = np.sin(2*np.pi*np.cumsum(1180 + 670*bit)/SR)
    noise = level(band(rng.normal(size=len(t)), 800, 3100), .7)
    train = np.minimum(1, t/(12*beat))
    pulse = (.5+.5*np.cos(2*np.pi*t/(beat*.5)))**3
    return (1-train)*answer + train*((.5+.5*pulse)*chatter + .4*pulse*noise)


def main():
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument('score', type=Path)
    p.add_argument('output', type=Path)
    args = p.parse_args()
    out = args.output.resolve()
    out.mkdir(parents=True, exist_ok=False)
    lines = args.score.read_text().splitlines()
    fields = [l.split() for l in lines]
    samples = {int(f[1]):Path(f[2]) for f in fields if f and f[0]=='smp'}
    ids = {v.stem:k for k,v in samples.items()}
    events = {int(f[1]):float(f[2]) for f in fields if f and f[0]=='sung'}
    screams = [path for path in samples.values() if path.name.startswith('scream-') and path.stat().st_size > 12000]
    bpm = float(next(f[2] for f in fields if f[:2]==['cfg','bpm']))
    beat = 60/bpm
    next_id = max(samples)+1
    defs, notes = [], []
    def add(name, data, at, gain, pan, wet):
        nonlocal next_id
        path = out/(name+'.f32')
        data.astype('<f4').tofile(path)
        defs.append(f'smp {next_id} {path}')
        notes.append(f'sung {next_id} {at:.6f} {len(data)/SR:.6f} {gain:.6f} {pan} 0 {wet} 0')
        next_id += 1
    for i, (colour, freq) in enumerate(zip(COLOURS, FUNDAMENTALS)):
        word_id = ids[colour+'-word']
        raw = np.fromfile(samples[word_id], dtype='<f4')
        sustain = np.fromfile(samples[ids[colour+'-right']], dtype='<f4')
        # Let the word ring over the following entrance, moving its consonant ending to the release.
        length = round((9 if i<7 else 12)*beat*SR)
        t = np.arange(length)/SR
        splice = min(round(.25*SR), len(raw)//2)
        cross = round(.085*SR)
        tail = raw[splice:]
        vowel = level(sustain[SR:SR+length], .13)
        lead = vowel.copy()
        lead[:splice] = raw[:splice]
        blend = np.linspace(0, 1, cross)
        lead[splice:splice+cross] = (1-blend)*raw[splice:splice+cross] + blend*vowel[splice:splice+cross]
        lead[-len(tail):] = np.linspace(1,0,len(tail))*lead[-len(tail):] + np.linspace(0,1,len(tail))*tail
        at = events[word_id]
        add(colour+'-legato', lead*envelope(t,length/SR,.012,1.25), at, 1.18, (-1)**i*.15, .24)
        voice = np.fromfile(screams[i%len(screams)], dtype='<f4')
        rasp = scream_word(lead, voice, freq)
        add(colour+'-scream', rasp*envelope(t,length/SR,.025,1.55), at+.035, .69+.105*i, (-1)**i*.33, .22)
    start = events[ids['lip-red-word']]
    end = events[ids['lip-black-word']]+13*beat
    t = np.arange(round((end-start)*SR))/SR
    tone = handshake(t,beat,np.random.default_rng(56000))
    add('modem-training', tone*envelope(t,end-start,.4,2*beat),start,.038,.08,.17)
    # Retain both sustained voices per colour from the choir score and replace its short centre words.
    replaced = {ids[c+'-word'] for c in COLOURS}
    kept = [l for l,f in zip(lines, fields) if not (f and f[0]=='sung' and int(f[1]) in replaced)]
    first = next(i for i,l in enumerate(kept) if l.startswith('sung '))
    (out/'modem.score.txt').write_text('\n'.join(kept[:first]+defs+notes+kept[first:])+'\n')
    shutil.copyfile(args.score.parent/'wattajetta-cutie.width.json',out/'wattajetta-cutie.width.json')

if __name__ == '__main__':
    main()
