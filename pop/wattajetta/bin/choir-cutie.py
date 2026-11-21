#!/usr/bin/env python3
"""Prepare the optional choir arrangement from a baked rock score and its own performances.

pop/.venv/bin/python pop/wattajetta/bin/choir-cutie.py INPUT.score.txt NEW_DIR
Keep the matching .cutie-line-jobs.json and wattajetta-cutie.width.json beside the baked score.
Render NEW_DIR/choir.score.txt with c/run-cutie.mjs --accel 124 --out NEW.mp3.
The two softer versions of Prutti's cadence are also written as standalone WAVs.
"""
import argparse
import json
from pathlib import Path

import numpy as np
import pyworld as world
from scipy.io import wavfile
from scipy.ndimage import gaussian_filter1d
from scipy.signal import resample_poly

SR = 48000
ANALYSIS_RATE = 16000
FRAME_MS = 5
COLOURS = ["lip-red", "lip-orange", "lip-yellow", "lip-green", "lip-blue", "lip-purple", "lip", "lip-black"]
# Keep orange at B2. The second singer doubles the opening bass notes an octave up.
NOTES = [40, 47, 52, 56, 59, 64, 66, 68]


def hz(midi):
    return 440 * 2 ** ((midi - 69) / 12)


def analyze(raw):
    y = np.ascontiguousarray(resample_poly(raw.astype("float64"), 1, 3))
    f0, t = world.harvest(y, ANALYSIS_RATE, f0_floor=55, f0_ceil=650, frame_period=FRAME_MS)
    f0 = world.stonemask(y, f0, t, ANALYSIS_RATE)
    return t, f0, world.cheaptrick(y, f0, t, ANALYSIS_RATE), world.d4c(y, f0, t, ANALYSIS_RATE)


def synth(f0, shape, air):
    audio = world.synthesize(np.ascontiguousarray(f0), np.ascontiguousarray(shape),
                             np.ascontiguousarray(air), ANALYSIS_RATE, FRAME_MS)
    return resample_poly(audio, 3, 1)


def rms(x):
    return np.sqrt(np.mean(x ** 2)) if len(x) else 0.


def level(x, target):
    return x * (target / max(rms(x), 1e-9))


def colour_hold(take, length, note, side):
    # The mouth shape comes from the stressed colour vowel, ahead of the "-est" suffix.
    # Use a run of voiced frames; mixing different consonants into it would pulse every cycle.
    t, pitch, shape, air = take
    core = np.flatnonzero((pitch > 0) & (t > .055) & (t < .39))
    if not len(core):
        raise ValueError("No voiced colour vowel in the first syllable")
    runs = np.split(core, np.flatnonzero(np.diff(core) > 1) + 1)
    core = max(runs, key=len)
    phase = .18 if side < 0 else .69
    times = np.arange(0, length + .01, FRAME_MS / 1000)
    position = .5 + .43 * np.sin(2 * np.pi * (.39 * times + phase))
    ix = core[np.rint(position * (len(core) - 1)).astype(int)]
    # Slow breath and small independent changes of pitch let the two singers move.
    vibrato = np.minimum(1, times / .7) * (8 * np.sin(2*np.pi*(4.8+.12*side)*times + phase)
                                          + 4 * np.sin(2*np.pi*.61*times + phase))
    f0 = hz(note) * np.exp2((vibrato + 3 * side) / 1200)
    smoothed = gaussian_filter1d(shape[core], 3, axis=0)
    timbre = smoothed[ix - core[0]]
    y = synth(f0, timbre, air[ix])[:round(length * SR)]
    seconds = np.arange(len(y)) / SR
    env = (np.minimum(1, seconds / .6) * np.minimum(1, (length-seconds) / .8)
           * (.72 + .22 * np.sin(1.36 * seconds + phase) ** 2))
    return level(y, .10) * env


def word_at_note(raw, take, note):
    t, f0, sp, ap = take
    mask = f0 > 0
    centre = np.median(f0[mask])
    cents = np.clip(1200*np.log2(f0[mask]/centre), -130, 130)
    f = f0.copy()
    f[mask] = hz(note) * np.exp2(.24 * cents / 1200)
    y = synth(f, sp, ap)
    y = np.pad(y[:len(raw)], (0, max(0, len(raw)-len(y))))
    # Keep the word's high consonants from the recorded performance.
    low = resample_poly(resample_poly(raw, 1, 3), 3, 1)[:len(raw)]
    return level(y + raw - low, rms(raw))


def super_phrase(raw, bounds, notes, beats, endings, beat, tone):
    # Durations and the chord are independent for each interpretation. Preserve each consonant.
    y = np.zeros(round((beats[-1] + endings[-1] + .1) * beat * SR))
    for i, ((a,b), note, at, duration) in enumerate(zip(bounds, notes, beats, endings)):
        syll = raw[round(max(0,a-.018)*SR):round((b+.025)*SR)]
        t, f0, sp, ap = analyze(syll)
        voiced = np.flatnonzero(f0>0)
        if len(voiced)<2:
            raise ValueError(f"No voiced interval in superstar syllable {i+1}")
        # Most of the added time belongs to the vowel; both consonant edges keep their duration.
        v0,v1 = voiced[len(voiced)//5],voiced[len(voiced)*4//5]
        end = max(duration*beat, t[v0] + t[-1] - t[v1] + .018)
        ts=np.arange(0,end,FRAME_MS/1000)
        src=np.interp(ts,[0,t[v0],end-(t[-1]-t[v1]),end],[0,t[v0],t[v1],t[-1]])
        ix=np.clip(np.rint(src*1000/FRAME_MS).astype(int),0,len(f0)-1)
        f=f0[ix].copy()
        v=f>0
        f[v] = hz(note)*np.exp2((5*np.sin(2*np.pi*4.7*ts[v])
                               + np.clip(1200*np.log2(f[v]/np.median(f0[voiced])), -120, 120)*.14)/1200)
        segment=synth(f,sp[ix],ap[ix])[:round(end*SR)]
        env=np.minimum(1,np.arange(len(segment))/(.012*SR))*np.minimum(1,np.arange(len(segment))[::-1]/(.035*SR))
        segment=level(segment, rms(syll))*env
        start=round(at*beat*SR)
        n=min(len(segment),len(y)-start)
        y[start:start+n]+=segment[:n]
    # Softer variants have gentler transients; retain high-frequency articulation in the lead.
    return level(tone*y+(1-tone)*gaussian_filter1d(y,3), .12)


def write_sample(path, x):
    np.asarray(x,dtype="<f4").tofile(path)


def main():
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument("score",type=Path);parser.add_argument("output",type=Path)
    args=parser.parse_args()
    out=args.output.resolve(); out.mkdir(exist_ok=False,parents=True)
    score=args.score.resolve(); lines=score.read_text().splitlines()
    table={int(v[1]):Path(v[2]).resolve() for line in lines if line.startswith("smp ") for v in [line.split(maxsplit=2)]}
    sample_ids={v.name:k for k,v in table.items()}; first_unused=max(table)+1
    jobs=json.loads((score.parent/".cutie-line-jobs.json").read_text())["jobs"]
    replacements={}
    timing={}
    def place(old_id, tracks):
        nonlocal first_unused
        puts=[]
        for name,sample,delay,gain,pan,wet in tracks:
            path=out/(name+".f32"); write_sample(path,sample)
            puts.append((first_unused,path,delay,gain,pan,wet))
            first_unused+=1
        replacements[old_id]=puts
    for i,(colour,note) in enumerate(zip(COLOURS,NOTES)):
        jobs_for_colour=[j for j in jobs if j["line"]==colour and set(j["midis"])=={note}]
        onset=next(j for j in jobs_for_colour if j["end_hold"]<2)
        sustain=next(j for j in jobs_for_colour if j["end_hold"]>4)
        word_id=sample_ids[onset["key"]+"-from2.f32"]
        hold_id=sample_ids[sustain["key"]+f"-from{len(sustain['starts'])-1}p.f32"]
        src=np.fromfile(table[word_id],dtype="<f4")
        take=analyze(src)
        lead=word_at_note(src,take,note+12 if i==0 else note)
        place(word_id, [(colour+"-word",lead,0,.86,0,.08)])
        length=table[hold_id].stat().st_size/(4*SR)
        lower=colour_hold(take,length,note,-1); upper=colour_hold(take,length,note+(12 if i<2 else 0),1)
        # Let later notes gather into the chord without eight full-volume voices underneath them.
        gain=.84 if i<3 else .67
        place(hold_id, [(colour+"-left",lower,0,gain,-.53,.32),
                        (colour+"-right",upper,.032,gain*.73,.55,.39)])
        if colour=="lip-orange":
            _,measured,_,_=analyze(lower[SR*2:SR*6])
            pitch=np.median(measured[measured>0]); timing["orange_hz"]=round(float(pitch),2)
            timing["orange_cents"]=round(float(1200*np.log2(pitch/hz(note))),1)
    cameo=sample_ids["raw-prutti-superstar-angel.f32"]
    raw=np.fromfile(table[cameo],dtype="<f4")
    bounds=[(.03,.27),(.35,.51),(.59,.75),(.91,1.038),(1.038,1.23),(1.39,1.79)]
    # The cameo lands over Am/E then turns toward B7. Its last word has room to ring.
    beat=60/float(next(l.split()[-1] for l in lines if l.startswith("cfg bpm ")))
    beats=[0,.75,1.25,2,2.5,3.25]; durations=[.65,.45,.45,.45,.6,1.5]
    interpretations=[("rise", [52,57,60,64,69,71],beats,durations,1.),
                     ("soft", [52,57,60,64,60,59],beats,durations,.57),
                     ("soft-low", [52,52,57,60,57,52],[0,1,1.5,2.25,2.75,3.5],durations,.38)]
    phrases={}
    for name,notes,attacks,holds,tone in interpretations:
        melody=super_phrase(raw,bounds,notes,attacks,holds,beat,tone)
        phrases[name]=melody
        wavfile.write(out/("superstar-"+name+".wav"),SR,(np.clip(melody*(1 if name=="rise" else .64 if name=="soft" else .44),-1,1)*32767).astype("<i2"))
    place(cameo,[("superstar-lead",phrases["rise"],0,.94,-.12,.32),
                 ("superstar-soft-answer",phrases["soft"],beat*4.5,.43,.39,.46)])
    written=[]
    for line in lines:
        cols=line.split()
        if cols[0]=="smp" and int(cols[1]) in replacements:
            written.extend(f"smp {k} {p}" for k,p,*_ in replacements[int(cols[1])]); continue
        if cols[0]=="sung" and int(cols[1]) in replacements:
            for k,p,delay,gain,pan,wet in replacements[int(cols[1])]:
                written.append(f"sung {k} {float(cols[2])+delay:.6f} {p.stat().st_size/(4*SR):.6f} {float(cols[4])*gain:.7f} {pan} 0 {wet} {cols[8]}")
            if int(cols[1])==cameo:timing["superstar_score_seconds"]=round(float(cols[2]),2)
            continue
        written.append(line)
    (out/"choir.score.txt").write_text("\n".join(written)+"\n")
    (out/"wattajetta-cutie.width.json").write_bytes((score.parent/"wattajetta-cutie.width.json").read_bytes())
    (out/"notes.json").write_text(json.dumps(timing,indent=2)+"\n")
    print(json.dumps(timing))

if __name__=="__main__":
    main()
