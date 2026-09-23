#!/usr/bin/env python3
"""Regression checks for measured pitch and the production word-marker resolver."""
from pathlib import Path
import subprocess
import tempfile
import numpy as np
from pitch_audit import measured_hz

sr = 44100
t = np.arange(round(.4*sr))/sr
for hz in (55, 61.735, 110, 174.614, 261.626, 440, 880, 1400):
    wave = np.sin(2*np.pi*hz*t) + .2*np.sin(4*np.pi*hz*t)
    assert abs(1200*np.log2(measured_hz(wave, sr)/hz)) < 3
assert measured_hz(np.zeros(sr), sr) is None
assert abs(1200*np.log2(measured_hz(np.sin(2*np.pi*440*t),sr)/220)-1200) < 3

repo = Path(__file__).resolve().parents[4]
source = (repo / "slab/menuband/Sources/MenuBand/MenuBandSinger.swift").read_text()
method = source.split("// BEGIN WORD SPAN RESOLVER")[1].split("\n",1)[1].split("// END WORD SPAN RESOLVER")[0]
checks = r'''
let text = "I run warm and I am carried"
let starts = [0, 4096, 10240, 17664, 20992, 27904]
let ranges = [NSRange(location:0,length:1),NSRange(location:2,length:3),NSRange(location:6,length:4),NSRange(location:11,length:3),NSRange(location:15,length:4),NSRange(location:20,length:7)]
let marks = zip(ranges,starts).map { (range:$0.0,sample:$0.1) }
let s = Check.wordSpans(text:text,syllables:[1,1,1,1,1,1,2],markers:marks,sampleCount:36000)!
assert(s.count == 7 && s[1].a == 4096 && s[2].a == 10240 && s[6].a == 27904 && s[6].b == 36000)
assert(s[4].a == 20992 && s[4].b == s[5].a && s[5].b == 27904)
assert(Check.wordSpans(text:text,syllables:[1,1,1,1,1,1,2],markers:Array(marks.dropFirst()),sampleCount:36000) == nil)
assert(Check.wordSpans(text:text,syllables:[1,1,1,1,1,1,2],markers:marks,sampleCount:1000) == nil)
let punct: [(range:NSRange,sample:Int)] = [(.init(location:0,length:7),0),(.init(location:7,length:1),100),(.init(location:9,length:5),200)]
let p = Check.wordSpans(text:"whistle, radio",syllables:[2,2],markers:punct,sampleCount:400)!
assert(p.count == 2 && p[0].a == 0 && p[0].b == 200 && p[1].a == 200)
let compound: [(range:NSRange,sample:Int)] = [(.init(location:0,length:4),0),(.init(location:4,length:4),100),(.init(location:9,length:5),200)]
let c = Check.wordSpans(text:"downbeat three",syllables:[2,1],markers:compound,sampleCount:400)!
assert(c.count == 2 && c[0].a == 0 && c[0].b == 200)
'''
with tempfile.TemporaryDirectory() as tmp:
    test = Path(tmp) / "check.swift"
    test.write_text("import Foundation\nenum Check {\n"+method+"\n}\n"+checks)
    subprocess.run(["swift",str(test)],check=True)
print("Pitch, silence, octave-error, grouped-word, punctuation, compound-word, and missing-timing checks passed.")
