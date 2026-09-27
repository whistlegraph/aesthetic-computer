#!/usr/bin/env python3
"""Check score slide curves against the production Swift implementation."""
from pathlib import Path
import subprocess
import tempfile
source=Path(__file__).resolve().parents[4]/'slab/menuband/Sources/MenuBand/SingerPerformance.swift'
checks=r'''
let score = SingerPerformance(keys: [.init(beat:0,space:0.2,pitch:-2),.init(beat:4,space:0.4,pitch:2)], expression:0.8)
assert(abs(score.axes(at:2).x + 0.3) < 0.0001 && abs(score.axes(at:2).y) < 0.0001)
assert(score.axes(at:-1).y == -2.0/12 && score.axes(at:9).y == 2.0/12)
let limited = SingerPerformance(keys: [.init(beat:0,space:2,pitch:99)], expression:1)
assert(limited.axes(at:0).x == -1 && limited.axes(at:0).y == 2)
assert(SingerPerformance.decode("not-base64") == nil)
let echo = SingerPerformance(keys: [.init(beat:0,space:0.3,pitch:0), .init(beat:2,space:0,pitch:0,echo:0.6), .init(beat:4,space:0,pitch:0)], expression:1)
assert(abs(echo.axes(at:0).x + 0.3) < 0.0001)
assert(abs(echo.axes(at:2).x - 0.6) < 0.0001)
assert(abs(echo.axes(at:1).x - 0.15) < 0.0001)
assert(echo.axes(at:4).x == 0)
let echoJSON = "{\"expression\":1,\"keys\":[{\"beat\":0,\"space\":0,\"pitch\":0,\"echo\":0.6}]}"
let decoded = SingerPerformance.decode(Data(echoJSON.utf8).base64EncodedString())!
assert(abs(decoded.axes(at:0).x - 0.6) < 0.0001)
let envelope = SingerDynamics(notation:"r:8,60:1,62:1,64:4",bpm:60,gains:[1,0.2,0.8],offset:7)
assert(envelope.notes.map { $0.start } == [1,2,3])
assert(envelope.gain(at:1.5) == 1)
assert(abs(envelope.gain(at:2.006)-0.6) < 0.0001)
assert(abs(envelope.gain(at:2.1)-0.2) < 0.0001)
assert(envelope.gain(at:6.99) > 0.35 && envelope.gain(at:6.99) < 0.37)
let gains = (0..<8000).map { envelope.gain(at:Double($0)/1000) }
assert(gains.allSatisfy { $0 >= 0 && $0 <= 1 })
assert(zip(gains,gains.dropFirst()).allSatisfy { abs($0-$1) < 0.11 })
print("Syllable accents, absolute offsets, smooth ramps and held-note fade passed.")
print("Slide interpolation, range and invalid-payload checks passed.")
'''
with tempfile.TemporaryDirectory(prefix='menuband-curves-') as tmp:
    script=Path(tmp)/'check.swift'
    script.write_text(source.read_text()+'\n'+checks)
    subprocess.run(['swift',str(script)],check=True)
