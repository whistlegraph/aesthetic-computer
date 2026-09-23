#!/usr/bin/env python3
"""Measure a live pitch slide through the production singer, with muted output."""
from pathlib import Path
import ast
import json
import subprocess
import tempfile
import numpy as np

repo=Path(__file__).resolve().parents[4]
source=repo/'slab/menuband/Sources/MenuBand'
tree=ast.parse(Path(__file__).with_name('check-singer-overlap.py').read_text())
stubs=next(ast.literal_eval(n.value) for n in tree.body if isinstance(n,ast.Assign) and any(isinstance(t,ast.Name) and t.id=='stubs' for t in n.targets))
check=r'''
let engine = AVAudioEngine(), meter = AVAudioMixerNode(), voice = MenuBandSingerVoice()
engine.attach(meter)
engine.connect(meter, to: engine.mainMixerNode, format: voice.format)
voice.attach(to: engine, output: meter)
engine.mainMixerNode.outputVolume = 0
let lock = NSLock()
var recording: [[Double]] = []
let start = Date().timeIntervalSince1970 + 0.4
meter.installTap(onBus: 0, bufferSize: 512, format: nil) { buffer, _ in
    guard let samples = buffer.floatChannelData?[0] else { return }
    let t = Date().timeIntervalSince1970-start
    lock.lock()
    recording.append([t] + (0..<Int(buffer.frameLength)).map { Double(samples[$0]) })
    lock.unlock()
}
try engine.start()
let n = AVAudioFrameCount(voice.format.sampleRate*2)
let buffer = AVAudioPCMBuffer(pcmFormat: voice.format, frameCapacity: n)!
buffer.frameLength = n
for i in 0..<Int(n) { buffer.floatChannelData![0][i] = Float(0.12*sin(2*Double.pi*440*Double(i)/voice.format.sampleRate)) }
voice.schedule(SungRender(buffer: buffer), atEpoch: start)
DispatchQueue.main.asyncAfter(deadline: .now() + max(0,start-Date().timeIntervalSince1970)+0.8) { voice.setBend(amount: 1) }
RunLoop.main.run(until: Date(timeIntervalSince1970: start+2.3))
voice.stop(); engine.stop()
lock.lock(); let captured = recording; lock.unlock()
try JSONSerialization.data(withJSONObject: captured).write(to: URL(fileURLWithPath: CommandLine.arguments[1]))
'''
with tempfile.TemporaryDirectory(prefix='menuband-slide-') as tmp:
    tmp=Path(tmp); script=tmp/'check.swift'; recorded=tmp/'audio.json'
    script.write_text((source/'SingerArticulation.swift').read_text()+stubs+(source/'MenuBandSingerVoice.swift').read_text()+check)
    subprocess.run(['swift',str(script),str(recorded)],check=True)
    rows=json.loads(recorded.read_text())
    results=[]
    for a,b,want in [(.3,.65,440),(1.25,1.65,880)]:
        samples=np.concatenate([np.asarray(r[1:]) for r in rows if a<r[0]<b])
        n=131072
        mag=np.abs(np.fft.rfft(samples*np.hanning(len(samples)),n))
        hz=np.fft.rfftfreq(n,1/44100)[np.argmax(mag)]
        assert abs(1200*np.log2(hz/want))<8,(hz,want)
        results.append(round(float(hz),2))
    tail=np.concatenate([np.asarray(r[1:]) for r in rows if r[0]>2.18])
    assert np.max(np.abs(tail))<.01,'Pitch-only slide must preserve phrase length'
    print('Production live slide measured:',results,'Hz; duration preserved; output muted.')
