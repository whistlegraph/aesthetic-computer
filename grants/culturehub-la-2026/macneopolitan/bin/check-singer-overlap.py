#!/usr/bin/env python3
"""Exercise production phrase scheduling, silently, through an AVAudioEngine.

Two constant test buffers must overlap at the requested wall-clock times.
The output mixer is muted; a preceding mixer measures the actual audio.
"""
from pathlib import Path
import subprocess
import tempfile

repo=Path(__file__).resolve().parents[4]
source=(repo/'slab/menuband/Sources/MenuBand/MenuBandSingerVoice.swift').read_text()
stubs='''
import AVFoundation
final class SingerFace {
    static let shared = SingerFace()
    func meter(rms: CGFloat, zcr: CGFloat) {}
    var clocks: [ObjectIdentifier: () -> Double?] = [:]
    func follow(_ articulation: SingerArticulation, slot: ObjectIdentifier, clock: @escaping () -> Double?) { clocks[slot] = clock }
    func clearArticulation() { clocks.removeAll() }
}
struct SungRender { let buffer: AVAudioPCMBuffer; var articulation: SingerArticulation? = nil }
enum MenuBandSinger {
    static func convert(_ pcm: AVAudioPCMBuffer, to format: AVAudioFormat) -> AVAudioPCMBuffer? { pcm }
}
'''
checks='''
let engine = AVAudioEngine()
let meter = AVAudioMixerNode()
engine.attach(meter)
let voice = MenuBandSingerVoice()
engine.connect(meter, to: engine.mainMixerNode, format: voice.format)
voice.attach(to: engine, output: meter)
engine.mainMixerNode.outputVolume = 0
let lock = NSLock()
var overlapped = false
var ended = false
let start = Date().timeIntervalSince1970 + 0.2
meter.installTap(onBus: 0, bufferSize: 1024, format: nil) { buffer, _ in
    guard let channel = buffer.floatChannelData?[0] else { return }
    let now = Date().timeIntervalSince1970 - start
    let peak = (0..<Int(buffer.frameLength)).map { abs(channel[$0]) }.max() ?? 0
    lock.lock(); defer { lock.unlock() }
    if now > 0.6 && now < 1.0 && peak > 0.14 { overlapped = true }
    if now > 1.25 && now < 1.5 && peak < 0.06 && peak > 0.04 { ended = true }
}
func tone(_ value: Float, _ seconds: Double) -> SungRender {
    let n = AVAudioFrameCount(seconds * voice.format.sampleRate)
    let b = AVAudioPCMBuffer(pcmFormat: voice.format, frameCapacity: n)!
    b.frameLength = n
    for i in 0..<Int(n) { b.floatChannelData![0][i] = value }
    return SungRender(buffer: b, articulation: SingerArticulation(units: [], samples: [Float](repeating: value, count: Int(n)), sampleRate: voice.format.sampleRate))
}
try engine.start()
voice.schedule(tone(0.05, 1.8), atEpoch: start)
voice.schedule(tone(0.10, 0.6), atEpoch: start + 0.5)
RunLoop.main.run(until: Date(timeIntervalSince1970: start + 0.8))
let clockTimes = voice.face.clocks.values.compactMap { $0() }.sorted()
assert(clockTimes.count == 2 && abs(clockTimes[0] - 0.3) < 0.10 && abs(clockTimes[1] - 0.8) < 0.10, "Face must track each actual player clock: \(clockTimes)")
RunLoop.main.run(until: Date(timeIntervalSince1970: start + 1.65))
voice.schedule(tone(0.02, 0.15), atEpoch: start + 1.8)
voice.schedule(tone(0.03, 0.3), atEpoch: start + 2.0)
RunLoop.main.run(until: Date(timeIntervalSince1970: start + 2.15))
let reusedTimes = voice.face.clocks.values.compactMap { $0() }.sorted()
assert(reusedTimes.count == 3 && abs(reusedTimes[0] - 0.15) < 0.10, "Reused phrase player must reset its mouth clock")
voice.stop(); engine.stop()
assert(voice.face.clocks.isEmpty, "Stop must cancel mouth playback too")
lock.lock(); let ok = overlapped && ended; lock.unlock()
assert(ok, "Phrase overlap drift: second buffer must sound at +0.5s and end at +1.1s")
print("Production singer: overlapping buffers began and ended on schedule; output muted.")
'''
with tempfile.TemporaryDirectory(prefix='menuband-overlap-') as d:
    articulation=(repo/'slab/menuband/Sources/MenuBand/SingerArticulation.swift').read_text()
    script=Path(d)/'check.swift'; script.write_text(articulation+stubs+source+checks)
    subprocess.run(['swift',str(script)],check=True)
