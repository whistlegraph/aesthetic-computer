#!/usr/bin/env python3
"""Check production mouth timing and render its actual AppKit drawings."""
from pathlib import Path
import subprocess
import tempfile

repo = Path(__file__).resolve().parents[4]
source = repo / 'slab/menuband/Sources/MenuBand'
out = Path('/Users/jas/Shelf/macneopolitan-doowop/lipsync')
out.mkdir(parents=True, exist_ok=True)
stubs = '''
enum LyricCaption { static func font(_ size: CGFloat) -> NSFont { NSFont.systemFont(ofSize: size, weight: .bold) } }
'''
checks = r'''
let voiced = [Float](repeating: 0.15, count: 48000)
let hum = SingerArticulation(units: [.init(syllable: "hmm", start: 0, vowelStart: 0.1, vowelEnd: 0.8, end: 1)], samples: voiced, sampleRate: 48000)
assert(hum.pose(at: 0.5).seal == 1, "A voiced hum must keep lips shut")
let boom = SingerArticulation(units: [.init(syllable: "boom", start: 0.1, vowelStart: 0.2, vowelEnd: 0.8, end: 0.9)], samples: voiced, sampleRate: 48000)
assert(boom.cues.map(\.shape) == [.closed, .oo, .closed])
assert(boom.pose(at: 0.13).seal > 0.95, "B closure before vowel")
assert(boom.pose(at: 0.4).round == 1, "Sustained OO, not generic amplitude flap")
assert(boom.pose(at: 0.81).seal > 0.95, "M coda seals the lips while voice continues")
assert(!boom.active(at: 1.2) && boom.level(at: 1.2) == 0)
let la = SingerArticulation(units: [.init(syllable: "la", start: 0.15, vowelStart: 0.25, vowelEnd: 0.8, end: 0.9)], samples: voiced, sampleRate: 48000)
assert(la.pose(at: 0.17).tongue > 0.9)
assert(la.pose(at: 0.4).jaw > 0.9)
assert(la.active(at: 0.12), "One frame of visual anticipation")
let silent = SingerArticulation(units: [], samples: [Float](repeating: 0, count: 48000), sampleRate: 48000)
assert(silent.pose(at: 0.5).seal == 1 && silent.level(at: 0.5) == 0)
let expected: [(String, SingerViseme)] = [("doo", .oo), ("wah", .ah), ("bee", .ee), ("fa", .ah), ("la", .ah), ("oh", .oh), ("eh", .eh), ("hmm", .hum)]
for (text, shape) in expected { assert(SingerArticulation.sounds(text).1 == shape, text) }
let distinct = Set(SingerViseme.allCases.map { String(describing: $0.pose) })
assert(distinct.count == 15, "Every key drawing must be distinct")

_ = NSApplication.shared
let breathFace = SingerFace(), breathSlot = NSObject()
var breathClock = boom.cues[0].start - 0.19
breathFace.follow(boom, slot: ObjectIdentifier(breathSlot)) { breathClock }
assert(breathFace.inhale() > 0.99, "Visual intake peaks before the first consonant")
breathClock = boom.cues[0].start + 0.1
assert(breathFace.inhale() == 0, "Intake finishes when the phrase begins")
breathFace.clearArticulation()
assert(breathFace.inhale() == 0, "Stopped singers cannot keep inhaling")
let output = CommandLine.arguments[1]
let members: [(String, NSColor)] = [("neo", NSColor(srgbRed: 143.0/255, green: 209.0/255, blue: 63.0/255, alpha: 1)), ("blueberry", NSColor(srgbRed: 90.0/255, green: 87.0/255, blue: 211.0/255, alpha: 1)), ("frisbee", NSColor(srgbRed: 242.0/255, green: 167.0/255, blue: 185.0/255, alpha: 1))]
for (member, color) in members {
    let cellW = 360, cellH = 260
    let bitmap = NSBitmapImageRep(bitmapDataPlanes: nil, pixelsWide: cellW*5, pixelsHigh: cellH*3, bitsPerSample: 8, samplesPerPixel: 4, hasAlpha: true, isPlanar: false, colorSpaceName: .deviceRGB, bytesPerRow: 0, bitsPerPixel: 0)!
    NSGraphicsContext.saveGraphicsState()
    NSGraphicsContext.current = NSGraphicsContext(bitmapImageRep: bitmap)
    for (i, shape) in SingerViseme.allCases.enumerated() {
        NSGraphicsContext.saveGraphicsState()
        let transform = NSAffineTransform()
        transform.translateX(by: CGFloat(i%5*cellW), yBy: CGFloat((2-i/5)*cellH))
        transform.concat()
        let face = SingerFaceView(frame: NSRect(x: 0, y: 0, width: cellW, height: cellH))
        face.member = member; face.accent = color; face.previewPose = shape.pose
        face.previewExpression(beat: Double(i)/2, effort: CGFloat(i%3)/2, breath: i%3 == 0 ? 0.7 : 0)
        face.draw(face.bounds)
        let label = NSAttributedString(string: shape.rawValue.uppercased(), attributes: [.font: NSFont.systemFont(ofSize: 32, weight: .bold), .foregroundColor: NSColor.black])
        label.draw(at: NSPoint(x: 16, y: 12))
        NSGraphicsContext.restoreGraphicsState()
    }
    NSGraphicsContext.restoreGraphicsState()
    try bitmap.representation(using: .png, properties: [:])!.write(to: URL(fileURLWithPath: "\(output)/\(member)-mouth-chart.png"))
}
print("15 distinct poses; closure, hum, tongue, vowel hold, silence, anticipation and phrase-inhale checks passed. AppKit charts rendered.")
'''
with tempfile.TemporaryDirectory(prefix='menuband-lipsync-') as tmp:
    p = Path(tmp) / 'check.swift'
    p.write_text((source/'SingerArticulation.swift').read_text() + '\n' + (source/'SingerFaceMetal.swift').read_text() + '\n' + (source/'SingerFace.swift').read_text() + stubs + checks)
    subprocess.run(['swift', str(p), str(out)], check=True)
