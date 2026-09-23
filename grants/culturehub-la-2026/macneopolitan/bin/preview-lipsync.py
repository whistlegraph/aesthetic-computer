#!/usr/bin/env python3
"""Render the actual AppKit mouth rig against singrender's exported cues/WAV."""
from pathlib import Path
import subprocess
import tempfile

repo = Path(__file__).resolve().parents[4]
root = Path('/Users/jas/Shelf/macneopolitan-doowop/lipsync')
source = repo/'slab/menuband/Sources/MenuBand'
swift = r'''
import AVFoundation
enum LyricCaption { static func font(_ size: CGFloat) -> NSFont { NSFont.systemFont(ofSize: size, weight: .bold) } }
_ = NSApplication.shared
let root = CommandLine.arguments[1]
let data = try Data(contentsOf: URL(fileURLWithPath: root + "/render-audit/manifest.json"))
let manifest = try JSONSerialization.jsonObject(with: data) as! [String: Any]
let line = (manifest["lines"] as! [[String: Any]])[0]
let wave = line["wav"] as! String
let file = try AVAudioFile(forReading: URL(fileURLWithPath: wave))
let audio = AVAudioPCMBuffer(pcmFormat: file.processingFormat, frameCapacity: AVAudioFrameCount(file.length))!
try file.read(into: audio)
let samples = Array(UnsafeBufferPointer(start: audio.floatChannelData![0], count: Int(audio.frameLength)))
let envelope = SingerArticulation(units: [], samples: samples, sampleRate: audio.format.sampleRate)
let cues = (line["mouthCues"] as! [[String: Any]]).map { SingerArticulation.Cue(start: $0["start"] as! Double, end: $0["end"] as! Double, shape: SingerViseme(rawValue: $0["shape"] as! String)!) }
let articulation = SingerArticulation(cues: cues, energy: envelope.energy, duration: envelope.duration)
let w = 960, h = 600, fps = 60
let encoder = Process(), pipe = Pipe()
encoder.executableURL = URL(fileURLWithPath: "/opt/homebrew/bin/ffmpeg")
encoder.arguments = ["-y", "-loglevel", "error", "-f", "rawvideo", "-pix_fmt", "rgba", "-s", "\(w)x\(h)", "-r", "\(fps)", "-i", "-", "-i", wave, "-c:v", "libx264", "-crf", "18", "-pix_fmt", "yuv420p", "-c:a", "aac", "-shortest", "-movflags", "+faststart", root + "/articulation-preview.mp4"]
encoder.standardInput = pipe
try encoder.run()
let face = SingerFaceView(frame: NSRect(x: 0, y: 0, width: w, height: h))
face.member = "neo"; face.accent = NSColor(srgbRed: 143.0/255, green: 209.0/255, blue: 63.0/255, alpha: 1)
for frame in 0..<Int(ceil(articulation.duration * Double(fps))) {
    let bitmap = NSBitmapImageRep(bitmapDataPlanes: nil, pixelsWide: w, pixelsHigh: h, bitsPerSample: 8, samplesPerPixel: 4, hasAlpha: true, isPlanar: false, colorSpaceName: .deviceRGB, bytesPerRow: w*4, bitsPerPixel: 32)!
    NSGraphicsContext.saveGraphicsState()
    NSGraphicsContext.current = NSGraphicsContext(bitmapImageRep: bitmap)
    let time = Double(frame)/Double(fps)
    face.previewPose = articulation.pose(at: time)
    let next = cues.first { $0.start > time }
    let lead = next.map { $0.start-time } ?? 9
    let breath = lead < 0.38 ? sin(Double.pi*(1-lead/0.38)) : 0
    face.previewExpression(beat: time*88/60, effort: CGFloat(articulation.level(at: time)), breath: CGFloat(breath))
    face.draw(face.bounds)
    NSGraphicsContext.restoreGraphicsState()
    pipe.fileHandleForWriting.write(Data(bytes: bitmap.bitmapData!, count: w*h*4))
}
try pipe.fileHandleForWriting.close()
encoder.waitUntilExit()
assert(encoder.terminationStatus == 0)
print("Rendered 60 fps mouth animation against the exact sung WAV.")
'''
with tempfile.TemporaryDirectory(prefix='menuband-mouth-preview-') as tmp:
    p = Path(tmp)/'preview.swift'
    p.write_text((source/'SingerArticulation.swift').read_text()+'\n'+(source/'SingerFaceMetal.swift').read_text() + '\n' + (source/'SingerFace.swift').read_text()+'\n'+swift)
    subprocess.run(['swift', str(p), str(root)], check=True)
