// faces.swift — the three singing faces, offline. Drives Menu Band's own
// SingerFaceView (slab/menuband/Sources/MenuBand/SingerFace.swift, the rig
// the trio performs with) from singrender's mouth cues for every sung line,
// on the record's timeline, and writes one face video per member.
//
//   bin/faces.sh            (compiles this against the MenuBand sources and runs it)
//   → out/faces-<member>.mp4   (W×H, fps, same length as out/eightgigabytes.wav)
import AppKit
import AVFoundation

enum LyricCaption { static func font(_ size: CGFloat) -> NSFont { NSFont.systemFont(ofSize: size, weight: .bold) } }
_ = NSApplication.shared

let args = CommandLine.arguments
let outDir = args[1], fps = Int(args[2]) ?? 30
let W = Int(args[3]) ?? 480, H = Int(args[4]) ?? 300
let tl = try JSONSerialization.jsonObject(with: Data(contentsOf: URL(fileURLWithPath: outDir + "/timeline.json"))) as! [String: Any]
let offset = tl["pre"] as! Double                      // seconds from the file's start to beat 0
let seconds = tl["seconds"] as! Double
let bpm = tl["bpm"] as! Double
let members: [(String, NSColor)] = [("neo", NSColor(srgbRed: 143/255, green: 209/255, blue: 63/255, alpha: 1)),
                                    ("blueberry", NSColor(srgbRed: 90/255, green: 87/255, blue: 211/255, alpha: 1)),
                                    ("frisbee", NSColor(srgbRed: 242/255, green: 167/255, blue: 185/255, alpha: 1))]
let paper = NSColor(srgbRed: 255/255, green: 253/255, blue: 246/255, alpha: 1)

struct Line { let start: Double; let articulation: SingerArticulation; let cues: [SingerArticulation.Cue] }

for (member, accent) in members {
    let manifest = try JSONSerialization.jsonObject(with: Data(contentsOf: URL(fileURLWithPath: "\(outDir)/stems/\(member)/manifest.json"))) as! [String: Any]
    var lines: [Line] = []
    for l in manifest["lines"] as! [[String: Any]] {
        guard let wav = l["wav"] as? String, let span = l["spanOffset"] as? Double else { continue }
        let file = try AVAudioFile(forReading: URL(fileURLWithPath: wav))
        let buf = AVAudioPCMBuffer(pcmFormat: file.processingFormat, frameCapacity: AVAudioFrameCount(file.length))!
        try file.read(into: buf)
        let samples = Array(UnsafeBufferPointer(start: buf.floatChannelData![0], count: Int(buf.frameLength)))
        let env = SingerArticulation(units: [], samples: samples, sampleRate: buf.format.sampleRate)
        let cues = ((l["mouthCues"] as? [[String: Any]]) ?? []).compactMap { c -> SingerArticulation.Cue? in
            guard let s = c["start"] as? Double, let e = c["end"] as? Double, let sh = c["shape"] as? String, let v = SingerViseme(rawValue: sh) else { return nil }
            return SingerArticulation.Cue(start: s, end: e, shape: v) }
        lines.append(Line(start: offset + span, articulation: SingerArticulation(cues: cues, energy: env.energy, duration: env.duration), cues: cues))
    }
    lines.sort { $0.start < $1.start }
    let rest = SingerArticulation(cues: [], energy: [], duration: 0).pose(at: 0)

    let out = "\(outDir)/faces-\(member).mp4"
    let enc = Process(), pipe = Pipe()
    enc.executableURL = URL(fileURLWithPath: "/opt/homebrew/bin/ffmpeg")
    enc.arguments = ["-y", "-loglevel", "error", "-f", "rawvideo", "-pix_fmt", "rgba", "-s", "\(W)x\(H)", "-r", "\(fps)", "-i", "-",
                     "-c:v", "libx264", "-crf", "16", "-pix_fmt", "yuv420p", "-movflags", "+faststart", out]
    enc.standardInput = pipe
    try enc.run()
    let face = SingerFaceView(frame: NSRect(x: 0, y: 0, width: W, height: H))
    face.member = member; face.accent = accent
    let frames = Int(ceil(seconds * Double(fps)))
    for f in 0..<frames {
        let t = (Double(f) + 0.5) / Double(fps)
        let bitmap = NSBitmapImageRep(bitmapDataPlanes: nil, pixelsWide: W, pixelsHigh: H, bitsPerSample: 8, samplesPerPixel: 4, hasAlpha: true, isPlanar: false, colorSpaceName: .deviceRGB, bytesPerRow: W * 4, bitsPerPixel: 32)!
        NSGraphicsContext.saveGraphicsState()
        NSGraphicsContext.current = NSGraphicsContext(bitmapImageRep: bitmap)
        paper.setFill(); NSRect(x: 0, y: 0, width: W, height: H).fill()
        // the line singing now, if any; the next one, for the inhale
        let active = lines.first { t >= $0.start && t < $0.start + $0.articulation.duration }
        let next = lines.first { $0.start > t }
        let lead = next.map { $0.start - t } ?? 9
        if let a = active {
            let lt = t - a.start
            face.previewPose = a.articulation.pose(at: lt)
            let nextCue = a.cues.first { $0.start > lt }
            let cueLead = nextCue.map { $0.start - lt } ?? lead
            let breath = cueLead < 0.38 ? sin(Double.pi * (1 - cueLead / 0.38)) : 0
            face.resting = false
            face.previewExpression(beat: (t - offset) * bpm / 60, effort: CGFloat(a.articulation.level(at: lt)), breath: CGFloat(breath))
        } else {
            face.previewPose = rest
            face.resting = lead > 1.2                      // lids down between lines, waking 1.2 s before the next
            let breath = lead < 0.38 ? sin(Double.pi * (1 - lead / 0.38)) : 0
            face.previewExpression(beat: (t - offset) * bpm / 60, effort: 0, breath: CGFloat(breath))
        }
        face.draw(face.bounds)
        NSGraphicsContext.restoreGraphicsState()
        pipe.fileHandleForWriting.write(Data(bytes: bitmap.bitmapData!, count: W * H * 4))
        if f % 600 == 0 { FileHandle.standardError.write("  \(member) \(f)/\(frames)\n".data(using: .utf8)!) }
    }
    try pipe.fileHandleForWriting.close()
    enc.waitUntilExit()
    print(out)
}
