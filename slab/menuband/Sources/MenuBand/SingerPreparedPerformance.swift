import AVFoundation
import CryptoKit

/// Silent, in-app preparation. The same post-dynamics buffers are exported for
/// reinforcement and retained for the originating Mac; preparation never plays.
final class SingerPreparedPerformance {
    struct Entry {
        let info: [String: String]
        let fingerprint: String
        let renders: [SungRender]
    }
    private(set) var entries: [String: Entry] = [:]
    private var generation = 0
    let instance = UUID().uuidString
    static let root = URL(fileURLWithPath: "/tmp/menuband-trio", isDirectory: true)

    static func validID(_ id: String) -> Bool {
        !id.isEmpty && id.count <= 80 && id.allSatisfy { $0.isASCII && ($0.isLetter || $0.isNumber || $0 == "-") }
    }
    static func fingerprint(_ info: [String: String]) -> String {
        let stable = info.filter { !["prepareId","preparedId","startEpoch","requestId"].contains($0.key) }
        let data = try! JSONSerialization.data(withJSONObject: stable, options: [.sortedKeys])
        return SHA256.hash(data: data).map { String(format:"%02x",$0) }.joined()
    }
    static func applyDynamics(_ render: SungRender, line: SungLine, index: Int, info: [String:String]) {
        let levels = (info["singLineGains"] ?? "").split(separator:",").map { Float($0) ?? 1 }
        let accents = (info["singNoteGains"] ?? "").split(separator:"/",omittingEmptySubsequences:false)
            .map { $0.split(separator:",").map { Float($0) ?? 1 } }
        let gain = index < levels.count && levels[index].isFinite ? min(1,max(0,levels[index])) : 1
        let row = index < accents.count ? accents[index] : []
        let dynamics = row.isEmpty ? nil : SingerDynamics(notation:line.notes,bpm:line.bpm,gains:row,offset:render.spanOffset)
        var peak: Float = 0, sum = 0.0
        if let samples = render.buffer.floatChannelData {
            for frame in 0..<Int(render.buffer.frameLength) {
                let g = gain * (dynamics?.gain(at:Double(frame)/render.buffer.format.sampleRate) ?? 1)
                for ch in 0..<Int(render.buffer.format.channelCount) {
                    samples[ch][frame] *= g; peak = max(peak,abs(samples[ch][frame]))
                }
                sum += Double(samples[0][frame]*samples[0][frame])
            }
        }
        NSLog("🎚 dynamics: line %d gain %.3f final peak %.3f RMS %.5f",index+1,gain,peak,
              sqrt(sum/Double(max(1,render.buffer.frameLength))))
    }
    private func status(_ id: String, phase: String, extra: [String:Any] = [:]) {
        var state: [String:Any] = ["schema":"menuband-prepared-v1","id":id,"phase":phase,
            "instance":instance,"pid":ProcessInfo.processInfo.processIdentifier,
            "updatedAt":Date().timeIntervalSince1970,"silentPrepare":true]
        extra.forEach { state[$0.key] = $0.value }
        do {
            let dir = Self.root.appendingPathComponent(id,isDirectory:true)
            try FileManager.default.createDirectory(at:dir,withIntermediateDirectories:true)
            let data = try JSONSerialization.data(withJSONObject:state,options:[.sortedKeys,.prettyPrinted])
            try data.write(to:dir.appendingPathComponent("status.json"),options:.atomic)
        } catch { NSLog("Trio prepare status write failed: %@",error.localizedDescription) }
    }
    func cancelAll() {
        generation += 1
        for id in entries.keys { status(id,phase:"stopped") }
        entries.removeAll()
    }
    func ready(_ id: String) {
        guard Self.validID(id) else { return }
        guard let entry = entries[id] else { status(id,phase:"unready",extra:["reason":"Not prepared in this app instance"]); return }
        status(id,phase:"ready",extra:["fingerprint":entry.fingerprint,"phrases":entry.renders.count])
    }
    func take(_ id: String, info: [String:String]) -> [SungRender]? {
        guard let entry = entries[id], entry.fingerprint == Self.fingerprint(info) else { return nil }
        return entry.renders
    }
    func prepare(_ id: String, info: [String:String], singer: MenuBandSinger, format: AVAudioFormat) {
        guard Self.validID(id) else { return }
        let bpm = Double(info["bpm"] ?? "") ?? 0
        guard bpm.isFinite && bpm > 0, let sung = SungLine(info:info,bpm:bpm),
              (sung.voice.hasSuffix("(Enhanced)") || sung.voice.hasSuffix("(Premium)")),
              format.channelCount == 1 else {
            status(id,phase:"error",extra:["reason":"Requires valid notes, BPM, pinned Enhanced/Premium voice and mono format"]); return
        }
        if let old = entries[id] {
            guard old.fingerprint == Self.fingerprint(info) else { status(id,phase:"error",extra:["reason":"ID already holds a different payload"]); return }
            ready(id); return
        }
        generation += 1
        let gen = generation, lines = sung.splitLines()
        var renders: [SungRender] = [], assets: [[String:Any]] = []
        let dir = Self.root.appendingPathComponent(id,isDirectory:true)
        status(id,phase:"preparing")
        func next(_ index: Int) {
            guard generation == gen else { status(id,phase:"cancelled"); return }
            guard index < lines.count else {
                do {
                    let fingerprint = Self.fingerprint(info)
                    let manifest: [String:Any] = ["schema":"menuband-prepared-v1","id":id,
                        "instance":instance,"pid":ProcessInfo.processInfo.processIdentifier,
                        "fingerprint":fingerprint,"voice":sung.voice,"bpm":bpm,
                        "sampleFormat":"float32-le","postDynamics":true,"preSlideEffects":true,
                        "payload":info,"phrases":assets]
                    let data = try JSONSerialization.data(withJSONObject:manifest,options:[.sortedKeys,.prettyPrinted])
                    try data.write(to:dir.appendingPathComponent("manifest.json"),options:.atomic)
                    entries[id] = Entry(info:info,fingerprint:fingerprint,renders:renders)
                    ready(id)
                } catch { status(id,phase:"error",extra:["reason":error.localizedDescription]) }
                return
            }
            singer.render(lines[index],into:format) { [self] render in
                guard generation == gen else { status(id,phase:"cancelled"); return }
                guard let render, render.notesUsed == render.noteCount, render.articulation != nil else {
                    status(id,phase:"error",extra:["reason":"Incomplete phrase render","line":index]); return
                }
                Self.applyDynamics(render,line:lines[index],index:index,info:info)
                do {
                    let name = String(format:"phrase-%02d.wav",index)
                    let url = dir.appendingPathComponent(name)
                    // Float WAV retains the exact samples kept in memory.
                    let settings: [String:Any] = [AVFormatIDKey:kAudioFormatLinearPCM,
                        AVSampleRateKey:format.sampleRate,AVNumberOfChannelsKey:1,
                        AVLinearPCMBitDepthKey:32,AVLinearPCMIsFloatKey:true,
                        AVLinearPCMIsBigEndianKey:false,AVLinearPCMIsNonInterleaved:false]
                    do { let file = try AVAudioFile(forWriting:url,settings:settings,
                        commonFormat:.pcmFormatFloat32,interleaved:false); try file.write(from:render.buffer) }
                    let bytes = try Data(contentsOf:url)
                    let hash = SHA256.hash(data:bytes).map { String(format:"%02x",$0) }.joined()
                    let rawName = String(format:"phrase-%02d.f32",index)
                    let raw = Data(bytes:render.buffer.floatChannelData![0],count:Int(render.buffer.frameLength)*4)
                    try raw.write(to:dir.appendingPathComponent(rawName),options:.atomic)
                    let rawHash = SHA256.hash(data:raw).map { String(format:"%02x",$0) }.joined()
                    let a = render.articulation!
                    assets.append(["index":index,"file":name,"sha256":hash,"bytes":bytes.count,
                        "rawFile":rawName,"rawSha256":rawHash,
                        "frames":Int(render.buffer.frameLength),"sampleRate":format.sampleRate,"channels":1,
                        "spanOffset":render.spanOffset,"duration":render.duration,
                        "notesUsed":render.notesUsed,"noteCount":render.noteCount,
                        "mouthCues":a.cues.map { ["start":$0.start,"end":$0.end,"shape":$0.shape.rawValue] as [String:Any] }])
                    renders.append(render)
                    next(index+1)
                } catch { status(id,phase:"error",extra:["reason":error.localizedDescription]); return }
            }
        }
        next(0)
    }
}
