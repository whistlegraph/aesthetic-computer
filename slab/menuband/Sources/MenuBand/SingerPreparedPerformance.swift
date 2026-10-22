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
    /// A stop cancels renders in flight; finished takes stay memorized, so the
    /// next fleetReady is instant instead of a fresh 35-second render (TIAT, Oct 22).
    func cancelAll() {
        generation += 1
        for id in entries.keys { status(id,phase:"stopped") }
    }
    func ready(_ id: String) {
        guard Self.validID(id) else { return }
        guard let entry = entries[id] ?? restore(id,info:nil) else { status(id,phase:"unready",extra:["reason":"Not prepared in this app instance"]); return }
        status(id,phase:"ready",extra:["fingerprint":entry.fingerprint,"phrases":entry.renders.count])
    }
    func take(_ id: String, info: [String:String]) -> [SungRender]? {
        guard let entry = entries[id] ?? restore(id,info:info), entry.fingerprint == Self.fingerprint(info) else { return nil }
        return entry.renders
    }
    /// A take rendered earlier (by this or a previous app instance, before a stop,
    /// a crash or a relaunch) comes back from its exported files: the exact samples,
    /// their mouth cues, and an energy curve re-measured from the samples.
    @discardableResult
    func restore(_ id: String, info: [String:String]?) -> Entry? {
        guard Self.validID(id) else { return nil }
        let dir = Self.root.appendingPathComponent(id,isDirectory:true)
        guard let data = try? Data(contentsOf:dir.appendingPathComponent("manifest.json")),
              let m = (try? JSONSerialization.jsonObject(with:data)) as? [String:Any],
              let fingerprint = m["fingerprint"] as? String,
              let payload = m["payload"] as? [String:String],
              let phrases = m["phrases"] as? [[String:Any]], !phrases.isEmpty else { return nil }
        if let info, Self.fingerprint(info) != fingerprint { return nil }
        var renders: [SungRender] = []
        for p in phrases {
            // The .f32 file is the exact sample memory, hash-pinned in the manifest.
            guard let rawName = p["rawFile"] as? String, let rawHash = p["rawSha256"] as? String,
                  let frames = p["frames"] as? Int, frames > 0,
                  let sr = p["sampleRate"] as? Double, let spanOffset = p["spanOffset"] as? Double,
                  let notesUsed = p["notesUsed"] as? Int, let noteCount = p["noteCount"] as? Int,
                  let cueList = p["mouthCues"] as? [[String:Any]],
                  let raw = try? Data(contentsOf:dir.appendingPathComponent(rawName)), raw.count == frames*4,
                  SHA256.hash(data:raw).map({ String(format:"%02x",$0) }).joined() == rawHash,
                  let format = AVAudioFormat(commonFormat:.pcmFormatFloat32,sampleRate:sr,channels:1,interleaved:false),
                  let buffer = AVAudioPCMBuffer(pcmFormat:format,frameCapacity:AVAudioFrameCount(frames)),
                  let samples = buffer.floatChannelData?[0] else {
                NSLog("🎤 prepared take %@: phrase files incomplete or altered; rendering again",id); return nil
            }
            buffer.frameLength = AVAudioFrameCount(frames)
            raw.withUnsafeBytes { bytes in _ = memcpy(samples,bytes.baseAddress!,frames*4) }
            let cues = cueList.compactMap { c -> SingerArticulation.Cue? in
                guard let s = c["start"] as? Double, let e = c["end"] as? Double,
                      let raw = c["shape"] as? String, let shape = SingerViseme(rawValue:raw) else { return nil }
                return .init(start:s,end:e,shape:shape)
            }
            let hop = max(1,Int(sr/SingerArticulation.energyRate))
            var energy: [Double] = []; var peak: Float = 0; var i = 0
            while i < frames {
                let n = min(hop,frames-i); var sum: Float = 0
                for j in 0..<n { let v = samples[i+j]; sum += v*v; peak = max(peak,abs(v)) }
                energy.append(Double(sqrt(sum/Float(n)))); i += hop
            }
            if peak > 0 { energy = energy.map { min(1,$0/Double(peak)) } }
            var render = SungRender(buffer:buffer,spanOffset:spanOffset,notesUsed:notesUsed,noteCount:noteCount,peak:peak)
            render.articulation = SingerArticulation(cues:cues,energy:energy,duration:Double(frames)/sr)
            renders.append(render)
        }
        let entry = Entry(info:payload,fingerprint:fingerprint,renders:renders)
        entries[id] = entry
        NSLog("🎤 prepared take %@ restored from disk: %d phrases",id,renders.count)
        return entry
    }
    func prepare(_ id: String, info: [String:String], singer: MenuBandSinger, format: AVAudioFormat) {
        guard Self.validID(id) else { return }
        let bpm = Double(info["bpm"] ?? "") ?? 0
        guard bpm.isFinite && bpm > 0, let sung = SungLine(info:info,bpm:bpm),
              (sung.voice.hasSuffix("(Enhanced)") || sung.voice.hasSuffix("(Premium)")),
              format.channelCount == 1 else {
            status(id,phase:"error",extra:["reason":"Requires valid notes, BPM, pinned Enhanced/Premium voice and mono format"]); return
        }
        if let old = entries[id] ?? restore(id,info:info) {
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
