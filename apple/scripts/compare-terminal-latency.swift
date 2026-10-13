// Native key injection -> WindowServer frame with a changed glyph.
// Build: xcrun swiftc -O -parse-as-library compare-terminal-latency.swift -o /tmp/ac-visible-latency
// Usage: /tmp/ac-visible-latency PID WINDOW_ID OUTPUT_DIR [TRIALS] [x,y,w,h]
import AppKit
import ScreenCaptureKit
import CoreMedia
import CoreVideo

func nowTicks() -> UInt64 { mach_absolute_time() }
func milliseconds(_ ticks: UInt64) -> Double {
    var info = mach_timebase_info_data_t(); mach_timebase_info(&info)
    return Double(ticks) * Double(info.numer) / Double(info.denom) / 1_000_000
}
func pause(_ seconds: Double) async { try? await Task.sleep(nanoseconds: UInt64(seconds * 1e9)) }
@discardableResult
func key(_ pid: pid_t, _ windowID: CGWindowID, _ code: CGKeyCode) throws -> UInt64 {
    var focused: CFTypeRef?
    var focusedID: CGWindowID = 0
    AXUIElementCopyAttributeValue(AXUIElementCreateApplication(pid),kAXFocusedWindowAttribute as CFString,&focused)
    guard NSWorkspace.shared.frontmostApplication?.processIdentifier == pid,
          let focused, _AXUIElementGetWindow(focused as! AXUIElement,&focusedID) == .success,
          focusedID == windowID else { throw NSError(domain:"Target window lost focus; no key sent",code:1) }
    let posted = nowTicks()
    let source = CGEventSource(stateID: .privateState)
    for down in [true, false] {
        let event = CGEvent(keyboardEventSource: source, virtualKey: code, keyDown: down)!
        event.flags = []
        event.postToPid(pid)
    }
    return posted
}

final class Capture: NSObject, SCStreamOutput {
    // Frames arrive here, off the main actor, so the trial loop's polling
    // cannot delay delivery. Every other access goes through `locked`.
    let queue = DispatchQueue(label: "ac.visible-latency.capture", qos: .userInteractive)
    func locked<T>(_ body: () -> T) -> T { queue.sync(execute: body) }
    struct Frame { let pixels: [UInt8]; let width: Int; let height: Int; let time: UInt64; let received: UInt64 }
    var latest: Frame?
    var calibration: [Frame] = []
    var collecting = false
    var mask: [(Int, [[UInt8]], [[UInt8]])] = []
    var pending: (time: UInt64, typed: Bool)?
    // Last captured frame at or after the key that still showed the old
    // state: the sample's lower bound at this capture cadence.
    var unchangedSince: UInt64?
    var samples: [[String: Any]] = []
    var frameTimes: [UInt64] = []
    var mismatch = false
    func stream(_ stream: SCStream, didOutputSampleBuffer buffer: CMSampleBuffer, of type: SCStreamOutputType) {
        let received = nowTicks()
        guard type == .screen, let image = buffer.imageBuffer,
              let info = (CMSampleBufferGetSampleAttachmentsArray(buffer, createIfNecessary: false) as? [[SCStreamFrameInfo: Any]])?.first,
              let status = info[.status] as? Int, status == SCFrameStatus.complete.rawValue,
              let time = info[.displayTime] as? UInt64 else { return }
        CVPixelBufferLockBaseAddress(image, .readOnly)
        defer { CVPixelBufferUnlockBaseAddress(image, .readOnly) }
        let w = CVPixelBufferGetWidth(image), h = CVPixelBufferGetHeight(image)
        let stride = CVPixelBufferGetBytesPerRow(image)
        guard let base = CVPixelBufferGetBaseAddress(image)?.assumingMemoryBound(to: UInt8.self) else { return }
        var bytes = [UInt8](repeating: 0, count: w*h*4)
        bytes.withUnsafeMutableBytes { dest in
            for y in 0..<h { memcpy(dest.baseAddress!.advanced(by:y*w*4), base.advanced(by:y*stride), w*4) }
        }
        let frame = Frame(pixels: bytes, width: w, height: h, time: time, received: received)
        if let latest, latest.width != w || latest.height != h { mismatch = true }
        latest = frame
        frameTimes.append(time)
        if collecting { calibration.append(frame) }
        guard let pending, time >= pending.time, !mask.isEmpty else { return }
        var matches = 0
        for (index, empty, typed) in mask {
            let target = pending.typed ? typed : empty
            let other = pending.typed ? empty : typed
            func distance(_ palette: [[UInt8]]) -> Int {
                palette.map { color in (0..<3).reduce(0) { $0 + abs(Int(bytes[index+$1])-Int(color[$1])) } }.min()!
            }
            let targetDistance = distance(target), otherDistance = distance(other)
            if targetDistance < otherDistance { matches += 1 }
        }
        let fraction = Double(matches)/Double(mask.count)
        if fraction >= 0.8 {
            samples.append(["action": pending.typed ? "insert" : "delete",
                            "keyToWindowServerMs": milliseconds(time-pending.time),
                            "lowerBoundMs": unchangedSince.map { milliseconds($0-pending.time) } ?? 0,
                            "keyToCaptureCallbackMs": milliseconds(received-pending.time),
                            "matchingPixels": fraction])
            self.pending = nil
            unchangedSince = nil
        } else {
            unchangedSince = time
        }
    }
    func save(_ frame: Frame, _ path: String) throws {
        let data = Data(frame.pixels) as CFData
        let image = CGImage(width:frame.width, height:frame.height, bitsPerComponent:8, bitsPerPixel:32,
                            bytesPerRow:frame.width*4, space:CGColorSpaceCreateDeviceRGB(),
                            bitmapInfo:CGBitmapInfo(rawValue:CGImageAlphaInfo.premultipliedFirst.rawValue).union(.byteOrder32Little),
                            provider:CGDataProvider(data:data)!, decode:nil, shouldInterpolate:false, intent:.defaultIntent)!
        let rep = NSBitmapImageRep(cgImage:image)
        try rep.representation(using:.png, properties:[:])!.write(to:URL(fileURLWithPath:path))
    }
}

@main struct Main {
    @MainActor static func main() async {
        do { try await run() }
        catch { fputs("\(error)\n", stderr); exit(1) }
    }
    @MainActor static func run() async throws {
        let args = CommandLine.arguments
        guard args.count >= 4, let pid = Int32(args[1]), let id = UInt32(args[2]) else {
            throw NSError(domain:"Usage: PID WINDOW_ID OUTPUT_DIR [TRIALS] [x,y,w,h]",code:1)
        }
        let output = args[3], trials = args.count > 4 ? Int(args[4])! : 60
        let roi = args.count > 5 ? args[5].split(separator:",").compactMap { Int($0) } : nil
        guard trials >= 0, trials == 0 || (roi?.count == 4 && roi![2] > 0 && roi![3] > 0) else {
            throw NSError(domain:"Measured runs require a positive glyph ROI: x,y,width,height",code:1)
        }
        try FileManager.default.createDirectory(atPath:output, withIntermediateDirectories:true)
        guard AXIsProcessTrusted(), CGPreflightScreenCaptureAccess() else {
            throw NSError(domain:"Accessibility and Screen Recording access required",code:1)
        }
        let content = try await SCShareableContent.excludingDesktopWindows(true,onScreenWindowsOnly:false)
        guard let window = content.windows.first(where:{$0.windowID == id && $0.owningApplication?.processID == pid}),
              let app = NSRunningApplication(processIdentifier:pid) else { throw NSError(domain:"Target window/PID missing",code:1) }
        let axApp = AXUIElementCreateApplication(pid)
        var value:CFTypeRef?
        AXUIElementCopyAttributeValue(axApp,kAXWindowsAttribute as CFString,&value)
        // Match the explicitly selected CG window; never raise another terminal session.
        for axWindow in value as? [AXUIElement] ?? [] {
            var number:CGWindowID = 0
            if _AXUIElementGetWindow(axWindow,&number) == .success, number == id {
                AXUIElementPerformAction(axWindow,kAXRaiseAction as CFString)
                AXUIElementSetAttributeValue(axApp,kAXFocusedWindowAttribute as CFString,axWindow)
            }
        }
        app.activate(options:[])
        await pause(1)
        let config = SCStreamConfiguration()
        config.width = Int(window.frame.width); config.height = Int(window.frame.height)
        config.pixelFormat = kCVPixelFormatType_32BGRA
        // Ask for the panel's full rate explicitly; .zero measured ~34 ms.
        config.minimumFrameInterval = CMTime(value: 1, timescale: 120)
        config.queueDepth = 3
        config.showsCursor = false
        config.ignoreShadowsSingleWindow = true
        let capture = Capture()
        let stream = SCStream(filter:SCContentFilter(desktopIndependentWindow:window),configuration:config,delegate:nil)
        try stream.addStreamOutput(capture,type:.screen,sampleHandlerQueue:capture.queue)
        try await stream.startCapture()
        await pause(1)
        guard let first = capture.locked({ capture.latest }) else { throw NSError(domain:"No captured frames",code:1) }
        try capture.save(first,output+"/initial.png")
        if trials == 0 { try await stream.stopCapture(); print("Snapshot: \(output)/initial.png"); return }
        // These tests must start at an EMPTY prompt in a dedicated test window.
        // One warm-up establishes focus; no Return or command is ever injected.
        try key(pid,id,0); await pause(0.5); try key(pid,id,51); await pause(0.5)
        func calibrate() async -> [Capture.Frame] {
            capture.locked { capture.collecting = true; capture.calibration = [capture.latest!] }
            await pause(1.4)
            return capture.locked { capture.collecting = false; return capture.calibration }
        }
        let emptyFrames = await calibrate()
        try key(pid,id,0); await pause(0.5)
        let typedFrames = await calibrate()
        let empty = emptyFrames.last!, typed = typedFrames.last!
        try capture.save(empty,output+"/empty.png"); try capture.save(typed,output+"/typed.png")
        var mask: [(Int, [[UInt8]], [[UInt8]])] = []
        for y in 0..<empty.height { for x in 0..<empty.width {
            if let roi, !(x >= roi[0] && x < roi[0]+roi[2] && y >= roi[1] && y < roi[1]+roi[3]) { continue }
            let index = (y*empty.width+x)*4
            // The empty prompt's block caret may blink over this glyph cell.
            // Accept its observed colors only when both states remain separable.
            func palette(_ frames: [Capture.Frame]) -> [[UInt8]] {
                Array(Set(frames.map { Array($0.pixels[index..<index+3]) }))
            }
            let a = palette(emptyFrames), b = palette(typedFrames)
            let separation = a.flatMap { ac in b.map { bc in
                zip(ac,bc).reduce(0) { $0+abs(Int($1.0)-Int($1.1)) }
            }}.min()!
            if separation > 120 { mask.append((index,a,b)) }
        }}
        guard mask.count >= 5 else { throw NSError(domain:"Insufficient stable glyph pixels: \(mask.count)",code:1) }
        capture.locked { capture.mask = mask }
        var maskImage = empty.pixels
        for (index,_,_) in mask { maskImage[index] = 0; maskImage[index+1] = 0; maskImage[index+2] = 255 }
        try capture.save(Capture.Frame(pixels:maskImage,width:empty.width,height:empty.height,time:0,received:0),output+"/mask.png")
        try key(pid,id,51); await pause(0.5)
        var timedOut = 0
        for index in 0..<trials {
            // Vary phase relative to display refresh; avoid a fixed-interval resonance.
            await pause(Double(130+(index*73)%141)/1000)
            guard NSWorkspace.shared.frontmostApplication?.processIdentifier == pid else {
                throw NSError(domain:"Focus changed; stopping without sending further keys",code:1)
            }
            var focused: CFTypeRef?
            var focusedID: CGWindowID = 0
            AXUIElementCopyAttributeValue(axApp,kAXFocusedWindowAttribute as CFString,&focused)
            guard let focused, _AXUIElementGetWindow(focused as! AXUIElement,&focusedID) == .success,
                  focusedID == id else { throw NSError(domain:"Target window lost focus",code:1) }
            let typed = index%2 == 0
            let posted = try key(pid,id,typed ? 0 : 51)
            capture.locked { capture.pending = (posted,typed); capture.unchangedSince = nil }
            for _ in 0..<100 {
                await pause(0.005)
                if capture.locked({ capture.pending == nil }) { break }
            }
            if capture.locked({ () -> Bool in
                defer { capture.pending = nil }
                return capture.pending != nil
            }) { timedOut += 1 }
        }
        // Even counts finish at the empty prompt.
        if trials%2 == 1 { try key(pid,id,51); await pause(0.2) }
        try await stream.stopCapture()
        let (latest, samples, frameTimes, mismatch) = capture.locked {
            (capture.latest!, capture.samples, capture.frameTimes, capture.mismatch)
        }
        try capture.save(latest,output+"/final.png")
        guard !mismatch else { throw NSError(domain:"Capture dimensions changed",code:1) }
        let report:[String:Any] = ["app":app.localizedName ?? "", "pid":pid,"windowID":id,
            "windowSize":[empty.width,empty.height],"roi":roi ?? [],"maskPixels":mask.count,
            "trials":trials,"timeouts":timedOut,"samples":samples,
            "measurement":"CGEvent.postToPid to SCStreamFrameInfo.displayTime of first matching glyph frame; excludes hardware keyboard and panel scanout",
            "calibrationFrames":[emptyFrames.count,typedFrames.count],
            "captureFrameIntervalsMs":zip(frameTimes.dropFirst(),frameTimes).map { milliseconds($0.0-$0.1) }]
        try JSONSerialization.data(withJSONObject:report,options:[.prettyPrinted,.sortedKeys])
            .write(to:URL(fileURLWithPath:output+"/result.json"))
        print("\(app.localizedName ?? ""): \(samples.count)/\(trials), \(mask.count) glyph pixels. \(output)/result.json")
        if timedOut > 0 { exit(2) }
    }
}
@_silgen_name("_AXUIElementGetWindow")
func _AXUIElementGetWindow(_ element: AXUIElement, _ id: UnsafeMutablePointer<CGWindowID>) -> AXError
