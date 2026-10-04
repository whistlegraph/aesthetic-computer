// vision-matte.swift — her and the guitar lifted from the room by Apple Vision, frame by frame, for perf-relight.mjs.
// (v103: "the masking on her could be better, the feathering is bad — use AI inference.")
//
// Reads raw rgb24 frames on stdin (ffmpeg -f rawvideo), runs VNGenerateForegroundInstanceMaskRequest (subject lifting:
// she and the guitar she holds come as one subject, soft-edged at full resolution through generateScaledMaskForImage),
// falls back to VNGeneratePersonSegmentationRequest(.accurate) when no instance is found, smooths the matte over time
// (EMA, --ema 0.3 of the new frame) so it does not flicker, and writes gray8 frames on stdout. Take clock, native size.
//
//   swiftc -O bin/vision-matte.swift -o /tmp/vision-matte
//   ffmpeg -v error -i src/take.mov -f rawvideo -pix_fmt rgb24 - | /tmp/vision-matte 960 540 [--ema 0.3] [--person] \
//     [--exclude 575,400,960,540] | ffmpeg -v error -y -f rawvideo -pix_fmt gray -s 960x540 -r 30 -i - -c:v libx264 -crf 10 -preset fast -pix_fmt yuv420p src/matte-take.mp4
import Foundation
import Vision
import CoreGraphics
import CoreVideo

let args = CommandLine.arguments
let W = Int(args[1])!, H = Int(args[2])!
var ema: Float = 0.3; var personOnly = false
if let i = args.firstIndex(of: "--ema"), i + 1 < args.count { ema = Float(args[i + 1])! }
if args.contains("--person") { personOnly = true }
// --exclude x0,y0,x1,y1: a region that is never her (the bed and its pillows, bottom right — subject lifting
// sometimes takes them along with her and they would flicker in and out); soft 24 px ramp at its edges
var excl: [Float]? = nil
if let i = args.firstIndex(of: "--exclude"), i + 1 < args.count { excl = args[i + 1].split(separator: ",").map { Float($0)! } }
var keep = [Float](repeating: 1, count: W * H)
if let e = excl { let ramp: Float = 24
    for y in 0..<H { for x in 0..<W { let fx = Float(x), fy = Float(y)
        let inx = min(1, max(0, (fx - e[0]) / ramp)) * min(1, max(0, (e[2] - fx) / ramp)), iny = min(1, max(0, (fy - e[1]) / ramp)) * min(1, max(0, (e[3] - fy) / ramp))
        keep[y * W + x] = 1 - inx * iny } } }
let frameBytes = W * H * 3
let stdin = FileHandle.standardInput, stdout = FileHandle.standardOutput, stderr = FileHandle.standardError
let cs = CGColorSpaceCreateDeviceRGB()
var prev = [Float](repeating: 0, count: W * H), cur = [Float](repeating: 0, count: W * H)
var out8 = [UInt8](repeating: 0, count: W * H)
var n = 0, fallbacks = 0

func readFrame() -> Data? {
    var d = Data(capacity: frameBytes)
    while d.count < frameBytes {
        let chunk = stdin.readData(ofLength: frameBytes - d.count)
        if chunk.isEmpty { return d.isEmpty ? nil : nil }
        d.append(chunk)
    }
    return d
}

// a CVPixelBuffer mask (OneComponent32Float or OneComponent8) → cur, resampled to W×H if Vision gave another size
func unpack(_ pb: CVPixelBuffer) {
    CVPixelBufferLockBaseAddress(pb, .readOnly); defer { CVPixelBufferUnlockBaseAddress(pb, .readOnly) }
    let mw = CVPixelBufferGetWidth(pb), mh = CVPixelBufferGetHeight(pb), stride = CVPixelBufferGetBytesPerRow(pb)
    let base = CVPixelBufferGetBaseAddress(pb)!, fmt = CVPixelBufferGetPixelFormatType(pb)
    let f32 = fmt == kCVPixelFormatType_OneComponent32Float
    for y in 0..<H {
        let sy = mh == H ? y : min(mh - 1, Int(Float(y) * Float(mh) / Float(H)))
        let row = base.advanced(by: sy * stride)
        for x in 0..<W {
            let sx = mw == W ? x : min(mw - 1, Int(Float(x) * Float(mw) / Float(W)))
            cur[y * W + x] = f32 ? row.load(fromByteOffset: sx * 4, as: Float.self) : Float(row.load(fromByteOffset: sx, as: UInt8.self)) / 255
        }
    }
}

while let data = readFrame() { autoreleasepool {                 // without this Vision's per-frame objects pile up until the machine kills us (died at frame ~3900)
    let provider = CGDataProvider(data: data as CFData)!
    let img = CGImage(width: W, height: H, bitsPerComponent: 8, bitsPerPixel: 24, bytesPerRow: W * 3, space: cs,
                      bitmapInfo: CGBitmapInfo(rawValue: CGImageAlphaInfo.none.rawValue), provider: provider, decode: nil, shouldInterpolate: false, intent: .defaultIntent)!
    let handler = VNImageRequestHandler(cgImage: img, options: [:])
    var got = false
    if !personOnly {
        let req = VNGenerateForegroundInstanceMaskRequest()
        if (try? handler.perform([req])) != nil, let obs = req.results?.first, !obs.allInstances.isEmpty,
           let pb = try? obs.generateScaledMaskForImage(forInstances: obs.allInstances, from: handler) {
            unpack(pb); got = true
        }
    }
    if !got {                                                      // no subject found: the person segmenter, accurate
        let req = VNGeneratePersonSegmentationRequest(); req.qualityLevel = .accurate; req.outputPixelFormat = kCVPixelFormatType_OneComponent8
        if (try? handler.perform([req])) != nil, let obs = req.results?.first { unpack(obs.pixelBuffer); fallbacks += 1 }
        else { for i in 0..<(W * H) { cur[i] = prev[i] } }
    }
    let a: Float = n == 0 ? 1 : ema
    for i in 0..<(W * H) { let v = prev[i] + (cur[i] * keep[i] - prev[i]) * a; prev[i] = v; out8[i] = UInt8(max(0, min(255, v * 255 + 0.5))) }
    stdout.write(Data(out8)); n += 1
    if n % 300 == 0 { stderr.write("\r  \(n) frames, \(fallbacks) fallbacks".data(using: .utf8)!) }
} }
stderr.write("\r✓ vision-matte: \(n) frames, \(fallbacks) person-segmenter fallbacks\n".data(using: .utf8)!)
