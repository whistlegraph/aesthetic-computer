// face-track.swift — sample a video and report where the face is.
//
// AVFoundation walks the frames, Vision finds the faces. We print one JSON
// record per sample; smoothing and framing decisions belong to reframe.mjs,
// not here. This file only answers "where was the face at time t."
//
//   swift marketing/lib/face-track.swift <video> [--fps 10] > track.json
//
// Coordinates come out of Vision normalized with a bottom-left origin; we
// flip Y and emit top-left-origin pixels, which is what canvas wants.

import AVFoundation
import Foundation
import Vision

struct Sample: Codable {
  let t: Double
  let x: Double
  let y: Double
  let w: Double
  let h: Double
  let confidence: Double
}

let args = CommandLine.arguments
guard args.count > 1 else {
  FileHandle.standardError.write("usage: face-track.swift <video> [--fps N]\n".data(using: .utf8)!)
  exit(2)
}
let videoPath = args[1]
var sampleFps = 10.0
if let i = args.firstIndex(of: "--fps"), i + 1 < args.count, let v = Double(args[i + 1]) {
  sampleFps = v
}

let url = URL(fileURLWithPath: videoPath)
let asset = AVURLAsset(url: url)

guard let track = asset.tracks(withMediaType: .video).first else {
  FileHandle.standardError.write("no video track\n".data(using: .utf8)!)
  exit(1)
}

let naturalSize = track.naturalSize.applying(track.preferredTransform)
let srcW = abs(naturalSize.width)
let srcH = abs(naturalSize.height)
let duration = CMTimeGetSeconds(asset.duration)

let reader = try AVAssetReader(asset: asset)
let output = AVAssetReaderTrackOutput(
  track: track,
  outputSettings: [kCVPixelBufferPixelFormatTypeKey as String: kCVPixelFormatType_32BGRA]
)
output.alwaysCopiesSampleData = false
reader.add(output)
reader.startReading()

let request = VNDetectFaceRectanglesRequest()
var samples: [Sample] = []
let interval = 1.0 / sampleFps
var nextSampleAt = 0.0

while let buf = output.copyNextSampleBuffer() {
  let t = CMTimeGetSeconds(CMSampleBufferGetPresentationTimeStamp(buf))
  guard t >= nextSampleAt, let pixels = CMSampleBufferGetImageBuffer(buf) else { continue }
  nextSampleAt += interval

  let handler = VNImageRequestHandler(cvPixelBuffer: pixels, orientation: .up, options: [:])
  try? handler.perform([request])

  // Several faces in frame is not our problem — take the biggest, which for a
  // talking head is the speaker and not a poster on the wall behind them.
  guard
    let face = (request.results ?? []).max(by: {
      $0.boundingBox.width * $0.boundingBox.height < $1.boundingBox.width * $1.boundingBox.height
    })
  else { continue }

  let b = face.boundingBox
  samples.append(
    Sample(
      t: t,
      x: Double(b.origin.x) * Double(srcW),
      y: (1.0 - Double(b.origin.y) - Double(b.height)) * Double(srcH),  // flip to top-left
      w: Double(b.width) * Double(srcW),
      h: Double(b.height) * Double(srcH),
      confidence: Double(face.confidence)
    ))
}

struct Track: Codable {
  let video: String
  let srcW: Double
  let srcH: Double
  let duration: Double
  let sampleFps: Double
  let samples: [Sample]
}

let out = Track(
  video: videoPath, srcW: Double(srcW), srcH: Double(srcH),
  duration: duration, sampleFps: sampleFps, samples: samples)

let encoder = JSONEncoder()
encoder.outputFormatting = .prettyPrinted
FileHandle.standardOutput.write(try encoder.encode(out))
