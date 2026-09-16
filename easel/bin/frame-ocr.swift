// Local-only Vision OCR for a captured Easel display. No network or language correction.
import Foundation
import Vision
import ImageIO

func fail(_ message: String) -> Never {
    FileHandle.standardError.write(Data((message + "\n").utf8))
    exit(1)
}
guard CommandLine.arguments.count == 2 else { fail("Usage: frame-ocr image.png") }
let url = URL(fileURLWithPath: CommandLine.arguments[1])
do {
    let attributes = try url.resourceValues(forKeys: [.isRegularFileKey, .isSymbolicLinkKey, .fileSizeKey])
    guard attributes.isRegularFile == true, attributes.isSymbolicLink != true,
          (attributes.fileSize ?? Int.max) <= 16 * 1024 * 1024 else { fail("Expected a regular image under 16 MiB.") }
    guard let source = CGImageSourceCreateWithURL(url as CFURL, nil),
          let properties = CGImageSourceCopyPropertiesAtIndex(source, 0, nil) as? [CFString: Any],
          let width = properties[kCGImagePropertyPixelWidth] as? Int,
          let height = properties[kCGImagePropertyPixelHeight] as? Int,
          width > 0, height > 0, width <= 1_048_576 / height,
          let image = CGImageSourceCreateImageAtIndex(source, 0, nil) else { fail("Image exceeds the one-megapixel capture limit or cannot be decoded.") }
    let request = VNRecognizeTextRequest()
    request.recognitionLevel = .accurate
    request.usesLanguageCorrection = false
    if #available(macOS 13.0, *) { request.revision = VNRecognizeTextRequestRevision3 }
    try VNImageRequestHandler(cgImage: image, options: [:]).perform([request])
    let regions: [[String: Any]] = (request.results ?? []).prefix(100).compactMap { result in
        guard let text = result.topCandidates(1).first else { return nil }
        let rect = result.boundingBox
        return ["text": String(text.string.prefix(2000)), "confidence": text.confidence,
                "x": rect.minX * Double(width), "y": (1 - rect.maxY) * Double(height),
                "width": rect.width * Double(width), "height": rect.height * Double(height)]
    }
    let output = try JSONSerialization.data(withJSONObject: ["regions": regions], options: [.sortedKeys])
    FileHandle.standardOutput.write(output)
    FileHandle.standardOutput.write(Data([10]))
} catch { fail("OCR failed: \(error.localizedDescription)") }
