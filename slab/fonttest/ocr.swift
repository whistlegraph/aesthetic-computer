import AppKit
import Vision
import CoreImage

// slab-font ocr — machine legibility test.
//
// Draws each font tiny, the way a Retina terminal would (2x, grayscale
// antialiasing), optionally blurs it like an eye at a distance, enlarges it so
// the recognizer can find the line, and asks Apple's text recognizer to read
// random code-like strings back. Prints the percentage of characters read.
//
// A proxy, not a person: one recognizer, a light page, 240 characters per
// cell. Differences of a few points are noise.
//
// Arguments: "Label=/path/to/font[=weight]" ... (missing files are skipped)

let sizes: [CGFloat] = [5, 6, 7, 8, 9, 10]
let blurs: [(String, Double)] = [("sharp", 0), ("blurred", 1.0)]   // sigma in device pixels at 2x
let chars = Array("Il1|i!jO0oQDB8S5Z2rnmhuvwce6G9gqC3ba7T{}()[];:.,Il1|O0o8B5Srnm6G")
let linesPer = 6, groups = 8, groupLen = 5

// One fixed set of strings, so every font reads the same text.
var seed: UInt64 = 0x5eed1234
func rnd() -> Int { seed = seed &* 6364136223846793005 &+ 1442695040888963407; return Int(seed >> 33) }
let truths: [String] = (0..<linesPer).map { _ in
  (0..<groups).map { _ in String((0..<groupLen).map { _ in chars[rnd() % chars.count] }) }.joined(separator: " ")
}

struct Candidate { let label: String; let descriptor: CTFontDescriptor }

func candidate(_ arg: String) -> Candidate? {
  let parts = arg.components(separatedBy: "=")
  guard parts.count >= 2, FileManager.default.fileExists(atPath: parts[1]) else { return nil }
  let url = URL(fileURLWithPath: parts[1]) as CFURL
  guard let all = CTFontManagerCreateFontDescriptorsFromURL(url) as? [CTFontDescriptor], !all.isEmpty else { return nil }
  // A collection holds several faces: take Regular.
  var d = all.first { (CTFontDescriptorCopyAttribute($0, kCTFontStyleNameAttribute) as? String) == "Regular" } ?? all[0]
  if parts.count > 2, let weight = Double(parts[2]) {
    d = CTFontDescriptorCreateCopyWithAttributes(d, [kCTFontVariationAttribute: [2003265652: weight]] as CFDictionary)  // 'wght'
  }
  return Candidate(label: parts[0], descriptor: d)
}

let ci = CIContext()
func render(_ text: String, _ font: CTFont, blur: Double) -> CGImage {
  let ink = NSColor(srgbRed: 0.11, green: 0.10, blue: 0.13, alpha: 1)
  let line = CTLineCreateWithAttributedString(NSAttributedString(string: text, attributes: [.font: font, .foregroundColor: ink]))
  let b = CTLineGetBoundsWithOptions(line, [])
  let pad: CGFloat = 8, scale: CGFloat = 2, up = 4
  let w = Int(ceil((b.width + pad * 2) * scale)), h = Int(ceil((b.height + pad * 2) * scale))
  let cs = CGColorSpace(name: CGColorSpace.sRGB)!, info = CGImageAlphaInfo.premultipliedLast.rawValue
  let c = CGContext(data: nil, width: w, height: h, bitsPerComponent: 8, bytesPerRow: 0, space: cs, bitmapInfo: info)!
  c.setFillColor(CGColor(srgbRed: 0.94, green: 0.91, blue: 0.96, alpha: 1))
  c.fill(CGRect(x: 0, y: 0, width: w, height: h))
  c.scaleBy(x: scale, y: scale)
  c.setShouldSmoothFonts(false)
  c.textPosition = CGPoint(x: pad, y: pad - b.minY)
  CTLineDraw(line, c)
  var img = c.makeImage()!
  if blur > 0 {
    let soft = CIImage(cgImage: img).clampedToExtent().applyingGaussianBlur(sigma: blur)
    img = ci.createCGImage(soft, from: CGRect(x: 0, y: 0, width: w, height: h))!
  }
  let big = CGContext(data: nil, width: w * up, height: h * up, bitsPerComponent: 8, bytesPerRow: 0, space: cs, bitmapInfo: info)!
  big.interpolationQuality = .high
  big.draw(img, in: CGRect(x: 0, y: 0, width: w * up, height: h * up))
  return big.makeImage()!
}

func read(_ img: CGImage) -> String {
  let req = VNRecognizeTextRequest()
  req.recognitionLevel = .accurate
  req.usesLanguageCorrection = false
  req.recognitionLanguages = ["en-US"]
  try? VNImageRequestHandler(cgImage: img, options: [:]).perform([req])
  let found = (req.results ?? []).sorted { $0.boundingBox.minX < $1.boundingBox.minX }
  return found.compactMap { $0.topCandidates(1).first?.string }.joined()
}

/// Characters of `truth` that survive in `got`: length minus edit distance.
func charactersRead(_ truth: [Character], _ got: [Character]) -> Int {
  var row = Array(0...got.count)
  for i in 0..<truth.count {
    var previous = row[0]
    row[0] = i + 1
    for j in 0..<got.count {
      let next = min(row[j + 1] + 1, row[j] + 1, previous + (truth[i] == got[j] ? 0 : 1))
      previous = row[j + 1]
      row[j + 1] = next
    }
  }
  return max(0, truth.count - row[got.count])
}

let candidates = CommandLine.arguments.dropFirst().compactMap(candidate)
guard candidates.count > 0 else { print("no fonts found"); exit(1) }
FileHandle.standardError.write("reading \(candidates.count) fonts at \(sizes.count) sizes, sharp and blurred (a few minutes)…\n".data(using: .utf8)!)

func pad(_ s: String, _ n: Int) -> String { s.padding(toLength: n, withPad: " ", startingAt: 0) }
var summary: [(String, Double, [Double])] = []

for c in candidates {
  let big = CTFontCreateWithFontDescriptor(c.descriptor, 1000, nil)
  var glyph = CGGlyph(0), n: UniChar = 110
  CTFontGetGlyphsForCharacters(big, &n, &glyph, 1)
  let advance = CTFontGetAdvancesForGlyphs(big, .horizontal, &glyph, nil, 1) / 1000
  var means: [Double] = []
  var lines: [String] = []
  for (name, blur) in blurs {
    var scores: [Double] = []
    for size in sizes {
      let font = CTFontCreateWithFontDescriptor(c.descriptor, size, nil)
      var hit = 0, total = 0
      for t in truths {
        let truth = Array(t.replacingOccurrences(of: " ", with: ""))
        let got = Array(read(render(t, font, blur: blur)).replacingOccurrences(of: " ", with: ""))
        hit += charactersRead(truth, got)
        total += truth.count
      }
      scores.append(100 * Double(hit) / Double(total))
    }
    means.append(scores.reduce(0, +) / Double(scores.count))
    lines.append("  \(pad(name, 8)) " + scores.map { String(format: "%4.0f", $0) }.joined())
  }
  print("\(c.label)  (cell \(String(format: "%.3f", advance)) em)")
  print("  " + pad("size pt", 8) + " " + sizes.map { String(format: "%4.0f", $0) }.joined())
  lines.forEach { print($0) }
  summary.append((c.label, advance, means))
}

print("\nmean % of characters read, 5 to 10 pt")
print(pad("font", 24) + pad("cell", 10) + blurs.map { pad($0.0, 9) }.joined())
for (label, advance, means) in summary.sorted(by: { $0.2.reduce(0, +) > $1.2.reduce(0, +) }) {
  print(pad(label, 24) + pad(String(format: "%.3f em", advance), 10) + means.map { pad(String(format: "%.0f", $0), 9) }.joined())
}
print("A few points either way is noise. Blurred is closer to tired eyes or sitting back.")
