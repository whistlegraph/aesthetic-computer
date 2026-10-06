import Foundation
import Vision
import ImageIO
// Local-only pixel verification. Never sends screenshots to a service.
var rows: [[String: Any]] = []
for path in CommandLine.arguments.dropFirst() {
    do {
        let request = VNRecognizeTextRequest()
        request.recognitionLevel = .accurate
        request.usesCPUOnly = true
        request.usesLanguageCorrection = false
        request.recognitionLanguages = ["en-US"]
        try VNImageRequestHandler(url: URL(fileURLWithPath: path)).perform([request])
        rows.append(["path":path,"text":request.results?.compactMap { $0.topCandidates(1).first?.string } ?? []])
    } catch { rows.append(["path":path,"error":error.localizedDescription]) }
}
let data = try JSONSerialization.data(withJSONObject: rows, options: [.sortedKeys])
print(String(data:data,encoding:.utf8)!)
