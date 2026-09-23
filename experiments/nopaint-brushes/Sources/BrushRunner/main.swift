import Foundation
import BrushCore

do {
    // One bounded JSON request on stdin. The native library itself has no IO.
    let input = FileHandle.standardInput.readData(ofLength: 8_000_001)
    guard input.count <= 8_000_000 else { throw BrushError.invalid("Request too large") }
    let request = try JSONDecoder().decode(RenderRequest.self, from: input)
    let pixels = try renderDocument(request.document, preview: request.preview)
    let encoder = JSONEncoder()
    encoder.outputFormatting = [.sortedKeys]
    let result = try encoder.encode(RenderResult(document: request.document, pixels: pixels))
    FileHandle.standardOutput.write(result)
} catch {
    FileHandle.standardError.write(Data("\(error)\n".utf8))
    exit(1)
}
