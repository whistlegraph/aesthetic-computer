import Foundation

@main struct StreamedCodeChecks {
    static func main() throws {
        let source = ["export function paint({write}) {", #"  write("bell 🔔 \\");"#, "}"].joined(separator: "\n")
        let json = String(data: try JSONSerialization.data(withJSONObject: ["source": source, "note": "bell"]), encoding: .utf8)!
        for size in [1, 2, 7, 32] {
            let session = Session()
            session.receive(["type": "source", "source": "last working piece"])
            let chars = Array(json)
            for start in stride(from: 0, to: chars.count, by: size) {
                let delta = String(chars[start..<min(chars.count, start + size)])
                session.receive(["type": "bridge", "method": "item/modelCode/delta", "params": ["itemId": "write-1", "delta": delta]])
                precondition(session.source == "last working piece", "partial code must not execute")
                precondition(source.hasPrefix(session.streamingCode))
            }
            precondition(session.streamingCode == source)
            session.receive(["type": "source", "source": source, "version": 1])
            precondition(session.streamingCode.isEmpty && session.source == source)
            session.receive(["type": "bridge", "method": "item/modelCode/delta", "params": ["itemId": "write-2", "delta": "{\"source\":\"next"]])
            precondition(session.streamingCode == "next")
            session.receive(["type": "bridge", "method": "turn/completed", "params": ["turn": ["status": "failed"]]])
            precondition(session.streamingCode.isEmpty && session.source == source)
        }
        print("Streamed source handles token splits, escapes, saves and failed turns.")
    }
}
