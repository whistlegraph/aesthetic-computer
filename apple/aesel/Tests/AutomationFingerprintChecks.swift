import Foundation
@main struct AutomationFingerprintChecks {
    static func main() throws {
        let root = FileManager.default.temporaryDirectory.appendingPathComponent(UUID().uuidString)
        defer { try? FileManager.default.removeItem(at: root) }
        try FileManager.default.createDirectory(at: root.appendingPathComponent("Contents/_CodeSignature"), withIntermediateDirectories: true)
        let source = root.appendingPathComponent("Contents/source")
        try Data("one".utf8).write(to: source)
        let original = AeselAutomation.hashBundle(root)
        precondition(original.count == 64)
        try Data("signature".utf8).write(to: root.appendingPathComponent("Contents/_CodeSignature/CodeResources"))
        precondition(AeselAutomation.hashBundle(root) == original, "Signing must not change source identity")
        try Data("two".utf8).write(to: source)
        precondition(AeselAutomation.hashBundle(root) != original, "Changed contents must change identity")
        try Data("one".utf8).write(to: source)
        try FileManager.default.moveItem(at: source, to: root.appendingPathComponent("Contents/renamed"))
        precondition(AeselAutomation.hashBundle(root) != original, "Paths are part of source identity")
        print("Automation bundle fingerprint checks passed")
    }
}
