import Foundation

@main struct MimeInterfaceCheck {
    @MainActor static func main() throws {
        let policy = MimeInterface(sourceURL: URL(fileURLWithPath: CommandLine.arguments[1]))
        let events = try JSONSerialization.jsonObject(with: FileHandle.standardInput.readDataToEndOfFile()) as! [[String: Any]]
        var snapshots: [[String: Any]] = []
        for event in events {
            let effects = policy.send(event["type"] as? String ?? "", values: event)
            let view = try JSONSerialization.jsonObject(with: JSONEncoder().encode(policy.presentation))
            let encodedEffects = try JSONSerialization.jsonObject(with: JSONEncoder().encode(effects))
            snapshots.append(["view": view, "effects": encodedEffects])
        }
        print(String(data: try JSONSerialization.data(withJSONObject: snapshots), encoding: .utf8)!)
    }
}
