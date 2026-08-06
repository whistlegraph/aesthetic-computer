import Foundation

enum JukeSource: Int, CaseIterable {
    case local, aesthetic, spotify, appleMusic

    var fixedLabel: String? {
        switch self {
        case .local: return nil
        case .aesthetic: return "Aesthetic"
        case .spotify: return "Spotify"
        case .appleMusic: return "Apple Music"
        }
    }

    var canDetachRecords: Bool {
        switch self {
        case .local, .aesthetic: return true
        case .spotify, .appleMusic: return false
        }
    }

    func label(machineName: String = JukeSource.machineName) -> String {
        fixedLabel ?? Self.shortMachineName(machineName)
    }

    static var machineName: String {
        Host.current().localizedName ?? ProcessInfo.processInfo.hostName
    }

    static func shortMachineName(_ raw: String) -> String {
        var name = raw.trimmingCharacters(in: .whitespacesAndNewlines)
        for suffix in [".localdomain", ".local"] where name.lowercased().hasSuffix(suffix) {
            name.removeLast(suffix.count)
            break
        }
        guard !name.isEmpty else { return "This Mac" }
        if let macRange = name.range(of: "MacBook ", options: [.caseInsensitive, .backwards]) {
            let suffix = name[macRange.upperBound...].trimmingCharacters(in: .whitespaces)
            if !suffix.isEmpty { return suffix }
        }
        if let last = name.split(separator: " ").last, name.count > 18 {
            return String(last)
        }
        return name
    }
}
