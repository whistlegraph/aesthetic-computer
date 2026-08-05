import Foundation

enum JukeResources {
    /// Resolve without SwiftPM's generated `Bundle.module` accessor, which
    /// embeds architecture-specific build directories in the executable.
    static func url(forResource name: String, withExtension ext: String) -> URL? {
        let home = FileManager.default.homeDirectoryForCurrentUser
        let executable = URL(fileURLWithPath: CommandLine.arguments[0])
            .standardizedFileURL.deletingLastPathComponent()
        let cwd = URL(fileURLWithPath: FileManager.default.currentDirectoryPath)
        let roots = [
            Bundle.main.resourceURL?.appendingPathComponent("Assets"),
            executable.appendingPathComponent("Assets"),
            home.appendingPathComponent(".local/lib/jukewizard/Assets"),
            home.appendingPathComponent("aesthetic-computer/juke-wizard/Sources/JukeWizard/Assets"),
            home.appendingPathComponent("Developer/aesthetic-computer/juke-wizard/Sources/JukeWizard/Assets"),
            cwd.appendingPathComponent("Sources/JukeWizard/Assets"),
            cwd.appendingPathComponent("juke-wizard/Sources/JukeWizard/Assets"),
        ].compactMap { $0 }
        for root in roots {
            let candidate = root.appendingPathComponent(name).appendingPathExtension(ext)
            if FileManager.default.fileExists(atPath: candidate.path) { return candidate }
        }
        return nil
    }
}
