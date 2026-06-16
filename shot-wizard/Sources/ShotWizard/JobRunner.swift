// JobRunner.swift — spawns the board's node driver (gen-vo / gen-shots /
// build) for generation and assembly, streaming its log lines to the UI.
// One job at a time. (Copied from ClipWizard.)
import Foundation

final class JobRunner {
    private(set) var running = false
    private var process: Process?

    /// Spawn `node <driver> <args>` from cwd. onLine fires on the main
    /// queue per output chunk; onExit with the exit code.
    func run(driver: URL, args: [String], cwd: URL,
             onLine: @escaping (String) -> Void,
             onExit: @escaping (Int32) -> Void) -> Bool {
        guard !running else { return false }
        running = true
        let p = Process()
        p.executableURL = URL(fileURLWithPath: "/usr/bin/env")
        p.arguments = ["node", driver.path] + args
        p.currentDirectoryURL = cwd
        let pipe = Pipe()
        p.standardOutput = pipe
        p.standardError = pipe
        pipe.fileHandleForReading.readabilityHandler = { h in
            let chunk = String(data: h.availableData, encoding: .utf8) ?? ""
            if !chunk.isEmpty { DispatchQueue.main.async { onLine(chunk) } }
        }
        p.terminationHandler = { [weak self] proc in
            pipe.fileHandleForReading.readabilityHandler = nil
            DispatchQueue.main.async {
                self?.running = false
                self?.process = nil
                onExit(proc.terminationStatus)
            }
        }
        do {
            try p.run()
            process = p
            return true
        } catch {
            running = false
            DispatchQueue.main.async { onExit(-1) }
            return false
        }
    }
}
